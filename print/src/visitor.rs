// Some headers
use rustc_hir::{StmtKind, Stmt, Local, Expr, ExprKind, UnOp, Param, QPath, PatKind, Mutability};
use rustc_ast::walk_list;
use rustc_hir::intravisit::{self, Visitor};
use crate::expr_visitor::*;
use crate::expr_visitor_utils::{bool_of_mut, match_op};
use rustviz_lib::data::*;
use rustc_middle::{
  mir::Body,
  ty::{TyCtxt,Ty, adjustment::*},
};
use std::collections::{HashSet, BTreeMap};

// these are the visitor trait itself
// the visitor will walk through the hir
// the approach we are using is simple here. when visiting an expression or a statement,
// match it with a pattern and do analysis accordingly. The difference between expressions 
// and statements is subtle.       
// See ExprKind at : https://doc.rust-lang.org/stable/nightly-rustc/rustc_hir/hir/enum.ExprKind.html
// See StmtKind at : https://doc.rust-lang.org/stable/nightly-rustc/rustc_hir/hir/enum.StmtKind.html
impl<'a, 'tcx> Visitor<'tcx> for ExprVisitor<'a, 'tcx> {
  fn visit_param(&mut self, param: &'tcx Param<'tcx>){
    let line_num=self.span_to_line(&param.span);
    let ty = self.tcx.typeck(param.hir_id.owner).pat_ty(param.pat);
    match param.pat.kind {
      PatKind::Binding(binding_annotation, _ann_hirid, ident, _op_pat) =>{
        let name: String = ident.to_string();
        if ty.is_ref() {
          self.add_ref(name.clone(), bool_of_mut(ty.ref_mutability().unwrap()),bool_of_mut(binding_annotation.1), line_num, ResourceTy::Anonymous, BTreeMap::new());
        }
        else if ty.is_adt() && !self.ty_is_special_owner(ty){ // kind of weird given we don't have a InitStructParam
          let owner_hash = self.rap_hashes as u64;
          self.add_struct(name.clone(), owner_hash, false, bool_of_mut(binding_annotation.1));
          for field in ty.ty_adt_def().unwrap().all_fields() {
            let field_name = format!("{}.{}", name.clone(), field.name.as_str());
            self.add_struct(field_name, owner_hash, true, bool_of_mut(binding_annotation.1));
          }
        }
        else {
          self.add_owner(name.clone(), bool_of_mut(binding_annotation.1));
        }
        self.add_external_event(line_num, ExternalEvent::InitRefParam { param: self.raps.get(&name).unwrap().0.to_owned() });
        self.annotate_src(name.clone(), ident.span, false, *self.raps.get(&name).unwrap().0.hash());
      }
      _=>{}
    }
  }
  fn visit_expr(&mut self, expr: &'tcx Expr<'tcx>) {
    match expr.kind {
      ExprKind::Call(fn_expr, args) => {
        match fn_expr.kind {
          // Match onto println! macro
          ExprKind::Path(QPath::Resolved(_,rustc_hir::Path{res: rustc_hir::def::Res::Def(_, id), ..})) 
          if !id.is_local() => {
            // to see what the macro expansion looks like:
            // println!("{:#?}", expr);
            match args {
              [Expr{kind: ExprKind::Call(_, a),..}] => {
                match a {
                  [_, Expr{kind: ExprKind::AddrOf(_, _, 
                    Expr{kind: ExprKind::Array(x),..}),..}] => {
                      for exp in x.iter() {
                        match exp {
                          Expr{kind: ExprKind::Call(_, format_args), ..} => {
                            let fn_name: String = String::from("println!");
                            for arg in format_args.iter() {
                              self.match_args(&arg, fn_name.clone());
                            }
                          }
                          _ => {
                            println!("getting here to the println 1");
                          }
                        }
                      }
                    }
                  _ => {
                    println!("getting here to the println 2");
                  }
                }
              }
              _ => {
                let fn_name: String = self.hirid_to_var_name(fn_expr.hir_id).unwrap();
                self.add_fn(fn_name.clone());
                for arg in args.iter(){
                  self.match_args(&arg, fn_name.clone());
                }
              }
            }
          }
          _ => {
            let fn_name: String = self.hirid_to_var_name(fn_expr.hir_id).unwrap();
            self.add_fn(fn_name.clone());
            for arg in args.iter(){
              self.match_args(&arg, fn_name.clone());
            }
          }
        }
      }
      ExprKind::MethodCall(name_and_generic_args, rcvr, args, _) => {
        let line_num = self.expr_to_line(&rcvr);
        // println!("typeck res: {:#?}", self.tcx.typeck(name_and_generic_args.hir_id.owner));
        // println!("item local id {:#?}", name_and_generic_args.hir_id.local_id);
        // println!("caller local id {:#?}", rcvr.hir_id.local_id);
        let rcvr_name = self.hirid_to_var_name(rcvr.hir_id).unwrap();
        let fn_name =self.hirid_to_var_name(name_and_generic_args.hir_id).unwrap();
        self.add_fn(fn_name.clone());
        let rcvr_rap = self.raps.get(&rcvr_name).unwrap().0.to_owned();
        self.update_rap(&rcvr_rap, line_num);
        let fn_rap = self.raps.get(&fn_name).unwrap().0.to_owned();
        // typecheck the whole function body
        let adjustment_map = self.tcx.typeck(name_and_generic_args.hir_id.owner).adjustments();
        match adjustment_map.get(rcvr.hir_id) {
          Some(adj_vec) => {
            for a in adj_vec.iter() {
              match a.kind {
                Adjust::Borrow(AutoBorrow::Ref(_, m)) => {
                  match m {
                    AutoBorrowMutability::Mut{allow_two_phase_borrow: AllowTwoPhase::Yes} => {
                      self.add_ev(line_num, Evt::PassByMRef, ResourceTy::Value(fn_rap.clone()), ResourceTy::Value(rcvr_rap.clone()));
                    },
                    AutoBorrowMutability::Not => {
                      self.add_ev(line_num, Evt::PassBySRef, ResourceTy::Value(fn_rap.clone()), ResourceTy::Value(rcvr_rap.clone()));
                    }
                    _ => {}
                  }
                }
                _ => {}
              }
            }
          }
          None => {}
        }

        for arg in args.iter(){
          self.match_args(&arg, fn_name.clone());
        }
      }
      ExprKind::Binary(_, expra, exprb) => { // don't know if this is necessary
        self.visit_expr(expra);
        self.visit_expr(exprb);
      }

      ExprKind::AddrOf(_, _, inner) if inner.is_syntactic_place_expr() && !inner.span.from_expansion() => {}

      ExprKind::Assign(lhs_expr, rhs_expr, _,) | ExprKind::AssignOp(_, lhs_expr, rhs_expr) => {
        self.visit_expr(rhs_expr); // visit rhs side first to order moves?
        let line_num = self.expr_to_line(&lhs_expr);
        let lhs_rty = self.resource_of_lhs(lhs_expr);
        let lhs_rap = self.raps.get(&lhs_rty.real_name()).unwrap().0.clone();
        let lhs_ty = self.tcx.typeck(lhs_expr.hir_id.owner).node_type(lhs_expr.hir_id);
        if lhs_rty.is_ref() && lhs_ty.is_ref() { // need to check type to avoid *ref = 8 
          match lhs_rty {
            ResourceTy::Value(_) => {
              self.update_ref_data_lhs(lhs_rty.clone(), rhs_expr, line_num);
            }
            ResourceTy::Deref(_) => {
              let num_derefs = self.num_derefs(lhs_expr);
              let new_lender = self.find_lender(&rhs_expr);
              println!("new lender {}", new_lender.name());
              let ref_data = self.borrow_map.get(&lhs_rty.real_name()).unwrap().clone();
              let modified_ref = ref_data.aliasing.get(&num_derefs).unwrap().to_owned();
              let modified_ref_data = self.borrow_map.get(&modified_ref).unwrap().clone();
              let new_data = self.new_aliasing_data(self.fetch_rap(&rhs_expr));

              // new underlying lender changes for all aliases above the ref we are mutating
              for (k, alias) in ref_data.aliasing.iter() {
                if *k <= num_derefs {
                  self.lender_update(new_lender.clone(), ResourceTy::Value(self.raps.get(alias).unwrap().0.clone()));
                  let old_alias_data = self.borrow_map.get(alias).unwrap().aliasing.clone();
                  let offset = num_derefs - *k;
                  for (k2, _) in old_alias_data.iter() { // remove everything below on the chain
                    if *k2 >= offset {
                      self.borrow_map.get_mut(alias).unwrap().aliasing.remove(k2);
                    }
                  }
                  self.update_aliasing_data(alias, &new_data, offset);
                }
                else { // no longer alias anything below mutated reference 
                  self.borrow_map.get_mut(&lhs_rty.real_name()).unwrap().aliasing.remove(k);
                }
              }
              let num_borrowers = self.lenders.get(&ref_data.lender.name())
              .unwrap_or(&HashSet::from([ResourceTy::Anonymous])).len(); // determine how many references are currently live for this resource

              let to_ro = match ref_data.lender {
                ResourceTy::Anonymous => ResourceTy::Deref(lhs_rap.clone()),
                _ => ref_data.lender.clone()
              };
              println!("old lenders: {:#?}", self.lenders);
              

              if num_borrowers > 1 { // if there are more than one, do not return the resource
                self.add_external_event(line_num, ExternalEvent::RefDie { from: lhs_rty.clone(), to: to_ro });
                self.lenders.get_mut(&ref_data.lender.name()).unwrap().remove(&lhs_rty);
              }
              else { // if we are the only borrower then return the resource
                match modified_ref_data.ref_mutability {
                  true => self.add_ev(line_num, Evt::MDie, to_ro, lhs_rty.clone()),
                  false => self.add_ev(line_num, Evt::SDie, to_ro, lhs_rty.clone())
                }
                println!("getting here");
                self.lenders.remove(&ref_data.lender.name());
              }

              // add new underlying lender
              self.borrow_map.get_mut(&lhs_rty.real_name()).unwrap().lender = new_lender.clone();
              self.lenders.get_mut(&new_lender.name()).unwrap().insert(lhs_rty.clone());

              println!("new lenders: {:#?}", self.lenders);
            }
            _ => panic!("not possible")
          }
        }

        self.match_rhs(lhs_rty.clone(), rhs_expr);
      }

      // The function body block
      ExprKind::Block(block, _) => {
        // this scoping logic isn't necessary except for when defining functions inside of functions
        let prev_scope = self.current_scope;
        let new_scope = self.tcx.sess.source_map().lookup_char_pos(expr.span.hi()).line;
        self.current_scope = new_scope;
        self.visit_block(block); // visit all the statements in the block
        // handle the return expr (if there is one)
        match block.expr {
          Some(expr) => {
            self.annotate_expr(expr);
            self.match_rhs(ResourceTy::Caller, expr); // this probably needs to be fixed
            //self.match_rhs(AccessPoint {mutability: Mutability::Not, name: "None".to_owned(), members: None}, expr, false);
          }
          None => {}
        }
        // backtrack
        self.current_scope = prev_scope;
      }

      ExprKind::Unary(UnOp::Deref, exp) => { self.visit_expr(exp) }
      ExprKind::Path(QPath::Resolved(_,p)) => {
        let name = self.tcx.hir().name(p.segments[0].hir_id).as_str().to_owned();
        let r = &self.raps.get(&name).unwrap().0.clone();
        let line_num = self.span_to_line(&p.span);
        self.update_rap(r, line_num);
      }
      ExprKind::DropTemps(exp) => {
        self.visit_expr(exp);
      }
      ExprKind::If(guard_expr, if_expr, else_expr) => {
        self.visit_expr(&guard_expr);
        self.visit_expr(&if_expr);
        match else_expr {
          Some(e) => self.visit_expr(e),
          None => {}
        }
      }
      
      _ => {
        intravisit::walk_expr(self, expr);
      }
    }
  }
  fn visit_stmt(&mut self, statement: &'tcx Stmt<'tcx>) {
    match statement.kind {
      StmtKind::Local(ref local) => self.visit_local(local),
      StmtKind::Item(item) => self.visit_nested_item(item),
      StmtKind::Expr(ref expression) | StmtKind::Semi(ref expression) => {
          self.visit_expr(expression)
      }
    }
    self.annotate_stmt(statement);
  }

  // locals are let statements: let <pat>:<ty> = <expr>
  fn visit_local(&mut self, local: &'tcx Local<'tcx>) {
    match local.pat.kind {
      PatKind::Binding(binding_annotation, ann_hirid, ident, op_pat) => {
        let lhs_var:String = ident.to_string();
        let tycheck_results = self.tcx.typeck(ann_hirid.owner);
        let lhs_ty = tycheck_results.node_type(ann_hirid);
        // println!("TY OF LHS: {:#?}", tycheck_results.node_type(ann_hirid));
        // println!("sorted lhs string {:#?}", tycheck_results.node_type(ann_hirid).sort_string(self.tcx));
        // println!("sorted lhs string {:#?}", tycheck_results.node_type(ann_hirid).prefix_string(self.tcx));
        // annotate left side of let statement
        //self.mutability_map.insert(lhs_var.clone(), binding_annotation.1);
        match local.init { // init refers to RHS of let
          Some(expr) => {
            self.visit_expr(expr);
            self.define_lhs(lhs_var.clone(), bool_of_mut(binding_annotation.1), expr, lhs_ty);
            self.match_rhs(ResourceTy::Value(self.raps.get(&lhs_var).unwrap().0.to_owned()), expr);
            // println!("lhs RAP :{:#?}", self.raps.get(&lhs_var).unwrap());
              // self.visit_expr(expr); -- may not be a bad idea to visit rhs expr then match it
              //self.match_rhs(AccessPoint { mutability: binding_annotation.1, name: lhs_var, members: None}, expr, false);
              // match expr.kind {
              //   ExprKind::Path(_) => {},
              //   ExprKind::Block(..)=>{},
              //   _=>{
              //     // if RHS is more than just a path (variable) we need to walk it to possibly append
              //     // additionaly events to the line
              //     self.visit_expr(expr);
              //   }
              // }
          }
           _ => {} // in the case of a declaration ex: let a; nothing happens
        }
      }
      _ => { panic!("unrecognized local pattern") }
    }
    
    
    //walk_list!(self, visit_expr, &local.init);
    // this has to do with let = if ... else statements
    if let Some(els) = local.els {
      self.visit_block(els);
    }
    // I think this just walks the type annotations
    walk_list!(self, visit_ty, &local.ty);
  }
} 