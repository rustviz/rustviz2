// Some headers
use rustc_hir::{StmtKind, Stmt, Expr, ExprKind, UnOp, Param, QPath, PatKind, Mutability, LetStmt, def::*};
// use rustc_ast::walk_list;
use rustc_hir::intravisit::{self, Visitor};
use crate::expr_visitor::*;
use crate::expr_visitor_utils::{bool_of_mut, match_op};
use rustviz_lib::data::*;
use rustc_middle::{
  mir::Body,
  ty::{TyCtxt,Ty, adjustment::*},
};
use core::borrow;
use std::collections::{BTreeMap, HashSet, VecDeque};
use rustc_utils::mir::borrowck_facts::get_body_with_borrowck_facts;

// these are the visitor trait itself
// the visitor will walk through the hir
// the approach we are using is simple here. when visiting an expression or a statement,
// match it with a pattern and do analysis accordingly. The difference between expressions 
// and statements is subtle.       
// See ExprKind at : https://doc.rust-lang.org/stable/nightly-rustc/rustc_hir/hir/enum.ExprKind.html
// See StmtKind at : https://doc.rust-lang.org/stable/nightly-rustc/rustc_hir/hir/enum.StmtKind.html
impl<'a, 'tcx> Visitor<'tcx> for ExprVisitor<'a, 'tcx> {
  fn visit_body(&mut self, body: &'tcx rustc_hir::Body<'tcx>) {
    self.current_scope = self.tcx.sess.source_map().lookup_char_pos(body.value.span.hi()).line;
    for param in body.params {
      self.visit_param(param);
    }
    // let borrow_data = get_body_with_borrowck_facts(self.tcx, body.value.hir_id.owner.def_id);
    // println!("location map {:#?}", borrow_data.borrow_set.location_map);
    // println!("activation map{:#?}", borrow_data.borrow_set.activation_map);
    // println!("local_map {:#?}", borrow_data.borrow_set.local_map);
    // // println!("locals state at exit {:#?}", borrow_data.borrow_set.locals_state_at_exit);
    // println!("input facts {:#?}", borrow_data.input_facts); 
    // println!("output facts {:#?}", borrow_data.output_facts);
    self.visit_expr(body.value); // visit fn body
    match body.value { // handle return expression if there is one
      Expr{kind: ExprKind::Block(b, _), ..} => {
        match b.expr {
          Some(e) => {
            let tycheck_results = self.tcx.typeck(e.hir_id.owner);
            let lhs_ty = tycheck_results.node_type(e.hir_id);
            // TODO: only append this event if parent fn ctxt doesn't return void
            let is_copyable = lhs_ty.is_copy_modulo_regions(self.tcx, self.tcx.param_env(e.hir_id.owner));
            let evt = if lhs_ty.is_ref() {
              match lhs_ty.ref_mutability().unwrap() {
                Mutability::Not => Evt::Copy,
                Mutability::Mut => Evt::Move,
              }
            } else {
              match is_copyable {
                true => Evt::Copy, 
                false => Evt::Move
              }
            };

            let to_ro = ResourceTy::Caller;
            let from_ro = match self.fetch_rap(e) {
              Some(r) => ResourceTy::Value(r), // todo, technically need to check for deref here
              None => ResourceTy::Anonymous
            };
            
            let line_num = self.expr_to_line(e);
            self.add_ev(line_num, evt, to_ro, from_ro);
          }
          _ => {}
        }
      }
      _ => {
        println!("unexpected fn body");
      }
    }
    self.annotate_expr(body.value); // then annotate the body
  } 


  fn visit_param(&mut self, param: &'tcx Param<'tcx>){
    let line_num=self.span_to_line(&param.span);
    let ty = self.tcx.typeck(param.hir_id.owner).pat_ty(param.pat);
    match param.pat.kind {
      PatKind::Binding(binding_annotation, _ann_hirid, ident, _op_pat) =>{
        let name: String = ident.to_string();
        if ty.is_ref() {
          self.add_ref(name.clone(), 
          bool_of_mut(ty.ref_mutability().unwrap()),
          bool_of_mut(binding_annotation.1), line_num, 
          ResourceTy::Anonymous, VecDeque::new());
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
                              self.visit_expr(&arg);
                              self.match_arg(&arg, fn_name.clone());
                              //self.match_args(&arg, fn_name.clone());
                            }
                          }
                          _ => {
                            println!("getting here to the println 1");
                          }
                        }
                      }
                    }
                  _ => {
                    //self.visit_expr(fn_expr);
                    println!("fn expr {:#?}", fn_expr);
                    let fn_name: String = self.hirid_to_var_name(fn_expr.hir_id).unwrap();
                    println!("function name {}", fn_name);
                    self.add_fn(fn_name.clone());
                    for arg in a.iter(){
                      self.visit_expr(&arg);
                      self.match_arg(&arg, fn_name.clone());
                      // self.match_args(&arg, fn_name.clone());
                    }
                  }
                }
              }
              _ => {
                let fn_name: String = self.hirid_to_var_name(fn_expr.hir_id).unwrap();
                self.add_fn(fn_name.clone());
                for arg in args.iter(){
                  self.visit_expr(&arg);
                  self.match_arg(&arg, fn_name.clone());
                  // self.match_args(&arg, fn_name.clone());
                }
              }
            }
          }
          _ => {
            let fn_name: String = self.hirid_to_var_name(fn_expr.hir_id).unwrap();
            self.add_fn(fn_name.clone());
            for arg in args.iter(){
              self.visit_expr(&arg);
              self.match_arg(&arg, fn_name.clone());
              //self.match_args(&arg, fn_name.clone());
            }
          }
        }
      }
      ExprKind::MethodCall(name_and_generic_args, rcvr, args, _) => {
        let line_num = self.expr_to_line(&rcvr);
        // println!("typeck res: {:#?}", self.tcx.typeck(name_and_generic_args.hir_id.owner));
        // println!("item local id {:#?}", name_and_generic_args.hir_id.local_id);
        // println!("caller local id {:#?}", rcvr.hir_id.local_id);
        self.visit_expr(rcvr);
        let rcvr_name = self.hirid_to_var_name(rcvr.hir_id).unwrap();
        let fn_name =self.hirid_to_var_name(name_and_generic_args.hir_id).unwrap();
        self.add_fn(fn_name.clone());
        println!("raps {:#?}", self.raps);
        println!("rcvr name {}", rcvr_name);
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
          self.visit_expr(&arg);
          self.match_arg(&arg, fn_name.clone());
          //self.match_args(&arg, fn_name.clone());
        }
      }
      ExprKind::Binary(_, expra, exprb) => { // don't know if this is necessary
        self.visit_expr(expra);
        self.visit_expr(exprb);
      }
      //       ExprKind::AddrOf(_, _, inner) if inner.is_syntactic_place_expr() && !inner.span.from_expansion() => {}

      ExprKind::AddrOf(_, _, exp) => {
        self.visit_expr(exp);
      }

      ExprKind::Assign(lhs_expr, rhs_expr, _,) | ExprKind::AssignOp(_, lhs_expr, rhs_expr) => {
        self.visit_expr(lhs_expr);
        self.visit_expr(rhs_expr); // visit rhs side first to order moves?
        let line_num = self.expr_to_line(&lhs_expr);
        let lhs_rty = self.resource_of_lhs(lhs_expr);
        let lhs_rap = self.raps.get(&lhs_rty.real_name()).unwrap().0.clone();
        let lhs_ty = self.tcx.typeck(lhs_expr.hir_id.owner).node_type(lhs_expr.hir_id);
        let is_copyable = lhs_ty.is_copy_modulo_regions(self.tcx, self.tcx.param_env(lhs_expr.hir_id.owner));
        let e = if lhs_ty.is_ref() {
          match lhs_ty.ref_mutability().unwrap() {
            Mutability::Not => Evt::Copy,
            Mutability::Mut => Evt::Move,
          }
        } else {
          match is_copyable {
            true => Evt::Copy, 
            false => Evt::Move
          }
        };
        if lhs_ty.is_ref() { // if we are pointing at a new piece of data
          match lhs_rty {
            // ex:
            // let mut a = &b (where b is &&i32)
            // a = &c (where c is &&i32)
            ResourceTy::Value(_) => {
              let ref_data = self.borrow_map.get(lhs_rap.name()).unwrap().clone();
              let to_ro = match ref_data.lender {
                ResourceTy::Anonymous => ResourceTy::Deref(lhs_rap.clone()),
                _ => ref_data.lender.clone()
              };

              // add event
              let borrowers = self.get_borrowers(&lhs_rty.real_name());
              if borrowers.len() > 1 { // there is another active reference at this point, the resource cannot be returned
                self.add_external_event(line_num, ExternalEvent::RefDie { from: lhs_rty.clone(), to: to_ro, num_curr_borrowers: borrowers.len() - 1 });
              }
              else {
                match ref_data.ref_mutability {
                  true => self.add_ev(line_num, Evt::MDie, to_ro, lhs_rty.clone()),
                  false => self.add_ev(line_num, Evt::SDie, to_ro, lhs_rty.clone())
                }
              }

              // update lhs_rty with new lender information
              let (new_lender, new_aliasing) = self.get_ref_data(&rhs_expr);
              let r = self.borrow_map.get_mut(&lhs_rty.real_name()).unwrap();
              r.aliasing = new_aliasing;
              r.lender = new_lender;
            }

            // ex:
            // let a = & mut b (where b is &i32)
            // *a = &c (where c is &i32)
            ResourceTy::Deref(_) => {
              let ref_data = self.borrow_map.get(lhs_rap.name()).unwrap().clone();
              let deref_index = self.num_derefs(&lhs_expr) - 1;
              let modified_ref_name = ref_data.aliasing.get(deref_index).unwrap();
              let modified_ref_data = self.borrow_map.get(modified_ref_name).unwrap().clone();
              // println!("lhs_rty {:#?}", lhs_rty);
              // println!("deref index {}", deref_index);
              // return resource to nth alias in the chain
              let to_ro =  self.borrow_map.get(modified_ref_name).unwrap().lender.to_owned();
              
              // add event
              let borrowers = self.get_borrowers(&lhs_rty.real_name());
              if borrowers.len() > 1 { // there is another active reference at this point, the resource cannot be returned
                self.add_external_event(line_num, ExternalEvent::RefDie { from: lhs_rty.clone(), to: to_ro, num_curr_borrowers: borrowers.len() - 1 });
              }
              else {
                match modified_ref_data.ref_mutability {
                  true => self.add_ev(line_num, Evt::MDie, to_ro, lhs_rty.clone()),
                  false => self.add_ev(line_num, Evt::SDie, to_ro, lhs_rty.clone())
                }
              }

              // update modified (derefed) reference's lender and aliasing data
              let (new_lender, new_aliasing) = self.get_ref_data(&rhs_expr);
              let r = self.borrow_map.get_mut(modified_ref_name).unwrap();
              r.aliasing = new_aliasing.clone();
              r.lender = new_lender;

              let old_aliasing_data = self.borrow_map.get(&lhs_rty.real_name()).unwrap().aliasing.clone();
              // update other aliases' aliasing data 
              for i in 0..deref_index {
                let ref_name = old_aliasing_data[i].clone();
                let offset = deref_index - i;
                let r = self.borrow_map.get_mut(&ref_name).unwrap();
                r.aliasing.drain(offset..r.aliasing.len()); // remove old aliasing data
                for (j, s) in new_aliasing.iter().enumerate() {
                  r.aliasing.insert(offset + j, s.to_owned());
                }
              }

              // update parent's aliasing data
              let r = self.borrow_map.get_mut(&lhs_rty.real_name()).unwrap();
              r.aliasing.drain(deref_index + 1..r.aliasing.len()); // remove old aliasing data
              for (j, s) in new_aliasing.iter().enumerate() {
                r.aliasing.insert(deref_index + 1 + j, s.to_owned());
              }
            }
            _ => panic!("not possible")
          }
        }

        self.match_rhs(lhs_rty.clone(), rhs_expr, e);
      }

      // The function body block
      ExprKind::Block(block, _) => {
        // this scoping logic isn't necessary except for when defining functions inside of functions
        let prev_scope = self.current_scope;
        let new_scope = self.tcx.sess.source_map().lookup_char_pos(expr.span.hi()).line;
        self.current_scope = new_scope;
        self.visit_block(block); // visit all the statements in the block
          self.current_scope = prev_scope;
      }

      ExprKind::Unary(UnOp::Deref, exp) => { self.visit_expr(exp) }
      ExprKind::Path(QPath::Resolved(_,p)) => {
        match p.res {
          Res::Def(DefKind::Ctor(_, CtorKind::Const), id) => {
            let mut name = String::new();
            for (i, segment) in p.segments.iter().enumerate() {
              name.push_str(self.tcx.hir().name(segment.hir_id).as_str());
              if i < p.segments.len() - 1 {
                name.push_str("::");
              }
            }
            self.add_fn(name);
            return;
          }
          _ => ()
        }
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
        let (else_live, else_decl) = match else_expr {
          Some(e) => {
            self.visit_expr(e);
            (self.get_live_of_expr(e), self.get_decl_of_expr(e))
          }
          None => { (HashSet::new(), HashSet::new()) }
        };
        // compute liveness
        let if_live = self.get_live_of_expr(if_expr);
        let if_decl = self.get_decl_of_expr(if_expr);
        let mut liveness:HashSet<ResourceAccessPoint> = if_live.union(&else_live).cloned().collect();
        liveness = liveness.difference(&if_decl).cloned().collect();
        liveness = liveness.difference(&else_decl).cloned().collect();
        // compute split and merge points
        let line_num = self.expr_to_line(&guard_expr);
        let split = self.tcx.sess.source_map().lookup_char_pos(if_expr.span.lo()).line;
        let if_end = self.tcx.sess.source_map().lookup_char_pos(if_expr.span.hi()).line;
        let merge = match else_expr {
          Some(e) => self.tcx.sess.source_map().lookup_char_pos(e.span.hi()).line,
          None => self.tcx.sess.source_map().lookup_char_pos(if_expr.span.hi()).line
        };

        // filter events that happened in the if/else block
        // println!("split {}, end_if {}, merge {}", split, if_end, merge);
        let if_ev: Vec<(usize, ExternalEvent)> = self.preprocessed_events.iter().filter(|i| self.filter_ev(i, split, if_end)).cloned().collect();
        let else_ev: Vec<(usize, ExternalEvent)> = self.preprocessed_events.iter().filter(|i| self.filter_ev(i, if_end, merge)).cloned().collect();
        self.preprocessed_events.retain(|(l, _)| 
          if *l <= merge && *l >= split {
            false
          }
          else {
            true
          }
        );
        let if_map = rustviz_lib::data::create_line_map(&if_ev);
        let else_map = create_line_map(&else_ev);
        // add if/else branch
        let b_ty = BranchType::If(vec!["If".to_owned(), "Else".to_owned()], vec![(split, if_end), (if_end, merge)]);
        self.add_external_event(line_num, 
          ExternalEvent::Branch { 
            live_vars: liveness, 
            branches: vec![ExtBranchData{ e_data: if_ev, line_map: if_map}, 
                          ExtBranchData {e_data: else_ev, line_map: else_map}], 
            branch_type:b_ty, 
            split_point: split, 
            merge_point: merge });
        // println!("poopoo eve {:#?}", self.preprocessed_events);  
      }

      ExprKind::Loop(block, _, loop_ty, span) => {
        println!("loop ty: {:#?}", loop_ty);
        println!("loop block : {:#?}", block);

        match loop_ty {
          rustc_hir::LoopSource::While => {
            match block.expr {
              Some(e) => {
                self.visit_expr(e);
              } 
              None => {


              }
            }
          }
          _ => {}
        }
      }

      // EXTREMELY SCUFFED - WILL NOT WORK FOR BASICALLY EVERY CASE, will have to come back and think of a better solution
      ExprKind::Match(guard_expr, arms, source) => {
        self.visit_expr(guard_expr);
        let parent = match self.get_rap(guard_expr) { // TODO: this will fail with matching tuples: ex match (a, b, c)
          ResourceTy::Value(ResourceAccessPoint::Function(_)) => ResourceTy::Anonymous,
          p => p
        };
        let typeck_res = self.tcx.typeck(guard_expr.hir_id.owner);
        let parent_ty = typeck_res.node_type(guard_expr.hir_id);
        let parent_ev = if parent_ty.is_ref() {
          if bool_of_mut(parent_ty.ref_mutability().unwrap()) { Evt::MBorrow }
          else { Evt::SBorrow }
        }
        else {
          if parent_ty.is_copy_modulo_regions(self.tcx, self.tcx.param_env(guard_expr.hir_id.owner)) { Evt::Copy }
          else { Evt::Move }
        };

        let split = self.tcx.sess.source_map().lookup_char_pos(guard_expr.span.hi()).line; // TODO: might need to alter this
        let merge = self.tcx.sess.source_map().lookup_char_pos(expr.span.hi()).line;

        let mut b_ty_names: Vec<String> = Vec::new();
        let mut b_slices: Vec<(usize, usize)> = Vec::new();
        let mut branch_data: Vec<ExtBranchData> = Vec::new();
        let mut liveness: HashSet<ResourceAccessPoint> = self.get_live_of_expr(guard_expr);

        
        match source {
          rustc_hir::MatchSource::Normal => {
            for arm in arms.iter() {
              // get line info
              let begin = self.tcx.sess.source_map().lookup_char_pos(arm.body.span.lo()).line;
              let end = self.tcx.sess.source_map().lookup_char_pos(arm.body.span.hi()).line;
              let pat_line = self.span_to_line(&arm.pat.span);
              
              // add/fetch raps that are initialized in arm expr 
              let pat_decls = self.get_decl_of_pat(&arm.pat, typeck_res, &parent, end);

              // visit expr
              self.visit_expr(arm.body);

              // get liveness info
              liveness = liveness.union(&self.get_live_of_expr(arm.body)).cloned().collect();
              liveness = liveness.difference(&pat_decls).cloned().collect(); // subtract decls from live variables

              // get name of constructor
              let constructor_name = self.get_name_of_pat(arm.pat);
              let cons_rap = match self.raps.get(&constructor_name) {
                Some((r, _)) => r.clone(),
                None => {
                  self.add_fn(constructor_name.clone());
                  self.raps.get(&constructor_name).unwrap().0.clone()
                }
              };

              // get event info
              let mut branch_e_data: Vec<(usize, ExternalEvent)> = Vec::new();
              // append event that happens when matching
              branch_e_data.push((pat_line, self.ext_ev_of_evt(parent_ev.clone(), ResourceTy::Value(cons_rap), parent.clone())));

              for v in pat_decls.iter() { // will need to append events at the end of the branch that return resources
                self.add_external_event(pat_line, ExternalEvent::Bind { from: ResourceTy::Anonymous, to: ResourceTy::Value(v.clone()) });
              }

              let temp: Vec<(usize, ExternalEvent)> = self.preprocessed_events.iter().filter(|i| self.filter_ev_match(i, begin, end, &liveness)).cloned().collect();
              for elem in temp {
                branch_e_data.push(elem);
              }

              // append raps that are declared inside the block (if there is one)
              let mut var_decls = pat_decls.clone();         
              match arm.body.kind {
                ExprKind::Block(b, _) => {
                  var_decls = var_decls.union(&self.get_decl_of_block(b)).cloned().collect();
                }
                _ => {}
              }

              b_slices.push((begin, end));
              b_ty_names.push(self.get_name_of_pat(arm.pat));


              // for each var decl append a GoOutOfScope
              for v in var_decls.iter() {
                self.add_external_event(end, ExternalEvent::GoOutOfScope { ro: v.clone() })
              }

              branch_e_data.push((end, self.ext_ev_of_evt(parent_ev.clone(), ResourceTy::Anonymous, parent.clone())));
              let branch_line_map = create_line_map(&branch_e_data);

              branch_data.push(ExtBranchData { e_data: branch_e_data, line_map: branch_line_map });
            }

            // filter events from main events
            self.preprocessed_events.retain(|(l, e)| 
              if *l <= merge && *l >= split {
                let (from, to) = ResourceAccessPoint_extract(e);
                let ev_is_live = match (from.extract_rap(), to.extract_rap()) {
                  (Some(x), Some(y)) => {
                    liveness.contains(x) || liveness.contains(y)
                  }
                  (Some(x), _) | (_, Some(x)) => {
                    liveness.contains(x)
                  }
                  _ => false
                };
                !ev_is_live
              }
              else {
                true
              }
            );
            // add branch event
            self.add_external_event(split, ExternalEvent::Branch { 
              live_vars: liveness, 
              branches: branch_data, 
              branch_type: BranchType::Match(b_ty_names, b_slices), 
              split_point: split, 
              merge_point: merge });
          }
          _ => {}
        }
      }
      
      _ => {
        intravisit::walk_expr(self, expr);
      }
    }
  }
  fn visit_stmt(&mut self, statement: &'tcx Stmt<'tcx>) {
    match statement.kind {
      StmtKind::Let(ref local) => self.visit_local(local),
      StmtKind::Item(item) => self.visit_nested_item(item),
      StmtKind::Expr(ref expression) | StmtKind::Semi(ref expression) => {
          self.visit_expr(expression)
      }
    }
  }

  // locals are let statements: let <pat>:<ty> = <expr>
  fn visit_local(&mut self, local: &'tcx LetStmt<'tcx>) {
    match local.pat.kind {
      PatKind::Binding(binding_annotation, ann_hirid, ident, op_pat) => {
        let lhs_var:String = ident.to_string();
        let tycheck_results = self.tcx.typeck(ann_hirid.owner);
        let lhs_ty = tycheck_results.node_type(ann_hirid);
        let is_copyable = lhs_ty.is_copy_modulo_regions(self.tcx, self.tcx.param_env(local.hir_id.owner));
        let e = if lhs_ty.is_ref() {
          match lhs_ty.ref_mutability().unwrap() {
            Mutability::Not => Evt::Copy,
            Mutability::Mut => Evt::Move,
          }
        } else {
          match is_copyable {
            true => Evt::Copy, 
            false => Evt::Move
          }
        };
        match local.init { // init refers to RHS of let
          Some(expr) => {
            self.visit_expr(expr);
            self.define_lhs(lhs_var.clone(), bool_of_mut(binding_annotation.1), expr, lhs_ty);
            self.match_rhs(ResourceTy::Value(self.raps.get(&lhs_var).unwrap().0.to_owned()), expr, e);
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
    //walk_list!(self, visit_ty, &local.ty);
  }
} 