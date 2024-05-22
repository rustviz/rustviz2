// Some headers
use rustc_hir::{StmtKind, Stmt, Local, Expr, ExprKind, UnOp, Param, QPath, PatKind, Mutability};
use rustc_ast::walk_list;
use rustc_hir::intravisit::{self, Visitor};
use crate::expr_visitor::{ExprVisitor, AccessPoint, AccessPointUsage, Reference};
use crate::expr_visitor_utils::match_op;
use rustc_middle::{
  mir::Body,
  ty::{TyCtxt,Ty, adjustment::{Adjust, AutoBorrowMutability, AutoBorrow}},
};


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
      PatKind::Binding(binding_annotation, ann_hirid, ident, op_pat) =>{
        let name: String = ident.to_string();
        self.annotate_src(name.clone(), ident.span, false);
        let mutability = binding_annotation.1;
        self.mutability_map.insert(name.clone(), mutability);
        if ty.is_ref() {
          self.add_event(line_num,format!("InitRefParam({})",name));
          if let Some(mutref)=ty.ref_mutability(){
            match mutref {
              Mutability::Not=>{
                self.update_lifetime(Reference::Static(name.clone()), line_num);
                self.add_access_point(AccessPointUsage::StaticRef(AccessPoint { mutability, name: name.clone(), members: None}), name.clone());
              }
              Mutability::Mut=>{
                self.update_lifetime(Reference::Mut(name.clone()), line_num);
                self.add_access_point(AccessPointUsage::MutRef(AccessPoint { mutability, name: name.clone(), members: None}), name.clone());
              }
            }
            self.borrow_map.insert(name, None); // ref parameters don't have an underlying owner they are borrowing from
          }
        }
        else{
          self.add_event(line_num,format!("InitOwnerParam({})",name));
          self.add_access_point(AccessPointUsage::Owner(AccessPoint { mutability, name: name.clone(), members: None}), name);
        }
      }
      _=>{}
    }
  }
  fn visit_expr(&mut self, expr: &'tcx Expr<'tcx>) {
      let line_num = self.expr_to_line(expr);
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
                                  let arg_line = self.span_to_line(&arg.span);
                                  self.match_args(arg_line, &arg, fn_name.clone());
                                }
                              }
                              _ => {}
                            }
                          }
                        }
                      _ => {}
                    }
                  }
                  _ => {}
                }
              }
              _ => {
                let fn_name: String = self.hirid_to_var_name(fn_expr.hir_id).unwrap();
                self.annotate_src(fn_name.clone(), fn_expr.span, true);
                for arg in args.iter(){
                  self.match_args(line_num, &arg, fn_name.clone());
                }
              }
            }
          }
          ExprKind::MethodCall(name_and_generic_args, rcvr, args, _) => {
            // println!("typeck res: {:#?}", self.tcx.typeck(name_and_generic_args.hir_id.owner));
            // println!("item local id {:#?}", name_and_generic_args.hir_id.local_id);
            // println!("caller local id {:#?}", rcvr.hir_id.local_id);
            let rcvr_name = self.hirid_to_var_name(rcvr.hir_id).unwrap();
            let fn_name =self.hirid_to_var_name(name_and_generic_args.hir_id).unwrap();
            // typecheck the whole function body
            let adjustment_map = self.tcx.typeck(name_and_generic_args.hir_id.owner).adjustments();
            match adjustment_map.get(rcvr.hir_id) {
              Some(adj_vec) => {
                for a in adj_vec.iter() {
                  match a.kind {
                    Adjust::Borrow(AutoBorrow::Ref(_, m)) => {
                      match m {
                        AutoBorrowMutability::Mut{..} => {
                          self.add_event(line_num,format!("PassByMutableReference({}->{}())", rcvr_name, fn_name.clone()));
                        },
                        AutoBorrowMutability::Not => {
                          self.add_event(line_num,format!("PassByStaticReference({}->{}())", rcvr_name, fn_name.clone()));
                        }
                      }
                    }
                    _ => {}
                  }
                }
              }
              None => {}
            }
  
            self.annotate_src(rcvr_name.clone(), rcvr.span, false);
            for arg in args.iter(){
              self.match_args(line_num, &arg, fn_name.clone());
            }
          }
          ExprKind::Binary(binop, expra, exprb) => {
            // // define operator as function
            // let op_of_string: String = match_op(binop.node);
            // // treat expra and exprb as parameters to the (OP) function
            // self.match_args(line_num, expra, op_of_string.clone());
            // self.match_args(line_num, exprb, op_of_string);
          }
    
          ExprKind::AddrOf(_, _, inner) if inner.is_syntactic_place_expr() && !inner.span.from_expansion() => {}

          ExprKind::Assign(lhs,rhs, _,) => {
            self.visit_expr(rhs); // visit rhs side first to order moves?
            match lhs.kind {
              ExprKind::Path(QPath::Resolved(_,p)) => { // assigning to a mutable variable
                let lhs_var = self.tcx.hir().name(p.segments[0].hir_id).as_str().to_owned();
                self.annotate_src(lhs_var.clone(), p.segments[0].ident.span, false);
                if let Some(mutability)=self.mutability_map.get(&lhs_var){
                  self.match_rhs(AccessPoint { mutability:*mutability, name: lhs_var, members: None }, rhs, false);
                }
              },
              ExprKind::Unary(rustc_hir::UnOp::Deref, deref_expr) => { // assigning to a mutable ref
                match deref_expr.kind {
                  ExprKind::Path(QPath::Resolved(_,p)) => {
                    let lhs_var = self.tcx.hir().name(p.segments[0].hir_id).as_str().to_owned();
                    self.annotate_src(lhs_var.clone(), p.segments[0].ident.span, false);
                    self.update_lifetime(Reference::Mut(lhs_var.clone()), line_num);
                    if let Some(mutability)=self.mutability_map.get(&lhs_var){
                      self.match_rhs(AccessPoint { mutability:*mutability, name: lhs_var, members: None }, rhs, true);
                    }
                  }
                  _ => { println!("haven't implemented lhs deref expr") }
                }
              }
              _=>{
                println!("assigning to something other than a variable - hasn't been implemented yet");
              }
            }
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
                // TODO: if function returns void then last expression can not have a semi-colon (and will be double annotated)
                // TODO: update this to a better solution, for now LHS is just an access point with the name "None"
                // this is a scuffed fix in order to adhere to rv1 visualization practices 
                self.match_rhs(AccessPoint {mutability: Mutability::Not, name: "None".to_owned(), members: None}, expr, false);
              }
              None => {}
            }
            // backtrack
            self.current_scope = prev_scope;
          }
          
          // <expr> <op> <expr>
          // ex: a += 1
          ExprKind::AssignOp(_, lhs, rhs) => {
            self.visit_expr(rhs);
            match lhs.kind {
              ExprKind::Path(QPath::Resolved(_,p)) => { // assigning to a mutable variable
                let lhs_var = self.tcx.hir().name(p.segments[0].hir_id).as_str().to_owned();
                self.annotate_src(lhs_var.clone(), p.segments[0].ident.span, false);
                if let Some(mutability)=self.mutability_map.get(&lhs_var){
                  self.match_rhs(AccessPoint { mutability:*mutability, name: lhs_var, members: None }, rhs, false);
                }
              },
              ExprKind::Unary(rustc_hir::UnOp::Deref, deref_expr) => { // assigning to a mutable ref
                match deref_expr.kind {
                  ExprKind::Path(QPath::Resolved(_,p)) => {
                    let lhs_var = self.tcx.hir().name(p.segments[0].hir_id).as_str().to_owned();
                    self.annotate_src(lhs_var.clone(), p.segments[0].ident.span, false);
                    self.update_lifetime(Reference::Mut(lhs_var.clone()), line_num);
                    if let Some(mutability)=self.mutability_map.get(&lhs_var){
                      self.match_rhs(AccessPoint { mutability:*mutability, name: lhs_var, members: None }, rhs, true);
                    }
                  }
                  _ => { println!("haven't implemented lhs deref expr") }
                }
              }
              _=>{
                println!("assigning to something other than a variable - hasn't been implemented yet");
              }
            }
          }

          ExprKind::Unary(UnOp::Deref, inner)
            if inner.is_syntactic_place_expr() && !inner.span.from_expansion() =>
          {
           let hir_id=inner.hir_id;
          }
          
          _ => {
            intravisit::walk_expr(self, expr);
          }
        }
  }
  fn visit_stmt(&mut self, statement: &'tcx Stmt<'tcx>) {
    match statement.kind {
      // locals are let statements: let <pat>:<ty> = <init>
      StmtKind::Local(ref local) => self.visit_local(local),
      StmtKind::Item(item) => self.visit_nested_item(item),
      StmtKind::Expr(ref expression) | StmtKind::Semi(ref expression) => {
          self.visit_expr(expression)
      }
    }
  }
  fn visit_local(&mut self, local: &'tcx Local<'tcx>) {
    match local.pat.kind {
      // A bind is occuring
      // ann_hirid refers to the variable being boun=-0
      PatKind::Binding(binding_annotation, ann_hirid, ident, op_pat) => {
        let lhs_var:String = ident.to_string();
        // annotate left side of let statement
        //println!("lhs name: {}, line: {}, lo: {:#?}, hi: {:#?}", lhs_var.clone(), self.span_to_line(&ident.span), self.tcx.sess.source_map().lookup_char_pos(ident.span.lo()), self.tcx.sess.source_map().lookup_char_pos(ident.span.hi()));
        self.annotate_src(lhs_var.clone(), ident.span, false);
        self.mutability_map.insert(lhs_var.clone(), binding_annotation.1);
        match local.init { // init refers to RHS of let
          | Some(expr) => {
              // self.visit_expr(expr); -- may not be a bad idea to visit rhs expr then match it
              self.match_rhs(AccessPoint { mutability: binding_annotation.1, name: lhs_var, members: None}, expr, false);
              match expr.kind {
                ExprKind::Path(_) => {},
                ExprKind::Block(..)=>{},
                _=>{
                  // if RHS is more than just a path (variable) we need to walk it to possibly append
                  // additionaly events to the line
                  self.visit_expr(expr);
                }
              }
          },
          | _ => {}, // in the case of a declaration ex: let a; nothing happens
          };
      }
      // I don't know when this is necessarily the case
      PatKind::Path(QPath::Resolved(_,p)) => {
        println!("lhs path: {:?}", self.tcx.def_path_str(p.res.def_id()));
      }
      _ => {
        println!("lhs is not listed");
      }
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