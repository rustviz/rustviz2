// Some headers
use rustc_hir::{StmtKind, Stmt, Expr, ExprKind, UnOp, Param, QPath, PatKind, Mutability, LetStmt, def::*};
// use rustc_ast::walk_list;
use rustc_hir::intravisit::{self, Visitor};
use crate::expr_visitor::*;
use crate::expr_visitor_utils::{bool_of_mut, match_op};
use rustviz_lib::data::*;
use rustc_middle::{
  mir::Location,
  ty::{TyCtxt,Ty, adjustment::*},
};
use core::borrow;
use std::collections::{BTreeMap, HashMap, HashSet, VecDeque};
use rustc_utils::mir::{borrowck_facts::get_body_with_borrowck_facts, body::BodyExt, place::PlaceExt};
use rustc_utils::source_map::spanner::*;
use rustc_utils::SpanExt;
use rustc_utils::mir::location_or_arg::LocationOrArg;
use rustc_borrowck::{
  borrow_set::{BorrowData, BorrowSet},
  consumers::{BodyWithBorrowckFacts, RichLocation, RustcFacts}
};
use polonius_engine::{Algorithm, AllFacts, Output, FactTypes};
type Loan = <RustcFacts as FactTypes>::Loan;
type Point = <RustcFacts as FactTypes>::Point;


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
    let borrow_data = get_body_with_borrowck_facts(self.tcx, body.value.hir_id.owner.def_id);
    let borrow_set = &borrow_data.borrow_set;
    let location_map = &borrow_set.location_map;
    println!("body string {}", borrow_data.body.to_string(self.tcx).unwrap());
    println!("name map {:#?}", borrow_data.body.debug_info_name_map());
    println!("local map {:#?}", borrow_set.local_map);
    println!("location map {:#?}", borrow_set.location_map);
    // println!("location table {:#?}", borrow_data.location_table);
    // let spanner = Spanner::new(self.tcx, body.id(), &borrow_data.body);
    fn point_to_location(p: Point, body_with_facts: &BodyWithBorrowckFacts) -> Location {
      match body_with_facts.location_table.as_ref().unwrap().to_location(p) {
        RichLocation::Start(s) | RichLocation::Mid(s) => s
      }
    }

    fn location_to_line(l: Location, body_with_facts: &BodyWithBorrowckFacts, tcx: &TyCtxt) -> usize {
      let mut span = body_with_facts.body.source_info(l).span;
      span = span.as_local(body_with_facts.body.span).unwrap_or(span);
      tcx.sess.source_map().lookup_char_pos(span.lo()).line
    }


    match &borrow_data.input_facts {
      Some(i_facts) => {
        let p_output = Output::compute(&**i_facts, Algorithm::Naive, true);
        //println!("loans live at {:#?}", p_output.loan_live_at);
        let mut loan_regions: HashMap<Loan, (usize, usize)> = HashMap::new();
        p_output.loan_live_at.iter().for_each(|(point, loans)| {
          loans.iter().for_each(|loan| {
            loan_regions.entry(*loan).and_modify(|(l1, l2)|{
              let l = point_to_location(*point, borrow_data);
              let line = location_to_line(l, borrow_data, &self.tcx);
              if line < *l1 { *l1 = line }
              else if line > *l2 { *l2 = line}
            })
            .or_insert_with(|| {
              let l = point_to_location(*point, borrow_data);
              let line = location_to_line(l, borrow_data, &self.tcx);
              (line, line)
            });
          });
        });
        println!("loan regions {:#?}", loan_regions);
        // println!("origin contains loan at {:#?}", p_output.origin_contains_loan_at);
        // println!("loans issued at {:#?}", i_facts.loan_issued_at);
        // println!("loans killed at {:#?}", i_facts.loan_killed_at);
        for (region, b_idx, location_idx) in i_facts.loan_issued_at.iter() {
          println!("loan issued at {:#?}", (region, b_idx, location_idx));
          let location = match borrow_data.location_table.as_ref().unwrap().to_location(*location_idx) {
            RichLocation::Start(s) | RichLocation::Mid(s) => s
          };
          match location_map.get(&location) {
            Some(b_data) => {
              println!("borrow_data for location {:#?} : {:#?}", location, b_data);
              let b_place = b_data.borrowed_place.local_or_deref_local();
              let a_place = b_data.assigned_place.local_or_deref_local();
              println!("borrowed_place: {:#?}, as local: {:#?}", b_data.borrowed_place.to_string(self.tcx, &borrow_data.body), b_place);
              println!("is source visible? {}", b_data.borrowed_place.is_source_visible(self.tcx, &borrow_data.body));
              println!("region inference context {:#?}", borrow_data.region_inference_context.var_infos.get(*region).unwrap());
              if b_place.is_some() { 
                let b_loc = borrow_data.body.local_decls.get(b_place.unwrap()).unwrap();
                println!("local decl {:#?}", b_loc); 
                println!("span {}", b_loc.source_info.span.to_string(self.tcx));
              }
              println!("assigned place {:#?}, as local {:#?}", b_data.assigned_place.to_string(self.tcx, &borrow_data.body), a_place);
              println!("is source visible? {}", b_data.assigned_place.is_source_visible(self.tcx, &borrow_data.body));
              if a_place.is_some() { 
                let a_loc =  borrow_data.body.local_decls.get(a_place.unwrap()).unwrap();
                println!("local decl {:#?}", a_loc);
                println!("span {}", a_loc.source_info.span.to_string(self.tcx));
              }
              // let expr = self.tcx.hir().expect_expr(borrow_data.body.location_to_hir_id(location.clone()));
              // println!("line {}\n\n", self.span_to_line(&expr.span));
            }
            None => { println!("no borrow data found for location"); }
          }
        }

        for (b_idx, location_idx) in i_facts.loan_killed_at.iter() {
          println!("loan {:#?} killed at {:#?}", b_idx, location_idx);

          let location = match borrow_data.location_table.as_ref().unwrap().to_location(*location_idx) {
            RichLocation::Start(s) | RichLocation::Mid(s) => s
          };
          let line = location_to_line(location, &borrow_data, &self.tcx);
          println!("line {}", line);
          // match location_map.get(&location) {
          //   Some(b_data) => {
          //     println!("borrow_data for location {:#?} : {:#?}", location, b_data);
          //     println!("borrowed_place: {:#?}", b_data.borrowed_place.to_string(self.tcx, &borrow_data.body));
          //     println!("assigned place {:#?}", b_data.assigned_place.to_string(self.tcx, &borrow_data.body));
              
          //   }
          //   None => { 
          //     println!("no borrow data found for location");
          //     let spans = spanner.location_to_spans(LocationOrArg::Location(location), &borrow_data.body, EnclosingHirSpans::Full);
          //     for span in spans.iter() {
          //       println!("line {}", self.span_to_line(&span));
          //     }
          //   }
          // }
        }

        // for location in borrow_data.body.all_locations() {
        //   match location_map.get(&location) {
        //     Some(b_data) => { 
        //       println!("borrow_data for location {:#?} : {:#?}", location, b_data);
        //       println!("borrowed_place: {:#?}", b_data.borrowed_place.to_string(self.tcx, &borrow_data.body));
        //       println!("assigned place {:#?}", b_data.assigned_place.to_string(self.tcx, &borrow_data.body));
        //       let borrow_idx = b_data.region;
        //       let 
        //       let spans = spanner.location_to_spans(LocationOrArg::Location(location), &borrow_data.body, EnclosingHirSpans::Full);
        //       println!("spans {:#?}", spans);
        //       println!("expr {:#?}", self.tcx.hir().expect_expr(borrow_data.body.location_to_hir_id(location.clone())));
        //     }
        //     None => {  }
        //   }
        // }
      }
      _ => {
        
      }
    }
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
          Some(e) => { // only append this event if parent fn ctxt doesn't return void
            if self.fn_ret {
              let tycheck_results = self.tcx.typeck(e.hir_id.owner);
              let lhs_ty = tycheck_results.node_type(e.hir_id);
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
              self.add_ev(line_num, evt, to_ro, from_ro, false);
            }
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
          ResourceTy::Anonymous, VecDeque::new(), self.current_scope, !self.inside_branch);
        }
        else if ty.is_adt() && !self.ty_is_special_owner(ty){ // kind of weird given we don't have a InitStructParam
          let owner_hash = self.rap_hashes as u64;
          self.add_struct(name.clone(), owner_hash, false, bool_of_mut(binding_annotation.1), self.current_scope, !self.inside_branch);
          for field in ty.ty_adt_def().unwrap().all_fields() {
            let field_name = format!("{}.{}", name.clone(), field.name.as_str());
            self.add_struct(field_name, owner_hash, true, bool_of_mut(binding_annotation.1), self.current_scope, !self.inside_branch);
          }
        }
        else {
          self.add_owner(name.clone(), bool_of_mut(binding_annotation.1), self.current_scope, !self.inside_branch);
        }
        self.add_external_event(line_num, ExternalEvent::InitRefParam { param: self.raps.get(&name).unwrap().rap.to_owned(), id: *self.unique_id });
        *self.unique_id += 1;
        self.annotate_src(name.clone(), ident.span, false, *self.raps.get(&name).unwrap().rap.hash());
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
                    // println!("fn expr {:#?}", fn_expr);
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
        // println!("path {:#?}", name_and_generic_args);
        // println!("rcvr {:#?}", rcvr);
        // println!("args {:#?}", args);
        let fn_name = name_and_generic_args.ident.as_str().to_owned();
        self.add_fn(fn_name.clone());
        // need to recurse down to the 
        self.visit_expr(rcvr);
        match rcvr.kind {
          ExprKind::MethodCall(p_seg, ..) => {
            let rcvr_name = p_seg.ident.as_str().to_owned();
            // let fn_ty = self.tcx.typeck(name_and_generic_args.hir_id.owner).node_type_opt(p_seg.hir_id);
            // let is_copy = self.is_return_type_copyable(rcvr);
            // println!("is copy {}", is_copy);
            // println!("fn_ty {:#?}", fn_ty);
            return;
          }
          _ => {}
        }
        let rcvr_name = self.hirid_to_var_name(rcvr.hir_id).unwrap();
        // println!("raps {:#?}", self.raps);
        // println!("rcvr name {}", rcvr_name);
        let rcvr_rap = self.raps.get(&rcvr_name).unwrap().rap.to_owned();
        self.update_rap(&rcvr_rap, line_num);
        let fn_rap = self.raps.get(&fn_name).unwrap().rap.to_owned();
        // typecheck the whole function body
        let adjustment_map = self.tcx.typeck(name_and_generic_args.hir_id.owner).adjustments();
        match adjustment_map.get(rcvr.hir_id) {
          Some(adj_vec) => {
            for a in adj_vec.iter() {
              match a.kind {
                Adjust::Borrow(AutoBorrow::Ref(_, m)) => {
                  match m {
                    AutoBorrowMutability::Mut{allow_two_phase_borrow: AllowTwoPhase::Yes} => {
                      self.add_ev(line_num, Evt::PassByMRef, ResourceTy::Value(fn_rap.clone()), ResourceTy::Value(rcvr_rap.clone()), false);
                    },
                    AutoBorrowMutability::Not => {
                      self.add_ev(line_num, Evt::PassBySRef, ResourceTy::Value(fn_rap.clone()), ResourceTy::Value(rcvr_rap.clone()), false);
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
        let lhs_rap = self.raps.get(&lhs_rty.real_name()).unwrap().rap.clone();
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
                self.add_external_event(line_num, ExternalEvent::RefDie { from: lhs_rty.clone(), to: to_ro, num_curr_borrowers: borrowers.len() - 1, id: *self.unique_id });
                *self.unique_id += 1;
              }
              else {
                match ref_data.ref_mutability {
                  true => self.add_ev(line_num, Evt::MDie, to_ro, lhs_rty.clone(), false),
                  false => self.add_ev(line_num, Evt::SDie, to_ro, lhs_rty.clone(), false)
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
                self.add_external_event(line_num, ExternalEvent::RefDie { from: lhs_rty.clone(), to: to_ro, num_curr_borrowers: borrowers.len() - 1, id: *self.unique_id });
                *self.unique_id += 1;
              }
              else {
                match modified_ref_data.ref_mutability {
                  true => self.add_ev(line_num, Evt::MDie, to_ro, lhs_rty.clone(), false),
                  false => self.add_ev(line_num, Evt::SDie, to_ro, lhs_rty.clone(), false)
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
        let r = &self.raps.get(&name).unwrap().rap.clone();
        let line_num = self.span_to_line(&p.span);
        self.update_rap(r, line_num);
      }
      ExprKind::DropTemps(exp) => {
        self.visit_expr(exp);
      }
      ExprKind::If(guard_expr, if_expr, else_expr) => {
        self.visit_expr(&guard_expr);
        self.inside_branch = true;
        self.visit_expr(&if_expr);
        let (else_live, else_decl) = match else_expr {
          Some(e) => {
            self.visit_expr(e);
            (self.get_live_of_expr(e), self.get_decl_of_expr(e))
          }
          None => { (HashSet::new(), HashSet::new()) }
        };
        self.inside_branch = false;
        // compute liveness
        let if_live = self.get_live_of_expr(if_expr);
        let if_decl = self.get_decl_of_expr(if_expr);
        let liveness:HashSet<ResourceAccessPoint> = if_live.union(&else_live).cloned().collect();

        // compute split and merge points
        let line_num = self.expr_to_line(&guard_expr);
        let split = self.tcx.sess.source_map().lookup_char_pos(if_expr.span.lo()).line;
        let mut if_end = self.tcx.sess.source_map().lookup_char_pos(if_expr.span.hi()).line;
        let merge = match else_expr {
          Some(e) => self.tcx.sess.source_map().lookup_char_pos(e.span.hi()).line,
          None => self.tcx.sess.source_map().lookup_char_pos(if_expr.span.hi()).line
        };

        // filter events that happened in the if/else block
        // println!("split {}, end_if {}, merge {}", split, if_end, merge);
        let mut if_ev: Vec<(usize, ExternalEvent)> = self.preprocessed_events.iter().filter(|i| self.filter_ev(i, split, if_end)).cloned().collect();
        let mut else_ev: Vec<(usize, ExternalEvent)> = self.preprocessed_events.iter().filter(|i| self.filter_ev(i, if_end, merge)).cloned().collect();
        self.preprocessed_events.retain(|(l, _)| 
          if *l <= merge && *l >= split {
            false
          }
          else {
            true
          }
        );

        // add gos events 
        for var in if_decl.iter() {
          if_ev.push((if_end, ExternalEvent::GoOutOfScope { ro: var.clone(), id: *self.unique_id }));
          *self.unique_id += 1;
        }

        for var in else_decl.iter() {
          else_ev.push((merge, ExternalEvent::GoOutOfScope { ro: var.clone(), id: *self.unique_id }));
          *self.unique_id += 1;
        }

        
        let if_map = create_line_map(&if_ev);
        let else_map = create_line_map(&else_ev);
        if if_end != merge { if_end += 1; }
        // add if/else branch
        let b_ty = BranchType::If(vec!["If".to_owned(), "Else".to_owned()], vec![(split + 1, if_end), (if_end, merge)]);
        self.add_external_event(line_num, 
          ExternalEvent::Branch { 
            live_vars: liveness, 
            branches: vec![ExtBranchData{ e_data: if_ev, line_map: if_map, decl_vars: if_decl }, 
                          ExtBranchData { e_data: else_ev, line_map: else_map, decl_vars: else_decl }], 
            branch_type: b_ty, 
            split_point: split, 
            merge_point: merge,
            id: *self.unique_id });
            *self.unique_id += 1;
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

      // match <expr> {
      // <pat> => <expr>
      // }
      ExprKind::Match(guard_expr, arms, source) => {
        // first visit the guard expression, annotate any events that happen there
        self.visit_expr(guard_expr);
        let typeck_res = self.tcx.typeck(expr.hir_id.owner);

        // To my knowledge a match has to either contain a singular expression or Tuple
        // get all the 'parents' ie things being matched on and their types
        let (parents, parents_ty) = match guard_expr.kind {
          ExprKind::Tup(fields) => {
            let mut res = Vec::new();
            let mut res_ty = Vec::new();
            for field in fields.iter() {
              res.push(self.get_rap(&field));
              res_ty.push(typeck_res.node_type(field.hir_id));
            }
            (res, res_ty)
          }
          _ => {
            (vec![self.get_rap(&guard_expr)], vec![typeck_res.node_type(guard_expr.hir_id)])
          }
        };
        let typeck_res = self.tcx.typeck(guard_expr.hir_id.owner);

        let split = self.tcx.sess.source_map().lookup_char_pos(guard_expr.span.hi()).line; // TODO: might need to alter this
        let merge = self.tcx.sess.source_map().lookup_char_pos(expr.span.hi()).line;

        let mut b_ty_names: Vec<String> = Vec::new();
        let mut b_slices: Vec<(usize, usize)> = Vec::new();
        let mut branch_data: Vec<ExtBranchData> = Vec::new();
        let mut liveness: HashSet<ResourceAccessPoint> = self.get_live_of_expr(guard_expr);

        
        match source {
          rustc_hir::MatchSource::Normal => {
            for arm in arms.iter() {
              let mut branch_e_data: Vec<(usize, ExternalEvent)> = Vec::new();
              let mut callback_events: Vec<(ResourceTy, ResourceTy, Evt)> = Vec::new();

              // get line info
              let begin = self.tcx.sess.source_map().lookup_char_pos(arm.body.span.lo()).line;
              let end = self.tcx.sess.source_map().lookup_char_pos(arm.body.span.hi()).line;
              // println!("begin {} | end {}", begin, end);
              let pat_line = self.span_to_line(&arm.pat.span);
              b_slices.push((begin, end));
              b_ty_names.push(self.get_name_of_pat(arm.pat));

              
              // add/fetch raps that are initialized in arm expr
              // need to also get their types, in order to figure out if we are moving, copying or borrowing something into the block
              let mut pat_decls: HashSet<ResourceAccessPoint> = HashSet::new();
              // println!("arm pat {:#?}", arm.pat);
              match arm.pat.kind {
                // (<pat>, <pat>, ..) => <expr>
                PatKind::TupleStruct(_, pat_list, _) | PatKind::Tuple(pat_list, _)=> {
                  for (i, p) in pat_list.iter().enumerate() {
                    let mut associated_ro = Vec::new();
                    self.get_dec_of_pat2(p, &typeck_res, &parents[i], &parents_ty[i], end, & mut associated_ro);
                    println!("associated ro {:#?}", associated_ro);
                    let temp: Vec<ResourceAccessPoint> = associated_ro.iter().map(|(r, _, _)| {r.clone()}).collect();
                    let temp2: HashSet<ResourceAccessPoint> = temp.into_iter().collect();
                    pat_decls.extend(temp2);
                    for (to_ro, e, parent_ty) in associated_ro.iter() {
                      // TODO: fix the is_partial logic
                      // let is_partial;
                      let is_partial = !(*parent_ty == typeck_res.node_type(p.hir_id));
                      branch_e_data.push((pat_line, self.ext_ev_of_evt(e.clone(), ResourceTy::Value(to_ro.clone()), parents[i].clone(), *self.unique_id, is_partial)));
                      *self.unique_id += 1;
                      match e {
                        Evt::SBorrow => {
                          callback_events.push((parents[i].clone(), ResourceTy::Value(to_ro.clone()), Evt::SDie));
                        }
                        Evt::MBorrow => {
                          callback_events.push((parents[i].clone(), ResourceTy::Value(to_ro.clone()), Evt::MDie));
                        }
                        _ => {}
                      }
                    }
                  }
                }
                // <expr> => <expr> (just a singleton variable)
                _ => {
                  for i in 0..parents.len() {
                    let mut associated_ro = Vec::new();
                    self.get_dec_of_pat2(arm.pat, &typeck_res, &parents[i], &parents_ty[i], end, & mut associated_ro);
                    let temp: Vec<ResourceAccessPoint> = associated_ro.iter().map(|(r, _, _)| {r.clone()}).collect();
                    let temp2: HashSet<ResourceAccessPoint> = temp.into_iter().collect();
                    pat_decls.extend(temp2);
                    for (to_ro, e, _) in associated_ro.iter() {
                      branch_e_data.push((pat_line, self.ext_ev_of_evt(e.clone(), ResourceTy::Value(to_ro.clone()),parents[i].clone(), *self.unique_id, false)));
                      *self.unique_id += 1;
                      match e {               // A borrow, mut/immut must be returned at the end of the block
                        Evt::SBorrow => {
                          callback_events.push((parents[i].clone(), ResourceTy::Value(to_ro.clone()), Evt::SDie));
                        }
                        Evt::MBorrow => {
                          callback_events.push((parents[i].clone(), ResourceTy::Value(to_ro.clone()), Evt::MDie));
                        }
                        _ => {}
                      }
                    }
                  }
                }
              }

              
              // println!("branch events {:#?}", branch_e_data);
              // println!("callbacks {:#?}", callback_events);
              // println!("ro to partials {:#?}", ro_to_partials);
              // let pat_decls = self.get_decl_of_pat(&arm.pat, typeck_res, &parent, end);
              // annotate those events 
              // a move/copy is simple (if any of the patterns have type move/copy, a move/copy occurs)
              // A borrow, mut/immut must be returned at the end of the block

              // Should think about introducing partial moves, copies, borrows, etc (where a portion of the data is borrowed)

              // visit expr
              self.inside_branch = true;
              self.visit_expr(arm.body);
              self.inside_branch = false;

              // update liveness info
              liveness = liveness.union(&self.get_live_of_expr(arm.body)).cloned().collect();
              liveness = liveness.difference(&pat_decls).cloned().collect();
              let arm_decls: HashSet<ResourceAccessPoint> = pat_decls.union(&self.get_decl_of_expr(arm.body)).cloned().collect();


              // get events that occured in the arm
              branch_e_data.extend(
                self.preprocessed_events.iter().filter(|i| self.filter_ev(i, begin, end)).cloned()
              );

              // remove elements from global container
              self.preprocessed_events.retain(|(l, _)| 
                if *l <= end && *l >= begin {
                  false
                }
                else {
                  true
                }
              );

              // add callback events
              for (to, from, e) in callback_events {
                let name = from.real_name();
                self.borrow_map.remove(&name);
                branch_e_data.push((end, self.ext_ev_of_evt(e, to, from, *self.unique_id, false)));
                *self.unique_id += 1;
              }

              // add gos events
              for r in arm_decls.iter() {
                branch_e_data.push((end, ExternalEvent::GoOutOfScope { ro: r.clone(), id: *self.unique_id }));
                *self.unique_id += 1;
              }

              // branch_e_data.push((end, self.ext_ev_of_evt(parent_ev.clone(), ResourceTy::Anonymous, parent.clone())));
              let branch_line_map = create_line_map(&branch_e_data);

              branch_data.push(ExtBranchData { e_data: branch_e_data, line_map: branch_line_map, decl_vars: arm_decls});
            }
            // add branch event
            self.add_external_event(split, ExternalEvent::Branch { 
              live_vars: liveness, 
              branches: branch_data, 
              branch_type: BranchType::Match(b_ty_names, b_slices), 
              split_point: split, 
              merge_point: merge - 1, // TODO: fix
              id: *self.unique_id });
            *self.unique_id += 1;
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
            self.match_rhs(ResourceTy::Value(self.raps.get(&lhs_var).unwrap().rap.to_owned()), expr, e);
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