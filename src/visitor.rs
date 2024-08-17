//! Most of the work happens here, if you want to learn more about the visitor then look here:  
//! https://doc.rust-lang.org/beta/nightly-rustc/rustc_hir/intravisit/trait.Visitor.html
//! Essentially we recursively traverse (walk) the HIR, visiting statements, expressions, etc
//! See ExprKind at : https://doc.rust-lang.org/stable/nightly-rustc/rustc_hir/hir/enum.ExprKind.html
//! See StmtKind at : https://doc.rust-lang.org/stable/nightly-rustc/rustc_hir/hir/enum.StmtKind.html
//! See tcx: https://doc.rust-lang.org/nightly/nightly-rustc/rustc_middle/ty/struct.TyCtxt.html

use log::{info, warn};
use rustc_hir::{StmtKind, Stmt, Expr, ExprKind, UnOp, Param, QPath, PatKind, Mutability, LetStmt, def::*};
use rustc_hir::intravisit::{self, Visitor};
use crate::expr_visitor::*;
use crate::expr_visitor_utils::*;
use rustviz_lib::data::*;
use rustc_middle::ty::adjustment::*;
use std::collections::{HashSet, VecDeque};


impl<'a, 'tcx> Visitor<'tcx> for ExprVisitor<'a, 'tcx> {
  // A fn body
  fn visit_body(&mut self, body: &'tcx rustc_hir::Body<'tcx>) {
    self.current_scope = self.tcx.sess.source_map().lookup_char_pos(body.value.span.hi()).line;
    for param in body.params {
      self.visit_param(param);
    }

    self.visit_expr(body.value); // visit fn body
    match body.value { // handle return expression if there is one
      Expr{kind: ExprKind::Block(b, _), ..} => {
        match b.expr {
          Some(e) => {
            if self.fn_ret { // only append this event if parent fn ctxt doesn't return void
              // currently this logic would not be able to handle functions with multiple return points
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
              let from_ro = match fetch_rap(e, &self.tcx, &self.raps) {
                Some(r) => ResourceTy::Value(r), // todo, technically need to check for deref here
                None => ResourceTy::Anonymous
              };
              
              let line_num = expr_to_line(e, &self.tcx);
              self.add_ev(line_num, evt, to_ro, from_ro, false);
            }
          }
          _ => {}
        }
      }
      _ => {
        warn!("unexpected fn body {:#?}", body);
      }
    }
    self.annotate_expr(body.value); // then annotate the body
  } 

  // visit parameter of current fn (add them as RAPs)
  fn visit_param(&mut self, param: &'tcx Param<'tcx>){
    // add RAP corresponding to parameter type
    let line_num=span_to_line(&param.span, &self.tcx);
    let ty = self.tcx.typeck(param.hir_id.owner).pat_ty(param.pat);
    let is_special = ty_is_special_owner(&self.tcx, &ty);
    match param.pat.kind {
      PatKind::Binding(binding_annotation, _ann_hirid, ident, _op_pat) =>{
        let name: String = ident.to_string();
        if ty.is_ref() {
          self.add_ref(name.clone(), 
          bool_of_mut(ty.ref_mutability().unwrap()),
          bool_of_mut(binding_annotation.1), line_num, 
          ResourceTy::Anonymous, VecDeque::new(), self.current_scope, !self.inside_branch);
        }
        else if ty.is_adt() && !is_special{ // kind of weird given we don't have a InitStructParam
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
      _ => {}
    }
  }

  fn visit_expr(&mut self, expr: &'tcx Expr<'tcx>) {
    match expr.kind {
      // fn call <expr>[<expr>]
      ExprKind::Call(fn_expr, args) => {

        // need to specifically handle println! macro because it's common
        // note that other macros will need to be resolved similarly (vec![], assert!, etc)
        // Need to match through all the desugaring and onto the args to the format ({}) function
        match fn_expr.kind {
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
                            let fn_name: String = String::from("println!"); // manually overrwrite name
                            for arg in format_args.iter() {
                              self.visit_expr(&arg);
                              self.match_arg(&arg, fn_name.clone());
                            }
                          }
                          _ => {
                            info!("getting here to the println 1");
                          }
                        }
                      }
                    }
                  _ => {
                    let fn_name: String = hirid_to_var_name(fn_expr.hir_id, &self.tcx).unwrap();
                    self.add_fn(fn_name.clone());
                    for arg in a.iter(){
                      self.visit_expr(&arg);
                      self.match_arg(&arg, fn_name.clone());
                    }
                  }
                }
              }
              _ => {
                let fn_name: String = hirid_to_var_name(fn_expr.hir_id, &self.tcx).unwrap();
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
            let fn_name: String = hirid_to_var_name(fn_expr.hir_id, &self.tcx).unwrap();
            self.add_fn(fn_name.clone());
            for arg in args.iter(){
              self.visit_expr(&arg);
              self.match_arg(&arg, fn_name.clone());
            }
          }
        }
      }
      
      // <expr>.<function>([args])
      ExprKind::MethodCall(name_and_generic_args, rcvr, args, _) => {
        let line_num = expr_to_line(&rcvr, &self.tcx);
        let fn_name = name_and_generic_args.ident.as_str().to_owned();
        self.add_fn(fn_name.clone());
        // need to recurse down to the variable calling the methods 
        // necessary for chained method calls scenarios: ie a.get().unwrap()
        self.visit_expr(rcvr);
        match rcvr.kind {
          ExprKind::MethodCall(p_seg, ..) => { // return early if not at the base
            let _rcvr_name = p_seg.ident.as_str().to_owned();
            return;
          }
          _ => {}
        }
        let rcvr_name = hirid_to_var_name(rcvr.hir_id, &self.tcx).unwrap();
        let rcvr_rap = self.raps.get(&rcvr_name).unwrap().rap.to_owned();
        self.update_rap(&rcvr_rap, line_num);
        let fn_rap = self.raps.get(&fn_name).unwrap().rap.to_owned();
        // typecheck
        // Annotate passByRef event, can check the type of borrow by looking at adjusments (usually borrows that are not explicit)
        // for example, in rust you don't have to dereference a reference to access members
        // https://doc.rust-lang.org/nightly/nightly-rustc/rustc_middle/ty/adjustment/struct.Adjustment.html
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
        }
      }
      ExprKind::Binary(_, expra, exprb) => {
        self.visit_expr(expra);
        self.visit_expr(exprb);
      }

      ExprKind::AddrOf(_, _, exp) => {
        self.visit_expr(exp);
      }

      // assignment 
      // ex a = <expr> or a += <expr>
      ExprKind::Assign(lhs_expr, rhs_expr, _,) | ExprKind::AssignOp(_, lhs_expr, rhs_expr) => {
        self.visit_expr(lhs_expr);
        self.visit_expr(rhs_expr);

        // typecheck to figure out what type of event is going to occur
        let line_num = expr_to_line(&lhs_expr, &self.tcx);
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
        // if we are pointing at a new piece of data
        if lhs_ty.is_ref() {
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
              let borrowers = get_borrowers(&lhs_rty.real_name(), &self.borrow_map);
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

              // Add event for possible lender if necessary
              // set possible lender to None

              // check for a match in MIR
              // let mir_b_data = self.gather_borrow_data(&self.bwf);
              // for m_data in mir_b_data.iter() {
              //   match ExprVisitor::borrow_match(&ref_data, m_data) {
              //     Some(kill) => {
              //       if kill > line_num { // this loan needs to be extended 
                      
              //       }
              //       break;
              //     }
              //     None => {}
              //   }
              // }

              // update lhs_rty with new lender information
              let (new_lender, new_aliasing) = self.get_ref_data(&rhs_expr);
              let r = self.borrow_map.get_mut(&lhs_rty.real_name()).unwrap();
              r.assigned_at = line_num;
              r.aliasing = new_aliasing;
              r.lender = new_lender;
            }

            // ex:
            // let a = & mut b (where b is &i32)
            // *a = &c (where c is &i32)
            ResourceTy::Deref(_) => {
              let ref_data = self.borrow_map.get(lhs_rap.name()).unwrap().clone();
              let deref_index = num_derefs(&lhs_expr) - 1;
              let modified_ref_name = ref_data.aliasing.get(deref_index).unwrap();
              let modified_ref_data = self.borrow_map.get(modified_ref_name).unwrap().clone();
              let to_ro =  self.borrow_map.get(modified_ref_name).unwrap().lender.to_owned();
              
              // add event
              let borrowers = get_borrowers(&lhs_rty.real_name(), &self.borrow_map);
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
              r.assigned_at = line_num;
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

      // a block eg: {}
      ExprKind::Block(block, _) => {
        // this scoping logic isn't necessary except for when defining functions inside of functions
        let prev_scope = self.current_scope;
        let new_scope = self.tcx.sess.source_map().lookup_char_pos(expr.span.hi()).line;
        self.current_scope = new_scope;
        self.visit_block(block); // visit all the statements in the block
          self.current_scope = prev_scope;
      }

      //unary operator */! <expr>
      ExprKind::Unary(UnOp::Deref, exp) => { self.visit_expr(exp) }

      // A path is a name for something 
      // can be a variable, or a path to a definition (function)
      ExprKind::Path(QPath::Resolved(_,p)) => {
        match p.res {
          Res::Def(DefKind::Ctor(_, CtorKind::Const), _id) => {
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
        let line_num = span_to_line(&p.span, &self.tcx);
        self.update_rap(r, line_num);
      }
      
      // Don't know what this is honestly
      ExprKind::DropTemps(exp) => {
        self.visit_expr(exp);
      }

      // if <expr> { } Option<else> 
      ExprKind::If(guard_expr, if_expr, else_expr) => {
        self.visit_expr(&guard_expr);
        self.inside_branch = true; // need this flag to correctly handle variables that are declared inside blocks
        self.visit_expr(&if_expr);
        let (else_live, else_decl) = match else_expr {
          Some(e) => {
            self.visit_expr(e);
            (get_live_of_expr(e, &self.tcx, &self.raps), get_decl_of_expr(e, &self.tcx, &self.raps))
          }
          None => { (HashSet::new(), HashSet::new()) }
        };
        self.inside_branch = false;

        // compute liveness
        // live variables are defined as variables that are defined outside the conditional but 
        // are used inside of it (the ones whose timelines will have a branch in the visualization)
        let if_live = get_live_of_expr(if_expr, &self.tcx, &self.raps);
        let if_decl = get_decl_of_expr(if_expr, &self.tcx, &self.raps);
        let liveness:HashSet<ResourceAccessPoint> = if_live.union(&else_live).cloned().collect();

        // compute split and merge points
        let line_num = expr_to_line(&guard_expr, &self.tcx);
        let split = self.tcx.sess.source_map().lookup_char_pos(if_expr.span.lo()).line;
        let mut if_end = self.tcx.sess.source_map().lookup_char_pos(if_expr.span.hi()).line;
        let merge = match else_expr {
          Some(e) => self.tcx.sess.source_map().lookup_char_pos(e.span.hi()).line,
          None => self.tcx.sess.source_map().lookup_char_pos(if_expr.span.hi()).line
        };

        // filter events that happened in the if/else block
        let mut if_ev: Vec<(usize, ExternalEvent)> = self.preprocessed_events.iter().filter(|i| filter_ev(i, split, if_end)).cloned().collect();
        let mut else_ev: Vec<(usize, ExternalEvent)> = self.preprocessed_events.iter().filter(|i| filter_ev(i, if_end, merge)).cloned().collect();
        self.preprocessed_events.retain(|(l, _)| 
          if *l <= merge && *l >= split {
            false
          }
          else {
            true
          }
        );

        // add gos events for variables declared in each block
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
        if if_end != merge { if_end += 1; } // this is for front-end formatting
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

      ExprKind::Loop(block, _, loop_ty, _span) => {
        match loop_ty {
          rustc_hir::LoopSource::While => {
            match block.expr {
              Some(e) => {
                // while loop is just desugared to an if expression so just visit it
                self.visit_expr(e);
              } 
              None => {


              }
            }
          }
          _ => {
            warn!("unhandled loop expr {:#?}", expr);
          }
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
              res.push(get_rap(&field, &self.tcx, &self.raps));
              res_ty.push(typeck_res.node_type(field.hir_id));
            }
            (res, res_ty)
          }
          _ => {
            (vec![get_rap(&guard_expr, &self.tcx, &self.raps)], vec![typeck_res.node_type(guard_expr.hir_id)])
          }
        };
        let typeck_res = self.tcx.typeck(guard_expr.hir_id.owner);

        let split = self.tcx.sess.source_map().lookup_char_pos(guard_expr.span.hi()).line; // TODO: might need to alter this
        let merge = self.tcx.sess.source_map().lookup_char_pos(expr.span.hi()).line;

        let mut b_ty_names: Vec<String> = Vec::new();
        let mut b_slices: Vec<(usize, usize)> = Vec::new();
        let mut branch_data: Vec<ExtBranchData> = Vec::new();
        let mut liveness: HashSet<ResourceAccessPoint> = get_live_of_expr(guard_expr, &self.tcx, &self.raps);

        
        match source {
          // A normal match (not desugared)
          rustc_hir::MatchSource::Normal => {
            for arm in arms.iter() {
              let mut branch_e_data: Vec<(usize, ExternalEvent)> = Vec::new();
              let mut callback_events: Vec<(ResourceTy, ResourceTy, Evt)> = Vec::new();

              // get line info
              let begin = self.tcx.sess.source_map().lookup_char_pos(arm.body.span.lo()).line;
              let end = self.tcx.sess.source_map().lookup_char_pos(arm.body.span.hi()).line;
              let pat_line = span_to_line(&arm.pat.span, &self.tcx);
              b_slices.push((begin, end));
              b_ty_names.push(get_name_of_pat(arm.pat, &self.tcx));

              
              // add/fetch raps that are initialized in arm expr
              // need to also get their types, in order to figure out if we are moving, copying or borrowing something into the block
              let mut pat_decls: HashSet<ResourceAccessPoint> = HashSet::new();
              // println!("arm pat {:#?}", arm.pat);
              match arm.pat.kind {
                // (<pat>, <pat>, ..) => <expr>
                // First need to annotate events that occur between parents (the variables being matched upon)
                // and the pattern bindings in each arm
                PatKind::TupleStruct(_, pat_list, _) | PatKind::Tuple(pat_list, _)=> {
                  for (i, p) in pat_list.iter().enumerate() {
                    let mut associated_ro = Vec::new();
                    self.get_dec_of_pat2(p, &typeck_res, &parents[i], &parents_ty[i], end, & mut associated_ro);
                    let temp: Vec<ResourceAccessPoint> = associated_ro.iter().map(|(r, _, _)| {r.clone()}).collect();
                    let temp2: HashSet<ResourceAccessPoint> = temp.into_iter().collect();
                    pat_decls.extend(temp2);
                    for (to_ro, e, parent_ty) in associated_ro.iter() {
                      // If the type of the pat is not the same as the parent associated with it then it must be a partial move
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

              // visit expr
              self.inside_branch = true;
              self.visit_expr(arm.body);
              self.inside_branch = false;

              // update liveness info
              liveness = liveness.union(&get_live_of_expr(arm.body, &self.tcx, &self.raps)).cloned().collect();
              liveness = liveness.difference(&pat_decls).cloned().collect();
              let arm_decls: HashSet<ResourceAccessPoint> = pat_decls.union(&get_decl_of_expr(arm.body, &self.tcx, &self.raps)).cloned().collect();


              // get events that occured in the arm
              branch_e_data.extend(
                self.preprocessed_events.iter().filter(|i| filter_ev(i, begin, end)).cloned()
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

              // add callback events - events that need to happen at the end of an arm block
              // ex: if a pattern binding borrows from a parent
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
          rustc_hir::MatchSource::ForLoopDesugar => {
            info!("loop desugar expr {:#?}", expr);
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
        // By finding out the type of LHS we know (kinda) what event will happen,
        // then we can just figure out what RHS is (match_rhs)
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
            // Figure out what LHS is and add it to list of RAPs
            self.define_lhs(lhs_var.clone(), bool_of_mut(binding_annotation.1), expr, lhs_ty);
            self.match_rhs(ResourceTy::Value(self.raps.get(&lhs_var).unwrap().rap.to_owned()), expr, e);
          }
           _ => {} // in the case of a declaration ex: let a; nothing happens, currently unhandled logic
        }
      }
      _ => { warn!("unrecognized local pattern") }
    }
    
    
    if let Some(els) = local.els {
      self.visit_block(els);
    }
  }
} 