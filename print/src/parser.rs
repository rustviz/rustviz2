use crate::ir_mapper;
use rustc_middle::{
  mir::{LocalDecl,Place,Body,Location,Rvalue, Statement, StatementKind},//{Place, ProjectionElem},
  ty::{self, TyCtxt},
};
use rustc_mir_dataflow::move_paths::{MoveData,InitKind};
use ir_mapper::{GatherDepth, IRMapper,GatherMode};
use rustc_hir::{StmtKind, Block, Stmt, Local, Expr, ExprKind, HirId, BodyId, ItemKind, UnOp, QPath, Path, def::Res, Let, PatKind};
use either::Either;
use std::path;
use std::{borrow::Cow, env, collections::HashMap};
use rustc_utils::{OperandExt,PlaceExt};
use clap::Parser;
use rustc_ast::walk_list;
use rustc_utils::source_map;
use serde::{Deserialize, Serialize};
use rustc_span::source_map::{SourceMap};
use rustc_span::{Span, SourceFile};
use aquascope::analysis::{AquascopeAnalysis,AquascopeResult,AnalysisOutput,
  boundaries::PermissionsBoundary};
use rustc_hir::intravisit::{self, Visitor, Map};
use rustc_utils::{
  source_map::range::{BytePos, ByteRange, CharPos, CharRange},
  SpanExt,
  mir::{borrowck_facts}, BodyExt,
};
use smallvec::{smallvec, SmallVec};
use std::ops::Index;
fn find_line_number(source_map: &SourceMap, span: Span) -> usize {
  let loc = source_map.lookup_char_pos(span.lo());
  loc.line
}
fn extract_var_name(input_string: &str ) -> Option<String> {
  let start_index = input_string.find('`')? + 1;
    let end_index = input_string.rfind('`')?;
    Some(input_string[start_index..end_index].to_owned())
}
pub struct ExprVisitor<'a, 'tcx:'a> {
  pub tcx: TyCtxt<'tcx>,
  pub mir_body: &'a Body<'tcx>,
  pub ir_mapper: IRMapper<'a, 'tcx>,
  pub move_data: MoveData<'a>,
  pub boundary_map: HashMap<rustc_span::BytePos,PermissionsBoundary>,
}

impl<'a, 'tcx> ExprVisitor<'a, 'tcx>{
  fn print_move_at_location(&self,location:Location){
    let body=self.mir_body;
    let move_data=&self.move_data;
    let span = body.source_info(location).span;
    let source_map = self.tcx.sess.source_map();
      let line_number = find_line_number(source_map, span);
      let init_index=move_data.init_loc_map.index(location);
      for item in init_index.iter(){
        println!("\tInit at line {}: {:?} ", line_number, move_data.inits[*item]);
        //let mpath = move_data.move_paths[move_data.inits[*item].path].clone();
        //let variable_name = tcx.hir().name(mpath.hir_id).to_ident_string();
      }

      let move_out_index= move_data.loc_map.index(location);
      for item in move_out_index.iter(){
       println!("\tMove out at line {}: {:?}", line_number, move_data.moves[*item]);
      }
  }
  fn get_mir_location(&self,hir_id:HirId,conflicting_node:Option<HirId>)->Option<Location>{
    let hir=self.tcx.hir();
    let search_at_hir_id = |hir_id| {
      let path_locations = self.paths_at_hir_id(hir_id)?;
      let (loc, place) = self.select_candidate_location(
        // thunk to compute the places within the conflicting HirId,
        || {
            conflicting_node
            .and_then(|hir_id| self.paths_at_hir_id(hir_id))
            .unwrap_or_default()
        },
        &path_locations,
      )?;
      Some((loc,place))
    };
    let resolved_location = search_at_hir_id(hir_id)
    .or_else(|| {
      hir.parent_iter(hir_id).find_map(|(hir_id, _)| {
        //println!("\tsearching upwards in: {}", hir.node_to_string(hir_id));
        search_at_hir_id(hir_id)
      })
    });
    if resolved_location.is_none() {
      log::debug!(
        "Could not resolve a MIR location for expected hir_node {}",
        hir.node_to_string(hir_id)
      );
    }
    match resolved_location{
      Some((location,_))=> Some(location),
      None=>None
    }
  }
  fn select_candidate_location(
    &self,
    subtract_from: impl FnOnce() -> Vec<(Location, Place<'tcx>)>,
    candidates: &Vec<(Location, Place<'tcx>)>,
  ) -> Option<(Location, Place<'tcx>)> {
    if candidates.is_empty() {
      return None;
    }
  
    if candidates.len() == 1 {
      return Some(candidates[0]);
    }
  
    let others = subtract_from();
    let candidates = candidates
      .iter()
      .filter(|t| !others.contains(t))
      .collect::<Vec<_>>();
    let base_local = candidates.first()?.1.local;
  
    let matching_locals = candidates
      .into_iter()
      .filter(|(_, p)| p.local == base_local);
    matching_locals
      .rev()
      .max_by_key(|(_, p)| p.projection.len())
      .copied()
  }
  fn paths_at_hir_id(
    &self,
    hir_id: HirId,
  ) -> Option<Vec<(Location, Place<'tcx>)>> {
    let ir_mapper=&self.ir_mapper;
    let tcx=self.tcx;
    let body=self.mir_body;

    type TempBuff<'tcx> = SmallVec<[(Location, Place<'tcx>); 3]>;
  
    let mir_locations_opt =
      ir_mapper.get_mir_locations(hir_id, GatherDepth::Outer);
    macro_rules! maybe_in_op {
      ($loc:expr, $op:expr) => {
        $op
          .as_place()
          .and_then(|p| p.is_source_visible(tcx, body).then_some(p))
          .map(|p| smallvec![($loc, p)])
          .unwrap_or(smallvec![])
      };
      ($loc:expr, $op1:expr, $op2:expr) => {{
        let mut v: TempBuff = maybe_in_op!($loc, $op1);
        let mut o: TempBuff = maybe_in_op!($loc, $op2);
        v.append(&mut o);
        v
      }};
    }
  
    let look_in_rvalue = |rvalue: &Rvalue<'tcx>, loc: Location| -> TempBuff {
      match rvalue {
        // Nested operand cases
        Rvalue::Use(op)
          | Rvalue::Repeat(op, _)
          | Rvalue::Cast(_, op, _)
          | Rvalue::UnaryOp(_, op)
          | Rvalue::ShallowInitBox(op, _) => maybe_in_op!(loc, op),
  
        // Given place cases.
        Rvalue::Ref(_, _, place)
          | Rvalue::AddressOf(_, place)
          | Rvalue::Len(place)
          | Rvalue::Discriminant(place)
          | Rvalue::CopyForDeref(place)
          if place.is_source_visible(tcx, body) =>
        {
          smallvec![(loc, *place)]
        }
  
        // Two operand cases
        Rvalue::BinaryOp(_, box (left_op, right_op))
          | Rvalue::CheckedBinaryOp(_, box (left_op, right_op)) => {
            maybe_in_op!(loc, left_op, right_op)
          }
  
        // Unimplemented cases, ignore nested information for now.
        //
        // These are separated in the or because they aren't implemented,
        // but still silently ignored.
        Rvalue::ThreadLocalRef(..)
          | Rvalue::NullaryOp(..)
          | Rvalue::Aggregate(..)
  
        // Wildcard for catching the previous guarded matches.
          | _ => {
            log::warn!("couldn't find in RVALUE {rvalue:?}");
            smallvec![]
          }
      }
    };
  
    let look_in_statement = |stmt: &Statement<'tcx>, loc: Location| -> TempBuff {
      match &stmt.kind {
        StatementKind::Assign(box (lhs_place, ref rvalue)) => {
          let mut found_so_far: TempBuff = look_in_rvalue(rvalue, loc);
          if lhs_place.is_source_visible(tcx, body) {
            found_so_far.push((loc, *lhs_place));
          }
          found_so_far
        }
        StatementKind::SetDiscriminant { place, .. }
          if place.is_source_visible(tcx, body) =>
        {
          smallvec![(loc, **place)]
        }
        StatementKind::FakeRead(box (_, place))
          if place.is_source_visible(tcx, body) =>
        {
          smallvec![(loc, *place)]
        }
  
  
        StatementKind::SetDiscriminant { .. }
        | StatementKind::FakeRead(..)
        | StatementKind::PlaceMention(..) // TODO: do we need to handle this new kind
  
        // These variants are compiler generated, but it would be
        // insufficient to find a source-visible place only in
        // compiler generated statements.
        //
        // They are also unimplemented so if something is missing
        // suspect something in here.
        | StatementKind::Deinit(..)
        | StatementKind::StorageLive(..)
        | StatementKind::StorageDead(..)
        | StatementKind::Retag(..)
        | StatementKind::AscribeUserType(..)
        | StatementKind::Coverage(..)
        | StatementKind::Intrinsic(..)
        | StatementKind::ConstEvalCounter
        | StatementKind::Nop => smallvec![],
      }
    };
  
    let mir_locations = mir_locations_opt?
      .values()
      .flat_map(|loc| {
        log::debug!("looking at {loc:?}");
        match body.stmt_at(loc) {
          Either::Left(stmt) => look_in_statement(stmt, loc),
          Either::Right(_term) => smallvec![],
        }
      })
      .collect::<Vec<_>>();
  
    Some(mir_locations)
  }
}



impl<'a, 'tcx> Visitor<'tcx> for ExprVisitor<'a, 'tcx> {
  fn visit_expr(&mut self, expr: &'tcx Expr<'tcx>) {
      let hirid = expr.hir_id;
      //println!("visiting {}", self.tcx.hir().node_to_string(hirid));
        match expr.kind {
          // Method calls 
          ExprKind::Call(fn_expr, args) => {
            //let fn_span = fn_expr.span;
            //let fn_hir_id = fn_expr.hir_id;
          }
          ExprKind::MethodCall(_, rcvr, args, fn_span)
            if !fn_span.from_expansion()
              && rcvr.is_place_expr(|e| !matches!(e.kind, ExprKind::Lit(_))) =>
          {
            
             let hir_id=rcvr.hir_id;
             //println!("visiting methodcall{}", self.tcx.hir().node_to_string(hir_id));
            for a in args.iter() {
              self.visit_expr(a);
            }
          }
          //a+b, a*b
          ExprKind::Binary(_, lhs, rhs) => {
            //println!("visiting binary {} {}", self.tcx.hir().node_to_string(lhs.hir_id), self.tcx.hir().node_to_string(rhs.hir_id));
            
            self.visit_expr(lhs);
            self.visit_expr(rhs);
          }
          ExprKind::Lit(_) => {
            //println!("visiting lit {}", self.tcx.hir().node_to_string(hirid));
          }
    
          ExprKind::AddrOf(_, _, inner)
            if inner.is_syntactic_place_expr() && !inner.span.from_expansion() =>
          {
            // We don't have to account for adjusted types because
            // taking a borrow provides explicit types.
            
          }
          ExprKind::Assign(
            lhs,
            rhs,
            _,
          ) => {
            //println!();
            //println!("This is an assign expression: {}", self.tcx.hir().node_to_string(hirid));
            //println!("This is the rhs: {}", self.tcx.hir().node_to_string(rhs.hir_id));
            /*
            let exprloc = self.get_mir_location(hirid, None);
            match exprloc {
              Some(loc)=>{
                println!{"Found following inits and moves:"};
                self.print_move_at_location(loc);
              },
              None=>log::debug!{"Can't find expr location."}
            }
             */
            
            self.visit_expr(lhs);
            self.visit_expr(rhs);
          }
    
         ExprKind::Block(block, _) => {
            //println!("visiting block");
            self.visit_block(block);
          }
    
          ExprKind::AssignOp(_, lhs, rhs) => {
            //println!("visiting assign lhs{}", self.tcx.hir().node_to_string(lhs.hir_id));
            self.visit_expr(lhs);
          }
          //  !x *x
          ExprKind::Unary(UnOp::Deref, inner)
            if inner.is_syntactic_place_expr() && !inner.span.from_expansion() =>
          {
           let hir_id=inner.hir_id;
           //println!("visiting unary {}", self.tcx.hir().node_to_string(hir_id));
          }
          
          // we only want to attach permissions to path resolved to `Local` ids.
          ExprKind::Path(QPath::Resolved(
            _,
            Path {
              span,
              res: Res::Local(_),
              ..
            },
          )) if !span.from_expansion() => {
            let bytepos=span.lo();
            let boundary=self.boundary_map.get(&bytepos);
            println!("{:?} with boundary {:?}",self.tcx.hir().node_to_string(hirid),boundary);
          }
          
          _ => {
            //println!("visiting default");
            intravisit::walk_expr(self, expr);
          }
        }
      }
      // Call the default visitor to visit nested expressions
      fn visit_stmt(&mut self, statement: &'tcx Stmt<'tcx>) {
        match statement.kind {
          StmtKind::Local(ref local) => self.visit_local(local),
          StmtKind::Item(item) => self.visit_nested_item(item),
          StmtKind::Expr(ref expression) | StmtKind::Semi(ref expression) => {
              self.visit_expr(expression)
          }
        }
      }
      fn visit_local(&mut self, local: &'tcx Local<'tcx>) {
        println!();
        println!("This is a let statement: {}", self.tcx.hir().node_to_string(local.hir_id));
        // lhs
        let mut lhs_var : String= "".to_string();
        match local.pat.kind {
          PatKind::Binding(binding_annotation, ann_hirid, ident, op_pat) => {
            println!("lhs is: {}", ident);
            //println!("lhs binding_annotation: {:?}", binding_annotation);//tell mut or not
            //println!("lhs ann_hirid: {:?}", ann_hirid);
            lhs_var = ident.to_string();
            
          }
          PatKind::Path(QPath::Resolved(_,p)) => {
            println!("lhs path: {:?}", self.tcx.def_path_str(p.res.def_id()));
          }
          _ => {
            println!("lhs is not listed");
          }
        }
        // rhs
        //let mut rhs_var : String = "".to_string();
          match local.init {
          | Some(expr) => {
              let e_id = expr.hir_id;
              match expr.kind{
                ExprKind::Lit(_) => {
                  let long_name = self.tcx.hir().node_to_string(e_id);
                  let name = extract_var_name(&long_name);
                  if let Some(name) = name {
                    if lhs_var !="" {
                      println!("Bind({})", lhs_var);
                    }
                  }
                }
                ExprKind::Call(fn_expr, args) => {
                  let fn_span = fn_expr.span;
                  let fn_hir_id = fn_expr.hir_id;
                  let fn_long_name = self.tcx.hir().node_to_string(fn_hir_id);
                  let fn_name = extract_var_name(&fn_long_name);
                  if let Some(fn_name) = fn_name {
                    let line_number= self.tcx.sess.source_map().lookup_char_pos(fn_span.lo());
                    println!("on line: {:#?}", line_number.line);
                    if (!fn_name.contains("crate::io") && !fn_name.contains("String::from")) { //neglect println
                      if lhs_var !="" {
                        println!("Move({}->{})", fn_name, lhs_var);
                      }
                      for a in args.iter() {
                        println!("arg: {:#?}", self.tcx.hir().node_to_string(a.hir_id));
                        self.visit_expr(a);
                      }
                    }else{
                      if fn_name.contains("String::from") {
                        if lhs_var !="" {
                          println!("Move({}->{})", "String::from()", lhs_var);
                        }
                      }
                    }
                  }
                  
                }
                ExprKind::Path(QPath::Resolved(_,p)) => {
                  let long_name = self.tcx.hir().node_to_string(p.segments[0].hir_id);
                  let name = extract_var_name(&long_name);
                  if let Some(name) = name {
                    if lhs_var !="" {
                      println!("Move({}->{})", name, lhs_var);
                    }
                  }
                }
                _ => {
                  println!("rhs is not implemented");
                }
              }
            
            /*
            let exprloc = self.get_mir_location(expr.hir_id, None);
            match exprloc {
              Some(loc)=>{
                //println!{"Found location: {:?} {:?}",loc.block,loc.statement_index};
                println!("Found following inits and moves:");
                self.print_move_at_location(loc);
              },
              None=>log::debug!{"Can't find expr location."}
            }
             */
            
          },
          | _ => {},
          };
        
        
        walk_list!(self, visit_expr, &local.init);
        //self.visit_pat(local.pat);
        if let Some(els) = local.els {
        self.visit_block(els);
        }
        walk_list!(self, visit_ty, &local.ty);
      }
      
}

