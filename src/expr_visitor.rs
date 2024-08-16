//! This file details all the datastructures that are filled while traversing the HIR.
//! Essentially we care about detailing interactions and relationships between 
//! Resource Access Points (RAPs - RV1 terminology)
//! The frontend (svg-generator crate) requires a list of external events:
//! which are generally events that cause some visually external effect on the visualization
//! For example: Moves, Copies, Borrows are all represented with arrows between timelines.


use log::{error, info, warn};
use rustc_middle::{
  mir::Body,
  ty::*,
};
use rustc_hir::{Expr, ExprKind, QPath, PatKind, Mutability, UnOp, StmtKind, Stmt, def::*, Pat, Path};
use std::collections::{BTreeMap, HashMap, HashSet, VecDeque};
use std::ops::Bound;
use rustc_span::Span;
use rustc_hir::hir_id::HirId;
use std::cmp::max;
use crate::expr_visitor_utils::*;
use rustviz_lib::data::*;
use rustc_borrowck::consumers::BodyWithBorrowckFacts;


// A struct to help with appending ExternalEvents
#[derive(Debug, Clone)]
pub enum Evt { 
  // A RAP is bound to some resource (this is usually used when the resource is anonymous)
  // ex: let a = 9;
  // technically all variables are bound at initialization, but we don't use the Bind event
  // for each scenario in which this happens.
  Bind,

  Copy, // let x: i32 = y; | let x: &i32 = y;
  Move, // let x:String = y;
  SBorrow, // let x: &i32 = &y;
  MBorrow, // let x: & mut i32 = & mut y;

  // When a RAP returns an immutably borrowed resource
  SDie,

  // When a RAP returns a mutably boirrowed resource
  MDie,

  // Passing a RAP by static (immutable) reference
  PassBySRef,

  // Passing a RAP by mutable reference
  PassByMRef, // String::push_str(s, "text") | s.push_str("text")
}

// RefData struct is used to represent a loan between a borrower and lender 
// at any time in the program lifetime. Ideally we should use the MIR and just grab 
// the list of borrows that occurs and append events accordingly, implementing our own
// logic is more difficult and not sufficient for complex borrowing scenarios.
#[derive(Debug, Clone)]
pub struct RefData {
  pub lender: ResourceTy, // who this reference borrowed from
  pub assigned_at: usize, // gen point
  pub lifetime: usize, // kill point
  pub ref_mutability: bool, // type
  pub aliasing: VecDeque<String> // aliasing other references
}

#[derive(Debug, Clone)]
pub struct RapData {
  pub rap: ResourceAccessPoint, 
  pub scope: usize,
  pub is_global: bool
}


pub struct ExprVisitor<'a, 'tcx:'a> {
  pub tcx: TyCtxt<'tcx>, // type context
  pub mir_body: &'a Body<'tcx>, 
  pub hir_body: &'a rustc_hir::Body<'tcx>,
  pub bwf: &'a BodyWithBorrowckFacts<'tcx>,

  // Data structure used to represent active loans
  // borrower name -> RefData
  pub borrow_map: HashMap<String, RefData>,

  // Where all the RAPs are stored
  // Rap name -> RapData
  pub raps: &'a mut HashMap<String, RapData>,

  // Used to determine the current scope when visiting expressions
  pub current_scope: usize,

  // Vestigial code, look at aquascope permissions_boundary map to see more
  pub analysis_result : HashMap<usize, Vec<String>>,

  // The event line map stores events that will
  // result in the generation of an arrow by the frontend 
  // Although it's somewhat redundant (events from the preprocessed events)
  // appear in the event_line_map, it's 'necessary' to figure out the arrow orientation
  pub event_line_map: &'a mut BTreeMap<usize, Vec<ExternalEvent>>,

  // Just a list of the events and on which line they occur
  pub preprocessed_events: &'a mut Vec<(usize, ExternalEvent)>,
  
  // These members are necessary for annotated_src computation
  pub rap_hashes: usize,
  pub source_map: & 'a BTreeMap<usize, String>,
  pub annotated_lines: & 'a mut BTreeMap<usize, Vec<String>>,
  pub id_map: & 'a mut HashMap<String, usize>,
  pub unique_id: & 'a mut usize,


  pub inside_branch: bool,
  pub fn_ret: bool
}

impl<'a, 'tcx> ExprVisitor<'a, 'tcx>{

  // Helpers
  pub fn expr_to_line(&self,expr:&Expr) -> usize{
    self.tcx.sess.source_map().lookup_char_pos(expr.span.lo()).line
  }

  pub fn span_to_line(&self,span:&Span) -> usize{
    self.tcx.sess.source_map().lookup_char_pos(span.lo()).line
  }

  pub fn return_type_of(&self,fn_expr:&Expr)->Option<Ty<'tcx>>{
    let type_check = self.tcx.typeck(fn_expr.hir_id.owner);
    let type_of_path = type_check.expr_ty(fn_expr);
    let mut fn_sig = type_of_path.fn_sig(self.tcx).skip_binder().output().walk();
    if let Some(return_type)= fn_sig.next(){
      Some(return_type.expect_ty())
    }
    else {
      None
    }
  }

  // Ideally we should actually just visit the fn expr 
  // which resolves to a path 
  pub fn hirid_to_var_name(&self,id:HirId)->Option<String>{
    let long_name = self.tcx.hir().node_to_string(id);
    extract_var_name(&long_name)
  }

  pub fn is_return_type_ref(&self,fn_expr:&Expr) -> bool{
    if let Some(return_type)=self.return_type_of(fn_expr){
      return_type.is_ref()
    }
    else{
      false
    }
  }

  pub fn is_return_type_copyable(&self,fn_expr:&Expr)->bool{
    if let Some(return_type)=self.return_type_of(fn_expr){
      if return_type.walk().fold(false,|flag,item|{flag||item.expect_ty().is_ref()}) {
        false
      }
      else{
        return_type.is_copy_modulo_regions(self.tcx, self.tcx.param_env(fn_expr.hir_id.owner))
      }
    }
    else{
      false
    }
  }

  // updates the lifetime of a loan (as well as for each of the aliases associated with this loan)
  pub fn update_lifetime(&mut self, name: &String, line:usize){
    self.borrow_map.get_mut(name).unwrap().lifetime = line;
    let aliasing = self.borrow_map.get(name).unwrap().aliasing.clone();
    for r in aliasing.iter() {
      self.update_lifetime(r, line);
    }
  }

  // Adds an owner RAP
  pub fn add_owner(&mut self, name: String, mutability: bool, scope: usize, is_global: bool) {
    self.add_rap(ResourceAccessPoint::Owner(Owner{name: name, hash: self.rap_hashes as u64, is_mut: mutability}), scope, is_global);
  }

  // Adds a reference RAP
  pub fn add_ref(&mut self, name: String, ref_mutability: bool, lhs_mut: bool, line_num: usize, lender: ResourceTy, alia: VecDeque<String>, scope: usize, is_global: bool) {
    match ref_mutability {
      true => {
        self.add_mut_ref(name.clone(), lhs_mut, scope, is_global);
      }
      false => { 
        self.add_static_ref(name.clone(), lhs_mut, scope, is_global);
      }
    }
    self.borrow_map.insert(name.clone(), RefData { lender: lender.clone(), assigned_at: line_num, lifetime: line_num, ref_mutability: ref_mutability, aliasing: alia });
  }
  
  pub fn add_static_ref(&mut self, name: String, mutability: bool, scope: usize, is_global: bool) {
    self.add_rap(ResourceAccessPoint::StaticRef(StaticRef { name: name, hash: self.rap_hashes as u64, is_mut: mutability }), scope, is_global);
  }

  pub fn add_mut_ref(&mut self, name: String, mutability: bool, scope: usize, is_global: bool) {
    self.add_rap(ResourceAccessPoint::MutRef(MutRef { name: name, hash: self.rap_hashes as u64, is_mut: mutability }), scope, is_global);
  }

  // Adds a function RAP
  pub fn add_fn(&mut self, name: String) {
    self.add_rap(ResourceAccessPoint::Function(Function { name: name, hash: self.rap_hashes as u64 }), self.current_scope, true);
  }

  // Adds a struct RAP
  // To be honest this struct logic is leftover from RV1 and should eventually be reworked
  // It doesn't make much sense that a struct should be represented any different than an owner
  // the reason for this is that in the frontend structs are visualized differently in the timeline header
  pub fn add_struct(&mut self, name: String, owner: u64, mem: bool, mutability: bool, scope: usize, is_global: bool) {
    self.add_rap(ResourceAccessPoint::Struct(Struct { 
      name: name, 
      hash: self.rap_hashes as u64, 
      owner: owner, 
      is_mut: mutability, 
      is_member: mem }),
      scope, is_global);
  }

  pub fn add_rap(&mut self, r: ResourceAccessPoint, scope: usize, is_global: bool) {
    self.raps.entry(r.name().to_string()).or_insert_with(|| {self.rap_hashes += 1; RapData{rap: r, scope, is_global}});
  }

  pub fn update_rap(&mut self, r: &ResourceAccessPoint, line_num: usize) {
    if r.is_ref() {
      self.update_lifetime(r.name(), line_num);
    }
  }

  pub fn num_derefs(&self, expr: &'tcx Expr) -> usize{
    match expr.kind {
      ExprKind::Unary(UnOp::Deref, exp) => {
        1 + self.num_derefs(exp)
      }
      _ => 0
    }
  }

  pub fn add_external_event(&mut self, line_num: usize, event: ExternalEvent) {
    self.preprocessed_events.push((line_num, event.clone()));
    let resourceaccesspoint = ResourceAccessPoint_extract(&event);
    match (resourceaccesspoint.0, resourceaccesspoint.1, &event) {
      (ResourceTy::Value(ResourceAccessPoint::Function(_)), ResourceTy::Value(ResourceAccessPoint::Function(_)), _) => {
      },
      (ResourceTy::Value(ResourceAccessPoint::Function(_)),_,  _) => {  
          // (Some(function), Some(variable), _)
      },
      (_, ResourceTy::Value(ResourceAccessPoint::Function(_function)), 
       ExternalEvent::PassByStaticReference{..}) => { 
           // (Some(variable), Some(function), PassByStatRef)
      },
      (_, ResourceTy::Value(ResourceAccessPoint::Function(_function)), 
       ExternalEvent::PassByMutableReference{..}) => {  
           // (Some(variable), Some(function), PassByMutRef)
      },
      (_, ResourceTy::Value(ResourceAccessPoint::Function(_)), _) => { 
          // (Some(variable), Some(function), _)
      },
      (ResourceTy::Anonymous, ResourceTy::Anonymous, _) | (_, ResourceTy::Caller, _) => {},
      (ResourceTy::Value(_), ResourceTy::Value(_), _) // maybe change later
      | (ResourceTy::Deref(_), ResourceTy::Deref(_), _) 
      | (ResourceTy::Value(_), ResourceTy::Deref(_), _)
      | (ResourceTy::Deref(_), ResourceTy::Value(_), _) => {
        self.event_line_map.get_mut(&line_num).unwrap().push(event);
      },
      _ => ()
    }
  }

  pub fn ext_ev_of_evt(&self, evt: Evt, lhs: ResourceTy, rhs: ResourceTy, id: usize, is_partial: bool) -> ExternalEvent{
    match evt {
      Evt::Bind => ExternalEvent::Bind { from: rhs, to: lhs, id },
      Evt::Copy => ExternalEvent::Copy { from: rhs, to: lhs, id, is_partial },
      Evt::Move => ExternalEvent::Move { from: rhs, to: lhs, id, is_partial },
      Evt::SBorrow => ExternalEvent::StaticBorrow { from: rhs, to: lhs, id, is_partial },
      Evt::MBorrow => ExternalEvent::MutableBorrow { from: rhs, to: lhs, id, is_partial },
      Evt::PassBySRef => ExternalEvent::PassByStaticReference { from: rhs, to: lhs, id },
      Evt::PassByMRef => ExternalEvent::PassByMutableReference { from: rhs, to: lhs, id },
      Evt::SDie =>  ExternalEvent::StaticDie { from: rhs, to: lhs, id },
      Evt::MDie => ExternalEvent::MutableDie { from: rhs, to: lhs, id }
    }
  }

  pub fn add_ev(&mut self, line_num: usize, evt: Evt, lhs: ResourceTy, rhs: ResourceTy, is_partial: bool) {
    self.add_external_event(line_num, self.ext_ev_of_evt(evt, lhs, rhs, *self.unique_id, is_partial));
    *self.unique_id += 1;
  }

  // Return the ResourceTy that corresponds to the LHS of a let stmt
  pub fn resource_of_lhs(&mut self, expr: &'tcx Expr) -> ResourceTy {
    match expr.kind {
      ExprKind::Path(QPath::Resolved(_, p)) => {
        let name = self.tcx.hir().name(p.segments[0].hir_id).as_str().to_owned();
        ResourceTy::Value(self.raps.get(&name).unwrap().rap.to_owned())
      }
      ExprKind::Field(expr, ident) => {
        match expr {
          Expr{kind: ExprKind::Path(QPath::Resolved(_,p)), ..} => {
            let name = self.tcx.hir().name(p.segments[0].hir_id).as_str().to_owned();
            let total_name = format!("{}.{}", name, ident.as_str());
            ResourceTy::Value(self.raps.get(&total_name).unwrap().rap.to_owned())
          }
          _ => { panic!("unexpected field expr") }
        } 
      }
      ExprKind::Unary(UnOp::Deref, exp) => {
        let rhs_rap = self.fetch_rap(&expr);
        let line_num = self.expr_to_line(&exp);
        match rhs_rap {
          Some(x) => {
            self.update_rap(&x, line_num);
            ResourceTy::Deref(x)
          }
          None => { ResourceTy::Anonymous }
        }
      }
      _ => panic!("invalid lhs")
    }
  }

  // get the non anonymous lenders and their respective 'active' borrowers
  pub fn get_non_anon_lenders(&self) -> HashMap<String, HashSet<String>> {
    let b_map = self.borrow_map.clone();
    let mut res: HashMap<String, HashSet<String>> = HashMap::new();
    for (k, v) in b_map.iter() {
      match v.lender {
        ResourceTy::Anonymous => {},
        _ => {
          let lender = v.lender.real_name();
          res.entry(lender.clone())
            .and_modify(|lendees| { lendees.insert(k.to_owned()); })
            .or_insert(HashSet::from([k.to_owned()]));
        }
      }
    }
    res
  }

  // get borrowers associated with a single lender
  pub fn get_borrowers(&self, borrower: &String) -> HashSet<String> {
    let lender = self.borrow_map.get(borrower).unwrap().lender.clone();
    match lender {
      ResourceTy::Anonymous | ResourceTy::Caller => HashSet::from([borrower.to_owned()]),
      _ => {
        let mut res:HashSet<String> = HashSet::new();
        for (k, v) in self.borrow_map.iter() {
          if v.lender == lender {
            res.insert(k.to_string());
          }
        } 
        res
      }
    }
  }

  // Gonna be honest these functions are poorly named and are all very similar but subtly different
  // Should probably be changed into a single function with a better name

  // Gets the RAP associated with an expr where we expect expr to resolve to a singular RAP
  pub fn get_rap(&self, expr: &'tcx Expr) -> ResourceTy {
    match expr.kind {
      ExprKind::Path(QPath::Resolved(_,p)) => {
        let name = self.tcx.hir().name(p.segments[0].hir_id).as_str().to_owned();
        ResourceTy::Value(self.raps.get(&name).unwrap().rap.to_owned())
      }
      ExprKind::Unary(rustc_hir::UnOp::Deref, expr) => {
        let rhs_rap = self.fetch_rap(&expr);
        match rhs_rap {
          Some(x) => {
            ResourceTy::Deref(x)
          }
          None => ResourceTy::Anonymous
        }
      }
      ExprKind::AddrOf(_, _, expr) | ExprKind::Unary(_, expr) => self.get_rap(expr),
      ExprKind::Call(fn_expr, _) => {
        let fn_name = self.hirid_to_var_name(fn_expr.hir_id).unwrap();
        ResourceTy::Value(self.raps.get(&fn_name).unwrap().rap.to_owned())
      }
      ExprKind::MethodCall(name_and_generic_args, ..) => {
        let fn_name = self.hirid_to_var_name(name_and_generic_args.hir_id).unwrap();
        ResourceTy::Value(self.raps.get(&fn_name).unwrap().rap.to_owned())
      }
      ExprKind::Block(b, _) => {
        match b.expr {
          Some(expr) => { self.get_rap(expr) }
          None => { panic!("invalid expr for getting rap") }
        }
      }
      ExprKind::Field(expr, ident) => {
        match expr {
          Expr{kind: ExprKind::Path(QPath::Resolved(_,p)), ..} => {
            let name = self.tcx.hir().name(p.segments[0].hir_id).as_str().to_owned();
            let field_name: String = ident.as_str().to_owned();
            let total_name = format!("{}.{}", name, field_name);
            ResourceTy::Value(self.raps.get(&total_name).unwrap().rap.to_owned())
          }
          _ => { panic!("unexpected field expr") }
        } 
      }
      _ => ResourceTy::Anonymous
    }
  }

  // Basically the same as get_rap but we don't care about function RAPs
  pub fn fetch_rap(&self, expr: &'tcx Expr) -> Option<ResourceAccessPoint> {
    match expr.kind {
      ExprKind::Call(..) | ExprKind::Binary(..) | ExprKind::Lit(_) | ExprKind::MethodCall(..) => None,
      ExprKind::Path(QPath::Resolved(_,p)) => {
        let name = self.tcx.hir().name(p.segments[0].hir_id).as_str().to_owned();
        Some(self.raps.get(&name).unwrap().rap.to_owned())
      }
      ExprKind::AddrOf(_, _, expr) | ExprKind::Unary(_, expr) => self.fetch_rap(expr),
      ExprKind::Block(b, _) => {
        match b.expr {
          Some(expr) => { self.fetch_rap(expr) }
          None => { panic!("invalid expr for fetching rap") }
        }
      }
      ExprKind::Field(expr, ident) => {
        match expr {
          Expr{kind: ExprKind::Path(QPath::Resolved(_,p)), ..} => {
            let name = self.tcx.hir().name(p.segments[0].hir_id).as_str().to_owned();
            let field_name: String = ident.as_str().to_owned();
            let total_name = format!("{}.{}", name, field_name);
            Some(self.raps.get(&total_name).unwrap().rap.to_owned())
          }
          _ => { panic!("unexpected field expr") }
        } 
      }
      _ => None
    }
  }

  // used to find the lender from the rhs of a let expr
  // ex: let a = &y; (y is the lender)
  // it gets a little interesting when rhs involves a * operator
  pub fn find_lender(&self, rhs: &'tcx Expr) -> ResourceTy {
    match rhs.kind {
      ExprKind::Path(QPath::Resolved(_,p)) => {
        let name = self.tcx.hir().name(p.segments[0].hir_id).as_str().to_owned();
        if self.borrow_map.contains_key(&name) {
          self.borrow_map.get(&name).unwrap().to_owned().lender
        }
        else{
          ResourceTy::Value(self.raps.get(&name).unwrap().rap.to_owned())
        }
      }
      ExprKind::Call(..) | ExprKind::MethodCall(..) | ExprKind::Lit(_) => {
        ResourceTy::Anonymous
      }
      ExprKind::AddrOf(_, _, expr) => {
        self.find_lender(expr)
      }
      ExprKind::Block(b, _) => {
        match b.expr {
          Some(expr) => { self.find_lender(expr) }
          None => { panic!("invalid rhs lender block") }
        }
      }
      ExprKind::Unary(op, expr) => { 
        match op {
          rustc_hir::UnOp::Deref => {
            match self.find_lender(expr) {
              ResourceTy::Deref(r) | ResourceTy::Value(r) => {
                self.borrow_map.get(r.name()).unwrap().to_owned().lender
              }
              _ => { ResourceTy::Anonymous }
            }
          },
          _ => {
            self.find_lender(expr)
          }
        }
      }
      ExprKind::Field(expr, ident) => {
        match expr {
          Expr{kind: ExprKind::Path(QPath::Resolved(_,p)), ..} => {
            let mut name = self.tcx.hir().name(p.segments[0].hir_id).as_str().to_owned();
            name = format!("{}.{}", name, ident.as_str());
            if self.borrow_map.contains_key(&name) {
              self.borrow_map.get(&name).unwrap().to_owned().lender
            }
            else{
              ResourceTy::Value(self.raps.get(&name).unwrap().rap.to_owned())
            }
          }
          _ => { panic!("unexpected field expr") }
        } 
      }
    _ => { panic!("unexpected rhs lender expr {:#?}", rhs) }
    }
  }

  // add events for arguments in a function call
  pub fn match_arg(&mut self, arg: &'tcx Expr, fn_name: String) {
    self.add_fn(fn_name.clone());
    let line_num = self.expr_to_line(&arg);
    let tycheck_results = self.tcx.typeck(arg.hir_id.owner);
    let arg_ty = tycheck_results.node_type(arg.hir_id);
    let is_copyable = arg_ty.is_copy_modulo_regions(self.tcx, self.tcx.param_env(arg.hir_id.owner));
    let from_ro = self.get_rap(arg);
    let to_ro = ResourceTy::Value(self.raps.get(&fn_name).unwrap().rap.to_owned());
    // type-check the arg and add event accordingly
    if arg_ty.is_ref() {
      match arg_ty.ref_mutability().unwrap() {
        Mutability::Not => self.add_ev(line_num, Evt::PassBySRef, to_ro, from_ro, false),
        Mutability::Mut => self.add_ev(line_num, Evt::PassByMRef, to_ro, from_ro, false)
      }
    }
    else {
      match is_copyable {
        true => self.add_ev(line_num, Evt::Copy, to_ro, from_ro, false),
        false => self.add_ev(line_num, Evt::Move, to_ro, from_ro, false)
      }
    }
  }

  // Basically all types are ADTs, but for some types we
  // don't want to actually represent them as such 
  // (since users aren't usually going to call String.buf)
  pub fn ty_is_special_owner (&self, t:Ty<'tcx>) -> bool {
    match &*t.sort_string(self.tcx) {
      "`std::string::String`" => { true }
      _ => {
        println!("t {}", &*t.sort_string(self.tcx));
        false
      }
    }
  }

  // Update the aliasing data for an alias
  pub fn update_aliasing_data(& mut self, alias: &String, new_data: &BTreeMap<usize, String>, offset: usize) {
    let ref_data = self.borrow_map.get_mut(alias).unwrap();
    for (k, v) in new_data {
      ref_data.aliasing.insert(*k + offset, v.to_owned());
    }
  }

  pub fn is_addr(&self, expr: &'tcx Expr) -> bool { // todo, probably a better way to do this using the typechecker
    match expr.kind {
      ExprKind::AddrOf(..) => true,
      _ => false
    }
  }

  pub fn get_aliasing_data(&self, r: &ResourceTy) -> VecDeque<String> {
    match r {
      ResourceTy::Anonymous | ResourceTy::Caller => VecDeque::new(),
      ResourceTy::Value(x) | ResourceTy::Deref(x) => {
        match self.borrow_map.get(x.name()) {
          Some(r_data) => r_data.aliasing.to_owned(),
          None => VecDeque::new()
        }
      }
    }
  }

  // 
  pub fn get_ref_data(&self, expr: &'tcx Expr) -> (ResourceTy, VecDeque<String>){
    // ex:
    // let a = &b (where b has type &&i32)
    // a's aliases consist of a -> (b -> ... -> ...)
    if self.is_addr(&expr) {
      let lender = self.get_rap(&expr); // in this case we are borrowing from b
      let mut aliasing = self.get_aliasing_data(&lender);
      // if expr has type &&
      let ty = self.tcx.typeck(expr.hir_id.owner).node_type(expr.hir_id);
      if ty.builtin_deref(false).unwrap().is_ref() {
        match &lender {
          ResourceTy::Anonymous | ResourceTy::Caller => { (lender, aliasing) }
          ResourceTy::Value(x) | ResourceTy::Deref(x) => {
            aliasing.push_front(x.name().to_owned()); // here the lender is added to the list of aliases 
            (lender, aliasing)
          }
        }
      }
      else {
        (lender, aliasing)
      }
    }
    else { // if we are copying a reference (ie let a = b (where b is ref))
      // in this case we are borrowing to whatever b refers to
      // likewise, aliasing data is just copied here
      let lender = self.find_lender(&expr);
      (self.find_lender(&expr), self.get_aliasing_data(&lender))
    }
  }

  // Add a RAP to our collection of raps for a let stmt
  pub fn define_lhs(&mut self, lhs_name: String, lhs_mutability: bool, rhs_expr: &'tcx Expr, lhs_ty: Ty <'tcx>) {
    if lhs_ty.is_ref() {
      let (lender, aliasing) = self.get_ref_data(&rhs_expr);
      self.add_ref(lhs_name.clone(), 
      bool_of_mut(lhs_ty.ref_mutability().unwrap()), 
      lhs_mutability, 
      self.expr_to_line(&rhs_expr),
      lender,
      aliasing,
      self.current_scope, !self.inside_branch);
    }
    else if lhs_ty.is_adt() && !self.ty_is_special_owner(lhs_ty) {
      match lhs_ty.ty_adt_def().unwrap().adt_kind() {
        AdtKind::Struct => {
          let owner_hash = self.rap_hashes as u64;
          self.add_struct(lhs_name.clone(), owner_hash, false, lhs_mutability, self.current_scope,!self.inside_branch);
          for field in lhs_ty.ty_adt_def().unwrap().all_fields() {
            let field_name = format!("{}.{}", lhs_name.clone(), field.name.as_str());
            self.add_struct(field_name, owner_hash, true, lhs_mutability, self.current_scope,  !self.inside_branch);
          }
        },
        AdtKind::Union => {
          warn!("lhs union not implemented yet")
        },
        AdtKind::Enum => {
          self.add_owner(lhs_name, lhs_mutability, self.current_scope, !self.inside_branch);
        }
      }
    }
    else if lhs_ty.is_fn() {
      error!("cannot have fn as lhs of expr");
    }
    else { 
      self.add_owner(lhs_name, lhs_mutability, self.current_scope, !self.inside_branch);
    }
  }

  // Get a string representation of a Path - usually used as a name for a RAP
  pub fn string_of_path(&self, p: &Path) -> String {
    match p.res {
      Res::Def(rustc_hir::def::DefKind::Ctor(_, _), _) => {
        let mut name = String::new();
        for (i, segment) in p.segments.iter().enumerate() {
          name.push_str(self.tcx.hir().name(segment.hir_id).as_str());
          if i < p.segments.len() - 1 {
            name.push_str("::");
          }
        }
        name
      }
      _ => {
        self.tcx.hir().name(p.segments[0].hir_id).as_str().to_owned()
      }
    }
  }

  // Get the name of a pattern
  // used for getting names of arms of Match expr
  pub fn get_name_of_pat(&self, pat: &Pat) -> String {
    match pat.kind {
      PatKind::Binding(_, _, ident, _) => ident.to_string(),
      PatKind::TupleStruct(QPath::Resolved(_, p), _, _) => {
        self.string_of_path(&p)
      }
      PatKind::Path(QPath::Resolved(_, p)) => {
        self.string_of_path(&p)
      }
      PatKind::Wild => {
        String::from("Wild")
      }
      PatKind::Tuple(pat_list, _) => {
        let mut res = String::from("(");
        for (i, p) in pat_list.iter().enumerate() {
          res.push_str(&self.get_name_of_pat(p));
          if i < pat_list.len() - 1{
            res.push_str(", ");
          }
          else {
            res.push(')');
          }
        }
        res
      }
      _ => panic!("unexpected pat kind {:#?}", pat)
    }
  }

  // Add a RAP for each variable bound in a pattern
  pub fn get_dec_of_pat2<'t>(
    &mut self,
    pat: &Pat,
    ty_results: &'tcx TypeckResults<'tcx>,
    parent: &ResourceTy,
    parent_ty: &'t Ty<'tcx>,
    scope: usize,
    res: &'t mut Vec<(ResourceAccessPoint, Evt, Ty<'tcx>)>,
) {
    match pat.kind {
        PatKind::TupleStruct(_p, tuple_members, _) => {
            for p in tuple_members.iter() {
                self.get_dec_of_pat2(p, ty_results, parent, parent_ty, scope, res);
            }
        }
        PatKind::Binding(mode, id, ident, _) => {
            // add raps that appear in binding
            let muta = bool_of_mut(mode.1); // You need to define this function
            let line_num = self.span_to_line(&ident.span); // Assuming self.span_to_line exists
            let ty: Ty<'tcx> = ty_results.node_type(id);
            let name = ident.to_string();
            // copied code - should definetly make a function of this
            if ty.is_ref() {
                let ref_mutability = bool_of_mut(ty.ref_mutability().unwrap()); // You need to define this function
                self.add_ref(name.clone(), ref_mutability, muta, line_num, parent.clone(), VecDeque::new(), scope, false);
            } else if ty.is_adt() {
                match ty.ty_adt_def().unwrap().adt_kind() {
                    AdtKind::Struct => {
                        let owner_hash = self.rap_hashes as u64; // Assuming self.rap_hashes exists
                        self.add_struct(name.clone(), owner_hash, false, muta, scope, false);
                        for field in ty.ty_adt_def().unwrap().all_fields() {
                            let field_name = format!("{}.{}", name, field.name.as_str());
                            self.add_struct(field_name, owner_hash, true, muta, scope, false);
                        }
                    },
                    AdtKind::Union => {
                        panic!("union not implemented yet")
                    },
                    AdtKind::Enum => {
                        self.add_owner(name.clone(), muta, scope, false);
                    }
                }
            } else {
                self.add_owner(name.clone(), muta, scope, false);
            }

            let evt = if parent_ty.is_ref() {
                if bool_of_mut(parent_ty.ref_mutability().unwrap()) { Evt::MBorrow }
                else { Evt::SBorrow }
            } else {
                if ty.is_copy_modulo_regions(self.tcx, self.tcx.param_env(pat.hir_id.owner)) { Evt::Copy }
                else { Evt::Move }
            };

            res.push((self.raps.get(&name).unwrap().rap.to_owned(), evt, parent_ty.clone()));
        }
        _ => {}
    }
}

  pub fn get_decl_of_block(&self, block: & 'tcx rustc_hir::Block) -> HashSet<ResourceAccessPoint>{
    let mut res:HashSet<ResourceAccessPoint> = HashSet::new();
    for stmt in block.stmts.iter() {
      res = res.union(&self.get_decl_of_stmt(&stmt)).cloned().collect();
    }
    res
  }
  // we only care about fetching the declarations in the current block, which is why these functions are not mutually recursive
  pub fn get_decl_of_expr(&self, expr: &'tcx Expr) -> HashSet<ResourceAccessPoint> {
    match expr.kind {
      ExprKind::Block(b, _) => self.get_decl_of_block(b),
      _ => HashSet::new() // maybe should handle match expressions as well? 
    }
  }

  pub fn get_decl_of_stmt(&self, stmt: &'tcx Stmt) -> HashSet<ResourceAccessPoint> {
    let mut res = HashSet::new();
    match stmt.kind {
      StmtKind::Let(ref local) => {
        let name = match local.pat.kind {
          PatKind::Binding(_, _, ident, _) => {
            ident.to_string()
          }
          _ => panic!("unexpected let binding pattern")
        };
        res.insert(self.raps.get(&name).unwrap().rap.to_owned());
      }
      _ => {}
    }
    res
  }

  // Get all the live variables in the expression
  // where live refers to the variables defined OUTSIDE of the expression
  // and used inside it. This is distinct from those declared inside it
  pub fn get_live_of_expr(&self, expr: &'tcx Expr) -> HashSet<ResourceAccessPoint> {
    match expr.kind {
      ExprKind::Path(QPath::Resolved(_,p)) => {
        match p.res {
          Res::Def(rustc_hir::def::DefKind::Ctor(_, _), _) => {
            // function, so we don't care about it in regards to live vars
            HashSet::new()
          }
          _ => {
            let name = self.tcx.hir().name(p.segments[0].hir_id).as_str().to_owned();
            HashSet::from([self.raps.get(&name).unwrap().rap.to_owned()])
          }
        }
      }
      ExprKind::Field(expr, ident) => {
        match expr {
          Expr{kind: ExprKind::Path(QPath::Resolved(_,p)), ..} => {
            let name = self.tcx.hir().name(p.segments[0].hir_id).as_str().to_owned();
            let field_name: String = ident.as_str().to_owned();
            let total_name = format!("{}.{}", name, field_name);
            HashSet::from([self.raps.get(&total_name).unwrap().rap.to_owned()])
          }
          _ => { panic!("unexpected field expr") }
        } 
      }
      ExprKind::AddrOf(_, _, exp) | ExprKind::Unary(_, exp) 
      | ExprKind::DropTemps(exp) => {
        self.get_live_of_expr(exp)
      }
      ExprKind::Binary(_, lhs_expr, rhs_expr) | ExprKind::Assign(lhs_expr, rhs_expr, _) | ExprKind::AssignOp(_, lhs_expr, rhs_expr) => {
        let lhs = self.get_live_of_expr(lhs_expr);
        let rhs = self.get_live_of_expr(&rhs_expr);
        let res = lhs.union(&rhs).cloned().collect();
        res
      }
      ExprKind::Call(fn_expr, args) => {
        let mut res = HashSet::new();
        match fn_expr.kind {
          // Match onto println! macro
          ExprKind::Path(QPath::Resolved(_,rustc_hir::Path{res: rustc_hir::def::Res::Def(_, id), ..})) 
          if !id.is_local() => {
            match args {
              [Expr{kind: ExprKind::Call(_, a),..}] => {
                match a {
                  [_, Expr{kind: ExprKind::AddrOf(_, _, 
                    Expr{kind: ExprKind::Array(x),..}),..}] => {
                      for exp in x.iter() {
                        match exp {
                          Expr{kind: ExprKind::Call(_, format_args), ..} => {
                            for a_expr in format_args.iter() {
                              res = res.union(&self.get_live_of_expr(&a_expr)).cloned().collect();
                            }
                          }
                          _ => {
                            warn!("getting here to the println 1");
                          }
                        }
                      }
                    }
                  _ => {
                    warn!("getting here to the println 2");
                  }
                }
              }
              _ => {
                for a_expr in args.iter() {
                  res = res.union(&self.get_live_of_expr(&a_expr)).cloned().collect();
                }
              }
            }
          }
          _ => {
            for a_expr in args.iter() {
              res = res.union(&self.get_live_of_expr(&a_expr)).cloned().collect();
            }
          }
        }
        res
      }
      ExprKind::MethodCall(_, rcvr, args, _) => {
        let rcvr_name = self.hirid_to_var_name(rcvr.hir_id).unwrap();
        let mut res = HashSet::from([self.raps.get(&rcvr_name).unwrap().rap.to_owned()]);
        for a_expr in args.iter() {
          res = res.union(&self.get_live_of_expr(&a_expr)).cloned().collect();
        }
        res
      }
      // Branch expressions need to be handled a bit differently, 
      // We want the variables that are live in each block, but not the ones that were declared in 
      // the blocks (since their timelines should not be split)
      ExprKind::Block(b, _) => {
        let mut res: HashSet<ResourceAccessPoint> = HashSet::new();
        for stmt in b.stmts.iter() {
          res = res.union(&self.get_live_of_stmt(&stmt)).cloned().collect();
        }
        match b.expr {
          Some(exp) => {
            res = res.union(&self.get_live_of_expr(exp)).cloned().collect();
          }
          None => {}
        }
        res.difference(&self.get_decl_of_block(b)).cloned().collect() // need to remove the variables that were declared in the current block 
      }
      ExprKind::If(guard_expr, if_expr, else_expr) => {
        let mut res: HashSet<ResourceAccessPoint> = HashSet::new();
        res = res.union(&self.get_live_of_expr(&guard_expr)).cloned().collect();
        res = res.union(&self.get_live_of_expr(&if_expr)).cloned().collect();
        match else_expr {
          Some(e) => {
            res = res.union(&self.get_live_of_expr(&e)).cloned().collect();
          }
          None => {}
        }
        res
      }
      ExprKind::Tup(expr_list) => {
        let mut res: HashSet<ResourceAccessPoint> = HashSet::new();
        for e in expr_list.iter() {
          res = res.union(&self.get_live_of_expr(e)).cloned().collect();
        }
        res
      }
      _ => {
        HashSet::new()
      }
    }
  }

  pub fn get_live_of_stmt(&self, stmt: &'tcx Stmt) -> HashSet<ResourceAccessPoint> {
    match stmt.kind {
      StmtKind::Let(ref local) => {
        match local.init {
          Some(expr) => {
            self.get_live_of_expr(&expr)
          }
          None => HashSet::new()
        }
      }
      StmtKind::Item(_item) => panic!("not yet able to handle this"),
      StmtKind::Expr(ref expression) | StmtKind::Semi(ref expression) => {
          self.get_live_of_expr(expression)
      }
    }
  }
  pub fn filter_ev(&self, (line_num, _ev): &(usize, ExternalEvent), split: usize, merge: usize) -> bool {
    if *line_num <= merge && *line_num >= split {
      true
    }
    else {
      false
    }
  }

  // Fetch the mutability of the borrow
  pub fn fetch_mutability(&self, expr: &'tcx Expr) -> Option<Mutability> {
    match expr.kind {
      ExprKind::Block(b, _) => {
        match b.expr {
          Some(expr) => { self.fetch_mutability(expr) }
          None => { panic!("invalid expr for fetching mutability") }
        }
      }
      ExprKind::AddrOf(_, mutability, expr) => {
        match self.fetch_mutability(&expr) {
          None => Some(mutability), 
          Some(m) => Some(m)
        }
      }
      _ => None
    }
  }

  // This function does the work of adding an event for let stmts
  pub fn match_rhs(&mut self, lhs: ResourceTy, rhs:&'tcx Expr, evt: Evt){
    match rhs.kind {
      ExprKind::Path(QPath::Resolved(_,p)) => {
        let line_num = self.span_to_line(&p.span);
        let rhs_name: String = match p.res {
          Res::Def(rustc_hir::def::DefKind::Ctor(_, _), _) => {
            let mut name = String::new();
            for (i, segment) in p.segments.iter().enumerate() {
              name.push_str(self.tcx.hir().name(segment.hir_id).as_str());
              if i < p.segments.len() - 1 {
                name.push_str("::");
              }
            }
            name
          }
          _ => {
            self.tcx.hir().name(p.segments[0].hir_id).as_str().to_owned()
          }
        };
        let rhs_rap = self.raps.get(&rhs_name).unwrap().rap.to_owned();
        self.update_rap(&rhs_rap, line_num);
        self.add_ev(line_num, evt, lhs, ResourceTy::Value(rhs_rap), false);
      },
      // fn_expr: resolves to function itself (Path)
      // second arg, is a list of args to the function
      ExprKind::Call(fn_expr, _) => {
        let line_num = self.span_to_line(&fn_expr.span);
        let fn_name = self.hirid_to_var_name(fn_expr.hir_id).unwrap();
        let rhs_rap = self.raps.get(&fn_name).unwrap().rap.to_owned();
        self.add_ev(line_num, evt, lhs, ResourceTy::Value(rhs_rap), false);
      },
      
      ExprKind::Lit(_) | ExprKind::Binary(..) | // Any type of literal on RHS implies a bind
      ExprKind::Unary(UnOp::Neg, _) | // ~<expr>
      ExprKind::Unary(UnOp::Not, _) // !<expr>
      => {
        let line_num = self.span_to_line(&rhs.span);
        self.add_ev(line_num, Evt::Bind, lhs, ResourceTy::Anonymous, false);
      }
      // ex : &<expr> or &mut <expr>
      ExprKind::AddrOf(_, _,expr) => {
        let line_num = self.expr_to_line(&expr);
        match self.fetch_rap(&expr) {
          Some(rhs_rap) => {
            self.update_rap(&rhs_rap, self.expr_to_line(&expr));
          }
          None => {} // taking addrOf some anonymous resource, ie: &String::from("")
        }
        let res = match self.fetch_rap(rhs) {
          Some(x) => ResourceTy::Value(x),
          None => ResourceTy::Anonymous
        };
        match self.fetch_mutability(&rhs) { // fetch last ref mutability in the chain -> &&&mut x
          Some(Mutability::Not) => self.add_ev(line_num, Evt::SBorrow, lhs, res, false),
          Some(Mutability::Mut) => self.add_ev(line_num, Evt::MBorrow, lhs, res, false),
          None => panic!("Shouldn't have been able to get here")
        }
      }
      //a block: { <stmt1>...<stmt_n>, <expr> };
      ExprKind::Block(block, _) => {
        // set new scope when entering a block (span.hi refers to the ending brace of the block)
        let prev_scope = self.current_scope;
        let new_scope = self.tcx.sess.source_map().lookup_char_pos(rhs.span.hi()).line;
        self.current_scope = new_scope;
        self.current_scope = prev_scope;
        // then, if the block has a return expr
        match block.expr {
          Some(res_expr) => {
            self.match_rhs(lhs.clone(), res_expr, evt);
          }
          None => {}
        }
      }
      ExprKind::Unary(option, expr) => {
        match option {
          rustc_hir::UnOp::Deref => {
            let line_num = self.expr_to_line(&expr);
            let rhs_rap = self.fetch_rap(&expr);
            let res = match rhs_rap {
              Some(x) => {
                self.update_rap(&x, line_num);
                ResourceTy::Deref(x)
              }
              None => {
                ResourceTy::Anonymous
              }
            };
            self.add_ev(line_num, evt, lhs, res, false);
          },
          _ => {}
        }
      } 
      ExprKind::MethodCall(name_and_generic_args, rcvr, _,  _) => {
        let line_num = self.span_to_line(&rcvr.span);
        let fn_name = self.hirid_to_var_name(name_and_generic_args.hir_id).unwrap();
        let rhs_rap = self.raps.get(&fn_name).unwrap().rap.to_owned();
        self.add_ev(line_num, evt, lhs, ResourceTy::Value(rhs_rap), false);
      }
      // Struct intializer list:
      // ex struct = {a: <expr>, b: <expr>, c: <expr>}
      ExprKind::Struct(_qpath, expr_fields, _base) => { 
        let line_num = self.span_to_line(&rhs.span);
        self.add_ev(line_num, Evt::Bind, lhs.clone(), ResourceTy::Anonymous, false);
        for field in expr_fields.iter() {
            let new_lhs_name = format!("{}.{}", lhs.name(), field.ident.as_str());
            let field_rap = self.raps.get(&new_lhs_name).unwrap().rap.to_owned();
            let field_ty = self.tcx.typeck(field.expr.hir_id.owner).node_type(field.expr.hir_id);
            let is_copyable = field_ty.is_copy_modulo_regions(self.tcx, self.tcx.param_env(field.expr.hir_id.owner));
            let e = if field_ty.is_ref() {
              match field_ty.ref_mutability().unwrap() {
                Mutability::Not => Evt::Copy,
                Mutability::Mut => Evt::Move,
              }
            } else {
              match is_copyable {
                true => Evt::Copy, 
                false => Evt::Move
              }
            };
            self.match_rhs(ResourceTy::Value(field_rap), field.expr, e);
        }
      },

      ExprKind::Field(expr, id) => {
        match expr {
          Expr{kind: ExprKind::Path(QPath::Resolved(_,p)), ..} => {
            let line_num = self.span_to_line(&p.span);
            let name = self.tcx.hir().name(p.segments[0].hir_id).as_str().to_owned();
            let total_name = format!("{}.{}", name, id.as_str());
            let rhs_rap = self.raps.get(&total_name).unwrap().rap.to_owned();
            self.add_ev(line_num, evt, lhs, ResourceTy::Value(rhs_rap), false);
          }
          _ => panic!("unexpected field expr")
        }
      }
      // explicitly using return keyword
      // ex: return <expr>
      ExprKind::Ret(ret) => {
        match ret {
          Some(ret_expr) => {
            self.match_rhs(lhs, ret_expr, evt);
          }
          // returning void, nothing happens
          None => {}
        }
      },
      // TODO: implement conditional let bindings:
      // let x = if {} else {};
      // let x = match z {};
      ExprKind::If(_, if_block, else_block) => {
        self.match_rhs(lhs.clone(), &if_block, evt.clone());
        match else_block {
          Some(e) => self.match_rhs(lhs, &e, evt),
          None => {}
        }
      }
      ExprKind::DropTemps(exp) => {
        self.match_rhs(lhs, &exp, evt);
      }
      _ => {
        println!("unmatched rhs {:#?}", rhs);
      }
    }
  }

  // add GOS events for RAPs in the current scope  
  pub fn print_out_of_scope(&mut self){
    for (_, rap) in self.raps.clone().iter() {
      // need this to avoid duplicating out of scope events, this is due to the fact that RAPS is a map that lives over multiple fn ctxts
      if !rap.rap.is_fn() && rap.is_global {
        let mut duplicate = false;
        for (_l, e) in self.preprocessed_events.iter() {
          match e.is_gos_ev() {
            Some(r) => {
              if *r == rap.rap {
                duplicate = true;
                break;
              }
            }
            None => {}
          }
        }
        if !duplicate {
          self.add_external_event(rap.scope, ExternalEvent::GoOutOfScope { ro: rap.rap.clone(), id: *self.unique_id });
          *self.unique_id += 1;
        }
      }
    }
  }

  // Group loans for a certain lender into regions
  // A region is defined as an area where multiple loans on the same local are active at the same time
  pub fn get_regions(&self, h: &HashSet<String>) -> Vec<HashSet<String>> {
    let mut res: BTreeMap<usize, (usize, HashSet<String>)>  = BTreeMap::new();

    for borrower in h {
      let b_data = self.borrow_map.get(borrower).unwrap();
      let a_place = b_data.assigned_at;
      let k_place = b_data.lifetime;

      let mut c = res.upper_bound_mut(Bound::Included(&a_place));
      let mut to_replace: Option<(usize, (usize, HashSet<String>))> = None;
      match c.peek_prev() { // look to our left
        Some((_, (k, map))) => {
          if a_place < *k { // if current borrower was assigned in the same region
            *k = max(*k, k_place); // adjust region to encapsulate all lifetimes (extending the lifetime to the right)
            map.insert(borrower.clone());
          }
          else { // borrower belongs to a different region
            res.insert(a_place, (k_place, HashSet::from([borrower.clone()])));
          }
        }
        None => {
          match c.peek_next() { // look to our right
            Some((a, (k, map))) => {
              if *a < k_place { // need to do this because we can't mutate keys (would break BTreeMap invariants) 
              // although in our case it wouldn't matter because you still wouldn't be able to change the relative ordering of regions (try a proof by contradiction)
                map.insert(borrower.clone());
                to_replace = Some((*a, (max(*k, k_place), map.clone()))); // extending the lifetime to the left
              }
              else {
                res.insert(a_place, (k_place, HashSet::from([borrower.clone()])));
              }
            }
            None => {
              res.insert(a_place, (k_place, HashSet::from([borrower.clone()])));
            }
          }
        }
      }

      // if we need to replace a key
      match to_replace {
        Some((key, (k, map))) => {
          res.remove(&key);
          res.insert(a_place, (k, map));
        }
        None => {}
      }
    }

    let mut z: Vec<HashSet<String>> = Vec::new();
    for (_, (_, map)) in res.into_iter() {
      z.push(map);
    }
    z
  }
  
  // This is where we annotate all RefDie events that were not handled by the assignment operator
  pub fn print_lifetimes(&mut self){

    // first refine the loans that we've computed using MIR information
    // this is necessary for some cases where we can't know where a lifetime actually ends
    // see tests/basic_if7.rs for an example
    let mir_b_data = self.gather_borrow_data(&self.bwf);
    for (_name, data) in self.borrow_map.iter_mut() {
      for m_data in mir_b_data.iter() {
        match ExprVisitor::borrow_match(data, m_data) {
          Some(kill) => {
            data.lifetime = kill;
            break;
          }
          None => {}
        }
      }
    }
    
    info!("BORROW MAP {:#?}", self.borrow_map);
    //let lifetime_map = self.lifetime_map.clone();
    let mut ultimate_refs: HashSet<String> = HashSet::new();
    let lender_to_refs = self.get_non_anon_lenders();
    info!("lender to refs {:#?}", lender_to_refs);

    // loop through each lender's 'active' references and get the ultimate reference (the one with the longest lifetime)
    for (_, refs) in lender_to_refs.iter() {
      let mut max_lifetime = 0;
      let mut ultimate_ref: &str = &String::from("");
      // need to seperate borrows into regions for each lender
      let regions = self.get_regions(refs);
      // append an ultimate reference for each region
      for region in regions.iter() {
        for r in region {
          let lifetime = self.borrow_map.get(r).unwrap().lifetime;
          if lifetime > max_lifetime {
            max_lifetime = lifetime;
            ultimate_ref = r;
          }
        }
        ultimate_refs.insert(ultimate_ref.to_owned());
      }
    }

    // if a reference borrowed from an anonymous owner then it must be an ultimate ref
    for (name, ref_data) in self.borrow_map.iter() {
      match ref_data.lender {
        ResourceTy::Anonymous => {
          ultimate_refs.insert(name.to_owned());
        }
        _ => {}
      }
    }
    let b_map = self.borrow_map.clone();

    // sort by number of aliases so events are ordered in a proper cascading fashion
    let mut vec: Vec<(String, RefData)> = b_map.into_iter().collect();
    vec.sort_by(|a, b| b.1.aliasing.len().cmp(&a.1.aliasing.len()));

    // Add events
    for (k, RefData {lender: r_ty, assigned_at, lifetime, ref_mutability: ref_mut, aliasing: _}) in &vec {
      let from_ro = self.raps.get(k).unwrap().rap.to_owned();
      let to_ro = match r_ty {
        ResourceTy::Anonymous => ResourceTy::Deref(from_ro.clone()),
        _ => r_ty.clone()
      };
      // if ref is not an ultimate ref then it dies without returning a resource
      if !ultimate_refs.contains(k) { 
        self.add_external_event(*lifetime, ExternalEvent::RefDie { 
          from: ResourceTy::Value(from_ro.clone()), to: to_ro, num_curr_borrowers: self.get_borrowers(from_ro.name()).len() - 1, 
          id: *self.unique_id });
        *self.unique_id += 1;
      }
      else {
        // otherwise it does return a resource
        match ref_mut {
          true => {
            self.add_ev(*lifetime, Evt::MDie, to_ro, ResourceTy::Value(from_ro), false);
          }
          false => {
            self.add_ev(*lifetime, Evt::SDie, to_ro, ResourceTy::Value(from_ro), false);
          }
        }
      }
    }
  }
}