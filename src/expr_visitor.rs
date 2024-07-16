use rustc_middle::{
    mir::Body,
    ty::*,
  };
  use rustc_hir::{Expr, ExprKind, QPath, PatKind, Mutability, UnOp, StmtKind, Stmt, def::*, Pat, Path};
use rustc_utils::mir::mutability;
use rustc_utils::MutabilityExt;
  use std::cell::Ref;
use std::collections::{BTreeMap, HashMap, HashSet, VecDeque};
  use rustc_span::Span;
  // use aquascope::analysis::boundaries::PermissionsBoundary;
  use rustc_hir::{intravisit::Visitor, hir_id::HirId};
  use std::cmp::{Eq, PartialEq};
  use std::hash::Hash;
  use crate::expr_visitor_utils::*;
  use rustviz_lib::data::*;


#[derive(Debug, Clone)]
pub enum Evt {
  Bind,
  Copy,
  Move,
  SBorrow,
  MBorrow,
  SDie,
  MDie,
  PassBySRef,
  PassByMRef,
}

#[derive(Debug, Clone)]
pub struct RefData {
  pub lender: ResourceTy,
  pub lifetime: usize,
  pub ref_mutability: bool,
  pub aliasing: VecDeque<String>
}


pub struct ExprVisitor<'a, 'tcx:'a> {
  pub tcx: TyCtxt<'tcx>,
  pub mir_body: &'a Body<'tcx>,
  //pub boundary_map: HashMap<rustc_span::BytePos,PermissionsBoundary>,
  pub borrow_map: HashMap<String, RefData>, //lendee -> (lender, lifetime, ref mutability) 
  pub lenders: HashMap<String, HashSet<ResourceTy>>, //todo, change this logic to shared ptrs when I feel like it
  pub raps: &'a mut HashMap<String, (ResourceAccessPoint, usize)>,
  pub current_scope: usize,
  pub analysis_result : HashMap<usize, Vec<String>>,
  pub event_line_map: &'a mut BTreeMap<usize, Vec<ExternalEvent>>,
  pub preprocessed_events: &'a mut Vec<(usize, ExternalEvent)>,
  pub rap_hashes: usize,
  pub source_map: & 'a BTreeMap<usize, String>,
  pub annotated_lines: & 'a mut BTreeMap<usize, Vec<String>>,
  pub id_map: & 'a mut HashMap<String, usize>,
}

impl<'a, 'tcx> ExprVisitor<'a, 'tcx>{
  pub fn expr_to_line(&self,expr:&Expr)->usize{
    self.tcx.sess.source_map().lookup_char_pos(expr.span.lo()).line
  }

  pub fn span_to_line(&self,span:&Span)->usize{
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
  pub fn hirid_to_var_name(&self,id:HirId)->Option<String>{
    let long_name = self.tcx.hir().node_to_string(id);
    extract_var_name(&long_name)
  }

  pub fn is_return_type_ref(&self,fn_expr:&Expr)->bool{
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

  pub fn update_lifetime(&mut self, name: &String, line:usize){
    self.borrow_map.get_mut(name).unwrap().lifetime = line;
    let aliasing = self.borrow_map.get(name).unwrap().aliasing.clone();
    for r in aliasing.iter() {
      self.update_lifetime(r, line);
    }
  }

  pub fn add_owner(&mut self, name: String, mutability: bool) {
    self.add_rap(ResourceAccessPoint::Owner(Owner{name: name, hash: self.rap_hashes as u64, is_mut: mutability}));
  }

  pub fn add_ref(&mut self, name: String, ref_mutability: bool, lhs_mut: bool, line_num: usize, lender: ResourceTy, alia: VecDeque<String>) {
    match ref_mutability {
      true => {
        self.add_mut_ref(name.clone(), lhs_mut);
      }
      false => { 
        self.add_static_ref(name.clone(), lhs_mut);
      }
    }
    self.borrow_map.insert(name.clone(), RefData { lender: lender.clone(), lifetime: line_num, ref_mutability: ref_mutability, aliasing: alia });
  }
  
  pub fn add_static_ref(&mut self, name: String, mutability: bool) {
    self.add_rap(ResourceAccessPoint::StaticRef(StaticRef { name: name, hash: self.rap_hashes as u64, is_mut: mutability }));
  }

  pub fn add_mut_ref(&mut self, name: String, mutability: bool) {
    self.add_rap(ResourceAccessPoint::MutRef(MutRef { name: name, hash: self.rap_hashes as u64, is_mut: mutability }));
  }

  pub fn add_fn(&mut self, name: String) {
    self.add_rap(ResourceAccessPoint::Function(Function { name: name, hash: self.rap_hashes as u64 }));
  }

  pub fn add_struct(&mut self, name: String, owner: u64, mem: bool, mutability: bool) {
    self.add_rap(ResourceAccessPoint::Struct(Struct { 
      name: name, 
      hash: self.rap_hashes as u64, 
      owner: owner, 
      is_mut: mutability, 
      is_member: mem }));
  }

  pub fn add_rap(&mut self, r: ResourceAccessPoint) {
    self.raps.entry(r.name().to_string()).or_insert_with(|| {self.rap_hashes += 1; (r, self.current_scope)});
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
          // do nothing case
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

  pub fn ext_ev_of_evt(&self, evt: Evt, lhs: ResourceTy, rhs: ResourceTy) -> ExternalEvent{
    match evt {
      Evt::Bind => ExternalEvent::Bind { from: rhs, to: lhs },
      Evt::Copy => ExternalEvent::Copy { from: rhs, to: lhs },
      Evt::Move => ExternalEvent::Move { from: rhs, to: lhs },
      Evt::SBorrow => ExternalEvent::StaticBorrow { from: rhs, to: lhs },
      Evt::MBorrow => ExternalEvent::MutableBorrow { from: rhs, to: lhs },
      Evt::PassBySRef => ExternalEvent::PassByStaticReference { from: rhs, to: lhs },
      Evt::PassByMRef => ExternalEvent::PassByMutableReference { from: rhs, to: lhs },
      Evt::SDie =>  ExternalEvent::StaticDie { from: rhs, to: lhs },
      Evt::MDie => ExternalEvent::MutableDie { from: rhs, to: lhs }
    }
  }

  pub fn add_ev(&mut self, line_num: usize, evt: Evt, lhs: ResourceTy, rhs: ResourceTy) {
    self.add_external_event(line_num, self.ext_ev_of_evt(evt, lhs, rhs));
  }

  pub fn resource_of_lhs(&mut self, expr: &'tcx Expr) -> ResourceTy {
    match expr.kind {
      ExprKind::Path(QPath::Resolved(_, p)) => {
        let name = self.tcx.hir().name(p.segments[0].hir_id).as_str().to_owned();
        ResourceTy::Value(self.raps.get(&name).unwrap().0.to_owned())
      }
      ExprKind::Field(expr, ident) => {
        match expr {
          Expr{kind: ExprKind::Path(QPath::Resolved(_,p)), ..} => {
            let name = self.tcx.hir().name(p.segments[0].hir_id).as_str().to_owned();
            let total_name = format!("{}.{}", name, ident.as_str());
            ResourceTy::Value(self.raps.get(&total_name).unwrap().0.to_owned())
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

  pub fn get_non_anon_lenders(&self) -> HashMap<String, HashSet<String>> {
    let b_map = self.borrow_map.clone();
    let mut res: HashMap<String, HashSet<String>> = HashMap::new();
    for (k, v) in b_map.iter() {
      match v.lender {
        ResourceTy::Anonymous => {},
        _ => {
          let lender = v.lender.real_name();
          res.entry(lender.clone()).and_modify(|lendees| { lendees.insert(k.to_owned()); }).or_insert(HashSet::from([k.to_owned()]));
        }
      }
    }
    res
  }

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

  pub fn get_rap(&self, expr: &'tcx Expr) -> ResourceTy {
    match expr.kind {
      ExprKind::Path(QPath::Resolved(_,p)) => {
        let name = self.tcx.hir().name(p.segments[0].hir_id).as_str().to_owned();
        ResourceTy::Value(self.raps.get(&name).unwrap().0.to_owned())
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
        ResourceTy::Value(self.raps.get(&fn_name).unwrap().0.to_owned())
      }
      ExprKind::MethodCall(name_and_generic_args, ..) => {
        let fn_name = self.hirid_to_var_name(name_and_generic_args.hir_id).unwrap();
        ResourceTy::Value(self.raps.get(&fn_name).unwrap().0.to_owned())
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
            ResourceTy::Value(self.raps.get(&total_name).unwrap().0.to_owned())
          }
          _ => { panic!("unexpected field expr") }
        } 
      }
      _ => ResourceTy::Anonymous
    }
  }

  pub fn match_arg(&mut self, arg: &'tcx Expr, fn_name: String) {
    self.add_fn(fn_name.clone());
    let line_num = self.expr_to_line(&arg);
    let tycheck_results = self.tcx.typeck(arg.hir_id.owner);
    let arg_ty = tycheck_results.node_type(arg.hir_id);
    let is_copyable = arg_ty.is_copy_modulo_regions(self.tcx, self.tcx.param_env(arg.hir_id.owner));
    let from_ro = self.get_rap(arg);
    let to_ro = ResourceTy::Value(self.raps.get(&fn_name).unwrap().0.to_owned());
    if arg_ty.is_ref() {
      match arg_ty.ref_mutability().unwrap() {
        Mutability::Not => self.add_ev(line_num, Evt::PassBySRef, to_ro, from_ro),
        Mutability::Mut => self.add_ev(line_num, Evt::PassByMRef, to_ro, from_ro)
      }
    }
    else {
      match is_copyable {
        true => self.add_ev(line_num, Evt::Copy, to_ro, from_ro),
        false => self.add_ev(line_num, Evt::Move, to_ro, from_ro)
      }
    }
  }

  pub fn find_lender(&self, rhs: &'tcx Expr) -> ResourceTy {
    match rhs.kind {
      ExprKind::Path(QPath::Resolved(_,p)) => {
        let name = self.tcx.hir().name(p.segments[0].hir_id).as_str().to_owned();
        if self.borrow_map.contains_key(&name) {
          self.borrow_map.get(&name).unwrap().to_owned().lender
        }
        else{
          ResourceTy::Value(self.raps.get(&name).unwrap().0.to_owned())
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
              ResourceTy::Value(self.raps.get(&name).unwrap().0.to_owned())
            }
          }
          _ => { panic!("unexpected field expr") }
        } 
      }
    _ => { panic!("unexpected rhs lender expr {:#?}", rhs) }
    }
  }

  pub fn ty_is_special_owner (&self, t:Ty<'tcx>) -> bool {
    match &*t.sort_string(self.tcx) {
      "`std::string::String`" => { true }
      _ => {
        println!("t {}", &*t.sort_string(self.tcx));
        false
      }
    }
  }

  pub fn lender_update(&mut self, new_lender: ResourceTy, curr_borrower: ResourceTy) {
    let old_lender = self.borrow_map.get(&curr_borrower.real_name()).unwrap().lender.clone();
    self.lenders.get_mut(&old_lender.real_name()).unwrap().remove(&curr_borrower);
    self.borrow_map.get_mut(&curr_borrower.real_name()).unwrap().lender = new_lender.clone();
    match new_lender {
      ResourceTy::Anonymous => {}
      _ => {
        self.lenders.entry(new_lender.name()).
          and_modify(|v| {v.insert(curr_borrower.clone());}).
            or_insert(HashSet::from([curr_borrower]));
      }
    }
  }

  pub fn new_aliasing_data(&self, r: Option<ResourceAccessPoint>) -> BTreeMap<usize, String> {
    let mut aliasing: BTreeMap<usize, String> = BTreeMap::new();
    match r {
      Some(r) => {
        if r.is_ref() {
          aliasing.insert(1, r.name().to_owned());
          println!("r {:#?}", r);
          // for (k, v) in self.borrow_map.get(r.name()).unwrap().aliasing.iter() {
          //   aliasing.insert(*k + 1, v.to_owned());
          // }
        }
      }
      None => println!("aliasing something unknown"),
    }
    aliasing
  }

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

  pub fn define_lhs(&mut self, lhs_name: String, lhs_mutability: bool, rhs_expr: &'tcx Expr, lhs_ty: Ty <'tcx>) {
    if lhs_ty.is_ref() {
      let (lender, aliasing) = self.get_ref_data(&rhs_expr);
      self.add_ref(lhs_name.clone(), 
      bool_of_mut(lhs_ty.ref_mutability().unwrap()), 
      lhs_mutability, 
      self.expr_to_line(&rhs_expr),
      lender,
      aliasing);
    }
    else if lhs_ty.is_adt() && !self.ty_is_special_owner(lhs_ty) {
      match lhs_ty.ty_adt_def().unwrap().adt_kind() {
        AdtKind::Struct => {
          let owner_hash = self.rap_hashes as u64;
          self.add_struct(lhs_name.clone(), owner_hash, false, lhs_mutability);
          for field in lhs_ty.ty_adt_def().unwrap().all_fields() {
            let field_name = format!("{}.{}", lhs_name.clone(), field.name.as_str());
            self.add_struct(field_name, owner_hash, true, lhs_mutability);
          }
        },
        AdtKind::Union => {
          panic!("lhs union not implemented yet")
        },
        AdtKind::Enum => {
          self.add_owner(lhs_name, lhs_mutability);
        }
      }
    }
    else if lhs_ty.is_fn() {
      panic!("cannot have fn as lhs of expr");
    }
    else { 
      self.add_owner(lhs_name, lhs_mutability);
    }
  }

  pub fn fetch_rap(&self, expr: &'tcx Expr) -> Option<ResourceAccessPoint> {
    match expr.kind {
      ExprKind::Call(..) | ExprKind::Binary(..) | ExprKind::Lit(_) | ExprKind::MethodCall(..) => None,
      ExprKind::Path(QPath::Resolved(_,p)) => {
        let name = self.tcx.hir().name(p.segments[0].hir_id).as_str().to_owned();
        Some(self.raps.get(&name).unwrap().0.to_owned())
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
            Some(self.raps.get(&total_name).unwrap().0.to_owned())
          }
          _ => { panic!("unexpected field expr") }
        } 
      }
      _ => None
    }
  }

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
      _ => panic!("unexpected pat kind {:#?}", pat)
    }
  }

  pub fn get_decl_of_pat(&mut self, pat: &Pat, ty_results: &TypeckResults, parent: &ResourceTy, scope: usize) -> HashSet<ResourceAccessPoint> {
    let mut res = HashSet::new();
    match pat.kind {
      PatKind::TupleStruct(_p, tuple_members, _) => {
        for p in tuple_members.iter() {
          let other: HashSet<ResourceAccessPoint> = self.get_decl_of_pat(p, ty_results, parent, scope);
          res = res.union(&other).cloned().collect();
        }

      }
      PatKind::Binding(mode, id, ident, _) => {
        // hacky fix for GoOutOfScope events
        let old_scope = self.current_scope;
        self.current_scope = scope; 
        // add raps that appear in binding
        let muta = bool_of_mut(mode.1);
        let line_num = self.span_to_line(&ident.span);
        let ty = ty_results.node_type(id);
        let name = ident.to_string();
        if ty.is_ref() {
          let ref_mutability = bool_of_mut(ty.ref_mutability().unwrap());
          self.add_ref(name.clone(), ref_mutability, muta, line_num, parent.clone(), VecDeque::new());
        }
        else if ty.is_adt() {
          match ty.ty_adt_def().unwrap().adt_kind() {
            AdtKind::Struct => {
              let owner_hash = self.rap_hashes as u64;
              self.add_struct(name.clone(), owner_hash, false, muta);
              for field in ty.ty_adt_def().unwrap().all_fields() {
                let field_name = format!("{}.{}", name, field.name.as_str());
                self.add_struct(field_name, owner_hash, true, muta);
              }
            },
            AdtKind::Union => {
              panic!("union not implemented yet")
            },
            AdtKind::Enum => {
              self.add_owner(name.clone(), muta);
            }
          }
          
        }
        else {
          self.add_owner(name.clone(), muta);
        }
        self.current_scope = old_scope;

        res.insert(self.raps.get(&name).unwrap().0.to_owned());
      }
      _ => {}
    }
    res

  }

  pub fn get_decl_of_block(&self, block: & 'tcx rustc_hir::Block) -> HashSet<ResourceAccessPoint>{
    let mut res:HashSet<ResourceAccessPoint> = HashSet::new();
    for stmt in block.stmts.iter() {
      res = res.union(&self.get_decl_of_stmt(&stmt)).cloned().collect();
    }
    res
  }

  pub fn get_decl_of_stmt(&self, stmt: &'tcx Stmt) -> HashSet<ResourceAccessPoint> {
    let mut res = HashSet::new();
    match stmt.kind {
      StmtKind::Let(ref local) => {
        let name = match local.pat.kind {
          PatKind::Binding(_, _, ident, _) => {
            ident.to_string()
          }
          _ => panic!("unexpected binding pattern")
        };
        res.insert(self.raps.get(&name).unwrap().0.to_owned());
      }
      _ => {}
    }
    res
  }

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
            HashSet::from([self.raps.get(&name).unwrap().0.to_owned()])
          }
        }
      }
      ExprKind::Field(expr, ident) => {
        match expr {
          Expr{kind: ExprKind::Path(QPath::Resolved(_,p)), ..} => {
            let name = self.tcx.hir().name(p.segments[0].hir_id).as_str().to_owned();
            let field_name: String = ident.as_str().to_owned();
            let total_name = format!("{}.{}", name, field_name);
            HashSet::from([self.raps.get(&total_name).unwrap().0.to_owned()])
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
        let mut res = HashSet::from([self.raps.get(&rcvr_name).unwrap().0.to_owned()]);
        for a_expr in args.iter() {
          res = res.union(&self.get_live_of_expr(&a_expr)).cloned().collect();
        }
        res
      }
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
        res
      }
      ExprKind::If(guard_expr, if_expr, else_expr) => {
        let mut res: HashSet<ResourceAccessPoint> = HashSet::new();
        res = res.union(&self.get_live_of_expr(&guard_expr)).cloned().collect();
        res = res.union(&self.get_live_of_expr(&if_expr)).cloned().collect();
        match else_expr {
          Some(e) => {
            res = res.union(&self.get_live_of_expr(&e)).cloned().collect();
          }
          None => {
            
          }
        }
        res

      }
      _ => {
        HashSet::new()
      }
    }
  }

  // TODO: will probably need to subtract the set of variables that are bound inside a block and those from the set of live variables
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
  pub fn filter_ev(&self, (line_num, ev): &(usize, ExternalEvent), split: usize, merge: usize) -> bool {
    if *line_num <= merge && *line_num >= split {
      true
    }
    else {
      false
    }
  }

  pub fn filter_ev_match(&self, (line_num, ev): &(usize, ExternalEvent), split: usize, merge: usize, liveness: &HashSet<ResourceAccessPoint>) -> bool {
    if *line_num <= merge && *line_num >= split {
      let (from, to) = ResourceAccessPoint_extract(ev);
      let ev_is_live = match (from.extract_rap(), to.extract_rap()) {
        (Some(x), Some(y)) => {
          liveness.contains(x) || liveness.contains(y)
        }
        (Some(x), _) | (_, Some(x)) => {
          liveness.contains(x)
        }
        _ => false
      };
      ev_is_live
    }
    else {
      false
    }
  }
 
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
        let rhs_rap = self.raps.get(&rhs_name).unwrap().0.to_owned();
        self.update_rap(&rhs_rap, line_num);
        self.add_ev(line_num, evt, lhs, ResourceTy::Value(rhs_rap));
      },
      // fn_expr: resolves to function itself (Path)
      // second arg, is a list of args to the function
      ExprKind::Call(fn_expr, _) => {
        let line_num = self.span_to_line(&fn_expr.span);
        let fn_name = self.hirid_to_var_name(fn_expr.hir_id).unwrap();
        println!("fn_name {:#?}", fn_name);
        let rhs_rap = self.raps.get(&fn_name).unwrap().0.to_owned();
        self.add_ev(line_num, evt, lhs, ResourceTy::Value(rhs_rap));
      },
      
      ExprKind::Lit(_) | ExprKind::Binary(..) | // Any type of literal on RHS implies a bind
      ExprKind::Unary(UnOp::Neg, _) | // ~<expr>
      ExprKind::Unary(UnOp::Not, _) // !<expr>
      => {
        let line_num = self.span_to_line(&rhs.span);
        self.add_ev(line_num, Evt::Bind, lhs, ResourceTy::Anonymous);
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
          Some(Mutability::Not) => self.add_ev(line_num, Evt::SBorrow, lhs, res),
          Some(Mutability::Mut) => self.add_ev(line_num, Evt::MBorrow, lhs, res),
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
            self.add_ev(line_num, evt, lhs, res);
          },
          _ => {}
        }
      } 
      ExprKind::MethodCall(name_and_generic_args, rcvr, _,  _) => {
        let line_num = self.span_to_line(&rcvr.span);
        let fn_name = self.hirid_to_var_name(name_and_generic_args.hir_id).unwrap();
        //let type_check = self.tcx.typeck(name_and_generic_args.hir_id.owner);
        let rhs_rap = self.raps.get(&fn_name).unwrap().0.to_owned();
        self.add_ev(line_num, evt, lhs, ResourceTy::Value(rhs_rap));
      }
      // Struct intializer list:
      // ex struct = {a: <expr>, b: <expr>, c: <expr>}
      ExprKind::Struct(_qpath, expr_fields, _base) => { 
        let line_num = self.span_to_line(&rhs.span);
        self.add_ev(line_num, Evt::Bind, lhs.clone(), ResourceTy::Anonymous);
        for field in expr_fields.iter() {
            let new_lhs_name = format!("{}.{}", lhs.name(), field.ident.as_str());
            let field_rap = self.raps.get(&new_lhs_name).unwrap().0.to_owned();
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
            let rhs_rap = self.raps.get(&total_name).unwrap().0.to_owned();
            self.add_ev(line_num, evt, lhs, ResourceTy::Value(rhs_rap));
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

  // pub fn handle_ret(&mut self, )
  
  pub fn print_out_of_scope(&mut self){
    for (_, rap) in self.raps.clone().iter() {
      // need this to avoid duplicating out of scope events, this is due to the fact that RAPS is a map that lives over multiple fn ctxts
      if !rap.0.is_fn() && !self.preprocessed_events.contains(&(rap.1, ExternalEvent::GoOutOfScope { ro: rap.0.clone() })){ 
        self.add_external_event(rap.1, ExternalEvent::GoOutOfScope { ro: rap.0.clone() })
        
      }
    }
  }
  
  pub fn print_lifetimes(&mut self){
    //println!("LIFETIME MAP: {:#?}", self.lifetime_map);
    println!("BORROW MAP {:#?}", self.borrow_map);
    //let lifetime_map = self.lifetime_map.clone();
    let mut ultimate_refs: HashSet<String> = HashSet::new();
    let lender_to_refs = self.get_non_anon_lenders();
    println!("lender to refs {:#?}", lender_to_refs);

    // loop through each lender's 'active' references and get the ultimate reference (the one with the longest lifetime)
    for (_, refs) in lender_to_refs.iter() {
      let mut max_lifetime = 0;
      let mut ultimate_ref: &str = &String::from("");
      for r in refs {
        let lifetime = self.borrow_map.get(r).unwrap().lifetime;
        if lifetime > max_lifetime {
          max_lifetime = lifetime;
          ultimate_ref = r;
        }
      }
      ultimate_refs.insert(ultimate_ref.to_owned());
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

    println!("ultimate refs {:#?}", ultimate_refs);
    let b_map = self.borrow_map.clone();

    // sort by number of aliases so events are ordered in a proper cascading fashion
    let mut vec: Vec<(String, RefData)> = b_map.into_iter().collect();
    vec.sort_by(|a, b| b.1.aliasing.len().cmp(&a.1.aliasing.len()));

    // Add events
    for (k, RefData {lender: r_ty, lifetime, ref_mutability: ref_mut, aliasing: _}) in &vec {
      let from_ro = self.raps.get(k).unwrap().0.to_owned();
      let to_ro = match r_ty {
        ResourceTy::Anonymous => ResourceTy::Deref(from_ro.clone()),
        _ => r_ty.clone()
      };
      // if ref is not an ultimate ref then it dies without returning a resource
      if !ultimate_refs.contains(k) { 
        self.add_external_event(*lifetime, ExternalEvent::RefDie { 
          from: ResourceTy::Value(from_ro.clone()), to: to_ro, num_curr_borrowers: self.get_borrowers(from_ro.name()).len() - 1 });
      }
      else {
        // otherwise it does return a resource
        match ref_mut {
          true => {
            self.add_ev(*lifetime, Evt::MDie, to_ro, ResourceTy::Value(from_ro));
          }
          false => {
            self.add_ev(*lifetime, Evt::SDie, to_ro, ResourceTy::Value(from_ro));
          }
        }
      }
    }
  }
}