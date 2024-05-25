use rustc_middle::{
    mir::Body,
    ty::{TyCtxt,Ty},
  };
  use rustc_hir::{Expr, ExprKind, QPath, Path, Mutability};
  use std::collections::{HashMap, BTreeMap, HashSet};
  use rustc_span::Span;
  use aquascope::analysis::boundaries::PermissionsBoundary;
  use rustc_hir::{intravisit::Visitor, hir_id::HirId};
  use std::cmp::{Eq, PartialEq};
  use std::hash::Hash;
  use crate::expr_visitor_utils::*;
  use rustviz_library::Rustviz;
  use rustviz_lib::data::*;


#[derive(Eq, PartialEq, Hash, Clone, Debug)]
pub struct AccessPoint {
  pub mutability: Mutability,
  pub name:String,
  pub members: Option<Vec<AccessPoint>>,
}

#[derive(Eq, PartialEq,Hash, Clone, Debug)]
pub enum AccessPointUsage{
  Owner(AccessPoint), 
  MutRef(AccessPoint),
  StaticRef(AccessPoint),
  Struct(AccessPoint, Vec<AccessPoint>),
  Function(String),
}

#[derive(Eq, PartialEq,Hash, Clone, Debug)]
pub enum Reference{
  Static(String),
  Mut(String),
}

#[derive(Eq, PartialEq,Hash, Clone, Debug)]
pub enum LhsTy {
  Unknown,
  Deref,
  Field,
}


pub struct ExprVisitor<'a, 'tcx:'a> {
  pub tcx: TyCtxt<'tcx>,
  pub mir_body: &'a Body<'tcx>,
  pub boundary_map: HashMap<rustc_span::BytePos,PermissionsBoundary>,
  pub mutability_map: HashMap<String,Mutability>, // map owner name to mutablility status
  pub lifetime_map: HashMap<Reference,usize>,
  pub borrow_map: HashMap<String, Option<ResourceAccessPoint>>, 
  pub access_points: HashMap<AccessPointUsage, usize>,
  pub raps: &'a mut HashMap<String, ResourceAccessPoint>,
  pub current_scope: usize,
  pub analysis_result : HashMap<usize, Vec<String>>,
  pub owners: Vec<AccessPointUsage>,
  pub name_to_access_point: HashMap<String, AccessPointUsage>,
  pub event_line_map: & 'a mut BTreeMap<usize, String>,
  pub event_line_map2: &'a mut BTreeMap<usize, Vec<ExternalEvent>>,
  pub preprocessed_events: &'a mut Vec<(usize, ExternalEvent)>,
  pub rap_hashes: usize,
  pub source_map: & 'a BTreeMap<usize, String>,
  pub annotated_lines: & 'a mut BTreeMap<usize, Vec<String>>,
  pub hash_map: & 'a mut HashMap<String, usize>,
  pub hashes: usize
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

  pub fn is_return_type_struct(&self, fn_expr: &Expr) -> bool {
    false
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

  pub fn update_lifetime(&mut self, reference:Reference, line:usize){
    if self.lifetime_map.contains_key(&reference){
      if let Some(old_line)=self.lifetime_map.get(&reference){
        if *old_line<line {
          self.lifetime_map.insert(reference, line);
        }
      }
    }
    else{
      self.lifetime_map.insert(reference, line);
    }
  }

  pub fn add_access_point(&mut self, a: AccessPointUsage, name: String) {
    self.access_points.insert(a.clone(), self.current_scope);
    self.name_to_access_point.insert(name.to_owned(), a.clone());
    self.owners.push(a);
  }

  pub fn add_owner(&mut self, name: String, mutability: bool) {
    self.add_rap(ResourceAccessPoint::Owner(Owner{name: name, hash: self.rap_hashes as u64, is_mut: mutability}));
  }

  pub fn add_ref(&mut self, name: String, mutability: bool) {
    match mutability {
      true => { self.add_mut_ref(name) }
      false => { self.add_static_ref(name) }
    }
  }
  
  pub fn add_static_ref(&mut self, name: String) {
    self.add_rap(ResourceAccessPoint::StaticRef(StaticRef { name: name, hash: self.rap_hashes as u64, is_mut: false }));
  }

  pub fn add_mut_ref(&mut self, name: String) {
    self.add_rap(ResourceAccessPoint::MutRef(MutRef { name: name, hash: self.rap_hashes as u64, is_mut: true }));
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
    self.raps.entry(r.name().to_string()).or_insert_with(|| {self.rap_hashes += 1; r});
  }

  pub fn add_event(&mut self, line_num: usize, event: String) {
    self.analysis_result
    .entry(line_num)
    .or_insert(Vec::new())
    .push(event.clone());

    // stuff for testing utils
    if let Some(value) = self.event_line_map.get(&line_num) {
      if value.contains("//") { // appending to same line
        self.event_line_map.entry(line_num).and_modify(|ev| {ev.push_str(&(", ".to_owned() + &event));});
      }
      else { // first thing in a line
        self.event_line_map.entry(line_num).and_modify(|ev| {ev.push_str(&("// !{ ".to_owned() + &event));});
      }
    }
  }

  pub fn add_external_event(&mut self, line_num: usize, evt: ExternalEvent) {
    self.preprocessed_events.push((line_num, evt.clone()));
    let v = self.event_line_map2.get_mut(&line_num).unwrap();
    v.push(evt);
  }

  pub fn match_args(&mut self, arg: &'tcx Expr, fn_name:String) {
    let line_num = self.span_to_line(&arg.span);
    // add callee no matter what
    self.add_access_point(AccessPointUsage::Function(fn_name.clone()), fn_name.clone());
    self.add_fn(fn_name.clone());
    let fn_rap = self.raps.get(&fn_name).unwrap();
    match arg.kind {
      // arg is variable
      ExprKind::Path(QPath::Resolved(_,p))=>{
        let bytepos=p.span.lo();
        let boundary=self.boundary_map.get(&bytepos);
        let name: String = self.tcx.hir().name(p.segments[0].hir_id).as_str().to_owned();
        let arg_rap = self.raps.get(&name).unwrap();
        if let Some(boundary) = boundary {
          let expected=boundary.expected;
          if expected.drop{
            self.add_external_event(line_num, ExternalEvent::Move { from: Some(arg_rap.to_owned()), to: Some(fn_rap.to_owned()) });
            self.add_event(line_num,format!("Move({}->{}())", name, fn_name));
          }
          else if expected.write{           
            self.add_external_event(line_num, ExternalEvent::PassByMutableReference { from: Some(arg_rap.to_owned()), to: Some(fn_rap.to_owned()) });               
            self.add_event(line_num,format!("PassByMutableReference({}->{}())", name, fn_name));
            self.update_lifetime(Reference::Mut(name), line_num);
          }
          else if expected.read{
            self.add_external_event(line_num, ExternalEvent::PassByStaticReference { from: Some(arg_rap.to_owned()), to: Some(fn_rap.to_owned()) });      
            self.add_event(line_num,format!("PassByStaticReference({}->{}())", name, fn_name));
            self.update_lifetime(Reference::Static(name), line_num);
          }
        }
      }
      ExprKind::AddrOf(_,mutability,expr)=>{        
        self.match_args(expr, fn_name);
      }
      ExprKind::Call(fn_expr, fn_args) => { //functions can be parameters too
        let callee_name= self.hirid_to_var_name(fn_expr.hir_id).unwrap();
        // generate annotations for function call
        (callee_name.clone(), fn_expr.span, true);
        for a in fn_args.iter() {
          self.match_args(a, callee_name.clone());
        }
        return;
        // TODO: implement a way to visualize relationship from an anonymous owner/RAP -> 
        // if return type is not a reference
        if !self.is_return_type_ref(fn_expr){
          if self.is_return_type_copyable(fn_expr) {
            self.add_event(line_num, format!("Copy({}()->{}())", callee_name, fn_name));
          }
          else {
            self.add_event(line_num, format!("Move({}()->{}())", callee_name, fn_name));
          }
        }
        else {
        // return type is a reference
          if let Some(return_type)=self.return_type_of(fn_expr){
            if let Some(mutability)=return_type.ref_mutability(){
              match mutability{
                // if rhs is mutable ref then a move must occur
                Mutability::Mut=>{
                  self.add_event(line_num, format!("Move({}()->{}())", callee_name, fn_name));
                }
                // Reference should be copied
                Mutability::Not=>{
                  self.add_event(line_num, format!("Copy({}()->{}())", callee_name, fn_name));
                }
              }
            }
          }
        }
      }
      ExprKind::Unary(option, expr) => {
        match option {
          rustc_hir::UnOp::Deref => {
            match expr.kind {
              ExprKind::Path(QPath::Resolved(_,p))=>{
                // TODO: implement a way to visualize events between/to dereferences
                let bytepos=arg.span.lo(); // small discrepancy with boundary map, bytePos corresponds to bytePos of deref operator not path
                let boundary=self.boundary_map.get(&bytepos);
                if let Some(boundary) = boundary {
                  let expected=boundary.expected;
                  let name = self.tcx.hir().name(p.segments[0].hir_id).as_str().to_owned();
                  if expected.drop{
                    self.add_event(line_num,format!("Move({}->{}())", name, fn_name));
                  }
                  else if expected.write{                          
                    self.add_event(line_num,format!("PassByMutableReference({}->{}())", name, fn_name));
                    self.update_lifetime(Reference::Mut(name), line_num);
                  }
                  else if expected.read{
                    self.add_event(line_num,format!("PassByStaticReference({}->{}())", name, fn_name));
                    self.update_lifetime(Reference::Static(name), line_num);
                  }
                }
              }
              _ => {} // todo: implement other types of derefs, only handling paths for now
            }
          }
          _ => {
            self.match_args( expr, fn_name);
          }
        }
      },
      ExprKind::Field(expr, id) => {
        match expr {
          Expr{kind: ExprKind::Path(QPath::Resolved(_,p)), ..} => {
            let bytepos=arg.span.lo(); // small discrepancy with boundary map, bytePos corresponds to bytePos of deref operator not path
            let boundary=self.boundary_map.get(&bytepos);
            if let Some(boundary) = boundary {
              let expected=boundary.expected;
              let name = self.tcx.hir().name(p.segments[0].hir_id).as_str().to_owned();
              let field_name: String = id.as_str().to_owned();
              let mem_name = format!("{}.{}", name, field_name);
              let arg_rap = self.raps.get(&mem_name).unwrap();
              if expected.drop{
                self.add_external_event(line_num, ExternalEvent::Move { from: Some(arg_rap.to_owned()), to: Some(fn_rap.to_owned()) });
                self.add_event(line_num,format!("Move({}->{}())", name, fn_name));
              }
              else if expected.write{           
                self.add_external_event(line_num, ExternalEvent::PassByMutableReference { from: Some(arg_rap.to_owned()), to: Some(fn_rap.to_owned()) });               
                self.add_event(line_num,format!("PassByMutableReference({}->{}())", name, fn_name));
                self.update_lifetime(Reference::Mut(name), line_num);
              }
              else if expected.read{
                self.add_external_event(line_num, ExternalEvent::PassByStaticReference { from: Some(arg_rap.to_owned()), to: Some(fn_rap.to_owned()) });      
                self.add_event(line_num,format!("PassByStaticReference({}->{}())", name, fn_name));
                self.update_lifetime(Reference::Static(name), line_num);
              }
            }
          }
          _ => { println!("wacky struct expr")}
        }
      }
      _=>{ println!("unmatched arg") }
    }
  }

  pub fn define_lhs(&mut self, lhs_name: String, lhs_mutability: bool, rhs: &'tcx Expr) {
    match rhs.kind {
      ExprKind::Path(QPath::Resolved(_, p)) => {
        let line_num = self.span_to_line(&p.span);
        let rhs_name: String = self.tcx.hir().name(p.segments[0].hir_id).as_str().to_owned();
        let rhs_rap = self.raps.get(&rhs_name).unwrap().to_owned();
        if rhs_rap.is_owner() || rhs_rap.is_member() {
          self.add_owner(lhs_name, lhs_mutability);
        }
        else if rhs_rap.is_struct() {
          self.add_struct(lhs_name, self.rap_hashes as u64 , false, lhs_mutability);
        }
        else if rhs_rap.is_ref() {
          self.add_ref(lhs_name, lhs_mutability);
          self.borrow_map.insert(lhs_name.clone(), self.borrow_map.get(&rhs_name).unwrap().clone());
          if rhs_rap.is_mutref() {
            self.update_lifetime(Reference::Mut(lhs_name), line_num);
          }
          else {
            self.update_lifetime(Reference::Static(lhs_name), line_num);
          }
        }
      }
      ExprKind::Call(fn_expr, _) => {
        let line_num = self.span_to_line(&fn_expr.span);
        let fn_name = self.hirid_to_var_name(fn_expr.hir_id).unwrap();
        self.add_fn(fn_name);
        let rhs_rap = self.raps.get(&fn_name).unwrap().to_owned();
        
      }
    }
  }

  // match the Right-hand-side of an expression
  // TODO: need to modify this function for when lhs is a field, dereference
  pub fn match_rhs(
    &mut self, 
    lhs_name: String, 
    lhs_mutability: bool, 
    rhs:&'tcx Expr, 
    is_deref: bool){
    match rhs.kind {
      ExprKind::Path(QPath::Resolved(_,p)) => {
        let line_num = self.span_to_line(&p.span);
        let bytepos=p.span.lo();
        let rhs_name: String = self.tcx.hir().name(p.segments[0].hir_id).as_str().to_owned();
        let boundary=self.boundary_map.get(&bytepos);
        // This if statement checks: Is something the path p actually happening here - see aquascope/analysis/boundaries/mod.rs for more info
        if let Some(boundary) = boundary {
          // will have to change this for structs
          let rhs_mut = self.mutability_map.get(&rhs_name).unwrap();
          let rhs_rap = self.raps.get(&rhs_name).unwrap().to_owned();
          if rhs_rap.is_owner() {
            self.add_owner(lhs_name, lhs_mutability);
          }
          else if rhs_rap.is_struct() { //TODO: Add support for structs

          }
          else if rhs_rap.is_ref() {
            self.add_ref(lhs_name, lhs_mutability);
            let owner = self.borrow_map.get(&rhs_name).unwrap();
            self.borrow_map.insert(lhs_name.clone(), owner.clone());
            if lhs_mutability { // mut ref
              self.update_lifetime(Reference::Mut(rhs_name), line_num);
              self.update_lifetime(Reference::Mut(lhs_name), line_num);
            }
            else { // static ref
              self.update_lifetime(Reference::Static(rhs_name), line_num);
              self.update_lifetime(Reference::Static(lhs_name), line_num);
            }
          }
          else {
            panic!("invalid rhs_rap");
          }
          
          let lhs_rap = self.raps.get(&lhs_name).unwrap().to_owned();
          // use aqua analysis to see if rhs is dropped (ie rhs is mut)
          if boundary.expected.drop { // if a resource is being dropped -> a move occurs
            self.add_external_event(line_num, ExternalEvent::Move { from: Some(rhs_rap), to: Some(lhs_rap) });
          }
          else { // a copy occurs
            self.add_external_event(line_num, ExternalEvent::Copy { from: Some(rhs_rap), to: Some(lhs_rap) });
          }
        }   
      },
      // fn_expr: resolves to function itself (Path)
      // second arg, is a list of args to the function
      ExprKind::Call(fn_expr, _) => {
        let line_num = self.span_to_line(&fn_expr.span);
        let fn_name = self.hirid_to_var_name(fn_expr.hir_id).unwrap();
        self.add_fn(fn_name);
        let rhs_rap = self.raps.get(&fn_name).unwrap().to_owned();
        // if return type is not a reference
        if !self.is_return_type_ref(fn_expr){
          if self.is_return_type_struct(fn_expr) { // TODO: how to implement when fn returns a struct

          }
          else {
            self.add_owner(lhs_name, lhs_mutability);
          }
          let lhs_rap = self.raps.get(&lhs_name).unwrap().to_owned();
          if self.is_return_type_copyable(fn_expr) {
            self.add_external_event(line_num, ExternalEvent::Copy { from: Some(rhs_rap), to: Some(lhs_rap) });
          }
          else {
            self.add_external_event(line_num, ExternalEvent::Move { from: Some(rhs_rap), to: Some(lhs_rap) });
          }
        }
        // return type is a reference
        else {
          self.add_ref(lhs_name, lhs_mutability);
          let lhs_rap = self.raps.get(&lhs_name).unwrap().to_owned();
          if let Some(return_type) = self.return_type_of(fn_expr){
            self.borrow_map.insert(lhs_name.clone(),None); //borrowing from an anonymous owner
            if let Some(mutability) = return_type.ref_mutability(){
              match mutability{
                Mutability::Mut=>{ // if rhs is mutable ref then a move must occur
                  self.add_external_event(line_num, ExternalEvent::Move { from: Some(rhs_rap), to: Some(lhs_rap) });
                  self.update_lifetime(Reference::Mut(lhs_name), line_num);
                }
                Mutability::Not=>{ // static reference should be copied
                  self.add_external_event(line_num, ExternalEvent::Copy { from: Some(rhs_rap), to: Some(lhs_rap) });
                  self.update_lifetime(Reference::Static(lhs_name), line_num);
                }
              }
            }
          }
        }
      },
      // Any type of literal on RHS implies a bind
      ExprKind::Lit(_) => {
        let line_num = self.span_to_line(&rhs.span);
        match self.raps.get(&lhs_name) {
          None => {
            self.add_event(line_num, format!("Bind({})", lhs_name));
            self.add_access_point(AccessPointUsage::Owner(lhs.clone()), lhs_var);
          }
          Some(p) => {
            match p {
              AccessPointUsage::MutRef(_) => {}
              _ => {
                // this is to prevent re-annotation of binds for mutable variables
                self.add_event(line_num, format!("Bind({})", lhs_var));
                self.add_access_point(AccessPointUsage::Owner(lhs.clone()), lhs_var);
              }
            }
          }
        }
      }
      // ex : &<expr> or &mut <expr>
      ExprKind::AddrOf(_,mutability,expr) => {
        match expr.kind{
          // only handling paths so far, could technically be other expr kind
          ExprKind::Path(QPath::Resolved(_,p))=>{
            let line_num = self.span_to_line(&p.span);
            let name: String = self.tcx.hir().name(p.segments[0].hir_id).as_str().to_owned();
            if let Some(ref_to) = self.name_to_access_point.get(&name) {
              self.borrow_map.insert(lhs_var.clone(),Some(ref_to.clone()));
            }
            else {
              self.borrow_map.insert(lhs_var.clone(),None);
            }
            (name.clone(), p.span, false);
            match mutability{
              Mutability::Not=>{
                if is_deref {
                  self.add_event(line_num,format!("StaticBorrow({}->*{})", name,lhs_var));
                }
                else {
                  self.add_event(line_num,format!("StaticBorrow({}->{})", name,lhs_var));
                }
                self.update_lifetime(Reference::Static(lhs_var.clone()), line_num);
                self.add_access_point(AccessPointUsage::StaticRef(lhs), lhs_var);
              }
              Mutability::Mut=>{
                if is_deref {
                  self.add_event(line_num,format!("MutableBorrow({}->*{})", name,lhs_var));
                }
                else {
                  self.add_event(line_num,format!("MutableBorrow({}->{})", name,lhs_var));
                }
                self.update_lifetime(Reference::Mut(lhs_var.clone()), line_num);
                self.add_access_point(AccessPointUsage::MutRef(lhs), lhs_var);
              }
            }
          }
          _=>{}
        }
      }
      // RHS is a block ie:
      // let a = { <stmt1>...<stmt_n>, <expr> };
      ExprKind::Block(block, _) => {
        // set new scope when entering a block (span.hi refers to the ending brace of the block)
        let prev_scope = self.current_scope;
        let new_scope = self.tcx.sess.source_map().lookup_char_pos(rhs.span.hi()).line;
        self.current_scope = new_scope;
        self.visit_block(block);
        // lhs of block exists in prev scope
        self.current_scope = prev_scope;
        // then, if the block has a return expr
        match block.expr {
          Some(res_expr) => {
            self.match_rhs(lhs.clone(), res_expr, false);
          }
          None => {}
        }

      }
      // A binary operation (e.g., <expr> + <expr>, <expr> * <expr>).
      ExprKind::Binary(..) => {
        let line_num = self.span_to_line(&rhs.span);
        if lhs.mutability == Mutability::Not && !is_deref && lhs.name != "None" {
          self.add_event(line_num, format!("Bind({})", lhs_var));
          self.add_access_point(AccessPointUsage::Owner(lhs), lhs_var);
        }
      },

      ExprKind::Unary(option, expr) => {
        match option {
          /* the '*' operator for dereferencing */
          rustc_hir::UnOp::Deref => {
            // find span corresponding to deref operator (this is the byte pos that's stored in the boundary map)
            match expr.kind {
              ExprKind::Path(QPath::Resolved(_,p))=>{
                let line_num = self.span_to_line(&p.span);
                let lhs_name = lhs.name.clone();
                let bytepos=rhs.span.lo(); // small discrepancy with boundary map, bytePos corresponds to bytePos of deref operator not path
                let boundary=self.boundary_map.get(&bytepos);
                if let Some(boundary) = boundary {
                  let expected=boundary.expected;
                  let name = self.tcx.hir().name(p.segments[0].hir_id).as_str().to_owned();
                  (name.clone(), p.span, false);

                  if expected.drop{ 
                    self.add_event(line_num,format!("Move(*{}->{})", name, lhs_name));
                  }
                  else {
                    self.add_event(line_num, format!("Copy(*{}->{})", name, lhs_name));
                  }

                  if let Some(a) = self.borrow_map.get(&name).unwrap() {
                    match a.clone() {
                      AccessPointUsage::Owner(_) => {
                        self.add_access_point(AccessPointUsage::Owner(lhs.clone()), lhs_var);
                      }
                      AccessPointUsage::StaticRef(_) => {
                        self.add_access_point(AccessPointUsage::StaticRef(lhs.clone()), lhs_var.clone());
                        self.update_lifetime(Reference::Static(lhs_var), line_num);
                      }
                      AccessPointUsage::MutRef(_) => { // TODO: test if this is even possible
                        self.add_access_point(AccessPointUsage::MutRef(lhs.clone()), lhs_var.clone());
                        self.update_lifetime(Reference::Mut(lhs_var), line_num);
                      }
                      _ => { println!("how did this happen");} // really messed up if you made it here
                    }
                  }
                  else { //we are dereferencing a function reference parameter
                    // we have no way of knowing what lhs should be (an owner or reference)
                  }
                }
              }
              _ => {} // todo: implement other types of derefs, only handling paths for now
            }
          }
          // ! and ~ operators
          _ => {
            let line_num = self.span_to_line(&expr.span);
            if lhs.mutability == Mutability::Not && !is_deref {
              self.add_event(line_num, format!("Bind({})", lhs_var));
              self.add_access_point(AccessPointUsage::Owner(lhs), lhs_var);
            }
          }
        }
      },
      ExprKind::MethodCall(name_and_generic_args, rcvr, args, _) => {
        let line_num = self.span_to_line(&rcvr.span);
        let fn_name = self.hirid_to_var_name(name_and_generic_args.hir_id).unwrap();
        let type_check = self.tcx.typeck(name_and_generic_args.hir_id.owner);
        self.add_access_point(AccessPointUsage::Function(fn_name.clone()), fn_name.clone());
        if let Some(return_type) = type_check.node_type_opt(rhs.hir_id){
          if !return_type.is_ref(){
            if return_type.is_copy_modulo_regions(self.tcx, self.tcx.param_env(name_and_generic_args.hir_id.owner)) {
              if is_deref {
                self.add_event(line_num, format!("Copy({}()->*{})", fn_name, lhs_var));
              }
              else { 
                self.add_event(line_num, format!("Copy({}()->{})", fn_name, lhs_var)); 
                self.add_access_point(AccessPointUsage::Owner(lhs.clone()), lhs_var);
              }
            }
            else {
              if is_deref {
                self.add_event(line_num, format!("Move({}()->*{})", fn_name, lhs_var));
              }
              else {
                self.add_event(line_num, format!("Move({}()->{})", fn_name, lhs_var));
                self.add_access_point(AccessPointUsage::Owner(lhs.clone()), lhs_var);
              }
            }
          }
          else {
            self.borrow_map.insert(lhs_var.clone(),None);
            if let Some(mutability)=return_type.ref_mutability(){
              match mutability{
                Mutability::Mut=>{
                  self.add_event(line_num, format!("Move({}()->{})", fn_name, lhs_var));
                  self.add_access_point(AccessPointUsage::MutRef(lhs), lhs_var.clone());
                  self.update_lifetime(Reference::Mut(lhs_var), line_num);
                }
                Mutability::Not=>{
                  self.add_event(line_num, format!("Copy({}()->{})",fn_name, lhs_var));
                  self.add_access_point(AccessPointUsage::StaticRef(lhs), lhs_var.clone());
                  self.update_lifetime(Reference::Static(lhs_var), line_num);
                }
              }
            }
          }      
        }      
      }
      // Struct intializer list:
      // ex struct = {a: <expr>, b: <expr>, c: <expr>}
      ExprKind::Struct(_qpath, expr_fields, _base) => {
        // LHS must be a struct
        let line_num = self.span_to_line(&rhs.span);
        //TODO: check what kind of QPath this is
        self.add_event(line_num, format!("Bind({})", lhs.name));
        let mut field_vec: Vec<AccessPoint> = Vec::new();
        // Insert Struct into access points -> with members
        for field in expr_fields.iter() {
          let mut new_lhs = lhs.clone();
          new_lhs.name = format!("{}.{}", lhs.name, field.ident.to_string());
          // TODO: will have to actually annotate the struct fields
          self.match_rhs(new_lhs.clone(), field.expr, false);
          (field.ident.to_string(), field.ident.span, false);
          field_vec.push(new_lhs);
        }
        self.add_access_point(AccessPointUsage::Struct(lhs, field_vec), lhs_var);
      
      },

      ExprKind::Field(expr, id) => {
        match expr {
          Expr{kind: ExprKind::Path(QPath::Resolved(_,p)), ..} => {
            let line_num = self.span_to_line(&p.span);
            let bytepos=p.span.lo(); // small discrepancy with boundary map, bytePos corresponds to bytePos of deref operator not path
            let boundary=self.boundary_map.get(&bytepos);
            if let Some(boundary) = boundary {
              let expected=boundary.expected;
              let name = self.tcx.hir().name(p.segments[0].hir_id).as_str().to_owned();
              let field_name: String = id.as_str().to_owned();
              (name.clone(), p.span, false);
              (field_name.clone(), id.span, false);
              let a = self.name_to_access_point.get(&name).unwrap().clone();
              if expected.drop{
                if is_deref {
                  self.add_event(line_num,format!("Move({}->*{})", format!("{}.{}", name, field_name), lhs_var));
                }
                else {
                  self.add_event(line_num,format!("Move({}->{})", format!("{}.{}", name, field_name), lhs_var));
                }
                match a {
                  AccessPointUsage::Owner(_) => {
                    self.add_access_point(AccessPointUsage::Owner(lhs), lhs_var);
                  }
                  AccessPointUsage::MutRef(_) => {
                    self.add_access_point(AccessPointUsage::MutRef(lhs), lhs_var.clone());
                    let owner =self.borrow_map.get(&name).unwrap();
                    if let Some(owner)=owner{
                      self.borrow_map.insert(lhs_var.clone(),Some(owner.clone()));
                    }
                    else {
                      self.borrow_map.insert(lhs_var.clone(),None);
                    }
                    self.update_lifetime(Reference::Mut(lhs_var), line_num);
                    self.update_lifetime(Reference::Mut(name), line_num);
                  }
                  _ => { println!("you really messed up if you got here") }
                }
              }
              else {       
                if is_deref {
                  self.add_event(line_num,format!("Copy({}->*{})", format!("{}.{}", name, field_name), lhs_var));
                }
                else {
                  self.add_event(line_num,format!("Copy({}->{})", format!("{}.{}", name, field_name), lhs_var));
                }

                match a {
                  AccessPointUsage::Owner(_) => {
                    self.add_access_point(AccessPointUsage::Owner(lhs), lhs_var);
                  }
                  AccessPointUsage::MutRef(_) => {
                    self.add_access_point(AccessPointUsage::StaticRef(lhs), lhs_var.clone());
                    let owner =self.borrow_map.get(&name).unwrap();
                    if let Some(owner)=owner{
                      self.borrow_map.insert(lhs_var.clone(),Some(owner.clone()));
                    }
                    else {
                      self.borrow_map.insert(lhs_var.clone(),None);
                    }
                    self.update_lifetime(Reference::Static(lhs_var), line_num);
                    self.update_lifetime(Reference::Static(name), line_num);
                  }
                  _ => { println!("you really messed up if you got here") }
                }

              }
            }
          }
          _ => { println!("wacky struct expr")}
        }
      },

      // explicitly using return keyword
      // ex: return <expr>
      ExprKind::Ret(ret) => {
        match ret {
          Some(ret_expr) => {
            self.match_rhs(lhs, ret_expr, false);
          }
          // returning void, nothing happens
          None => {}
        }
      },
      _ => {
        println!("unmatched rhs {:#?}", rhs);
      }
    }
  }
  
  pub fn print_definitions(&mut self) -> Vec<String> {
    let mut declarations : Vec<String> = Vec::new();
    
    for point in &self.owners {
      let owner_name: String = 
        match point {
          AccessPointUsage::Owner(p)=>{
            format!("Owner {:?} {};",p.mutability,p.name)
          }
          AccessPointUsage::StaticRef(p)=>{
            format!("StaticRef {:?} {};",p.mutability,p.name)
          }
          AccessPointUsage::MutRef(p)=>{
            format!("MutRef {:?} {};",p.mutability,p.name)
          }
          AccessPointUsage::Function(name)=>{
            format!("Function {}();",name)
          }
          AccessPointUsage::Struct(p, field_vec) => {
            format!(
              "Struct {}{{{}}}", p.name,
              field_vec.iter()
                  .map(|s| 
                    if let Some(dot_index) = s.name.find('.') {
                      s.name[dot_index + 1..].to_string()
                    } else {
                        println!("No dot found in the string.");
                      s.name.clone()
                    })
                  .collect::<Vec<String>>()
                  .join(", "))
          }
        };
      if !declarations.contains(&owner_name) && !owner_name.contains("."){
        declarations.push(owner_name);
      }
    }
    return declarations;
  }
  
  pub fn print_out_of_scope(&mut self){
    let access = self.access_points.clone();
    for (point,gos) in &access {
      if gos!=&0 {
        match point {
          AccessPointUsage::Owner(p)|
          AccessPointUsage::StaticRef(p)|
          AccessPointUsage::MutRef(p)=>{
            // temporary fix for how we are currently handling return expressions 
            if p.name != "None" {
              self.add_event(*gos,format!("GoOutOfScope({})",p.name));
            }
          }
          AccessPointUsage::Struct(p, _)=>{
            self.add_event(*gos,format!("GoOutOfScope({})",p.name));
          }
          _ => {}
        }
      }
    }
  }
  
  pub fn print_lifetimes(&mut self){
    println!("LIFETIME MAP: {:#?}", self.lifetime_map);
    println!("BORROW MAP {:#?}", self.borrow_map);
    println!("NAME TO ACCESS POINT MAP {:#?}", self.name_to_access_point);
    let lifetime_map = self.lifetime_map.clone();
    for (reference,line_num) in &lifetime_map{
      let linenum: usize = *line_num;
      match reference{
        Reference::Mut(name)=>{
          if let Some(owner)=self.borrow_map.get(name){
            if let Some(owner)=owner{
              self.add_event(linenum,format!("MutableDie({}->{})", name, string_of_access_point(owner)));
            }
            // else {
            //   self.add_event(linenum,format!("MutableDie({}->*{})", name,name));
            // }
          }
        }
        Reference::Static(name)=>{
          if let Some(owner)=self.borrow_map.get(name){
            if let Some(owner)=owner{
              self.add_event(linenum,format!("StaticDie({}->{})", name, string_of_access_point(owner)));
            }
            // else {
            //   self.add_event(linenum,format!("StaticDie({}->*{})", name,name));
            // }
          }
        }
      }
    }
  }
}