use rustc_middle::{
    mir::Body,
    ty::*,
  };
  use rustc_hir::{Expr, ExprKind, QPath, Path, Mutability, UnOp};
use rustc_utils::mir::mutability;
  use std::cell::Ref;
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


pub struct ExprVisitor<'a, 'tcx:'a> {
  pub tcx: TyCtxt<'tcx>,
  pub mir_body: &'a Body<'tcx>,
  pub boundary_map: HashMap<rustc_span::BytePos,PermissionsBoundary>,
  pub borrow_map: HashMap<String, (ResourceTy, usize, bool)>, //lendee -> (lender, lifetime, ref mutability) 
  pub lenders: HashMap<String, HashSet<ResourceTy>>,
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

  pub fn update_lifetime(&mut self, name: &String, line:usize, ref_mutability: bool){
    let (r, lifetime, muta) = self.borrow_map.get_mut(name).unwrap();
    *lifetime = line;
  }

  // pub fn add_access_point(&mut self, a: AccessPointUsage, name: String) {
  //   self.access_points.insert(a.clone(), self.current_scope);
  //   self.name_to_access_point.insert(name.to_owned(), a.clone());
  //   self.owners.push(a);
  // }

  pub fn add_owner(&mut self, name: String, mutability: bool) {
    self.add_rap(ResourceAccessPoint::Owner(Owner{name: name, hash: self.rap_hashes as u64, is_mut: mutability}));
  }

  pub fn add_ref(&mut self, name: String, ref_mutability: bool, lhs_mut: bool, line_num: usize, lender: ResourceTy) {
    match ref_mutability {
      true => {
        self.add_mut_ref(name.clone(), lhs_mut);
      }
      false => { 
        self.add_static_ref(name.clone(), lhs_mut);
      }
    }
    self.borrow_map.insert(name.clone(), (lender.clone(), line_num, ref_mutability));
    match lender.clone() {
      ResourceTy::Anonymous => {}
      _ => {
        let borrower = ResourceTy::Value(self.raps.get(&name).unwrap().0.clone());
        self.lenders.entry(lender.name()).and_modify(|v| {v.insert(borrower.clone());}).or_insert(HashSet::from([borrower]));
      }
    }
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
      self.update_lifetime(r.name(), line_num, r.is_mutref());
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

  pub fn add_ev(&mut self, line_num: usize, evt: Evt, lhs: ResourceTy, rhs: ResourceTy) {
    match evt {
      Evt::Bind => self.add_external_event(line_num, ExternalEvent::Bind { from: rhs, to: lhs }),
      Evt::Copy => self.add_external_event(line_num, ExternalEvent::Copy { from: rhs, to: lhs }),
      Evt::Move => self.add_external_event(line_num, ExternalEvent::Move { from: rhs, to: lhs }),
      Evt::SBorrow => self.add_external_event(line_num, ExternalEvent::StaticBorrow { from: rhs, to: lhs }),
      Evt::MBorrow => self.add_external_event(line_num, ExternalEvent::MutableBorrow { from: rhs, to: lhs }),
      Evt::PassBySRef => self.add_external_event(line_num, ExternalEvent::PassByStaticReference { from: rhs, to: lhs }),
      Evt::PassByMRef => self.add_external_event(line_num, ExternalEvent::PassByMutableReference { from: rhs, to: lhs }),
      Evt::SDie =>  self.add_external_event(line_num, ExternalEvent::StaticDie { from: rhs, to: lhs }),
      Evt::MDie => self.add_external_event(line_num, ExternalEvent::MutableDie { from: rhs, to: lhs })
    }
  }

  pub fn resource_of_lhs(&mut self, expr: &'tcx Expr) -> ResourceTy{
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
      ExprKind::Unary(UnOp::Deref, exp) => { // TODO: FIX THIS
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

  pub fn match_args(&mut self, arg: &'tcx Expr, fn_name:String) {
    println!("matching args");
    let line_num = self.span_to_line(&arg.span);
    // add callee no matter what
    self.add_fn(fn_name.clone());
    let fn_rap = self.raps.get(&fn_name).unwrap().0.to_owned();
    match arg.kind {
      // arg is variable
      ExprKind::Path(QPath::Resolved(_,p))=>{
        let name: String = self.tcx.hir().name(p.segments[0].hir_id).as_str().to_owned();
        let arg_rap = self.raps.get(&name).unwrap().0.to_owned();
        self.update_rap(&arg_rap, line_num);
        let boundary=self.boundary_map.get(&p.span.lo());
        if let Some(boundary) = boundary {
          let expected=boundary.expected;
          if expected.drop{
            self.add_ev(line_num, Evt::Move, ResourceTy::Value(fn_rap), ResourceTy::Value(arg_rap));
          }
          else if expected.write{           
            self.add_ev(line_num, Evt::PassByMRef, ResourceTy::Value(fn_rap), ResourceTy::Value(arg_rap));
          }
          else if expected.read{
            self.add_ev(line_num, Evt::PassBySRef, ResourceTy::Value(fn_rap), ResourceTy::Value(arg_rap));
          }
        }
      }
      ExprKind::AddrOf(_,_,expr)=>{        
        self.match_args(expr, fn_name);
      }
      ExprKind::Call(fn_expr, fn_args) => { //functions can be parameters too
        let callee_name= self.hirid_to_var_name(fn_expr.hir_id).unwrap();
        self.add_fn(callee_name.clone());
        for a in fn_args.iter() {
          self.match_args(a, callee_name.clone());
        }
        if self.is_return_type_copyable(fn_expr) {
          self.add_ev(line_num, Evt::Copy, ResourceTy::Value(fn_rap), ResourceTy::Anonymous);
        }
        else {
          self.add_ev(line_num, Evt::Move, ResourceTy::Value(fn_rap), ResourceTy::Anonymous);
          //self.add_event(line_num, format!("Move({}()->{}())", callee_name, fn_name));
        }
      }
      ExprKind::Unary(option, expr) => {
        match option {
          rustc_hir::UnOp::Deref => {
            println!("we getting here?");
            let line_num = self.expr_to_line(&expr);
            let rhs_rap = self.fetch_rap(&expr);
            let res = match rhs_rap {
              Some(x) => {
                println!("updating x {:#?}", x);
                self.update_rap(&x, line_num);
                ResourceTy::Deref(x)
              }
              None => ResourceTy::Anonymous
            };
            // small discrepancy with boundary map, bytePos corresponds to bytePos of deref operator not pat
            let boundary=self.boundary_map.get(&arg.span.lo());
            if let Some(boundary) = boundary {
              if boundary.expected.drop { //TODO: will have to update with a new type of RAP, maybe a DEREF{Option<RAP>}
                self.add_ev(line_num, Evt::Move, ResourceTy::Value(fn_rap), res);
              }
              else if boundary.expected.write{
                self.add_ev(line_num, Evt::PassByMRef, ResourceTy::Value(fn_rap), res);
              }
              else if boundary.expected.read {
                self.add_ev(line_num, Evt::PassBySRef, ResourceTy::Value(fn_rap), res)
              }
            }
            else {
              panic!("unable to grab boundary map for Unary expr")
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
            let name = self.tcx.hir().name(p.segments[0].hir_id).as_str().to_owned();
            let mem_name = format!("{}.{}", name, id.as_str());
            let arg_rap = self.raps.get(&mem_name).unwrap().0.to_owned();
            self.update_rap(&arg_rap, line_num);
            let boundary=self.boundary_map.get(&bytepos);
            if let Some(boundary) = boundary {
              let expected=boundary.expected;
              
              if expected.drop{
                self.add_ev(line_num, Evt::Move, ResourceTy::Value(fn_rap), ResourceTy::Value(arg_rap));
              }
              else if expected.write{           
                self.add_ev(line_num, Evt::PassByMRef, ResourceTy::Value(fn_rap), ResourceTy::Value(arg_rap));
              }
              else if expected.read{
                self.add_ev(line_num, Evt::PassBySRef, ResourceTy::Value(fn_rap), ResourceTy::Value(arg_rap));
              }
            }
          }
          _ => { println!("wacky struct expr")}
        }
      }
      _=>{ }
    }
  }

  pub fn find_lender(&self, rhs: &'tcx Expr) -> ResourceTy {
    match rhs.kind {
      ExprKind::Path(QPath::Resolved(_,p)) => {
        let name = self.tcx.hir().name(p.segments[0].hir_id).as_str().to_owned();
        if self.borrow_map.contains_key(&name) {
          self.borrow_map.get(&name).unwrap().to_owned().0
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
                self.borrow_map.get(r.name()).unwrap().to_owned().0
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
            let name = format!("{}.{}", name, ident.as_str());
            if self.borrow_map.contains_key(&name) {
              self.borrow_map.get(&name).unwrap().to_owned().0
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

  pub fn define_lhs(&mut self, lhs_name: String, lhs_mutability: bool, rhs_expr: &'tcx Expr, lhs_ty: Ty <'tcx>) {
    //println!("NODE TYPES {:#?}", tycheck_results.node_types().items_in_stable_order());
    if lhs_ty.is_ref() {
      self.add_ref(lhs_name.clone(), 
      bool_of_mut(lhs_ty.ref_mutability().unwrap()), 
      lhs_mutability, 
      self.expr_to_line(&rhs_expr),
      self.find_lender(&rhs_expr));
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
          panic!("lhs enum not implemented yet")
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

  pub fn match_rhs(&mut self, lhs: ResourceTy, rhs:&'tcx Expr){
    match rhs.kind {
      ExprKind::Path(QPath::Resolved(_,p)) => {
        let line_num = self.span_to_line(&p.span);
        let rhs_name: String = self.tcx.hir().name(p.segments[0].hir_id).as_str().to_owned();
        let rhs_rap = self.raps.get(&rhs_name).unwrap().0.to_owned();
        self.update_rap(&rhs_rap, line_num);
        let boundary= self.boundary_map.get(&p.span.lo());
        // This if statement checks: Is something the path p actually happening here - see aquascope/analysis/boundaries/mod.rs for more info
        if let Some(boundary) = boundary {
          if boundary.expected.drop { // if a resource is being dropped
            self.add_ev(line_num, Evt::Move, lhs, ResourceTy::Value(rhs_rap));
          }
          else {
            self.add_ev(line_num, Evt::Copy, lhs, ResourceTy::Value(rhs_rap));
          }
        }   
      },
      // fn_expr: resolves to function itself (Path)
      // second arg, is a list of args to the function
      ExprKind::Call(fn_expr, _) => {
        let line_num = self.span_to_line(&fn_expr.span);
        let fn_name = self.hirid_to_var_name(fn_expr.hir_id).unwrap();
        let rhs_rap = self.raps.get(&fn_name).unwrap().0.to_owned();
        if self.is_return_type_copyable(fn_expr) {
          self.add_ev(line_num, Evt::Copy, lhs, ResourceTy::Value(rhs_rap));
        }
        else {
          self.add_ev(line_num, Evt::Move, lhs, ResourceTy::Value(rhs_rap));
        }
      },
      
      ExprKind::Lit(_) | ExprKind::Binary(..) | // Any type of literal on RHS implies a bind
      ExprKind::Unary(UnOp::Neg, _) | // ~<expr>
      ExprKind::Unary(UnOp::Not, _) // !<expr>
      => {
        let line_num = self.span_to_line(&rhs.span);
        if let ResourceTy::Caller = lhs {
          self.add_ev(line_num, Evt::Copy, lhs, ResourceTy::Anonymous);
        }
        else {
          self.add_ev(line_num, Evt::Bind, lhs, ResourceTy::Anonymous);
        }
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
        //self.visit_block(block);
        // lhs of block exists in prev scope
        self.current_scope = prev_scope;
        // then, if the block has a return expr
        match block.expr {
          Some(res_expr) => {
            self.match_rhs(lhs.clone(), res_expr);
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
            // small discrepancy with boundary map, bytePos corresponds to bytePos of deref operator not pat
            let boundary=self.boundary_map.get(&rhs.span.lo());
            if let Some(boundary) = boundary {
              if boundary.expected.drop { 
                self.add_ev(line_num, Evt::Move, lhs, res);
              }
              else {
                self.add_ev(line_num, Evt::Copy, lhs, res);
              }
            }
            else {
              panic!("unable to grab boundary map for Unary expr")
            }
          },
          _ => {}
        }
      } 
      ExprKind::MethodCall(name_and_generic_args, rcvr, _,  _) => {
        let line_num = self.span_to_line(&rcvr.span);
        let fn_name = self.hirid_to_var_name(name_and_generic_args.hir_id).unwrap();
        let type_check = self.tcx.typeck(name_and_generic_args.hir_id.owner);
        let rhs_rap = self.raps.get(&fn_name).unwrap().0.to_owned();

        if let Some(return_type) = type_check.node_type_opt(rhs.hir_id){
          if return_type.is_copy_modulo_regions(self.tcx, self.tcx.param_env(name_and_generic_args.hir_id.owner)) {
            self.add_ev(line_num, Evt::Copy, lhs, ResourceTy::Value(rhs_rap));
          }
          else {
            self.add_ev(line_num, Evt::Move, lhs, ResourceTy::Value(rhs_rap));
          }
        }
      }
      // Struct intializer list:
      // ex struct = {a: <expr>, b: <expr>, c: <expr>}
      ExprKind::Struct(_qpath, expr_fields, _base) => { 
        let line_num = self.span_to_line(&rhs.span);
        if let ResourceTy::Caller = lhs {
          self.add_ev(line_num, Evt::Move, lhs.clone(), ResourceTy::Anonymous); // weird to do here    
        }
        else {
          self.add_ev(line_num, Evt::Bind, lhs.clone(), ResourceTy::Anonymous);
          for field in expr_fields.iter() {
              let new_lhs_name = format!("{}.{}", lhs.name(), field.ident.as_str());
              let field_rap = self.raps.get(&new_lhs_name).unwrap().0.to_owned();
              self.match_rhs(ResourceTy::Value(field_rap), field.expr);
          }
        }
      },

      ExprKind::Field(expr, id) => {
        match expr {
          Expr{kind: ExprKind::Path(QPath::Resolved(_,p)), ..} => {
            let line_num = self.span_to_line(&p.span);
            let bytepos=p.span.lo(); // small discrepancy with boundary map, bytePos corresponds to bytePos of deref operator not path
            let boundary=self.boundary_map.get(&bytepos);
            let name = self.tcx.hir().name(p.segments[0].hir_id).as_str().to_owned();
            let total_name = format!("{}.{}", name, id.as_str());
            let rhs_rap = self.raps.get(&total_name).unwrap().0.to_owned();
            if let Some(boundary) = boundary {
              let expected=boundary.expected;
              if expected.drop {
                self.add_ev(line_num, Evt::Move, lhs, ResourceTy::Value(rhs_rap));
              }
              else {
                self.add_ev(line_num, Evt::Copy, lhs, ResourceTy::Value(rhs_rap));
              }
            }
          }
          _ => panic!("unexpected field expr")
        }
      }
      // explicitly using return keyword
      // ex: return <expr>
      ExprKind::Ret(ret) => {
        match ret {
          Some(ret_expr) => {
            self.match_rhs(lhs, ret_expr);
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
    let mut res_to_ref: HashMap<String, (usize, String)> = HashMap::new();
    for (k, (r, lifetime, _)) in &self.borrow_map {
      match r {
        ResourceTy::Anonymous => {}
        ResourceTy::Caller => {panic!("bruh not possible")}
        ResourceTy::Deref(ro) | ResourceTy::Value(ro) => {
          if res_to_ref.contains_key(ro.name()) {
            if res_to_ref.get(ro.name()).unwrap().0 < *lifetime {
              ultimate_refs.remove(&res_to_ref.get(ro.name()).unwrap().1);
              *res_to_ref.get_mut(ro.name()).unwrap() = (*lifetime, k.to_owned());
              ultimate_refs.insert(k.to_owned());
            }
          }
          else {
            res_to_ref.insert(r.name().to_owned(), (*lifetime, k.to_owned()));
            ultimate_refs.insert(k.to_owned());
          }
        }
      }
    }

    // test-case: r and *r are both references
    // test-case r points to multiple references one of which has multiple borrowers

    println!("res refs {:#?}", res_to_ref);
    println!("ultimate refs {:#?}", ultimate_refs);
    let b_map = self.borrow_map.clone();

    // loop through all 'active' references
    for (k, (r_ty, lifetime, ref_mut)) in &b_map {
      let from_ro = self.raps.get(k).unwrap().0.to_owned();
      let to_ro = match r_ty {
        ResourceTy::Anonymous => ResourceTy::Deref(from_ro.clone()),
        _ => r_ty.clone()
      };
      if !ultimate_refs.contains(k) {
        self.add_external_event(*lifetime, ExternalEvent::RefDie { from: ResourceTy::Value(from_ro), to: to_ro });
      }
      else {
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