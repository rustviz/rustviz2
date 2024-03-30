// Some headers
use rustc_middle::{
  mir::Body,
  ty::{TyCtxt,Ty},
};
use rustc_hir::{StmtKind, Stmt, Local, Expr, ExprKind, UnOp, Param,
  QPath, Path, def::Res, PatKind, Mutability, FnDecl, BodyId, Constness, Unsafety, IsAsync};
use std::{collections::{HashMap, BTreeMap}, clone};
use rustc_ast::walk_list;
use rustc_span::Span;
use aquascope::analysis::boundaries::PermissionsBoundary;
use rustc_hir::{intravisit::{self, Visitor, FnKind},hir_id::HirId, def_id::LocalDefId};
use std::cmp::{Eq, PartialEq};
use std::hash::Hash;

#[derive(Eq, PartialEq, Hash, Clone)]
pub struct AccessPoint {
  mutability: Mutability,
  pub name:String,
  pub members: Option<Vec<AccessPoint>>,
}

#[derive(Eq, PartialEq,Hash, Clone)]
pub enum AccessPointUsage{
  Owner(AccessPoint),
  MutRef(AccessPoint),
  StaticRef(AccessPoint),
  Struct(Vec<AccessPoint>),
  Function(String),
}

#[derive(Eq, PartialEq,Hash, Clone)]
pub enum Reference{
  Static(String),
  Mut(String),
}

// A small helper function
pub fn extract_var_name(input_string: &str ) -> Option<String> {
  let start_index = input_string.find('`')? + 1;
  let end_index = input_string.rfind('`')?;
  let rough_string=input_string[start_index..end_index].to_owned();
  if rough_string.contains("String::from"){
    Some(String::from("String::from"))
  }
  else{
    Some(rough_string)
  }
}

// given an operator (add, sub, etc), returns string representation
fn match_op(op: rustc_hir::BinOpKind) -> String {
  use rustc_hir::BinOpKind::*;
  match op {
    Add => "+".to_owned(),
    Sub => "-".to_owned(),
    Mul => "*".to_owned(),
    Div => "/".to_owned(),
    Rem => "%".to_owned(),
    And => "AND".to_owned(), //javascript doesn't like ampersands
    Or => "||".to_owned(),
    BitXor => "^".to_owned(),
    BitAnd => "BITAND".to_owned(), //javascript doesn't like ampersands
    BitOr => "|".to_owned(),
    Shl => "<<".to_owned(),
    Shr => ">>".to_owned(),
    Eq => "==".to_owned(),
    Lt => "<".to_owned(),
    Le => "<=".to_owned(),
    Ne => "!=".to_owned(),
    Ge => ">=".to_owned(),
    Gt => ">".to_owned()
  }
}


// Implement the visitor 
pub struct ExprVisitor<'a, 'tcx:'a> {
  pub tcx: TyCtxt<'tcx>,
  pub mir_body: &'a Body<'tcx>,
  pub boundary_map: HashMap<rustc_span::BytePos,PermissionsBoundary>,
  pub mutability_map: HashMap<String,Mutability>, // map owner name to mutablility status
  pub lifetime_map: HashMap<Reference,usize>,
  pub borrow_map: HashMap<String, Option<String>>,
  pub access_points: HashMap<AccessPointUsage, usize>,
  pub current_scope: usize,
  pub analysis_result : HashMap<usize, Vec<String>>,
  pub owner_names: Vec<String>,
  pub event_line_map: & 'a mut BTreeMap<usize, String>,
}

// These are helper functions used the visitor
impl<'a, 'tcx> ExprVisitor<'a, 'tcx>{
  fn expr_to_line(&self,expr:&Expr)->usize{
    self.tcx.sess.source_map().lookup_char_pos(expr.span.lo()).line
  }

  fn span_to_line(&self,span:&Span)->usize{
    self.tcx.sess.source_map().lookup_char_pos(span.lo()).line
  }

  fn hirid_to_var_name(&self,id:HirId)->Option<String>{
    let long_name = self.tcx.hir().node_to_string(id);
    extract_var_name(&long_name)
  }

  fn return_type_of(&self,fn_expr:&Expr)->Option<Ty<'tcx>>{
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

  fn is_return_type_ref(&self,fn_expr:&Expr)->bool{
    if let Some(return_type)=self.return_type_of(fn_expr){
      return_type.is_ref()
    }
    else{
      false
    }
  }

  fn is_return_type_copyable(&self,fn_expr:&Expr)->bool{
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

  fn update_lifetime(&mut self, reference:Reference, line:usize){
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

  fn add_event(&mut self, line_num: usize, event: String) {
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

  fn match_args(&mut self, line_num: usize, arg: &Expr, mut fn_name:String) {
    match arg.kind {
      // arg is variable
      ExprKind::Path(QPath::Resolved(_,p))=>{
        let bytepos=p.span.lo();
        let boundary=self.boundary_map.get(&bytepos);
        if let Some(boundary) = boundary {
          let expected=boundary.expected;
          let name = self.hirid_to_var_name(p.segments[0].hir_id);
          if let Some(name) = name {
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
            self.access_points.insert(AccessPointUsage::Function(fn_name),self.current_scope);
          }
        }
      }
      // arg is ref
      ExprKind::AddrOf(_,mutability,expr)=>{          
        match expr.kind{
          ExprKind::Path(QPath::Resolved(_,p))=>{
            if let Some(name)=self.hirid_to_var_name(p.segments[0].hir_id){
              if fn_name.contains("{") { // println
                fn_name = "println".to_string(); // this seems sus to me
                let mut_reference=Reference::Mut(name.clone());
                let sta_reference=Reference::Static(name.clone());
                if self.lifetime_map.contains_key(&mut_reference) {
                  self.update_lifetime(mut_reference, line_num);
                } else if self.lifetime_map.contains_key(&sta_reference) {
                  self.update_lifetime(sta_reference, line_num);
                }
              }
              match mutability{
                Mutability::Not=>{
                  self.add_event(line_num,format!("PassByStaticReference({}->{}())", name,fn_name));
                }
                Mutability::Mut=>{
                  self.add_event(line_num,format!("PassByMutableReference({}->{}())", name,fn_name));
                }
              }
              self.access_points.insert(AccessPointUsage::Function(fn_name),self.current_scope);
            }
          }
          _=>{}
        }
      }
      ExprKind::Call(fn_expr, _) => { //functions can be parameters too
        // NOTE, can't seem to use fn boundaries here?
    
        let callee = self.hirid_to_var_name(fn_expr.hir_id);
        if let Some(callee_name) = callee {
          if callee_name != "$crate::format_args_nl!($($arg)*)"{ // very scuffed, don't know why this gets here
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
            // add callee if we haven't already
            self.access_points.insert(AccessPointUsage::Function(callee_name),self.current_scope);
          }
        }
        
      }
      _=>{}
    }
  }


  // given a path (variable) determines if a move occurs on the line
  fn determine_move(&self, path: &'tcx Path) -> bool {
    let bytepos: rustc_span::BytePos = path.span.lo();
    if let Some(boundary) = self.boundary_map.get(&bytepos){
      return boundary.expected.drop
    }
    false
  }

  // given an expression, determine if a move occured
  fn determine_move_expr(&self, expr: &'tcx Expr) -> bool {
    match expr.kind {
      ExprKind::Path(QPath::Resolved(_,p)) => self.determine_move(p),
      ExprKind::Call(fn_expr, _) => {
        if !self.is_return_type_ref(fn_expr){
          !self.is_return_type_copyable(fn_expr)
        }
        else {
          if let Some(return_type)=self.return_type_of(fn_expr){
            if let Some(mutability)=return_type.ref_mutability(){
              match mutability{
                // if rhs is mutable ref then a move must occur
                Mutability::Mut=>{
                  false
                }
                // Reference should be copied
                Mutability::Not=>{
                  true
                }
              }
            }
            else {
              false
            }
          }
          else {
            false
          }
        }
      }
      ExprKind::Block(..) => {
        // TODO: implement
        false
      }
      ExprKind::MethodCall(..) => {
        false         // TODO: implement
      }
      _ => { false } // TODO: implement, at least for expressions we care about

    }
  }

  // match the Right-hand-side of an expression
  fn match_rhs(&mut self,lhs:AccessPoint,rhs:&'tcx Expr){
    let lhs_var=lhs.name.clone();
    let line_num = self.expr_to_line(rhs);
    match rhs.kind {
      // A path is a name and/or identifier(s)
      ExprKind::Path(QPath::Resolved(_,p)) => {
        // A resolved path to the location of it's definition
        // p is a path as well
        let bytepos=p.span.lo();
        // First thing in path.segments corresponds to a possible value (owner)
        let name = self.hirid_to_var_name(p.segments[0].hir_id);
        // Use aquascope analysis to give information about this path: is it mut, copyable, etc
        let boundary=self.boundary_map.get(&bytepos);
        // hirid_to_var_name never returns None so idk why it returns option
        if let Some(name)=name {
          // This if statement checks: Is something the path p actually happening here - see aquascope/analysis/boundaries/mod.rs for more info
          if let Some(boundary) = boundary {
            // is it mutable
            if let Some(rhs_mut)=self.mutability_map.get(&name){
              let rhs = AccessPoint{mutability:*rhs_mut,name:name.clone(), members: None};
              // use aqua analysis (pulls MIR info for us) to see if rhs is dropped
              if boundary.expected.drop {   
                // if it is dropped, that means a move occured
                  self.add_event(line_num, format!("Move({}->{})", name, lhs_var));
                // if RHS is already in our map then add LHS as owner as well (since move is occuring)
                // TODO: LHS may already be in our map - for example take expr: x += y
                if self.access_points.contains_key(&AccessPointUsage::Owner(rhs)){
                  self.access_points.insert(AccessPointUsage::Owner(lhs),self.current_scope);
                }
                // I believe this accounting for the case
                // let ref = &mut my_var, my_var already exists in the access_points map but only as an owner, not a mut ref
                else {
                  self.access_points.insert(AccessPointUsage::MutRef(lhs),self.current_scope);
                  if let Some(owner)=self.borrow_map.get(&name){
                    // lhs is borrowing from another owner (rhs)
                    if let Some(owner)=owner{
                      self.borrow_map.insert(lhs_var.clone(),Some(owner.clone()));
                    }
                    // or it's borrowing from something we don't care about?
                    else {
                      self.borrow_map.insert(lhs_var.clone(),None);
                    }
                  }
                  // can only have one live mutable ref at a time so lifetimes of each must be updated
                  self.update_lifetime(Reference::Mut(name), line_num);
                  self.update_lifetime(Reference::Mut(lhs_var), line_num);
                }
              }
              // Else a copy occurs (no drop)
              else {
                self.add_event(line_num, format!("Copy({}->{})", name, lhs_var));
                // Indicative of a let statement (let a = rhs)
                if self.access_points.contains_key(&AccessPointUsage::Owner(rhs)){
                  self.access_points.insert(AccessPointUsage::Owner(lhs),self.current_scope);
                }
                // let a = &b 
                else {
                  self.access_points.insert(AccessPointUsage::StaticRef(lhs),self.current_scope);
                  if let Some(owner)=self.borrow_map.get(&name){
                    if let Some(owner)=owner{
                      self.borrow_map.insert(lhs_var.clone(),Some(owner.clone()));
                    }
                    else {
                      self.borrow_map.insert(lhs_var.clone(),None);
                    }
                  }
                  // lifetimes updated when as borrow occurs
                  self.update_lifetime(Reference::Static(name), line_num);
                  self.update_lifetime(Reference::Static(lhs_var), line_num);
                }
              }
            }
          }
        }   
      },
      // fn_expr: resolves to function itself (Path)
      // second arg, is a list of args to the function
      ExprKind::Call(fn_expr, _) => {
        let fn_name = self.hirid_to_var_name(fn_expr.hir_id);
        if let Some(fn_name) = fn_name {
          // if return type is not a reference
          if !self.is_return_type_ref(fn_expr){
            if self.is_return_type_copyable(fn_expr) {
              self.add_event(line_num, format!("Copy({}()->{})", fn_name, lhs_var));
            }
            else {
              self.add_event(line_num, format!("Move({}()->{})", fn_name, lhs_var));
            }
            self.access_points.insert(AccessPointUsage::Owner(lhs),self.current_scope);
          }
          else {
            // return type is a reference
            if let Some(return_type)=self.return_type_of(fn_expr){
              self.borrow_map.insert(lhs_var.clone(),None);
              if let Some(mutability)=return_type.ref_mutability(){
                match mutability{
                  // if rhs is mutable ref then a move must occur
                  Mutability::Mut=>{
                    self.add_event(line_num, format!("Move({}()->{})", fn_name, lhs_var));
                    self.access_points.insert(AccessPointUsage::MutRef(lhs),self.current_scope);
                    self.update_lifetime(Reference::Mut(lhs_var), line_num);
                  }
                  // Reference should be copied
                  Mutability::Not=>{
                    self.add_event(line_num, format!("Copy({}()->{})",fn_name, lhs_var));
                    self.access_points.insert(AccessPointUsage::StaticRef(lhs),self.current_scope);
                    self.update_lifetime(Reference::Static(lhs_var), line_num);
                  }
                }
              }
            }
          }
          self.access_points.insert(AccessPointUsage::Function(fn_name),self.current_scope);
        }
      },
      // Any type of literal on RHS implies a bind
      ExprKind::Lit(_) => {
        self.add_event(line_num, format!("Bind({})", lhs_var));
        self.access_points.insert(AccessPointUsage::Owner(lhs),self.current_scope);
      }
      // Some kind of reference operation,
      // ex : &a or &mut a
      ExprKind::AddrOf(_,mutability,expr) => {
        // only care about paths of expr
        match expr.kind{
          ExprKind::Path(QPath::Resolved(_,p))=>{
            if let Some(name)=self.hirid_to_var_name(p.segments[0].hir_id){
              self.borrow_map.insert(lhs_var.clone(),Some(name.clone()));
              match mutability{
                Mutability::Not=>{
                  self.add_event(line_num,format!("StaticBorrow({}->{})", name,lhs_var));
                  self.update_lifetime(Reference::Static(lhs_var.clone()), line_num);
                  self.access_points.insert(AccessPointUsage::StaticRef(lhs),self.current_scope);
                }
                Mutability::Mut=>{
                  self.add_event(line_num,format!("MutableBorrow({}->{})", name,lhs_var));
                  self.update_lifetime(Reference::Mut(lhs_var.clone()), line_num);
                  self.access_points.insert(AccessPointUsage::MutRef(lhs),self.current_scope);
                }
              }
            }
          }
          _=>{}
        }
      }
      // RHS is a block ie:
      // let a = { if x < 3 then 3 else 2 };
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
            self.match_rhs(lhs.clone(), res_expr);
          }
          // don't know in which scenarios there is not a returned expression in a block
          None => {}
        }

      }
      // A binary operation (e.g., a + b, a * b).
      ExprKind::Binary(binop, expra, exprb) => {
        // a bind occurs lhs = a + b
        self.add_event(line_num, format!("Bind({})", lhs_var));
        self.access_points.insert(AccessPointUsage::Owner(lhs),self.current_scope);
        // define operator as function
        let new_lhs: AccessPointUsage = AccessPointUsage::Function("+".to_owned());
        self.access_points.insert(new_lhs, self.current_scope);
        // If a move occurs from one of the two operators then a move will occur from (a op b) -> LHS
        let op_of_string: String = match_op(binop.node);
        // treat expra and exprb as parameters to the (OP) function
        self.match_args(line_num, expra, op_of_string.clone());
        self.match_args(line_num, exprb, op_of_string.clone());

        // if expra or exprb move, then move occurs from OP -> LHS
        if self.determine_move_expr(expra){
          self.add_event(line_num, format!("Move({}()->{})", op_of_string, lhs_var)); // move from op to lhs
        }
        else if self.determine_move_expr(expra){
          self.add_event(line_num, format!("Move({}()->{})", op_of_string, lhs_var)); // move from op to lhs
        }
      },

      ExprKind::Unary(option, _) => {
        match option {

          /* the '*' operator for dereferencing */
          rustc_hir::UnOp::Deref => {
            // TODO: to be implemeted
            println!("Not implemented yet!");
          }

          /* the '!' operator for logical inversion */
          rustc_hir::UnOp::Not => {
            self.add_event(line_num, format!("Bind({})", lhs_var));
            self.access_points.insert(AccessPointUsage::Owner(lhs),self.current_scope);
          }

          /* the '-' operator for negation */
          rustc_hir::UnOp::Neg => {
            self.add_event(line_num, format!("Bind({})", lhs_var));
            self.access_points.insert(AccessPointUsage::Owner(lhs),self.current_scope);
          }

          _ => {
            // nothing else to do 
          }
        }
      },
      ExprKind::MethodCall(name_and_generic_args, rcvr, args, _) => {
        if let Some(fn_name) = self.hirid_to_var_name(name_and_generic_args.hir_id){
          let type_check = self.tcx.typeck(name_and_generic_args.hir_id.owner);
          if let Some(return_type) = type_check.node_type_opt(rhs.hir_id){
            if !return_type.is_ref(){
              if return_type.is_copy_modulo_regions(self.tcx, self.tcx.param_env(name_and_generic_args.hir_id.owner)) {
                self.add_event(line_num, format!("Copy({}()->{})", fn_name, lhs_var));
              }
              else {
                self.add_event(line_num, format!("Move({}()->{})", fn_name, lhs_var));
              }
              self.access_points.insert(AccessPointUsage::Owner(lhs),self.current_scope);
            }
            else {
              self.borrow_map.insert(lhs_var.clone(),None);
              if let Some(mutability)=return_type.ref_mutability(){
                match mutability{
                  Mutability::Mut=>{
                    self.add_event(line_num, format!("Move({}()->{})", fn_name, lhs_var));
                    self.access_points.insert(AccessPointUsage::MutRef(lhs),self.current_scope);
                    self.update_lifetime(Reference::Mut(lhs_var), line_num);
                  }
                  Mutability::Not=>{
                    self.add_event(line_num, format!("Copy({}()->{})",fn_name, lhs_var));
                    self.access_points.insert(AccessPointUsage::StaticRef(lhs),self.current_scope);
                    self.update_lifetime(Reference::Static(lhs_var), line_num);
                  }
                }
              }
            }
          }      
        }      
      }
      // Struct intializer list:
      // ex let struct = {a: <expr>, b: <expr>, c: <expr>}
      ExprKind::Struct(_qpath, expr_fields, _base) => {
        // LHS must be a struct 
        // A bind must occur for LHS
        self.add_event(line_num, format!("Bind({})", lhs.name));
        self.access_points.insert(AccessPointUsage::Owner(lhs.clone()), self.current_scope);
        // Insert Struct into access points -> with members
        // how to handle public members? / Functions, etc
        for field in expr_fields.iter() {
          let mut new_lhs = lhs.clone();
          new_lhs.name = format!("{}.{}", lhs.name, field.ident.to_string());
          self.match_rhs(new_lhs, field.expr);
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
    }
      _=>{}
    }
  }

  pub fn print_definitions(&mut self) -> Vec<String> {
    let mut declarations : Vec<String> = Vec::new();
    
    for (point,_) in &self.access_points {
      match point {
        AccessPointUsage::Owner(p)=>{
          declarations.push(format!("Owner {:?} {};",p.mutability,p.name));
        }
        AccessPointUsage::StaticRef(p)=>{
          declarations.push(format!("StaticRef {:?} {};",p.mutability,p.name));
        }
        AccessPointUsage::MutRef(p)=>{
          declarations.push(format!("MutRef {:?} {};",p.mutability,p.name));
        }
        AccessPointUsage::Function(name)=>{
          declarations.push(format!("Function {}();",name));
        }
        _=>{}
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
          _=>{}
        }
      }
    }
  }

  pub fn print_lifetimes(&mut self){
    let lifetime_map = self.lifetime_map.clone();
    for (reference,line_num) in &lifetime_map{
      let linenum: usize = *line_num;
      match reference{
        Reference::Mut(name)=>{
          if let Some(owner)=self.borrow_map.get(name){
            if let Some(owner)=owner{
              self.add_event(linenum,format!("MutableDie({}->{})", name,owner));
            }
            else {
              self.add_event(linenum,format!("MutableDie({}->*{})", name,name));
            }
          }
        }
        Reference::Static(name)=>{
          if let Some(owner)=self.borrow_map.get(name){
            if let Some(owner)=owner{
              self.add_event(linenum,format!("StaticDie({}->{})", name,owner));
            }
            else {
              self.add_event(linenum,format!("StaticDie({}->*{})", name,name));
            }
          }
        }
      }
    }
  }
}

// these are the visitor trait itself
// the visitor will walk through the hir
// the approach we are using is simple here. when visiting an expression or a statement,
// match it with a pattern and do analysis accordingly. The difference between expressions 
// and statements is subtle. As far as I can infer, everything is expression except "let a = b"
// is only found as statement.      
// See ExprKind at : https://doc.rust-lang.org/stable/nightly-rustc/rustc_hir/hir/enum.ExprKind.html
// See StmtKind at : https://doc.rust-lang.org/stable/nightly-rustc/rustc_hir/hir/enum.StmtKind.html
impl<'a, 'tcx> Visitor<'tcx> for ExprVisitor<'a, 'tcx> {
  fn visit_fn(&mut self, fk: FnKind<'a>, fd: &'a FnDecl<'a>, b: BodyId, span: Span, id: LocalDefId) {
  }
  fn visit_param(&mut self, param: &'tcx Param<'tcx>){
    let line_num=self.span_to_line(&param.span);
    let ty = self.tcx.typeck(param.hir_id.owner).pat_ty(param.pat);
    match param.pat.kind {
      PatKind::Binding(binding_annotation, ann_hirid, ident, op_pat) =>{
        let name = ident.to_string();
        let mutability = binding_annotation.1;
        if ty.is_ref() {
          self.add_event(line_num,format!("InitRefParam({})",name));
          if let Some(mutref)=ty.ref_mutability(){
            match mutref {
              Mutability::Not=>{
                self.update_lifetime(Reference::Static(name.clone()), line_num);
                let copy_name = name.clone();
                self.access_points.insert(AccessPointUsage::StaticRef(AccessPoint { mutability, name, members: None}), self.current_scope);
              }
              Mutability::Mut=>{
                self.update_lifetime(Reference::Mut(name.clone()), line_num);
                self.access_points.insert(AccessPointUsage::MutRef(AccessPoint { mutability, name, members: None}), self.current_scope);
              }
            }
          }
        }
        else{
          self.add_event(line_num,format!("InitOwnerParam({})",name));
          self.access_points.insert(AccessPointUsage::Owner(AccessPoint { mutability, name, members: None}), self.current_scope);
        }
      }
      _=>{}
    }
  }
  fn visit_expr(&mut self, expr: &'tcx Expr<'tcx>) {
      let hirid = expr.hir_id;
      let line_num = self.expr_to_line(expr);
        match expr.kind {
          ExprKind::Call(fn_expr, args) => {
            let fn_name = self.hirid_to_var_name(fn_expr.hir_id);
            if let Some(fn_name) = fn_name {
              if fn_name.contains("crate::io::_print"){
                // args[0] is the format string: crate::format_args_nl!($($arg)*)
                match args[0].kind {
                  ExprKind::Call(format_expr, format_args)=>{
                    for a in format_args {
                      self.visit_expr(a);
                    }
                  }
                  _=>{} 
                }
              }
            }
            for arg in args.iter(){
              if let Some(fn_name) = self.hirid_to_var_name(fn_expr.hir_id){
                self.match_args(line_num, &arg, fn_name);
              }
              // self.visit_expr(arg);
            }
          }
          ExprKind::MethodCall(name_and_generic_args, rcvr, args, _) => {
            if let Some(rcvr_name) = self.hirid_to_var_name(rcvr.hir_id){
              if let Some(fn_name) =self.hirid_to_var_name(name_and_generic_args.hir_id){
                self.add_event(line_num,format!("PassByMutableReference({}->{}())", rcvr_name, fn_name.clone()));
                for arg in args.iter(){
                  self.match_args(line_num, &arg, fn_name.clone());
                }
              }
            }
          }
          ExprKind::Binary(_, lhs, rhs) => {
            println!("Visiting higher-level binary");
          }
          ExprKind::Lit(_) => {
          }
    
          ExprKind::AddrOf(_, _, inner)
            if inner.is_syntactic_place_expr() && !inner.span.from_expansion() =>
          {
          }
          ExprKind::Assign(
            lhs,
            rhs,
            _,
          ) => {
            let mut lhs_var = "".to_string();
            match lhs.kind {
              ExprKind::Path(QPath::Resolved(_,p)) => {
                let name = self.hirid_to_var_name(p.segments[0].hir_id);
                if let Some(name) = name {
                  lhs_var = name;
                }
              },
              _=>{}
            }
            if let Some(mutability)=self.mutability_map.get(&lhs_var){
              self.match_rhs(AccessPoint { mutability:*mutability, name: lhs_var, members: None }, rhs);
            }
            //self.visit_expr(lhs);
            match rhs.kind {
              ExprKind::Path(_) => {},
              ExprKind::Block(..)=> {},
              _=>{
                self.visit_expr(rhs);
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
                // TODO: update this to a better solution, for now LHS is just an access point with the name "None"
                // this is a scuffed fix in order to adhere to rv1 visualization practices 
                self.match_rhs(AccessPoint {mutability: Mutability::Not, name: "None".to_owned(), members: None}, expr);
              }
              None => {}
            }
            // backtrack
            self.current_scope = prev_scope;
          }
    
          ExprKind::AssignOp(_, lhs, rhs) => {
            self.visit_expr(lhs);
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
        let expression = self.tcx.hir().node_to_string(local.hir_id);

        match local.pat.kind {
          // A bind is occuring
          // ann_hirid refers to the variable being bound
          PatKind::Binding(binding_annotation, ann_hirid, ident, op_pat) => {
            let lhs_var:String = ident.to_string();
            self.mutability_map.insert(lhs_var.clone(), binding_annotation.1);
            match local.init { // init refers to RHS of let
              | Some(expr) => {
                  self.match_rhs(AccessPoint { mutability: binding_annotation.1, name: lhs_var, members: None}, expr);
                  match expr.kind {
                    ExprKind::Path(_) => {},
                    ExprKind::Block(..)=>{
                    },
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