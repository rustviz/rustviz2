// Some headers
use rustc_middle::{
  mir::Body,
  ty::{TyCtxt,Ty},
};
use rustc_hir::{StmtKind, Stmt, Local, Expr, ExprKind, UnOp, Param,
  QPath, Path, def::Res, PatKind, Mutability, FnDecl, BodyId, Constness, Unsafety, IsAsync};
use rustc_utils::mir::mutability;
use std::{collections::{HashMap, BTreeMap}, clone};
use rustc_ast::walk_list;
use rustc_span::Span;
use aquascope::analysis::boundaries::PermissionsBoundary;
use rustc_hir::{intravisit::{self, Visitor, FnKind},hir_id::HirId, def_id::LocalDefId};
use std::cmp::{Eq, PartialEq};
use std::hash::Hash;

#[derive(Eq, PartialEq, Hash, Clone)]
pub struct AccessPoint {
  pub mutability: Mutability,
  pub name:String,
  pub members: Option<Vec<AccessPoint>>,
}

#[derive(Eq, PartialEq,Hash, Clone)]
pub enum AccessPointUsage{
  Owner(AccessPoint),
  MutRef(AccessPoint),
  StaticRef(AccessPoint),
  Struct(AccessPoint, Vec<AccessPoint>),
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
  pub owners: Vec<AccessPointUsage>,
  pub event_line_map: & 'a mut BTreeMap<usize, String>,
  pub source_map: & 'a BTreeMap<usize, String>,
  pub annotated_lines: & 'a mut BTreeMap<usize, Vec<String>>,
  pub hash_map: & 'a mut HashMap<String, usize>,
  pub hashes: usize
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

  fn annotate_src(&mut self, name: String, s: Span, is_func: bool) {
    let hash:usize = if self.hash_map.contains_key(&name) {
      self.hash_map[&name]
    }
    else {
      self.hash_map.insert(name.clone(), self.hashes);
      self.hashes = (self.hashes + 1) % 10;
      self.hash_map[&name]
    };


    let line: usize = self.span_to_line(&s);
    let left:usize = self.tcx.sess.source_map().lookup_char_pos(s.lo()).col_display;
    let right: usize = self.tcx.sess.source_map().lookup_char_pos(s.hi()).col_display;

    let mut line_contents:String = self.source_map.get(&line).unwrap().clone();
    let replace_with: String = if is_func {
        format!("<tspan class=\"fn\" data-hash=\"{}\" hash=\"{}\">{}</tspan>", 0, hash, name)
      } else {
        format!("<tspan data-hash=\"{}\">{}</tspan>", hash, name)
      };
    line_contents.replace_range(left..right, &replace_with);
    let v = self.annotated_lines.get_mut(&line).unwrap();
    match v.iter().position(|x| x == &line_contents) {
      Some(_) => {} // can't have duplicates in the map, due to string union algorithm
      None => {
        v.push(line_contents);
      }
    }
  }

  fn match_args(&mut self, line_num: usize, arg: &'tcx Expr, mut fn_name:String) {
    if fn_name == "{}" {
      fn_name = "println".to_string();
    }
    match arg.kind {
      // arg is variable
      ExprKind::Path(QPath::Resolved(_,p))=>{
        let bytepos=p.span.lo();
        let boundary=self.boundary_map.get(&bytepos);
        let name: String = self.tcx.hir().name(p.segments[0].hir_id).as_str().to_owned();
        if let Some(boundary) = boundary {
          let expected=boundary.expected;
          self.annotate_src(name.clone(), p.span, false);
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
          self.access_points.insert(AccessPointUsage::Function(fn_name.clone()),self.current_scope);
          self.owners.push(AccessPointUsage::Function(fn_name));
        }
      }
      // arg is ref
      ExprKind::AddrOf(_,mutability,expr)=>{    
        println!("addrOf arg");      
        // match expr.kind{
        //   ExprKind::Path(QPath::Resolved(_,p))=>{
        //     if let Some(name)=self.hirid_to_var_name(p.segments[0].hir_id){
        //       if fn_name.contains("{") { // println (refers to formatting function {})
        //         fn_name = "println".to_string(); // scuffed (and incorrect for other formatting cases)
        //         let mut_reference=Reference::Mut(name.clone());
        //         let sta_reference=Reference::Static(name.clone());
        //         if self.lifetime_map.contains_key(&mut_reference) {
        //           self.update_lifetime(mut_reference, line_num);
        //         } else if self.lifetime_map.contains_key(&sta_reference) {
        //           self.update_lifetime(sta_reference, line_num);
        //         }
        //       }
        //       match mutability{
        //         Mutability::Not=>{
        //           self.add_event(line_num,format!("PassByStaticReference({}->{}())", name,fn_name));
        //         }
        //         Mutability::Mut=>{
        //           self.add_event(line_num,format!("PassByMutableReference({}->{}())", name,fn_name));
        //         }
        //       }
        //       self.access_points.insert(AccessPointUsage::Function(fn_name),self.current_scope);
        //     }
        //   }
        //   _=>{
            self.access_points.insert(AccessPointUsage::Function(fn_name.clone()),self.current_scope);
            self.owners.push(AccessPointUsage::Function(fn_name.clone()));
            self.match_args(self.expr_to_line(expr), expr, fn_name);
          //}
        //}
      }
      ExprKind::Call(fn_expr, fn_args) => { //functions can be parameters too
          let callee_name = self.hirid_to_var_name(fn_expr.hir_id).unwrap();
          // very scuffed find a more elegant solution, this is for handling {} function
          if callee_name != "$crate::format_args_nl!($($arg)*)"{
            // generate annotations for function call
            self.annotate_src(callee_name.clone(), fn_expr.span, true);
            for a in fn_args.iter() {
              self.match_args(self.expr_to_line(a), a, callee_name.clone());
            }


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
            self.access_points.insert(AccessPointUsage::Function(callee_name.clone()),self.current_scope);
            self.owners.push(AccessPointUsage::Function(callee_name));
          }
      }
      ExprKind::Unary(option, expr) => {
        match option {
          rustc_hir::UnOp::Deref => {
            match expr.kind {
              ExprKind::Path(QPath::Resolved(_,p))=>{
                let bytepos=arg.span.lo(); // small discrepancy with boundary map, bytePos corresponds to bytePos of deref operator not path
                let boundary=self.boundary_map.get(&bytepos);
                if let Some(boundary) = boundary {
                  let expected=boundary.expected;
                  let name = self.tcx.hir().name(p.segments[0].hir_id).as_str().to_owned();
                  self.annotate_src(name.clone(), p.span, false);
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
                  self.access_points.insert(AccessPointUsage::Function(fn_name.clone()),self.current_scope);
                  self.owners.push(AccessPointUsage::Function(fn_name));
                }
              }
              _ => {} // todo: implement other types of derefs, only handling paths for now
            }
          }

          /* the '!' operator for logical inversion */
          rustc_hir::UnOp::Not => {
            self.match_args(self.expr_to_line(&expr), expr, fn_name);
          }

          /* the '~' operator for negation */
          rustc_hir::UnOp::Neg => {
            self.match_args(self.expr_to_line(&expr), expr, fn_name);
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
          let return_type = self.return_type_of(fn_expr).unwrap();
          let mutability = return_type.ref_mutability().unwrap();
              match mutability{
                // if rhs is mutable ref then a move must occur
                Mutability::Mut=>{
                  true
                }
                // Reference should be copied
                Mutability::Not=>{
                  false
                }
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
        // p is a path as 
        let bytepos=p.span.lo();
        // First thing in path.segments corresponds to a possible value (owner)
        //let name = self.hirid_to_var_name(p.segments[0].hir_id).unwrap();
        let name: String = self.tcx.hir().name(p.segments[0].hir_id).as_str().to_owned();
        //println!("NAME: {}", name);
        self.annotate_src(name.clone(), p.segments[0].ident.span, false);
        // Use aquascope analysis to give information about this path: is it mut, copyable, etc
        let boundary=self.boundary_map.get(&bytepos);
        // hirid_to_var_name never returns None so idk why it returns option
        //println!("rhs name: {}, line: {}, lo: {:#?}, hi: {:#?}", name, self.expr_to_line(rhs), self.tcx.sess.source_map().lookup_char_pos(rhs.span.lo()), self.tcx.sess.source_map().lookup_char_pos(rhs.span.hi()));
        // This if statement checks: Is something the path p actually happening here - see aquascope/analysis/boundaries/mod.rs for more info
        if let Some(boundary) = boundary {
          //println!("lhs made it here {}", lhs.name);
          // is it mutable
          if let Some(rhs_mut)=self.mutability_map.get(&name){
            //println!("rhs_mut made it here");
            let rhs = AccessPoint{mutability:*rhs_mut,name:name.clone(), members: None};
            // use aqua analysis (pulls MIR info for us) to see if rhs is dropped
            if boundary.expected.drop {   
              // if it is dropped, that means a move occured
                self.add_event(line_num, format!("Move({}->{})", name, lhs_var));
              // if RHS is already in our map then add LHS as owner as well (since move is occuring)
              // TODO: LHS may already be in our map - for example take expr: x += y
              if self.access_points.contains_key(&AccessPointUsage::Owner(rhs)){
                self.access_points.insert(AccessPointUsage::Owner(lhs.clone()),self.current_scope);
                self.owners.push(AccessPointUsage::Owner(lhs));
              }
              // I believe this accounting for the case
              // let ref = &mut my_var, my_var already exists in the access_points map but only as an owner, not a mut ref
              else {
                self.access_points.insert(AccessPointUsage::MutRef(lhs.clone()),self.current_scope);
                self.owners.push(AccessPointUsage::MutRef(lhs));
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
                self.access_points.insert(AccessPointUsage::Owner(lhs.clone()),self.current_scope);
                self.owners.push(AccessPointUsage::Owner(lhs));
              }
              // let a = &b 
              else {
                self.access_points.insert(AccessPointUsage::StaticRef(lhs.clone()),self.current_scope);
                self.owners.push(AccessPointUsage::StaticRef(lhs));
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
      },
      // fn_expr: resolves to function itself (Path)
      // second arg, is a list of args to the function
      ExprKind::Call(fn_expr, _) => {
        let fn_name = self.hirid_to_var_name(fn_expr.hir_id);
        if let Some(fn_name) = fn_name {
          // if return type is not a reference
          self.annotate_src(fn_name.clone(), fn_expr.span, true);
          //println!("name: {}, line: {}, lo: {:#?}, hi: {:#?}", fn_name, self.expr_to_line(rhs), self.tcx.sess.source_map().lookup_char_pos(fn_expr.span.lo()), self.tcx.sess.source_map().lookup_char_pos(fn_expr.span.hi()));
          if !self.is_return_type_ref(fn_expr){
            if self.is_return_type_copyable(fn_expr) {
              self.add_event(line_num, format!("Copy({}()->{})", fn_name, lhs_var));
            }
            else {
              self.add_event(line_num, format!("Move({}()->{})", fn_name, lhs_var));
            }
            self.access_points.insert(AccessPointUsage::Owner(lhs.clone()),self.current_scope);
            self.owners.push(AccessPointUsage::Owner(lhs));
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
                    self.access_points.insert(AccessPointUsage::MutRef(lhs.clone()),self.current_scope);
                    self.owners.push(AccessPointUsage::MutRef(lhs));
                    self.update_lifetime(Reference::Mut(lhs_var), line_num);
                  }
                  // Reference should be copied
                  Mutability::Not=>{
                    self.add_event(line_num, format!("Copy({}()->{})",fn_name, lhs_var));
                    self.access_points.insert(AccessPointUsage::StaticRef(lhs.clone()),self.current_scope);
                    self.owners.push(AccessPointUsage::StaticRef(lhs));
                    self.update_lifetime(Reference::Static(lhs_var), line_num);
                  }
                }
              }
            }
          }
          self.access_points.insert(AccessPointUsage::Function(fn_name.clone()),self.current_scope);
          self.owners.push(AccessPointUsage::Function(fn_name));
        }
      },
      // Any type of literal on RHS implies a bind
      ExprKind::Lit(_) => {
        self.add_event(line_num, format!("Bind({})", lhs_var));
        self.access_points.insert(AccessPointUsage::Owner(lhs.clone()),self.current_scope);
        self.owners.push(AccessPointUsage::Owner(lhs));
      }
      // Some kind of reference operation,
      // ex : &a or &mut a
      ExprKind::AddrOf(_,mutability,expr) => {
        // only care about paths of expr
        println!("rhs addrof span {:#?}", expr.span.lo());
        match expr.kind{
          ExprKind::Path(QPath::Resolved(_,p))=>{
            let name = self.hirid_to_var_name(p.segments[0].hir_id).unwrap();
            self.borrow_map.insert(lhs_var.clone(),Some(name.clone()));
            match mutability{
              Mutability::Not=>{
                self.add_event(line_num,format!("StaticBorrow({}->{})", name,lhs_var));
                self.update_lifetime(Reference::Static(lhs_var.clone()), line_num);
                self.access_points.insert(AccessPointUsage::StaticRef(lhs.clone()),self.current_scope);
                self.owners.push(AccessPointUsage::StaticRef(lhs));
              }
              Mutability::Mut=>{
                self.add_event(line_num,format!("MutableBorrow({}->{})", name,lhs_var));
                self.update_lifetime(Reference::Mut(lhs_var.clone()), line_num);
                self.access_points.insert(AccessPointUsage::MutRef(lhs.clone()),self.current_scope);
                self.owners.push(AccessPointUsage::MutRef(lhs));
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
        self.access_points.insert(AccessPointUsage::Owner(lhs.clone()),self.current_scope);
        self.owners.push(AccessPointUsage::Owner(lhs));
        // define operator as function
        let new_lhs: AccessPointUsage = AccessPointUsage::Function("+".to_owned());
        self.access_points.insert(new_lhs, self.current_scope);
        self.owners.push(AccessPointUsage::Function("+".to_owned()));
        // If a move occurs from one of the two operators then a move will occur from (a op b) -> LHS
        let op_of_string: String = match_op(binop.node);
        // treat expra and exprb as parameters to the (OP) function
        self.match_args(line_num, expra, op_of_string.clone());
        self.match_args(line_num, exprb, op_of_string.clone());

        // if expra or exprb move, then move occurs from OP -> LHS
        // if self.determine_move_expr(expra){
        //   self.add_event(line_num, format!("Move({}()->{})", op_of_string, lhs_var)); // move from op to lhs
        // }
        // else if self.determine_move_expr(expra){
        //   self.add_event(line_num, format!("Move({}()->{})", op_of_string, lhs_var)); // move from op to lhs
        // }
      },

      ExprKind::Unary(option, expr) => {
        match option {
          /* the '*' operator for dereferencing */
          rustc_hir::UnOp::Deref => {
            // find span corresponding to deref operator (this is the byte pos that's stored in the boundary map)
            match expr.kind {
              ExprKind::Path(QPath::Resolved(_,p))=>{
                let lhs_name = lhs.name.clone();
                let bytepos=rhs.span.lo(); // small discrepancy with boundary map, bytePos corresponds to bytePos of deref operator not path
                let boundary=self.boundary_map.get(&bytepos);
                if let Some(boundary) = boundary {
                  let expected=boundary.expected;
                  let name = self.tcx.hir().name(p.segments[0].hir_id).as_str().to_owned();
                  self.annotate_src(name.clone(), p.span, false);
                  if expected.drop{
                    self.add_event(line_num,format!("Move({}->{}())", name, lhs_name));
                  }
                  else {
                    self.add_event(line_num, format!("Copy({}->{})", name, lhs_name));
                  }
                }
              }
              _ => {} // todo: implement other types of derefs, only handling paths for now
            }
          }

          /* the '!' operator for logical inversion */
          rustc_hir::UnOp::Not => {
            self.add_event(line_num, format!("Bind({})", lhs_var));
            self.access_points.insert(AccessPointUsage::Owner(lhs.clone()),self.current_scope);
            self.owners.push(AccessPointUsage::Owner(lhs));
          }

          /* the '~' operator for negation */
          rustc_hir::UnOp::Neg => {
            self.add_event(line_num, format!("Bind({})", lhs_var));
            self.access_points.insert(AccessPointUsage::Owner(lhs.clone()),self.current_scope);
            self.owners.push(AccessPointUsage::Owner(lhs));
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
              self.access_points.insert(AccessPointUsage::Owner(lhs.clone()),self.current_scope);
              self.owners.push(AccessPointUsage::Owner(lhs));
            }
            else {
              self.borrow_map.insert(lhs_var.clone(),None);
              if let Some(mutability)=return_type.ref_mutability(){
                match mutability{
                  Mutability::Mut=>{
                    self.add_event(line_num, format!("Move({}()->{})", fn_name, lhs_var));
                    self.access_points.insert(AccessPointUsage::MutRef(lhs.clone()),self.current_scope);
                    self.owners.push(AccessPointUsage::MutRef(lhs));
                    self.update_lifetime(Reference::Mut(lhs_var), line_num);
                  }
                  Mutability::Not=>{
                    self.add_event(line_num, format!("Copy({}()->{})",fn_name, lhs_var));
                    self.access_points.insert(AccessPointUsage::StaticRef(lhs.clone()),self.current_scope);
                    self.owners.push(AccessPointUsage::StaticRef(lhs));
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
        let mut field_vec: Vec<AccessPoint> = Vec::new();
        // Insert Struct into access points -> with members
        for field in expr_fields.iter() {
          let mut new_lhs = lhs.clone();
          new_lhs.name = format!("{}.{}", lhs.name, field.ident.to_string());
          self.match_rhs(new_lhs.clone(), field.expr);
          field_vec.push(new_lhs);
        }

        self.access_points.insert(AccessPointUsage::Struct(lhs.clone(), field_vec.clone()), self.current_scope);
        self.owners.push(AccessPointUsage::Struct(lhs, field_vec));
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
              "{{{}}}",
              field_vec
                  .iter()
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
      if !declarations.contains(&owner_name) {
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
                self.access_points.insert(AccessPointUsage::StaticRef(AccessPoint { mutability, name: name.clone(), members: None}), self.current_scope);
                self.owners.push(AccessPointUsage::StaticRef(AccessPoint { mutability, name, members: None}));
              }
              Mutability::Mut=>{
                self.update_lifetime(Reference::Mut(name.clone()), line_num);
                self.access_points.insert(AccessPointUsage::MutRef(AccessPoint { mutability, name: name.clone(), members: None}), self.current_scope);
                self.owners.push(AccessPointUsage::MutRef(AccessPoint { mutability, name, members: None}));
              }
            }
          }
        }
        else{
          self.add_event(line_num,format!("InitOwnerParam({})",name));
          self.access_points.insert(AccessPointUsage::Owner(AccessPoint { mutability, name: name.clone(), members: None}), self.current_scope);
          self.owners.push(AccessPointUsage::Owner(AccessPoint { mutability, name, members: None}));
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
            let fn_name = self.hirid_to_var_name(fn_expr.hir_id).unwrap();
            // if fn_name.contains("crate::io::_print"){
            //   println!("args: {:#?}", args);
            //   // args[0] is the format string: crate::format_args_nl!($($arg)*)
            //   match args[0].kind {
            //     ExprKind::Call(format_expr, format_args)=>{
            //       let fn_name2 = self.hirid_to_var_name(format_expr.hir_id);
            //       if let Some(fn_name2) = fn_name2 {
            //         println!("func 2 name {}", fn_name2);
            //       }
            //       for a in format_args {
            //         //self.visit_expr(a);
            //       }
            //     }
            //     _=>{} 
            //   }
            // }
            // self.access_points.insert(AccessPointUsage::Function(fn_name.clone()), self.current_scope);
            // let lhs = AccessPoint{mutability: Mutability::Not, name: fn_name, members: None};
            // for arg in args.iter() {
            //   self.match_rhs(lhs.clone(), arg);
            // }
            if fn_name.contains("crate::io::_print"){
              // args[0] is the format string: crate::format_args_nl!($($arg)*)
              match args[0].kind {
                ExprKind::Call(_, format_args)=>{
                  for a in format_args {
                    self.visit_expr(a);
                  }
                }
                _=>{} 
              }
            }
            else if fn_name != "{}"{
              self.annotate_src(fn_name.clone(), fn_expr.span, true);
            }
            for arg in args.iter(){
              self.match_args(line_num, &arg, fn_name.clone());
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
    
          ExprKind::AddrOf(_, _, inner) if inner.is_syntactic_place_expr() && !inner.span.from_expansion() => {}

          ExprKind::Assign(lhs,rhs, _,) => {
            let mut lhs_var = "".to_string();
            self.visit_expr(rhs); // visit rhs side first to order moves?
            match lhs.kind {
              // doesn't really make sense to assign to anything but a variable, TODO: test with assigning to ref, deref
              ExprKind::Path(QPath::Resolved(_,p)) => {
                lhs_var = self.hirid_to_var_name(p.segments[0].hir_id).unwrap();
                self.annotate_src(lhs_var.clone(), p.segments[0].ident.span, false);
              },
              _=>{
                println!("assigning to something other than a variable");
              }
            }
            if let Some(mutability)=self.mutability_map.get(&lhs_var){
              self.match_rhs(AccessPoint { mutability:*mutability, name: lhs_var, members: None }, rhs);
            }
            //self.visit_expr(lhs);
            // match rhs.kind {
            //   ExprKind::Path(_) => {},
            //   ExprKind::Block(..)=> {},
            //   _=>{
            //     self.visit_expr(rhs);
            //   }
            // }
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
              self.match_rhs(AccessPoint { mutability: binding_annotation.1, name: lhs_var, members: None}, expr);
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