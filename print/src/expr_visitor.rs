use rustc_middle::{
    mir::Body,
    ty::{TyCtxt,Ty},
  };
  use rustc_hir::{Expr, ExprKind, QPath, Path, Mutability};
  use std::collections::{HashMap, BTreeMap};
  use rustc_span::Span;
  use aquascope::analysis::boundaries::PermissionsBoundary;
  use rustc_hir::{intravisit::Visitor, hir_id::HirId};
  use std::cmp::{Eq, PartialEq};
  use std::hash::Hash;
  use crate::expr_visitor_utils::{extract_var_name, string_of_access_point};


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

pub enum Event{
  Bind(String),
  Copy(String, String),
  Move(String, String),
  StaticBorrow(String, String),
  MutableBorrow(String, String),
  StaticDie(String, String),
  MutableDie(String, String),
  PassByStaticReference(String, String),
  PassByMutableReference(String, String),
  GoOutOfScope(String),
  InitRefParam(String),
  InitOwnerParam(String)
}

pub struct ExprVisitor<'a, 'tcx:'a> {
  pub tcx: TyCtxt<'tcx>,
  pub mir_body: &'a Body<'tcx>,
  pub boundary_map: HashMap<rustc_span::BytePos,PermissionsBoundary>,
  pub mutability_map: HashMap<String,Mutability>, // map owner name to mutablility status
  pub lifetime_map: HashMap<Reference,usize>,
  pub borrow_map: HashMap<String, Option<AccessPointUsage>>,
  pub access_points: HashMap<AccessPointUsage, usize>,
  pub current_scope: usize,
  pub analysis_result : HashMap<usize, Vec<String>>,
  pub owners: Vec<AccessPointUsage>,
  pub name_to_access_point: HashMap<String, AccessPointUsage>,
  pub event_line_map: & 'a mut BTreeMap<usize, String>,
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
  
  pub fn annotate_src(&mut self, name: String, s: Span, is_func: bool) {
    let hash = *self.hash_map.entry(name.to_string()).or_insert_with(|| {
      let current_hash = self.hashes;
      self.hashes = (self.hashes + 1) % 10;
      current_hash
    });

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
    if !v.contains(&line_contents) {
      v.push(line_contents);
    }
  }

  pub fn match_args(&mut self, line_num: usize, arg: &'tcx Expr, mut fn_name:String) {
    // add callee no matter what
    self.add_access_point(AccessPointUsage::Function(fn_name.clone()), fn_name.clone());
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
        }
      }
      // arg is ref
      ExprKind::AddrOf(_,mutability,expr)=>{        
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
            self.match_args(self.expr_to_line(expr), expr, fn_name);
          //}
        //}
      }
      ExprKind::Call(fn_expr, fn_args) => { //functions can be parameters too
        let callee_name= self.hirid_to_var_name(fn_expr.hir_id).unwrap();
        // generate annotations for function call
        self.annotate_src(callee_name.clone(), fn_expr.span, true);
        for a in fn_args.iter() {
          self.match_args(self.expr_to_line(a), a, callee_name.clone());
        }
        return;

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
                }
              }
              _ => {} // todo: implement other types of derefs, only handling paths for now
            }
          }
          _ => {
            self.match_args(self.expr_to_line(&expr), expr, fn_name);
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
              self.annotate_src(name.clone(), p.span, false);
              self.annotate_src(field_name.clone(), id.span, false); 
              if expected.drop{
                self.add_event(line_num,format!("Move({}->{}())", format!("{}.{}", name, field_name), fn_name));
              }
              else if expected.write{                          
                self.add_event(line_num,format!("PassByMutableReference({}->{}())", format!("{}.{}", name, field_name), fn_name));
              }
              else if expected.read{
                self.add_event(line_num,format!("PassByStaticReference({}->{}())", format!("{}.{}", name, field_name), fn_name));
              }
            }
          }
          _ => { println!("wacky struct expr")}
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
  pub fn match_rhs(&mut self, lhs:AccessPoint, rhs:&'tcx Expr, is_deref: bool){
    let lhs_var=lhs.name.clone();
    let line_num = self.expr_to_line(rhs);
    match rhs.kind {
      ExprKind::Path(QPath::Resolved(_,p)) => {
        let bytepos=p.span.lo();
        let name: String = self.tcx.hir().name(p.segments[0].hir_id).as_str().to_owned();
        self.annotate_src(name.clone(), p.segments[0].ident.span, false);
        // Use aquascope analysis to give information about this path: is it mut, copyable, etc
        let boundary=self.boundary_map.get(&bytepos);
        //println!("rhs name: {}, line: {}, lo: {:#?}, hi: {:#?}", name, self.expr_to_line(rhs), self.tcx.sess.source_map().lookup_char_pos(rhs.span.lo()), self.tcx.sess.source_map().lookup_char_pos(rhs.span.hi()));
        // This if statement checks: Is something the path p actually happening here - see aquascope/analysis/boundaries/mod.rs for more info
        if let Some(boundary) = boundary {
          if let Some(rhs_mut)=self.mutability_map.get(&name){
            // will have to change this for structs
            let rhs = AccessPoint{mutability:*rhs_mut,name:name.clone(), members: None};
            // use aqua analysis to see if rhs is dropped (ie rhs is mut)
            if boundary.expected.drop {
              if is_deref {
                self.add_event(line_num, format!("Move({}->*{})", name, lhs_var));
              }
              else {
                self.add_event(line_num, format!("Move({}->{})", name, lhs_var));
              }
              if self.access_points.contains_key(&AccessPointUsage::Owner(rhs.clone())){
                self.add_access_point(AccessPointUsage::Owner(lhs), lhs_var);
              }
              // let a = b (where b is a mut ref)
              else {
                self.add_access_point(AccessPointUsage::MutRef(lhs), lhs_var.clone());
                let owner =self.borrow_map.get(&name).unwrap();
                // lhs is borrowing from another owner (rhs)
                if let Some(owner)=owner{
                  self.borrow_map.insert(lhs_var.clone(),Some(owner.clone()));
                }
                else {
                  self.borrow_map.insert(lhs_var.clone(),None);
                }
                self.update_lifetime(Reference::Mut(name), line_num);
                self.update_lifetime(Reference::Mut(lhs_var), line_num);
              }
            }
            // Else a copy occurs (no drop)
            else {
              if is_deref {
                self.add_event(line_num, format!("Copy({}->*{})", name, lhs_var));
              }
              else {
                self.add_event(line_num, format!("Copy({}->{})", name, lhs_var));
              }
              // if rhs is an owner then lhs is an owner
              if self.access_points.contains_key(&AccessPointUsage::Owner(rhs.clone())){
                self.add_access_point(AccessPointUsage::Owner(lhs), lhs_var);
              }
              // else rhs is a static ref
              else {
                self.add_access_point(AccessPointUsage::StaticRef(lhs), lhs_var.clone());
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
        let fn_name = self.hirid_to_var_name(fn_expr.hir_id).unwrap();
        // if return type is not a reference
        if !self.is_return_type_ref(fn_expr){
          if self.is_return_type_copyable(fn_expr) {
            if is_deref {
              self.add_event(line_num, format!("Copy({}()->*{})", fn_name, lhs_var));
            }
            else {
              self.add_event(line_num, format!("Copy({}()->{})", fn_name, lhs_var));
            }
          }
          else {
            if is_deref {
              self.add_event(line_num, format!("Move({}()->*{})", fn_name, lhs_var));;
            }
            else {
              self.add_event(line_num, format!("Move({}()->{})", fn_name, lhs_var));
            }
          }
          self.annotate_src(fn_name.clone(), fn_expr.span, true);
          self.add_access_point(AccessPointUsage::Owner(lhs), lhs_var);
        }
        // return type is a reference
        else {
          if let Some(return_type)=self.return_type_of(fn_expr){
            self.borrow_map.insert(lhs_var.clone(),None); //borrowing from none
            if let Some(mutability)=return_type.ref_mutability(){
              match mutability{
                // if rhs is mutable ref then a move must occur
                Mutability::Mut=>{
                  if is_deref {
                    self.add_event(line_num, format!("Move({}()->*{})", fn_name, lhs_var));;
                  }
                  else {
                    self.add_event(line_num, format!("Move({}()->{})", fn_name, lhs_var));
                  }
                  self.add_access_point(AccessPointUsage::MutRef(lhs), lhs_var.clone());
                  self.update_lifetime(Reference::Mut(lhs_var), line_num);
                }
                // Reference should be copied
                Mutability::Not=>{
                  if is_deref {
                    self.add_event(line_num, format!("Copy({}()->*{})", fn_name, lhs_var));
                  }
                  else {
                    self.add_event(line_num, format!("Copy({}()->{})", fn_name, lhs_var));
                  }
                  self.add_access_point(AccessPointUsage::StaticRef(lhs), lhs_var.clone());
                  self.update_lifetime(Reference::Static(lhs_var), line_num);
                }
              }
            }
          }
        }
        self.add_access_point(AccessPointUsage::Function(fn_name.clone()), fn_name);
      },
      // Any type of literal on RHS implies a bind
      ExprKind::Lit(_) => {
        match self.name_to_access_point.get(&lhs_var) {
          None => {
            self.add_event(line_num, format!("Bind({})", lhs_var));
            self.add_access_point(AccessPointUsage::Owner(lhs.clone()), lhs_var);
          }
          Some(p) => {
            match p {
              AccessPointUsage::MutRef(_) => {}
              _ => {
                // this is to prevent re-annotation of binds for mutable variables
                if lhs.mutability == Mutability::Not {
                  self.add_event(line_num, format!("Bind({})", lhs_var));
                  self.add_access_point(AccessPointUsage::Owner(lhs.clone()), lhs_var);
                }
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
            let name: String = self.tcx.hir().name(p.segments[0].hir_id).as_str().to_owned();
            if let Some(ref_to) = self.name_to_access_point.get(&name) {
              self.borrow_map.insert(lhs_var.clone(),Some(ref_to.clone()));
            }
            else {
              self.borrow_map.insert(lhs_var.clone(),None);
            }
            self.annotate_src(name.clone(), p.span, false);
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
          // don't know in which scenarios there is not a returned expression in a block
          None => {}
        }

      }
      // A binary operation (e.g., <expr> + <expr>, <expr> * <expr>).
      ExprKind::Binary(..) => {
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
                let lhs_name = lhs.name.clone();
                let bytepos=rhs.span.lo(); // small discrepancy with boundary map, bytePos corresponds to bytePos of deref operator not path
                let boundary=self.boundary_map.get(&bytepos);
                if let Some(boundary) = boundary {
                  let expected=boundary.expected;
                  let name = self.tcx.hir().name(p.segments[0].hir_id).as_str().to_owned();
                  self.annotate_src(name.clone(), p.span, false);

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
                    if expected.drop{ // TODO: test in what scenarios this happens
                      self.add_event(line_num,format!("Move(*{}->{}())", name, lhs_name));
                      self.update_lifetime(Reference::Mut(name), line_num);
                    }
                    else {
                      self.add_event(line_num, format!("Copy(*{}->{})", name, lhs_name));
                      self.update_lifetime(Reference::Static(name), line_num);
                    }
                  }
                }
              }
              _ => {} // todo: implement other types of derefs, only handling paths for now
            }
          }
          // ! and ~ operators
          _ => {
            if lhs.mutability == Mutability::Not && !is_deref {
              self.add_event(line_num, format!("Bind({})", lhs_var));
              self.add_access_point(AccessPointUsage::Owner(lhs), lhs_var);
            }
          }
        }
      },
      ExprKind::MethodCall(name_and_generic_args, rcvr, args, _) => {
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
        if lhs.mutability == Mutability::Not && !is_deref {
          self.add_event(line_num, format!("Bind({})", lhs.name));
          let mut field_vec: Vec<AccessPoint> = Vec::new();
          // Insert Struct into access points -> with members
          for field in expr_fields.iter() {
            let mut new_lhs = lhs.clone();
            new_lhs.name = format!("{}.{}", lhs.name, field.ident.to_string());
            // TODO: will have to actually annotate the struct fields
            self.match_rhs(new_lhs.clone(), field.expr, false);
            field_vec.push(new_lhs);
          }
          self.add_access_point(AccessPointUsage::Struct(lhs, field_vec), lhs_var);
        }
      },

      ExprKind::Field(expr, id) => {
        match expr {
          Expr{kind: ExprKind::Path(QPath::Resolved(_,p)), ..} => {
            let bytepos=p.span.lo(); // small discrepancy with boundary map, bytePos corresponds to bytePos of deref operator not path
            let boundary=self.boundary_map.get(&bytepos);
            if let Some(boundary) = boundary {
              let expected=boundary.expected;
              let name = self.tcx.hir().name(p.segments[0].hir_id).as_str().to_owned();
              let field_name: String = id.as_str().to_owned();
              self.annotate_src(name.clone(), p.span, false);
              self.annotate_src(field_name.clone(), id.span, false);
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