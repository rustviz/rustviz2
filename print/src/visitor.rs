// Some headers
use rustc_middle::{
  mir::{Body},
  ty::{TyCtxt,Ty},
};
use rustc_hir::{StmtKind, Stmt, Local, Expr, ExprKind, UnOp, QPath, Path, def::Res, PatKind, Mutability};
use std::{collections::HashMap};
use rustc_ast::walk_list;
use rustc_span::Span;
use aquascope::analysis::{
  boundaries::PermissionsBoundary};
use rustc_hir::{intravisit::{self, Visitor},hir_id::HirId};
use std::cmp::{Eq, PartialEq};
use std::hash::{Hash, Hasher};

#[derive(Eq, PartialEq)]
pub struct AccessPoint {
  mutability: Mutability,
  name:String,
} 
impl Hash for AccessPoint {
  fn hash<H: Hasher>(&self, state: &mut H) {
      self.name.hash(state);
  }
}

#[derive(Eq, PartialEq,Hash)]
pub enum AccessPointUsage{
  Owner(AccessPoint),
  MutRef(AccessPoint),
  StaticRef(AccessPoint),
  Struct(Vec<AccessPoint>),
  Function(String),
}


// A small helper function
fn extract_var_name(input_string: &str ) -> Option<String> {
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

// Implement the visitor 
pub struct ExprVisitor<'a, 'tcx:'a> {
  pub tcx: TyCtxt<'tcx>,
  pub mir_body: &'a Body<'tcx>,
  pub boundary_map: HashMap<rustc_span::BytePos,PermissionsBoundary>,
  pub access_points: HashMap<AccessPointUsage,bool>,
  pub mutability_map: HashMap<String,Mutability>,
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
  fn match_rhs(&mut self,lhs:AccessPoint,rhs:&Expr){
    let lhs_var=&lhs.name;
    match rhs.kind {
      ExprKind::Path(QPath::Resolved(_,p)) => {
        let bytepos=p.span.lo();
          let boundary=self.boundary_map.get(&bytepos);
          if let Some(boundary) = boundary {
            if boundary.expected.drop {
              let name = self.hirid_to_var_name(p.segments[0].hir_id);
              if let Some(name) = name {
                println!("Move({}->{})", name, lhs_var);
              }
            } else {
              let name = self.hirid_to_var_name(p.segments[0].hir_id);
              if let Some(name) = name {
                println!("Copy({}->{})", name, lhs_var);
              }
            }
            self.access_points.insert(AccessPointUsage::Owner(lhs),false);
          }
      },
      ExprKind::Call(fn_expr, _) => {
        let fn_name = self.hirid_to_var_name(fn_expr.hir_id);
        if let Some(fn_name) = fn_name {
          if !self.is_return_type_ref(fn_expr){
            if self.is_return_type_copyable(fn_expr) {
              println!("Copy({}()->{})", fn_name, lhs_var);
            }
            else {
              println!("Move({}()->{})", fn_name, lhs_var);
            }
            self.access_points.insert(AccessPointUsage::Owner(lhs),false);
            self.access_points.insert(AccessPointUsage::Function(fn_name),false);
          }
        }
      },
      ExprKind::Lit(_) => {
        println!("Bind({})", lhs_var);
        self.access_points.insert(AccessPointUsage::Owner(lhs),false);
      }
      ExprKind::AddrOf(_,mutability,expr) => {
        match expr.kind{
          ExprKind::Path(QPath::Resolved(_,p))=>{
            if let Some(name)=self.hirid_to_var_name(p.segments[0].hir_id){
              match mutability{
                Mutability::Not=>{
                  println!("StaticBorrow({}->{})",name,lhs_var);
                  self.access_points.insert(AccessPointUsage::StaticRef(lhs),false);
                }
                Mutability::Mut=>{
                  println!("MutableBorrow({}->{})",name,lhs_var);
                  self.access_points.insert(AccessPointUsage::MutRef(lhs),false);
                }
              }
            }
          }
          _=>{}
        }
      }
      _=>{}
    }
  }
  pub fn print_definitions(&self){
    println!();
    println!("/*--- BEGIN Variable Definitions ---");
    for (point,_) in &self.access_points {
      match point {
        AccessPointUsage::Owner(p)=>{
          println!("Owner {:?} {};",p.mutability,p.name);
        }
        AccessPointUsage::StaticRef(p)=>{
          println!("StaticRef {:?} {};",p.mutability,p.name);
        }
        AccessPointUsage::MutRef(p)=>{
          println!("MutRef {:?} {};",p.mutability,p.name);
        }
        AccessPointUsage::Function(name)=>{
          println!("Function {}();",name);
        }
        _=>{}
      }
    }
    println!("--- END Variable Definitions ---*/");
  }
  pub fn print_out_of_scope(&self){
    println!();
    for (point,gos) in &self.access_points {
      if !gos{
        match point {
          AccessPointUsage::Owner(p)|
          AccessPointUsage::StaticRef(p)|
          AccessPointUsage::MutRef(p)=>{
            println!("GoOutOfScope({})",p.name);
          }
          _=>{}
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
  fn visit_expr(&mut self, expr: &'tcx Expr<'tcx>) {
      let hirid = expr.hir_id;
        match expr.kind {
          ExprKind::Call(fn_expr, args) => {
            println!();
            println!("Function Call: {}", self.tcx.hir().node_to_string(hirid));
            println!("On line: {}", self.expr_to_line(expr));
            for arg in args.iter(){
              match arg.kind {
                ExprKind::Path(QPath::Resolved(_,p))=>{
                  let bytepos=p.span.lo();
                  let boundary=self.boundary_map.get(&bytepos);
                  if let Some(boundary) = boundary {
                    let expected=boundary.expected;
                    let name = self.hirid_to_var_name(p.segments[0].hir_id);
                    if let Some(name) = name {
                      if let Some(fn_name) = self.hirid_to_var_name(fn_expr.hir_id) {
                        if expected.drop{
                          println!("Move({}->{}())", name, fn_name);
                        }
                        else if expected.write{
                          println!("PassByMutableReference({}->{}())", name, fn_name);
                        }
                        else if expected.read{
                          println!("PassByStaticReference({}->{}())", name, fn_name);
                        }
                        self.access_points.insert(AccessPointUsage::Function(fn_name),false);
                      }
                    }
                  }
                }
                ExprKind::AddrOf(_,mutability,expr)=>{
                  if let Some(fn_name) = self.hirid_to_var_name(fn_expr.hir_id){
                    match expr.kind{
                      ExprKind::Path(QPath::Resolved(_,p))=>{
                        if let Some(name)=self.hirid_to_var_name(p.segments[0].hir_id){
                          match mutability{
                            Mutability::Not=>{
                              println!("PassByStaticReference({}->{}())",name,fn_name);
                            }
                            Mutability::Mut=>{
                              println!("PassByMutableReference({}->{}())",name,fn_name);
                            }
                          }
                          self.access_points.insert(AccessPointUsage::Function(fn_name),false);
                        }
                      }
                      _=>{}
                    }
                  }
                }
                _=>{}
              }
              // self.visit_expr(arg);
            }
          }
          ExprKind::MethodCall(_, rcvr, args, fn_span)
            if !fn_span.from_expansion()
              && rcvr.is_place_expr(|e| !matches!(e.kind, ExprKind::Lit(_))) =>
          {
            
             let hir_id=rcvr.hir_id;
            for a in args.iter() {
              self.visit_expr(a);
            }
          }
          ExprKind::Binary(_, lhs, rhs) => {
            self.visit_expr(lhs);
            self.visit_expr(rhs);
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
            println!();
            println!("Assign Expression: {}", self.tcx.hir().node_to_string(hirid));
            println!("On line: {}", self.expr_to_line(expr));
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
              self.match_rhs(AccessPoint { mutability:*mutability, name: lhs_var }, rhs);
            }
            self.visit_expr(lhs);
            match rhs.kind {
              ExprKind::Path(_) => {},
              _=>{
                self.visit_expr(rhs);
              }
            }
          }
    
          ExprKind::Block(block, _) => {
            self.visit_block(block);
          }
    
          ExprKind::AssignOp(_, lhs, rhs) => {
            self.visit_expr(lhs);
          }

          ExprKind::Unary(UnOp::Deref, inner)
            if inner.is_syntactic_place_expr() && !inner.span.from_expansion() =>
          {
           let hir_id=inner.hir_id;
          }
          
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
            if let Some(boundary)=boundary{
              if boundary.expected.drop {
                let name = self.hirid_to_var_name(hirid);
                if let Some(name) = name {
                  println!();
                  println!("On line: {}", self.expr_to_line(expr));
                  println!("Move({}->none)", name);
                }
              }
            }
          }
          
          _ => {
            intravisit::walk_expr(self, expr);
          }
        }
      }
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
        println!("Statement: {}", self.tcx.hir().node_to_string(local.hir_id));
        println!("on line: {:#?}", self.span_to_line(&local.span));
        match local.pat.kind {
          PatKind::Binding(binding_annotation, ann_hirid, ident, op_pat) => {
            let lhs_var = ident.to_string();
            self.mutability_map.insert(lhs_var.clone(), binding_annotation.1);
            match local.init {
              | Some(expr) => {
                  self.match_rhs(AccessPoint { mutability: binding_annotation.1, name: lhs_var}, expr);
                  match expr.kind {
                    ExprKind::Path(_) => {},
                    _=>{
                      self.visit_expr(expr);
                    }
                  }
              },
              | _ => {},
              };
          }
          PatKind::Path(QPath::Resolved(_,p)) => {
            println!("lhs path: {:?}", self.tcx.def_path_str(p.res.def_id()));
          }
          _ => {
            println!("lhs is not listed");
          }
        }
        
        
        //walk_list!(self, visit_expr, &local.init);
        if let Some(els) = local.els {
        self.visit_block(els);
        }
        walk_list!(self, visit_ty, &local.ty);
      }
      
}
