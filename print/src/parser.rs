// Some headers
use rustc_middle::{
  mir::{Body},
  ty::{TyCtxt},
};
use rustc_hir::{StmtKind, Stmt, Local, Expr, ExprKind, UnOp, QPath, Path, def::Res, PatKind};
use std::{collections::HashMap};
use rustc_ast::walk_list;
use aquascope::analysis::{
  boundaries::PermissionsBoundary};
use rustc_hir::intravisit::{self, Visitor};

// A small helper function
fn extract_var_name(input_string: &str ) -> Option<String> {
  let start_index = input_string.find('`')? + 1;
    let end_index = input_string.rfind('`')?;
    Some(input_string[start_index..end_index].to_owned())
}

// Implement the visitor 
pub struct ExprVisitor<'a, 'tcx:'a> {
  pub tcx: TyCtxt<'tcx>,
  pub mir_body: &'a Body<'tcx>,
  pub boundary_map: HashMap<rustc_span::BytePos,PermissionsBoundary>,
}

impl<'a, 'tcx> ExprVisitor<'a, 'tcx>{
}

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
            let line_number = self.tcx.sess.source_map().lookup_char_pos(expr.span.lo()).line;
            println!("On line: {}", line_number);
            let mut lhs_var = "".to_string();
            match lhs.kind {
              ExprKind::Path(QPath::Resolved(_,p)) => {
                let long_name = self.tcx.hir().node_to_string(p.segments[0].hir_id);
                let name = extract_var_name(&long_name);
                if let Some(name) = name {
                  lhs_var = name;
                }
              },
              _=>{}
            }
            match rhs.kind {
              ExprKind::Path(QPath::Resolved(_,p)) => {
                let bytepos=p.span.lo();
                  let boundary=self.boundary_map.get(&bytepos);
                  //println!("boundary {:?}",boundary);
                  if let Some(boundary) = boundary {
                    if boundary.expected.drop {
                      let long_name = self.tcx.hir().node_to_string(p.segments[0].hir_id);
                      let name = extract_var_name(&long_name);
                      if let Some(name) = name {
                        if lhs_var !="" {
                          println!("Move({}->{})", name, lhs_var);
                        }
                      }
                    } else {
                      let long_name = self.tcx.hir().node_to_string(p.segments[0].hir_id);
                      let name = extract_var_name(&long_name);
                      if let Some(name) = name {
                        if lhs_var !="" {
                          println!("Copy({}->{})", name, lhs_var);
                        }
                      }
                    }
                  }
              },
              ExprKind::Call(fn_expr, args) => {
                let fn_hir_id = fn_expr.hir_id;
                let fn_long_name = self.tcx.hir().node_to_string(fn_hir_id);
                let fn_name = extract_var_name(&fn_long_name);
                if let Some(fn_name) = fn_name {
                  if (!fn_name.contains("crate::io") && !fn_name.contains("String::from")) { //neglect println
                    if lhs_var !="" {
                      println!("Move({}()->{})", fn_name, lhs_var);
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
              },
              ExprKind::Lit(_) => {
                let long_name = self.tcx.hir().node_to_string(hirid);
                let name = extract_var_name(&long_name);
                if let Some(name) = name {
                  if lhs_var !="" {
                    println!("Bind({})", lhs_var);
                  }
                }
              }
              _=>{}
            }
            
            self.visit_expr(lhs);
            self.visit_expr(rhs);
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
        let line_number= self.tcx.sess.source_map().lookup_char_pos(local.span.lo());
        println!("on line: {:#?}", line_number.line);
        let mut lhs_var : String= "".to_string();
        match local.pat.kind {
          PatKind::Binding(binding_annotation, ann_hirid, ident, op_pat) => {
            lhs_var = ident.to_string();
            
          }
          PatKind::Path(QPath::Resolved(_,p)) => {
            println!("lhs path: {:?}", self.tcx.def_path_str(p.res.def_id()));
          }
          _ => {
            println!("lhs is not listed");
          }
        }
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
                  let fn_hir_id = fn_expr.hir_id;
                  let fn_long_name = self.tcx.hir().node_to_string(fn_hir_id);
                  let fn_name = extract_var_name(&fn_long_name);
                  if let Some(fn_name) = fn_name {
                    if (!fn_name.contains("crate::io") && !fn_name.contains("String::from")) { //neglect println
                      if lhs_var !="" {
                        println!("Move({}()->{})", fn_name, lhs_var);
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
                  let bytepos=p.span.lo();
                  let boundary=self.boundary_map.get(&bytepos);
                  if let Some(boundary) = boundary {
                    if boundary.expected.drop {
                      let long_name = self.tcx.hir().node_to_string(p.segments[0].hir_id);
                      let name = extract_var_name(&long_name);
                      if let Some(name) = name {
                        if lhs_var !="" {
                          println!("Move({}->{})", name, lhs_var);
                        }
                      }
                    } else {
                      let long_name = self.tcx.hir().node_to_string(p.segments[0].hir_id);
                      let name = extract_var_name(&long_name);
                      if let Some(name) = name {
                        if lhs_var !="" {
                          println!("Copy({}->{})", name, lhs_var);
                        }
                      }
                    }
                  }
                  
                }
                _ => {
                  println!("rhs is not implemented");
                }
              }
          },
          | _ => {},
          };
        
        
        walk_list!(self, visit_expr, &local.init);
        if let Some(els) = local.els {
        self.visit_block(els);
        }
        walk_list!(self, visit_ty, &local.ty);
      }
      
}

