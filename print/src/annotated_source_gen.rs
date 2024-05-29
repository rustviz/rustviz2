use crate::expr_visitor::ExprVisitor;

use rustc_middle::{
    mir::Body,
    ty::{TyCtxt,Ty},
  };
use rustc_hir::{Expr, ExprKind, QPath, Stmt, StmtKind, Local, Path, Mutability};
use rustviz_lib::data::ResourceAccessPoint;
use std::collections::{HashMap, BTreeMap};
use rustc_span::Span;




impl <'a, 'tcx> ExprVisitor<'a, 'tcx> {

pub fn annotate_src(&mut self, name: String, s: Span, is_func: bool, hash: u64) {
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

pub fn annotate_expr(& mut self, expr: &'tcx Expr) {
  match expr.kind {
    ExprKind::Path(QPath::Resolved(_, p)) => {
      let name: String = self.tcx.hir().name(p.segments[0].hir_id).as_str().to_owned();
      self.annotate_src(name.clone(), p.span, false, *self.raps.get(&name).unwrap().0.hash());
    }
    ExprKind::Call(fn_expr, fn_args) => {
      match fn_expr.kind {
        ExprKind::Path(QPath::Resolved(_,rustc_hir::Path{res: rustc_hir::def::Res::Def(_, id), ..})) 
          if !id.is_local() => {
            match fn_args {
              [Expr{kind: ExprKind::Call(_, a),..}] => {
                match a {
                  [_, Expr{kind: ExprKind::AddrOf(_, _, 
                    Expr{kind: ExprKind::Array(x),..}),..}] => {
                      for exp in x.iter() {
                        match exp {
                          Expr{kind: ExprKind::Call(_, format_args), ..} => {
                            for arg in format_args.iter() {
                              self.annotate_expr(arg);
                            }
                          }
                          _ => {}
                        }
                      }
                    }
                  _ => {}
                }
              }
              _ => {
                let fn_name = self.hirid_to_var_name(fn_expr.hir_id).unwrap();
                self.annotate_src(fn_name.clone(), fn_expr.span, true, *self.raps.get(&fn_name).unwrap().0.hash());
                for a in fn_args.iter() {
                  self.annotate_expr(a);
                }
              }
            }
          }
          _ => {
            let fn_name = self.hirid_to_var_name(fn_expr.hir_id).unwrap();
            self.annotate_src(fn_name.clone(), fn_expr.span, true, *self.raps.get(&fn_name).unwrap().0.hash());
            for a in fn_args.iter() {
              self.annotate_expr(a);
            }
          }
      }
    }
    ExprKind::Unary(_, ex) | ExprKind::AddrOf(_, _, ex) 
    | ExprKind::Ret(Some(ex)) => {
      self.annotate_expr(ex);
    }
    ExprKind::Binary(_, exp1, exp2) => {
      self.annotate_expr(exp1);
      self.annotate_expr(exp2);
    }
    ExprKind::MethodCall(name_and_generic_args, rcvr, args, _) => {
      let rcvr_name = self.hirid_to_var_name(rcvr.hir_id).unwrap();
      let fn_name = self.hirid_to_var_name(name_and_generic_args.hir_id).unwrap();
      self.annotate_src(rcvr_name.clone(), rcvr.span, false, *self.raps.get(&rcvr_name).unwrap().0.hash());
      self.annotate_src(fn_name.clone(), name_and_generic_args.ident.span, true, *self.raps.get(&fn_name).unwrap().0.hash());
      for arg in args.iter() {
        self.annotate_expr(arg);
      }
    }
    ExprKind::Assign(exp1, exp2, _) | ExprKind::AssignOp(_, exp1, exp2) => {
      self.annotate_expr(exp1);
      self.annotate_expr(exp2);
    }
    ExprKind::Block(block, _) => {
      for stmt in block.stmts.iter() {
        self.annotate_stmt(stmt);
      }
      match block.expr {
        Some(ex) => {
          self.annotate_expr(ex);
        }
        _ => {}
      }
    }
    ExprKind::Struct(_, fields, _) => {
      for field in fields.iter() {
        self.annotate_src(field.ident.to_string(), field.ident.span, false, *self.id_map.get(field.ident.as_str()).unwrap() as u64);
        self.annotate_expr(field.expr);
      }
    }
    ExprKind::Field(exp, ident) => {
      self.annotate_src(ident.to_string(), ident.span, false, *self.id_map.get(ident.as_str()).unwrap() as u64);
      self.annotate_expr(exp);
    }
    _ => {}
  }
}

pub fn annotate_local(&mut self, loc: &'tcx Local<'tcx>) {
  match loc.pat.kind {
    rustc_hir::PatKind::Binding(_, _, ident, _) => {
      let lhs_var:String = ident.to_string();
      self.annotate_src(lhs_var.clone(), ident.span, false, *self.raps.get(&lhs_var).unwrap().0.hash());
      match loc.init {
        Some(exp) => {
          self.annotate_expr(exp);
        }
        _ => {}
      }
    }
    _ => {}
  }
}

pub fn annotate_stmt(&mut self, stmt: &'tcx Stmt<'tcx>) {
  match stmt.kind {
    StmtKind::Local(ref local) => {
      self.annotate_local(local);
    }
    StmtKind::Item(_) => {}
    StmtKind::Expr(exp) | StmtKind::Semi(exp) => {
      self.annotate_expr(exp);
    }
  }
}
}