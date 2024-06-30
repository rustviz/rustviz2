
use rustc_hir::Mutability;
use rustviz_lib::data::{ResourceAccessPoint, ResourceTy};
use crate::expr_visitor::*;
use rustc_hir::*;
use std::collections::HashSet;


// pub fn fetch_rap <'tcx>(expr: &'tcx Expr, raps: & HashMap<String, (ResourceAccessPoint, usize)>) -> Option<ResourceAccessPoint> {
//   match expr.kind {
//     ExprKind::Call(..) | ExprKind::Binary(..) | ExprKind::Lit(_) | ExprKind::MethodCall(..) => None,
//     ExprKind::Path(QPath::Resolved(_,p)) => {
//       let name = self.tcx.hir().name(p.segments[0].hir_id).as_str().to_owned();
//       Some(self.raps.get(&name).unwrap().0.to_owned())
//     }
//     ExprKind::AddrOf(_, _, expr) | ExprKind::Unary(_, expr) => self.fetch_rap(expr),
//     ExprKind::Block(b, _) => {
//       match b.expr {
//         Some(expr) => { self.fetch_rap(expr) }
//         None => { panic!("invalid expr for fetching rap") }
//       }
//     }
//     ExprKind::Field(expr, ident) => {
//       match expr {
//         Expr{kind: ExprKind::Path(QPath::Resolved(_,p)), ..} => {
//           let name = self.tcx.hir().name(p.segments[0].hir_id).as_str().to_owned();
//           let field_name: String = ident.as_str().to_owned();
//           let total_name = format!("{}.{}", name, field_name);
//           Some(self.raps.get(&total_name).unwrap().0.to_owned())
//         }
//         _ => { panic!("unexpected field expr") }
//       } 
//     }
//     _ => None
//   }
// }

// pub fn live_of_expr <'tcx>(expr: &'tcx Expr) -> HashSet<ResourceTy> {

// }

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
pub fn match_op(op: rustc_hir::BinOpKind) -> String {
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

pub fn bool_of_mut (m: Mutability) -> bool {
  match m {
    Mutability::Not => {
      false
    }
    _ => { true }
  }
}