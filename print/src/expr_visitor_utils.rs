use crate::expr_visitor::{ExprVisitor, AccessPoint, AccessPointUsage, Reference};
use rustc_hir::Mutability;

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
  
pub fn string_of_access_point(a: &AccessPointUsage) -> String {
    match a {
        AccessPointUsage::Function(f) => { f.to_string() },
        AccessPointUsage::MutRef(b) | AccessPointUsage::Owner(b) | 
        AccessPointUsage::StaticRef(b) => { b.name.clone() },
        AccessPointUsage::Struct(b, _) => { b.name.clone() }
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