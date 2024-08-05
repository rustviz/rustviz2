
fn main () {
  let mut v: Vec<i32> = Vec::new();
  for i in v.iter_mut() {
    *i += 1;
  }
}