
fn main () {
  let mut v: Vec<i32> = Vec::new();
  v.push(8);
  let z: &i32 = v.get(0).unwrap();
}