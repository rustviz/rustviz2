
fn main () {
  let mut x = 9;
  let z = 10;
  let mut c = &x;
  if true {
    c = &z;
    x += 9;
  }

  let s = *c;
}