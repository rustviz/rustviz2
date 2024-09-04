fn main () {
  let mut x = 7;
  let mut y = & mut x;
  let mut c = 8;
  y = &mut c; // fix this dot
  x += 5;
  *y += 5;
  println!("x value {}", *y);
}