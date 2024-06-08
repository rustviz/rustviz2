fn main () {
    let x = 7;
    let c = 8;
    let mut y = &x;
    y = &c;
    let z = & mut y;
    *z = &x;
    println!("x value {}", **z);
  }