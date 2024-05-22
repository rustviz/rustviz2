fn main() {
    let mut x = 5;
    let y = & mut x;
    *y = 5;
    println!("some junk value {}", x)
}