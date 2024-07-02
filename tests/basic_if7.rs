fn main () {
    let mut y = 7;
    let c = 5;
    let mut x = &c;
    if true {
        
        x = &y;
    }


    println!(" x {}", x);
}