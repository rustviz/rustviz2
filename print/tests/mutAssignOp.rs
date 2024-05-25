fn main() {
    let mut x = 5;
    x += 5;

    let mut y = String::from("huh");
    y = String::from("huh2");

    let z = & mut x;
    z = y;

}