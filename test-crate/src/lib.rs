pub enum Fruit {
  Apple(String),
  Banana
}

fn main () {
  let mut x = Fruit::Apple(String::from("x"));
  let mut y = Fruit::Banana;
  match x {
    Fruit::Apple(z) => { 
      println!("x {}", z)
    }
    _ => {}
  };

  match x {
    c => {
      y = c;
    }
  };
}