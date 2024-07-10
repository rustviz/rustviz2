#[derive(Clone, Copy)]
pub enum Fruit {
  Apple(u32),
  Banana,
}


fn main () {
  let mut x = Fruit::Apple(0);
  let mut y = Fruit::Banana;

}