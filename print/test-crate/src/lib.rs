fn main() {
  let stra:String = String::from(" world");
  let strc:String = owner() + &stra;
  println!("{}", strc);

}

fn owner() -> String{
   String::from("hello")
}