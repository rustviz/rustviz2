fn main() {
  let s1=String::from("Hello");
  let s2={
    let s3=String::from("World");
    s3
  };
  //let s2=return_id(s1);
  println!("{}",s1);
  println!("{}",s2);
}
fn return_id(s:String)->String{
  s
}

