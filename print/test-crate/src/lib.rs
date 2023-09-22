fn main() {  
  let mut s1 = String::from("hello"); 
  let s2 = String::from(", world"); 
  String::push_str(&mut s1, &s2);  
  let x = s1.push_str(&s2);
  let p = String::from("3.14");
  println!("{}", s1); 
} 