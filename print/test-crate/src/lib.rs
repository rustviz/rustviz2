fn main() {
  let mut s =String::from("hello");
  let s1=s;
  let mut a=returnone();
  let b=a; 
  let c = returnstring();
  let d=c;
} 
fn returnone()->usize{
  1
}
   
fn returnstring()->String{
  let s=String::from("hello");
  s
}