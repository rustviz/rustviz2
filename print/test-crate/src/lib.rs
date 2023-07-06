fn main() {
  let mut s =String::from("hello");
  let s0 =String::from("hello");
  let s2 =String::from("hello");
  let mut s3 =String::from("hello");
  let s1;
  s1=s;
  let s1r=&s1;
  let s3r=&mut s3;
  //let mut a=returnone();
  let a=takestring_returnone(s0,s1r,s2,s3r);
  let b;
  b=a; 
  let c;
  c = returnstring();
  let d=c;
} 
fn returnone()->usize{
  1
}
fn takestring_returnone(s0:String,s1:&String,s2:String,s3:&mut String)->usize{
  1
}
fn returnstring()->String{
  let s=String::from("hello");
  s
}