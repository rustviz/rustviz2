fn main() {
  let z = returnonetup();
  let ss = returnstringtup();
  let mut s =String::from("hello");
  let s0 =String::from("hello");
  let s2 =String::from("hello");
  let mut s3 =String::from("hello");
  let s4 =String::from("hello");
  println!("{}",s);
  let s1; 
  s1=s;
  let s1r=&s1;
  let s3rr=&mut s3;
  let s3r=foo(&s4,s0);
  //let mut a=returnone();
  let a=1;
  let b;
  b=a; 
  let c;
  c = returnstring();
  let d=c;
} 
fn returnone()->usize{
  1
}
fn returnonetup()->(usize,usize){
  (1,1)
}
fn takestring_returnone(s0:String,s1:&String,s2:String,s3:&mut String)->usize{
  1
}
fn returnstring()->String{
  let s=String::from("hello");
  s
}
fn returnstringtup()->(String,String){
  let s1=String::from("hello");
  let s2=String::from("hello");
  (s1,s2)
}
fn foo(s1: & String,s2:String)->(& String,String){
  (s1,s2)
}