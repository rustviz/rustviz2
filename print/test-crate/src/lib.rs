fn main() {
} 
fn returnone()->usize{
  1
}
fn returnonetup()->(usize,usize){
  (1,1)
}
fn takestring_returnone(mut s0:String,s1:&String,s3:&mut String)->usize{
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
fn foo(s1:&String,s2:String)->(&String,String){
  (s1,s2)
}