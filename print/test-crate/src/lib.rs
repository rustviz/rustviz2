fn main() {
  let mut s = String::from("hello");
  let mut s1= String::from("hello");
  let sr = &mut s;
  let s1r= &mut s1;
  foo(s1r);
  let a = {
    let sr1= sr;
    println!("{}",sr1);
    let b = { 
      let s2=String::from("hello");
      let s3=String::from("hello");
      println!("var1{:?}, var2{}",s2,s3);
    };
  }; 
   
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
fn foo(s1:&mut String)->(&mut String){
  s1
}