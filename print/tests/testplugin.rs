use print_all_items::print;

#[test]
fn test_my_plugin() {
    let _input_code = r#"
        // Your test input code here
        fn main() {
            let s1=String::from("Hello");
            let s0={
              let s3=String::from("World");
              s3
            };
            let s2 = String::from("World");
            //let s2=return_id(s1);
            println!("{}",s1);
            println!("{}",s2);
          }
          fn return_id(s:String)->String{
            s
          }
          
          
    "#;

    let _expected_output = r#"
    /* --- BEGIN Variable Definitions ---
    Function println();
    Owner Not s0;
    Function String::from();
    Owner Not s2;
    Owner Not s3;
    Owner Not s1;
    --- END Variable Definitions --- */
    
    !{Move(String::from()->s1)}
    !{Move(String::from()->s3)}
    !{Move(s3->s0)}
    !{Move(String::from()->s2)}
    !{PassByStaticReference(s1->println())}
    !{PassByStaticReference(s2->println())}
    !{GoOutOfScope(s0)}
    !{GoOutOfScope(s2)}
    !{GoOutOfScope(s3)}
    !{GoOutOfScope(s1)}
    varmap:{"String::from()": Function(Function { name: "String::from()", hash: 3 }), "println()": Function(Function { name: "println()", hash: 1 }), "s1": Owner(Owner { name: "s1", hash: 2, is_mut: false })},contents:Lines { buf: BufReader { reader: File { fd: 9, path: "/home/hyy/code_pl/rustviz/print/output", read: true, write: false }, buffer: 89/8192 } },line_num:5
    line 1: Ok("")
    line 2: Ok("!{Move(String::from()->s1)}")
    line 3: Ok("!{PassByStaticReference(s1->println())}")
    line 4: Ok("!{GoOutOfScope(s1)}")
    print_event:[(2, "Move(String::from()->s1)"), (3, "PassByStaticReference(s1->println())"), (4, "GoOutOfScope(s1)")]
    vd:VisualizationData { timelines: {}, external_events: [], preprocess_external_events: [(2, Move { from: Some(Function(Function { name: "String::from()", hash: 3 })), to: Some(Owner(Owner { name: "s1", hash: 2, is_mut: false })) }), (3, PassByStaticReference { from: Some(Owner(Owner { name: "s1", hash: 2, is_mut: false })), to: Some(Function(Function { name: "println()", hash: 1 })) }), (4, GoOutOfScope { ro: Owner(Owner { name: "s1", hash: 2, is_mut: false }) })], event_line_map: {} }
    successfully wrote to src/vis_code.svg
    successfully wrote to src/vis_timeline.svg
    /* --- BEGIN Variable Definitions ---
    Owner Not s;
    --- END Variable Definitions --- */
    
    !{InitOwnerParam(s)}
    !{Move(s->None)}
    varmap:{"String::from()": Function(Function { name: "String::from()", hash: 3 }), "s1": Owner(Owner { name: "s1", hash: 2, is_mut: false }), "println()": Function(Function { name: "println()", hash: 1 })},contents:Lines { buf: BufReader { reader: File { fd: 9, path: "/home/hyy/code_pl/rustviz/print/output", read: true, write: false }, buffer: 89/8192 } },line_num:5
    line 1: Ok("")
    line 2: Ok("!{Move(String::from()->s1)}")
    line 3: Ok("!{PassByStaticReference(s1->println())}")
    line 4: Ok("!{GoOutOfScope(s1)}")
    print_event:[(2, "Move(String::from()->s1)"), (3, "PassByStaticReference(s1->println())"), (4, "GoOutOfScope(s1)")]
    vd:VisualizationData { timelines: {}, external_events: [], preprocess_external_events: [(2, Move { from: Some(Function(Function { name: "String::from()", hash: 3 })), to: Some(Owner(Owner { name: "s1", hash: 2, is_mut: false })) }), (3, PassByStaticReference { from: Some(Owner(Owner { name: "s1", hash: 2, is_mut: false })), to: Some(Function(Function { name: "println()", hash: 1 })) }), (4, GoOutOfScope { ro: Owner(Owner { name: "s1", hash: 2, is_mut: false }) })], event_line_map: {} }
    successfully wrote to src/vis_code.svg
    successfully wrote to src/vis_timeline.svg
    
    "#;

    // Call your plugin with the input code and capture the output
    let actual_output = print::run(input_code);

    // Use an assertion macro to compare the actual output with the expected output
    // assert_eq!(actual_output, expected_output);
}