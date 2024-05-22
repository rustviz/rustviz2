use crate::rustc_span::Pos;
use std::collections::{HashMap, BTreeMap};
use rustc_span::source_map::SourceMap;
use aquascope::analysis::{AquascopeAnalysis,
  boundaries::PermissionsBoundary};
use rustc_hir::intravisit::Visitor;
use rustc_hir::{ItemKind};
use rustc_middle::ty::TyCtxt;
use rustc_utils::{
  source_map::range::CharRange,
  mir::borrowck_facts,
};
use crate::expr_visitor::{AccessPointUsage, ExprVisitor};
use crate::PrintAllItemsPluginArgs;
use crate::utils::RV1Helper;


  
//This is a small helper function
fn charrange_to_line(crange:CharRange,source_map:&SourceMap)->usize{
  let file = crange.filename.find_source_file(source_map).unwrap();
  let cpos = crange.end;
  let mut total_bytes = cpos.0 as u32 + file.start_pos.to_u32();
  let mut flag = true;
  while flag {
    flag = false;
    for mbc in file.multibyte_chars.iter().take(cpos.0 as usize) {
      if mbc.pos.0 <= total_bytes {
        total_bytes += mbc.bytes as u32 - 1;
        flag = true;
      }
    }
  }
  let bpos = rustc_span::BytePos(total_bytes as u32);
  let (line,_,_)=file.lookup_file_pos_with_col_display(bpos);
  line
}

// given an identifier returns the name of that item
// expects the form of node_str to be:
// HirId(DefId(0:6 ~ test_crate[f525]::name).0) (<item_str> name)
fn item_name (item_str: &str, node_str: &str) -> String {
  use regex::Regex;
  // pattern starts with identifier, (struct, enum) followed by whitespace
  // followed by any number of characters and terminates with )
  let pattern = format!(r"{}\s+(\w+)\)", item_str);
  let re = Regex::new(&pattern).unwrap();
  if let Some(captures) = re.captures(node_str) {
    if let Some(identifier) = captures.get(1) {
        identifier.as_str().to_owned()
    }
    else {
      println!("error, pattern not matched");
      String::from("")
    }
  }
  else {
    println!("error, pattern not matched");
    String::from("")
  }
}

fn annotate_struct_field (
  line_str: &str,
  hash_map: & mut HashMap<String, usize>,
  a_map: & mut BTreeMap<usize, Vec<String>>,
  hashes: & mut usize,
  field: & rustc_hir::FieldDef,
  m: & TyCtxt
) {
  let name:String = field.ident.as_str().to_owned();
  let hash = *hash_map.entry(name.clone()).or_insert_with(|| {
    let current_hash = *hashes;
    *hashes = (*hashes + 1) % 10;
    current_hash
  });

  let line:usize = m.sess.source_map().lookup_char_pos(field.span.lo()).line;
  let left: usize = m.sess.source_map().lookup_char_pos(field.span.lo()).col_display;
  let right: usize = m.sess.source_map().lookup_char_pos(field.span.hi()).col_display;

  let mut line_contents = line_str.to_string();
  let replace_with = format!("<tspan data-hash=\"{}\">{}</tspan>", hash, name);
  line_contents.replace_range(left..right, &replace_with);
  let v = a_map.get_mut(&line).unwrap();
  if !v.contains(&line_contents) {
    v.push(line_contents);
  }
}

// use regex to help annotate top level fn signature
fn annotate_toplevel_fn (
  func_name: &str, 
  line_str: &str, 
  hash_map: & mut HashMap<String, usize>,
  a_map: & mut BTreeMap<usize, Vec<String>>,
  hashes: & mut usize,
  line: usize)  {
  use regex::Regex;
  let pattern: String = format!(r"fn\s+({})\s*\(", regex::escape(func_name));

  let re = Regex::new(&pattern).unwrap();
  if let Some(caps) = re.captures(line_str) {
    if let Some(m) = caps.get(1) {
      let left: usize = m.start();
      let right: usize = m.end();
      let hash = *hash_map.entry(func_name.to_string()).or_insert_with(|| {
        let current_hash = *hashes;
        *hashes = (*hashes + 1) % 10;
        current_hash
      });

      let mut line_contents = line_str.to_string();
      let replace_with: String = format!("<tspan class=\"fn\" data-hash=\"{}\" hash=\"{}\">{}</tspan>", 0, hash, func_name);
      line_contents.replace_range(left..right, &replace_with);
      let v = a_map.get_mut(&line).unwrap();
      if !v.contains(&line_contents) {
        v.push(line_contents);
      }
    }
  }
  else {
    println!("error, toplevel function pattern didn't match");
  }

}

// "The main function"
pub fn print_all_items(tcx: TyCtxt, _args: &PrintAllItemsPluginArgs) {

  // TESTING HELPER STUFF
  let mut testing_helper: RV1Helper = RV1Helper::new();
  let mut line_map: BTreeMap<usize, String> = BTreeMap::new();
  match testing_helper.initialize_line_map() {
    Ok(l) => {
      line_map = l;
    }
    Err(e) => {
      eprintln!("{}", e);
    }
  }
  let a_map: BTreeMap<usize, String> = line_map.clone();
  let mut a_line_map: BTreeMap<usize, Vec<String>> = BTreeMap::new();
  let mut owner_to_hash: HashMap<String, usize> = HashMap::new();
  for k in a_map.keys() {
    a_line_map.insert(*k, vec![a_map[k].clone()]);
  }
  let mut hash_num: usize = 1;

  // Generate a few things needed for later analysis. They
  // are basically things generated when compiling code.
  let mut access_point_map:HashMap<AccessPointUsage, usize> = HashMap::new();

  let mut declarations: Vec<String> = Vec::new();
  let mut analysis_result: Vec<(u64, String)> = Vec::new();
  let hir = tcx.hir().clone();
  
  for id in hir.items() {
    match &hir.item(id).kind {
      ItemKind::Struct(vardata, generics) => {
        match vardata {
          // A struct variant E.g., Bar { .. } as in enum Foo { Bar { .. } }.
          rustc_hir::VariantData::Struct(fields, _recovered) => {
            if fields.len() > 0 {
              for field in fields.iter(){
                let line = tcx.sess.source_map().lookup_char_pos(field.span.lo()).line;
                let line_str = &a_map[&line];
                annotate_struct_field(line_str, & mut owner_to_hash, & mut a_line_map , & mut hash_num, &field, &tcx);
              }
            }
          }
          _ => {}
        }
      }
      ItemKind::Fn(fn_sig, _generic, body_id) => {
        let func_name:String = hir.name(hir.parent_id(body_id.hir_id)).as_str().to_owned();
        let line = tcx.sess.source_map().lookup_char_pos(fn_sig.span.lo()).line;
        let line_str = &a_map[&line];
        if func_name != "main" {
          annotate_toplevel_fn(&func_name, line_str, & mut owner_to_hash, & mut a_line_map, & mut hash_num, line);
        }
        
        let hir_body = hir.body(*body_id);
        let def_id = tcx.hir().body_owner_def_id(*body_id);
        let bwf = borrowck_facts::get_body_with_borrowck_facts(tcx, def_id);
        let body = &bwf.body;
        let span = tcx.def_span(def_id);
        // Our analysis begins here. Things are printed out.
        // run the AquascopeAnalysis and get the result
        let result=AquascopeAnalysis::run(tcx,*body_id);
        let source_map = tcx.sess.source_map();

        match result {
          Ok(output) => {
            let _body_range = output.body_range;
            let boundaries = output.boundaries;
            let steps = output.steps;
            // Create a hashmap from BytePos to boundaries generated by aquascope
            let mut boundary_map: HashMap<rustc_span::BytePos,PermissionsBoundary> = HashMap::new();

            for boundary in boundaries {
              let bytepos=boundary.byte_location.0 as u32;
              boundary_map.insert(rustc_span::BytePos(bytepos),boundary);
            };

            println!("BOUNDARY MAP : {:#?}", boundary_map);

            let pos = charrange_to_line(_body_range,source_map);
            let mut visitor = ExprVisitor { 
              tcx, 
              mir_body:body, 
              boundary_map,
              access_points: HashMap::new(),
              mutability_map: HashMap::new(),
              lifetime_map: HashMap::new(),
              current_scope: pos,
              borrow_map: HashMap::new(),
              analysis_result: HashMap::new(),
              owners: Vec::new(),
              name_to_access_point: HashMap::new(),
              event_line_map: & mut line_map,
              source_map: & a_map,
              annotated_lines: & mut a_line_map,
              hash_map: & mut owner_to_hash,
              hashes: hash_num
            };
            visitor.visit_body(hir_body);
            visitor.print_out_of_scope();
            visitor.print_lifetimes();
            //declarations.push(visitor.print_definitions());
            declarations.extend(visitor.print_definitions());
            access_point_map.extend(visitor.access_points);
            hash_num = visitor.hashes;
          },
          Err(_) => {}
        }
      },
      _ => {}
    }
  }

  // hir.items()
  // .filter_map(|id| match &hir.item(id).kind {
  //   ItemKind::Fn(fn_sig, _generic, body) => {
  //     let func_name:String = hir.name(hir.parent_id(body.hir_id)).as_str().to_owned();
  //     let line = tcx.sess.source_map().lookup_char_pos(fn_sig.span.lo()).line;
  //     //let line_str = &line_map[&line];
      
  //     Some(body)
  //   },
  //   ItemKind::Struct(vardata, generics) => {
  //     // var data contains 
  //     // println!("Vardata of struct: {:#?}", vardata);
  //     // println!("generics of struct: {:#?}", generics);
  //     // Fields and constructor IDs of enum variants and structs.
  //     match vardata {
  //       // A struct variant E.g., Bar { .. } as in enum Foo { Bar { .. } }.
  //       rustc_hir::VariantData::Struct(fields, _recovered) => {
  //         if fields.len() > 0 {
  //           let parent_id = hir.parent_id(fields[0].hir_id);
  //           let parent_name = item_name("struct", &hir.node_to_string(parent_id));
  //           //println!("parent_name {},", parent_name);
  //           let mut field_vec:Vec<crate::visitor::AccessPoint> = Vec::new();
  //           for field in fields.iter(){
  //             match extract_var_name(&hir.node_to_string(field.hir_id)){
  //               Some(field_name) => {
  //                 field_vec.push(AccessPoint{mutability: Mutability::Not, name: field_name, members: None});
  //               }
  //               None => {}
  //             }
  //           }
  //           //let struct_point: AccessPointUsage = AccessPointUsage::Struct(field_vec);
  //           //access_point_map.insert(struct_point, 0);
  //         }
  //       }
  //       // A tuple variant E.g., Bar(..) as in enum Foo { Bar(..) }.
  //       rustc_hir::VariantData::Tuple(fields, _hir_id, _def_id) => {
        
  //       }
  //       // A unit variant E.g., Bar = .. as in enum Foo { Bar = .. }.
  //       rustc_hir::VariantData::Unit(_hir_id, _def_id) => {
  //         // we may not have to do anything extra for these variants, will see later
  //       }
  //     }
  //     None
  //   },
  //   ItemKind::Enum(enum_def, _generics) => {
  //     let parent_id = hir.parent_id(enum_def.variants[0].hir_id);
  //     let parent_name = item_name("enum", &hir.node_to_string(parent_id));
  //     println!("parent name, {}", parent_name);
  //     for variant in enum_def.variants.iter(){
  //       println!("Ident: {:#?}", variant.ident);
  //     }
  //     None
  //   },
  //   _ => {
  //     None
  //   },
  // })
  // .for_each(|body_id| {
  // let hir_body = hir.body(*body_id);
  // let def_id = tcx.hir().body_owner_def_id(*body_id);
  // let bwf = borrowck_facts::get_body_with_borrowck_facts(tcx, def_id);
  // let body = &bwf.body;
  // let span = tcx.def_span(def_id);
  // // Our analysis begins here. Things are printed out.
  // // run the AquascopeAnalysis and get the result
  // let result=AquascopeAnalysis::run(tcx,*body_id);
  // let source_map = tcx.sess.source_map();
  // match result {
  //   Ok(output) =>{
  //    let _body_range = output.body_range;
  //    let boundaries = output.boundaries;
  //    let steps = output.steps;
  //    // Create a hashmap from BytePos to boundaries generated by aquascope
  //    let mut boundary_map: HashMap<rustc_span::BytePos,PermissionsBoundary> = HashMap::new();

  //    for boundary in boundaries {
  //     let bytepos=boundary.byte_location.0 as u32;
  //     boundary_map.insert(rustc_span::BytePos(bytepos),boundary);
  //   };
  //   /*
  //    // This is to print out the permission tables in aquascope (which we currently don't actually 
  //    // use in the analysis but might be useful). If you want to glance at what's in the table, 
  //    // uncomment the println!().
  //    for step in steps {
  //     let location = step.location;
  //     let _line = charrange_to_line(location,source_map);
  //     //println!("Step on line {} from {:?} to {:?}.",line,location.start,location.end);
  //     let state = step.state;
  //     for table in state {
  //       let _state = table.state;
  //       let _from = table.from;
  //       let _to = table.to;
  //       //println!("Table from {:?}-{:?} to {:?}-{:?}.",
  //       //from.start,from.end,to.start,to.end);
  //       //for (x,diff) in state{
  //       // println!("{} with permission diff {:?} ",
  //       //  x,diff);
  //       //};
  //     };
  //    };
  //    */
     
  //    // The visitor will walk through the hir and analysis it, see visitor.rs.
  //    // use last line of the body as the current scope
  //   //  println!("crange: {:#?}", _body_range);
  //    let pos = charrange_to_line(_body_range,source_map);
  //    let mut visitor = ExprVisitor { 
  //     tcx, 
  //     mir_body:body, 
  //     boundary_map,
  //     access_points: HashMap::new(),
  //     mutability_map: HashMap::new(),
  //     lifetime_map: HashMap::new(),
  //     current_scope: pos,
  //     borrow_map: HashMap::new(),
  //     analysis_result: HashMap::new(),
  //     owners: Vec::new(),
  //     event_line_map: & mut line_map,
  //     source_map: & a_map,
  //     annotated_lines: & mut a_line_map,
  //     hash_map: HashMap::new(),
  //     hashes: hash_num
  //   };
  //   visitor.visit_body(hir_body);
  //   visitor.print_out_of_scope();
  //   visitor.print_lifetimes();
  //   //declarations.push(visitor.print_definitions());
  //   declarations.extend(visitor.print_definitions());
  //   access_point_map.extend(visitor.access_points);
  //   hash_num = visitor.hashes;
  //   //push_str(&visitor.print_definitions());
  //   let ana_result = visitor.analysis_result.clone();
  //   for (line, elem) in ana_result{
  //     let newline = line as u64;
  //     let elem = elem;
  //      //println!(" [used for debug] {}:{:?}",newline,elem);
  //     for el in elem{
  //       analysis_result.push((newline,el));
  //     }
  //   }
  //   // order the elemens in analysis_result
  //   analysis_result.sort_by(|a,b|a.1.cmp(&b.1));
  //   for (line, elem) in analysis_result.clone(){
  //     let newline = line as u64;
  //     let elem = elem;
  //     println!(" [used for debug] {}:{:?}",newline,elem);
  //   }
    
  // }
  //   _ =>{println!("Analysis Error.");}
  // }});
  // Print out the variable definitions.
  //declarations.sort();
  // println!("/* --- BEGIN Variable Definitions ---");
  // for declaration in &declarations {
  //   println!("{}", declaration);
  // }
  // println!("--- END Variable Definitions --- */");

  // println!("ANNOTATED LINE MAP");
  // println!("{:#?}", a_line_map);

  // TESTING HELPER STUFF
  match testing_helper.generate_vis(&mut line_map, &declarations, &a_line_map) {
    Ok(_) => {}
    Err(e) => {
      eprintln!("{}", e);
    }
  }

}