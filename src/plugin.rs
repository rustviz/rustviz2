use crate::rustc_span::Pos;
use std::collections::{HashMap, BTreeMap, HashSet};
use rustc_span::source_map::SourceMap;
// use aquascope::analysis::{AquascopeAnalysis,
//   boundaries::PermissionsBoundary};
use rustc_hir::intravisit::Visitor;
use rustc_hir::{ItemKind};
use rustc_middle::ty::TyCtxt;
use rustc_utils::{
  source_map::range::CharRange,
  mir::borrowck_facts,
};
use rustviz_lib::data::ResourceAccessPoint;
use crate::expr_visitor::ExprVisitor;
use crate::RVPluginArgs;
use crate::utils::RV1Helper;

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

  let line: usize = m.sess.source_map().lookup_char_pos(field.span.lo()).line;
  let left: usize = m.sess.source_map().lookup_char_pos(field.span.lo()).col_display;
  let right: usize = m.sess.source_map().lookup_char_pos(field.span.hi()).col_display;

  let mut line_contents = line_str.to_string();
  let replace_with = format!("[_tspan data-hash=\"{}\"_]{}[_/tspan_]", hash, name);
  line_contents.replace_range(left..right, &replace_with);
  let v = a_map.get_mut(&line).unwrap();
  if !v.contains(&line_contents) {
    v.push(line_contents);
  }
}

// use regex to help annotate top level fn signature
fn annotate_toplevel_fn (
  func_ident: rustc_span::symbol::Ident, 
  line_str: &str, 
  raps: & HashMap<String, (ResourceAccessPoint, usize)>,
  a_map: & mut BTreeMap<usize, Vec<String>>,
  hashes: & mut usize,
  m: &TyCtxt)  {
  let func_name = func_ident.as_str().to_owned();
  
  let line: usize = m.sess.source_map().lookup_char_pos(func_ident.span.lo()).line;
  let left: usize = m.sess.source_map().lookup_char_pos(func_ident.span.lo()).col_display;
  let right: usize = m.sess.source_map().lookup_char_pos(func_ident.span.hi()).col_display;
  let hash = match raps.get(&func_name) {
    Some(r) => { *r.0.hash() }
    None => {
      let current_hash = *hashes;
      *hashes = (*hashes + 1) % 10;
      current_hash as u64
    }
  };


  let mut line_contents = line_str.to_string();
  let replace_with: String = format!("[_tspan class=\"fn\" data-hash=\"{}\" hash=\"{}\"_]{}[_/tspan_]", 0, hash, func_name);
  line_contents.replace_range(left..right, &replace_with);
  let v = a_map.get_mut(&line).unwrap();
  if !v.contains(&line_contents) {
    v.push(line_contents);
  }
}

fn annotate_enum_variant(
  ctor_name: &str, 
  parent_name: &str,
  variant: & rustc_hir::Variant,
  rap_map: & HashMap<String, (ResourceAccessPoint, usize)>,
  a_map: & mut BTreeMap<usize, Vec<String>>,
  m: & TyCtxt
) {
  let rap_name = format!("{}::{}", parent_name, ctor_name);
  if rap_map.contains_key(&rap_name) {
    let span = variant.ident.span;
    let hash = rap_map.get(&rap_name).unwrap().0.hash();
    let line: usize = m.sess.source_map().lookup_char_pos(span.lo()).line;
    let left: usize = m.sess.source_map().lookup_char_pos(span.lo()).col_display;
    let line_str = &a_map[&line][0];
    let right = m.sess.source_map().lookup_char_pos(span.hi()).col_display;

    let mut line_contents = line_str.to_string();
    let replace_with = format!("[_tspan class=\"fn\" data-hash=\"{}\" hash=\"{}\"_]{}[_/tspan_]", 0, hash, ctor_name);
    line_contents.replace_range(left..right, &replace_with);
    let v = a_map.get_mut(&line).unwrap();
    if !v.contains(&line_contents) {
      v.push(line_contents);
    }
  }
}

// "The main function"
pub fn rv_visitor(tcx: TyCtxt, _args: &RVPluginArgs) {

  // TESTING HELPER STUFF
  let mut testing_helper: RV1Helper = RV1Helper::new();
  let mut line_map: BTreeMap<usize, String> = BTreeMap::new();
  let mut line_map2: BTreeMap<usize, Vec<rustviz_lib::data::ExternalEvent>> = BTreeMap::new();
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
  let mut pre_events: Vec<(usize, rustviz_lib::data::ExternalEvent)> = Vec::new();
  let mut rap_map: HashMap<String, (rustviz_lib::data::ResourceAccessPoint, usize)> = HashMap::new();
  for k in a_map.keys() {
    let s = a_map[k].clone();
    a_line_map.insert(*k, vec![s]);
    line_map2.insert(*k, vec![]);
  }
  let mut rap_hash_num: usize = 1;

  // Generate a few things needed for later analysis. They
  // are basically things generated when compiling code.

  //let analysis_result: Vec<(u64, String)> = Vec::new();
  let hir = tcx.hir().clone();
  
  for id in hir.items() {
    match &hir.item(id).kind {
      ItemKind::Struct(vardata, generics) => {
        match vardata {
          // A struct variant E.g., Bar { .. } as in enum Foo { Bar { .. } }.
          rustc_hir::VariantData::Struct{fields: fields, ..} => {
            if fields.len() > 0 {
              for field in fields.iter(){
                let line = tcx.sess.source_map().lookup_char_pos(field.span.lo()).line;
                let line_str = &a_map[&line];
                annotate_struct_field(line_str, & mut owner_to_hash, & mut a_line_map , & mut rap_hash_num, &field, &tcx);
              }
            }
          }
          _ => {}
        }
      }
      ItemKind::Fn(fn_sig, _generic, body_id) => {        
        let hir_body = hir.body(*body_id);
        let def_id = tcx.hir().body_owner_def_id(*body_id);
        let bwf = borrowck_facts::get_body_with_borrowck_facts(tcx, def_id);
        let body = &bwf.body;
        // Our analysis begins here. Things are printed out.
        // run the AquascopeAnalysis and get the result
        //let result=AquascopeAnalysis::run(tcx,*body_id);
        let source_map = tcx.sess.source_map();

        let mut visitor = ExprVisitor { 
          tcx, 
          mir_body:body, 
          //boundary_map,
          current_scope: 0,
          borrow_map: HashMap::new(), // TODO turn this to mut ref
          lenders: HashMap::new(),
          raps: &mut rap_map,
          analysis_result: HashMap::new(),
          event_line_map: & mut line_map2,
          preprocessed_events: & mut pre_events,
          rap_hashes: rap_hash_num,
          source_map: & a_map,
          annotated_lines: & mut a_line_map,
          id_map: & mut owner_to_hash,
        };
        visitor.visit_body(hir_body);
        visitor.print_lifetimes();
        visitor.print_out_of_scope();
        rap_hash_num = visitor.rap_hashes;
      },
      _ => {}
    }
  }

  for id in hir.items() {
    match &hir.item(id).kind {
      ItemKind::Fn(fn_sig, _generic, _body_id) => {
        let fn_ident = tcx.hir_owner_node(id.hir_id().owner).ident().unwrap();
        let line = tcx.sess.source_map().lookup_char_pos(fn_sig.span.lo()).line;
        let line_str = &a_map[&line];
        if fn_ident.as_str() != "main" {
          annotate_toplevel_fn(fn_ident, line_str, &rap_map, & mut a_line_map, & mut rap_hash_num, &tcx);
        }
      }
      ItemKind::Enum(e_def, _generics) => {
        for variant in e_def.variants.iter() {
          let ctor_name = hir.name(variant.data.ctor_hir_id().unwrap()).as_str().to_owned();
          let parent_name = hir.name(tcx.parent_hir_id(variant.hir_id)).as_str().to_owned();
          annotate_enum_variant(ctor_name.as_str(), parent_name.as_str(), &variant, &rap_map, & mut a_line_map, &tcx);
        }
      }
      _ => {}
    }
  }

  pre_events.sort_by_key(|k| k.0);
  // println!("EVENTS: {:#?}", pre_events);
  // println!("LINE MAP {:#?}", line_map2);

  // TESTING HELPER STUFF
  match testing_helper.generate_vis(line_map2, pre_events, & mut a_line_map, rap_map.len() + 1) {
    Ok(_) => {}
    Err(e) => {
      eprintln!("{}", e);
    }
  }

}