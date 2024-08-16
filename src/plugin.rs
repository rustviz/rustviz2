use std::collections::{HashMap, BTreeMap};
use simplelog::*;
use std::fs::OpenOptions;
use log::{debug, info, error};
use rustc_hir::intravisit::Visitor;
use rustc_hir::{ItemKind, FnRetTy};
use rustc_middle::ty::TyCtxt;
use rustc_utils::mir::borrowck_facts;
use crate::expr_visitor::{ExprVisitor, RapData};
use crate::RVPluginArgs;
use crate::utils::{RV1Helper, annotate_enum_variant, annotate_toplevel_fn, annotate_struct_field};
// "The main function"
pub fn rv_visitor(tcx: TyCtxt, _args: &RVPluginArgs) {

  // configure the logger
  let log_file = match OpenOptions::new()
    .create(true)
    .write(true)
    .append(true)
    .open("output.log")
  {
    Ok(file) => file,
    Err(e) => {
        eprintln!("Error opening log file: {:?}", e);
        return;
    }
  };
  CombinedLogger::init(
    vec![
        WriteLogger::new(
            LevelFilter::Debug,
            Config::default(),
            log_file,
        )
    ]
  ).expect("Failed to initialize logger");

  // Data structures to be passed to visitor
  let mut testing_helper: RV1Helper = RV1Helper::new();
  let mut line_map: BTreeMap<usize, String> = BTreeMap::new();
  let mut line_map2: BTreeMap<usize, Vec<rustviz_lib::data::ExternalEvent>> = BTreeMap::new();
  match testing_helper.initialize_line_map() {
    Ok(l) => {
      line_map = l;
    }
    Err(e) => {
      error!("{}", e);
    }
  }
  let a_map: BTreeMap<usize, String> = line_map.clone();
  let mut a_line_map: BTreeMap<usize, Vec<String>> = BTreeMap::new();
  let mut owner_to_hash: HashMap<String, usize> = HashMap::new();
  let mut pre_events: Vec<(usize, rustviz_lib::data::ExternalEvent)> = Vec::new();
  let mut rap_map: HashMap<String, RapData> = HashMap::new();
  for k in a_map.keys() {
    let s = a_map[k].clone();
    a_line_map.insert(*k, vec![s]);
    line_map2.insert(*k, vec![]);
  }
  let mut rap_hash_num: usize = 1;
  let mut ids: usize = 0;

  let hir = tcx.hir().clone();
  
  // loop through top-level hir items
  for id in hir.items() {
    match &hir.item(id).kind {
      ItemKind::Struct(vardata, _generics) => {
        match vardata {
          rustc_hir::VariantData::Struct{fields, ..} => {
            // annotate all the struct fields
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
        let output = match fn_sig.decl.output {
          FnRetTy::Return(_ty) => {
            true
          }
          _ => false
        };

        let mut visitor = ExprVisitor { 
          tcx, 
          mir_body: body,
          hir_body: hir_body,
          bwf: bwf, 
          current_scope: 0,
          borrow_map: HashMap::new(),
          raps: &mut rap_map,
          analysis_result: HashMap::new(),
          event_line_map: & mut line_map2,
          preprocessed_events: & mut pre_events,
          rap_hashes: rap_hash_num,
          source_map: & a_map,
          annotated_lines: & mut a_line_map,
          id_map: & mut owner_to_hash,
          unique_id: & mut ids,
          inside_branch: false,
          fn_ret: output
        };
        visitor.visit_body(hir_body);
        visitor.print_lifetimes();
        visitor.print_out_of_scope();
        rap_hash_num = visitor.rap_hashes;
      },
      _ => {}
    }
  }

  // annotate items after visiting all function bodies since we can assign hashes to top-level structures in fn bodies
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

  // Pass information to svg-generator
  match testing_helper.generate_vis(line_map2, pre_events, & mut a_line_map, rap_map.len() + 1) {
    Ok(_) => {}
    Err(e) => {
      eprintln!("{}", e);
    }
  }

}