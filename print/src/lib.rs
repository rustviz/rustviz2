// This file is based on the rust plugin example.
// After installed with cargo install, call "cargo print-all-items" in the 
// test file directory. It will compile the test file and run the print_all_items() function.
// Look at print_all_items() function.
#![feature(
  rustc_private,
  box_patterns,
  associated_type_defaults,
  min_specialization,
  type_alias_impl_trait,
  trait_alias,
  let_chains,
  unboxed_closures,
  exact_size_is_empty,
  hash_drain_filter,
  drain_filter,
  type_changing_struct_update
)]
#![deny(
  clippy::all,
  clippy::bool_to_int_with_if,
  clippy::case_sensitive_file_extension_comparisons,
  clippy::cloned_instead_of_copied,
  clippy::default_trait_access,
  clippy::empty_enum,
  clippy::enum_glob_use,
  clippy::expl_impl_clone_on_copy,
  clippy::explicit_deref_methods,
  clippy::filter_map_next,
  clippy::flat_map_option,
  clippy::float_cmp,
  clippy::fn_params_excessive_bools,
  clippy::from_iter_instead_of_collect,
  clippy::if_not_else,
  clippy::implicit_clone,
  clippy::inconsistent_struct_constructor,
  clippy::large_stack_arrays,
  clippy::large_types_passed_by_value,
  clippy::macro_use_imports,
  clippy::manual_assert,
  clippy::manual_let_else,
  clippy::manual_ok_or,
  clippy::manual_string_new,
  clippy::many_single_char_names,
  clippy::map_unwrap_or,
  clippy::match_bool,
  clippy::match_on_vec_items,
  clippy::match_same_arms,
  clippy::mut_mut,
  clippy::needless_for_each,
  clippy::option_option,
  clippy::similar_names
)]

extern crate rustc_ast;
extern crate datafrog;
extern crate either;
extern crate polonius_engine;
extern crate rustc_abi;
extern crate rustc_apfloat;
extern crate rustc_borrowck;
extern crate rustc_const_eval;
extern crate rustc_data_structures;
extern crate rustc_driver;
extern crate rustc_error_messages;
extern crate rustc_errors;
extern crate rustc_graphviz;
extern crate rustc_hir;
extern crate rustc_hir_pretty;
extern crate rustc_index;
extern crate rustc_infer;
extern crate rustc_interface;
extern crate rustc_macros;
extern crate rustc_middle;
extern crate rustc_mir_dataflow;
extern crate rustc_mir_transform;
extern crate rustc_serialize;
extern crate rustc_session;
extern crate rustc_span;
extern crate rustc_target;
extern crate rustc_trait_selection;
extern crate rustc_type_ir;
extern crate smallvec;
pub mod ir_mapper;
use crate::rustc_span::Pos;
use std::{borrow::Cow, env, collections::HashMap};
use clap::Parser;
use rustc_plugin::{CrateFilter, RustcPlugin, RustcPluginArgs, Utf8Path};
use serde::{Deserialize, Serialize};
use rustc_span::source_map::{SourceMap};
use aquascope::analysis::{AquascopeAnalysis,
  boundaries::PermissionsBoundary};
use rustc_hir::intravisit::{Visitor};
use rustc_hir::{ItemKind};
use rustc_middle::{
  ty::{TyCtxt},
};
use rustc_utils::{
  source_map::range::{CharRange},
  mir::{borrowck_facts},
};
use crate::visitor::{ExprVisitor};
pub mod visitor;
pub struct PrintAllItemsPlugin;

#[derive(Parser, Serialize, Deserialize)]
pub struct PrintAllItemsPluginArgs {
  #[arg(short, long)]//add '--allcaps' or '-a' in command line both works
  allcaps: bool,//can set default value
}

impl RustcPlugin for PrintAllItemsPlugin {
  type Args = PrintAllItemsPluginArgs;

 
  fn version(&self) -> Cow<'static, str> { 
    env!("CARGO_PKG_VERSION").into()
  }

  fn driver_name(&self) -> Cow<'static, str> {//which driver to use
    "print-all-items-driver".into()
  }

  fn args(&self, _target_dir: &Utf8Path) -> RustcPluginArgs<Self::Args> {
    let args = PrintAllItemsPluginArgs::parse_from(env::args().skip(1));//read command line arguments
    let filter = CrateFilter::AllCrates;//decide which crate to compile
    RustcPluginArgs { args, filter }
  }


  //'run' is called when the plugin is executed by the Rust compiler
  fn run(
    self,
    compiler_args: Vec<String>, //vector of command line arguments
    plugin_args: Self::Args, //any plugin-specific arguments
  ) -> rustc_interface::interface::Result<()> {
    let mut compiler_args=compiler_args;
    compiler_args.extend(
      "-Z identify-regions -Z mir-opt-level=0 -Z track-diagnostics=yes -Z maximal-hir-to-mir-coverage --allow warnings"
      .split(' ')
      .map(|s| s.to_owned()),
    );
    let mut callbacks = PrintAllItemsCallbacks { args: plugin_args };
    let compiler = rustc_driver::RunCompiler::new(&compiler_args, &mut callbacks);

    compiler.run()
  }
}

struct PrintAllItemsCallbacks {
  args: PrintAllItemsPluginArgs,
}

impl rustc_driver::Callbacks for PrintAllItemsCallbacks {

    fn config(&mut self, config: &mut rustc_interface::Config) {
      // You MUST configure rustc to ensure `get_body_with_borrowck_facts` will work.
      config.override_queries = Some(borrowck_facts::override_queries);
    }

    fn after_analysis<'tcx>(
      &mut self,
      _compiler: &rustc_interface::interface::Compiler,
      queries: &'tcx rustc_interface::Queries<'tcx>, 
    ) -> rustc_driver::Compilation {
      queries
        .global_ctxt()//return an option TyCtxt
        .unwrap()
        .enter(|tcx| print_all_items(tcx, &self.args));
      // allow compilation to continue.
      rustc_driver::Compilation::Continue
    }
}


  

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

// "The main function"
fn print_all_items(tcx: TyCtxt, args: &PrintAllItemsPluginArgs) {
  // Generate a few things needed for later analysis. They
  // are basically things generated when compiling code.
  let hir = tcx.hir().clone();
  hir
  .items()
  .filter_map(|id| match hir.item(id).kind {
    ItemKind::Fn(_, _, body) => Some(body),
    _ => {
      None
    },
  })
  .for_each(|body_id| {
    let hir_body = hir.body(body_id);
    let def_id = tcx.hir().body_owner_def_id(body_id);
    let bwf = borrowck_facts::get_body_with_borrowck_facts(tcx, def_id);
    let body = &bwf.body;
    
    let owner = tcx.hir().body_owner(body_id);
    let name = match tcx.hir().opt_name(owner) {
    Some(name) => name.to_ident_string(),
    None => "<anonymous>".to_owned(),
  };
  // Our analysis begins here. Things are printed out.
  println!("********************************************************************");
  println!("computing body permissions {:?}", name);
  // run the AquascopeAnalysis and get the result
  let result=AquascopeAnalysis::run(tcx,body_id);
  let source_map = tcx.sess.source_map();
  match result {
    Ok(output) =>{
     let body_range = output.body_range;
     let boundaries = output.boundaries;
     let steps = output.steps;
     println!("Body_range from {:?} to {:?}.", body_range.start , body_range.end);
     // Create a hashmap from BytePos to boundaries generated by aquascope
     let mut boundary_map: HashMap<rustc_span::BytePos,PermissionsBoundary> = HashMap::new();

     for boundary in boundaries {
      let bytepos=boundary.byte_location.0 as u32;
      boundary_map.insert(rustc_span::BytePos(bytepos),boundary);
    };
     // This is to print out the permission tables in aquascope (which we currently don't actually 
     // use in the analysis but might be useful). If you want to glance at what's in the table, 
     // uncomment the println!().
     for step in steps {
      let location = step.location;
      let line = charrange_to_line(location,source_map);
      //println!("Step on line {} from {:?} to {:?}.",line,location.start,location.end);
      let state = step.state;
      for table in state {
        let state = table.state;
        let from = table.from;
        let to = table.to;
        //println!("Table from {:?}-{:?} to {:?}-{:?}.",
        //from.start,from.end,to.start,to.end);
        //for (x,diff) in state{
        // println!("{} with permission diff {:?} ",
        //  x,diff);
        //};
      };
     };
     // The visitor will walk through the hir and analysis it, see visitor.rs.
     let mut visitor = ExprVisitor { 
      tcx, 
      mir_body:body, 
      boundary_map,
      access_points:HashMap::new(),
      mutability_map:HashMap::new(),
      lifetime_map:HashMap::new(),
    };
    visitor.visit_body(hir_body);
    visitor.print_definitions();
    visitor.print_out_of_scope();
    visitor.print_lifetimes();
    },
    _ =>{println!("Analysis Error.");}
  }
})

}

