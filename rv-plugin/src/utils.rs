use anyhow::{Result, anyhow};
use log::info;
use rustc_middle::ty::TyCtxt;
use crate::svg_generator::data::ExternalEvent;
use std::collections::{HashMap, BTreeMap};
use std::{path::PathBuf, fs};
use std::env::current_dir;
use crate::rustviz_library::rv::Rustviz;
use std::fs::File;

use crate::expr_visitor::RapData;

// toplevel annotation helpers
pub fn annotate_struct_field (
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


pub fn annotate_toplevel_fn (
  func_ident: rustc_span::symbol::Ident, 
  line_str: &str, 
  raps: & HashMap<String, RapData>,
  a_map: & mut BTreeMap<usize, Vec<String>>,
  hashes: & mut usize,
  m: &TyCtxt)  {
  let func_name = func_ident.as_str().to_owned();
  
  let line: usize = m.sess.source_map().lookup_char_pos(func_ident.span.lo()).line;
  let left: usize = m.sess.source_map().lookup_char_pos(func_ident.span.lo()).col_display;
  let right: usize = m.sess.source_map().lookup_char_pos(func_ident.span.hi()).col_display;
  let hash = match raps.get(&func_name) {
    Some(r) => { *r.rap.hash() }
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

pub fn annotate_enum_variant(
  ctor_name: &str, 
  parent_name: &str,
  variant: & rustc_hir::Variant,
  rap_map: & HashMap<String, RapData>,
  a_map: & mut BTreeMap<usize, Vec<String>>,
  m: & TyCtxt
) {
  let rap_name = format!("{}::{}", parent_name, ctor_name);
  if rap_map.contains_key(&rap_name) {
    let span = variant.ident.span;
    let hash = rap_map.get(&rap_name).unwrap().rap.hash();
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


pub struct RV1Helper {
  source_str: String,
  source_path: PathBuf
}

impl RV1Helper {
  pub fn new () -> RV1Helper {
    RV1Helper { source_str: String::new(), source_path: PathBuf::new() }
  }
  pub fn initialize_line_map(&mut self) -> Result<BTreeMap<usize, String>> {
    self.source_path = current_dir()?;
    self.source_path = self.source_path.join("src/lib.rs"); // could change this to whatever
    info!("source path {:#?}", self.source_path);
  
    let mut line_map: BTreeMap<usize, String> = BTreeMap::new();
  
  
    match fs::read_to_string(self.source_path.clone()) {
      Ok(contents) => {
        self.source_str = contents.clone(); // allows for comments in source string
        let mut res_str: String = String::new();
        // remove all comments from main string
        for line in contents.lines() {
          res_str.push_str(line);
          res_str.push('\n');
  
        }
        let lines: Vec<&str> = res_str.lines().collect();
        for (line_num, line_content) in lines.iter().enumerate() {
          line_map.insert(line_num + 1, line_content.to_string());
        }
      }
  
      Err(e) => {
        return Err(anyhow!("Error with reading source file : {}", e));
      }
    }

    //println!("BT MAP: {:#?}", line_map);
    return Ok(line_map);
  }

  pub fn generate_vis(& mut self, 
    mut line_map: BTreeMap<usize, Vec<ExternalEvent>>, 
    p_events: Vec<(usize, ExternalEvent)>, 
    a_map: & mut BTreeMap<usize, Vec<String>>,
    num_raps: usize,
    write_to_cwd: bool) -> Result<()> {
    let mut keys_to_remove: Vec<usize> = Vec::new();
    for (k, v) in line_map.iter() {
      if v.is_empty() {
        keys_to_remove.push(*k);
      }
    }

    for k in keys_to_remove.iter() {
      line_map.remove(k);
    }

    let annotated_source_str: String = generate_annotated_src(a_map);
    //println!("ANNOTATED : \n{}", annotated_source_str);


    // send stuff to RV1
    let rv = Rustviz::new(&annotated_source_str, &self.source_str, p_events, line_map, num_raps)?;

    if write_to_cwd { // write the SVG files
      self.source_path.pop(); // just write to inside cwd
      let code_panel_path: PathBuf = self.source_path.join("vis_code.svg");
      let timeline_panel_path: PathBuf = self.source_path.join("vis_timeline.svg");
  
      if !code_panel_path.exists(){
        File::create(code_panel_path.clone())?;
      }
    
      if !timeline_panel_path.exists(){
        File::create(timeline_panel_path.clone())?;
      }
  
      fs::write(code_panel_path, rv.code_panel())?;
      fs::write(timeline_panel_path, rv.timeline_panel())?;
    }
    else {
      // write SVG files to stdio
      let res = format!("{}:::{}", rv.code_panel(), rv.timeline_panel());
      println!("{res}");
    }
    Ok(())
  }
  

}

fn union_strings (strings: &Vec<String>) -> String {
  if strings.len() == 1 {
    strings[0].clone()
  }
  else if strings.len() == 2 {
    strings[1].clone()
  }
  else {
    let mut res = String::new();
    let i = 0;
    let mut offsets: HashMap<String, usize> = HashMap::new();
    for string in strings {
      offsets.insert(string.clone(), 0);
    }
    for i in i..strings[0].len() {
      let consistent_char = strings[0].chars().nth(i).unwrap();
      for string in strings {
        let mut j = offsets[string];
        let mut char_at_i = string.chars().nth(j).unwrap();
        if char_at_i != consistent_char {
          assert_eq!(char_at_i, '['); //todo: fix
          while char_at_i != ']' {
            res.push(char_at_i);
            j += 1;
            char_at_i = string.chars().nth(j).unwrap();
          } // loop until closing ']' since characters [_.._] could contain consistent_char
          res.push(char_at_i); // add ']'
          j += 1;
        }
        j += 1;
        *offsets.get_mut(string).unwrap() = j;
      }
      res.push(consistent_char);
    }
    
    for string in strings {
      if offsets[string] != string.len() {
        res.push_str(&string[offsets[string]..]);
        break;
      }
    }
    res
  }
}

pub fn generate_annotated_src(annotated_line_map: & mut BTreeMap<usize, Vec<String>>) -> String {
  let mut annotated_str = String::new();
  for (_k, v) in annotated_line_map {
    annotated_str.push_str(&union_strings(v));
    annotated_str.push('\n');
  }
  annotated_str = annotated_str.replace("&", "&amp;");
  annotated_str = annotated_str.replace("<", "&lt;");
  annotated_str = annotated_str.replace(">", "&gt;");
  annotated_str = annotated_str.replace("[_", "<");
  annotated_str = annotated_str.replace("_]", ">");

  annotated_str
}