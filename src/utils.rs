use anyhow::{Result, anyhow};
use regex::Regex;
use rustviz_lib::data::ExternalEvent;
use std::collections::{HashMap, BTreeMap};
use std::{path::PathBuf, fs};
use std::env::current_dir;
use rustviz_library::Rustviz;
use std::fs::File;


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

  pub fn generate_vis(& mut self, mut line_map: BTreeMap<usize, Vec<ExternalEvent>>, p_events: Vec<(usize, ExternalEvent)>, a_map: & mut BTreeMap<usize, Vec<String>>,
  num_raps: usize) -> Result<()> {
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
  println!("annotated line map {:#?}", annotated_line_map);
  for (_k, v) in annotated_line_map {
    annotated_str.push_str(&union_strings(v));
    annotated_str.push('\n');
  }
  annotated_str = annotated_str.replace("&", "&amp;");
  annotated_str = annotated_str.replace("<", "&lt;");
  annotated_str = annotated_str.replace(">", "&gt;");
  annotated_str = annotated_str.replace("[_", "<");
  annotated_str = annotated_str.replace("_]", ">");

  println!("annotated str {}", annotated_str);

  annotated_str
}