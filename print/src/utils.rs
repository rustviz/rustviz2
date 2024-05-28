use anyhow::{Result, anyhow};
use rustviz_lib::data::ExternalEvent;
use crate::expr_visitor::AccessPointUsage;
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
          let line_without_comment = if let Some(pos) = line.find("//") {
            &line[..pos]
          }
          else {
            line
          };
          res_str.push_str(line_without_comment);
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

  pub fn generate_vis(& mut self, mut line_map: BTreeMap<usize, Vec<ExternalEvent>>, p_events: Vec<(usize, ExternalEvent)>, a_map: &BTreeMap<usize, Vec<String>>) -> Result<()> {
    //let mut main_str = String::new();

    // add closing brace for event strings
    // for (_, value) in line_map.iter_mut(){
    //   if value.contains("//") {
    //     value.push_str(" }");
    //   }
    // }
  
    // add variable definitions
    // main_str.push_str("/* --- BEGIN Variable Definitions ---\n");
    // for name in owners {
    //   if name.contains("Not") {
    //     let n = name.replace("Not", "");
    //     main_str.push_str(&(n + "\n"));
    //   }
    //   else if name.contains("Mut") && !name.contains("MutRef") {
    //     let n = name.replace("Mut", "mut");
    //     main_str.push_str(&(n + "\n"));
    //   }
    //   else {
    //     main_str.push_str(&(name.to_owned() + "\n"));
    //   }
    // }
    // main_str.push_str("--- END Variable Definitions --- */\n");
  
  
    // // add the rest from the line map
    // for (_, value) in line_map {
    //   main_str.push_str(&(value.to_owned() + "\n"));
    // }
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
    // println!("MAIN :\n {}", main_str);
    // println!("SOURCE : \n{}", self.source_str);
    // println!("ANNOTATED : \n{}", annotated_source_str);


    // send stuff to RV1
    let rv = Rustviz::new(&annotated_source_str, &self.source_str, p_events, line_map)?;

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
          assert_eq!(char_at_i, '<');
          while char_at_i != '>' {
            res.push(char_at_i);
            j += 1;
            char_at_i = string.chars().nth(j).unwrap();
          } // loop until closing '>' since characters <..> could contain consistent_char
          res.push(char_at_i); // add '>'
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

pub fn generate_annotated_src(annotated_line_map: &BTreeMap<usize, Vec<String>>) -> String {
  let mut annotated_str = String::new();
  for (_k, v) in annotated_line_map {
    annotated_str.push_str(&union_strings(v));
    annotated_str.push('\n');
  }
  annotated_str.replace("&", "&amp;")
}


// for now not a very viable solution
pub fn generate_annotated_src_scuffed(source_str: String, access_points: &HashMap<AccessPointUsage, usize>) -> String {
  let mut annotated_src_str = source_str;
  let mut num_hashes: usize = 1;
  // let mut fn_hashes: usize = 1;
  for (point,_) in access_points {
    let nombre: String;
    let mut fn_flag: bool = false;
    match point {
      AccessPointUsage::Owner(p)=>{
        nombre = p.name.clone();
      }
      AccessPointUsage::StaticRef(p)=>{
        nombre = p.name.clone();
      }
      AccessPointUsage::MutRef(p)=>{
        nombre = p.name.clone();
      }
      AccessPointUsage::Function(name)=>{
        nombre = name.clone();
        fn_flag = true;
      }
      AccessPointUsage::Struct(p, fields)=>{
        let field_names: Vec<String> = 
          fields.iter().
          map(|s| 
            if let Some(dot_index) = s.name.find('.') {
              s.name[dot_index + 1..].to_string()
            } else {
                println!("No dot found in the string.");
              s.name.clone()
            }).collect::<Vec<String>>();
        for i in 0..fields.len() {
          // replace identifier definitions in struct {<a>:i32, <b>:i32}
          let replace_with: String = format!("<tspan data-hash=\"{}\">{}</tspan>", num_hashes, field_names[i]);
          annotated_src_str = annotated_src_str.replace(&field_names[i], &replace_with);

          num_hashes += 1;

        }
        // struct instance name
        nombre = p.name.clone(); 
      }
    }

    if fn_flag{
      let replace_with: String = format!("<tspan class=\"fn\" data-hash=\"{}\" hash=\"{}\">{}</tspan>", 0, num_hashes, nombre);
      annotated_src_str = annotated_src_str.replace(&nombre, &replace_with);
    }
    else{
      let replace_with: String = format!("<tspan data-hash=\"{}\">{}</tspan>", num_hashes, nombre);
      annotated_src_str = annotated_src_str.replace(&nombre, &replace_with);
    }

    if num_hashes < 9 {
      num_hashes += 1;
    }
  }

  // handle amp
  annotated_src_str = annotated_src_str.replace("&", "&amp;");
  annotated_src_str
}