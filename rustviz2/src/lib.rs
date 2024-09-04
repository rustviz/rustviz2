
use std::{error::Error, fmt, fs, process::{Command, Stdio}};
use tempfile::tempdir;


static TOOLCHAIN: &str = 
  r#"[toolchain]
  channel = "nightly-2024-05-20"
  components = ["rust-src", "rustc-dev", "llvm-tools-preview"]"#;

#[derive(Debug)]
enum RvError {
  FsError(String),
  PluginError(String)
}

impl fmt::Display for RvError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
      match self {
          RvError::FsError(msg) => write!(f, "File system error: {}", msg),
          RvError::PluginError(msg) => write!(f, "Internal plugin error {}", msg)
      }
  }
}

impl Error for RvError {}

#[derive(Debug)]
pub struct Rustviz {
  code_panel: String,
  timeline_panel: String,
  height: i32,
}


impl Rustviz {
  pub fn new(code_str: &str) -> Result<Rustviz, Box<dyn std::error::Error>> {
    // add new temporary cargo crate (test-crate)
    let tempdir = tempdir()?;
    let root = tempdir.path();
    let status = Command::new("cargo")
      .args(["new", "--lib", "test-crate"])
      .current_dir(root)
      .stdout(Stdio::null())
      .stderr(Stdio::null())
      .status()?;
    if !status.success() {
      return Err(Box::new(RvError::FsError("Cargo failed, failed to create test-crate".to_string())));
    }

    // create rust-toolchain.toml file, write TOOLCHAIN contents to it
    let cwd = root.join("test-crate");
    let toolchain_path = cwd.join("rust-toolchain.toml");
    fs::write(&toolchain_path, TOOLCHAIN)?;
    
    // write code_str to test-crate
    let lib_rs_path = cwd.join("src").join("lib.rs");
    fs::write(&lib_rs_path, code_str)?;

    // navigate to test-crate and run rv-plugin
    let output = Command::new("cargo")
      .arg("rv-plugin")
      .current_dir(cwd)
      .stdout(Stdio::piped())
      .stderr(Stdio::piped())
      .output()?;
    
    if !output.status.success() {
      let stderr = String::from_utf8_lossy(&output.stderr);
      return Err(Box::new(RvError::PluginError(stderr.to_string())));
    }

    // parse the output for code_panel, timeline_panel, and height
    // ideally we have the output from the plugin be JSON
    let stdout = String::from_utf8(output.stdout)?;
    let parts: Vec<&str> = stdout.splitn(2, ":::").collect();

    if parts.len() != 2 {
      return Err(Box::new(RvError::PluginError(format!("Unexpected output format {}", stdout))));
    }

    let code_p = parts[0];
    let time_p = parts[1]; 

    let height = match time_p.find("height=") {
      Some(index) => {
        let start = index + "height=\"".len();
                      if let Some(end) = time_p[start..].find("px\"") {
                          let height_str = &time_p[start..start + end];
                          match height_str.parse::<i32>() {
                              Ok(value) => value,
                              Err(e) => {
                                return Err(Box::new(e));
                              }
                          }
                      } else {
                        return Err(Box::new(RvError::PluginError(format!("couldn't find px height identifier {}", time_p))));
                      }
      }
      None => { return Err(Box::new(RvError::PluginError(format!("couldn't find height identifier {}", time_p)))); }
    };

    Ok(Rustviz { code_panel: code_p.to_string(), timeline_panel: time_p.to_string(), height: height })
  }

  pub fn code_panel_string(&self) -> String {
    self.code_panel.clone()
  }

  pub fn timeline_panel_string(&self) -> String {
    self.timeline_panel.clone()
  }

  pub fn height(&self) -> i32 {
    self.height
  }
}

