//! Public interface to the rustviz2 plugin.
//!
//! `Rustviz::new(code)` runs the plugin against a single-file crate built
//! from `code` and returns the rendered code-panel and timeline-panel SVGs.
//!
//! There are two execution backends, selected by the `RV_RUNNER` env var:
//!
//! - `RV_RUNNER=docker` (default) runs the plugin inside the
//!   `rustviz/rustviz-runner` image with no network, a read-only filesystem,
//!   tmpfs-backed `/work`, and capped memory / CPU / pids / wall-time. This
//!   is the only backend appropriate for untrusted input — proc-macro
//!   expansion in user code is arbitrary code execution, so the plugin must
//!   not run unsandboxed.
//! - `RV_RUNNER=local` runs the plugin in-process by shelling out to a
//!   tempdir and `cargo rv-plugin`. Convenient for development, **never
//!   safe for untrusted input**.

use std::{
    env,
    error::Error,
    fmt, fs,
    io::Write,
    process::{Command, Stdio},
    thread,
    time::{Duration, Instant},
};
use tempfile::tempdir;

const TOOLCHAIN: &str = r#"[toolchain]
channel = "nightly-2024-05-20"
components = ["rust-src", "rustc-dev", "llvm-tools-preview"]"#;

/// Default image tag for the docker backend.
const DEFAULT_RUNNER_IMAGE: &str = "rustviz/rustviz-runner:latest";

/// Hard wall-clock cap on a single visualization request, in seconds.
/// Compilation of legitimate examples completes well under a second; this is
/// a safety net against malicious input that wedges rustc.
const RUNNER_TIMEOUT_SECS: u64 = 20;

#[derive(Debug)]
enum RvError {
    FsError(String),
    PluginError(String),
    Timeout,
    DockerUnavailable(String),
    BadConfig(String),
}

impl fmt::Display for RvError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RvError::FsError(msg) => write!(f, "File system error: {}", msg),
            RvError::PluginError(msg) => write!(f, "Internal plugin error {}", msg),
            RvError::Timeout => write!(f, "Visualization timed out"),
            RvError::DockerUnavailable(msg) => write!(f, "Docker unavailable: {}", msg),
            RvError::BadConfig(msg) => write!(f, "Invalid runner configuration: {}", msg),
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
    pub fn new(code_str: &str) -> Result<Rustviz, Box<dyn Error>> {
        let raw = match runner_backend()?.as_str() {
            "docker" => run_docker(code_str)?,
            "local" => run_local(code_str)?,
            other => {
                return Err(Box::new(RvError::BadConfig(format!(
                    "RV_RUNNER must be \"docker\" or \"local\" (got {:?})",
                    other
                ))));
            }
        };
        parse_output(&raw)
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

fn runner_backend() -> Result<String, Box<dyn Error>> {
    Ok(env::var("RV_RUNNER").unwrap_or_else(|_| "docker".to_string()))
}

/// Production path: spawn a sandboxed container, pipe the user's source on
/// stdin, capture stdout. Container flags must be kept in sync with the
/// guarantees described in SECURITY.md.
fn run_docker(code_str: &str) -> Result<Vec<u8>, Box<dyn Error>> {
    let image = env::var("RV_RUNNER_IMAGE").unwrap_or_else(|_| DEFAULT_RUNNER_IMAGE.to_string());

    let mut child = Command::new("docker")
        .args([
            "run",
            "--rm",
            "-i",
            "--network=none",
            "--read-only",
            "--memory=512m",
            "--memory-swap=512m",
            "--cpus=1",
            "--pids-limit=64",
            "--cap-drop=ALL",
            "--security-opt=no-new-privileges",
            "--tmpfs=/work:rw,size=128m,mode=1777",
            "--tmpfs=/tmp:rw,size=32m",
            &image,
        ])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .map_err(|e| RvError::DockerUnavailable(format!("failed to spawn docker: {}", e)))?;

    if let Some(stdin) = child.stdin.as_mut() {
        stdin.write_all(code_str.as_bytes())?;
    }
    drop(child.stdin.take());

    let deadline = Instant::now() + Duration::from_secs(RUNNER_TIMEOUT_SECS);
    loop {
        if let Some(status) = child.try_wait()? {
            let mut stdout = Vec::new();
            let mut stderr = Vec::new();
            if let Some(mut s) = child.stdout.take() {
                std::io::Read::read_to_end(&mut s, &mut stdout)?;
            }
            if let Some(mut s) = child.stderr.take() {
                std::io::Read::read_to_end(&mut s, &mut stderr)?;
            }
            if !status.success() {
                return Err(Box::new(RvError::PluginError(
                    String::from_utf8_lossy(&stderr).to_string(),
                )));
            }
            return Ok(stdout);
        }
        if Instant::now() >= deadline {
            let _ = child.kill();
            let _ = child.wait();
            return Err(Box::new(RvError::Timeout));
        }
        thread::sleep(Duration::from_millis(50));
    }
}

/// Dev-only path: run the plugin in-process against a tempdir. Convenient
/// when iterating without Docker, but UNSAFE for untrusted input — proc
/// macros in user code execute as the host process.
fn run_local(code_str: &str) -> Result<Vec<u8>, Box<dyn Error>> {
    let tempdir = tempdir()?;
    let root = tempdir.path();
    let status = Command::new("cargo")
        .args(["new", "--lib", "test-crate"])
        .current_dir(root)
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status()?;
    if !status.success() {
        return Err(Box::new(RvError::FsError(
            "cargo new failed".to_string(),
        )));
    }

    let cwd = root.join("test-crate");
    fs::write(cwd.join("rust-toolchain.toml"), TOOLCHAIN)?;
    fs::write(cwd.join("src").join("lib.rs"), code_str)?;

    let output = Command::new("cargo")
        .arg("rv-plugin")
        .current_dir(cwd)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()?;

    if !output.status.success() {
        return Err(Box::new(RvError::PluginError(
            String::from_utf8_lossy(&output.stderr).to_string(),
        )));
    }
    Ok(output.stdout)
}

fn parse_output(stdout: &[u8]) -> Result<Rustviz, Box<dyn Error>> {
    let stdout = std::str::from_utf8(stdout)?;
    let parts: Vec<&str> = stdout.splitn(2, ":::").collect();
    if parts.len() != 2 {
        return Err(Box::new(RvError::PluginError(format!(
            "Unexpected output format {}",
            stdout
        ))));
    }
    let code_p = parts[0];
    let time_p = parts[1];

    let height = match time_p.find("height=") {
        Some(index) => {
            let start = index + "height=\"".len();
            if let Some(end) = time_p[start..].find("px\"") {
                time_p[start..start + end].parse::<i32>()?
            } else {
                return Err(Box::new(RvError::PluginError(format!(
                    "couldn't find px height identifier {}",
                    time_p
                ))));
            }
        }
        None => {
            return Err(Box::new(RvError::PluginError(format!(
                "couldn't find height identifier {}",
                time_p
            ))));
        }
    };

    Ok(Rustviz {
        code_panel: code_p.to_string(),
        timeline_panel: time_p.to_string(),
        height,
    })
}
