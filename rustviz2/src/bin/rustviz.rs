//! `rustviz` — render a single Rust source file to a RustViz
//! visualization.
//!
//! Two output modes:
//!
//! - `rustviz svg foo.rs [-o DIR]` writes `foo.code.svg` and
//!   `foo.timeline.svg` side by side. Useful for embedding in your
//!   own HTML/Markdown workflow.
//! - `rustviz html foo.rs [-o FILE]` writes a single self-contained
//!   `foo.html` with both SVGs inlined and the tooltip JS embedded.
//!   Open in any browser; no server required.
//!
//! Under the hood we call the same `rustviz2::Rustviz::new(code)`
//! API that the playground and the mdbook preprocessor use, which
//! shells out to the rustc plugin (see `rustviz2-plugin/`). That
//! requires the nightly toolchain pinned by `rust-toolchain.toml`
//! plus `cargo rv-plugin` on `PATH` — installed by `rustviz init`
//! (TODO) or manually via `cargo install --path rustviz2-plugin`
//! against this repo.

use std::{
    fs,
    path::{Path, PathBuf},
};

use anyhow::{Context, Result, anyhow};
use clap::{Parser, Subcommand};
use rustviz2::{HELPERS_JS, Rustviz};

#[derive(Parser)]
#[command(
    name = "rustviz",
    version,
    about = "Render a Rust source file to a RustViz visualization.",
    long_about = None,
)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Write the two SVG panels (code + timeline) as separate files.
    Svg {
        /// Path to the Rust source file to visualize.
        input: PathBuf,
        /// Directory to write the output SVGs into. Defaults to the
        /// input file's directory. Outputs are named
        /// `<stem>.code.svg` and `<stem>.timeline.svg`.
        #[arg(short, long, value_name = "DIR")]
        output: Option<PathBuf>,
    },
    /// Write a single self-contained HTML page with both SVGs +
    /// tooltip JS inlined.
    Html {
        /// Path to the Rust source file to visualize.
        input: PathBuf,
        /// Path to the output HTML file. Defaults to
        /// `<input-stem>.html` next to the input.
        #[arg(short, long, value_name = "FILE")]
        output: Option<PathBuf>,
        /// Page title. Defaults to the input file's stem.
        #[arg(long)]
        title: Option<String>,
    },
}

fn main() -> Result<()> {
    let cli = Cli::parse();
    match cli.command {
        Commands::Svg { input, output } => render_svg(&input, output.as_deref()),
        Commands::Html { input, output, title } => {
            render_html(&input, output.as_deref(), title.as_deref())
        }
    }
}

/// Stem of the input file's basename, used to name output files.
/// `path/to/foo.rs` → `"foo"`.
fn stem(input: &Path) -> Result<String> {
    input
        .file_stem()
        .and_then(|s| s.to_str())
        .map(str::to_owned)
        .ok_or_else(|| anyhow!("input path has no usable file stem: {}", input.display()))
}

fn render(input: &Path) -> Result<Rustviz> {
    let code = fs::read_to_string(input)
        .with_context(|| format!("failed to read {}", input.display()))?;
    Rustviz::new(&code).map_err(|e| anyhow!(e.to_string()))
}

fn render_svg(input: &Path, output_dir: Option<&Path>) -> Result<()> {
    let rv = render(input)?;
    let stem = stem(input)?;
    let dir = match output_dir {
        Some(d) => d.to_path_buf(),
        None => input.parent().unwrap_or_else(|| Path::new(".")).to_path_buf(),
    };
    fs::create_dir_all(&dir)
        .with_context(|| format!("failed to create {}", dir.display()))?;

    let code_path = dir.join(format!("{stem}.code.svg"));
    let tl_path = dir.join(format!("{stem}.timeline.svg"));
    fs::write(&code_path, rv.code_panel_string())
        .with_context(|| format!("failed to write {}", code_path.display()))?;
    fs::write(&tl_path, rv.timeline_panel_string())
        .with_context(|| format!("failed to write {}", tl_path.display()))?;

    eprintln!("wrote {}", code_path.display());
    eprintln!("wrote {}", tl_path.display());
    Ok(())
}

fn render_html(input: &Path, output: Option<&Path>, title: Option<&str>) -> Result<()> {
    let rv = render(input)?;
    let stem = stem(input)?;
    let title = title.unwrap_or(&stem);
    let out_path = match output {
        Some(p) => p.to_path_buf(),
        None => input
            .parent()
            .unwrap_or_else(|| Path::new("."))
            .join(format!("{stem}.html")),
    };

    // Plain string concatenation, not format!() — the helpers JS
    // body has unescaped `{` / `}` everywhere and would blow up
    // any format-template engine. We give each panel its own pair
    // of identifying classes (`example-1 code_panel` and
    // `example-1 tl_panel`) so the helpers script's
    // `getElementsByClassName('example-1')[0|1]` lookup matches.
    let code_panel = annotate_panel(&rv.code_panel_string(), "example-1 code_panel");
    let tl_panel = annotate_panel(&rv.timeline_panel_string(), "example-1 tl_panel");
    let height = rv.height();

    let mut html = String::new();
    html.push_str("<!DOCTYPE html>\n<html lang=\"en\">\n<head>\n  <meta charset=\"utf-8\" />\n  <title>");
    html.push_str(&html_escape(title));
    html.push_str("</title>\n  <style>\n");
    html.push_str(STYLE_CSS);
    html.push_str("  </style>\n</head>\n<body>\n  <div class=\"vis-container\" style=\"height: ");
    html.push_str(&height.to_string());
    html.push_str("px\" onmouseenter=\"helpers('example-1')\">\n");
    html.push_str(&code_panel);
    html.push_str("\n");
    html.push_str(&tl_panel);
    html.push_str("\n  </div>\n  <script>\n");
    html.push_str(HELPERS_JS);
    html.push_str("\n  </script>\n</body>\n</html>\n");

    fs::write(&out_path, html)
        .with_context(|| format!("failed to write {}", out_path.display()))?;
    eprintln!("wrote {}", out_path.display());
    Ok(())
}

/// Inject a `class="…"` attribute into the leading `<svg …>` tag of
/// a panel string so the helpers script can find both panels by
/// class name (e.g. `example-1 code_panel` + `example-1 tl_panel`).
/// Returns the original string unchanged if no `<svg` tag is found.
fn annotate_panel(svg: &str, class: &str) -> String {
    let Some(open_start) = svg.find("<svg") else { return svg.to_owned() };
    // Insert `class="…"` right after `<svg` and before the rest of
    // the open tag's attributes.
    let mut out = String::with_capacity(svg.len() + class.len() + 12);
    out.push_str(&svg[..open_start + 4]);
    out.push_str(" class=\"");
    out.push_str(class);
    out.push('"');
    out.push_str(&svg[open_start + 4..]);
    out
}

fn html_escape(s: &str) -> String {
    s.replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
        .replace('"', "&quot;")
}

const STYLE_CSS: &str = r#"
    body { margin: 1em; font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif; }
    .vis-container { display: flex; align-items: flex-start; gap: 1em; }
    .vis-container > svg { background: #f1f1f1; }
"#;
