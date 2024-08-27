extern crate semver;
extern crate pulldown_cmark;
extern crate pulldown_cmark_to_cmark;

use mdbook::errors::Error;
use mdbook::book::{Book, BookItem, Chapter};
use mdbook::preprocess::{CmdPreprocessor, Preprocessor, PreprocessorContext};

use crate::Result;
use crate::cfg::Cfg;

use core::str;
use std::path::{Path, PathBuf};
use std::fs::{self, File};
use std::process::{Command, Stdio};

pub struct RustvizPlugin {
	src_dir: PathBuf,
	test_crate_dir: PathBuf
}

impl RustvizPlugin {
	pub fn new(path: &Path) -> RustvizPlugin {
		// create test-crate directory
		let parent_dir = match path.parent() {
			Some(p) => p.to_owned(),
			None => panic!("don't put this in root")
		};
		
		let output = Command::new("cargo")
			.arg("new")
			.arg("--lib")
			.arg("test-crate")
			.current_dir(parent_dir.clone())
			.output();

		match output {
			Ok(_res) => {
				info!("test-crate successfully created")
			}
			Err(e) => {
				warn!("error with creating test-crate {}", e)
			}
		}
		
		RustvizPlugin {
			src_dir: PathBuf::from(path).join("src"),
			test_crate_dir: parent_dir.join("test-crate")
		}
	}

	#[allow(dead_code)]
	pub fn handle_preprocessing(&self) -> Result {
		use std::io::{stdin, stdout};
		use semver::{Version, VersionReq};

		let (ctx, book) = CmdPreprocessor::parse_input(stdin())?;
		let current = Version::parse(&ctx.mdbook_version)?;
		let built = VersionReq::parse(&format!("~{}", mdbook::MDBOOK_VERSION))?;

		if ctx.mdbook_version != mdbook::MDBOOK_VERSION && !built.matches(&current) {
			warn!(
			      "The {} plugin was built against version {} of mdbook, \
				      but we're being called from version {}, so may be incompatible.",
			      self.name(),
			      mdbook::MDBOOK_VERSION,
			      ctx.mdbook_version
			);
		}
		let processed_book = self.run(&ctx, book)?;
		serde_json::to_writer(stdout(), &processed_book)?;
		Ok(())
	}
}


impl Preprocessor for RustvizPlugin {
	fn name(&self) -> &str { "rustviz" }

	fn supports_renderer(&self, renderer: &str) -> bool { renderer != "not-supported" }


	fn run(&self, ctx: &PreprocessorContext, mut book: Book) -> Result<Book, Error> {
		// create examples directory -- NOTE: needs to be called example for rv1
		use std::fs;
		let assets_dir = self.src_dir.join("examples");
		if let Err(err) = fs::create_dir(self.src_dir.join("examples")){
			warn!("Error creating assets directory: {}", err);
		}
		else {
			info!("Assets dir created successfully.")
		}

		
		let mut ex_counter :u32 = 0;
		
		let cfg: Cfg = {
			               ctx.config
			                  .get_preprocessor(self.name())
			                  .and_then(|map| map.try_into().map_err(|err| error!("{}", err)).ok())
		               }.unwrap_or_default();

		book.for_each_mut(|item| {
			    if let BookItem::Chapter(chapter) = item {
				    let _ = process_code_blocks(chapter, &cfg, &assets_dir, &self.test_crate_dir, &mut ex_counter).map(|s| {
					                                              chapter.content = s;
					                                              trace!("chapter '{}' processed", &chapter.name);
				                                              })
				                                              .map_err(|err| {
					                                              error!("I'm blue dabodee {}", err);
				                                              });
			    }
		    });

		// could add some callbacks here to delete the examples directory - not necessary though
		Ok(book)
	}
}




fn rustviz_handler(code_string: &str, a_dir: &PathBuf, tc_dir: &PathBuf, ex_counter: u32) -> String{
	// create new example directory: src/assets/example-x/
	let example_dir_str = format!("example-{}", ex_counter);
	let example_dir = a_dir.join(example_dir_str.clone());

	if let Err(err) = fs::create_dir(example_dir.clone()){
		info!("Error creating example directory: {}", err);
	}

	// write code string to test-crate/lib.rs
	let src_path = tc_dir.join("src/lib.rs");
	match fs::write(src_path, code_string.as_bytes()) {
		Ok(_) => info!("successfully wrote code string to test-crate/lib.rs"),
		Err(e) => warn!("error when writing code string to test-crate/lib.rs {}", e)
	}

	// run rv2
	let output = Command::new("cargo")
		.arg("rv-plugin")
		.current_dir(tc_dir)
		.stdout(Stdio::piped()) // very important that we pipe any output since the plugin parses stdout as the HTML for the book
		.stderr(Stdio::piped())
		.output();

	match output {
		Ok(res) => {
			if !res.status.success() {
				// if there's an issue then just return stderr
				warn!("rustviz failed on example {}", ex_counter);
				info!("code: {}", code_string);
				let err_string = String::from(str::from_utf8(&res.stderr).unwrap());
				return format!("<div class=\"err-container\" style=\"position:relative; margin-left:-75px; margin-right:-75px;\">
				<p>{}</p></div>", err_string)
			}
		}
		Err(e) => {
			warn!("error running rv-plugin {}", e)
		}
	}

	// copy svgs to correct example directory
	let code_panel_path = tc_dir.join("src/vis_code.svg");
	let timeline_panel_path = tc_dir.join("src/vis_timeline.svg");

	match fs::rename(code_panel_path, example_dir.join("vis_code.svg")) {
		Ok(_) => {},
		Err(e) => warn!("error moving code panel {:#?}", e)
	}

	match fs::rename(timeline_panel_path, example_dir.join("vis_timeline.svg")) {
		Ok(_) => {},
		Err(e) => warn!("error moving timeline panel {:#?}", e)
	}

	// hacky fix - read height off of timeline_panel
	let height: i32 = match fs::read(example_dir.join("vis_timeline.svg")) {
		Ok(v) => {
			let timeline_str: &str = &String::from_utf8_lossy(&v);
			match timeline_str.find("height=") {
				Some(index) => {
					let start = index + "height=\"".len();
                        if let Some(end) = timeline_str[start..].find("px\"") {
                            let height_str = &timeline_str[start..start + end];
                            match height_str.parse::<i32>() {
                                Ok(value) => value,
                                Err(_) => {
                                    warn!("unable to parse height value");
                                    600 
                                }
                            }
                        } else {
                            warn!("px\" not found in the height attribute");
                            600
                        }
				}
				None => { 600 }
			}
		}
		Err(e) => {
			warn!("error reading timeline panel for height {}", e);
			600
		}
	};

	info!("successfully created visualization for example {}", ex_counter);

	let visualization_div = format!("<div class=\"flex-container vis_block\" style=\"position:relative; margin-left:-75px; margin-right:-75px; display: flex; height: {}px\">
	<object type=\"image/svg+xml\" class=\"{} code_panel\" data=\"examples/{}/vis_code.svg\"></object>
	<object type=\"image/svg+xml\" class=\"{} tl_panel\" data=\"examples/{}/vis_timeline.svg\" style=\"width: auto;\" onmouseenter=\"helpers('{}')\"></object>
	</div>", height, example_dir_str, example_dir_str, example_dir_str, example_dir_str, example_dir_str);

	visualization_div
}

fn process_code_blocks(
chapter: &mut Chapter, 
cfg: &Cfg, 
assets_dir: &PathBuf, 
tc_dir: &PathBuf, 
ex_counter: &mut u32) -> Result<String, std::fmt::Error> {

	use pulldown_cmark::{CodeBlockKind, Event, CowStr, Tag};
	use pulldown_cmark_to_cmark::cmark;

	enum State {
		None,
		Open,
		Closing,
	}

	let mut state = State::None;
	let mut buf = String::with_capacity(chapter.content.len());
	// The curly_quotes setting is left at false so that people can
	// set it in book.toml (mdBook will apply the setting when it
	// parses our output). It is important to use new_cmark_parser so
	// that we parse things like tables consistently with mdBook.
	let parser = mdbook::utils::new_cmark_parser(&chapter.content, false);
	// Clippy false-positive issue:
	// https://github.com/rust-lang/rust-clippy/issues/9211#issuecomment-1335173323
	#[allow(clippy::unnecessary_filter_map)]
	let events = parser.filter_map(|e| {
		                use State::*;
		                use CowStr::*;
		                use CodeBlockKind::*;
		                use Tag::{CodeBlock, Paragraph};


						// info!("event {:#?}", e);

		                match (&e, &mut state) {
			                (Event::Start(CodeBlock(Fenced(Borrowed(mark)))), None) if mark == &cfg.code_block => {
			                   state = Open;
			                   Some(Event::Start(Paragraph))
		                   },

		                   (Event::Text(Borrowed(text)), Open) => {
                          state = Closing;
                          let res = rustviz_handler(text, &assets_dir, tc_dir, *ex_counter);
                          *ex_counter += 1;
                          Some(Event::Html(res.into()))
		                   },

		                   (Event::End(CodeBlock(Fenced(Borrowed(mark)))), Closing) if mark == &cfg.code_block => {
                          state = None;
                          Some(Event::End(Paragraph))
		                   },
		                   _ => Some(e),
		                }
	                });
	cmark(events, &mut buf).map(|_| buf)
}
