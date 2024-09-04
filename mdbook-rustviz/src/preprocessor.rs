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
use std::fs;
use rustviz2::Rustviz;

pub struct RustvizPlugin {
	src_dir: PathBuf
}

impl RustvizPlugin {
	pub fn new(path: &Path) -> RustvizPlugin {
		
		RustvizPlugin {
			src_dir: PathBuf::from(path).join("src"),
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
				    let _ = process_code_blocks(chapter, &cfg, &assets_dir, &mut ex_counter).map(|s| {
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




fn rustviz_handler(code_string: &str, a_dir: &PathBuf,  ex_counter: u32) -> String{
	// create new example directory: src/assets/example-x/
	let example_dir_str = format!("example-{}", ex_counter);
	let example_dir = a_dir.join(example_dir_str.clone());

	if let Err(err) = fs::create_dir(example_dir.clone()){
		info!("Error creating example directory: {}", err);
	}

  match Rustviz::new(code_string) {
    Ok(rv) => {
      // write strings to file
      match fs::write(example_dir.join("vis_code.svg"), rv.code_panel_string()) {
        Ok(_) => {}
        Err(e) => warn!("error writing code panel to file {:#?}", e)
      }

      match fs::write(example_dir.join("vis_timeline.svg"), rv.timeline_panel_string()) {
        Ok(_)  => {}
        Err(e) => warn!("error writing timeline panel to file {:#?}", e)
      }

      info!("successfully created visualization for example {}", ex_counter);
      let visualization_div = format!("<div class=\"flex-container vis_block\" style=\"position:relative; margin-left:-75px; margin-right:-75px; display: flex; height: {}px\">
      <object type=\"image/svg+xml\" class=\"{} code_panel\" data=\"examples/{}/vis_code.svg\"></object>
      <object type=\"image/svg+xml\" class=\"{} tl_panel\" data=\"examples/{}/vis_timeline.svg\" style=\"width: auto;\" onmouseenter=\"helpers('{}')\"></object>
      </div>", rv.height(), example_dir_str, example_dir_str, example_dir_str, example_dir_str, example_dir_str);
    
      visualization_div
    }
    Err(e) =>{
      warn!("example {} failed with status: {:#?}", ex_counter, e);
      warn!("example code {}", code_string);
      format!("<p><b>Error generating visualization</b> {}</p>", e)
    }
  }
}

fn process_code_blocks(
chapter: &mut Chapter, 
cfg: &Cfg, 
assets_dir: &PathBuf, 
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
                          let res = rustviz_handler(text, &assets_dir, *ex_counter);
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
