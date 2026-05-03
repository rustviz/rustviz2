extern crate semver;
extern crate pulldown_cmark;
extern crate pulldown_cmark_to_cmark;

// mdbook 0.5 split its API into per-purpose crates. Preprocessors
// pull `Book`/`BookItem`/`Chapter` + the `Preprocessor` trait from
// `mdbook-preprocessor`; the markdown helpers (notably
// `new_cmark_parser`) moved into `mdbook-markdown`. Errors surface
// as `anyhow::Error` (re-exported under `errors::Error`).
//
// 0.4's `CmdPreprocessor::parse_input(stdin)` helper was retired:
// in 0.5 `CmdPreprocessor` is the *invocation*-side type (used by
// mdbook to spawn a preprocessor), not the parsing-side helper a
// preprocessor would use to read stdin. The wire format is just a
// JSON-encoded `(PreprocessorContext, Book)` tuple, so we
// deserialize that directly with serde_json.
use mdbook_preprocessor::book::{Book, BookItem, Chapter};
use mdbook_preprocessor::errors::Error;
use mdbook_preprocessor::{Preprocessor, PreprocessorContext};

use crate::Result;
use crate::cfg::Cfg;

use core::str;
use std::path::{Path, PathBuf};
use std::fs;
use rustviz2::Rustviz;

pub static HELPERS_STRING:  &'static str=r#"<script>
  const SVG = {
    'text': {
        'label': 'label',
        'functionLogo': 'label'
    },
    'path': {
        'hollow': 'timeline_mut',
        'staticref': 'static_ref_line',
        'mutref': 'mut_ref_line'
    },
    'polyline': 'arrow',
    'circle': 'event',
    'line': 'timeline_mut',
    'use': 'function_event',
    'rect': 'structBox'
};

function helpers(classname) {
    let page = document.querySelector('#page-wrapper');
    let tooltip = document.getElementById('svg_tooltip');
    if (!tooltip) {
        tooltip = document.createElement('p');
        tooltip.id = 'svg_tooltip';
        tooltip.style.cssText = "position: absolute; padding: 0.5em; font-size: 0.75em; border-radius: 8px;" +
                                "font-family: 'Trebuchet MS', Helvetica, sans-serif;" +
                                "background: rgb(70, 70, 70, 0.6); color: white; z-index: 100; display: none;";
        page.parentNode.insertBefore(tooltip, page);
    }

    displayFn(classname);
    displayTooltip(tooltip, classname);
}

function displayFn(classname) {
    let vis_num = document.getElementsByClassName(classname);
    let code_obj = vis_num[0];
    let tl_obj = vis_num[1];
    let c_svg = code_obj.contentDocument.firstChild;
    let tl_svg = tl_obj.contentDocument.firstChild
    let triggers = tl_svg.getElementsByClassName('fn-trigger');
    var functions = c_svg.getElementsByClassName('fn');
    
    for (let i = 0; i < triggers.length; i++) {
        triggers[i].addEventListener('mouseover', showFn);
        triggers[i].addEventListener('mouseout', hideFn);
    }
    
    function showFn(evt) {
        let evt_hash = evt.target.dataset.hash;

        for (let i = 0; i < functions.length; i++) {
            if (functions[i].getAttribute('hash') === evt_hash) {
                functions[i].dataset.hash = evt_hash;
            }
        }
    }

    function hideFn() {
        for (let i = 0; i < functions.length; i++) {
            functions[i].dataset.hash = 0;
        }
    }
}
function sizeToFit(object) {
    if (navigator.userAgent.indexOf("Chrome") !== -1) {
        object.addEventListener('load', function() {
            let svg_doc = object.contentDocument;
            let code_width = svg_doc.getElementById('code').getBBox().width;
            let new_width = Math.max(code_width + 30, 400);
            svg_doc.firstChild.setAttribute('width', new_width + 'px');
        }, {once: true});
    }
    else {
        if (object.contentDocument.readyState === "complete") {
            let svg_doc = object.contentDocument;
            let code_width = svg_doc.getElementById('code').getBBox().width;
            let new_width = Math.max(code_width + 30, 400);
            svg_doc.firstChild.setAttribute('width', new_width + 'px');
        }
    }
}


function displayTooltip(tooltip, classname) {
    let tl_obj = document.getElementsByClassName(classname)[1];
    let tl_svg = tl_obj.contentDocument.firstChild
    let triggers = tl_svg.getElementsByClassName('tooltip-trigger');

    var time_start = null;

    for (let i = 0; i < triggers.length; i++) {
        if (triggers[i].classList.contains('listener')) break;
        else triggers[i].classList.add('listener');

        triggers[i].addEventListener('mousemove', showTooltip);
        triggers[i].addEventListener('mouseleave', hideTooltip);
        triggers[i].addEventListener('mouseenter', insertUnderline);
    }
    
    function showTooltip(e) {
        if (!time_start) time_start = Date.now();

        let mouse = mousePos(e, tl_obj);
        tooltip.style.transform = "translate(" + mouse.x + "px, " + mouse.y + "px)";
        tooltip.style.display = "block";
        
        let text = e.currentTarget.getAttributeNS(null, "data-tooltip-text");
        tooltip.innerHTML = text;

        if (tooltip.getBoundingClientRect().right >= document.body.clientWidth) breakText(text, tooltip);
    }

    function hideTooltip(e) {
        tooltip.style.display = 'none';
        tooltip.innerHTML = '';

        let tgt = e.currentTarget;
        let e_label = (tgt.tagName === 'text') ? SVG['text'][tgt.classList[0]]
            : ((tgt.tagName === 'path') ? SVG['path'][tgt.classList[0]]
            : SVG[tgt.tagName]);

        time_start = null;

        removeUnderline(e, classname);
    }

    function insertUnderline(e) {
        let doc = document.getElementsByClassName(classname + ' code_panel')[0].contentDocument; 
        let begin = 0, end = 0;
        if (e.currentTarget.tagName === 'path') {
            let arr = e.currentTarget.getAttribute('d').split(' ');
            if (e.currentTarget.parentNode.id === 'ref_line') {
                begin = parseInt(arr[2]);
                end = parseInt(begin) + 2*parseInt(arr[5]) + parseInt(arr[7]) + 5; 
            }
            else {
                let y1 = parseInt(arr[1].split(',')[1]);
                let y2 = parseInt(arr[3].split(',')[1]);
                begin = Math.min(y1, y2);
                end = Math.max(y1, y2);
            }
        }
        else if (e.currentTarget.tagName === 'line') {
            let y1 = e.currentTarget.getAttribute('y1');
            let y2 = e.currentTarget.getAttribute('y2');
            begin = Math.min(y1, y2);
            end = Math.max(y1, y2);
        }
        else {
            let pos;
            if (e.currentTarget.tagName === 'circle') {
                begin = end = parseInt(e.currentTarget.getAttribute('cy')) + 5;
            }
            else if (e.currentTarget.tagName === 'use') {
                begin = end = parseInt(e.currentTarget.getAttribute('y')) + 5;
            }
            else if (e.currentTarget.tagName === 'polyline') {
                let arr = e.currentTarget.getAttribute('points').split(' ');
                begin = end = parseInt(arr[1]) + 5;
            }
            else {
                begin = end = parseInt(e.currentTarget.getAttribute('y'));
            }
        }

        let lines = doc.getElementById('code').children;
        let len = lines.length; 
        for (let i=0; i<len; ++i) {
            let ly = parseInt(lines[i].getAttribute('y'));
            if (ly >= begin && ly <= end) { 
                let emph = doc.createElementNS('http://www.w3.org/2000/svg', 'text');
                emph.setAttribute('class', 'code emph');
                emph.setAttribute('x', '25');
                emph.setAttribute('y', ly + 3); 
                emph.innerHTML = new Array(
                    Math.floor(lines[i].getBBox().width/8) 
                ).join('_'); 
                doc.getElementById('code').appendChild(emph);
            }
        }
    }
}


function mousePos(evt, obj) {
    let x_pos = evt.clientX + obj.getBoundingClientRect().x + 15;
    let y_pos = evt.clientY + obj.getBoundingClientRect().y + window.scrollY + 45; 

    return {
        x: Math.round(x_pos),
        y: Math.round(y_pos)
    };
}

function removeUnderline(e, classname) {
    let doc = document.getElementsByClassName(classname + ' code_panel')[0].contentDocument; 
    let arr = doc.getElementsByClassName('emph');
    for (let i = arr.length-1; i >= 0; --i) {
        arr[i].remove();
    }
}

function breakText(text, tooltip) {
    let split_text = text.split(' ');
    let words = [];
    let last = 0, span = false;
    for(const elt of split_text) {
        if (elt.startsWith('<')) {
            span = true;
            words.push(elt);
            last = words.length-1;
        }
        else if (elt.startsWith('!important')) {
            span = false;
            words[last] += elt;
        }
        else {
            if (span) {
                words[last] = words[last] + ' ' + elt;
            }
            else {
                words.push(elt);
            }
        }
    }

    tooltip.innerHTML = '';
    let left = tooltip.getBoundingClientRect().left;
    for (const word of words) {
        tooltip.innerHTML += (word + ' ');
        if (left + tooltip.clientWidth > document.body.clientWidth - 20) {
            let idx = tooltip.innerHTML.lastIndexOf(' ', tooltip.innerHTML.length-2);
            let temp = tooltip.innerHTML.substr(0, idx);
            let other = tooltip.innerHTML.substr(idx + 1);

            tooltip.innerHTML = '';
            tooltip.innerHTML += temp;
            tooltip.innerHTML += ('<br />' + other);
        }
    }
}

function toggleAll(turn_on) {
    let evt = new MouseEvent("click", {
      bubbles: true,
      cancelable: true,
      view: window
    });

    let arr = document.getElementsByClassName('toggle-button');
    for (const obj of arr) {
        if (turn_on && obj.classList.contains('fa-toggle-off')) {
            obj.dispatchEvent(evt);
        }
        else if (!turn_on && obj.classList.contains('fa-toggle-on')) {
            obj.dispatchEvent(evt);
        }
    }
}

function toggleStruct(turn_on) {
    var evt = new MouseEvent("click", {
      bubbles: true,
      cancelable: true,
      view: window
    });

    var arr = document.getElementsByClassName('non-struct');
    for (const obj of arr) {
        if (turn_on && obj.classList.contains('fa-toggle-off')) {
            obj.dispatchEvent(evt);
        }
        else if (!turn_on && obj.classList.contains('fa-toggle-on')) {
            obj.dispatchEvent(evt);
        }
    }
}</script>"#;

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

		// Wire format is JSON: `[PreprocessorContext, Book]` — see
		// mdbook-driver/src/builtin_preprocessors/cmd.rs for the
		// matching `serde_json::to_writer(writer, &(ctx, book))`.
		let (ctx, book): (PreprocessorContext, Book) =
			serde_json::from_reader(stdin())?;
		let current = Version::parse(&ctx.mdbook_version)?;
		let built = VersionReq::parse(&format!("~{}", mdbook_preprocessor::MDBOOK_VERSION))?;

		if ctx.mdbook_version != mdbook_preprocessor::MDBOOK_VERSION && !built.matches(&current) {
			warn!(
			      "The {} plugin was built against version {} of mdbook, \
				      but we're being called from version {}, so may be incompatible.",
			      self.name(),
			      mdbook_preprocessor::MDBOOK_VERSION,
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

	// 0.5 changed the return type from `bool` to `Result<bool>` so
	// preprocessors can fail cleanly during the renderer-compat
	// probe — we don't have a failure mode here, so just wrap.
	fn supports_renderer(&self, renderer: &str) -> mdbook_preprocessor::errors::Result<bool> {
		Ok(renderer != "not-supported")
	}


	fn run(&self, ctx: &PreprocessorContext, mut book: Book) -> Result<Book, Error> {
		use std::fs;
		let assets_dir = self.src_dir.join("examples");
		if let Err(err) = fs::create_dir(self.src_dir.join("examples")){
			warn!("Error creating assets directory: {}", err);
		}
		else {
			info!("Assets dir created successfully.")
		}

		
		let mut ex_counter :u32 = 0;
		
		// 0.5 dropped the `get_preprocessor` shortcut. `Config::get`
		// is now generic and serde-deserializes the value, taking a
		// dotted key path like `preprocessor.<name>`. Same end
		// result, just routed through serde directly.
		let cfg: Cfg = match ctx.config.get::<Cfg>(&format!("preprocessor.{}", self.name())) {
			Ok(Some(c)) => c,
			Ok(None) => Cfg::default(),
			Err(e) => {
				error!("{}", e);
				Cfg::default()
			}
		};

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
      let visualization_div = format!("<div class=\"flex-container vis_block\" 
      style=\"position:relative; margin-left:-75px; margin-right:-75px; display: flex; flex-direction: row; justify-content: flex-start; flex-wrap: nowrap; flex-shrink: 0; height: {}px\">
      <object type=\"image/svg+xml\" class=\"{} code_panel\" data=\"examples/{}/vis_code.svg\" style=\"flex-grow: 1\"></object>
      <object type=\"image/svg+xml\" class=\"{} tl_panel\" data=\"examples/{}/vis_timeline.svg\" style=\"width: auto; flex-grow: 0\" onmouseenter=\"helpers('{}')\"></object>
      </div>", rv.height(),example_dir_str, example_dir_str, example_dir_str, example_dir_str, example_dir_str);
    
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
chapter: &Chapter,
cfg: &Cfg,
assets_dir: &PathBuf,
// pulldown-cmark-to-cmark 11+ widened its error type from
// `std::fmt::Error` to its own enum (so it can surface things like
// "wrote bad reference link" alongside formatter failures). Switch
// the return type accordingly; the caller already prints it via
// `Display`.
ex_counter: &mut u32) -> Result<String, pulldown_cmark_to_cmark::Error> {

	// pulldown-cmark 0.10+ split end-tags into a separate `TagEnd`
	// enum (start-tags carry data like the code-block language;
	// end-tags don't need it). We don't have to re-check the
	// language at the close because our state machine already
	// remembers we're inside an `rv` block.
	use pulldown_cmark::{CodeBlockKind, Event, CowStr, Tag, TagEnd};
	use pulldown_cmark_to_cmark::cmark;

	enum State {
		None,
		Open,
		Closing,
	}

	let mut state = State::None;
	let mut buf = format!("{HELPERS_STRING}\n"); // inject the helpers script directly into the page
	// The curly_quotes setting is left at false so that people can
	// set it in book.toml (mdBook will apply the setting when it
	// parses our output). It is important to use new_cmark_parser so
	// that we parse things like tables consistently with mdBook.
	// Use mdbook-markdown's parser so we stay in lockstep with the
	// pulldown-cmark feature flags mdbook itself enables (tables,
	// footnotes, strikethrough, etc.) — drift here would parse a
	// chapter differently than the renderer that ultimately sees
	// our output. `MarkdownOptions::default()` mirrors mdbook 0.5's
	// own defaults (smart-punctuation, definition-lists, admonitions
	// all on); the prior `false` arg is gone in the 0.5 API.
	let parser = mdbook_markdown::new_cmark_parser(
		&chapter.content,
		&mdbook_markdown::MarkdownOptions::default(),
	);
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

		                   // 0.13: `Event::End` takes `TagEnd::CodeBlock`
		                   // (no language payload — the language was on
		                   // the matching `Tag::CodeBlock` start). We're
		                   // already gated by the `Closing` state set
		                   // when our `rv` opener fired, so the bare
		                   // `TagEnd::CodeBlock` is enough.
		                   (Event::End(TagEnd::CodeBlock), Closing) => {
                          state = None;
                          Some(Event::End(TagEnd::Paragraph))
		                   },
		                   _ => Some(e),
		                }
	                });
	cmark(events, &mut buf).map(|_| buf)
}
