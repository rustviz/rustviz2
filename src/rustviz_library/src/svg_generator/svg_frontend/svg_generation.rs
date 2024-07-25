extern crate handlebars;

use crate::data::{ExtBranchData, ExternalEvent, ResourceAccessPoint_extract, Visualizable, VisualizationData, LINE_SPACE};
use crate::svg_frontend::{code_panel, timeline_panel};
use handlebars::Handlebars;
use serde::Serialize;
use core::num;
use std::cmp::{self, max};
use std::collections::{BTreeMap, HashMap, HashSet};
use std::usize::MAX;

#[derive(Serialize)]
struct SvgData {
    visualization_name: String,
    css: String,
    code: String,
    diagram: String,
    tl_id: String,
    tl_width: i32,
    height: i32,
}

fn sort_branch_external_events(b_data: & mut ExtBranchData) {
    // sort first,
    for (_, event_vec) in b_data.line_map.iter_mut() {
        event_vec.sort_by(|a, b| {
            ResourceAccessPoint_extract(a)
                .1
                .hash()
                .cmp(&ResourceAccessPoint_extract(b).1.hash())
                .then(
                    ResourceAccessPoint_extract(a)
                        .0
                        .hash()
                        .cmp(&ResourceAccessPoint_extract(b).0.hash()),
                )
        });
    }
    
    // then recurse
    for (_, ev) in b_data.e_data.iter_mut() {
        match ev {
            ExternalEvent::Branch { branches, ..} => {
                for branch in branches {
                    sort_branch_external_events(branch);
                }
            }
            _ => {}
        }
    }
}

pub fn mutate_branch_lines(b_data: & mut ExtBranchData, l_map: & mut HashMap<usize, usize>, mut extra_lines: usize) -> usize {
    let old_line_map = b_data.line_map.clone();
    let mut new_line_map: BTreeMap<usize, Vec<ExternalEvent>> = BTreeMap::new();
    let mut skippable_ev: HashSet<(usize, ExternalEvent)> = HashSet::new();

    // mutate actual events
    let mut i: usize = 0;
    let size: usize = b_data.e_data.len();
    while i < size {
        let (l, e) = b_data.e_data.get_mut(i).unwrap();
        let line_num = *l;
        let new_line_num = *l + extra_lines;
        let event = e.clone();
        if skippable_ev.contains(&(line_num, event.clone())) {
            i = i + 1;
            continue;
        }
        *l += extra_lines; // update starting line of event
        match e {
            ExternalEvent::Branch {branches, split_point, merge_point, .. } => {
                *split_point = *l; // update split point
                let mut b_lines = 0;
                for branch in branches { // mutate branches 
                    b_lines = max(b_lines, mutate_branch_lines(branch, l_map, extra_lines));      
                }
                
                // update merge point
                extra_lines += b_lines;
                *merge_point += extra_lines;
            }
            _ => {
                // add extra lines if we need to
                let res = old_line_map.get(&line_num).cloned();
                let ex = match res {
                    Some(ev) => { // if there are multiple arrow events on this line
                        if *ev.get(0).unwrap() == event { // if event is the first arrow event
                            for e in ev.clone() {
                                let mut j = i;
                                while j < size {
                                    let (l, p_e) = b_data.e_data.get_mut(i).unwrap();
                                    if *l == line_num && e == *p_e {
                                        *l = new_line_num;
                                        break;
                                    }
                                    j += 1;
                                }
                                skippable_ev.insert((line_num, e)); // they all become skippable
                            }
                            let ev_len = ev.len() - 1;
                            new_line_map.insert(new_line_num, ev); // insert new line number into new event line map
                            l_map.insert(line_num, ev_len);
                            ev_len
                        }
                        else {
                            0
                        }
                    },
                    None => {
                        0
                    }
                };
                extra_lines += ex;
            }
        }
        i += 1;
    }

    // replace line_map
    b_data.line_map = new_line_map;
    extra_lines

}

pub fn render_svg(
    annotated_src_str: &str,
    source_rs_str: &str,
    visualization_data: &mut VisualizationData,
) -> (String, String){
    //------------------------sort HashMap<usize, Vec<ExternalEvent>>----------------------
    for (_, event_vec) in &mut visualization_data.event_line_map {
        event_vec.sort_by(|a, b| {
            ResourceAccessPoint_extract(a)
                .1
                .hash()
                .cmp(&ResourceAccessPoint_extract(b).1.hash())
                .then(
                    ResourceAccessPoint_extract(a)
                        .0
                        .hash()
                        .cmp(&ResourceAccessPoint_extract(b).0.hash()),
                )
        });
    }
    // sort all the line maps in the branch events
    for (_, e) in visualization_data.preprocess_external_events.iter_mut() {
        match e {
            ExternalEvent::Branch { branches, .. } => {
                for b in branches.iter_mut() {
                    sort_branch_external_events(b);
                }
            }
            _ => {}
        }
    }

    println!("preprocessed events : {:#?}", visualization_data.preprocess_external_events);
    println!("ev_line_map: {:#?}", visualization_data.event_line_map);
    //-----------------------update line number for external events------------------
    // single pass 
    let mut i: usize = 0;
    let size: usize = visualization_data.preprocess_external_events.len();
    let mut event_line_map_replace: BTreeMap<usize, Vec<ExternalEvent>> = BTreeMap::new();
    let mut extra_lines: usize = 0;
    let mut skippable_ev: HashSet<(usize, ExternalEvent)> = HashSet::new();
    let mut line_insertion_map: HashMap<usize, usize> = HashMap::new();
    while i < size {
        let (line_num, event) = visualization_data.preprocess_external_events.get_mut(i).unwrap();
        let mut branch_line = 0;
        if skippable_ev.contains(&(*line_num, event.clone())) {
            i += 1;
            continue;
        }
        match event {
            // need to mutate line numbers of events inside the branch
            ExternalEvent::Branch { branches, split_point, merge_point, .. } => {
                let mut extra_branch_lines: usize = 0;
                *split_point += extra_lines;
                branch_line = *line_num + extra_lines;
                for branch in branches {
                    extra_branch_lines = max(extra_branch_lines, mutate_branch_lines(branch, & mut line_insertion_map, extra_lines));
                }
                // upate extra lines with total lines computed from traversing the branch
                *merge_point += extra_lines + extra_branch_lines;
                extra_lines += extra_branch_lines;
            }
            _ => {}
        }
        // copies
        let line_num = *line_num;
        let event = event.clone();
        let final_line_num = if branch_line != 0 { branch_line } else {line_num + extra_lines};

        // append event
        // append any events that are on the same line in event line map to avoid double counting
        let res = visualization_data.event_line_map.get(&line_num).cloned();
        let ex = match res {
            Some(ev) => { // if there are multiple arrow events on this line
                if *ev.get(0).unwrap() == event { // if event is the first arrow event
                    for e in ev.clone() { // append all the events in the line map on the same line
                        visualization_data.append_processed_external_event(e.clone(), final_line_num);
                        skippable_ev.insert((line_num, e)); // they all become skippable 
                    }
                    let ev_len = ev.len() - 1;
                    event_line_map_replace.insert(final_line_num, ev); // insert new line number into new event line map
                    line_insertion_map.insert(line_num, ev_len);
                    ev_len
                }
                else {
                    visualization_data.append_processed_external_event(event.clone(), final_line_num);
                    0
                }
            },
            None => {
                visualization_data.append_processed_external_event(event.clone(), final_line_num);
                0
            }
        };

        extra_lines += ex;

        i += 1;
    }



    // for (line_number, event) in visualization_data.preprocess_external_events.clone() {
    //     let mut extra_line: usize = 0;
    //     for (info_line_number, event_vec) in &visualization_data.event_line_map {
    //         if info_line_number < &line_number {
    //             extra_line += event_vec.len() - 1;
    //         } else {
    //             break;
    //         }
    //     }
    //     let final_line_num = line_number.clone() + extra_line;
    //     visualization_data.append_processed_external_event(event, final_line_num, & mut None);
    // }
    visualization_data.external_events.sort_by(|(l, _), (l1, _)| l.cmp(l1));
    println!("processed events {:#?}", visualization_data.external_events);
    //-----------------------update event_line_map line number------------------
    // let mut event_line_map_replace: BTreeMap<usize, Vec<ExternalEvent>> = BTreeMap::new();
    // let mut extra_line_sum = 0;
    // for (line_number, event_vec) in &visualization_data.event_line_map {
    //     event_line_map_replace.insert(line_number + extra_line_sum, event_vec.clone());
    //     extra_line_sum += event_vec.len() - 1;
    // }
    visualization_data.event_line_map = event_line_map_replace;
    println!("processed line map {:#?}", visualization_data.event_line_map);

    let svg_code_template = String::from(
    "<svg width=\"{{tl_width}}px\" height=\"{{height}}px\" xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\">

      <desc>{{ visualization_name }}</desc>

      <defs>
          <style type=\"text/css\">
          <![CDATA[
          {{ css }}
          ]]>
          </style>
      </defs>

      <g>
          <text id=\"caption\" x=\"30\" y=\"30\">Hover over timeline events (dots), states (vertical lines),</text>
          <text id=\"caption\" x=\"30\" y=\"50\">and actions (arrows) for extra information.</text>
      </g>

      {{ code }}

      </svg>");
  // utils::read_file_to_string(code_template_path.as_os_str())
  //       .unwrap_or("Reading template.svg failed.".to_owned());
    let svg_timeline_template = String::from("
    <svg width=\"{{tl_width}}px\" height=\"{{height}}px\" 
        xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" 
        id=\"{{tl_id}}\">

    <desc>{{ visualization_name }}</desc>

    <defs>
        <style type=\"text/css\">
        <![CDATA[
        {{ css }}
        
        text {
            user-select: none;
            -webkit-user-select: none;
            -moz-user-select: none;
            -ms-user-select: none;
        }
        ]]>
        </style>
        <!-- used when pass to function by ref -->
        <g id=\"functionDot\">
             <circle cx=\"0\" cy=\"0\" r=\"5\" fill=\"transparent\"/>
             <text class=\"functionIcon\" dx=\"-3.5\" dy=\"0\" fill=\"#6e6b5e\">f</text>
        </g>
        <marker id=\"arrowHead\" viewBox=\"0 0 10 10\"
            refX=\"0\" refY=\"4\"
            markerUnits=\"strokeWidth\"
            markerWidth=\"3px\" markerHeight=\"3px\"
            orient=\"auto\" fill=\"gray\">
            <path d=\"M 0 0 L 8.5 4 L 0 8 z\" fill=\"inherit\"/>
        </marker>
        <!-- glow highlight filter -->
        <filter id=\"glow\" x=\"-5000%\" y=\"-5000%\" width=\"10000%\" height=\"10000%\" filterUnits=\"userSpaceOnUse\">
            <feComposite in=\"flood\" result=\"mask\" in2=\"SourceGraphic\" operator=\"in\"></feComposite>
            <feGaussianBlur stdDeviation=\"2\" result=\"coloredBlur\"/>
            <feMerge>
                <feMergeNode in=\"coloredBlur\"></feMergeNode>
                <feMergeNode in=\"coloredBlur\"></feMergeNode>
                <feMergeNode in=\"coloredBlur\"></feMergeNode>
                <feMergeNode in=\"SourceGraphic\"></feMergeNode>
            </feMerge>
            <!-- increase brightness -->
            <feComponentTransfer>
                <feFuncR type=\"linear\" slope=\"2\"/>
                <feFuncG type=\"linear\" slope=\"2\"/>
                <feFuncB type=\"linear\" slope=\"2\"/>
            </feComponentTransfer>
        </filter>
    </defs>

    {{ diagram }}

    </svg>");
        // utils::read_file_to_string(svg_template_path.as_os_str())
        //     .unwrap_or("Reading template.svg failed.".to_owned());
    let css_string = String::from("/* general setup */
    :root {
        --bg-color:#f1f1f1;
        --text-color: #6e6b5e;
    }
    
    svg {
        background-color: var(--bg-color);
    }
    
    text {
        vertical-align: baseline;
        text-anchor: start;
    }
    
    #heading {
        font-size: 24px;
        font-weight: bold;
    }
    
    #caption {
        font-size: 0.875em;
        font-family: \"Open Sans\", sans-serif;
        font-style: italic;
    }
    
    /* code related styling */
    text.code {
        fill: #6e6b5e;
        white-space: pre;
        font-family: \"Source Code Pro\", Consolas, \"Ubuntu Mono\", Menlo, \"DejaVu Sans Mono\", monospace, monospace !important;
        font-size: 0.875em;
    }
    
    text.label {
        font-family: \"Source Code Pro\", Consolas, \"Ubuntu Mono\", Menlo, \"DejaVu Sans Mono\", monospace, monospace !important;
        font-size: 0.875em;
    }
    
    /* timeline/event interaction styling */
    .solid {
        stroke-width: 5px;
    }
    
    .hollow {
        stroke-width: 1.5;
    }
    
    .dotted {
        stroke-width: 5px;
        stroke-dasharray: \"2 1\";
    }
    
    .extend {
        stroke-width: 1px;
        stroke-dasharray: \"2 1\";
    }
    
    .functionIcon {
        paint-order: stroke;
        stroke-width: 3px;
        fill: var(--bg-color);
        font-size: 20px;
        font-family: times;
        font-weight: lighter;
        dominant-baseline: central;
        text-anchor: start;
        font-style: italic;
    }
    
    .functionLogo {
        font-size: 20px;
        font-style: italic;
        paint-order: stroke;
        stroke-width: 3px;
        fill: var(--bg-color) !important;
    }
    
    /* flex related styling */
    .flex-container {
        display: flex;
        flex-direction: row;
        justify-content: flex-start;
        flex-wrap: nowrap;
        flex-shrink: 0;
    }
    
    object.tl_panel {
        flex-grow: 1;
    }
    
    object.code_panel {
        flex-grow: 0;
    }
    
    .tooltip-trigger {
        cursor: default;
    }
    
    .tooltip-trigger:hover{
        filter: url(#glow);
    }
    
    /* hash based styling */
    [data-hash=\"0\"] {
        fill: #6e6b5e;
    }
    
    [data-hash=\"1\"] {
        fill: #1893ff;
        stroke: #1893ff;
    }
    
    [data-hash=\"2\"] {
        fill: #ff7f50;
        stroke: #ff7f50;
    }
    
    [data-hash=\"3\"] {
        fill: #8635ff;
        stroke: #8635ff;
    }
    
    [data-hash=\"4\"] {
        fill: #dc143c;
        stroke: #dc143c;
    }
    
    [data-hash=\"5\"] {
        fill: #0a810a;
        stroke: #0a810a;
    }
    
    [data-hash=\"6\"] {
        fill: #008080;
        stroke: #008080;
    }
    
    [data-hash=\"7\"] {
        fill: #ff6cce;
        stroke: #ff6cce;
    }
    
    [data-hash=\"8\"] {
        fill: #00d6fc;
        stroke: #00d6fc;
    }
    
    [data-hash=\"9\"] {
        fill: #b99f35;
        stroke: #b99f35;
    }");

    println!("timelines {:#?}", visualization_data.timelines);
    
    let a_lines = annotated_src_str.lines();
    let s_lines = source_rs_str.lines();

    //println!("bruh {:#?}", visualization_data.timelines.get(&1).unwrap().history);
    let mut handlebars = Handlebars::new();
    // We want to preserve the inputs `as is`, and want to make no changes based on html escape.
    handlebars.register_escape_fn(handlebars::no_escape);
    let code_svg_template = svg_code_template;
    let tl_svg_template = svg_timeline_template;
    // register the template. The template string will be verified and compiled.
    assert!(handlebars
        .register_template_string("code_svg_template", code_svg_template)
        .is_ok());
    assert!(handlebars
        .register_template_string("timeline_svg_template", tl_svg_template)
        .is_ok());

    // data for code panel
    let mut max_x_space: i64 = 0;
    let (output, line_of_code) =
            code_panel::render_code_panel(a_lines, s_lines, &mut max_x_space, &visualization_data.event_line_map, &line_insertion_map);
    let code_panel_string = output;
    let num_lines = line_of_code;

    // data for tl panel
    let (timeline_panel_string, max_width) = timeline_panel::render_timeline_panel(visualization_data);
    
    let mut svg_data = SvgData {
        visualization_name: "vis".to_owned(),
        css: css_string,
        code: code_panel_string,
        diagram: timeline_panel_string,
        tl_id: "tl_".to_owned() + "vis",
        tl_width: 400,
        height: (num_lines * LINE_SPACE as i32 + 80) + 50,
    };

    let final_code_svg_content = handlebars.render("code_svg_template", &svg_data).unwrap();
    svg_data.tl_width = cmp::max(max_width, 200);
    let final_timeline_svg_content = handlebars
        .render("timeline_svg_template", &svg_data)
        .unwrap();

    // write to file
    // utils::create_and_write_to_file(&final_code_svg_content, code_image_file_path); // write svg code
    // utils::create_and_write_to_file(&final_timeline_svg_content, timeline_image_file_path); // write svg timeline
    // println!("{}", final_code_svg_content);
    (final_code_svg_content, final_timeline_svg_content)
}
