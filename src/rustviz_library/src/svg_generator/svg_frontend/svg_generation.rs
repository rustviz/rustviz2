extern crate handlebars;

use crate::data::{ExtBranchData, ExternalEvent, ResourceAccessPoint_extract, Visualizable, VisualizationData, LINE_SPACE};
use crate::svg_frontend::{code_panel, timeline_panel};
use handlebars::Handlebars;
use serde::Serialize;
use std::cmp::{self, max};
use std::collections::{BTreeMap, HashMap, HashSet};
use crate::svg_frontend::templates::*;
use log::info;

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
    let old_extra_lines = extra_lines;
    let old_line_map = b_data.line_map.clone();
    let mut new_line_map: BTreeMap<usize, Vec<ExternalEvent>> = BTreeMap::new();
    let mut skippable_ev: HashSet<usize> = HashSet::new();

    // mutate actual events
    let mut i: usize = 0;
    let size: usize = b_data.e_data.len();
    while i < size {
        let (l, e) = b_data.e_data.get_mut(i).unwrap();
        let line_num = *l;
        let new_line_num = *l + extra_lines;
        if skippable_ev.contains(&e.get_id()) {
            i = i + 1;
            continue;
        }
        *l += extra_lines; // update starting line of event
        match e {
            ExternalEvent::Branch {branches, split_point, merge_point, branch_type, .. } => {
                *split_point = *l; // update split point
                for (j, branch) in branches.iter_mut().enumerate(){ // mutate branches
                    let (start, end) = branch_type.get_mut_start_end(j);
                    *start += extra_lines;
                    let b = mutate_branch_lines(branch, l_map, extra_lines);
                    extra_lines += b;
                    *end += extra_lines;
                }
                
                *merge_point += extra_lines;
            }
            _ => {
                // add extra lines if we need to
                if e.is_arrow_ev() {
                    let res = old_line_map.get(&line_num).cloned();
                    let ex = match res {
                    Some(ev) => { // if there are multiple arrow events on this line
                        for e in ev.clone() {
                            skippable_ev.insert(e.get_id()); // they all become skippable
                            let mut j = i;
                            while j < size { // need to mutate all their line numbers
                                let (l, p_e) = b_data.e_data.get_mut(j).unwrap();
                                if p_e.get_id() == e.get_id(){
                                    *l = new_line_num;
                                    break;
                                }
                                j += 1;
                            }
                        }
                        let ev_len = ev.len() - 1;
                        new_line_map.insert(new_line_num, ev); // insert new line number into new event line map
                        l_map.insert(new_line_num, ev_len);
                        ev_len
                        },
                        None => {
                            0
                        }
                    };
                    extra_lines += ex;
                }
                // no need for else since we already updated the line
            }
        }
        i += 1;
    }

    // replace line_map
    b_data.line_map = new_line_map;
    extra_lines - old_extra_lines

}

pub fn render_svg(
    annotated_src_str: &str,
    source_rs_str: &str,
    visualization_data: &mut VisualizationData,
) -> (String, String){
    info!("preprocessed events : {:#?}", visualization_data.preprocess_external_events);
    info!("ev_line_map: {:#?}", visualization_data.event_line_map);


    //-----------------------update line number for external events------------------
    // This might be the worst part of the code-base
    // extra lines need to be 'inserted' when two (or more) events that produce an arrow
    // occur on the same line (since the second+ arrow is rendered like a trapezoid)
    // However, it becomes more complicated with branches. The events inside branches need to actually be
    // mutated while the global events are just re-added. Honestly there's probably a better way to do this
    // but it turned out this way because it's built on RV1 code.
    // It's disgusting because it needs to be a single pass.
    let mut i: usize = 0;
    let size: usize = visualization_data.preprocess_external_events.len();
    let mut event_line_map_replace: BTreeMap<usize, Vec<ExternalEvent>> = BTreeMap::new();
    let mut extra_lines: usize = 0;
    let mut skippable_ev: HashSet<usize> = HashSet::new();
    let mut line_insertion_map: HashMap<usize, usize> = HashMap::new();
    while i < size {
        let (line_num, event) = visualization_data.preprocess_external_events.get_mut(i).unwrap();
        let mut branch_line = 0;
        if skippable_ev.contains(&event.get_id()) {
            i += 1;
            continue;
        }
        println!("skippable events {:#?}", skippable_ev);
        println!("line {} event {:#?}", line_num, event);
        match event {
            // need to mutate line numbers of events inside the branch
            ExternalEvent::Branch { branches, split_point, merge_point, branch_type, .. } => {
                *split_point += extra_lines;
                branch_line = *line_num + extra_lines;
                for (j, branch) in branches.iter_mut().enumerate() {
                    let (start, end) = branch_type.get_mut_start_end(j);
                    *start += extra_lines;
                    // recurse into the branch
                    let b = mutate_branch_lines(branch, &mut line_insertion_map, extra_lines);
                    extra_lines += b;
                    *end += extra_lines;
                }

                *merge_point += extra_lines;
            }
            _ => {}
        }
        // copies
        let line_num = *line_num;
        let event = event.clone();
        let final_line_num = if branch_line != 0 { branch_line } else {line_num + extra_lines};

        // append event
        // append any events that are on the same line in event line map to avoid double counting
        if event.is_arrow_ev() {
            let res = visualization_data.event_line_map.get(&line_num).cloned();
            let ex = match res {
                Some(ev) => { // if there are multiple arrow events on this line
                    for e in ev.clone() { // append all the events in the line map on the same line
                        visualization_data.append_processed_external_event(e.clone(), final_line_num);
                        skippable_ev.insert(e.get_id()); // they all become skippable 
                    }
                    let ev_len = ev.len() - 1;
                    event_line_map_replace.insert(final_line_num, ev); // insert new line number into new event line map
                    line_insertion_map.insert(final_line_num, ev_len);
                    ev_len
                },
                None => {
                    visualization_data.append_processed_external_event(event.clone(), final_line_num);
                    0
                }
            };
    
            extra_lines += ex;
        }
        else {
            visualization_data.append_processed_external_event(event.clone(), final_line_num);
        }

        i += 1;
    }

    info!("insert line map {:#?}", line_insertion_map);
    visualization_data.external_events.sort_by(|(l, _), (l1, _)| l.cmp(l1));
    info!("processed events {:#?}", visualization_data.external_events);
    visualization_data.event_line_map = event_line_map_replace;


    //------------------------sort HashMap<usize, Vec<ExternalEvent>>----------------------
    // We need to sort the event line map (the data structure that holds the events that produce arrows between timelines) by hash 
    // because when rendering the arrow we need some way to determine the direction of the arrow
    //
    // We have to sort after appending events because the order by which events are added to the processed external events matter
    // For example: say at line x: [StaticDie(a, b), StaticDie(b, c)]
    // but because of sorting the order gets switched such that at line x: [StaticDie(b, c), StaticDie(a, b)]
    // then (due to how events are appended above) b will return it's resource to c before reacquiring it from b which messes up its state
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
    info!("processed line map {:#?}", visualization_data.event_line_map);

    let svg_code_template = CODE_PANEL_TEMPLATE;
    let svg_timeline_template = TIMELINE_PANEL_TEMPLATE;
    let css_string = CSS_TEMPLATE;
    visualization_data.compute_states();

    // println!("timelines {:#?}", visualization_data.timelines);
    
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
        css: css_string.to_owned(),
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

    (final_code_svg_content, final_timeline_svg_content)
}
