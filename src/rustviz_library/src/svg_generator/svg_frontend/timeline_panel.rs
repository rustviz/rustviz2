extern crate handlebars;

use crate::data::{branch_state_converter, convert_back, string_of_branch, string_of_external_event, BranchData, BranchType, Event, ExternalEvent, LineState, ResourceAccessPoint, ResourceAccessPoint_extract, ResourceTy, State, StructsInfo, Timeline, Visualizable, VisualizationData, LINE_SPACE};
use crate::svg_frontend::line_styles::{RefDataLine, RefValueLine, OwnerLine};
use handlebars::Handlebars;
use std::collections::{BTreeMap, HashMap, HashSet, VecDeque};
use serde::Serialize;
use std::cmp::{self, max, min};

// set style for code string
static SPAN_BEGIN : &'static str = "&lt;span style=&quot;font-family: 'Source Code Pro', Consolas, 'Ubuntu Mono', Menlo, 'DejaVu Sans Mono', monospace, monospace !important;&quot;&gt;";
static SPAN_END : &'static str = "&lt;/span&gt;";
static BRANCH_WEIGHT: i64 = 25;
#[derive(Debug, Clone)]
pub struct TimelineColumnData {
    pub name: String,
    pub x_val: i64,
    pub title: String,
    pub is_ref: bool,
    pub is_struct_group: bool,
    pub is_member: bool,
    pub owner: u64
}

#[derive(Serialize)]
struct TimelinePanelData {
    labels: String,
    dots: String,
    timelines: String,
    ref_line: String,
    arrows: String
}

#[derive(Serialize)]
struct ResourceAccessPointLabelData {
    x_val: i64,
    hash: String,
    name: String,
    title: String
}

#[derive(Serialize)]
struct EventDotData {
    hash: u64,
    dot_x: i64,
    dot_y: i64,
    title: String,
}

#[derive(Serialize)]
struct FunctionDotData {
    hash: u64,
    x: i64,
    y: i64,
    title: String
}

#[derive(Serialize)]
struct ArrowData {
    coordinates: Vec<(f64, f64)>,
    coordinates_hbs: String,
    title: String
}

#[derive(Serialize)]
struct FunctionLogoData {
    hash: u64,
    x: i64,
    y: i64,
    title: String
}

#[derive(Serialize)]
struct BoxData {
    name: u64,
    hash: u64,
    x: i64,
    y: i64,
    w: i64,
    h: i64,
    title: String
}

#[derive(Serialize)]
struct StructTimelinePanelData {
    struct_name: String,
    struct_members: String
}

#[derive(Serialize, Clone)]
struct VerticalLineData {
    line_class: String,
    hash: u64,
    x1: f64,
    x2: f64,
    y1: i64,
    y2: i64,
    title: String,
    opacity: f64
}

#[derive(Serialize, Clone, Debug)]
struct HollowLineData {
    line_class: String,
    hash: u64,
    x1: f64,
    y1: f64,
    x2: f64,
    y2: f64,
    x3: f64,
    y3: f64,
    x4: f64,
    y4: f64,
    title: String,
    opacity: f64
}

#[derive(Serialize, Clone)]
struct RefLineData {
    line_class: String,
    hash: u64,
    x1: i64,
    x2: i64,
    y1: i64,
    y2: i64,
    dx: i64,
    dy: i64,
    v: i64,
    title: String
}

#[derive(Serialize)]
struct OutputStringData {
    struct_name: String,
    struct_instance: String,
    struct_members: String
}

pub fn render_timeline_panel(visualization_data : & mut VisualizationData) -> (String, i32) {
    /* Template creation */
    let mut registry = Handlebars::new();
    prepare_registry(&mut registry);

    let mut structs_info = StructsInfo { structs: Vec::new() };

    // hash -> TimelineColumnData
    let (resource_owners_layout, width) = compute_column_layout(visualization_data, &mut structs_info);

    let mut output : BTreeMap<i64, (TimelinePanelData, TimelinePanelData)> = BTreeMap::new();
    output.insert(-1, (TimelinePanelData{ labels: String::new(), dots: String::new(), timelines: String::new(), 
        ref_line: String::new(), arrows: String::new() }, TimelinePanelData{ labels: String::new(), dots: String::new(), 
            timelines: String::new(), ref_line: String::new(), arrows: String::new() })); 
    // Note: key {-1} = non-struct timelines
    
    // render resource owner labels
    render_timelines(&mut output, visualization_data, &resource_owners_layout, &registry); // vertical bars
    render_labels_string(&mut output, &resource_owners_layout, &registry); // headers
    render_dots_string(&mut output, visualization_data, &resource_owners_layout, &registry); // dot events
    render_ref_line(&mut output, visualization_data, &resource_owners_layout, &registry); // reference lines
    render_arrows_string_external_events_version(&mut output, visualization_data, &resource_owners_layout, &registry); // arrows
    render_struct_box(&mut output, &structs_info, &registry); // struct box

    let mut output_string : String = String::new();
    for (hash, (timelinepanel, member_timelinepanel)) in output{
        let struct_name;
        if hash == -1 {
            struct_name = String::from("non-struct");
        } else {
            struct_name = match visualization_data.get_name_from_hash(&(hash as u64)) {
                Some(_name) => _name,
                None => panic!("no matching resource owner for hash {}", hash),
            };
        }
        let timelinepanel_string = registry.render("timeline_panel_template", &timelinepanel).unwrap();
        let member_timelinepanel_string = registry.render("timeline_panel_template", &member_timelinepanel).unwrap();
        let output_data = OutputStringData{
            struct_name: struct_name,
            struct_instance: timelinepanel_string,
            struct_members: member_timelinepanel_string,
        };
        output_string.push_str(&registry.render("struct_template", &output_data).unwrap());
    }

    (output_string, width)

}

fn prepare_registry(registry: &mut Handlebars) {
    // We want to preserve the inputs `as is`, and want to make no changes based on html escape.
    registry.register_escape_fn(handlebars::no_escape);

    let timeline_panel_template =
        "    <g id=\"labels\">\n{{ labels }}    </g>\n\n    \
        <g id=\"timelines\">\n{{ timelines }}    </g>\n\n    \
        <g id=\"ref_line\">\n{{ ref_line }}    </g>\n\n    \
        <g id=\"events\">\n{{ dots }}    </g>\n\n    \
        <g id=\"arrows\">\n{{ arrows }}    </g>";

    let struct_template  = 
        "    <g id=\"{{struct_name}}\">\n\
             \t<g class=\"struct_instance\">\n{{ struct_instance }}</g>\n\
             \t<g class=\"struct_members\">\n{{ struct_members }}</g>\n\
             \t</g>\n    ";
    
    let label_template =
        "        <text x=\"{{x_val}}\" y=\"70\" style=\"text-anchor:middle\" data-hash=\"{{hash}}\" class=\"label tooltip-trigger\" data-tooltip-text=\"{{title}}\">{{name}}</text>\n";
    let dot_template =
        "        <circle cx=\"{{dot_x}}\" cy=\"{{dot_y}}\" r=\"5\" data-hash=\"{{hash}}\" class=\"tooltip-trigger\" data-tooltip-text=\"{{title}}\"/>\n";
    let function_dot_template =    
        "        <use xlink:href=\"#functionDot\" data-hash=\"{{hash}}\" x=\"{{x}}\" y=\"{{y}}\" class=\"tooltip-trigger\" data-tooltip-text=\"{{title}}\"/>\n";
    let function_logo_template =
        "        <text x=\"{{x}}\" y=\"{{y}}\" data-hash=\"{{hash}}\" class=\"functionLogo tooltip-trigger fn-trigger\" data-tooltip-text=\"{{title}}\">f</text>\n";
    let arrow_template =
        "        <polyline stroke-width=\"5px\" stroke=\"gray\" points=\"{{coordinates_hbs}}\" marker-end=\"url(#arrowHead)\" class=\"tooltip-trigger\" data-tooltip-text=\"{{title}}\" style=\"fill: none;\"/> \n";
    let vertical_line_template =
        "        <line data-hash=\"{{hash}}\" class=\"{{line_class}} tooltip-trigger\" x1=\"{{x1}}\" x2=\"{{x2}}\" y1=\"{{y1}}\" y2=\"{{y2}}\" data-tooltip-text=\"{{title}}\" style=\"opacity: {{opacity}};\"/>\n";
    let hollow_line_template =
        "        <path data-hash=\"{{hash}}\" class=\"hollow tooltip-trigger\" style=\"fill:transparent;\" d=\"M {{x1}},{{y1}} V {{y2}} h 3.5 V {{y1}} h -3.5\" data-tooltip-text=\"{{title}}\"/>\n";
    let new_hollow_line_template = "<path 
        data-hash=\"{{hash}}\"
        class=\"hollow tooltip-trigger\"
        style=\"fill:transparent; stroke-opacity: {{opacity}};\"
        d=\"M {{x1}},{{y1}} L {{x2}},{{y2}} L {{x3}},{{y3}} L {{x4}},{{y4}} Z\"
        data-tooltip-text=\"{{title}}\"/>";
    let solid_ref_line_template =
        "        <path data-hash=\"{{hash}}\" class=\"mutref {{line_class}} tooltip-trigger\" style=\"fill:transparent; stroke-width: 2px !important;\" d=\"M {{x1}} {{y1}} l {{dx}} {{dy}} v {{v}} l -{{dx}} {{dy}}\" data-tooltip-text=\"{{title}}\"/>\n";
    let hollow_ref_line_template =
        "        <path data-hash=\"{{hash}}\" class=\"staticref tooltip-trigger\" style=\"fill: transparent;\" stroke-width=\"2px\" stroke-dasharray=\"3\" d=\"M {{x1}} {{y1}} l {{dx}} {{dy}} v {{v}} l -{{dx}} {{dy}}\" data-tooltip-text=\"{{title}}\"/>\n";
    let box_template =
        "        <rect id=\"{{name}}\" x=\"{{x}}\" y=\"{{y}}\" rx=\"20\" ry=\"20\" width=\"{{w}}\" height=\"{{h}}\" style=\"fill:white;stroke:black;stroke-width:3;opacity:0.1\" pointer-events=\"none\" />\n";

    assert!(
        registry.register_template_string("new_hollow_line_template", new_hollow_line_template).is_ok()
    );
    assert!(
        registry.register_template_string("struct_template", struct_template).is_ok()
    );
    assert!(
        registry.register_template_string("timeline_panel_template", timeline_panel_template).is_ok()
    );
    assert!(
        registry.register_template_string("label_template", label_template).is_ok()
    );
    assert!(
        registry.register_template_string("dot_template", dot_template).is_ok()
    );
    assert!(
        registry.register_template_string("arrow_template", arrow_template).is_ok()
    );
    assert!(
        registry.register_template_string("vertical_line_template", vertical_line_template).is_ok()
    );
    assert!(
        registry.register_template_string("function_dot_template", function_dot_template).is_ok()
    );
    assert!(
        registry.register_template_string("function_logo_template", function_logo_template).is_ok()
    );
    assert!(
        registry.register_template_string("hollow_line_template", hollow_line_template).is_ok()
    );
    assert!(
        registry.register_template_string("solid_ref_line_template", solid_ref_line_template).is_ok()
    );
    assert!(
        registry.register_template_string("hollow_ref_line_template", hollow_ref_line_template).is_ok()
    );
    assert!(
        registry.register_template_string("box_template", box_template).is_ok()
    );
}

 
// computes a width coefficient for a resource, considering branches
fn compute_width(events: & mut Vec<(usize, Event)>) -> usize {
    let mut max_width = 0;
    for (_, ev) in events {
        match ev {
            Event::Branch { branch_history, ..} => {
                let mut b_width = 0;
                // DFS to calculate width of each branch
                for branch in branch_history.iter_mut() {
                    let branch_w = compute_width(& mut branch.e_data);
                    branch.width = branch_w; // store branch width for later DOES NOT INCLUDE PADDING BETWEEN BRANCHES AT SAME LEVEL
                    b_width += branch_w;
                }
                let padding = (branch_history.len() - 1) * 2; // TODO: update for match expr
                b_width += padding;
                max_width = max(b_width, max_width);
            }
            _ => {}
        }
    }
    max_width
}

fn update_timeline_data(events: & mut Vec<(usize, Event)>, parent_data: &TimelineColumnData) {
    for (_, ev) in events {
        match ev {
            Event::Branch { branch_history, ty, ..} => {
                for branch in & mut *branch_history {
                    // copy the parent data
                    branch.t_data = parent_data.clone();
                }
                // update the xvalue based on width
                let mut parent_branch_data: Vec<TimelineColumnData> = Vec::new();
                // println!("parent_data x_val: {:#?}", parent_data.x_val);
                match ty {
                    BranchType::Match(..) => {
                        let halfway = branch_history.len() / 2;
                        let mut running_x = parent_data.x_val;
                        for i in (0..halfway).rev() {
                            let b_data = branch_history.get_mut(i).unwrap();
                            let b_width = b_data.width;
                            let padding = if i == halfway - 1 {1} else {0};
                            let left_side_coefficient = -1 * (b_width + padding) as i64;
                            // println!("left side {}, coefficient {}", ty.string_of_branch(i), left_side_coefficient);
                            let x = left_side_coefficient * BRANCH_WEIGHT;
                            running_x += x;
                            b_data.t_data.x_val = running_x;
                            running_x -= 2 * BRANCH_WEIGHT;
                        }
                        for i in 0..halfway {
                          let b_data = branch_history.get(i).unwrap();
                          parent_branch_data.push(b_data.t_data.clone());
                        }

                        running_x = parent_data.x_val;
                        for i in halfway..branch_history.len() {
                            let b_data = branch_history.get_mut(i).unwrap();
                            let b_width = b_data.width;
                            let padding = if i == halfway {1} else {0};
                            let right_side_coefficient = (b_width + padding) as i64;
                            // println!("right side {}, coefficient {}", ty.string_of_branch(i), right_side_coefficient);
                            let x = right_side_coefficient * BRANCH_WEIGHT;
                            running_x += x;
                            b_data.t_data.x_val = running_x;
                            running_x += 2 * BRANCH_WEIGHT;
                            parent_branch_data.push(b_data.t_data.clone());
                        }
                    }
                    _ => {
                        let if_bw = branch_history.get(0).unwrap().width;
                        let else_bw = branch_history.get(1).unwrap().width;
                        let if_offset_coefficient: i64 = -1 * ((if_bw + 1) as i64); // + 1 for padding between branches
                        let else_offset_coefficient: i64 = (else_bw + 1) as i64;

                        branch_history.get_mut(0).unwrap().t_data.x_val = parent_data.x_val + (if_offset_coefficient * BRANCH_WEIGHT);
                        branch_history.get_mut(1).unwrap().t_data.x_val = parent_data.x_val + (else_offset_coefficient * BRANCH_WEIGHT);
                        parent_branch_data.push(branch_history.get(0).unwrap().t_data.clone());
                        parent_branch_data.push(branch_history.get(1).unwrap().t_data.clone());
                    }
                }

                // recurse
                for (i, branch) in branch_history.iter_mut().enumerate() {
                    update_timeline_data(&mut branch.e_data, &parent_branch_data[i])
                    
                }

            }
            _ => {}
        }
    }
}

// Returns: a binary tree map from the hash of the ResourceOwner to its Column information
fn compute_column_layout<'a>(
    visualization_data: &'a mut VisualizationData,
    structs_info: &'a mut StructsInfo,
) -> (BTreeMap< u64, TimelineColumnData>, i32) {
    let mut resource_owners_layout = BTreeMap::new();
    let mut x = 0; // Right-most Column x-offset.
    let mut owner = -1;
    let mut owner_x = 0;
    let mut last_x = 0;
    let mut w_map: HashMap<u64, i64> = HashMap::new();

    // get all the widths of each timeline
    for (h, timeline) in visualization_data.timelines.iter_mut() {
        let width = compute_width(&mut timeline.history);
        w_map.insert(*h, width as i64);
    }

    println!("w_map {:#?}", w_map);

    
    for (hash, timeline) in visualization_data.timelines.iter() {

        // only put variable in the column layout
        match timeline.resource_access_point {
            ResourceAccessPoint::Function(_) => {
                /* do nothing */
            },
            ResourceAccessPoint::Owner(_) | ResourceAccessPoint::Struct(_) | ResourceAccessPoint::MutRef(_) | ResourceAccessPoint::StaticRef(_) =>
            {
                let name = match visualization_data.get_name_from_hash(hash) {
                    Some(_name) => _name,
                    None => panic!("no matching resource owner for hash {}", hash),
                };
                let mut x_space = cmp::max(70, (&(name.len() as i64)-1)*13);
                let branch_width = *w_map.get(hash).unwrap() * BRANCH_WEIGHT;
                let branch_offset = branch_width / 2;
                x = x + x_space + branch_offset;
                let title = match visualization_data.is_mut(hash) {
                    true => String::from("mutable"),
                    false => String::from("immutable"),
                };
                let mut ref_bool = false;

                // render reference label
                if timeline.resource_access_point.is_ref() {
                    let temp_name = name.clone() + "|*" + &name; // used for calculating x_space
                    x = x - x_space; // reset
                    x_space = cmp::max(90, (&(temp_name.len() as i64)-1)*7); // new x_space
                    x = x + x_space; // new x pos
                    ref_bool = true; // hover msg displays only "s" rather than "s|*s"
                }

                let styled_name = SPAN_BEGIN.to_string() + &name + SPAN_END;
                
                if (owner == -1) && timeline.resource_access_point.is_struct_group() && !timeline.resource_access_point.is_member() {
                    owner = timeline.resource_access_point.hash().clone() as i64;
                    owner_x = x;
                } else if (owner != -1) && timeline.resource_access_point.is_struct_group() && timeline.resource_access_point.is_member() {
                    last_x = x;
                } else if (owner != -1) && !timeline.resource_access_point.is_struct_group() {
                    structs_info.structs.push((owner, owner_x, last_x));
                    owner = -1;
                    owner_x = 0;
                    last_x = 0;
                }

                resource_owners_layout.insert(*hash, TimelineColumnData
                    { 
                        name: name.clone(), 
                        x_val: x, 
                        title: styled_name.clone() + ", " + &title,
                        is_ref: ref_bool,
                        is_struct_group: timeline.resource_access_point.is_struct_group(),
                        is_member: timeline.resource_access_point.is_member(),
                        owner: timeline.resource_access_point.get_owner(),
                    });
                x += branch_offset;
            }
        }
    }
    for (h, timeline) in visualization_data.timelines.iter_mut() {

        match timeline.resource_access_point {
            ResourceAccessPoint::Function(_) => {},
            _ => {
                let root_data = resource_owners_layout.get(h).unwrap();
                update_timeline_data(& mut timeline.history, root_data);
            }
        }
    }

    (resource_owners_layout, (x as i32)+100)
}

fn render_labels_string(
    output: &mut BTreeMap<i64, (TimelinePanelData, TimelinePanelData)>,
    resource_owners_layout: &BTreeMap<u64, TimelineColumnData>,
    registry: &Handlebars
) {
    for (hash, column_data) in resource_owners_layout.iter() {
        let mut data = ResourceAccessPointLabelData {
            x_val: column_data.x_val,
            hash: hash.to_string(),
            name: column_data.name.clone(),
            title: column_data.title.clone(),
        };

        if column_data.is_ref {
            let new_name = column_data.name.to_owned() + "<tspan stroke=\"none\">|</tspan>*" + &column_data.name;
            data.name = new_name;
        }

        // push to individual timelines
        if column_data.is_struct_group {
            if column_data.is_member {
                output.get_mut(&(column_data.owner.to_owned() as i64)).unwrap().1.labels.push_str(&registry.render("label_template", &data).unwrap());
            } else {
                output.get_mut(&(column_data.owner.to_owned() as i64)).unwrap().0.labels.push_str(&registry.render("label_template", &data).unwrap());
            }
        }
        else {
            output.get_mut(&-1).unwrap().0.labels.push_str(&registry.render("label_template", &data).unwrap());
        }
    }
}

fn append_dot(
    dot_data: &EventDotData,
    output: &mut BTreeMap<i64, (TimelinePanelData, TimelinePanelData)>,
    timeline_data: &TimelineColumnData,
    registry: &Handlebars
) {
    let column = timeline_data;
    if column.is_struct_group {
        if column.is_member {
            output.get_mut(&(column.owner.to_owned() as i64)).unwrap().1.dots.push_str(&registry.render("dot_template", &dot_data).unwrap());
        } else {
            output.get_mut(&(column.owner.to_owned() as i64)).unwrap().0.dots.push_str(&registry.render("dot_template", &dot_data).unwrap());
        }
    }
    else {
        output.get_mut(&-1).unwrap().0.dots.push_str(&registry.render("dot_template", &dot_data).unwrap());
    }
}

fn render_dot(
    hash: &u64,
    history: &Vec<(usize, Event)>,
    timeline_data: &TimelineColumnData,
    output: &mut BTreeMap<i64, (TimelinePanelData, TimelinePanelData)>,
    visualization_data: &VisualizationData,
    registry: &Handlebars,
    resource_hold: bool
) {
    for (line_number, event) in history.iter() {
        //matching the event
        match event {
            Event::RefDie { .. } => {
                continue;
            }
            Event::Branch { is, branch_history, ty, split_point, merge_point, id } => { 
                // first append split dot
                let b_data = EventDotData {
                    hash: *hash as u64,
                    dot_x: timeline_data.x_val,
                    dot_y: get_y_axis_pos(*line_number),
                    title: event.print_message_with_name(& mut is.real_name())
                };
                append_dot(&b_data, output, timeline_data, registry);

                // render dots for each of the branches
                for (i, branch) in branch_history.iter().enumerate() {
                    // render a dot at the beginning of the timeline
                    let split_data = EventDotData {
                        hash: *hash as u64,
                        dot_x: branch.t_data.x_val,
                        dot_y: get_y_axis_pos(*split_point + 1),
                        title: string_of_branch(ty, i)
                    };
                    append_dot(&split_data, output, &branch.t_data, registry);

                    render_dot(hash, &branch.e_data, &branch.t_data, output, visualization_data, registry, false);

                    // render a dot at the end of the timeline
                    let merge_data = EventDotData {
                        hash: *hash as u64,
                        dot_x: branch.t_data.x_val,
                        dot_y: get_y_axis_pos(*merge_point),
                        title: string_of_branch(ty, i)
                    };
                    append_dot(&merge_data, output, &branch.t_data, registry);
                }

                // render merge dot
                let m_data = EventDotData {
                    hash: *hash as u64,
                    dot_x: timeline_data.x_val,
                    dot_y: get_y_axis_pos(*merge_point + 1),
                    title: "merge".to_owned()
                };
                append_dot(&m_data, output, timeline_data, registry);
                continue;
            }
            _ => {} //do nothing
        }
        
        let mut data = EventDotData {
            hash: *hash as u64,
            dot_x: timeline_data.x_val,
            dot_y: get_y_axis_pos(*line_number),
            // default value if print_message_with_name() fails
            title: "Unknown Resource Owner Value".to_owned()
        };
        if let Some(mut name) = visualization_data.get_name_from_hash(hash) {
            match event {
                Event::OwnerGoOutOfScope => {
                    if !resource_hold {
                        let resource_info: &str = ". No resource is dropped.";
                        data.title = event.print_message_with_name(& mut name);
                        data.title.push_str(resource_info);
                    } else {
                        let resource_info: &str = ". Its resource is dropped.";
                        data.title = event.print_message_with_name(& mut name);
                        data.title.push_str(resource_info);
                    }
                },
                _ => {
                    data.title = event.print_message_with_name(& mut name);
                }
            }
        }
        // push to individual timelines
        append_dot(&data, output, timeline_data, registry);
    }
}

fn render_dots_string(
    output: &mut BTreeMap<i64, (TimelinePanelData, TimelinePanelData)>,
    visualization_data: &VisualizationData,
    resource_owners_layout: &BTreeMap<u64, TimelineColumnData>,
    registry: &Handlebars
){
    let timelines = &visualization_data.timelines;
    for (hash, timeline) in timelines {
        // render just the name of Owners and References
        match timeline.resource_access_point {
            ResourceAccessPoint::Function(_) => {
                // nothing to be done
            },
            ResourceAccessPoint::Owner(_) | ResourceAccessPoint::Struct(_) | ResourceAccessPoint::MutRef(_) | ResourceAccessPoint::StaticRef(_) =>
            {
                let resource_hold = if timeline.resource_access_point.is_owner() {
                    // we can assume each owner has at least two states (initialization -> gos) 
                    // technically it can have less if you bind something to the unit value eg: let z = ();
                    let penultimate_state = timeline.states.get(timeline.states.len() - 2).unwrap_or(&(0, 0, State::Invalid)).2.clone();
                    match penultimate_state {
                        State::FullPrivilege { .. } => true,
                        _ => false
                    }
                } else { false }; // need to change this for when structs can be references
                render_dot(hash, &timeline.history, &resource_owners_layout[hash], output, visualization_data, registry, resource_hold);
            },
        }
    }
}

fn traverse_timeline2<'a> (t: &'a TimelineColumnData, history: & 'a Vec<(usize, Event)>, id: usize) -> Option<& 'a TimelineColumnData> {
    for (_, e) in history {
        match e {
            Event::Branch { is, branch_history, .. } => {
                for branch in branch_history {
                    let res = traverse_timeline2(&branch.t_data, &branch.e_data, id);
                    if res.is_some() {
                        return res;
                    }
                }
            }
            _ => {
                if e.get_id() == id {
                    return Some(t);
                }
            }
        }
    }
    None
}


fn fetch_timeline<'a>(hash: &u64, vd: &'a VisualizationData, ro_layout: & 'a BTreeMap<u64, TimelineColumnData>, id: usize) -> & 'a TimelineColumnData {
    match traverse_timeline2(&ro_layout[hash], &vd.timelines[hash].history, id) {
        Some(t) => t,
        None => panic!("Shouldn't be happening")
    }
}

// fn fetch_timeline<'a>(
//     hash: &u64, 
//     gep: & mut VecDeque<(usize, usize)>, 
//     vd: &'a VisualizationData, 
//     ro_layout: & 'a BTreeMap<u64, TimelineColumnData>) -> & 'a TimelineColumnData 
//     {
//     if gep.is_empty() {
//         &ro_layout[hash]
//     }
//     else {
//         let var_history = &vd.timelines.get(hash).unwrap().history;
//         traverse_timeline(& mut gep.clone(), var_history)
//     }
// }

fn traverse_events2<'a> (
    history: & 'a Vec<(usize, ExternalEvent)>, 
    line_map: & 'a BTreeMap<usize, Vec<ExternalEvent>>,
    id: usize
) ->  Option<& 'a BTreeMap<usize, Vec<ExternalEvent>>> {
    for (_, e) in history {
        match e {
            ExternalEvent::Branch { branches, .. } => {
                for branch in branches {
                    let res = traverse_events2(&branch.e_data, &branch.line_map, id);
                    if res.is_some() {
                        return res;
                    }
                }
            }
            _ => {
                if e.get_id() == id {
                    return Some(line_map);
                }
            }
        }
    }
    None
}

fn fetch_line_map<'a>(
    vd: &'a VisualizationData,
    id: usize 
) -> & 'a BTreeMap<usize, Vec<ExternalEvent>> {
    match traverse_events2(&vd.external_events, &vd.event_line_map, id) {
        Some(t) => t,
        None => panic!("Error getting a line map")
    }
}

// render arrow
fn render_arrow (
    line_number : &usize,
    external_event: &ExternalEvent,
    output: &mut BTreeMap<i64, (TimelinePanelData, TimelinePanelData)>,
    visualization_data: &VisualizationData,
    resource_owners_layout: &BTreeMap<u64, TimelineColumnData>,
    registry: &Handlebars
) {
    match external_event {
        ExternalEvent::Branch { live_vars, branches, .. } => {
            // render all the events in the branch
            for (b_index, branch) in branches.iter().enumerate() {
                //gep.push_back((old_branch_freq, b_index));
                for (l, e) in branch.e_data.iter() {
                    // somewhat redundant but have to filter out external events here
                    match e {
                        ExternalEvent::Bind {..} | ExternalEvent::GoOutOfScope {..} | ExternalEvent::InitRefParam { .. }
                        | ExternalEvent::RefDie {..} => {}
                        _ => {
                            render_arrow(l, e,  output, visualization_data, resource_owners_layout, registry);
                        }
                    }
                }
                //gep.pop_back(); // backtrack
            }
            // *branch_freq = old_branch_freq;
        }
        _ => {
            // get the resource owners involved in the event
            let (from, to) = ResourceAccessPoint_extract(external_event);
            match (from, to) { // don't render arrow for anything to caller or anonymous or fn -> fn
                (ResourceTy::Anonymous, _) | (_, ResourceTy::Caller) | (_, ResourceTy::Anonymous) 
                | (ResourceTy::Value(ResourceAccessPoint::Function(_)), ResourceTy::Value(ResourceAccessPoint::Function(_))) => return,
                _ => {}
            }
            let mut title = string_of_external_event(external_event);
            // complete title
            let styled_from_string = SPAN_BEGIN.to_string() + &from.name() + SPAN_END;
            title = format!("{} from {}", title, styled_from_string);
            let styled_to_string = SPAN_BEGIN.to_string() + &to.name() + SPAN_END;
            title = format!("{} to {}", title, styled_to_string);

            // order of points is to -> from
            let mut data = ArrowData {
                coordinates: Vec::new(),
                coordinates_hbs: String::new(),
                title: title
            };

            let arrow_length = 20;


            match (from, to, external_event) {
                (ResourceTy::Value(ResourceAccessPoint::Function(from_function)), to_variable, _)  => {  // (Some(function), Some(variable), _)
                    // ro1 (to_variable) <- ro2 (from_function)
                    // arrow go from (x2, y2) -> (x1, y1)
                    // get position of to_variable
                    let to_timeline = fetch_timeline(to_variable.hash(), visualization_data, resource_owners_layout, external_event.get_id());
                    let x1 = to_timeline.x_val + 3; // adjust arrow head pos
                    let x2 = x1 + arrow_length;
                    let y1 = get_y_axis_pos(*line_number);
                    let y2 = get_y_axis_pos(*line_number);
                    data.coordinates.push((x1 as f64, y1 as f64));
                    data.coordinates.push((x2 as f64, y2 as f64));
    
                    let function_data = FunctionLogoData {
                        x: x2 + 3,
                        y: y2 + 5,
                        hash: from_function.hash.to_owned() as u64,
                        title: SPAN_BEGIN.to_string() + &from_function.name + SPAN_END,
                    };
    
                    if to_timeline.is_struct_group {
                        if to_timeline.is_member {
                            output.get_mut(&(to_timeline.owner.to_owned() as i64)).unwrap().1.dots.push_str(&registry.render("function_logo_template", &function_data).unwrap());
                        } else {
                            output.get_mut(&(to_timeline.owner.to_owned() as i64)).unwrap().0.dots.push_str(&registry.render("function_logo_template", &function_data).unwrap());
                        }
                    }
                    else {
                        output.get_mut(&-1).unwrap().0.dots.push_str(&registry.render("function_logo_template", &function_data).unwrap());
                    }
                }
                (from_variable, ResourceTy::Value(ResourceAccessPoint::Function(function)), ExternalEvent::PassByStaticReference { .. }) 
                | (from_variable, ResourceTy::Value(ResourceAccessPoint::Function(function)), ExternalEvent::PassByMutableReference { .. }) => { 
                    // (Some(variable), Some(function), PassByRef)
                    let styled_fn_name = SPAN_BEGIN.to_string() + &function.name + SPAN_END;
                    let styled_from_name = SPAN_BEGIN.to_string() + &from_variable.name() + SPAN_END;

                    // get position of to_variable
                    let from_timeline = fetch_timeline(from_variable.hash(), visualization_data, resource_owners_layout, external_event.get_id());

                    let title_fn = match external_event {
                        ExternalEvent::PassByStaticReference { .. } => " reads from ",
                        ExternalEvent::PassByMutableReference { .. } => " reads from/writes to ",
                        _ => unreachable!()
                    };
                    
                    let function_dot_data = FunctionDotData {
                        x: from_timeline.x_val,
                        y: get_y_axis_pos(*line_number),
                        title: styled_fn_name + title_fn + &styled_from_name,
                        hash: from_variable.hash().to_owned() as u64,
                    };

                    if from_timeline.is_struct_group {
                        if from_timeline.is_member {
                            output.get_mut(&(from_timeline.owner.to_owned() as i64)).unwrap().1.dots.push_str(&registry.render("function_dot_template", &function_dot_data).unwrap());
                        } else {
                            output.get_mut(&(from_timeline.owner.to_owned() as i64)).unwrap().0.dots.push_str(&registry.render("function_dot_template", &function_dot_data).unwrap());
                        }
                    }
                    else {
                        output.get_mut(&-1).unwrap().0.dots.push_str(&registry.render("function_dot_template", &function_dot_data).unwrap());
                    }
                }
                (from_variable, ResourceTy::Value(ResourceAccessPoint::Function(to_function)), _e) => { // (Some(variable), Some(function), _)
                    let styled_fn_name = SPAN_BEGIN.to_string() + &to_function.name + SPAN_END;
                    //  ro1 (to_function) <- ro2 (from_variable)
                    let from_timeline = fetch_timeline(from_variable.hash(), visualization_data, resource_owners_layout, external_event.get_id());
                    let x2 = from_timeline.x_val - 5;
                    let x1 = x2 - arrow_length;
                    let y1 = get_y_axis_pos(*line_number);
                    let y2 = get_y_axis_pos(*line_number);
                    data.coordinates.push((x1 as f64, y1 as f64));
                    data.coordinates.push((x2 as f64, y2 as f64));
    
                    let function_data = FunctionLogoData {
                        // adjust Function logo pos
                        x: x1 - 10,  
                        y: y1 + 5,
                        hash: to_function.hash.to_owned() as u64,
                        title: styled_fn_name,
                    };
    
                    if from_timeline.is_struct_group {
                        if from_timeline.is_member {
                            output.get_mut(&(from_timeline.owner.to_owned() as i64)).unwrap().1.dots.push_str(&registry.render("function_logo_template", &function_data).unwrap());
                        } else {
                            output.get_mut(&(from_timeline.owner.to_owned() as i64)).unwrap().0.dots.push_str(&registry.render("function_logo_template", &function_data).unwrap());
                        }
                    }
                    else {
                        output.get_mut(&-1).unwrap().0.dots.push_str(&registry.render("function_logo_template", &function_data).unwrap());
                    }
                },
                (from_variable, to_variable, e) => {
                    let line_map = fetch_line_map(&visualization_data, e.get_id());
                    let arrow_order = line_map.get(line_number).unwrap().iter().position(|x| x == external_event).unwrap() as i64;

                    let from_timeline = fetch_timeline(from_variable.hash(), visualization_data, resource_owners_layout, e.get_id());
                    let to_timeline = fetch_timeline(to_variable.hash(), visualization_data, resource_owners_layout, e.get_id());

                    let x1 = to_timeline.x_val;
                    let x2 = from_timeline.x_val;
                    let y1 = get_y_axis_pos(*line_number);
                    let y2 = get_y_axis_pos(*line_number);
                    // if the arrow is pointing from left to right
                    if arrow_order > 0 && x2 <= x1{
                        let x3 = from_timeline.x_val + 20;
                        let x4 = to_timeline.x_val - 20;
                        let y3 = get_y_axis_pos(*line_number)+LINE_SPACE*arrow_order;
                        let y4 = get_y_axis_pos(*line_number)+LINE_SPACE*arrow_order;
    
                        data.coordinates.push((x1 as f64, y1 as f64));
                        data.coordinates.push((x4 as f64, y4 as f64));
                        data.coordinates.push((x3 as f64, y3 as f64));
                        data.coordinates.push((x2 as f64, y2 as f64));
    
                    // if the arrow is pointing from right to left
                    } else if arrow_order > 0 && x2 > x1 {
                        let x3 = from_timeline.x_val - 20;
                        let x4 = to_timeline.x_val + 20;
                        let y3 = get_y_axis_pos(*line_number)+LINE_SPACE*arrow_order;
                        let y4 = get_y_axis_pos(*line_number)+LINE_SPACE*arrow_order;
    
                        data.coordinates.push((x1 as f64, y1 as f64));
                        data.coordinates.push((x4 as f64, y4 as f64));
                        data.coordinates.push((x3 as f64, y3 as f64));
                        data.coordinates.push((x2 as f64, y2 as f64));
    
                    } else {
                        data.coordinates.push((x1 as f64, y1 as f64));
                        data.coordinates.push((x2 as f64, y2 as f64));
                    }
                }
            }

            // draw arrow only if data.x1 is not default value
            if !data.coordinates.is_empty() {
                let last_index = data.coordinates.len() - 1;

                if data.coordinates.len() == 2 {
                    // [0]     [last index]
                    // <-------------------
                    if data.coordinates[0].0 < data.coordinates[last_index].0 {

                        data.coordinates[0].0 += 10 as f64;
                    }
                    // [last index]     [0]
                    // ------------------->
                    else {
                        data.coordinates[0].0 -= 10 as f64;
                    }
                } else {

                    if data.coordinates[0].0 < data.coordinates[last_index].0 {
                        let hypotenuse = (((data.coordinates[1].0 - data.coordinates[0].0) as f64).powi(2) + ((data.coordinates[1].1 - data.coordinates[0].1) as f64).powi(2)).sqrt();
                        let cos_ratio = ((data.coordinates[1].0 - data.coordinates[0].0) as f64) / hypotenuse;
                        let sin_ratio = ((data.coordinates[1].1 - data.coordinates[0].1) as f64) / hypotenuse;
                        data.coordinates[0].0 += cos_ratio*10 as f64;
                        data.coordinates[0].1 += sin_ratio*10 as f64;
                    }
                    else {
                        let hypotenuse = (((data.coordinates[0].0 - data.coordinates[1].0) as f64).powi(2) + ((data.coordinates[1].1 - data.coordinates[0].1) as f64).powi(2)).sqrt();
                        let cos_ratio = ((data.coordinates[0].0 - data.coordinates[1].0) as f64) / hypotenuse;
                        let sin_ratio = ((data.coordinates[1].1 - data.coordinates[0].1) as f64) / hypotenuse;
                        data.coordinates[0].0 -= cos_ratio*10 as f64;
                        data.coordinates[0].1 += sin_ratio*10 as f64;
                    }
                }

                while !data.coordinates.is_empty() {
                    let recent = data.coordinates.pop();
                    data.coordinates_hbs.push_str(&recent.unwrap().0.to_string());
                    data.coordinates_hbs.push_str(&String::from(" "));
                    data.coordinates_hbs.push_str(&recent.unwrap().1.to_string());
                    data.coordinates_hbs.push_str(&String::from(" "));
                }

                // will need to change this later for structs in conditionals
                if resource_owners_layout.contains_key(from.hash()) && resource_owners_layout[from.hash()].is_struct_group {
                    if resource_owners_layout[from.hash()].is_member {
                        output.get_mut(&(resource_owners_layout[from.hash()].owner.to_owned() as i64)).unwrap().1.arrows.push_str(&registry.render("arrow_template", &data).unwrap());
                    } else {
                        output.get_mut(&(resource_owners_layout[from.hash()].owner.to_owned() as i64)).unwrap().0.arrows.push_str(&registry.render("arrow_template", &data).unwrap());
                    }
                }
                else {
                    output.get_mut(&-1).unwrap().0.arrows.push_str(&registry.render("arrow_template", &data).unwrap());
                }
            }
        }
    }
}

// render arrows that support function
fn render_arrows_string_external_events_version(
    output: &mut BTreeMap<i64, (TimelinePanelData, TimelinePanelData)>,
    visualization_data: &VisualizationData,
    resource_owners_layout: &BTreeMap<u64, TimelineColumnData>,
    registry: &Handlebars
){
    for (line_number, external_event) in &visualization_data.external_events {
        match external_event {
            // events that should be skipped (we don't render arrows for them)
            ExternalEvent::Bind {..} | ExternalEvent::GoOutOfScope {..} | ExternalEvent::InitRefParam { .. }
            | ExternalEvent::RefDie {..} => {}

            _ => {

                // render external event arrow
                render_arrow(line_number, 
                    external_event,
                    output, visualization_data, resource_owners_layout, registry)
            }
        }
    }
}

fn determine_owner_line_styles(
    rap: &ResourceAccessPoint,
    state: &State
) -> OwnerLine {
    match (state, rap.is_mut()) {
        (State::FullPrivilege{..}, true) => OwnerLine::Solid,
        (State::FullPrivilege{..}, false) => OwnerLine::Hollow,
        // cannot assign to to variable because it is borrowed
        // partialprivilege ~= immutable, otherwise it would be an error
        (State::PartialPrivilege{..}, _) => OwnerLine::Hollow, // let (mut) a = 5;
        _ => OwnerLine::Empty, // Otherwise its empty
    }
}

fn compute_hollow_line_data(v_data: VerticalLineData, w: f64) -> HollowLineData{
    // Direction vector components
    let x1 = v_data.x1 as f64;
    let x2 = v_data.x2 as f64;
    let y1 = v_data.y1 as f64;
    let y2 = v_data.y2 as f64;
    let dx = x1 - x2;
    let dy = y1 - y2;
    
    // Length of the direction vector
    let d_length = (dx.powi(2) + dy.powi(2)).sqrt();


    let p_x = -dy / d_length * (-w / 2.0);
    let p_y  = dx / d_length * (-w / 2.0);

    // create new x and y coordinates
    let x1 = x1 + p_x;
    let x2 = x2 + p_x;
    let y1 = y1 + p_y;
    let y2 = y2 + p_y;
    
    // Perpendicular vector components, normalized and scaled by the width
    let px = -dy / d_length * w;
    let py = dx / d_length * w;

    // Compute the remaining points
    let x3 = x1 + px;
    let y3 = y1 + py;
    let x4 = x2 + px;
    let y4 = y2 + py;

    HollowLineData { line_class: v_data.line_class, 
        hash: v_data.hash, 
        x1: x1, x2: x2, x3: x4, x4: x3, 
        y1: y1, y2: y2, y3: y4, y4: y3, title: v_data.title, opacity: v_data.opacity }
}

// generate a Owner Line string from the RAP and its State
fn create_owner_line_string(
    rap: &ResourceAccessPoint,
    state: &State,
    data: &mut VerticalLineData,
    registry: &Handlebars,
) -> String {
    // TODO: prevent creating line when function dot already exists
    let style = determine_owner_line_styles(rap, state);

    match state {
        State::FullPrivilege { s: LineState::Gray } | State::PartialPrivilege { s: LineState::Gray } => {
            data.opacity = 0.5;
        }
        _ => {}
    }
    match (state, style) {
        (State::FullPrivilege{..}, OwnerLine::Solid) | (State::PartialPrivilege{ .. }, OwnerLine::Solid) => {
            data.line_class = String::from("solid");
            data.title += ". The binding can be reassigned.";
            registry.render("vertical_line_template", &data).unwrap()
        },
        (State::FullPrivilege{..}, OwnerLine::Hollow) | (State::PartialPrivilege{..}, OwnerLine::Hollow) => {
            let mut hollow_line_data = data.clone();
            hollow_line_data.title += ". The binding cannot be reassigned.";
            let new_hollow_line_data = compute_hollow_line_data(hollow_line_data, 3.5);
            let s = registry.render("new_hollow_line_template", &new_hollow_line_data).unwrap();
            s
        },
        (State::OutOfScope, _) => "".to_owned(),
        // do nothing when the case is (RevokedPrivilege, false), (OutofScope, _), (ResourceMoved, false)
        (_, _) => "".to_owned()
    }
}

// generate Reference Line(s) string from the RAP and its State
fn create_reference_line_string(
    rap: &ResourceAccessPoint,
    state: &State,
    data: &mut VerticalLineData,
    registry: &Handlebars,
) -> String {
    match state {
        State::FullPrivilege { s: LineState::Gray } | State::PartialPrivilege { s: LineState::Gray } => {
            data.opacity = 0.5;
        }
        _ => {}
    }
    match (state, rap.is_mut()) {
        (State::FullPrivilege{..}, true) => {
            data.line_class = String::from("solid");
            if rap.is_ref() {
                data.title += "; can read and write data; can point to another piece of data.";
            } else {
                data.title += "; can read and write data";
            }
            registry.render("vertical_line_template", &data).unwrap()
        },
        (State::FullPrivilege{..}, false) => {
            if rap.is_ref() {
                data.title += "; can read and write data; cannot point to another piece of data.";
            } else {
                data.title += "; can only read data";
            }
            
            let mut hollow_line_data = data.clone();
            // hollow_line_data.x1 -= 1.8; // center middle of path to center of event dot
            
            // registry.render("hollow_line_template", &hollow_line_data).unwrap()
            registry.render("new_hollow_line_template", &compute_hollow_line_data(hollow_line_data, 3.5)).unwrap()
        },
        (State::PartialPrivilege{ .. }, muta) => {
            data.line_class = String::from("solid");
            if muta {
              data.title += "; can point to another piece of data"
            }
            data.title += "; can only read data.";
            
            let mut hollow_line_data = data.clone();
            // hollow_line_data.x1 -= 1.8; // center middle of path to center of event dot
            hollow_line_data.title = data.title.to_owned();
            
            // registry.render("hollow_line_template", &hollow_line_data).unwrap()
            registry.render("new_hollow_line_template", &compute_hollow_line_data(hollow_line_data, 3.5)).unwrap()
        },
        (State::ResourceMoved{ .. }, true) => {
            data.line_class = String::from("extend");
            data.title += "; cannot access data.";
            registry.render("vertical_line_template", &data).unwrap()
        }
        // do nothing when the case is (RevokedPrivilege, _), (OutofScope, _), (ResourceMoved, false)
        _ => "".to_owned(),
    }
}

fn append_line(
    state: &State,
    data: & mut VerticalLineData,
    rap: &ResourceAccessPoint,
    timeline_data: &TimelineColumnData,
    output: &mut BTreeMap<i64, (TimelinePanelData, TimelinePanelData)>,
    registry: &Handlebars
) {
    match rap {
        ResourceAccessPoint::Function(_) => {}, // Don't do anything
        ResourceAccessPoint::Owner(_) | ResourceAccessPoint::Struct(_) => {
            if timeline_data.is_struct_group { //TODO: not sure if this is correct
                if !output.contains_key(&(timeline_data.owner.to_owned() as i64)) {
                    output.insert(timeline_data.owner.to_owned() as i64, (TimelinePanelData{ labels: String::new(), dots: String::new(), timelines: String::new(), 
                        ref_line: String::new(), arrows: String::new() }, TimelinePanelData{ labels: String::new(), dots: String::new(), 
                            timelines: String::new(), ref_line: String::new(), arrows: String::new() })); 
                }
                if timeline_data.is_member {
                    output.get_mut(&(timeline_data.owner.to_owned() as i64)).unwrap().1.timelines.push_str(&create_owner_line_string(rap, state, data, registry));
                } else {
                    output.get_mut(&(timeline_data.owner.to_owned() as i64)).unwrap().0.timelines.push_str(&create_owner_line_string(rap, state, data, registry));
                }
            }
            else {
                output.get_mut(&-1).unwrap().0.timelines.push_str(&create_owner_line_string(rap, state, data, registry));
            }
        },
        ResourceAccessPoint::StaticRef(_) | ResourceAccessPoint::MutRef(_) => {
            output.get_mut(&-1).unwrap().0.timelines.push_str(&create_reference_line_string(rap, state, data, registry));
        },
    }
}

// render timeline given a hash
fn render_timeline(
    hash: &u64,
    rap: &ResourceAccessPoint,
    history: &Vec<(usize, Event)>,
    states: &Vec<(usize, usize, State)>,
    output: &mut BTreeMap<i64, (TimelinePanelData, TimelinePanelData)>,
    visualization_data: &VisualizationData,
    timeline_data: &TimelineColumnData,
    registry: &Handlebars
) {
    if rap.is_fn() { return; } // functions have no timelines
    for (_, ev) in history {
        match ev {
            Event::Branch { is, branch_history, ty, split_point, merge_point, id } => {
                let begin_state = states.iter().find(|&item| item.1 == *split_point).unwrap().clone();
                let p_state = convert_back(&begin_state.2);
                for (i, branch) in branch_history.iter().enumerate() {
                    let mut split_line_data = VerticalLineData {
                        line_class: String::new(),
                        hash: *hash,
                        x1: timeline_data.x_val as f64,
                        y1: get_y_axis_pos(*split_point),
                        x2: branch.t_data.x_val as f64,
                        y2: get_y_axis_pos(*split_point + 1),
                        title: p_state.print_message_with_name(rap.name()),
                        opacity: 1.0
                    };

                    if branch.e_data.is_empty() || i != 0{
                        split_line_data.opacity = 0.5;
                    }
                    append_line(&p_state, &mut split_line_data, rap, timeline_data, output, registry);

                    // get ending state
                    let e_state = branch.states.last().unwrap().2.clone();

                    render_timeline(hash, 
                        rap, 
                        &branch.e_data,
                        &branch.states, 
                        output, 
                        visualization_data, 
                        &branch.t_data, 
                        registry);
                    
                    // render line from branch to merge
                    let mut merge_line_data = VerticalLineData {
                        line_class: String::new(),
                        hash: *hash,
                        x1: timeline_data.x_val as f64,
                        y1: get_y_axis_pos(*merge_point + 1),
                        x2: branch.t_data.x_val as f64,
                        y2: get_y_axis_pos(*merge_point),
                        title: e_state.print_message_with_name(rap.name()),
                        opacity: 1.0
                    };

                    if branch.e_data.is_empty() {
                        merge_line_data.opacity = 0.5;
                    }

                    append_line(&e_state, &mut merge_line_data, rap, timeline_data, output, registry);
                }
            }
            _ => {}
        }
    }

    for (line_start, line_end, state) in states {
        // println!("{} -> start: {}, end: {}, state: {}", visualization_data.get_name_from_hash(hash).unwrap(), line_start, line_end, state); // DEBUG PURPOSES
        let mut data = VerticalLineData {
                line_class: String::new(),
                hash: *hash,
                x1: timeline_data.x_val as f64,
                y1: get_y_axis_pos(*line_start),
                x2: timeline_data.x_val as f64,
                y2: get_y_axis_pos(*line_end),
                title: state.print_message_with_name(rap.name()),
                opacity: 1.0
        };
        if *line_start != *line_end {
            append_line(state, & mut data, rap, timeline_data, output, registry);
        }
    }
}

// render timelines (states) for RAPs using vertical lines
fn render_timelines(
    output: &mut BTreeMap<i64, (TimelinePanelData, TimelinePanelData)>,
    visualization_data: &VisualizationData,
    resource_owners_layout: &BTreeMap<u64, TimelineColumnData>,
    registry: &Handlebars
){
    let timelines = &visualization_data.timelines;
    for (hash, timeline) in timelines {
        let rap = &timeline.resource_access_point;
        match rap {
            ResourceAccessPoint::Function(_) => {},
            _ => {
                println!("hash {}, timeline {:#?}", hash, timeline);
                let t_data = resource_owners_layout.get(hash).unwrap();
                render_timeline(hash, rap, &timeline.history, &timeline.states, output, visualization_data, t_data, registry);
            }
        }
    }
}

// vertical lines indicating whether a reference can mutate its resource(deref as many times)
// (iff it's a MutRef && it has FullPrivilege)
fn render_ref_line(
    output: &mut BTreeMap<i64, (TimelinePanelData, TimelinePanelData)>,
    visualization_data: &VisualizationData,
    resource_owners_layout: &BTreeMap<u64, TimelineColumnData>,
    registry: &Handlebars
){
    let timelines = &visualization_data.timelines;

    for (hash, timeline) in timelines{
        match timeline.resource_access_point {
            ResourceAccessPoint::Function(_) => (), /* do nothing */
            ResourceAccessPoint::Struct(_) | ResourceAccessPoint::Owner(_) | ResourceAccessPoint::MutRef(_) | ResourceAccessPoint::StaticRef(_) =>
            {
                let ro = timeline.resource_access_point.to_owned();
                // verticle state lines
                let states = visualization_data.fetch_states(hash, &timeline.history, None, None);
                // struct can live over events
                let mut alive = false;
                let mut data = RefLineData {
                    line_class: String::new(),
                    hash: 0,
                    x1: 0,
                    x2: 0,
                    y1: 0,
                    y2: 0,
                    v: 0,
                    dx: 15,
                    dy: 0,
                    title: String::new(),
                };
                for (line_start, _line_end, state) in states.iter() {
                    match state { // consider removing .clone()
                        State::OutOfScope | State::ResourceMoved{ .. } => {
                            if alive {
                                // finish line template
                                data.x2 = data.x1.clone();
                                data.y2 = get_y_axis_pos(*line_start);
                                let dv = get_y_axis_pos(*line_start)-data.y1;
                                data.v = dv - 2*dv/5;
                                data.dy = dv/5;

                                match ro {
                                    ResourceAccessPoint::MutRef(_) => {
                                        output.get_mut(&-1).unwrap().0.ref_line.push_str(&registry.render("solid_ref_line_template", &data).unwrap());
                                    },
                                    ResourceAccessPoint::StaticRef(_) => {
                                        output.get_mut(&-1).unwrap().0.ref_line.push_str(&registry.render("hollow_ref_line_template", &data).unwrap());
                                    },
                                    _ => (),
                                }

                                alive = false;
                            }
                        },
                        State::FullPrivilege{..} => {
                            if !alive {
                                // set known vals
                                data.hash = *hash;
                                data.x1 = resource_owners_layout[hash].x_val;
                                data.y1 = get_y_axis_pos(*line_start);

                                data.title = String::from(
                                    format!("can mutate *{}", visualization_data.get_name_from_hash(hash).unwrap())
                                );
                                data.line_class = String::from("solid");
                                alive = true;
                            }
                        },
                        State::PartialPrivilege{..} => {
                            if !alive {
                                // set known vals
                                data.hash = *hash;
                                data.x1 = resource_owners_layout[hash].x_val;
                                data.y1 = get_y_axis_pos(*line_start);

                                data.title = String::from(
                                    format!("cannot mutate *{}",visualization_data.get_name_from_hash(hash).unwrap())
                                );
                                data.line_class = String::from("solid");
                                alive = true;
                            }
                        },
                        _ => (),
                    }
                }
            },  
        }
    }
}

fn render_struct_box(
    output: &mut BTreeMap<i64, (TimelinePanelData, TimelinePanelData)>,
    structs_info: &StructsInfo, 
    registry: &Handlebars,
) {
    for (owner, owner_x, last_x) in structs_info.structs.iter() {
        let mut box_data = BoxData {
            name: owner.clone() as u64,
            hash: 0,
            x: 0,
            y: 50,
            w: 0,
            h: 0,
            title: String::new(),
        };   
        box_data.x = owner_x - 20;
        box_data.w = last_x - owner_x + 60;
        box_data.h = 30;
        output.get_mut(owner).unwrap().1.arrows.push_str(&registry.render("box_template", &box_data).unwrap());
    }
}

fn get_y_axis_pos(line_number : usize) -> i64 {
    85 - LINE_SPACE + LINE_SPACE * line_number as i64
}
