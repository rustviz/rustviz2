use std::collections::{HashSet, BTreeMap};
use std::vec::Vec;
use std::fmt::{Formatter, Result, Display};
use std::hash::{Hash, Hasher};
use crate::data::Event::*;
use crate::hover_messages;
use crate::svg_frontend::timeline_panel::TimelineColumnData;
use std::cmp::Ordering;
/*
 * Basic Data Structure Needed by Lifetime Visualization
 */
pub static LINE_SPACE: i64 = 30;
// Top level Api that the Timeline object supports
pub trait Visualizable {
    // returns None if the hash does not exist
    fn get_name_from_hash(&self, hash: &u64) -> Option<String>;
    
    // returns None if the hash does not exist
    fn get_state(&self, hash: &u64, line_number: &usize) -> Option<State>;
    
    // for querying states of a resource owner using its hash
    //                                         start line, end line, state
    fn get_states(&self, hash: &u64,  history: & Vec<(usize, Event)>, prev_state: Option<State>, range: Option<(usize, usize)>) -> Vec::<(usize,      usize,    State)>;

    // WARNING do not call this when making visualization!! 
    // use append_external_event instead
    fn _append_event(&mut self, resource_access_point: &ResourceAccessPoint, event: Event, line_number: &usize);
    
    // add an event to the Visualizable data structure
    fn append_processed_external_event(&mut self, event: ExternalEvent, line_number: usize, collection: & mut Option<Vec<(usize, Event)>>);
    
    // if resource_access_point with hash is mutable
    fn is_mut(&self, hash: &u64 ) -> bool;
    // if resource_access_point with hash is a function
    fn is_mutref(&self, hash: &u64) -> bool;

    fn is_ref(&self, hash: &u64) -> bool;

    fn calc_state(&self, previous_state: & State, event: & Event, event_line: usize, hash: &u64) -> State;
}


// Every object in Rust should belong in one of these catagories
// A ResourceAccessPoint is either an Owner, a reference, or a Function that
// have ownership to a memory object, during some stage of
// a the program execution.
#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum ResourceAccessPoint {
    Owner(Owner),
    MutRef(MutRef),
    StaticRef(StaticRef),
    Function(Function),
    Struct(Struct),
}

// when something is not a reference
#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct Owner {
    pub name: String,
    pub hash: u64,
    pub is_mut: bool,                     // let a = 42; vs let mut a = 42;
}

// when something is a struct member
#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct Struct {
    pub name: String,
    pub hash: u64,
    pub owner: u64,
    pub is_mut: bool,                     
    pub is_member: bool,
    //pub rap: Option<Box<ResourceAccessPoint>>
}

// a reference of type &mut T
#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct MutRef {         // let (mut) r1 = &mut a;
    pub name: String,
    pub hash: u64,
    pub is_mut: bool,
}

// a reference of type & T
#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct StaticRef {                // let (mut) r1 = & a;
    pub name: String,
    pub hash: u64,
    pub is_mut: bool,
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct Function {
    pub name: String,
    pub hash: u64,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum ResourceTy {
    Anonymous, // an anonymous resource holder
    Deref(ResourceAccessPoint), // Dereferencing a RAP
    Caller, // For expressing relationship between return expr and fn
    Value(ResourceAccessPoint) 
}

impl ResourceTy {
    pub fn name(&self) -> String {
        match self {
            ResourceTy::Anonymous => "Anonymous resource".to_owned(),
            ResourceTy::Value(r) => r.name().to_owned(),
            ResourceTy::Deref(r) => format!("*{}", r.name()),
            ResourceTy::Caller => "Caller".to_owned()
        }
    }

    pub fn real_name(&self) -> String {
        match self {
            ResourceTy::Anonymous => "Anonymous resource".to_owned(),
            ResourceTy::Value(r) | ResourceTy::Deref(r) => r.name().to_owned(),
            ResourceTy::Caller => "Caller".to_owned()
        }
    }

    pub fn hash(&self) -> &u64 {
        match self {
            ResourceTy::Caller | ResourceTy::Anonymous => &std::u64::MAX,
            ResourceTy::Value(r) | ResourceTy::Deref(r) => r.hash(),
        }
    }

    pub fn is_ref(&self) -> bool {
      match self {
        ResourceTy::Value(r) | ResourceTy::Deref(r) => r.is_ref(),
        _ => false
      }
    }

    pub fn extract_rap(&self) -> &ResourceAccessPoint {
        match self {
            ResourceTy::Anonymous | ResourceTy::Caller => panic!("don't call this function unless certain that resourceTy is val or deref"),
            ResourceTy::Deref(r) | ResourceTy::Value(r) => r
        }
    }

    
}

impl Hash for ResourceTy {
  fn hash<H: Hasher>(&self, state: &mut H) {
    self.real_name().hash(state);
  }
}

impl ResourceAccessPoint {
    // get the attribute hash
    pub fn hash(&self) -> &u64 {
        match self {
            ResourceAccessPoint::Owner(Owner{hash, ..}) => hash,
            ResourceAccessPoint::Struct(Struct{hash, ..}) => hash,
            ResourceAccessPoint::MutRef(MutRef{hash, ..}) => hash,
            ResourceAccessPoint::StaticRef(StaticRef{hash, ..}) => hash,
            ResourceAccessPoint::Function(Function{hash, ..}) => hash,
        }
    }

    // get the name field
    pub fn name(&self) -> &String {
        match self {
            ResourceAccessPoint::Owner(Owner{name, ..}) => name,
            ResourceAccessPoint::Struct(Struct{name, ..}) => name,
            ResourceAccessPoint::MutRef(MutRef{name, ..}) => name,
            ResourceAccessPoint::StaticRef(StaticRef{name, ..}) => name,
            ResourceAccessPoint::Function(Function{name, ..}) => name,
        }
    }

    // get the is_mut field, if any
    pub fn is_mut(&self) -> bool {
        match self {
            ResourceAccessPoint::Owner(Owner{is_mut, ..}) => *is_mut,
            ResourceAccessPoint::Struct(Struct{is_mut, ..}) => *is_mut,
            ResourceAccessPoint::MutRef(MutRef{is_mut, ..}) => *is_mut,
            ResourceAccessPoint::StaticRef(StaticRef{is_mut, ..}) => *is_mut,
            ResourceAccessPoint::Function(_) => false,
        }
    }

    pub fn is_ref(&self) -> bool {
        match self {
            ResourceAccessPoint::MutRef(_) | ResourceAccessPoint::StaticRef(_) => true,
            _ => false
        }
    }

    pub fn is_mutref(&self) -> bool {
        match self {
            ResourceAccessPoint::MutRef(_) => true,
            _ => false
        }
    }

    pub fn is_struct_group(&self) -> bool {
        match self {
            ResourceAccessPoint::Struct(_) => true,
            _ => false
        }
    }

    pub fn is_struct(&self) -> bool {
        match self {
            ResourceAccessPoint::Struct(Struct{is_member, ..}) => !is_member,
            _ => false
        }
    }

    pub fn is_member(&self) -> bool {
        match self {
            ResourceAccessPoint::Struct(Struct{is_member, ..}) => *is_member,
            _ => false
        }
    }

    pub fn get_owner(&self) -> u64 {
        match self {
            ResourceAccessPoint::Owner(Owner{hash, ..}) => hash.to_owned(),
            ResourceAccessPoint::Struct(Struct{owner, ..}) => owner.to_owned(),
            ResourceAccessPoint::MutRef(MutRef{hash, ..}) => hash.to_owned(),
            ResourceAccessPoint::StaticRef(StaticRef{hash, ..}) => hash.to_owned(),
            ResourceAccessPoint::Function(Function{hash, ..}) => hash.to_owned(),
        }
    }

    pub fn is_owner(&self) -> bool {
        match self {
            ResourceAccessPoint::Owner(_) => true,
            _ => false
        }
    }

    pub fn is_fn(&self) -> bool {
        match self {
            ResourceAccessPoint::Function(_) => true,
            _ => false
        }
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum BranchType {
  If(Vec<String>),
  Loop(Vec<String>),
  Match(Vec<String>)
}

pub fn string_of_branch(b: &BranchType, index: usize) -> String {
    match b {
        BranchType::If(x) | BranchType::Loop(x) | BranchType::Match(x) => {
            x.get(index).unwrap().clone()
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum ExternalEvent {
    /* let binding, e.g.: let x = 1 */
    Bind {
        from: ResourceTy,
        to: ResourceTy
    },
    Copy {
        from: ResourceTy,
        to: ResourceTy
    },
    Move {
        from: ResourceTy,
        to: ResourceTy,
    },
    StaticBorrow {
        from: ResourceTy,
        to: ResourceTy,
    },
    MutableBorrow {
        from: ResourceTy,
        to: ResourceTy,
    },
    StaticDie {
        // return the resource to "to"
        from: ResourceTy,
        to: ResourceTy,
    },
    MutableDie {
        // return the resource to "to"
        from: ResourceTy,
        to: ResourceTy,
    },

    RefDie {
        from: ResourceTy,
        to: ResourceTy,
        num_curr_borrowers: usize
    },
    // a use of the Owner, happens when var pass by reference
    // its really borrow and return but happens on the same line,
    // use this event instead of borrow and return for more concise visualization 
    PassByStaticReference {
        from: ResourceTy,
        to: ResourceTy, // must be a function
    },
    PassByMutableReference {
        from: ResourceTy,
        to: ResourceTy, // must be a function
    },
    GoOutOfScope {
        ro: ResourceAccessPoint
    },
    // only use this event to initialize fn parameters
    InitRefParam {
        param: ResourceAccessPoint,
    },

    Branch {
      live_vars: HashSet<ResourceAccessPoint>,
      branches: Vec<Vec<(usize, ExternalEvent)>>,
      branch_type: BranchType,
      split_point: usize,
      merge_point: usize
    }
}

#[derive(Debug, Clone)]
pub struct BranchData {
  pub t_data: TimelineColumnData,
  pub e_data: Vec<(usize, Event)>,
  pub width: usize
}
// ASSUMPTION: a reference must return resource before borrow;
//
// An Event describes the acquisition or release of a
// resource ownership by a Owner on any given line.
// There are six types of them.
#[derive(Debug, Clone)]
pub enum Event {
    // this happens when a variable is initiated, it should obtain
    // its resource from either another variable or from a
    // contructor.
    //
    // E.g. in the case of
    //      let x = Vec::new();
    // x obtained the resource from global resource allocator,
    // the Acquire Event's "from" variable is None.
    // in the case of
    //      let y = x;
    // y obtained its value from x, which means that the Acquire
    // Event's "from" variable is x.
    // TODO do we need mut/static_acquire for get_state?
    Acquire {
        from: ResourceTy,
        is: ResourceTy
    },
    // this happens when a ResourceAccessPoint implements copy trait or
    // explicitly calls .clone() function
    // to another ResourceAccessPoint, or a function.
    //
    // e.g.
    // 1. x: i32 = y + 15;              here y duplicate to + op, and x acquire from +
    //                                  at this point, we treat it as y duplicates to None
    // 2. x: MyStruct = y.clone();      here y duplicates to x.
    Duplicate {
        to: ResourceTy,
        is: ResourceTy
    },
    // this happens when a ResourceAccessPoint transfers a copy of its contents
    // to another ResourceAccessPoint.
    // Typically, this occurs when a resource owner implements the Copy trait.
    Copy {
        from: ResourceTy,
        is: ResourceTy
    },
    // this happens when a ResourceAccessPoint transfer the ownership of its resource
    // to another ResourceAccessPoint, or if it is no longer used after this line.
    // Typically, this happens at one of the following two cases:
    //
    // 1. variable is not used after this line.
    // 2. variable's resource has the move trait, and it transfered
    //    its ownership on this line. This includes tranfering its
    //    ownership to a function as well.
    Move {
        to: ResourceTy,
        is: ResourceTy
    },

    Branch {
      is: ResourceTy,
      branch_history: Vec<BranchData>,
      ty: BranchType,
      split_point: usize,
      merge_point: usize
    },

    MutableLend {
        to: ResourceTy,
        is: ResourceTy
    },
    MutableBorrow {
        from: ResourceTy,
        is: ResourceTy
    },
    MutableDie {
        to: ResourceTy,
        is: ResourceTy
    },
    MutableReacquire {
        from: ResourceTy,
        is: ResourceTy
    },
    StaticLend {
        to: ResourceTy,
        is: ResourceTy
    },
    StaticBorrow {
        from: ResourceTy,
        is: ResourceTy
    },
    StaticDie {
        to: ResourceTy,
        is: ResourceTy
    },
    StaticReacquire {
        from: ResourceTy,
        is: ResourceTy
    },

    RefDie {
      from: ResourceTy,
      is: ResourceTy,
      num_curr_borrowers: usize
    },
    // this happens when a owner is returned this line,
    // or if this owner's scope ends at this line. The data must be dropped. 
    OwnerGoOutOfScope,
    // this happens when a vairable that is not an owner goes out of scope. 
    // The data is not dropped in this case
    RefGoOutOfScope,
    // SPECIAL CASE: use only to initialize a fn's paramter
    // Requires param to be Owner, StaticRef, or MutRef (cannot be Function)
    InitRefParam {
        param: ResourceAccessPoint
    },
}

// A State is a description of a ResourceAccessPoint IMMEDIATELY AFTER a specific line.
// We think of this as what read/write access we have to its resource.
#[derive(Clone, Debug)]
pub enum State {
    // The viable is no longer in the scope after this line.
    OutOfScope,
    // The resource is transferred on this line or before this line,
    // thus it is impossible to access this variable anymore.
    ResourceMoved {
        move_to: ResourceTy,
        move_at_line: usize
    },
    // This ResourceAccessPoint is the unique object that holds the ownership to the underlying resource.
    FullPrivilege,
    // More than one ResourceAccessPoint has access to the underlying resource
    // This means that it is not possible to create a mutable reference
    // on the next line.
    // About borrow_count: this value is at least one at any time.
    //      When the first static reference of this ResourceAccessPoint is created,
    //          this value is set to 1;
    //      When a new static reference is borrowed from this variable, increment by 1;
    //      When a static reference goes out of scope, decrement this value by 1;
    //      When a decrement happens while the borrow_count is 1, the state becomes
    //          FullPrivilege once again.
    PartialPrivilege {
        borrow_count: u32,
        borrow_to: HashSet<ResourceTy>
    },
    // temporarily no read or write access right to the resource, but eventually
    // the privilege will come back. Occurs when mutably borrowed
    RevokedPrivilege {
        to: ResourceTy,
        borrow_to: ResourceTy,
        prev_state: Box<State>
    },
    // should not appear for visualization in a correct program
    Invalid,
}

impl std::fmt::Display for State {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result {
        match self {
            State::OutOfScope => write!(f, "OutOfScope"),
            State::ResourceMoved { move_to: _, move_at_line: _ } => write!(f, "ResourceMoved"),
            State::FullPrivilege => write!(f, "FullPrivilege"),
            State::PartialPrivilege { .. } => write!(f, "PartialPrivilege"),
            State::RevokedPrivilege { .. } => write!(f, "RevokedPrivilege"),
            State::Invalid => write!(f, "Invalid"),
        }
    }
}


fn safe_message(
    message_functor: fn(&String, &String) -> String,
    my_name: &String,
    some_target: &ResourceTy
) -> String {

    let target_name = match some_target {
        ResourceTy::Deref(r) => {
            let mut temp = r.name().clone();
            temp.insert(0, '*');
            temp
        }
        _ => some_target.name()
    };
    message_functor(my_name, &target_name)
}


impl State {
    pub fn print_message_with_name(&self, my_name: &String) -> String {
        match self {
            State::OutOfScope => {
                hover_messages::state_out_of_scope(my_name)
            }
            State::ResourceMoved{ move_to , move_at_line: _ } => {
                safe_message(hover_messages::state_resource_moved, my_name, move_to)
            }
            State::FullPrivilege => {
                hover_messages::state_full_privilege(my_name)
            }
            State::PartialPrivilege { .. } => {
                hover_messages::state_partial_privilege(my_name)
            }
            State::RevokedPrivilege { to: _, borrow_to , prev_state: _} => {
                safe_message(hover_messages::state_resource_revoked, my_name, borrow_to)
            }
            State::Invalid => {
                hover_messages::state_invalid(my_name)
            }
        }
    }
}

impl PartialOrd for State {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for State {
    fn cmp(&self, other: &Self) -> Ordering {
        use State::*;
        
        fn rank(s: &State) -> u8 {
            match s {
                ResourceMoved { .. } | RevokedPrivilege { .. } => 0,
                PartialPrivilege { .. } => 1,
                FullPrivilege => 2,
                Invalid | OutOfScope => 3,
            }
        }

        let self_rank = rank(self);
        let other_rank = rank(other);

        self_rank.cmp(&other_rank)
    }
}

impl PartialEq for State {
    fn eq(&self, other: &State) -> bool {
        self.cmp(other) == Ordering::Equal
    }
}

impl Eq for State {}



// provide string output for usages like format!("{}", eventA)
impl Display for Event {
    fn fmt(&self, f: &mut Formatter) -> Result {       
        match self {
            Event::Acquire{ from , ..} => { write!(f, "Acquiring resource from {}", from.name()) },
            Event::Duplicate{ to , ..} => { write!(f, "Duplicating resource to {}", to.name())},
            Event::Copy{ from , ..} => { write!(f, "Copying resource from {}", from.name())},
            Event::Move{ to , ..} => {write!(f, "Moving resource to {}", to.name())},
            Event::MutableLend{ to , ..} => {write!(f, "Mutable lend to {}", to.name())},
            Event::MutableBorrow{ from , ..} => { write!(f, "Fully borrows resource from {}", from.name())},
            Event::MutableDie{ to , ..} => { write!(f, "Fully returns resource to {}", to.name())},
            Event::MutableReacquire{ from, .. } => {write!(f, "Fully reacquires resource from {}", from.name())},
            Event::StaticLend{ to , ..} => {write!(f, "Partially lends resource to {}", to.name())},
            Event::StaticBorrow{ from , ..} => { write!(f, "Partially borrows resource from {}", from.name())},
            Event::StaticDie{ to , ..} => { write!(f, "Partially returns resource to {}", to.name())},
            Event::StaticReacquire{ from , ..} => { write!(f, "Partially acquires resource from {}", from.name()) },
            Event::InitRefParam{ param: _ } => { write!(f, "Function parameter is initialized") },
            Event::OwnerGoOutOfScope => { 
                write!(f, "Goes out of Scope as an owner of resource" ) }
            Event::RefGoOutOfScope => {
                write!(f, "Goes out of Scope as a reference to resource")
            },
            Event::RefDie { from, .. } => {
              write!(f, "{} reference dies", from.name())
            }
            Event::Branch { is, .. } => {
              write!(f, "{} branch occuring ", is.real_name())
            }
        }
    }
}

impl Event {
    pub fn deref_name <'a>(&self, name: &'a mut String) -> & 'a String {
        match self {
            Duplicate {is: x , ..} | Move {is: x, ..} | StaticLend {is : x, ..} | 
            MutableLend {is: x, ..} | MutableDie {is: x, ..} | StaticDie {is: x, ..} | 
            Acquire { is: x, .. } | Copy { is: x, .. } | MutableBorrow { is: x, .. } |
            StaticBorrow { is: x, .. } | StaticReacquire { is: x, .. } | MutableReacquire {is: x, ..} => {
                match x {
                    ResourceTy::Deref(_) => {
                        name.insert(0, '*');
                        name
                    },
                    _ => name
                }
            },
            _ => name
        }
    }
    pub fn print_message_with_name(&self, my_name: &mut String) -> String {
        match self {
            // no arrow involved
            OwnerGoOutOfScope => { 
                hover_messages::event_dot_owner_go_out_out_scope(my_name)
            }
            RefGoOutOfScope => {
                hover_messages::event_dot_ref_go_out_out_scope(my_name)
            }
            InitRefParam{ param: _ } => {
                hover_messages::event_dot_init_param(my_name)
            }
            // arrow going out
            Duplicate{ to ,..} => {
                match to {
                    ResourceTy::Caller => safe_message(hover_messages::event_dot_copy_to_caller, &self.deref_name(my_name), to),
                    _ => safe_message(hover_messages::event_dot_copy_to, &self.deref_name(my_name), to)
                }
            }
            Move{ to ,..} => {
                match to {
                    ResourceTy::Caller => safe_message(hover_messages::event_dot_move_to_caller, &self.deref_name(my_name), to),
                    _ => safe_message(hover_messages::event_dot_move_to, &self.deref_name(my_name), to),
                }
                
            }
            StaticLend{ to ,..} => {
                safe_message(hover_messages::event_dot_static_lend, &self.deref_name(my_name), to)
            }
            MutableLend{ to ,..} => {
                safe_message(hover_messages::event_dot_mut_lend, &self.deref_name(my_name), to)
            }
            StaticDie{ to,.. } => {
                safe_message(hover_messages::event_dot_static_return, &self.deref_name(my_name), to)
            }
            MutableDie{ to ,..} => {
                safe_message(hover_messages::event_dot_mut_return, &self.deref_name(my_name), to)
            }
            // arrow going in
            Acquire{ from ,..} => {
                safe_message(hover_messages::event_dot_acquire, &self.deref_name(my_name), from)
            }
            Copy{ from ,..} => {
                safe_message(hover_messages::event_dot_copy_from, &self.deref_name(my_name), from)
            }
            MutableBorrow{ from ,..} => {
                hover_messages::event_dot_mut_borrow(&self.deref_name(my_name), &from.name())
            }
            StaticBorrow{ from ,..} => {
                hover_messages::event_dot_static_borrow(&self.deref_name(my_name), &from.name())
            }
            StaticReacquire{ from ,..} => {
                safe_message(hover_messages::event_dot_static_reacquire, &self.deref_name(my_name), from)
            }
            MutableReacquire{ from ,..} => {
                safe_message(hover_messages::event_dot_mut_reacquire, &self.deref_name(my_name), from)
            }
            RefDie {..} => {
              panic!("should never be calling this function with this event");
            },
            Branch { .. } => {
              format!("{} is live in a conditional expression ", my_name)
            }
        } 
    }
}

// a vector of ownership transfer history of a specific variable,
// in a sorted order by line number.
#[derive(Debug)]
pub struct Timeline {
    pub resource_access_point: ResourceAccessPoint,    // a reference of an Owner or a (TODO) Reference, 
                                // since Functions don't have a timeline 
    // line number in usize
    pub history: Vec<(usize, Event)>,
}

// a vector of structs information
#[derive(Debug)]
pub struct StructsInfo {
    //struct owner hash, x val of struct owner, x val of the rightmost member
    pub structs: Vec<(i64, i64, i64)>,
}

// VisualizationData supplies all the information we need in the frontend,
// from rendering a PNG to px roducing an interactive HTML guide.
// The internal data is simple: a map from variable hash to its Timeline.
#[derive(Debug)]
pub struct VisualizationData {
    // When displaying all timelines in the frontend of choice, one should
    // consider picking a hash function that gives the BTreeMap a sensible order.
    //      timelines: an orderred map from a Variable's hash to 
    //      the Variable's Timeline.
    pub timelines: BTreeMap<u64, Timeline>,
    
    pub external_events: Vec<(usize, ExternalEvent)>,
    //temp container for external_events
    pub preprocess_external_events: Vec<(usize, ExternalEvent)>,
    //line_info
    pub event_line_map: BTreeMap<usize, Vec<ExternalEvent>>,

    pub num_valid_raps: usize,
}

#[allow(non_snake_case)]
pub fn ResourceAccessPoint_extract (external_event : &ExternalEvent) -> (&ResourceTy, &ResourceTy){
    let (from, to) = match external_event {
        ExternalEvent::Bind{from: from_ro, to: to_ro} => (from_ro, to_ro),
        ExternalEvent::Copy{from: from_ro, to: to_ro} => (from_ro, to_ro),
        ExternalEvent::Move{from: from_ro, to: to_ro} => (from_ro, to_ro),
        ExternalEvent::StaticBorrow{from: from_ro, to: to_ro} => (from_ro, to_ro),
        ExternalEvent::StaticDie{from: from_ro, to: to_ro} => (from_ro, to_ro),
        ExternalEvent::MutableBorrow{from: from_ro, to: to_ro} => (from_ro, to_ro),
        ExternalEvent::MutableDie{from: from_ro, to: to_ro} => (from_ro, to_ro),
        ExternalEvent::PassByMutableReference{from: from_ro, to: to_ro} => (from_ro, to_ro),
        ExternalEvent::PassByStaticReference{from: from_ro, to: to_ro} => (from_ro, to_ro),
        _ => (&ResourceTy::Anonymous, &ResourceTy::Anonymous)
    };
    (from, to)
}

pub fn string_of_external_event(e: &ExternalEvent) -> String {
    match e {
        ExternalEvent::Bind{ .. } => {
            String::from("Bind")
        },
        ExternalEvent::Copy{ .. } => {
            String::from("Copy")
        },
        ExternalEvent::Move{ .. } => {
          String::from("Move")
        },
        ExternalEvent::StaticBorrow{ .. } => {
            String::from("Immutable borrow")
        },
        ExternalEvent::StaticDie{ .. } => {
            String::from("Return immutably borrowed resource")
        },
        ExternalEvent::MutableBorrow{ .. } => {
            String::from("Mutable borrow")
        },
        ExternalEvent::MutableDie{ .. } => {
           String::from("Return mutably borrowed resource")
        },
        ExternalEvent::PassByMutableReference{ .. } => {
            String::from("Pass by mutable reference")
        },
        ExternalEvent::PassByStaticReference{ .. } => {
            String::from("Pass by immutable reference")
        },
        _ => unreachable!(),
    }
}

// fulfills the promise that we can support all the methods that a
// frontend would need.
impl Visualizable for VisualizationData {
    fn get_name_from_hash(&self, hash: &u64) -> Option<String> {
        match self.timelines.get(hash) {
            Some(timeline) => Some(timeline.resource_access_point.name().to_owned()),
            _ => None
        }
    }

    // if the ResourceAccessPoint is declared mutable
    fn is_mut(&self, hash: &u64) -> bool {
        self.timelines[hash].resource_access_point.is_mut()
    }

    // if the ResourceAccessPoint is a function
    fn is_mutref(&self, hash: &u64) -> bool {
        self.timelines[hash].resource_access_point.is_mutref()
    }

    fn is_ref(&self, hash: &u64) -> bool {
      self.timelines[hash].resource_access_point.is_ref()
    }

    // a Function does not have a State, so we assume previous_state is always for Variables
    // TODO: get rid of borrowing logic here, backend handles all of it
    fn calc_state(&self, previous_state: & State, event: & Event, event_line: usize, hash: &u64) -> State {
        /* a Variable cannot borrow or return resource from Functions, 
        but can 'lend' or 'reaquire' to Functions (pass itself by reference and take it back); */
        fn event_invalid(event: & Event) -> bool {
            match event {
                Event::StaticBorrow{ from: ResourceTy::Value(ResourceAccessPoint::Function(_)) ,..} => true,
                Event::MutableBorrow{ from: ResourceTy::Value(ResourceAccessPoint::Function(_)) ,..} => true,
                Event::StaticDie{ to: ResourceTy::Value(ResourceAccessPoint::Function(_)) ,..} => true,
                Event::MutableDie{ to: ResourceTy::Value(ResourceAccessPoint::Function(_)) ,..} => true,
                _ => false,
            }
        }
        if event_invalid(event) { return State::Invalid; }

        match (previous_state, event) {
            (State::Invalid, _) => State::Invalid,

            (State::OutOfScope, Event::Acquire{ .. }) => State::FullPrivilege,

            (State::OutOfScope, Event::Copy{ from: ro, is: is_ro }) => {
                match ro {
                    ResourceTy::Anonymous => State::FullPrivilege,
                    ResourceTy::Deref(r) | ResourceTy::Value(r) => {
                        if r.is_ref() && is_ro.is_ref() {
                            if r.is_mutref() {
                                panic!("Not possible, has to be a move");
                            }
                            else {
                                let mut prev_state = State::OutOfScope;
                                // TODO: this will break inside of branches (doesn't this have to always be PartialPrivilege???)
                                for (line_number, event) in self.timelines[r.hash()].history.iter() {
                                    prev_state = self.calc_state(&prev_state, &event, *line_number, hash);
                                    if event_line == *line_number {
                                        break;
                                    }
                                }
                                match prev_state {
                                    State::PartialPrivilege { borrow_count: b_c, borrow_to: b_to } => {
                                        let mut b_new_to = b_to.clone();
                                        b_new_to.insert(is_ro.to_owned());
                                        State::PartialPrivilege { borrow_count: b_c + 1, borrow_to: b_new_to}
                                    }
                                    _ => panic!("ref copy state has to be partial privilege")
                                }
                            }   
                        }
                        else {
                            State::FullPrivilege
                        }
                    }
                    _ => panic!("not possible")
                }
            }

            (State::OutOfScope, Event::StaticBorrow{ from: ro,.. }) =>
                State::PartialPrivilege {
                    borrow_count: 1,
                    borrow_to: [ro.to_owned()].iter().cloned().collect()
                },

            (State::OutOfScope, Event::MutableBorrow{ .. }) => State::FullPrivilege,

            (State::OutOfScope, Event::InitRefParam{ param: ro })  => {
                match ro {
                    ResourceAccessPoint::Function(..) => {
                        panic!("Cannot initialize function as as valid parameter!")
                    },
                    ResourceAccessPoint::Owner(..) | ResourceAccessPoint::MutRef(..) => {
                        State::FullPrivilege
                    },
                    ResourceAccessPoint::Struct(..) => {
                        State::FullPrivilege
                    },
                    ResourceAccessPoint::StaticRef(..) => {
                        State::PartialPrivilege {
                            borrow_count: 1,
                            borrow_to: [ResourceTy::Value(ro.to_owned())].iter().cloned().collect()
                        }
                    }
                }
            },

            (State::FullPrivilege, Event::Move{to: to_ro,..}) =>
                State::ResourceMoved{ move_to: to_ro.to_owned(), move_at_line: event_line },

            (State::ResourceMoved{ .. }, Event::Acquire{ .. }) => {
                if self.is_mut(hash) {
                    State::FullPrivilege
                }
                else { // immut variables cannot reacquire resource
                    eprintln!("Immutable variable {} cannot reacquire resources!", self.get_name_from_hash(hash).unwrap());
                    std::process::exit(1);
                }
            },

            (State::FullPrivilege, Event::MutableLend{ to: to_ro ,..}) => {
            // Assumption: variables can lend mutably if
            // 1) variable instance is mutable or 2) variable is a mutable reference
            // Use cases: 'mutable_borrow' & 'nll_lexical_scope_different'
                if self.is_mut(hash) | self.is_mutref(hash) {
                    State::RevokedPrivilege{ to: ResourceTy::Anonymous, borrow_to: to_ro.to_owned(), prev_state: Box::from(previous_state.to_owned()) }
                } else {
                    State::Invalid
                }
            },
            
            // happends when a mutable reference returns, invalid otherwise
            (State::FullPrivilege, Event::MutableDie{ .. }) =>
                State::OutOfScope,

            (State::FullPrivilege, Event::Acquire{ from: _ ,..}) | (State::FullPrivilege, Event::Copy{ from: _ ,..}) => {
                    State::FullPrivilege
            },

            (State::FullPrivilege, Event::OwnerGoOutOfScope) =>
                State::OutOfScope,

            (State::FullPrivilege, Event::RefGoOutOfScope) =>
                State::OutOfScope,

            (State::FullPrivilege, Event::StaticLend{ to: to_ro ,..}) =>
                State::PartialPrivilege {
                    borrow_count: 1,
                    borrow_to: [(to_ro.to_owned())].iter().cloned().collect() // we assume there is no borrow_to:None
                },

            (State::PartialPrivilege{ .. }, Event::MutableLend{ to: to_ro, .. }) => 
            State::RevokedPrivilege { to: ResourceTy::Anonymous, borrow_to: to_ro.to_owned(), prev_state: Box::from(previous_state.to_owned()) },

            (State::PartialPrivilege{ borrow_count: current, borrow_to }, Event::StaticLend{ to: to_ro ,..}) => {
                let mut new_borrow_to = borrow_to.clone();
                // Assume can not lend to None
                new_borrow_to.insert(to_ro.to_owned());
                State::PartialPrivilege {
                    borrow_count: current+1,
                    borrow_to: new_borrow_to,
                }
            }
                
            // self statically borrowed resource, and it returns; TODO what about references to self?
            (State::PartialPrivilege{ .. }, Event::StaticDie{ .. }) =>
                State::OutOfScope,

            // TODO: checking borrow count in states is unecessary, update later
            (State::PartialPrivilege{ borrow_count, borrow_to }, Event::StaticReacquire{ from: ro ,..}) => {
                let new_borrow_count = borrow_count - 1;
                // check if it resumes to full privilege    
                if borrow_count - 1 == 0 {
                        State::FullPrivilege 
                    } else {
                        let mut new_borrow_to = borrow_to.clone();
                        // TODO ro.unwrap() should not panic, because Reacquire{from: None} is not possible
                        // TODO change to Reaquire{from: ResourceAccessPoint}
                        assert_eq!(new_borrow_to.remove(&ro.to_owned()), true); // borrow_to must contain ro
                        State::PartialPrivilege{
                            borrow_count: new_borrow_count,
                            borrow_to: new_borrow_to,
                        }
                    }
                }

            (State::PartialPrivilege { borrow_count, borrow_to }, Event::RefDie { from: ro, num_curr_borrowers, .. })=> {
              State::PartialPrivilege { borrow_count: *num_curr_borrowers as u32, borrow_to: borrow_to.clone()}
            }

            (State::PartialPrivilege{ .. }, Event::OwnerGoOutOfScope) =>
                State::OutOfScope,

            (State::PartialPrivilege{ .. }, Event::RefGoOutOfScope) =>
                State::OutOfScope,

            (State::RevokedPrivilege{ prev_state: p,.. }, Event::MutableReacquire{ .. }) => {
              *p.clone()
            }

            (State::FullPrivilege, Event::StaticDie { .. }) | 
            (State::FullPrivilege, Event::StaticBorrow { .. }) => {
              State::FullPrivilege
            }
            (_, Event::Duplicate { to: ResourceTy::Caller,..}) => State::OutOfScope,
            (_, Event::Duplicate { .. }) => (*previous_state).clone(),

            // Branching logic

            //
            (_, Event::Branch { .. }) => {
                State::OutOfScope
                // let mut branch_states: Vec<State> = Vec::new();
                // // get the last state in each of the branches
                // for branch in branch_history {
                //     let last_state = self.get_states(hash, &branch.e_data, Some(previous_state.clone())).last().unwrap().2.clone();
                //     branch_states.push(last_state);
                // }

                // // compute the least upper bound of all the last states
                // (*previous_state).clone()
            }
            

            (_, _) => State::Invalid,
        }
    }

    fn get_states(&self, hash: &u64, 
        history: & Vec<(usize, Event)>, 
        p_state: Option<State>, 
        range: Option<(usize, usize)>) -> Vec::<(usize, usize, State)> {
        match history.is_empty() {
            false => {
                let mut states = Vec::<(usize, usize, State)>::new();
                let mut previous_line_number: usize = 1;
                let mut prev_state = match p_state.clone() {
                    Some(p) => p, 
                    None => State::OutOfScope
                };
                for (i, (line_number, event)) in history.iter().enumerate() {
                    if i != 0 || p_state.is_none() {
                        states.push(
                            (previous_line_number, *line_number, prev_state.clone())
                        );
                    }
                    match event {
                        Event::Branch {branch_history,  split_point, merge_point, .. } => {
                            // add a new state for when branch is merged
                            states.push((*split_point, *merge_point, State::OutOfScope));
                            // get the last state in each of the branches
                            let mut branch_states: Vec<State> = Vec::new();
                            for branch in branch_history {
                                let last_state = 
                                self.get_states(hash, &branch.e_data, Some(prev_state.clone()), Some((*split_point, *merge_point)))
                                    .last().unwrap().2.clone();
                                let b_states =  self.get_states(hash, &branch.e_data, Some(prev_state.clone()), Some((*split_point, *merge_point)));
                                branch_states.push(last_state);
                            }

                            // compute the least upper bound across all of the previous states - this is the same as sorting
                            branch_states.sort();
                            prev_state = branch_states.first().unwrap().clone();
                            previous_line_number = *merge_point + 1; // merge one line after the curly brace
                        }
                        _ => {
                            prev_state = self.calc_state(&prev_state, &event, *line_number, hash);
                            previous_line_number = *line_number;
                        }
                    }
                }
                // if this timeline is part of a branch we need to extend it to the merge point
                match range {
                    Some((s, m)) => {
                        // likewise we need to extend the first state up to the split point
                        if !states.is_empty() {
                            states.first_mut().unwrap().0 = s + 1; // start timelines one line after opening curly brace
                            states.push(
                                (previous_line_number, m, prev_state.clone())
                            );
                        }
                        else {
                            states.push(
                                (s + 1, m, prev_state.clone())
                            );

                        }
                    }
                    None => { // for when an owner goes out of scope normally 
                        states.push(
                            (previous_line_number, previous_line_number, prev_state.clone())
                        );
                    }
                }
                states
            }
            true => {
                match range {
                    Some((s, m)) => {
                        vec![(s + 1, m, p_state.unwrap())]
                    }
                    None => panic!("shouldn't be here, a range must be specified")
                }
            }
        }
    }

    fn get_state(&self, hash: &u64, _line_number: &usize) -> Option<State> {
        // TODO: the line_number variable should be used to determine state here
        match self.timelines.get(hash) {
            Some(_timeline) => {
                // example return value
                Some(State::OutOfScope)
            },
            _ => None
        }
    }

    // WARNING do not call this when making visualization!! 
    // use append_external_event instead
    fn _append_event(&mut self, resource_access_point: &ResourceAccessPoint, event: Event, line_number: &usize) {
        let hash = &resource_access_point.hash();
        // if this event belongs to a new ResourceAccessPoint hash,
        // create a new Timeline first, thenResourceAccessPoint bind it to the corresponding hash.
        match self.timelines.get(hash) {
            None => {
                let timeline = Timeline {
                    resource_access_point: resource_access_point.clone(),
                    history: Vec::new(),
                };
                self.timelines.insert(**hash, timeline);
            },
            _ => {}
        }

        // append the event to the end of the timeline of the corresponding hash
        match self.timelines.get_mut(hash) {
            Some(timeline) => {
                timeline.history.push(
                    (*line_number, event)
                );
            },
            _ => {
                panic!("Timeline disappeared right after creation or when we could index it. This is impossible.");
            }
        }
    }


    // store them in external_events, and call append_events
    // default way to record events
    fn append_processed_external_event(&mut self, event: ExternalEvent, line_number: usize, collection: & mut Option<Vec<(usize, Event)>>) {
        if collection.is_none() { // don't double count events inside of Branch blocks
            self.external_events.push((line_number, event.clone()));
        }
        
        // append_event if resource_access_point is not null
        fn maybe_append_event (vd: & mut VisualizationData, resource_ty: &ResourceTy, event: Event, line_number: usize, collection: & mut Option<Vec<(usize, Event)>>){
          match resource_ty {
            ResourceTy::Anonymous | ResourceTy::Caller => {}
            ResourceTy::Deref(r) | ResourceTy::Value(r) => {
              match collection {
                Some(v) => v.push((line_number, event)),
                None => vd._append_event(&r, event, &line_number)
              }
            }
          }
        }

        match event {
            // eg let ro_to = String::from("");
            ExternalEvent::Move{from: from_ro, to: to_ro} => {
                maybe_append_event(self, &to_ro.clone(), Event::Acquire{from : from_ro.to_owned(), is: to_ro.clone()}, line_number, collection);
                maybe_append_event(self, &from_ro.clone(), Event::Move{to : to_ro.to_owned(), is: from_ro}, line_number, collection);
            },
            // eg: let ro_to = 5;
            ExternalEvent::Bind{from: from_ro, to: to_ro} => {
                maybe_append_event(self, &to_ro.clone(), Event::Acquire{from : from_ro.to_owned(), is: to_ro.clone()}, line_number, collection);
                maybe_append_event(self, &from_ro.clone(), Event::Duplicate{to : to_ro.to_owned(), is: from_ro}, line_number, collection);
            },
            // eg: let x : i64 = y as i64;
            ExternalEvent::Copy{from: from_ro, to: to_ro} => {
                maybe_append_event(self, &to_ro.clone(), Event::Copy{from : from_ro.to_owned(), is: to_ro.clone()}, line_number, collection);
                maybe_append_event(self, &from_ro.clone(), Event::Duplicate{to : to_ro.to_owned(), is: from_ro}, line_number, collection);
            },
            ExternalEvent::Branch { live_vars, branches, branch_type, split_point, merge_point } => {
              for var in live_vars.iter() {
                let is = ResourceTy::Value(var.clone());
                let could_be = ResourceTy::Deref(var.clone());
                let mut branch_history: Vec<BranchData> = Vec::new();
                for branch in branches.iter() { // for each branch
                  let mut  branch_h: Option<Vec<(usize, Event)>> = Some(Vec::new());
                  for (l, ev) in branch.iter() { // for each event in each branch
                    let (from, to) = ResourceAccessPoint_extract(ev);
                    if *from == is || *to == is || *from == could_be || *to == could_be {
                      // append each of the events in a branch related to given live var
                      self.append_processed_external_event(ev.clone(), *l, & mut branch_h);
                    }
                  }
                  branch_history.push(BranchData { t_data: TimelineColumnData { // produce some dummy timeline data for now
                    name: "".to_owned(),
                    x_val: -1,
                    title: "".to_owned(),
                    is_ref: false,
                    is_struct_group: false,
                    is_member: false, 
                    owner: 0
                  }, e_data: branch_h.unwrap(), width: 0 });
                }
                maybe_append_event(self, &is.clone(), Event::Branch { is: is, branch_history: branch_history, ty: branch_type.clone(), split_point: split_point, merge_point: merge_point}, line_number, collection);
              }
            }
            ExternalEvent::StaticBorrow{from: from_ro, to: to_ro} => {
                maybe_append_event(self, &from_ro, Event::StaticLend{to : to_ro.to_owned(), is: from_ro.clone()}, line_number, collection);
                maybe_append_event(self, &to_ro.clone(), Event::StaticBorrow{from : from_ro.to_owned(), is: to_ro}, line_number, collection);
                
            },
            ExternalEvent::StaticDie{from: from_ro, to: to_ro} => {
                maybe_append_event(self, &to_ro.clone(), Event::StaticReacquire{from : from_ro.to_owned(), is: to_ro.clone()}, line_number, collection);
                maybe_append_event(self, &from_ro.clone(), Event::StaticDie{to : to_ro.to_owned(), is: from_ro}, line_number, collection);
            },
            ExternalEvent::MutableBorrow{from: from_ro, to: to_ro} => {
                maybe_append_event(self, &from_ro, Event::MutableLend{to : to_ro.to_owned(), is: from_ro.clone()}, line_number, collection);
                maybe_append_event(self, &to_ro.clone(), Event::MutableBorrow{from : from_ro.to_owned(), is: to_ro}, line_number, collection);
                
            },
            ExternalEvent::MutableDie{from: from_ro, to: to_ro} => {
                maybe_append_event(self, &to_ro.clone(), Event::MutableReacquire{from : from_ro.to_owned(), is: to_ro.clone()}, line_number, collection);
                maybe_append_event(self, &from_ro.clone(), Event::MutableDie{to : to_ro.to_owned(), is: from_ro}, line_number, collection);
            },
            ExternalEvent::RefDie { from: from_ro, to: to_ro , num_curr_borrowers} => { // need Ref Event to avoid drawing redundant arrows when rendering timelines
                match from_ro.clone() {
                    ResourceTy::Deref(ro_is) | ResourceTy::Value(ro_is) => {
                        if ro_is.is_mutref() {
                            maybe_append_event(self, &from_ro.clone(), Event::MutableDie { to: ResourceTy::Anonymous, is: from_ro.clone() }, line_number, collection);
                            //maybe_append_event(self, &to_ro.clone(), Event::MutableReacquire { from: from_ro, is: to_ro }, &line_number);
                        }
                        else {
                            maybe_append_event(self, &from_ro.clone(), Event::StaticDie { to: ResourceTy::Anonymous, is: from_ro.clone() }, line_number, collection);
                            maybe_append_event(self, &to_ro.clone(), Event::RefDie { from: from_ro, is: to_ro, num_curr_borrowers}, line_number, collection);
                        }
                    }
                    _ => panic!("not possible")
                }
            },

            // TODO do we really need to add these events, since pass by ref does not change the state?
            ExternalEvent::PassByStaticReference{from: from_ro, to: to_ro} => {
                // maybe_append_event(self, &from_ro.to_owned(), Event::StaticLend{to : to_ro.to_owned(), is: from_ro.clone()}, line_number, collection);
                // maybe_append_event(self, &to_ro.to_owned(), Event::StaticBorrow{from : from_ro.to_owned(), is: to_ro.clone()}, line_number, collection);
                // maybe_append_event(self, &from_ro, Event::StaticReacquire{from : to_ro.to_owned(), is: from_ro.clone()}, line_number, collection);
                // maybe_append_event(self, &to_ro.clone(), Event::StaticDie{to : from_ro.to_owned(), is: to_ro}, line_number, collection);
            },
            ExternalEvent::PassByMutableReference{from: from_ro, to: to_ro} => {
                // maybe_append_event(self, &from_ro.clone(), Event::MutableLend{to : to_ro.to_owned(), is: from_ro.clone()}, line_number, collection);
                // maybe_append_event(self, &to_ro.clone(), Event::MutableBorrow{from : from_ro.to_owned(), is: to_ro.clone()}, line_number, collection);
                // maybe_append_event(self, &from_ro.clone(), Event::MutableReacquire{from : to_ro.to_owned(), is: from_ro.clone()}, line_number, collection);
                // maybe_append_event(self, &to_ro.clone(), Event::MutableDie{to : from_ro.to_owned(), is: to_ro}, line_number, collection);
            },
            ExternalEvent::InitRefParam{param: ro} => {
                maybe_append_event(self, &ResourceTy::Value(ro.clone()), Event::InitRefParam{param : ro.to_owned()}, line_number, collection);
            },
            ExternalEvent::GoOutOfScope{ro} => {
                match ro {
                    ResourceAccessPoint::Owner(..) | ResourceAccessPoint::Struct(..) => {
                        maybe_append_event(self, &ResourceTy::Value(ro), Event::OwnerGoOutOfScope, line_number, collection);
                    },
                    ResourceAccessPoint::MutRef(..) | ResourceAccessPoint::StaticRef(..)=> {
                        maybe_append_event(self, &ResourceTy::Value(ro), Event::RefGoOutOfScope, line_number, collection);
                    },
                    ResourceAccessPoint::Function(func) => {
                        panic!(
                            "Functions do not go out of scope! We do not expect to see \"{}\" here.",
                            func.name
                        );
                    }
                }
            },
        }
    }
}

/* TODO use this function to create a single copy of resource owner in resource_access_point_map,
 and use hash to refer to it */ 
// impl VisualizationData {
//     fn create_resource_access_point(&mut self, ro: ResourceAccessPoint) -> &ResourceAccessPoint {
//         self.resource_access_point_map.entry(ro.get_hash()).or_insert(ro);
//         self.resource_access_point_map.get(ro.get_hash())
//     }
// }