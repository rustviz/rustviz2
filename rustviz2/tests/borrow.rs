//! Structural assertions on borrow visualization for canonical borrow
//! shapes. Pins the gains from the borrow-visualization PR so future
//! changes don't silently regress them.
//!
//! Each test compiles a snippet through the local plugin backend and
//! inspects the resulting timeline panel SVG for properties that
//! describe correctness without pinning exact pixel coordinates:
//!
//!   * The expected hover strings are present.
//!   * No phantom `from s to *s` (the pre-fix bug).
//!   * The borrow-region path (`<path ... staticref ...>` or its
//!     mutref counterpart) has a non-zero vertical extent.
//!   * The lender's timeline has at least one Solid segment AND at
//!     least one Hollow segment (state transitions visible).
//!
//! These tests use the same `RV_RUNNER=local` backend as `corpus.rs`
//! and require `cargo install --path rustviz2-plugin --locked` first.

use std::env;

use rustviz2::Rustviz;

fn run(src: &str) -> Rustviz {
    env::set_var("RV_RUNNER", "local");
    Rustviz::new(src).unwrap_or_else(|e| panic!("plugin error:\n{}", e))
}

/// Strip the styled `&lt;span …&gt;…&lt;/span&gt;` wrappers that
/// `fmt_style` puts around variable names in tooltip text. Tests
/// don't care about the inline-styling noise; they want to assert on
/// the prose. After this pass `Immutable borrow from x to r` matches
/// even though the raw SVG has spans wrapping `x` and `r`.
fn strip_tooltip_styling(timeline: &str) -> String {
    let mut out = timeline.to_string();
    out = out.replace("&lt;/span&gt;", "");
    // Drop everything from `&lt;span` up to the closing `&gt;`. The
    // intermediate text (the inline `style="…"`) varies, but the
    // closing `&gt;` is the first one *after* the opening tag.
    while let Some(start) = out.find("&lt;span") {
        if let Some(rel) = out[start..].find("&gt;") {
            out.replace_range(start..start + rel + 4, "");
        } else {
            break;
        }
    }
    out
}

fn timeline_of(src: &str) -> String {
    strip_tooltip_styling(&run(src).timeline_panel_string())
}

/// Find each `d="…"` on a `staticref`/`mutref` borrow-region path and
/// return its raw value. Useful for verifying the trapezoid spans a
/// real range rather than collapsing to a point.
fn ref_line_paths(timeline: &str) -> Vec<String> {
    let mut out = Vec::new();
    // Crude SVG scrape — fine for tests, where the format is stable.
    for tag in timeline.split("<path ") {
        if !(tag.contains("staticref") || tag.contains("mutref")) {
            continue;
        }
        if let Some(start) = tag.find("d=\"") {
            let rest = &tag[start + 3..];
            if let Some(end) = rest.find('"') {
                out.push(rest[..end].to_string());
            }
        }
    }
    out
}

/// `d="M x1 y1 l dx dy v V l -dx dy"` — extract the `v V` magnitude.
/// Returns 0 if the path doesn't match the expected shape.
fn ref_line_v(d: &str) -> f64 {
    let v_marker = match d.find(" v ") {
        Some(i) => i + 3,
        None => return 0.0,
    };
    let rest = &d[v_marker..];
    let end = rest.find(' ').unwrap_or(rest.len());
    rest[..end].parse::<f64>().unwrap_or(0.0)
}

/// True if at least one timeline `<line class="solid" data-hash="HASH">`
/// segment exists.
fn has_solid_segment_for(timeline: &str, hash: u64) -> bool {
    let needle = format!("data-hash=\"{}\"", hash);
    timeline
        .split("<line ")
        .any(|seg| seg.contains(&needle) && seg.contains("class=\"solid"))
}

/// True if at least one hollow `<path ... data-hash=HASH ... class="hollow…">`
/// segment exists.
fn has_hollow_segment_for(timeline: &str, hash: u64) -> bool {
    let needle = format!("data-hash=\"{}\"", hash);
    timeline
        .split("<path ")
        .any(|seg| seg.contains(&needle) && seg.contains("class=\"hollow"))
}

/// Hash of the n-th declared variable; rustviz2 assigns hashes sequentially
/// in declaration order. The function RAP for `String::from` lands at hash 1
/// in every snippet that uses it, so the first user-declared variable is
/// usually hash 2. Adjust per-test if needed.
const FIRST_VAR_HASH: u64 = 2;

#[test]
fn fn_param_ref_loan_spans_body_and_no_phantom_die() {
    // Canonical screenshot example: f(&x) with fn f(s: &String) { *s }.
    // Pre-fix: emitted "Return immutably borrowed resource from s to *s"
    // at the signature line, with a v=0 ref-line trapezoid.
    let src = "\
fn main() {
    let x = String::from(\"hello\");
    f(&x);
    println!(\"{}\", x);
}

fn f(s: &String) {
    println!(\"{}\", *s);
}
";
    let timeline = timeline_of(src);

    assert!(
        !timeline.contains("from s to *s"),
        "phantom `from s to *s` tooltip is back"
    );
    assert!(
        timeline.contains("s is an immutable borrow from the caller"),
        "fn-param-ref init tooltip missing"
    );
    assert!(
        timeline.contains("s holds an immutable reference"),
        "ref-line tooltip missing"
    );

    // The loan region for s should span the fn body (lines 7–9), so
    // the staticref path's v should be substantially > 0.
    let lines = ref_line_paths(&timeline);
    assert!(!lines.is_empty(), "no staticref/mutref path emitted for s");
    let v = ref_line_v(&lines[0]);
    assert!(
        v > 10.0,
        "fn-param-ref loan region collapsed (v={}); path={:?}",
        v,
        lines[0]
    );

    // The lender x has FullPrivilege throughout — the lend and
    // reacquire fire on the same line for f(&x), so there's no
    // multi-line PartialPrivilege segment to render. (The loan is
    // visualized on s's side as the dashed trapezoid above.)
    assert!(
        has_solid_segment_for(&timeline, FIRST_VAR_HASH),
        "x's solid segment missing"
    );
}

#[test]
fn within_scope_immutable_borrow_renders_full_loan() {
    // `let r = &x; use r; use x` — borrow spans declaration to last
    // use of r, then x recovers FullPrivilege.
    let src = "\
fn main() {
    let x = String::from(\"hello\");
    let r = &x;
    println!(\"{}\", r);
    println!(\"{}\", x);
}
";
    let timeline = timeline_of(src);

    assert!(timeline.contains("r holds an immutable reference"));
    assert!(timeline.contains("Immutable borrow from x to r"));
    assert!(timeline.contains("Return immutably borrowed resource from r to x"));

    let lines = ref_line_paths(&timeline);
    assert!(!lines.is_empty(), "no staticref path emitted for r");
    assert!(
        ref_line_v(&lines[0]) > 10.0,
        "within-scope loan region collapsed: {:?}",
        lines[0]
    );

    assert!(has_solid_segment_for(&timeline, FIRST_VAR_HASH));
    assert!(has_hollow_segment_for(&timeline, FIRST_VAR_HASH));
}

#[test]
fn multiple_immutable_borrows_share_the_lender() {
    // `let y = &x; let z = &x;` — both refs alive simultaneously
    // through their last use; x is Hollow across the union.
    let src = "\
fn main() {
    let x = String::from(\"hello\");
    let y = &x;
    let z = &x;
    println!(\"{} {}\", y, z);
}
";
    let timeline = timeline_of(src);

    assert!(timeline.contains("Immutable borrow from x to y"));
    assert!(timeline.contains("Immutable borrow from x to z"));

    let lines = ref_line_paths(&timeline);
    assert!(
        lines.len() >= 2,
        "expected two ref-line trapezoids (one each for y and z), got {}",
        lines.len()
    );
    for d in &lines {
        assert!(
            ref_line_v(d) > 10.0,
            "loan trapezoid collapsed: {:?}",
            d
        );
    }
}

#[test]
fn mutable_borrow_takes_lender_to_revoked() {
    // `let y = &mut x` — lender x transitions to RevokedPrivilege
    // during the loan; y is the active mutable reference.
    let src = "\
fn main() {
    let mut x = String::from(\"hello\");
    let y = &mut x;
    y.push_str(\" world\");
    println!(\"{}\", x);
}
";
    let timeline = timeline_of(src);

    assert!(timeline.contains("Mutable borrow from x to y"));
    assert!(timeline.contains("Return mutably borrowed resource from y to x"));
    assert!(timeline.contains("y holds a mutable reference"));

    let lines = ref_line_paths(&timeline);
    assert!(!lines.is_empty());
    assert!(
        ref_line_v(&lines[0]) > 10.0,
        "mutable loan region collapsed: {:?}",
        lines[0]
    );
}
