// -------------------------------------------------------------------------
// Goal driven Parallel Sequences -- Jos De Roo
// See background paper https://www.sciencedirect.com/science/article/pii/S1532046421000794
//
// This Rust program is a faithful translation of the Prolog code that
//  * searches a finite state space
//  * obeys resource limits (duration, cost, belief, comfort, stage count)
//  * returns **all** paths from an initial state to a goal state.
// -------------------------------------------------------------------------

use std::fmt;

// -------------------------------------------------------------------------
// % find paths in the state space from initial state to goal state within limits
//
// In Rust we model each Prolog 'description/2' fact as a `Transition` value.
// -------------------------------------------------------------------------
#[derive(Clone)]
struct Transition {
    map: &'static str,
    from: &'static str,
    to:   &'static str,
    action: &'static str,
    duration: f64,
    cost:     f64,
    belief:   f64,
    comfort:  f64,
}

#[derive(Clone, Copy)]
struct Limits {
    max_duration:  f64,
    max_cost:      f64,
    min_belief:    f64,
    min_comfort:   f64,
    max_stagecnt:  usize,
}

#[derive(Clone)]
struct PathResult {
    path:     Vec<&'static str>,
    duration: f64,
    cost:     f64,
    belief:   f64,
    comfort:  f64,
}

impl fmt::Display for PathResult {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Path   : {:?}", self.path)?;
        writeln!(f, "  Time   : {} s", self.duration)?;
        writeln!(f, "  Cost   : €{}", self.cost)?;
        writeln!(f, "  Belief : {:.2}", self.belief)?;
        writeln!(f, "  Comfort: {:.2}", self.comfort)
    }
}

// -------------------------------------------------------------------------
// % counting the number of stages (a stage is a sequence of steps in
// % the same map)
//
// Direct translation of the `stagecount/2` helper predicate.
// -------------------------------------------------------------------------
fn stage_count(maps: &[&str]) -> usize {
    if maps.is_empty() {
        1 // Prolog base case `stagecount([], 1).`
    } else {
        1 + maps.windows(2).filter(|pair| pair[0] != pair[1]).count()
    }
}

// -------------------------------------------------------------------------
// Recursive depth-first search that collects **all** paths
// (Prolog predicate `findpaths/13` without the cut).
// -------------------------------------------------------------------------
#[allow(clippy::too_many_arguments)]
fn dfs_all(
    current_state: &str,
    goal_state: &str,
    maps_so_far: &mut Vec<&'static str>,
    path_so_far: &mut Vec<&'static str>,
    acc_duration: f64,
    acc_cost: f64,
    acc_belief: f64,
    acc_comfort: f64,
    limits: &Limits,
    transitions: &[Transition],
    results: &mut Vec<PathResult>,
) {
    // -----------------------------------------------------------------
    // findpaths(_Maps, Goal, Path, Duration, Cost, Belief, Comfort,
    //           Path, Duration, Cost, Belief, Comfort, _Limits) :-
    //     Goal,
    //     !.
    //
    // Rust: goal test
    // -----------------------------------------------------------------
    if current_state == goal_state {
        results.push(PathResult {
            path: path_so_far.clone(),
            duration: acc_duration,
            cost: acc_cost,
            belief: acc_belief,
            comfort: acc_comfort,
        });
        return; // keep looking for alternative solutions
    }

    // -----------------------------------------------------------------
    // Second clause of findpaths/13: choose a description/2 fact,
    // check resource limits, recurse, and back-track on failure.
    // -----------------------------------------------------------------
    for t in transitions.iter().filter(|t| t.from == current_state) {
        let dur_t = acc_duration + t.duration;
        if dur_t > limits.max_duration { continue; }

        let cost_t = acc_cost + t.cost;
        if cost_t > limits.max_cost { continue; }

        let belief_t = acc_belief * t.belief;
        if belief_t < limits.min_belief { continue; }

        let comfort_t = acc_comfort * t.comfort;
        if comfort_t < limits.min_comfort { continue; }

        maps_so_far.push(t.map);
        if stage_count(maps_so_far) > limits.max_stagecnt {
            maps_so_far.pop();         // undo push
            continue;                  // prune this branch
        }

        path_so_far.push(t.action);
        dfs_all(
            t.to,
            goal_state,
            maps_so_far,
            path_so_far,
            dur_t,
            cost_t,
            belief_t,
            comfort_t,
            limits,
            transitions,
            results,
        );
        path_so_far.pop();             // back-tracking
        maps_so_far.pop();
    }
}

// -------------------------------------------------------------------------
// Public wrapper – corresponds to Prolog `findpath/7` but returns *all* paths
// -------------------------------------------------------------------------
fn find_paths(
    start_state: &'static str,
    goal_state:  &'static str,
    limits: Limits,
    transitions: &[Transition],
) -> Vec<PathResult> {
    let mut results = Vec::new();
    dfs_all(
        start_state,
        goal_state,
        &mut Vec::new(),
        &mut Vec::new(),
        0.0, 0.0, 1.0, 1.0,
        &limits,
        transitions,
        &mut results,
    );
    results
}

// -------------------------------------------------------------------------
// % test data: partial map of Belgium
// :- dynamic(description/2).
//
// These are literal translations of the `description/2` facts.
// -------------------------------------------------------------------------
const MAP_BE: &[Transition] = &[
    Transition { map: "map_be", from: "gent",      to: "brugge",  action: "drive_gent_brugge",     duration: 1500.0, cost: 0.006, belief: 0.96, comfort: 0.99 },
    Transition { map: "map_be", from: "gent",      to: "kortrijk",action: "drive_gent_kortrijk",   duration: 1600.0, cost: 0.007, belief: 0.96, comfort: 0.99 },
    Transition { map: "map_be", from: "kortrijk",  to: "brugge",  action: "drive_kortrijk_brugge", duration: 1600.0, cost: 0.007, belief: 0.96, comfort: 0.99 },
    Transition { map: "map_be", from: "brugge",    to: "oostende",action: "drive_brugge_oostende", duration:  900.0, cost: 0.004, belief: 0.98, comfort: 1.00 },
];

// -------------------------------------------------------------------------
// % current state
// location(i1, gent) :+ true.
//
// % query
// true :+ findpath(...).
//
// Rust equivalent demo.
// -------------------------------------------------------------------------
fn main() {
    let start = "gent";
    let goal  = "oostende";

    let limits = Limits {
        max_duration: 5000.0,
        max_cost:     5.0,
        min_belief:   0.2,
        min_comfort:  0.4,
        max_stagecnt: 1,
    };

    let mut solutions = find_paths(start, goal, limits, MAP_BE);
    solutions.sort_by(|a, b| a.duration.total_cmp(&b.duration)); // cosmetic order

    if solutions.is_empty() {
        println!("No path satisfies the given limits.");
    } else {
        for (i, s) in solutions.iter().enumerate() {
            println!("Solution #{i}:\n{s}");
        }
    }
}
