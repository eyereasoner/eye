// Goal-Driven Parallel Sequences (Prolog → C)
// Translation by ChatGPT – June 2025
// ------------------------------------------------------------
// The program reproduces (most of) the behaviour of the original
// Prolog search.  It performs a depth-first search from an initial
// state to a goal state while respecting cumulative limits on
// duration, cost, belief, comfort and number of stages.
//
// 2025-06-04 – *Update*: The code now enumerates **all** admissible
// solution paths instead of stopping after the first one.  All
// original comments have been retained; additional comments are
// clearly marked with “(added)”.
// ------------------------------------------------------------
#include <stdio.h>
#include <stdbool.h>
#include <string.h>

// ------------------------------
// Domain modelling
// ------------------------------
// Locations in the sample map
typedef enum {
    GENT,
    BRUGGE,
    KORTRIJK,
    OOSTENDE,
    LOCATION_COUNT
} Location;

// One edge in the state-space graph.
// (The Prolog code calls this a description row.)
typedef struct {
    Location from;
    Location to;
    const char *action;   // e.g. "drive_gent_brugge"
    double duration;      // milliseconds in the example
    double cost;
    double belief;
    double comfort;
} Transition;

// Sample dataset (partial road map of Belgium)
static const Transition transitions[] = {
    { GENT,      BRUGGE,   "drive_gent_brugge",      1500.0, 0.006, 0.96, 0.99 },
    { GENT,      KORTRIJK, "drive_gent_kortrijk",    1600.0, 0.007, 0.96, 0.99 },
    { KORTRIJK,  BRUGGE,   "drive_kortrijk_brugge",  1600.0, 0.007, 0.96, 0.99 },
    { BRUGGE,    OOSTENDE, "drive_brugge_oostende",   900.0, 0.004, 0.98, 1.00 }
};
static const size_t TRANSITION_COUNT = sizeof(transitions) / sizeof(transitions[0]);

// Limits structure – mirrors the 7-element list in Prolog query
typedef struct {
    double max_duration;
    double max_cost;
    double min_belief;
    double min_comfort;
    int    max_stagecount;
} Limits;

// A growing solution path – fixed upper bound for simplicity.
#define MAX_PATH_LEN 32

typedef struct {
    const Transition *edges[MAX_PATH_LEN];
    int  length;            // number of filled entries
    double duration;
    double cost;
    double belief;
    double comfort;
} Path;

// ------------------------------
// Helper for stage counting
// ------------------------------
static int stage_count(const int *maps, int len)
{
    if (len == 0) return 1;            // Prolog’s base case
    int stages = 1;
    for (int i = 1; i < len; ++i) {
        if (maps[i] != maps[i - 1])
            ++stages;
    }
    return stages;
}

// ------------------------------
// Depth-first search (core of translation)
// (updated to return the **number of solutions** found in the branch)
// ------------------------------
static int find_paths(
        Location goal,
        Location current_loc,
        const Limits *limits,
        int *maps, int maps_len,
        Path *path)
{
    // SUCCESS: goal state reached → path is complete
    if (current_loc == goal) {
        // printf("\nSolution %d:\n", 1); // placeholder; corrected in main (added)
        // print_path will be called in main – keep recursion free of I/O here (added)
        return 1;
    }

    int solutions = 0;  // (added) count solutions in this subtree

    // Iterate over all outgoing transitions from current_loc
    for (size_t i = 0; i < TRANSITION_COUNT; ++i) {
        const Transition *t = &transitions[i];
        if (t->from != current_loc)
            continue;

        // Prepare updated cumulative values
        double duration_t = path->duration + t->duration;
        if (duration_t > limits->max_duration)
            continue;

        double cost_t = path->cost + t->cost;
        if (cost_t > limits->max_cost)
            continue;

        double belief_t = path->belief * t->belief;
        if (belief_t < limits->min_belief)
            continue;

        double comfort_t = path->comfort * t->comfort;
        if (comfort_t < limits->min_comfort)
            continue;

        // Stage counting – here every edge is in the same map (map_be = 0)
        int maps_t[MAX_PATH_LEN];
        memcpy(maps_t, maps, maps_len * sizeof(int));
        maps_t[maps_len] = 0;   // map_be == 0
        int stagecount = stage_count(maps_t, maps_len + 1);
        if (stagecount > limits->max_stagecount)
            continue;

        // Extend path
        Path next = *path;  // copy current best-so-far
        next.edges[next.length++] = t;
        next.duration = duration_t;
        next.cost = cost_t;
        next.belief = belief_t;
        next.comfort = comfort_t;

        // Recurse – aggregate solution count (added)
        solutions += find_paths(goal, t->to, limits, maps_t, maps_len + 1, &next);

        // (added) If desired, we could prune here based on other criteria
    }
    return solutions;  // includes all deeper successes
}

// ------------------------------
// Pretty-printing utilities
// (unchanged apart from an "index" argument)
// ------------------------------
static void print_path(const Path *p, int index)
{
    printf("\nSolution %d:\n", index);
    printf("  Path with %d step(s):\n", p->length);
    for (int i = 0; i < p->length; ++i) {
        printf("    %d. %s\n", i + 1, p->edges[i]->action);
    }
    printf("  Totals → duration: %.1f, cost: %.3f, belief: %.3f, comfort: %.3f\n",
           p->duration, p->cost, p->belief, p->comfort);
}

// ------------------------------
// Wrapper that captures each solution as the DFS unwinds (added)
// ------------------------------
static void enumerate_paths(Location goal, Location start, const Limits *limits)
{
    Path root = {
        .edges   = {NULL},
        .length  = 0,
        .duration = 0.0,
        .cost     = 0.0,
        .belief   = 1.0,
        .comfort  = 1.0
    };

    int maps[MAX_PATH_LEN] = {0};
    int maps_len = 0;

    // We need an auxiliary stack to reconstruct solutions when they are found.
    // To avoid rewriting the DFS, we perform the traversal twice:
    //   1) First pass – count solutions (already implemented above).
    //   2) Second pass – actually print them once we know how many there are.
    // This keeps code simple and avoids dynamic allocation. (added)

    int total = find_paths(goal, start, limits, maps, maps_len, &root);

    if (total == 0) {
        puts("No path found within the given limits.");
        return;
    }

    printf("\nTotal solutions found: %d\n", total);

    // Second pass – produce the solutions in the same order.
    // We re-run DFS but now print every time we hit the goal.
    int solution_index = 0;

    // Local recursive lambda-like static function (C99 GCC/Clang extension).
    struct Rec {
        int (*fn)(struct Rec*, Location, Path*, int*, int);
    } rec;

    int rec_fn(struct Rec* self, Location loc, Path* path, int *maps_r, int maps_len_r) {
        if (loc == goal) {
            print_path(path, ++solution_index);
            return 1;
        }
        for (size_t i = 0; i < TRANSITION_COUNT; ++i) {
            const Transition *t = &transitions[i];
            if (t->from != loc)
                continue;
            double dur = path->duration + t->duration;
            double cost = path->cost + t->cost;
            double bel = path->belief * t->belief;
            double com = path->comfort * t->comfort;
            int stage;
            {
                int maps_tmp[MAX_PATH_LEN];
                memcpy(maps_tmp, maps_r, maps_len_r * sizeof(int));
                maps_tmp[maps_len_r] = 0;
                stage = stage_count(maps_tmp, maps_len_r + 1);
                if (stage > limits->max_stagecount) continue;
            }
            if (dur > limits->max_duration || cost > limits->max_cost ||
                bel < limits->min_belief || com < limits->min_comfort)
                continue;
            Path next = *path;
            next.edges[next.length++] = t;
            next.duration = dur;
            next.cost = cost;
            next.belief = bel;
            next.comfort = com;
            int maps_next[MAX_PATH_LEN];
            memcpy(maps_next, maps_r, maps_len_r * sizeof(int));
            maps_next[maps_len_r] = 0;
            self->fn(self, t->to, &next, maps_next, maps_len_r + 1);
        }
        return 0;
    }
    rec.fn = rec_fn;
    rec.fn(&rec, start, &root, maps, 0);
}

// ------------------------------
// Main driver – mirrors the sample Prolog query.
// ------------------------------
int main(void)
{
    Location start = GENT;
    Location goal  = OOSTENDE;

    Limits limits = {
        .max_duration   = 5000.0,
        .max_cost       = 5.0,
        .min_belief     = 0.2,
        .min_comfort    = 0.4,
        .max_stagecount = 1
    };

    enumerate_paths(goal, start, &limits);
    return 0;
}

