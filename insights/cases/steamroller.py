#!/usr/bin/env python3
"""
Self-contained translation of the Steamroller-style N3 into a tiny disjunctive rule engine.

We model triples (s, p, o) with variables like "?X". Rules have preconditions and
disjunctive cases. Deterministic rules are just single-case rules. We do:

1) Load the initial facts from your N3.
2) Saturate *deterministic* rules to a fixpoint (types, 'smaller', bird→caterpillar).
3) Use DFS with backtracking over *disjunctive* choices to find models satisfying:

   Goal (your query):
     ?X a :Animal.
     ?Y a :Animal.
     ?X :eats ?Y.
     ?Z a :Grain.
     ?Y :eats ?Z.

Key modeling notes
------------------
• Existentials like "{?X a :Snail} => {?X :eats [ a :Plant ]}":
  We implement as a nondeterministic choice between eating the known :grain
  or a fresh symbol :somePlant (declared a :Plant). This is enough to witness
  all goal-relevant models.

• The general “big eats or herbivory” rule:
    {?X a :Animal. ?Y a :Animal. ?Y :smaller ?X.
     ?U a :Plant. ?V a :Plant. ?Y :eats ?V}
    => ($ {?X :eats ?U} {?X :eats ?Y} $).
  We bind U,V to known plants in the world (here :grain and :somePlant),
  and branch over the two cases.

• Constraints “=> false.” are enforced by rejecting any world where a match occurs:
    Wolf eats Fox      (forbidden)
    Wolf eats Grain    (forbidden)
    Bird eats Snail    (forbidden)

Implementation details
----------------------
• Breadth-first search (BFS) over *worlds* (sets of triples), not over rule sequences,
  so we find minimal-choice models and terminate as soon as we’ve enumerated all of them.
• Monotonic expansion: successors must add at least one **new** triple; this prevents cycles.
• Deterministic saturation at each step to a fixpoint.
• Constraint pruning and a safety cap on explored nodes.

What we print
-------------
• Answer: whether models exist; we list minimal-choice models and show one or more
  concrete solutions (X eats Y; Y eats grain).
• Reason: a plain-English sketch of how the derivations work.
• Check (harness): enumeration details and a few sanity checks.
"""

from dataclasses import dataclass
from collections import deque, defaultdict
from typing import List, Tuple, Dict, Set, Optional, FrozenSet
import ast

Triple = Tuple[str, str, str]  # (subject, predicate, object)

# -------------------- Unification / matching --------------------

def is_var(x: str) -> bool:
    return x.startswith("?")

def subst_term(t: str, b: Dict[str, str]) -> str:
    return b.get(t, t) if is_var(t) else t

def subst_triple(tr: Triple, b: Dict[str, str]) -> Triple:
    s, p, o = tr
    return (subst_term(s, b), subst_term(p, b), subst_term(o, b))

def unify_triple_to_fact(pat: Triple, fact: Triple, b: Dict[str, str]) -> Optional[Dict[str, str]]:
    s1, p1, o1 = pat
    s2, p2, o2 = fact
    nb = dict(b)
    for a, bnd in ((s1, s2), (p1, p2), (o1, o2)):
        if is_var(a):
            if a in nb and nb[a] != bnd:
                return None
            nb[a] = bnd
        else:
            if a != bnd:
                return None
    return nb

def all_matches(preconds: List[Triple], world: Set[Triple]) -> List[Dict[str, str]]:
    results: List[Dict[str, str]] = []
    preconds = preconds[:]
    def backtrack(i: int, b: Dict[str, str]):
        if i == len(preconds):
            results.append(b)
            return
        pat = preconds[i]
        for fact in world:
            nb = unify_triple_to_fact(pat, fact, b)
            if nb is not None:
                backtrack(i + 1, nb)
    backtrack(0, {})
    return results

# -------------------- Rule representation --------------------

@dataclass(frozen=True)
class Case:
    name: str
    adds: List[Triple]

@dataclass(frozen=True)
class Rule:
    name: str
    preconds: List[Triple]
    cases: Tuple[Case, ...]   # one case => deterministic rule

# -------------------- Facts from the N3 --------------------

W0: Set[Triple] = {
    (":wolf", "a", ":Wolf"),
    (":fox", "a", ":Fox"),
    (":bird", "a", ":Bird"),
    (":snail", "a", ":Snail"),
    (":caterpillar", "a", ":Caterpillar"),
    (":grain", "a", ":Grain"),
    # helper individual to instantiate “some plant”
    (":somePlant", "a", ":Plant"),
}

# Deterministic rules (single-case)
DETERMINISTIC_RULES: List[Rule] = [
    Rule("T1_WolfIsAnimal", [("?X", "a", ":Wolf")], (Case("c", [("?X", "a", ":Animal")]),)),
    Rule("T1_FoxIsAnimal", [("?X", "a", ":Fox")], (Case("c", [("?X", "a", ":Animal")]),)),
    Rule("T1_BirdIsAnimal", [("?X", "a", ":Bird")], (Case("c", [("?X", "a", ":Animal")]),)),
    Rule("T1_SnailIsAnimal", [("?X", "a", ":Snail")], (Case("c", [("?X", "a", ":Animal")]),)),
    Rule("T1_CaterIsAnimal", [("?X", "a", ":Caterpillar")], (Case("c", [("?X", "a", ":Animal")]),)),
    Rule("T1_GrainIsPlant", [("?X", "a", ":Grain")], (Case("c", [("?X", "a", ":Plant")]),)),

    Rule("S_cater_lt_bird", [("?X", "a", ":Caterpillar"), ("?Y", "a", ":Bird")], (Case("c", [("?X", ":smaller", "?Y")]),)),
    Rule("S_snail_lt_bird",  [("?X", "a", ":Snail"),       ("?Y", "a", ":Bird")], (Case("c", [("?X", ":smaller", "?Y")]),)),
    Rule("S_bird_lt_fox",    [("?X", "a", ":Bird"),        ("?Y", "a", ":Fox")],  (Case("c", [("?X", ":smaller", "?Y")]),)),
    Rule("S_fox_lt_wolf",    [("?X", "a", ":Fox"),         ("?Y", "a", ":Wolf")], (Case("c", [("?X", ":smaller", "?Y")]),)),

    Rule("Bird_eats_caterpillar",
         [("?X", "a", ":Bird"), ("?Y", "a", ":Caterpillar")],
         (Case("c", [("?X", ":eats", "?Y")]),)),
]

# Disjunctive rules (choices)
DISJ_RULES: List[Rule] = [
    # Existentials “eats a Plant” -> choose :grain or :somePlant
    Rule("Snail_eats_Plant",
         [("?X", "a", ":Snail")],
         (Case("grain", [("?X", ":eats", ":grain")]),
          Case("some",  [("?X", ":eats", ":somePlant")]))),
    Rule("Caterpillar_eats_Plant",
         [("?X", "a", ":Caterpillar")],
         (Case("grain", [("?X", ":eats", ":grain")]),
          Case("some",  [("?X", ":eats", ":somePlant")]))),
    # The “big” either/or rule
    Rule("Big_rule",
         [("?X", "a", ":Animal"),
          ("?Y", "a", ":Animal"),
          ("?Y", ":smaller", "?X"),
          ("?U", "a", ":Plant"),
          ("?V", "a", ":Plant"),
          ("?Y", ":eats", "?V")],
         (Case("X_eats_U", [("?X", ":eats", "?U")]),
          Case("X_eats_Y", [("?X", ":eats", "?Y")]))),
]

# Constraints (=> false.)
def violates_constraints(world: Set[Triple]) -> bool:
    # Wolf eats Fox
    if all_matches([("?X", "a", ":Wolf"), ("?Y", "a", ":Fox"), ("?X", ":eats", "?Y")], world):
        return True
    # Wolf eats Grain
    if all_matches([("?X", "a", ":Wolf"), ("?Y", "a", ":Grain"), ("?X", ":eats", "?Y")], world):
        return True
    # Bird eats Snail
    if all_matches([("?X", "a", ":Bird"), ("?Y", "a", ":Snail"), ("?X", ":eats", "?Y")], world):
        return True
    return False

# Deterministic closure
def saturate(world: Set[Triple]) -> Set[Triple]:
    world = set(world)
    changed = True
    applied: Set[Tuple[str, Tuple[Tuple[str, str], ...]]] = set()
    while changed:
        changed = False
        for r in DETERMINISTIC_RULES:
            for b in all_matches(r.preconds, world):
                fp = (r.name, tuple(sorted(b.items())))
                if fp in applied:
                    continue
                before = len(world)
                for tr in r.cases[0].adds:
                    world.add(subst_triple(tr, b))
                if len(world) > before:
                    changed = True
                applied.add(fp)
    return world

# -------------------- BFS search (terminating) --------------------

def bfs_minimal_models(world_start: Set[Triple], max_nodes: int = 20000):
    start = saturate(world_start)
    q = deque()
    q.append((start, []))  # (world, sequence of choices)
    seen_worlds: Set[FrozenSet[Triple]] = {frozenset(start)}
    nodes = 0

    goal_preconds = [
        ("?X", "a", ":Animal"),
        ("?Y", "a", ":Animal"),
        ("?X", ":eats", "?Y"),
        (":grain", "a", ":Grain"),
        ("?Y", ":eats", ":grain"),
    ]

    def goal_bindings(w: Set[Triple]) -> List[Dict[str, str]]:
        return all_matches(goal_preconds, w)

    minimal_models = []
    found_depth = None

    while q:
        w, seq = q.popleft()
        nodes += 1
        if nodes > max_nodes:
            break  # safety cap

        gb = goal_bindings(w)
        if gb:
            if found_depth is None:
                found_depth = len(seq)
            if len(seq) == found_depth:
                minimal_models.append((seq, w, gb[0]))
            else:
                break
            continue

        if found_depth is not None and len(seq) >= found_depth:
            continue  # don't expand deeper than minimal solutions

        # Expand with genuinely novel worlds only
        for r in DISJ_RULES:
            for b in all_matches(r.preconds, w):
                for c in r.cases:
                    new_world = set(w)
                    for tr in c.adds:
                        new_world.add(subst_triple(tr, b))
                    if new_world == w:
                        continue
                    new_world = saturate(new_world)
                    if violates_constraints(new_world):
                        continue
                    fz = frozenset(new_world)
                    if fz in seen_worlds:
                        continue
                    seen_worlds.add(fz)
                    choice = f"{r.name}[{tuple(sorted(b.items()))}]:{c.name}"
                    q.append((new_world, seq + [choice]))

    return minimal_models, nodes

def enumerate_models_upto(world_start: Set[Triple], max_depth: int = 4, max_nodes: int = 50000):
    start = saturate(world_start)
    q = deque([(start, [])])
    seen_by_depth: Dict[int, Set[FrozenSet[Triple]]] = defaultdict(set)
    seen_by_depth[0].add(frozenset(start))
    nodes = 0

    goal_preconds = [
        ("?X", "a", ":Animal"),
        ("?Y", "a", ":Animal"),
        ("?X", ":eats", "?Y"),
        (":grain", "a", ":Grain"),
        ("?Y", ":eats", ":grain"),
    ]
    def goal_bindings(w: Set[Triple]): return all_matches(goal_preconds, w)

    models = []
    while q and nodes <= max_nodes:
        w, seq = q.popleft()
        nodes += 1
        for b in goal_bindings(w):
            models.append((seq, w, b))
        if len(seq) >= max_depth:
            continue
        for r in DISJ_RULES:
            for b in all_matches(r.preconds, w):
                for c in r.cases:
                    new_world = set(w)
                    for tr in c.adds:
                        new_world.add(subst_triple(tr, b))
                    if new_world == w:
                        continue
                    new_world = saturate(new_world)
                    if violates_constraints(new_world):
                        continue
                    depth = len(seq) + 1
                    fz = frozenset(new_world)
                    if fz in seen_by_depth[depth]:
                        continue
                    seen_by_depth[depth].add(fz)
                    choice = f"{r.name}[{tuple(sorted(b.items()))}]:{c.name}"
                    q.append((new_world, seq + [choice]))
    return models, nodes

# -------------------- Helpers for a prettier "Answer" --------------------

def goal_pairs_from_world(w: Set[Triple]) -> Set[Tuple[str, str]]:
    pairs = set()
    for b in all_matches([
        ("?X", "a", ":Animal"),
        ("?Y", "a", ":Animal"),
        ("?X", ":eats", "?Y"),
        ("?Y", ":eats", ":grain"),
    ], w):
        pairs.add((b["?X"], b["?Y"]))
    return pairs

def compact_choice_label(choice: str) -> str:
    # Example choice string: "Big_rule[(('?U', ':Plant'), ('?X', ':fox'), ... )]:X_eats_Y"
    try:
        rule_and_binding, case = choice.split("]:")
        rule, binding_str = rule_and_binding.split("[", 1)
        binding = dict(ast.literal_eval(binding_str))
    except Exception:
        return choice  # fallback

    def val(k): return binding.get(k, "?")
    if rule in ("Snail_eats_Plant", "Caterpillar_eats_Plant"):
        plant = ":grain" if case == "grain" else ":somePlant"
        return f"{val('?X')} eats {plant}"
    if rule == "Big_rule":
        if case == "X_eats_U":
            return f"{val('?X')} eats {val('?U')}"
        if case == "X_eats_Y":
            return f"{val('?X')} eats {val('?Y')}"
    return f"{rule}:{case}"

def pretty_steps(seq: List[str]) -> List[str]:
    return [f"- {compact_choice_label(s)}" for s in seq] if seq else ["- (no choices required)"]

def make_table(rows: List[List[str]], headers: List[str]) -> str:
    # simple ASCII table with auto column widths
    cols = list(zip(*([headers] + rows)))
    widths = [max(len(cell) for cell in col) for col in cols]
    def fmt_row(r): return " | ".join(cell.ljust(w) for cell, w in zip(r, widths))
    line = "-+-".join("-" * w for w in widths)
    out = [fmt_row(headers), line]
    out += [fmt_row(r) for r in rows]
    return "\n".join(out)

# -------------------- Run & Report --------------------

minimal, nodes_bfs = bfs_minimal_models(W0)
models_upto, nodes_enum = enumerate_models_upto(W0, max_depth=4)

# Extract unique (X,Y) pairs and minimal sequences for each
min_seq_by_pair: Dict[Tuple[str, str], List[str]] = {}
for seq, w, _ in models_upto:
    for pair in goal_pairs_from_world(w):
        if pair not in min_seq_by_pair or len(seq) < len(min_seq_by_pair[pair]):
            min_seq_by_pair[pair] = seq

def fmt_seq(seq: List[str]) -> str:
    return " → ".join(compact_choice_label(s) for s in seq) if seq else "(none)"

print("Answer")
print("------")
if not minimal:
    print("❌ No model found (within safe search bounds).")
else:
    # Collect all solutions we can show briefly
    all_solutions: Set[Tuple[str, str]] = set()
    for _, w, _ in minimal:
        all_solutions |= goal_pairs_from_world(w)
    all_solutions |= set(min_seq_by_pair.keys())

    # Build table rows
    rows = []
    for i, (X, Y) in enumerate(sorted(all_solutions), 1):
        steps = min_seq_by_pair.get((X, Y), [])
        rows.append([str(i), X, Y, "Yes", str(len(steps))])
    print("Solutions that satisfy the query (?X eats ?Y and ?Y eats :grain):")
    print()
    print(make_table(rows, headers=["#", "X (Animal)", "Y (Animal)", "Y eats :grain", "Choices"]))

    # Then show compact step lists per solution
    print("\nHow each solution is derived (minimal choices):")
    for i, (X, Y) in enumerate(sorted(all_solutions), 1):
        steps = min_seq_by_pair.get((X, Y), [])
        print(f"{i}. X={X}, Y={Y}")
        for line in pretty_steps(steps):
            print("   ", line)

print()
print("Reason")
print("------")
print("• Typing rules make each creature an :Animal and :grain a :Plant; size facts give:")
print("  :caterpillar < :bird < :fox < :wolf, and :snail < :bird.")
print("• Deterministic diet: every :Bird eats any :Caterpillar, so :bird eats :caterpillar.")
print("• Existential diet: :Snail and :Caterpillar each eat *some* :Plant; we can instantiate")
print("  that as :grain (or :somePlant).")
print("• Big disjunctive rule: if a smaller animal Y eats a plant, a larger X either eats a plant")
print("  or eats Y. Constraints forbid (:Wolf eats :Fox), (:Wolf eats :Grain), and (:Bird eats :Snail).")
print("• This yields two canonical goal patterns:")
print("  – Bird→Caterpillar→Grain: choose that the caterpillar eats :grain; then the bird (deterministic)")
print("    eats the caterpillar. (X=:bird, Y=:caterpillar)")
print("  – Fox→Bird→Grain: make a bird herbivorous via a smaller snail/caterpillar and the big rule")
print("    (choose :grain), then the fox (larger) eats the bird. (X=:fox, Y=:bird)")

print()
print("Check (harness)")
print("----------------")
print(f"• BFS nodes (minimal models): explored up to a safe cap.")
print(f"• Enumeration up to depth 4: also bounded for safety.")
# Validate each reported solution is actually present in an enumerated world
for (X, Y), seq in sorted(min_seq_by_pair.items()):
    found_world = None
    for s, w, _ in models_upto:
        if (X, Y) in goal_pairs_from_world(w):
            found_world = w
            break
    ok = found_world is not None and (X, ":eats", Y) in found_world and (Y, ":eats", ":grain") in found_world
    print(f"• Validate {X} eats {Y} and {Y} eats :grain: {'OK' if ok else 'MISSING'}")
# Constraint check on a sample minimal world
if minimal:
    def violates_constraints(world: Set[Triple]) -> bool:  # reuse earlier (shadowed for clarity)
        if all_matches([("?X", "a", ":Wolf"), ("?Y", "a", ":Fox"), ("?X", ":eats", "?Y")], world): return True
        if all_matches([("?X", "a", ":Wolf"), ("?Y", "a", ":Grain"), ("?X", ":eats", "?Y")], world): return True
        if all_matches([("?X", "a", ":Bird"), ("?Y", "a", ":Snail"), ("?X", ":eats", "?Y")], world): return True
        return False
    sample_world = minimal[0][1]
    bad = violates_constraints(sample_world)
    print("• Constraints satisfied on a minimal world:", "YES" if not bad else "NO")

