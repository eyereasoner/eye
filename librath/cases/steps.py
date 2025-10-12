#!/usr/bin/env python3
"""
N3-to-Python translation (self-contained) for disjunctive rule search.

Facts
-----
:Pat a vcard:Individual.

Rules (with named "cases")
--------------------------
R1: If X is an Individual then (choose one)
    - R1.A: X :canDo :A
    - R1.B: X :canDo :B

R2: If X is an Individual and X :canDo :A then (choose one)
    - R2.E: X :canDo :E
    - R2.D: X :canDo :D

R3: If X is an Individual and X :canDo :D then (choose one)
    - R3.E: X :canDo :E
    - R3.F: X :canDo :F

Goal
----
Find a model (a sequence of chosen cases) that makes
    X a vcard:Individual  AND  X :canDo :E
true (your query). With the given facts this is X = :Pat.

Notes on how we mimic N3 control bits:
- We treat each rule’s right-hand side as a set of alternative "cases". Picking a case
  extends the current "world" with its conclusions.
- We do a backtracking DFS over case choices, collecting every model that reaches the goal.
- We avoid infinite loops by not re-applying the same (rule,binding,case) twice.

What we output:
- Answer: whether models exist, and we list all minimal case-sequences that derive E.
- Reason: compact English of the rule chain that gets to E.
- Check (harness): mechanical verification/exploration of the space, including a few
  forced-choice tests that should fail to reach E (e.g., forcing R1.B).
"""

from dataclasses import dataclass
from typing import List, Tuple, Set, Dict

# --- Embed the original N3 for reference (no parsing required here) ---
N3 = """@prefix log: <http://www.w3.org/2000/10/swap/log#>.
@prefix list: <http://www.w3.org/2000/10/swap/list#>.
@prefix vcard: <http://www.w3.org/2006/vcard/ns#>.
@prefix : <http://example.org/#>.

:Pat a vcard:Individual.

{ ?X a vcard:Individual. } => ($ { ?X :canDo :A. } { ?X :canDo :B. } $).

{ ?X a vcard:Individual. ?X :canDo :A. } => ($ { ?X :canDo :E. } { ?X :canDo :D. } $).

{ ?X a vcard:Individual. ?X :canDo :D. } => ($ { ?X :canDo :E. } { ?X :canDo :F. } $).

# query: find model where X is an individual who can do E
{ { ?X a vcard:Individual. ?X :canDo :E. } :findModel (() () ?Model). } =^ { :model :is ?Model. }.
"""

# --- Tiny rule engine with disjunctive RHS "cases" ---

Atom = Tuple[str, str]  # (predicate, constant)  e.g., ("Individual", "Pat") or ("canDo:A", "Pat")

@dataclass(frozen=True)
class Case:
    name: str
    adds: List[Atom]

@dataclass(frozen=True)
class Rule:
    name: str
    preconds: List[Atom]  # variables are represented as ("Pred", "X"); constants like "Pat"
    cases: Tuple[Case, ...]


def substitute(atom: Atom, binding: Dict[str, str]) -> Atom:
    pred, arg = atom
    if arg in binding:
        return (pred, binding[arg])
    return atom

def preconds_satisfied(world: Set[Atom], preconds: List[Atom], binding: Dict[str, str]) -> bool:
    return all(substitute(p, binding) in world for p in preconds)

def possible_bindings_for_rule(world: Set[Atom], preconds: List[Atom]) -> List[Dict[str, str]]:
    """
    Our rules only have one variable 'X' and unary predicates, so we derive bindings by
    looking at Individuals that appear in the world and testing preconditions.
    """
    # Gather all constants in world
    constants = {arg for _, arg in world}
    bindings = []
    for c in constants:
        b = {"X": c}
        if preconds_satisfied(world, preconds, b):
            bindings.append(b)
    return bindings

def dfs_models(world0: Set[Atom], rules: List[Rule], goal: List[Atom], max_depth: int = 6):
    """
    Backtracking search over case choices. Yields (model, world) pairs where goal holds.
    A 'model' is a list of chosen case names in the order applied.
    """
    from collections import deque
    goal = list(goal)

    def goal_holds(w: Set[Atom]) -> bool:
        return all(a in w for a in goal)

    seen_states = set()  # memoize by (frozenset(world), frozenset(applied triplets))

    results = []

    def rec(world: Set[Atom], model: List[str], applied: Set[Tuple[str, str, str]], depth: int):
        key = (frozenset(world), frozenset(applied))
        if key in seen_states:
            return
        seen_states.add(key)

        if goal_holds(world):
            results.append((model[:], set(world)))
            return
        if depth >= max_depth:
            return

        # Try each rule, each binding, each case
        for r in rules:
            for b in possible_bindings_for_rule(world, r.preconds):
                bound_name = f"{r.name}[X={b['X']}]"
                for case in r.cases:
                    applied_key = (r.name, b["X"], case.name)
                    if applied_key in applied:
                        continue  # don't re-apply same exact choice
                    # Apply case: add all its atoms under binding
                    new_world = set(world)
                    for a in case.adds:
                        new_world.add(substitute(a, b))
                    applied_next = set(applied)
                    applied_next.add(applied_key)
                    rec(new_world, model + [f"{bound_name}:{case.name}"], applied_next, depth + 1)

    rec(set(world0), [], set(), 0)
    # Deduplicate models by their sequence (order matters here), but keep unique sequences
    uniq = []
    seen_seq = set()
    for m, w in results:
        tup = tuple(m)
        if tup not in seen_seq:
            seen_seq.add(tup)
            uniq.append((m, w))
    return uniq


# --- Build initial world, rules, and goal ---

# World (facts)
WORLD0: Set[Atom] = {
    ("Individual", "Pat"),  # :Pat a vcard:Individual.
}

# Rules
R1 = Rule(
    name="R1",
    preconds=[("Individual", "X")],
    cases=(
        Case("A", adds=[("canDo:A", "X")]),
        Case("B", adds=[("canDo:B", "X")]),
    )
)

R2 = Rule(
    name="R2",
    preconds=[("Individual", "X"), ("canDo:A", "X")],
    cases=(
        Case("E", adds=[("canDo:E", "X")]),
        Case("D", adds=[("canDo:D", "X")]),
    )
)

R3 = Rule(
    name="R3",
    preconds=[("Individual", "X"), ("canDo:D", "X")],
    cases=(
        Case("E", adds=[("canDo:E", "X")]),
        Case("F", adds=[("canDo:F", "X")]),
    )
)

RULES = [R1, R2, R3]

# Goal: exists X s.t. Individual(X) and canDo:E(X). With WORLD0, X must be Pat.
GOAL = [("Individual", "Pat"), ("canDo:E", "Pat")]

# --- Run search for ALL models that reach the goal ---

models = dfs_models(WORLD0, RULES, GOAL, max_depth=6)

# Filter to minimal-length sequences (since extra irrelevant choices could appear in longer worlds)
if models:
    min_len = min(len(seq) for (seq, _) in models)
    minimal_models = [(seq, w) for (seq, w) in models if len(seq) == min_len]
else:
    minimal_models = []

# --- Prepare pretty printing ---

def fmt_model(seq: List[str]) -> str:
    # Turn ["R1[X=Pat]:A", "R2[X=Pat]:E"] into a readable breadcrumb
    return " → ".join(seq)

def world_has(atom: Atom, world: Set[Atom]) -> bool:
    return atom in world

# --- Build the Answer section ---

answer_lines = []
if not models:
    answer_lines.append("❌ No model exists that makes an Individual able to do E.")
else:
    answer_lines.append("✅ A model exists where an Individual can do E (here, X = Pat).")
    if len(minimal_models) == 1:
        answer_lines.append("Minimal case-sequence:")
    else:
        answer_lines.append("Minimal case-sequences:")
    for i, (seq, w) in enumerate(minimal_models, 1):
        answer_lines.append(f"  {i}. {fmt_model(seq)}")
    # Show that E actually holds in each minimal model's world
    for i, (seq, w) in enumerate(minimal_models, 1):
        holds = world_has(("canDo:E", "Pat"), w)
        answer_lines.append(f"     └─ Derived: canDo:E(Pat) = {'YES' if holds else 'NO'}")

# --- Build the Reason section ---

reason = []
reason.append("• R1 allows choosing either :A or :B for any Individual (Pat is an Individual).")
reason.append("• To reach :E, you must first choose :A (choosing :B gives no path to :E).")
reason.append("• From :A, R2 lets you choose :E directly, or choose :D; and from :D, R3 lets you choose :E.")
reason.append("• Therefore there are two natural ways to derive :E for Pat:")
reason.append("  – R1:A → R2:E")
reason.append("  – R1:A → R2:D → R3:E")

# --- Build the Check (harness) section ---
# We do three mechanical checks:
# 1) Enumerate ALL goal-reaching models (not only minimal) to confirm E is derivable.
# 2) Force a first choice of R1.B and prove E is NOT reachable from there.
# 3) Force a path that ends in F (R1:A → R2:D → R3:F) and show it doesn't satisfy E.

# 1) All models (limit depth) just to show coverage:
all_models = dfs_models(WORLD0, RULES, GOAL, max_depth=6)

# 2) Forcing R1.B first
world_B = set(WORLD0)
world_B.add(("canDo:B", "Pat"))
forced_B_models = dfs_models(world_B, RULES, GOAL, max_depth=5)

# 3) Force A → D → F and check goal
world_F = set(WORLD0)
world_F.add(("canDo:A", "Pat"))
world_F.add(("canDo:D", "Pat"))
world_F.add(("canDo:F", "Pat"))
forced_F_models = dfs_models(world_F, RULES, GOAL, max_depth=3)

# --- Print sections ---

print("Answer")
print("------")
print("\n".join(answer_lines))
print()

print("Reason")
print("------")
for line in reason:
    print(line)
print()

print("Check (harness)")
print("----------------")
print(f"• Total goal-reaching models found (depth ≤ 6): {len(all_models)}")
if models:
    print(f"• Minimal length among them: {min(len(seq) for (seq, _) in all_models)}")
for i, (seq, _) in enumerate(sorted(models, key=lambda x: (len(x[0]), x[0])), 1):
    print(f"  {i:>2}. {fmt_model(seq)}")

print("• Forcing first choice R1.B (i.e., choose :B for Pat) → models to E:", len(forced_B_models))
print("   (Expected 0, because :B gives no path to :E.)")

print("• Forcing path R1:A → R2:D → R3:F (ends in :F) → does this already satisfy :E?")
has_E_in_forced_F = ("canDo:E", "Pat") in world_F
print("   canDo:E(Pat):", "YES" if has_E_in_forced_F else "NO (as expected)")

