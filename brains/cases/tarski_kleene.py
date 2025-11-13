#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
README (plain text)
===================
Purpose
-------
This CASE module for `eyezero.py` illustrates **Tarski + Kleene** fixed-point
semantics using only the generic reasoning engine.

We fix a tiny domain D = {0,1,2,3,4} with a successor relation succ:

  succ(0,1), succ(1,2), succ(2,3), succ(3,4)

We consider the monotone operator on subsets S⊆D:

  F(S) = {0} ∪ succ(S)

The least fixed point LFP(F) is the **smallest** S such that:

  1. 0 ∈ S
  2. S is closed under succ:  ∀x,y (x∈S ∧ succ(x,y) → y∈S).

In this finite chain, LFP(F) is all of D = {0,1,2,3,4}.

Encoding in EyeZero
-------------------
We encode:

  • The succ relation via a *name* and a fixed binary application predicate

        ex:holds2(ex:succ, x, y)

    so that succ is just a **named relation** (an intension).

  • The least fixed point of F as a unary predicate

        ex:Reach(x)

    with rules:

        Reach(0).
        Reach(y) :- Reach(x), holds2(ex:succ, x, y).

EyeZero’s bottom-up engine computes the unique least model for these Horn rules.
This least model is exactly the Kleene least fixed point of F. The harness then
checks, using only EyeZero for logical consequences, that:

  • Reach is closed under succ and contains 0.
  • For every S ⊆ D closed under succ and containing 0, Reach ⊆ S,

which corresponds to Tarski’s characterization of the **least** fixed point.

How to run
----------
Standalone:

    python tarski_kleene.py

Via EyeZero:

    python eyezero.py tarski_kleene.py

Output
------
Model → Question → Answer → Reason why → Check (harness)
"""

from __future__ import annotations

from typing import List, Tuple, Set, Dict
from itertools import combinations

# Import the generic engine primitives from EyeZero
from eyezero import (
    Var, Atom, Clause, atom, fact,
    solve_topdown, solve_bottomup, match_against_facts,
    NAME, IND, Signature, deref,
)

# ─────────────────────────────────────────────────────────────────────────────
# Model: domain, relation name, and Horn program
# ─────────────────────────────────────────────────────────────────────────────

# Domain as *strings* (logic side): "0","1","2","3","4"
D_STR: Tuple[str, ...] = tuple(str(i) for i in range(5))

EX = "ex:"
Holds2Pred = EX + "holds2"      # (NAME, IND, IND)
SuccName   = EX + "succ"       # the *name* of the succ relation
ReachPred  = EX + "Reach"      # unary predicate for the fixed point

# Signature: tells EyeZero which arguments are NAME vs IND
SIGNATURE: Signature = {
    Holds2Pred: (NAME, IND, IND),
    ReachPred:  (IND,),
}

# Program: facts + rules
PROGRAM: List[Clause] = []

# succ facts as holds2(ex:succ, x, y)
succ_edges: List[Tuple[str, str]] = [("0", "1"), ("1", "2"), ("2", "3"), ("3", "4")]
for x, y in succ_edges:
    PROGRAM.append(fact(Holds2Pred, SuccName, x, y))

# Reach(0).
PROGRAM.append(Clause(atom(ReachPred, "0"), []))

# Reach(y) :- Reach(x), holds2(ex:succ, x, y).
X, Y = Var("X"), Var("Y")
PROGRAM.append(
    Clause(
        atom(ReachPred, Y),
        [
            atom(ReachPred, X),
            atom(Holds2Pred, SuccName, X, Y),
        ],
    )
)

# ─────────────────────────────────────────────────────────────────────────────
# Tiny engine glue for this CASE
# ─────────────────────────────────────────────────────────────────────────────

def _is_var(t) -> bool:
    return isinstance(t, Var)

def choose_engine(goals: List[Atom]) -> str:
    """
    Very small heuristic:

      • If a goal has variables (e.g. Reach(X)) → bottom-up (enumeration).
      • Otherwise (fully ground)               → top-down (goal-directed).
    """
    for g in goals:
        if any(_is_var(a) for a in g.args):
            return "bottomup"
    return "topdown"

def ask(goals: List[Atom], step_limit: int = 10000):
    """
    Ask the engine a conjunctive query.

    Returns: (engine_tag, solutions, metric)
      - engine_tag ∈ {"bottomup","topdown"}
      - metric is number of rounds/steps (we don't display it here).
    """
    engine = choose_engine(goals)
    if engine == "topdown":
        sols, steps = solve_topdown(PROGRAM, goals, step_limit=step_limit)
        return engine, sols, steps
    else:
        facts, rounds = solve_bottomup(PROGRAM, SIGNATURE)
        sols = match_against_facts(goals, facts)
        return engine, sols, rounds

def compute_reach_set() -> Set[str]:
    """Enumerate all x with Reach(x) using the engine."""
    X = Var("X")
    _, sols, _ = ask([atom(ReachPred, X)])
    return {deref(X, s) for s in sols if isinstance(deref(X, s), str)}

def compute_succ_edges_from_engine() -> Set[Tuple[str, str]]:
    """Read succ edges back from holds2 facts (to show we trust the engine)."""
    facts, _ = solve_bottomup(PROGRAM, SIGNATURE)
    edges: Set[Tuple[str, str]] = set()
    for (rel, x, y) in facts.get(Holds2Pred, set()):
        if rel == SuccName:
            edges.add((x, y))
    return edges

# ─────────────────────────────────────────────────────────────────────────────
# Model + Question pretty-printing
# ─────────────────────────────────────────────────────────────────────────────

def fmt_set_str(S: Set[str]) -> str:
    if not S: return "∅"
    return "{" + ", ".join(sorted(S, key=int)) + "}"

def print_model() -> None:
    print("Model")
    print("=====")
    print(f"Domain D = {[int(x) for x in D_STR]}  (represented as strings in the logic)")
    print()
    print("Fixed predicates (signature)")
    print("----------------------------")
    print("• ex:holds2(R, x, y)  — binary application; sorts: (NAME, IND, IND)")
    print(f"   here R = {SuccName} names the succ relation.")
    print(f"• {ReachPred}(x)      — unary predicate for the least fixed point of F(S) = {{0}} ∪ succ(S).")
    print()
    print("Succ relation (as holds2 facts)")
    print("--------------------------------")
    print("succ = { (0,1), (1,2), (2,3), (3,4) }")
    print()
    print("Horn program")
    print("------------")
    print("1) Reach(0).")
    print("2) Reach(y) :- Reach(x), holds2(ex:succ, x, y).")
    print()
    print("Semantics")
    print("---------")
    print("The bottom-up (least fixed-point) model of this program is exactly")
    print("the Kleene least fixed point of F(S) = {0} ∪ succ(S) on P(D).")
    print("Tarski’s theorem says this is the **smallest** S containing 0 and")
    print("closed under succ. The harness checks those properties explicitly.\n")

def print_question() -> None:
    print("Question")
    print("========")
    print("Q1) Enumerate all x with Reach(x).                      [auto engine]")
    print("Q2) Ground checks: Reach(0)? Reach(4)?                  [auto engine]")
    print("Q3) Is Reach the least set S⊆D with 0∈S and S succ-closed? [meta + engine]\n")

# ─────────────────────────────────────────────────────────────────────────────
# Queries
# ─────────────────────────────────────────────────────────────────────────────

def is_least_closed_from0(reach: Set[str], succ_edges: Set[Tuple[str, str]]) -> bool:
    """
    Check Tarski-style minimality on the finite domain D:
      Reach is the least S with
        (i) "0" in S, and
        (ii) succ-closed: ∀(x,y)∈succ, x∈S → y∈S.

    Implementation:
      enumerate all subsets S⊆D, filter those closed and containing "0",
      and ensure Reach ⊆ S for all such S.

    This uses engine-derived succ_edges and reach-set; the rest is finite
    set reasoning in Python.
    """
    domain = list(D_STR)

    def closed(S: Set[str]) -> bool:
        for x, y in succ_edges:
            if x in S and y not in S:
                return False
        return True

    from_zero_closed_subsets: List[Set[str]] = []
    # enumerate all subsets via combinations
    for r in range(1, len(domain)+1):
        for comb in combinations(domain, r):
            S = set(comb)
            if "0" in S and closed(S):
                from_zero_closed_subsets.append(S)

    # Tarski minimality: Reach ⊆ S for every such S
    for S in from_zero_closed_subsets:
        if not reach.issubset(S):
            return False
    return True

def run_queries():
    # Q1: enumerate Reach(x)
    reach_set = compute_reach_set()
    eng1 = choose_engine([atom(ReachPred, Var("X"))])

    # Q2: ground checks
    eng2a, sols0, _ = ask([atom(ReachPred, "0")])
    eng2b, sols4, _ = ask([atom(ReachPred, "4")])
    ok0 = bool(sols0)
    ok4 = bool(sols4)

    # Q3: Tarski minimality check (meta + engine)
    succ_from_engine = compute_succ_edges_from_engine()
    minimal = is_least_closed_from0(reach_set, succ_from_engine)
    eng3 = "mixed"

    return (
        ("Q1", eng1, reach_set, "n/a"),
        ("Q2", f"{eng2a}/{eng2b}", (ok0, ok4), "n/a"),
        ("Q3", eng3, minimal, "n/a"),
    )

# ─────────────────────────────────────────────────────────────────────────────
# Answer + Reason
# ─────────────────────────────────────────────────────────────────────────────

def print_answer(res1, res2, res3) -> None:
    print("Answer")
    print("======")
    tag1, eng1, reach_set, _ = res1
    tag2, eng2, (ok0, ok4), _ = res2
    tag3, eng3, minimal, _ = res3

    print(f"{tag1}) Engine: {eng1} → Reach = {fmt_set_str(reach_set)}")
    print(f"{tag2}) Engine: {eng2} → Reach(0): {'Yes' if ok0 else 'No'}, Reach(4): {'Yes' if ok4 else 'No'}")
    print(f"{tag3}) Engine: {eng3} → Reach is least succ-closed S with 0∈S: {'Yes' if minimal else 'No'}\n")

def print_reason(eng1: str, eng2: str) -> None:
    # eng1, eng2 kept for compatibility with eyezero, but not used here.
    print("Reason why")
    print("==========")
    print("• The Horn program:")
    print("    Reach(0).")
    print("    Reach(y) :- Reach(x), succ(x,y).")
    print("  induces a monotone operator F on subsets S⊆D:")
    print("    F(S) = {0} ∪ { y | ∃x∈S . succ(x,y) }.")
    print("• EyeZero’s bottom-up semantics computes the least fixed point of F,")
    print("  i.e., the smallest S such that S = F(S). On this finite chain,")
    print("  the result is D = {0,1,2,3,4}.")
    print("• The harness checks the Tarski conditions:")
    print("    1) Reach contains 0 and is closed under succ,")
    print("    2) Any S⊆D that contains 0 and is succ-closed must contain Reach.")
    print("  Together, this confirms that Reach is the Tarski–Kleene least fixed")
    print("  point of F.\n")

# ─────────────────────────────────────────────────────────────────────────────
# Check (harness)
# ─────────────────────────────────────────────────────────────────────────────

class CheckFailure(AssertionError):
    pass

def check(c: bool, msg: str) -> None:
    if not c:
        raise CheckFailure(msg)

def run_checks() -> List[str]:
    notes: List[str] = []

    # 1) succ edges as expected (via engine)
    succ_engine = compute_succ_edges_from_engine()
    check(succ_engine == set(succ_edges), "succ edges mismatch between program and expectation.")
    notes.append("PASS 1: succ = {(0,1),(1,2),(2,3),(3,4)} as expected.")

    # 2) bottom-up Reach enumeration is complete
    reach = compute_reach_set()
    check(reach == set(D_STR), f"Reach should be all of D, got {reach}.")
    notes.append("PASS 2: bottom-up Reach enumeration = D.")

    # 3) top-down Reach(4) and Reach(0) are provable
    td0, _ = solve_topdown(PROGRAM, [atom(ReachPred, "0")])
    td4, _ = solve_topdown(PROGRAM, [atom(ReachPred, "4")])
    check(bool(td0) and bool(td4), "Top-down should prove Reach(0) and Reach(4).")
    notes.append("PASS 3: top-down proves Reach(0) and Reach(4).")

    # 4) Closure: if Reach(x) and succ(x,y), then Reach(y)
    for x, y in succ_engine:
        # ask engine for Reach(x) -> then Reach(y) must hold
        _, sols_x, _ = ask([atom(ReachPred, x)])
        if sols_x:
            _, sols_y, _ = ask([atom(ReachPred, y)])
            check(bool(sols_y), f"Closure violated at edge {x}->{y}.")
    notes.append("PASS 4: Reach is closed under succ.")

    # 5) 0 is in Reach, and 4 is reachable by some chain
    check("0" in reach and "4" in reach, "0 and 4 must be in Reach.")
    notes.append("PASS 5: 0 and 4 are in Reach.")

    # 6) No element outside D is in Reach (sanity; domain fixed)
    for s in reach:
        check(s in D_STR, f"Unexpected element {s} in Reach.")
    notes.append("PASS 6: Reach is subset of D.")

    # 7) Tarski minimality: Reach is least closed from 0
    check(is_least_closed_from0(reach, succ_engine),
          "Reach must be the least succ-closed set containing 0.")
    notes.append("PASS 7: Reach is the Tarski least fixed point of F(S) = {0} ∪ succ(S).")

    # 8) Determinism: recomputing Reach yields same set
    reach2 = compute_reach_set()
    check(reach == reach2, "Recomputation of Reach changed the result.")
    notes.append("PASS 8: Reach computation deterministic.")

    return notes

# ─────────────────────────────────────────────────────────────────────────────
# Main
# ─────────────────────────────────────────────────────────────────────────────

def main() -> None:
    print_model()
    print_question()
    res1, res2, res3 = run_queries()
    print_answer(res1, res2, res3)
    print_reason(res1[1], res2[1])
    print("Check (harness)")
    print("===============")
    try:
        for note in run_checks():
            print(note)
    except CheckFailure as e:
        print("FAIL:", e)
        raise

if __name__ == "__main__":
    main()

