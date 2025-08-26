#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
knights_knaves.py — Knights & Knaves (8 people) with ARC output
────────────────────────────────────────────────────────────────────

Puzzle
------
Eight people, A..H. Each is either a Knight (always tells the truth) or a
Knave (always lies). Their statements are:

  A: "B is a Knave."
  B: "C is a Knight."
  C: "D is a Knave."
  D: "E and F are of the same type."
  E: "G is a Knave."
  F: "I am a Knight."
  G: "H is a Knave."
  H: "A is a Knight."

Goal
----
Find all truth assignments consistent with the rules (Knights ⇔ statement true;
Knaves ⇔ statement false). Then print:

  • Answer — first solution and how many total solutions exist.
  • Reason why — per-person explanation for the chosen solution and a note
    about F's self-referential statement.
  • Check (harness) — validates solutions and determinism; re-checks that each
    reported Knight/Knave matches the truth value of their statement.

Notes
-----
A *self-affirming* statement like F’s (“I am a Knight”) imposes **no**
constraint: Knight→true, Knave→false; both satisfy “role == truth_of_statement”.
"""

from __future__ import annotations
from itertools import product
from typing import Dict, List, Tuple

# ─────────────────────────────────────────────────────────────
# 1) Names and statements
# ─────────────────────────────────────────────────────────────
NAMES: List[str] = ['A','B','C','D','E','F','G','H']
Assignment = Dict[str, bool]  # True = Knight, False = Knave

def stmt_A(a: Assignment) -> bool: return not a['B']               # A: B is a Knave
def stmt_B(a: Assignment) -> bool: return a['C']                   # B: C is a Knight
def stmt_C(a: Assignment) -> bool: return not a['D']               # C: D is a Knave
def stmt_D(a: Assignment) -> bool: return a['E'] == a['F']         # D: E and F same type
def stmt_E(a: Assignment) -> bool: return not a['G']               # E: G is a Knave
def stmt_F(a: Assignment) -> bool: return a['F']                   # F: I am a Knight
def stmt_G(a: Assignment) -> bool: return not a['H']               # G: H is a Knave
def stmt_H(a: Assignment) -> bool: return a['A']                   # H: A is a Knight

STATEMENTS = [stmt_A, stmt_B, stmt_C, stmt_D, stmt_E, stmt_F, stmt_G, stmt_H]
EXPLAIN_TXT = {
    'A': "B is a Knave.",
    'B': "C is a Knight.",
    'C': "D is a Knave.",
    'D': "E and F are of the same type.",
    'E': "G is a Knave.",
    'F': "I am a Knight.",
    'G': "H is a Knave.",
    'H': "A is a Knight.",
}

# ─────────────────────────────────────────────────────────────
# 2) Solving by brute force (2^8 = 256 assignments)
# ─────────────────────────────────────────────────────────────
def satisfies(a: Assignment) -> bool:
    """Knight iff their statement evaluates to True; Knave iff it evaluates to False."""
    for i, name in enumerate(NAMES):
        is_knight = a[name]
        says_true = STATEMENTS[i](a)
        if is_knight != says_true:
            return False
    return True

def solve_all() -> List[Assignment]:
    sols: List[Assignment] = []
    for bits in product([True, False], repeat=len(NAMES)):
        a = dict(zip(NAMES, bits))
        if satisfies(a):
            sols.append(a)
    return sols

# ─────────────────────────────────────────────────────────────
# 3) ARC — Answer
# ─────────────────────────────────────────────────────────────
def arc_answer(solutions: List[Assignment]) -> Assignment:
    print("Answer")
    print("------")
    if not solutions:
        print("No solution exists.\n")
        return {}
    first = solutions[0]
    print(f"Total solutions found: {len(solutions)}")
    print("First (deterministic) solution:")
    for name in NAMES:
        print(f"  {name}: {'Knight' if first[name] else 'Knave'}")
    print()
    return first

# ─────────────────────────────────────────────────────────────
# 4) ARC — Reason why (per-person explanation for chosen solution)
# ─────────────────────────────────────────────────────────────
def arc_reason(sol: Assignment) -> None:
    print("Reason why")
    print("----------")
    if not sol:
        print("Unsatisfiable: no assignment meets all constraints.\n")
        return

    for name in NAMES:
        role = 'Knight' if sol[name] else 'Knave'
        text = EXPLAIN_TXT[name]
        print(f"{name} is a {role} and says: \"{text}\"")
        if name == 'A':
            print(f"  B is {'Knave' if not sol['B'] else 'Knight'} → statement is {str(stmt_A(sol)).lower()}")
        elif name == 'B':
            print(f"  C is {'Knight' if sol['C'] else 'Knave'} → statement is {str(stmt_B(sol)).lower()}")
        elif name == 'C':
            print(f"  D is {'Knave' if not sol['D'] else 'Knight'} → statement is {str(stmt_C(sol)).lower()}")
        elif name == 'D':
            same = sol['E'] == sol['F']
            print(f"  E={('K' if sol['E'] else 'k')}, F={('K' if sol['F'] else 'k')} → same? {same} → statement is {str(stmt_D(sol)).lower()}")
        elif name == 'E':
            print(f"  G is {'Knave' if not sol['G'] else 'Knight'} → statement is {str(stmt_E(sol)).lower()}")
        elif name == 'F':
            print("  Self-referential: 'I am a Knight.'")
            print("  This imposes no constraint: Knight→true; Knave→false; both satisfy role == statement.")
        elif name == 'G':
            print(f"  H is {'Knave' if not sol['H'] else 'Knight'} → statement is {str(stmt_G(sol)).lower()}")
        elif name == 'H':
            print(f"  A is {'Knight' if sol['A'] else 'Knave'} → statement is {str(stmt_H(sol)).lower()}")
    print()

# ─────────────────────────────────────────────────────────────
# 5) ARC — Check (harness)
# ─────────────────────────────────────────────────────────────
def arc_check(solutions: List[Assignment], chosen: Assignment) -> None:
    print("Check (harness)")
    print("---------------")
    # 1) There is at least one solution
    assert len(solutions) >= 1, "Expected ≥ 1 solution, found none."

    # 2) Every solution satisfies the constraints
    for a in solutions:
        assert satisfies(a), "Found a listed 'solution' that does not satisfy constraints."

    # 3) Determinism: re-run solver yields identical set (same order with our iteration)
    again = solve_all()
    assert len(again) == len(solutions), "Non-deterministic solution count."
    for i in range(len(solutions)):
        assert all(solutions[i][nm] == again[i][nm] for nm in NAMES), "Non-deterministic ordering or values."

    # 4) For the chosen solution, flipping any single person breaks consistency
    #    (local minimality check; if this assertion ever fails, puzzle has an
    #     alternative solution at Hamming distance 1).
    for nm in NAMES:
        b = chosen.copy()
        b[nm] = not b[nm]
        assert not satisfies(b), f"Flipping {nm} still satisfies constraints — multiple nearby solutions?"

    print("OK: solutions validated; deterministic; chosen solution locally unique under single flips.\n")

# ─────────────────────────────────────────────────────────────
# Main
# ─────────────────────────────────────────────────────────────
if __name__ == "__main__":
    sols = solve_all()
    chosen = arc_answer(sols)
    arc_reason(chosen)
    arc_check(sols, chosen)

