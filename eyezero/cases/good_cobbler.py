#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Good Cobbler — ARC (Answer / Reason / Check), self-contained

Idea (term-logic reading):
  The composite class/list  (:good :Cobbler)  denotes ‘good-at(Cobbler)’.
  From  x a (:good Y)  (or  x :is (:good Y) ) we should infer:
      - good_at(x, Y)
      - x a Y
  BUT from  x a :Good  and  x a :Cobbler  alone, we must NOT infer  x a (:good :Cobbler).

Facts (two equivalent ways to assert the composite):
  _:x a (:good :Cobbler).
  _:x :is (:good :Cobbler).

Query:
  ∃x ∃y . good_at(x, y)  — list all witnesses (x, y).

This script forward-chains with a tiny matcher/unifier over nested tuples.
"""

from __future__ import annotations
from typing import Any, Dict, Iterable, List, Set, Tuple

# --------------------------- Term & fact model ---------------------------
# We use triples (pred, subj, obj). Terms are strings or nested tuples.
# Variables are strings that start with '?' (e.g., '?x', '?y').

Term = Any                   # str or tuple
Triple = Tuple[str, Term, Term]

def is_var(t: Term) -> bool:
    return isinstance(t, str) and t.startswith("?")

def subst(t: Term, env: Dict[str, Term]) -> Term:
    """Substitute using env (with simple deref in case env maps to vars)."""
    while is_var(t) and t in env:
        t = env[t]
    if isinstance(t, tuple):
        return tuple(subst(x, env) for x in t)
    return t

def unify(a: Term, b: Term, env: Dict[str, Term]) -> Dict[str, Term] | None:
    """Unify nested terms with occurs-check omitted (safe for our finite patterns)."""
    a = subst(a, env); b = subst(b, env)
    if a == b:
        return env
    if is_var(a):
        env2 = env.copy(); env2[a] = b; return env2
    if is_var(b):
        env2 = env.copy(); env2[b] = a; return env2
    if isinstance(a, tuple) and isinstance(b, tuple) and len(a) == len(b):
        for aa, bb in zip(a, b):
            env = unify(aa, bb, env)
            if env is None:
                return None
        return env
    return None

# ------------------------------ KB (facts) -------------------------------
# Skolem-like fresh individual for the existential in the premise.
X = "_:x"

FACTS: Set[Triple] = {
    ("a", X, (":good", ":Cobbler")),        # _:x a (:good :Cobbler)
    (":is", X, (":good", ":Cobbler")),      # _:x :is (:good :Cobbler)
}

# ------------------------------ Rules ------------------------------------
# R1  a(x, (:good Y))  ->  :is(x, (:good Y))
# R2  :is(x, (:good Y)) ->  good_at(x, Y)
# R3  :is(x, (:good Y)) ->  a(x, Y)
RULES: List[Tuple[List[Triple], List[Triple], str]] = [
    ([("a", "?x", (":good", "?Y"))],           [(":is", "?x", (":good", "?Y"))], "R1_a_to_is"),
    ([(":is", "?x", (":good", "?Y"))],         [("good_at", "?x", "?Y")],        "R2_goodat"),
    ([(":is", "?x", (":good", "?Y"))],         [("a", "?x", "?Y")],              "R3_memberY"),
]

# NOTE: Intentionally NO rule like { a(x,:Good) & a(x,Y) } => { a(x,(:good Y)) }.
# That captures the non-compositionality (you can’t generally fuse predicates).

# -------------------------- Forward chaining -----------------------------
def apply_rule_once(facts: Set[Triple], rule: Tuple[List[Triple], List[Triple], str]) -> Set[Triple]:
    body, head, _name = rule
    out: Set[Triple] = set()
    # backtracking joins over body patterns
    envs = [dict()]
    for (p, s, o) in body:
        next_envs: List[Dict[str, Term]] = []
        for env in envs:
            pat = (p, subst(s, env), subst(o, env))
            for (fp, fs, fo) in facts:
                e2 = unify((p, s, o), (fp, fs, fo), env.copy())
                if e2 is not None:
                    next_envs.append(e2)
        envs = next_envs
        if not envs:
            break
    # emit heads
    for env in envs:
        for (p, s, o) in head:
            out.add((p, subst(s, env), subst(o, env)))
    return out - facts

def closure(start: Set[Triple]) -> Set[Triple]:
    facts = set(start)
    changed = True
    while changed:
        changed = False
        for r in RULES:
            new = apply_rule_once(facts, r)
            if new:
                facts |= new
                changed = True
    return facts

# ------------------------------- Query -----------------------------------
def witnesses_good_at(facts: Set[Triple]) -> List[Tuple[Term, Term]]:
    ws = []
    for (p, s, o) in facts:
        if p == "good_at":
            ws.append((s, o))
    # sort for stable printing
    def key(pair): 
        s, o = pair
        return (str(s), str(o))
    return sorted(ws, key=key)

# ------------------------------- ARC: Answer ------------------------------
def print_answer():
    print("Answer")
    print("======")
    facts = closure(FACTS)
    ws = witnesses_good_at(facts)
    if ws:
        print("Witnesses for ∃x ∃y . good_at(x, y):")
        for (s, y) in ws:
            print(f"  x={s} , y={y}")
    else:
        print("No witnesses for good_at/2 were derived.")

    # Also show that we decomposed membership to Y
    has_member = ("a", X, ":Cobbler") in facts
    print(f"\nFrom _:x a (:good :Cobbler) we derived  _:x a :Cobbler ? {has_member}")

# ---------------------------- ARC: Reason why -----------------------------
def print_reason():
    print("\nReason why")
    print("==========")
    print("We interpret the composite class (:good Y) as meaning “good-at(Y)”.")
    print("Thus, if some x satisfies  a(x, (:good Y))  (or  :is(x, (:good Y))),")
    print("we infer both  good_at(x, Y)  and  a(x, Y).")
    print("Crucially, we *do not* allow the converse: from  a(x, :Good)  and  a(x, Y)")
    print("we may NOT compose  a(x, (:good Y)). This matches the classic observation")
    print("behind the ‘good cobbler’ example: predicate conjunction does not license")
    print("the fused compound class automatically.")

# -------------------------- ARC: Check (harness) --------------------------
def print_check():
    print("\nCheck (harness)")
    print("===============")
    ok_all = True

    # 1) Existence: our initial facts entail a concrete witness for good_at(_, :Cobbler)
    facts = closure(FACTS)
    ws = witnesses_good_at(facts)
    has_cobbler_witness = (X, ":Cobbler") in ws
    print(f"Witness exists for good_at(_, :Cobbler)? {has_cobbler_witness}")
    ok_all &= has_cobbler_witness

    # 2) Decomposition: from :is(x, (:good :Cobbler)) we also get a(x, :Cobbler)
    decomposed = ("a", X, ":Cobbler") in facts
    print(f"Decomposition to membership in :Cobbler derived? {decomposed}")
    ok_all &= decomposed

    # 3) Non-compositionality: good ∧ cobbler alone does NOT yield the composite
    Y = "_:y"
    facts_sep: Set[Triple] = {
        ("a", Y, ":Good"),         # merely 'good' as a unary class
        ("a", Y, ":Cobbler"),      # and 'cobbler'
    }
    f2 = closure(facts_sep)
    # ensure no composite membership or good_at was (incorrectly) deduced
    bad_comp = any(p == "a" and isinstance(o, tuple) and o[:1] == (":good",) for (p, s, o) in f2)
    bad_goodat = any(p == "good_at" for (p, s, o) in f2)
    print(f"NO spurious a(y, (:good :Cobbler)) from Good∧Cobbler? {not bad_comp}")
    print(f"NO spurious good_at(y, :Cobbler) from Good∧Cobbler?   {not bad_goodat}")
    ok_all &= (not bad_comp) and (not bad_goodat)

    # 4) Generalization: if we assert :is(z, (:good :Baker)) then good_at(z,:Baker) follows
    Z = "_:z"
    facts_baker: Set[Triple] = {(":is", Z, (":good", ":Baker"))}
    f3 = closure(facts_baker)
    gen_ok = ("good_at", Z, ":Baker") in f3 and ("a", Z, ":Baker") in f3
    print(f"Generalizes to other Y (e.g., :Baker)? {gen_ok}")
    ok_all &= gen_ok

    # 5) Idempotence: closure is a fixed point
    fixed = (closure(facts) == facts)
    print(f"Closure is idempotent (fixed point)? {fixed}")
    ok_all &= fixed

    print(f"\nAll checks passed? {ok_all}")

# --------------------------------- Main ----------------------------------
if __name__ == "__main__":
    print_answer()
    print_reason()
    print_check()

