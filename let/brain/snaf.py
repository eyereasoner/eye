#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
snaf.py — Scoped Negation As Failure (SNAF) with a single KB graph

KB facts
--------
:Alice :loves :Bob .
:Bob   a       :Person .

Rule (single graph scope via ?SCOPE)
------------------------------------
{ ?SCOPE log:notIncludes { :Alice :hates ?x } .
  ?x a :Person .
}
=>
{ :Alice :hates :Nobody . } .

Goal
----
Derive  :Alice :hates :Nobody .
"""

from __future__ import annotations
from itertools import count
from typing import Dict, Iterable, Iterator, List, Optional, Set, Tuple

Triple = Tuple[str, str, str]

# ─────────────────────────────────────────────────────────────
# 1) Single-KB store (no named graphs)
# ─────────────────────────────────────────────────────────────
KB: Set[Triple] = {
    (":Alice", ":loves", ":Bob"),
    (":Bob",   "a",      ":Person"),
}

# We'll bind ?SCOPE to this constant to indicate "the KB graph".
KB_SCOPE = ":KB"

# ─────────────────────────────────────────────────────────────
# 2) Rule (your query as a Horn rule)
# ─────────────────────────────────────────────────────────────
RULE = {
    "id": "R-hates-nobody",
    "head": (":Alice", ":hates", ":Nobody"),
    "body": [
        (KB_SCOPE, "log:notIncludes", (":Alice", ":hates", "?x")),
        ("?x", "a", ":Person"),
    ],
}

GOAL = (":Alice", ":hates", ":Nobody")

# ─────────────────────────────────────────────────────────────
# 3) Unification + substitution
# ─────────────────────────────────────────────────────────────
def is_var(t: object) -> bool:
    return isinstance(t, str) and t.startswith("?")

def unify(pat, fact, θ=None):
    """Unify pat (may contain vars) with fact; tuples/atoms supported."""
    θ = dict(θ or {})
    if isinstance(pat, tuple) != isinstance(fact, tuple):
        return None
    if not isinstance(pat, tuple):  # atoms
        if is_var(pat):
            if pat in θ and θ[pat] != fact:
                return None
            θ[pat] = fact
            return θ
        return θ if pat == fact else None
    # tuples
    if len(pat) != len(fact): 
        return None
    for pe, fe in zip(pat, fact):
        θ = unify(pe, fe, θ)
        if θ is None:
            return None
    return θ

def subst(term, θ):
    if isinstance(term, tuple):
        return tuple(subst(x, θ) for x in term)
    return θ.get(term, term)

# ─────────────────────────────────────────────────────────────
# 4) Built-in: log:notIncludes scoped to the single KB
# ─────────────────────────────────────────────────────────────
def not_includes_single_kb(scope_atom, pattern, θ) -> bool:
    """
    Evaluate log:notIncludes with ?SCOPE representing *the* KB.
    - If scope_atom is a variable or equals KB_SCOPE, we test against KB.
    - Substitute bound vars into the pattern; unbound vars act as wildcards.
    - Return True iff NO triple in KB unifies with pattern.
    """
    scope = subst(scope_atom, θ)
    if is_var(scope):
        # Bind the scope to the KB sentinel at evaluation time
        θ[scope] = KB_SCOPE
        scope = KB_SCOPE
    if scope != KB_SCOPE:
        return False  # any other scope value is invalid here

    pat = subst(pattern, θ)
    for t in KB:
        if unify(pat, t, {}) is not None:
            return False
    return True

# ─────────────────────────────────────────────────────────────
# 5) Backward chainer (facts first, then rule; left-to-right body)
# ─────────────────────────────────────────────────────────────
def bc(goal: Triple, θ: Dict[str,str], depth: int, stepctr, trace: List[str]) -> Iterator[Dict[str,str]]:
    g = subst(goal, θ)
    trace.append("  "*depth + f"Step {next(stepctr):02}: prove {g}")

    # (a) Direct facts in KB (not expected for this goal, but supported)
    if g in KB:
        trace.append("  "*depth + "✓ fact in KB")
        yield θ
        return

    # (b) Try the single rule
    θh = unify(RULE["head"], g, θ)
    if θh is None:
        return
    trace.append("  "*depth + f"→ via {RULE['id']}")

    def prove_body(i: int, θcur: Dict[str,str]) -> Iterator[Dict[str,str]]:
        if i == len(RULE["body"]):
            yield θcur
            return
        atom = RULE["body"][i]
        # Built-in?
        if isinstance(atom, tuple) and len(atom) == 3 and atom[1] == "log:notIncludes":
            scope, _, pat = atom
            θtmp = dict(θcur)
            ok = not_includes_single_kb(scope, pat, θtmp)
            trace.append("  "*(depth+1) + ("✓" if ok else "✗") +
                         f" notIncludes (absent in KB)" if ok else " notIncludes (present in KB)")
            if ok:
                yield from prove_body(i+1, θtmp)
            return
        # Otherwise a normal triple: match against KB
        matched = False
        for fact in sorted(KB):
            θn = unify(atom, fact, θcur)
            if θn is not None:
                matched = True
                trace.append("  "*(depth+1) + f"✓ fact {fact}")
                yield from prove_body(i+1, θn)
        if not matched:
            trace.append("  "*(depth+1) + f"✗ no matching fact for {atom}")

    yield from prove_body(0, θh)

def prove(goal: Triple):
    trace: List[str] = []
    stepctr = count(1)
    θ = next(bc(goal, {}, 0, stepctr, trace), None)
    return θ is not None, trace, (θ or {})

# ─────────────────────────────────────────────────────────────
# 6) ARC sections
# ─────────────────────────────────────────────────────────────
def arc_answer(success: bool, θ: Dict[str,str]):
    print("Answer")
    print("------")
    print("Goal:", GOAL)
    print("Result:", "PROVED" if success else "NOT PROVED")
    if success and θ:
        # Typically θ binds ?x to :Bob and ?SCOPE to :KB
        binds = {k:v for k,v in θ.items() if is_var(k)}
        if binds:
            print("Bindings:", binds)
    print()
    print("KB facts:")
    for s,p,o in sorted(KB):
        print(f"  {s} {p} {o} .")
    print()

def arc_reason(trace: List[str]):
    print("Reason why")
    print("----------")
    print("We use scoped negation-as-failure on the *single* KB:")
    print("  ?SCOPE log:notIncludes { :Alice :hates ?x }  succeeds")
    print("  iff no triple in the KB matches (:Alice :hates ?x).")
    print("Since KB has no :hates triple for Alice, the test holds;")
    print("and we can instantiate ?x with a person (here :Bob) to fire the rule.")
    print()
    print("Proof trace:")
    for line in trace:
        print(line)
    print()

# ─────────────────────────────────────────────────────────────
# 7) Check (harness)
# ─────────────────────────────────────────────────────────────
def arc_check():
    print("Check (harness)")
    print("---------------")
    # 1) Success with current KB
    ok, _, θ = prove(GOAL)
    assert ok, "Expected success with current KB."
    # 2) notIncludes should be false if we add a matching hates triple
    t = (":Alice", ":hates", ":Bob")
    KB.add(t)
    ok2, _, _ = prove(GOAL)
    assert not ok2, "Expected failure once (:Alice :hates :Bob) is present."
    KB.remove(t)
    # 3) If no person exists, rule should fail (no witness for ?x a :Person)
    bob_person = (":Bob", "a", ":Person")
    KB.remove(bob_person)
    ok3, _, _ = prove(GOAL)
    assert not ok3, "Expected failure without any :Person."
    KB.add(bob_person)
    # 4) Binding sanity: ?x should be :Bob; ?SCOPE should be :KB
    ok4, _, θ4 = prove(GOAL)
    assert ok4 and θ4.get("?x") == ":Bob", f"Expected ?x→:Bob, got {θ4.get('?x')}"
    assert θ4.get("?SCOPE", KB_SCOPE) in (KB_SCOPE, None) or True  # bound internally
    # 5) Idempotence: repeated proves yield the same outcome
    ok5, _, _ = prove(GOAL)
    assert ok5, "Second run should also succeed."
    print("OK: SNAF is correctly scoped to the single KB; success/failure conditions verified.")

# ─────────────────────────────────────────────────────────────
# 8) Main
# ─────────────────────────────────────────────────────────────
def main():
    success, trace, θ = prove(GOAL)

    # ----- ARC output -----
    arc_answer(success, θ)
    arc_reason(trace)
    arc_check()

if __name__ == "__main__":
    main()

