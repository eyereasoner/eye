#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
fuse.py  –  backward-chaining demo for the “fuse” rule (ARC-ified)
───────────────────────────────────────────────────────────────────────
Based on: https://github.com/eyereasoner/eye/blob/master/reasoning/fuse/fuse.n3

Data
----
:stone :color :black .
:stone :color :white .

Rule
----
{ ?X :color :black .
  ?X :color :white . }  =>  false .

Goal
----
false       (i.e., derive a contradiction when an object has two incompatible colors)

What this script adds
---------------------
• Answer — shows whether `false` is derivable and prints a numbered proof trace.
• Reason why — explains the rule and how the backward prover fires it.
• Check (harness) — asserts there’s an entity with both colors, that the
  rule indeed proves `false`, and that dropping either color prevents a proof.
"""

from __future__ import annotations
from itertools import count
from typing import Dict, Iterable, List, Optional, Tuple

# ─────────────────────────────────────────────────────────────
# 1) KB: facts and the single “fuse” rule
# ─────────────────────────────────────────────────────────────
Triple = Tuple[str, str, str]

facts: set[Triple] = {
    (":stone", ":color", ":black"),
    (":stone", ":color", ":white"),
}

rule = {
    "id": "R-fuse",
    "head": ("false",),                     # 1-tuple head
    "body": [
        ("?X", ":color", ":black"),
        ("?X", ":color", ":white"),
    ],
}

# ─────────────────────────────────────────────────────────────
# 2) Unification & substitution helpers
# ─────────────────────────────────────────────────────────────
Subst = Dict[str, str]
is_var = lambda t: isinstance(t, str) and t.startswith("?")

def unify(pat, fact, θ: Optional[Subst] = None) -> Optional[Subst]:
    """Positional unification (tuples of same arity)."""
    θ = dict(θ or {})
    if isinstance(pat, tuple) != isinstance(fact, tuple):
        return None
    if isinstance(pat, tuple):
        if len(pat) != len(fact):
            return None
        for p, f in zip(pat, fact):
            θ = unify(p, f, θ)
            if θ is None:
                return None
        return θ
    # atom case
    if is_var(pat):
        if pat in θ and θ[pat] != fact:
            return None
        θ[pat] = fact
        return θ
    return θ if pat == fact else None

def subst(term, θ: Subst):
    """Apply substitution to atom or tuple."""
    if isinstance(term, tuple):
        return tuple(subst(x, θ) for x in term)
    return θ.get(term, term)

# ─────────────────────────────────────────────────────────────
# 3) Backward-chaining with a pretty trace
# ─────────────────────────────────────────────────────────────
def bc(goal, θ: Subst, depth: int, stepper, trace: List[str]) -> Iterable[Subst]:
    g = subst(goal, θ)
    k = next(stepper)
    pad = "  " * depth
    trace.append(f"{pad}Step {k:02}: prove {g}")

    # (a) try facts (no ('false',) fact exists, but kept for completeness)
    for f in sorted(facts):
        θ2 = unify(g, f, θ)
        if θ2 is not None:
            trace.append(f"{pad}  ✓ fact {f}")
            yield θ2   # keep searching for alternative proofs

    # (b) try the rule
    θh = unify(rule["head"], g, θ)
    if θh is None:
        return
    trace.append(f"{pad}  → via {rule['id']}")
    def prove_body(i: int, θcur: Subst) -> Iterable[Subst]:
        if i == len(rule["body"]):
            yield θcur
        else:
            for θnext in bc(rule["body"][i], θcur, depth+1, stepper, trace):
                yield from prove_body(i+1, θnext)
    yield from prove_body(0, θh)

# Convenience: run once and collect the first proof (if any)
def prove_false_once() -> Tuple[bool, List[str]]:
    trace: List[str] = []
    stepper = count(1)
    gen = bc(("false",), {}, 0, stepper, trace)
    proved = next(gen, None) is not None
    return proved, trace

# ─────────────────────────────────────────────────────────────
# 4) ARC — Answer / Reason / Check
# ─────────────────────────────────────────────────────────────
def arc_answer() -> None:
    ok, trace = prove_false_once()

    print("Answer")
    print("------")
    print("Goal: false")
    print("Result:", "✔ PROVED" if ok else "✗ NOT PROVED")
    print("\nProof trace:")
    for line in trace:
        print(line)
    print()

def arc_reason() -> None:
    print("Reason why")
    print("----------")
    print("The rule encodes a *fuse* (inconsistency detector):")
    print("  if some ?X has color :black AND color :white, then derive `false`.")
    print("In the data, :stone has both :black and :white, so the body succeeds")
    print("with ?X←:stone and the head `false` is concluded. The backward prover")
    print("starts from the goal `false`, unifies it with the rule head, then")
    print("proves each body atom from the facts.\n")

def arc_check() -> None:
    print("Check (harness)")
    print("---------------")
    # 1) There exists an entity with both colors
    entities = {s for (s,p,o) in facts if p == ":color"}
    both = [x for x in entities
            if (x, ":color", ":black") in facts and (x, ":color", ":white") in facts]
    assert both, "No entity has both :black and :white in the KB."

    # 2) The rule proves false
    proved, _ = prove_false_once()
    assert proved, "Expected to prove false but did not."

    # 3) Removing either color breaks the proof
    for missing in [(":stone", ":color", ":black"),
                    (":stone", ":color", ":white")]:
        facts.remove(missing)
        proved2, _ = prove_false_once()
        assert not proved2, "Removing one color should prevent proving false."
        facts.add(missing)  # restore

    print("OK: both-colors entity exists; rule fires; proof disappears if any color is removed.\n")

# ─────────────────────────────────────────────────────────────
# 5) Main
# ─────────────────────────────────────────────────────────────
if __name__ == "__main__":
    arc_answer()
    arc_reason()
    arc_check()

