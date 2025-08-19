#!/usr/bin/env python3
# -*- coding: utf-8 -*-
r"""
water_logic.py — ARC-ified

Backward-proof (purely logical) version of water.py.

Facts
-----
inorganic_compound(water).

Exclusive choice (no weights, just exclusivity)
-----------------------------------------------
exactly one of { solid(water), liquid(water), gas(water) } is true.
(Three mutually exclusive worlds.)

Rule
----
observable(A) ← solid(A) ∨ liquid(A) ∨ gas(A).

Queries
-------
prop(observable,water)
prop(solid,water)
prop(liquid,water)
prop(gas,water)

For each query we classify it over the three worlds as:
  VALID           — true in all worlds
  SATISFIABLE     — true in some but not all worlds
  UNSATISFIABLE   — true in no world
And we print a short proof trace explaining the truth in each world.
"""

from __future__ import annotations
from dataclasses import dataclass
from typing import Dict, List, Tuple

# ─────────────────────────────────────────────────────────────
# 1) Worlds for the annotated disjunction
# ─────────────────────────────────────────────────────────────

World = str  # one of: "solid", "liquid", "gas"
WORLDS: List[World] = ["solid", "liquid", "gas"]


# ─────────────────────────────────────────────────────────────
# 2) Deterministic truth assignment inside a world
# ─────────────────────────────────────────────────────────────

def atoms_in(world: World, arg: str = "water") -> Dict[str, bool]:
    """
    Truth assignment for predicates about the constant 'water' under the given world (phase).
    We encode the exclusive choice by making *exactly one* of the phase predicates true.
    The rule observable(A) ← solid(A) ∨ liquid(A) ∨ gas(A) is realized as disjunction.
    """
    is_solid = world == "solid"
    is_liquid = world == "liquid"
    is_gas = world == "gas"

    # observable(A) ← solid(A) ∨ liquid(A) ∨ gas(A)
    is_observable = is_solid or is_liquid or is_gas

    return {
        "inorganic_compound": True,  # fact: inorganic_compound(water)
        "solid": is_solid,
        "liquid": is_liquid,
        "gas": is_gas,
        "observable": is_observable,
    }


# ─────────────────────────────────────────────────────────────
# 3) Query parser + evaluation (logical only)
# ─────────────────────────────────────────────────────────────

def parse_query(q: str) -> Tuple[str, str]:
    """Parse 'prop(pred,arg)' and return (pred, arg)."""
    q = q.strip()
    assert q.startswith("prop(") and q.endswith(")"), f"Bad query: {q}"
    inner = q[len("prop("):-1]
    pred, arg = inner.split(",", 1)
    return pred.strip(), arg.strip()

def holds_in_world(pred: str, world: World, arg: str) -> bool:
    mapping = atoms_in(world, arg)
    if pred not in mapping:
        raise ValueError(f"Unknown predicate: {pred!r}")
    return mapping[pred]


# ─────────────────────────────────────────────────────────────
# 4) Result containers + classification
# ─────────────────────────────────────────────────────────────

@dataclass
class QueryResult:
    query: str
    holds_in: List[World]        # worlds where it holds
    classification: str          # VALID / SATISFIABLE / UNSATISFIABLE
    trace_lines: List[str]       # per-world truth with small reasons

def classify(holds_in: List[World], total: int) -> str:
    k = len(holds_in)
    if k == total:
        return "VALID"
    if k == 0:
        return "UNSATISFIABLE"
    return f"SATISFIABLE ({k}/{total} worlds)"


def prove(query: str) -> QueryResult:
    """
    Compute the truth of a ground query over all worlds and build an ARC-friendly trace.
    We give a tiny reason for 'observable' using the rule; phases are self-evident.
    """
    pred, arg = parse_query(query)
    holds: List[World] = []
    trace: List[str] = []
    for idx, world in enumerate(WORLDS, 1):
        truth = holds_in_world(pred, world, arg)
        reason = ""
        if pred == "observable":
            if world == "solid":
                reason = " (solid ⇒ observable by rule)"
            elif world == "liquid":
                reason = " (liquid ⇒ observable by rule)"
            elif world == "gas":
                reason = " (gas ⇒ observable by rule)"
        glyph = "✓" if truth else "✗"
        trace.append(f"World {idx}: phase={world:<6} {glyph} {pred}{reason}")
        if truth:
            holds.append(world)
    return QueryResult(query, holds, classify(holds, len(WORLDS)), trace)


# ─────────────────────────────────────────────────────────────
# 5) ARC-style presentation
# ─────────────────────────────────────────────────────────────

def arc_answer(results: List[QueryResult]) -> None:
    print("Answer")
    print("------")
    for r in results:
        worlds_note = "" if (r.classification in {"VALID", "UNSATISFIABLE"}) else " → " + ", ".join(r.holds_in)
        print(f"{r.query}: {r.classification}{worlds_note}")
    print()

def arc_reason(results: List[QueryResult]) -> None:
    print("Reason why")
    print("----------")
    print("We consider three mutually exclusive worlds for the phase of water: solid, liquid, gas.")
    print("The rule observable(A) holds if any phase predicate holds in that world.")
    print()
    for r in results:
        print(f"[Trace for {r.query}]")
        for line in r.trace_lines:
            print("  " + line)
        print()


# ─────────────────────────────────────────────────────────────
# 6) Check (harness)
# ─────────────────────────────────────────────────────────────

def check_harness(results: List[QueryResult]) -> None:
    """
    Re-verify:
      1) Exclusivity: in every world exactly one of {solid, liquid, gas} is True.
      2) observable is VALID (true in all worlds).
      3) Each phase predicate is SATISFIABLE (true in exactly one world).
      4) Classification strings match counts.
    """
    # 1) Exclusivity
    for w in WORLDS:
        mp = atoms_in(w)
        phases_true = sum(1 for k in ("solid", "liquid", "gas") if mp[k])
        assert phases_true == 1, f"Exclusivity broken in world={w}: {mp}"

    # Build a quick lookup
    res_map = {r.query: r for r in results}

    # 2) observable VALID
    obs = res_map["prop(observable,water)"]
    assert obs.classification == "VALID" and len(obs.holds_in) == len(WORLDS), "observable should be VALID."

    # 3) phase predicates SATISFIABLE in exactly 1 world
    for q in ("prop(solid,water)", "prop(liquid,water)", "prop(gas,water)"):
        r = res_map[q]
        assert r.classification.startswith("SATISFIABLE"), f"{q} should be SATISFIABLE."
        assert len(r.holds_in) == 1, f"{q} should hold in exactly one world; got {r.holds_in}"

    # 4) Classifications consistent with counts
    total = len(WORLDS)
    for r in results:
        k = len(r.holds_in)
        if k == 0:
            assert r.classification == "UNSATISFIABLE"
        elif k == total:
            assert r.classification == "VALID"
        else:
            assert r.classification.startswith("SATISFIABLE"), f"Bad classification for {r.query}: {r.classification}"


# ─────────────────────────────────────────────────────────────
# 7) Main — run all queries with ARC sections
# ─────────────────────────────────────────────────────────────

def main():
    queries = (
        "prop(observable,water)",
        "prop(solid,water)",
        "prop(liquid,water)",
        "prop(gas,water)",
    )
    results = [prove(q) for q in queries]

    # ----- ARC output -----
    arc_answer(results)
    arc_reason(results)

    print("Check (harness)")
    print("---------------")
    try:
        check_harness(results)
        print("OK: exclusivity holds; traces match; classifications are consistent.")
    except AssertionError as e:
        print("FAILED:", e)
        raise

if __name__ == "__main__":
    main()

