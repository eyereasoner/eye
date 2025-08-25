#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
forward_more.py — Forward-chaining with full proof trace (ARC-ified)
────────────────────────────────────────────────────────────────────────

Description
-----------
We compute the relation :moreInterestingThan ⊆ Entities×Entities using
three forward rules over a small fact base, and we print:

  • Answer      – results for the original queries + compact listings
  • Reason why  – the *ordered* forward proof trace (each derived edge)
  • Check       – a harness that re-derives the closure independently and
                  verifies soundness (facts, rules, and transitivity closure)

Rules
-----
R1 (score):         X > Y  ←  (X :score SX), (Y :score SY), SX > SY
R2 (award/boring):  X > Y  ←  (X :hasAward true), (Y :boring true)
R3 (transitive):    X > Z  ←  X > Y, Y > Z

Facts
-----
:A :score 8 .
:B :score 5 .
:B :moreInterestingThan :C .
:D :hasAward true .
:B :boring true .

Queries (as in the original script)
-----------------------------------
(:A, :B), (:A, :C), (:D, :B), (:C, :A)
and the open queries:
  - Who is more interesting than :C?
  - All pairs (?X > ?Y)
"""

from collections import deque
from typing import List, Tuple, Deque, Set, Dict, Iterable

# ---------- FACTS ----------
facts: List[Tuple[str, str, object]] = [
    (":A", ":score", 8),
    (":B", ":score", 5),
    (":B", ":moreInterestingThan", ":C"),
    (":D", ":hasAward", True),
    (":B", ":boring", True),
]

entities: List[str] = sorted({s for s, _, _ in facts} |
                             {o for _, _, o in facts if isinstance(o, str)})

# ---------- HELPERS ----------
def score(e: str) -> int | None:
    return next((v for s, p, v in facts if s == e and p == ":score"), None)

def award(e: str) -> bool:
    return any(v is True for s, p, v in facts if s == e and p == ":hasAward")

def boring(e: str) -> bool:
    return any(v is True for s, p, v in facts if s == e and p == ":boring")

# ---------- DERIVATION ENGINE (build trace + closure) ----------
def derive_all() -> tuple[Set[Tuple[str, str]], List[str]]:
    """
    Return (derived_pairs, trace_lines) in the *order* they were proven.
    Pairs are edges (X,Y) meaning X :moreInterestingThan Y.
    """
    lines: List[str] = []
    derived: Set[Tuple[str, str]] = set()

    # 1) Direct facts
    for s, p, o in facts:
        if p == ":moreInterestingThan":
            if (s, o) not in derived:
                derived.add((s, o))
                lines.append(f"✓ fact ({s}, :moreInterestingThan, {o})")

    # 2) Score rule (lexicographic to keep output deterministic)
    for a in entities:
        for b in entities:
            sa, sb = score(a), score(b)
            if sa is not None and sb is not None and sa > sb:
                if (a, b) not in derived:
                    derived.add((a, b))
                    lines.append(f"✓ {a} > {b}  via score rule ({sa} > {sb})")

    # 3) Award / boring rule
    for a in entities:
        for b in entities:
            if award(a) and boring(b):
                if (a, b) not in derived:
                    derived.add((a, b))
                    lines.append(f"✓ {a} > {b}  via award/boring rule")

    # 4) Transitive closure (FIFO, deterministic order)
    queue: Deque[Tuple[str, str]] = deque(sorted(derived))
    while queue:
        a, b = queue.popleft()
        for c in sorted(entities):
            if (b, c) in derived and (a, c) not in derived:
                derived.add((a, c))
                queue.append((a, c))
                lines.append(f"✓ {a} > {c}  via transitivity ({a} > {b} and {b} > {c})")

    return derived, lines

DERIVED, TRACE = derive_all()

def prove(a: str, b: str) -> bool:
    """Query helper: is (a,b) in the derived closure?"""
    return (a, b) in DERIVED

# ---------- ARC: Answer ----------
def arc_answer() -> None:
    print("Answer")
    print("------")
    # Original point queries
    queries = [(":A", ":B"), (":A", ":C"), (":D", ":B"), (":C", ":A")]
    for a, b in queries:
        print(f"({a}, :moreInterestingThan, {b})  →  {'YES' if prove(a,b) else 'NO'}")

    # Who > :C ?
    winners = [e for e in entities if prove(e, ":C")]
    print("\nWho is more interesting than :C?")
    print("  " + (", ".join(winners) if winners else "(none)"))

    # All pairs (?X > ?Y)
    print("\nAll pairs ?X > ?Y:")
    pairs = sorted(DERIVED)
    for x, y in pairs:
        print(f"  {x} > {y}")
    print()

# ---------- ARC: Reason why ----------
def arc_reason() -> None:
    print("Reason why")
    print("----------")
    print("Forward-chaining proof trace (each derived edge in order):")
    for line in TRACE:
        print(line)
    print()

# ---------- ARC: Check (harness) ----------
def reference_closure() -> Set[Tuple[str, str]]:
    """
    Independent re-derivation: iterate R1/R2 until quiescence, then take
    transitive closure. This should match DERIVED exactly.
    """
    base: Set[Tuple[str, str]] = set()
    # facts
    for s, p, o in facts:
        if p == ":moreInterestingThan":
            base.add((s, o))
    # R1/R2 saturation
    changed = True
    while changed:
        changed = False
        # R1 (score)
        for a in entities:
            for b in entities:
                sa, sb = score(a), score(b)
                if sa is not None and sb is not None and sa > sb and (a, b) not in base:
                    base.add((a, b)); changed = True
        # R2 (award/boring)
        for a in entities:
            for b in entities:
                if award(a) and boring(b) and (a, b) not in base:
                    base.add((a, b)); changed = True
    # Transitive closure
    closure = set(base)
    added = True
    while added:
        added = False
        for x, y in list(closure):
            for z in entities:
                if (y, z) in closure and (x, z) not in closure:
                    closure.add((x, z)); added = True
    return closure

def arc_check() -> None:
    print("Check (harness)")
    print("---------------")
    ref = reference_closure()
    # (1) Equality with independent derivation
    assert DERIVED == ref, f"Derived closure differs from reference.\nDerived: {sorted(DERIVED)}\nRef: {sorted(ref)}"

    # (2) Sanity: expected edges present / absent
    expected_present = {(":B", ":C"), (":A", ":B"), (":D", ":B"), (":A", ":C"), (":D", ":C")}
    for edge in expected_present:
        assert edge in DERIVED, f"Missing expected edge {edge}"
    assert (":C", ":A") not in DERIVED, "Unexpected edge (:C,:A)"

    # (3) Closure property: if x>y and y>z then x>z
    for x, y in DERIVED:
        for z in entities:
            if (y, z) in DERIVED:
                assert (x, z) in DERIVED, f"Transitivity broken for {x}>{y}>{z}"

    # (4) Rule soundness checks
    #     — every score-comparison should be in DERIVED
    for a in entities:
        for b in entities:
            sa, sb = score(a), score(b)
            if sa is not None and sb is not None and sa > sb:
                assert (a, b) in DERIVED, f"Score rule missing {a}>{b} ({sa}>{sb})"
    #     — every award/boring pair should be in DERIVED
    for a in entities:
        for b in entities:
            if award(a) and boring(b):
                assert (a, b) in DERIVED, f"Award/boring rule missing {a}>{b}"

    print("OK: re-derivation matches; expected edges present; closure & rule soundness verified.\n")

# ---------- Main ----------
if __name__ == "__main__":
    arc_answer()
    arc_reason()
    arc_check()

