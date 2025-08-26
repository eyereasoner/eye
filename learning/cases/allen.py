#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
allen.py — Qualitative temporal reasoning à la Allen (ARC-ified)
────────────────────────────────────────────────────────────────────

What this is
------------
A tiny, self-contained replica of the EYE examples in:
  https://github.com/eyereasoner/eye/tree/master/reasoning/allen

• Implements the 13 Allen base relations and their converses.
• Derives new constraints by *path consistency*:
      R[i,j] ← R[i,j] ∩ ( ⋃_k  R[i,k] ∘ R[k,j] )
• The composition operator ∘ is generated *on the fly* by brute-force
  enumeration over a small grid of integer intervals — so we avoid a big,
  hard-coded 13×13 table while remaining correct for qualitative reasoning.

ARC output
----------
• Answer
    – Deductions for two scenarios (mirrors of EYE’s example1.n3 / example2.n3).
    – Compact relation matrices (singleton sets shown as the relation; else a set).

• Reason why
    – Explains Allen relations, composition by enumeration, and path consistency.
    – Shows a few key composition results (e.g., during ∘ before ⇒ before).

• Check (harness)
    – Verifies:
        1) symmetry via converses:  R[j,i] = inv(R[i,j])
        2) path-consistency fixpoint: each R[i,j] ⊆ ⋃_k R[i,k]∘R[k,j]
        3) the expected singletons in example 1 and 2
        4) spot-checks of composition (meets∘meets = {before}, etc.)
"""

from __future__ import annotations
from itertools import product
from typing import Dict, List, Set, Tuple

# ───────────────────────── 13 base relations & converses
BASE = [
    "before", "after",
    "meets", "met-by",
    "overlaps", "overlapped-by",
    "starts", "started-by",
    "during", "contains",
    "finishes", "finished-by",
    "equal",
]
INV = {  # converse map
    "before": "after",      "after": "before",
    "meets": "met-by",      "met-by": "meets",
    "overlaps": "overlapped-by", "overlapped-by": "overlaps",
    "starts": "started-by", "started-by": "starts",
    "during": "contains",   "contains": "during",
    "finishes": "finished-by", "finished-by": "finishes",
    "equal": "equal",
}

# ───────────────────────── numeric test for each relation
def satisfies(rel: str, a: Tuple[int, int], b: Tuple[int, int]) -> bool:
    s1, e1 = a
    s2, e2 = b
    if rel == "before":           return e1 < s2
    if rel == "after":            return s1 > e2
    if rel == "meets":            return e1 == s2
    if rel == "met-by":           return s1 == e2
    if rel == "overlaps":         return s1 < s2 < e1 < e2
    if rel == "overlapped-by":    return s2 < s1 < e2 < e1
    if rel == "starts":           return s1 == s2 and e1 <  e2
    if rel == "started-by":       return s1 == s2 and e1 >  e2
    if rel == "finishes":         return e1 == e2 and s1 >  s2
    if rel == "finished-by":      return e1 == e2 and s1 <  s2
    if rel == "during":           return s2 <  s1 and e1 <  e2
    if rel == "contains":         return s1 <  s2 and e2 <  e1
    if rel == "equal":            return s1 == s2 and e1 == e2
    raise ValueError(rel)

# ───────────────────────── lazy composition operator
COMP: Dict[Tuple[str, str], Set[str]] = {}

def compose(r1: str, r2: str) -> Set[str]:
    """Return the (possibly non-singleton) set R₁∘R₂."""
    key = (r1, r2)
    if key in COMP:
        return COMP[key]

    # brute-force search over a small grid of integer intervals
    rng = range(0, 6)                      # endpoints 0…5     (enough!)
    intervals = [(s, e) for s in rng for e in rng if s < e]
    out: Set[str] = set()
    for a, b, c in product(intervals, repeat=3):
        if satisfies(r1, a, b) and satisfies(r2, b, c):
            for r in BASE:
                if satisfies(r, a, c):
                    out.add(r)
    COMP[key] = out
    return out

# ───────────────────────── Allen path-consistency closure
def allen_closure(nodes: List[str],
                  facts: List[Tuple[str, str, str]]
                  ) -> Dict[Tuple[str, str], Set[str]]:
    """
    Minimise each pair’s relation-set by enforcing
        Rᵢⱼ  ←  Rᵢⱼ ∩ ( ⋃_{k} Rᵢₖ∘Rₖⱼ ).
    """
    full = set(BASE)
    R = { (i, j): (full.copy() if i != j else {"equal"})
          for i in nodes for j in nodes }

    # inject the given atomic facts
    for i, j, r in facts:
        R[(i, j)].intersection_update({r})
        R[(j, i)].intersection_update({INV[r]})

    changed = True
    while changed:
        changed = False
        for i in nodes:
            for j in nodes:
                if i == j:
                    continue
                for k in nodes:
                    if k in (i, j):
                        continue
                    allowed: Set[str] = set()
                    for r1 in R[(i, k)]:
                        for r2 in R[(k, j)]:
                            allowed |= compose(r1, r2)
                    new = R[(i, j)] & allowed
                    if new != R[(i, j)]:
                        R[(i, j)] = new
                        R[(j, i)] = {INV[r] for r in new}
                        changed = True
    return R

# ───────────────────────── Pretty helpers
def fmt_set(S: Set[str]) -> str:
    return next(iter(S)) if len(S) == 1 else "{" + ", ".join(sorted(S)) + "}"

def show_matrix(nodes: List[str], R: Dict[Tuple[str, str], Set[str]]) -> None:
    w = max(len(n) for n in nodes)
    header = " " * (w+2) + " ".join(n.rjust(w) for n in nodes)
    print(header)
    for i in nodes:
        row = [i.rjust(w) + "  "]
        for j in nodes:
            row.append(fmt_set(R[(i, j)]).rjust(w))
        print("".join(row))

# ───────────────────────── ARC sections
def arc_answer() -> None:
    print("Answer")
    print("------")
    # Example 1 — newspaper DURING dinner ; dinner BEFORE bed ⇒ newspaper BEFORE bed
    n, d, b = "newspaper", "dinner", "bed"
    nodes1 = [n, d, b]
    facts1 = [(n, d, "during"), (d, b, "before")]
    R1 = allen_closure(nodes1, facts1)
    rel_nb = fmt_set(R1[(n, b)])
    print("Example 1:")
    print(f"  Given: {n} during {d}, {d} before {b}")
    print(f"  Deduces: {n} {rel_nb} {b}\n")
    show_matrix(nodes1, R1)

    # Example 2 — a MEETS b ; b MEETS c ⇒ a BEFORE c
    a, x, c = "a", "b", "c"
    nodes2 = [a, x, c]
    facts2 = [(a, x, "meets"), (x, c, "meets")]
    R2 = allen_closure(nodes2, facts2)
    rel_ac = fmt_set(R2[(a, c)])
    print("\nExample 2:")
    print(f"  Given: {a} meets {x}, {x} meets {c}")
    print(f"  Deduces: {a} {rel_ac} {c}\n")
    show_matrix(nodes2, R2)
    print()

def arc_reason() -> None:
    print("Reason why")
    print("----------")
    print("• Allen’s 13 base relations capture all qualitative orderings of two closed intervals.")
    print("• Composition R₁∘R₂ is computed by enumerating small integer intervals a,b,c and")
    print("  collecting all R such that a R b and b R₂ c imply a R c. Only relative order")
    print("  matters, so a tiny grid (0…5) suffices to witness all qualitative patterns.")
    print("• Path consistency repeatedly tightens each pair (i,j) by intersecting R[i,j]")
    print("  with the union over k of composed paths R[i,k]∘R[k,j] until a fixpoint.")
    print("\nKey compositions (generated on the fly):")
    samples = [
        ("during", "before"),
        ("meets", "meets"),
        ("starts", "before"),
        ("overlaps", "meets"),
    ]
    for r1, r2 in samples:
        print(f"  {r1:>12} ∘ {r2:<12} ⇒ {sorted(compose(r1, r2))}")
    print()

def arc_check() -> None:
    print("Check (harness)")
    print("---------------")
    # Rebuild examples
    n, d, b = "newspaper", "dinner", "bed"
    R1 = allen_closure([n, d, b], [(n, d, "during"), (d, b, "before")])
    a, x, c = "a", "b", "c"
    R2 = allen_closure([a, x, c], [(a, x, "meets"), (x, c, "meets")])

    # 1) converses symmetry on both examples
    def check_sym(nodes, R):
        for i in nodes:
            for j in nodes:
                inv = {INV[r] for r in R[(i, j)]}
                assert inv == R[(j, i)], f"Converse mismatch for ({i},{j})."
    check_sym([n,d,b], R1)
    check_sym([a,x,c], R2)

    # 2) path-consistency fixpoint: R[i,j] ⊆ ⋃_k R[i,k]∘R[k,j]
    def check_pc(nodes, R):
        for i in nodes:
            for j in nodes:
                if i == j: continue
                allowed: Set[str] = set()
                for k in nodes:
                    if k in (i, j): continue
                    for r1 in R[(i, k)]:
                        for r2 in R[(k, j)]:
                            allowed |= compose(r1, r2)
                assert R[(i, j)] <= allowed, f"Path-consistency violated for ({i},{j})."
    check_pc([n,d,b], R1)
    check_pc([a,x,c], R2)

    # 3) expected singletons
    assert R1[(n, b)] == {"before"}, "Example 1 should deduce newspaper BEFORE bed."
    assert R2[(a, c)] == {"before"}, "Example 2 should deduce a BEFORE c."

    # 4) composition spot checks
    assert compose("meets", "meets") == {"before"}, "meets∘meets must be {before}."
    assert "before" in compose("during", "before"), "during∘before must include before."

    print("OK: converses, path-consistency, expected deductions, and composition spot-checks.\n")

# ───────────────────────── main entry point
if __name__ == "__main__":
    arc_answer()
    arc_reason()
    arc_check()

