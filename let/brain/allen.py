#!/usr/bin/env python3
"""
Qualitative temporal reasoning à la Allen (1983).

This is a tiny stand-alone replica of the EYE examples in
https://github.com/eyereasoner/eye/tree/master/reasoning/allen

• It implements the 13 base relations and their converses.
• It derives new constraints by path-consistency (intersection of
  relation sets with the ∘ composition operator).
• The composition operator itself is generated *on the fly* by
  brute-force enumeration of small numeric intervals, so no gigantic
  13 × 13 hard-coded table is needed.

Two little scenarios (mirrors of EYE’s `example1.n3` and `example2.n3`)
are hard-coded at the bottom for a runnable demo.
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

# ───────────────────────── Demo scenarios (= EYE’s two examples)
def run_examples():
    print("Allen-interval-algebra demonstration")

    # Example 1  — the well-known dinner / newspaper / bed narrative
    n, d, b = "newspaper", "dinner", "bed"
    nodes = [n, d, b]
    facts = [
        (n, d, "during"),   # newspaper DURING dinner
        (d, b, "before"),   # dinner   BEFORE bed
    ]
    rels = allen_closure(nodes, facts)
    print("Example 1  – narrative:")
    print(f"  {n:<9} {next(iter(rels[(n, b)])):^8} {b}       ✅")

    # Example 2  — a simple chain:  a MEETS b ; b MEETS c  ⇒  a BEFORE c
    a, x, c = "a", "b", "c"
    nodes = [a, x, c]
    facts = [(a, x, "meets"), (x, c, "meets")]
    rels = allen_closure(nodes, facts)
    print("\nExample 2  – simple chain:")
    print(f"  {a}  {next(iter(rels[(a, c)])):^8}  {c}                ✅")


# ───────────────────────── main entry point
if __name__ == "__main__":
    run_examples()

