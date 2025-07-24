#!/usr/bin/env python3
"""
Self-contained translation of
https://github.com/eyereasoner/eye/tree/master/reasoning/proof-by-induction

• Example 1  — inductive definition of  :sum
• Example 2  — Kaprekar 6174 process, counting recursion depth
"""

from __future__ import annotations
from typing import Dict, List, Tuple

# ─────────────────────────── Example 1: Σ 1..n via induction
def inductive_sum(n: int) -> int:
    """
    Inductively generate facts  k :sum S(k)  using
        0 :sum 0                                   (base)
        n>0 ∧ (n-1 :sum s) ⇒ n :sum (s+n)          (step)
    exactly as in example1.n3.
    """
    facts: Dict[int, int] = {0: 0}
    for k in range(1, n + 1):
        facts[k] = facts[k - 1] + k
    return facts[n]


# ─────────────────────────── Example 2: Kaprekar recursion depth
def kaprekar_steps(start: int) -> int:
    """
    Return the number of Kaprekar-6174 iterations needed for a 4-digit
    number   start   (leading zeros allowed) until it reaches
        • 6174  ➔  stop,
        • 0000  ➔  stop (all digits identical).
    Mirrors the recursive rule in example2.n3.
    """
    n = start
    seen = set()
    steps = 0
    while True:
        if n == 6174 or n == 0:
            return steps
        if n in seen:                   # just in case: avoid infinite loops
            raise ValueError("cycle")
        seen.add(n)

        digits = f"{n:04d}"
        asc = int("".join(sorted(digits)))
        desc = int("".join(sorted(digits, reverse=True)))
        n = desc - asc
        steps += 1


def kaprekar_table(limit: int = 10000) -> List[Tuple[int, int]]:
    """(n, steps) for all 1 ≤ n < limit."""
    return [(n, kaprekar_steps(n)) for n in range(1, limit)]


# ─────────────────────────── Demo
if __name__ == "__main__":
    print("Proof-by-induction demonstration")

    # —— Example 1 ——————————————————————————
    N = 4096
    s = inductive_sum(N)
    print(f"Example 1: Σ₁…{N} = {s:,}")

    # —— Example 2 ——————————————————————————
    tbl = kaprekar_table(10000)
    maxi = max(c for _, c in tbl)
    assert maxi == 7                    # matches the EYE answer set
    print(f"Example 2: 1–9999 all reach 6174 (or 0000) in ≤ {maxi} steps")

    # quick sanity check: the repository asked specifically about 10000
    print(f"           {9957:d} → {kaprekar_steps(9957)} steps\n")

