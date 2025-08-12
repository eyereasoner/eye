#!/usr/bin/env python3
# -*- coding: utf-8 -*-
r"""
combinatorics.py

Original intent (kept exactly):
  • Print the power set of a given list of elements
  • For each k=1..n, print all k-combinations and k-permutations (and their counts)
  • Print combinations-with-replacement for k=2 (multiset combinations)

This version additionally:
  • Adds extensive comments explaining each section and formula
  • Structures the computation so we can verify identities in a "Check (harness)"
  • Produces three ARC-style sections on stdout:
        Answer      – results (counts and full listings)
        Reason why  – the combinatorial formulas behind those counts
        Check       – a harness that re-derives everything and asserts identities

Notes
-----
- For clarity, we assume the input list contains distinct elements.
- With 5 elements, outputs are modest (power set has 32 subsets).
- Python 3.8+ is fine (uses math.comb / math.perm).
"""

from __future__ import annotations
from dataclasses import dataclass
from typing import Any, Dict, List, Tuple
import itertools as it
import math


# ─────────────────────────────────────────────────────────────
# Data containers
# ─────────────────────────────────────────────────────────────

@dataclass
class Results:
    elements: List[Any]
    subsets: List[Tuple[Any, ...]]                      # all subsets (power set)
    combinations: Dict[int, List[Tuple[Any, ...]]]      # k -> all k-combinations
    permutations: Dict[int, List[Tuple[Any, ...]]]      # k -> all k-permutations
    cwr_k: int                                          # k used for combinations-with-replacement
    cwr_list: List[Tuple[Any, ...]]                     # all multiset combinations (k with repetition)


# ─────────────────────────────────────────────────────────────
# Core computation
# ─────────────────────────────────────────────────────────────

def advanced_combinatorics(elements: List[Any], cwr_k: int = 2) -> Results:
    """
    Compute:
      - Power set (via combinations for r = 0..n)
      - All k-combinations and k-permutations for k = 1..n
      - Combinations-with-replacement for a fixed k (default 2), as in the original
    """
    n = len(elements)

    # Power set = ⋃_{r=0}^{n} C(elements, r)
    subsets: List[Tuple[Any, ...]] = []
    for r in range(n + 1):
        subsets.extend(it.combinations(elements, r))

    # Combinations and permutations by k
    combos: Dict[int, List[Tuple[Any, ...]]] = {
        k: list(it.combinations(elements, k)) for k in range(1, n + 1)
    }
    perms: Dict[int, List[Tuple[Any, ...]]] = {
        k: list(it.permutations(elements, k)) for k in range(1, n + 1)
    }

    # Combinations with repetition (multiset combinations) for a fixed k
    cwr_list = list(it.combinations_with_replacement(elements, cwr_k))

    return Results(
        elements=elements,
        subsets=subsets,
        combinations=combos,
        permutations=perms,
        cwr_k=cwr_k,
        cwr_list=cwr_list,
    )


# ─────────────────────────────────────────────────────────────
# ARC-style sections
# ─────────────────────────────────────────────────────────────

def print_answer(res: Results) -> None:
    """Printout (plus minor formatting), then some summary counts."""
    n = len(res.elements)
    print("Answer")
    print("------")
    print(f"Elements: {res.elements}")
    print(f"Total number of subsets: 2^{n} = {2**n}\n")

    # Power set
    print("All subsets (power set):")
    for tup in res.subsets:
        print(tup)
    print("\n---\n")

    # Combinations & permutations per k
    for k in range(1, n + 1):
        c = math.comb(n, k)
        p = math.perm(n, k)
        print(f"k = {k}:")
        print(f" Number of combinations C({n},{k}) = {c}")
        print(f" All combinations of size {k}: {res.combinations[k]}")
        print(f" Number of permutations P({n},{k}) = {p}")
        print(f" All permutations of length {k}: {res.permutations[k]}\n")

    # Combinations with repetition (k fixed, like in the original)
    k = res.cwr_k
    cwr = math.comb(n + k - 1, k)
    print(f"Combinations with repetition: choosing {k} from {n} with repetition allowed")
    print(f" Number of multiset combinations: C({n + k - 1},{k}) = {cwr}")
    print(f" List: {res.cwr_list}")
    print()


def print_reason(res: Results) -> None:
    """Explain the formulas used."""
    n = len(res.elements)
    lines = []
    lines.append("Reason why")
    lines.append("----------")
    lines.append("Power set:")
    lines.append(f"  |P({res.elements})| = 2^{n} because each element is either included or not included.")
    lines.append("")
    lines.append("Combinations (order doesn’t matter):")
    lines.append("  C(n, k) = n! / (k! (n-k)!).")
    lines.append("")
    lines.append("Permutations (order matters, no repetition):")
    lines.append("  P(n, k) = n! / (n-k)! (i.e., n · (n-1) · ... · (n-k+1)).")
    lines.append("")
    lines.append("Combinations with repetition (multiset combinations):")
    lines.append("  # = C(n+k-1, k)  — stars and bars.")
    lines.append("")
    # quick numeric summary
    lines.append("Numeric summary for your input:")
    lines.append(f"  Power set size: 2^{n} = {2**n}")
    lines.append("  For k = 1..n:")
    for k in range(1, n + 1):
        lines.append(f"    C({n},{k}) = {math.comb(n,k)},   P({n},{k}) = {math.perm(n,k)}")
    lines.append(f"  With repetition (k={res.cwr_k}): C({n+res.cwr_k-1},{res.cwr_k}) = {math.comb(n+res.cwr_k-1,res.cwr_k)}")
    print("\n".join(lines))
    print()


# ─────────────────────────────────────────────────────────────
# Check (harness)
# ─────────────────────────────────────────────────────────────

def check_harness(res: Results) -> None:
    """
    Re-verify core identities and counts:
      1) |power set| = 2^n and equals sum_k C(n,k)
      2) |combinations[k]| = C(n,k), |permutations[k]| = P(n,k)
      3) |combinations_with_replacement(k)| = C(n+k-1,k)
      4) Input elements are distinct (to match classical formulas)
    """
    elems = res.elements
    n = len(elems)

    # 4) Distinctness
    assert len(set(elems)) == n, "Input elements should be distinct."

    # 1) Power set count and binomial identity
    total_subsets = len(res.subsets)
    assert total_subsets == 2**n, f"Power set size mismatch: {total_subsets} != 2^{n}"
    sum_binoms = sum(math.comb(n, k) for k in range(0, n + 1))
    assert total_subsets == sum_binoms, "Sum_k C(n,k) should equal 2^n."

    # 2) Per-k counts
    for k in range(1, n + 1):
        assert len(res.combinations[k]) == math.comb(n, k), f"C({n},{k}) count mismatch."
        assert len(res.permutations[k]) == math.perm(n, k), f"P({n},{k}) count mismatch."
        # sanity: each k-combination has length k, each k-permutation has distinct elements
        assert all(len(t) == k for t in res.combinations[k])
        assert all(len(t) == k and len(set(t)) == k for t in res.permutations[k])

    # 3) Combinations with repetition count
    k = res.cwr_k
    expected_cwr = math.comb(n + k - 1, k)
    assert len(res.cwr_list) == expected_cwr, f"CWR count mismatch: {len(res.cwr_list)} != C({n+k-1},{k})"


# ─────────────────────────────────────────────────────────────
# Main — run with your default elements (A..E)
# ─────────────────────────────────────────────────────────────

def main():
    # Same default sample as the original script
    sample_elements = ['A', 'B', 'C', 'D', 'E']
    res = advanced_combinatorics(sample_elements, cwr_k=2)

    # ----- ARC output -----
    print_answer(res)
    print_reason(res)

    print("Check (harness)")
    print("---------------")
    try:
        check_harness(res)
        print("OK: counts, formulas, and listings are mutually consistent.")
    except AssertionError as e:
        print("FAILED:", e)
        raise

if __name__ == "__main__":
    main()

