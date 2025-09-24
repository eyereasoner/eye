#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
N-Queens — ARC (Answer / Reason / Check), self-contained.

Usage:
  python n_queens.py            # N=8
  python n_queens.py 10         # N=10
  python n_queens.py 12 --samples 2
"""

from __future__ import annotations
import argparse
from dataclasses import dataclass
from typing import List, Tuple

# ─────────────────────────── Solver (bitmask backtracking) ───────────────────────────

@dataclass
class SolveResult:
    n: int
    count: int
    samples: List[List[int]]  # each sample is a list of column indices per row (0-based)

def solve_n_queens(n: int, sample_limit: int = 3) -> SolveResult:
    """
    Return total solution count and up to `sample_limit` example solutions.

    Representation:
      - We place one queen per row.
      - `cols` tracks used columns.
      - `d1` tracks used main diagonals (r - c), implemented as a bitmask shifted left each row.
      - `d2` tracks used anti-diagonals (r + c), implemented as a bitmask shifted right each row.
      - For the current row, the available columns are:
            avail = ~(cols | d1 | d2) & mask
    """
    if n <= 0:
        return SolveResult(n, 0, [])

    mask = (1 << n) - 1
    count = 0
    samples: List[List[int]] = []
    placement: List[int] = [0] * n  # placement[row] = col

    def backtrack(row: int, cols: int, d1: int, d2: int) -> None:
        nonlocal count, samples
        if row == n:
            count += 1
            if len(samples) < sample_limit:
                samples.append(placement.copy())
            return
        avail = (~(cols | d1 | d2)) & mask
        while avail:
            bit = avail & -avail
            avail -= bit
            col = (bit.bit_length() - 1)
            placement[row] = col
            backtrack(row + 1, cols | bit, (d1 | bit) << 1 & mask, (d2 | bit) >> 1)
        # nothing to return

    backtrack(0, 0, 0, 0)
    return SolveResult(n, count, samples)

# ─────────────────────────── Pretty printing & validation ───────────────────────────

def board_from_placement(n: int, placement: List[int]) -> List[str]:
    rows: List[str] = []
    for r in range(n):
        c = placement[r]
        row = "." * c + "Q" + "." * (n - c - 1)
        rows.append(row)
    return rows

def is_valid_solution(n: int, placement: List[int]) -> bool:
    if len(placement) != n:
        return False
    cols_seen = set()
    d1_seen = set()  # r - c
    d2_seen = set()  # r + c
    for r, c in enumerate(placement):
        if not (0 <= c < n):
            return False
        if c in cols_seen:
            return False
        d1 = r - c
        d2 = r + c
        if d1 in d1_seen or d2 in d2_seen:
            return False
        cols_seen.add(c)
        d1_seen.add(d1)
        d2_seen.add(d2)
    return True

# ───────────────────────────────── ARC: Answer ─────────────────────────────────

def print_answer(n: int, res: SolveResult) -> None:
    print("Answer")
    print("======")
    print(f"N-Queens for N = {n}")
    print(f"Total solutions: {res.count}")

    if not res.samples:
        print("\n(no example solutions to display)")
        return

    k = len(res.samples)
    print(f"\nShowing {k} example solution{'s' if k > 1 else ''}:")
    for idx, placement in enumerate(res.samples, 1):
        print(f"\nSolution {idx}:")
        for line in board_from_placement(n, placement):
            print("  " + line)
        coords = ", ".join(f"(r{r+1}, c{c+1})" for r, c in enumerate(placement))
        print(f"  Coordinates: {coords}")

# ──────────────────────────────── ARC: Reason why ────────────────────────────────

def print_reason() -> None:
    print("\nReason why")
    print("==========")
    print("We place one queen per row and forbid conflicts via three sets:")
    print("  • columns used     (no two queens share a column),")
    print("  • main diagonals   (constant r−c),")
    print("  • anti-diagonals   (constant r+c).")
    print("Bitmasks track these constraints efficiently. For each row we compute")
    print("the bitmask of available columns and try each set bit recursively.")
    print("When we reach row N, we’ve built a valid board. This guarantees correctness;")
    print("bitmasks keep it fast enough to enumerate all solutions for moderate N.")

# ─────────────────────────────── ARC: Check (harness) ──────────────────────────────

def print_check() -> None:
    print("\nCheck (harness)")
    print("===============")
    ok_all = True

    # A) Validate that sample solutions are actually valid according to the constraints.
    sample = solve_n_queens(8, sample_limit=3)
    valid_samples = all(is_valid_solution(8, s) for s in sample.samples)
    print(f"Sample solutions for N=8 pass row/col/diagonal constraints? {valid_samples}")
    ok_all &= valid_samples

    # B) Known counts for small N (deterministic self-check)
    #    (These are standard, stable combinatorial values.)
    expected_counts = {
        1: 1,
        2: 0, 3: 0,
        4: 2, 5: 10, 6: 4, 7: 40, 8: 92, 9: 352, 10: 724,
    }
    counts_ok = True
    for n, want in expected_counts.items():
        got = solve_n_queens(n, sample_limit=0).count
        same = (got == want)
        print(f"N={n}: count = {got} (expected {want})  ->  {'OK' if same else 'MISMATCH'}")
        counts_ok &= same
        if not counts_ok:
            break
    ok_all &= counts_ok

    # C) Determinism / idempotence: re-solving N=8 gives same count and sample solutions
    a = solve_n_queens(8, sample_limit=2)
    b = solve_n_queens(8, sample_limit=2)
    deterministic = (a.count == b.count) and (len(a.samples) == len(b.samples))
    print(f"Deterministic across runs (N=8 count & sample count)? {deterministic}")
    ok_all &= deterministic

    print(f"\nAll checks passed? {ok_all}")

# ───────────────────────────────────── Main ─────────────────────────────────────

def main() -> None:
    ap = argparse.ArgumentParser(description="N-Queens with ARC output")
    ap.add_argument("N", nargs="?", type=int, default=8, help="board size (default: 8)")
    ap.add_argument("--samples", type=int, default=3, help="number of example solutions to print (default: 3)")
    args = ap.parse_args()

    res = solve_n_queens(args.N, sample_limit=max(0, args.samples))
    print_answer(args.N, res)
    print_reason()
    print_check()

if __name__ == "__main__":
    main()

