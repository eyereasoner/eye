#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
takuzu.py — Find *all* solutions of a 6×6 Takuzu / Binairo puzzle (ARC-ified)
─────────────────────────────────────────────────────────────────────────────────

Description
-----------
Solves a 6×6 Takuzu puzzle under the classic rules:

  1) Each row has exactly three 0's and three 1's.
  2) Each column has exactly three 0's and three 1's.
  3) No "000" or "111" triples appear horizontally or vertically.
  4) Finished rows are pairwise unique; finished columns are pairwise unique.

Representation
--------------
- Grid is a list of 6 rows × 6 columns with values in {0,1} and EMPTY=-1 for blanks.

ARC output
----------
• Answer — prints the puzzle, all solutions (or first N if you cap), and stats.
• Reason why — explains the backtracking strategy + a compact trace to the first solution.
• Check — verifies every reported solution satisfies all Takuzu rules and preserves givens.

API
---
solve_all(grid, limit=None) -> List[Grid]    # all solutions (can stop early at 'limit')
"""

from __future__ import annotations
from dataclasses import dataclass
from typing import List, Optional, Tuple
import copy
import time

# ───────────────────────────── constants & types ─────────────────────────────
N      = 6
HALF   = N // 2       # = 3
EMPTY  = -1
Grid   = List[List[int]]

# ────────────────────────────── core solver ──────────────────────────────
def solve_all(grid: Grid, limit: Optional[int] = None) -> List[Grid]:
    """Return a list with *all* solutions. If `limit` is given, stop early."""
    solutions: List[Grid] = []

    def backtrack() -> None:
        if limit is not None and len(solutions) >= limit:
            return

        r, c = _find_empty(grid)
        if r is None:                         # fully filled → record a solution
            solutions.append([row[:] for row in grid])
            return

        for val in (0, 1):
            grid[r][c] = val
            if _is_locally_valid(grid, r, c):
                backtrack()
            grid[r][c] = EMPTY                # undo

    backtrack()
    return solutions

# ────────────────────────────── utilities ───────────────────────────────
def _find_empty(g: Grid) -> Tuple[Optional[int], Optional[int]]:
    for i in range(N):
        for j in range(N):
            if g[i][j] == EMPTY:
                return i, j
    return None, None

def _is_locally_valid(g: Grid, r: int, c: int) -> bool:
    return (
        _check_line(g[r])                                # row rules
        and _check_line([g[i][c] for i in range(N)])     # col rules
        and _unique_rows(g, r)
        and _unique_cols(g, c)
    )

def _check_line(line: List[int]) -> bool:
    zeros = line.count(0)
    ones  = line.count(1)

    if zeros > HALF or ones > HALF:
        return False
    if EMPTY not in line and zeros != HALF:
        return False

    # no three consecutive identical digits
    for i in range(N - 2):
        if line[i] != EMPTY and line[i] == line[i + 1] == line[i + 2]:
            return False
    return True

def _unique_rows(g: Grid, r: int) -> bool:
    row = g[r]
    if EMPTY in row:
        return True
    for i in range(N):
        if i != r and EMPTY not in g[i] and g[i] == row:
            return False
    return True

def _unique_cols(g: Grid, c: int) -> bool:
    col = [g[i][c] for i in range(N)]
    if EMPTY in col:
        return True
    for j in range(N):
        other = [g[i][j] for i in range(N)]
        if j != c and EMPTY not in other and other == col:
            return False
    return True

# ────────────────────────────── pretty print ─────────────────────────────
def show(grid: Grid) -> None:
    for r in range(N):
        for c in range(N):
            v = grid[r][c]
            print("." if v == EMPTY else v, end=" ")
        print()

# ────────────────────────────── tracing helpers ──────────────────────────
@dataclass
class TraceStats:
    decisions: int = 0
    backtracks: int = 0
    placements: int = 0

def solve_first_with_trace(grid: Grid) -> Tuple[Optional[Grid], List[str], TraceStats]:
    """
    Find the first solution with a compact textual trace.
    Returns (solution_or_None, trace_lines, stats).
    """
    g = copy.deepcopy(grid)
    tr: List[str] = []
    st = TraceStats()

    def bt() -> bool:
        spot = _find_empty(g)
        if spot[0] is None:
            return True
        r, c = spot
        for val in (0, 1):
            tr.append(f"TRY ({r+1},{c+1}) ← {val}")
            st.decisions += 1
            g[r][c] = val
            if _is_locally_valid(g, r, c):
                tr.append(f"OK  ({r+1},{c+1}) ← {val}")
                st.placements += 1
                if bt():
                    return True
            g[r][c] = EMPTY
            tr.append(f"BT  ({r+1},{c+1}) ← {val}")
            st.backtracks += 1
        return False

    ok = bt()
    return (g if ok else None), tr, st

# ────────────────────────────── validation (harness) ─────────────────────
def is_finally_valid(g: Grid, givens: Grid) -> bool:
    # 1) no empties
    if any(EMPTY in row for row in g):
        return False
    # 2) row/col counts & no triples
    for r in range(N):
        if not _check_line(g[r]):
            return False
        if g[r].count(0) != HALF:
            return False
    for c in range(N):
        col = [g[i][c] for i in range(N)]
        if not _check_line(col):
            return False
        if col.count(0) != HALF:
            return False
    # 3) uniqueness of rows/cols
    if len({tuple(row) for row in g}) != N:
        return False
    cols = [tuple(g[i][c] for i in range(N)) for c in range(N)]
    if len(set(cols)) != N:
        return False
    # 4) givens preserved
    for r in range(N):
        for c in range(N):
            if givens[r][c] != EMPTY and givens[r][c] != g[r][c]:
                return False
    return True

# ────────────────────────────── ARC sections ─────────────────────────────
def arc_answer(puzzle: Grid, solutions: List[Grid], st_first: TraceStats) -> None:
    print("Answer")
    print("------")
    print("Puzzle:\n")
    show(puzzle)
    print(f"\nSolutions found: {len(solutions)}")
    if solutions:
        print(f"First solution stats: decisions={st_first.decisions}, placements={st_first.placements}, backtracks={st_first.backtracks}")
    print()
    for idx, sol in enumerate(solutions, 1):
        print(f"Solution #{idx}")
        show(sol)
        print()

def arc_reason(trace: List[str], max_lines: int = 140) -> None:
    print("Reason why")
    print("----------")
    print("Strategy: depth-first search with backtracking.\n"
          "At each step we take the next empty cell (row-major), try 0 then 1, and\n"
          "prune immediately with the four Takuzu constraints (counts, triples,\n"
          "row/col uniqueness when complete). Below is a compact trace to the *first*\n"
          "solution (TRY/OK/BT events).")
    print(f"\nTrace (first {max_lines} events):")
    for i, line in enumerate(trace[:max_lines], 1):
        print(f"{i:>3}. {line}")
    if len(trace) > max_lines:
        print(f"... ({len(trace) - max_lines} more events omitted)")
    print()

def arc_check(puzzle: Grid, solutions: List[Grid]) -> None:
    print("Check (harness)")
    print("---------------")
    assert len(solutions) >= 1, "No solutions found."
    # Each reported solution must satisfy all rules and preserve givens
    for i, sol in enumerate(solutions, 1):
        assert is_finally_valid(sol, puzzle), f"Solution #{i} violates Takuzu rules or alters givens."
    # If there are multiple solutions, ensure they are distinct
    if len(solutions) > 1:
        tup_solutions = [tuple(tuple(row) for row in s) for s in solutions]
        assert len(set(tup_solutions)) == len(tup_solutions), "Duplicate solutions reported."
    print("OK: all solutions satisfy the rules, preserve givens, and are distinct.\n")

# ─────────────────────────────── demo ───────────────────────────────────
if __name__ == "__main__":
    puzzle: Grid = [
        [EMPTY, 0,     1,     EMPTY, EMPTY, EMPTY],
        [EMPTY, EMPTY, EMPTY, 0,     EMPTY, 1    ],
        [1,     EMPTY, EMPTY, EMPTY, 0,     EMPTY],
        [EMPTY, 1,     EMPTY, EMPTY, EMPTY, 0    ],
        [EMPTY, EMPTY, 0,     EMPTY, 1,     EMPTY],
        [0,     EMPTY, EMPTY, EMPTY, EMPTY, EMPTY],
    ]

    # Find ALL solutions (and time it)
    grid_all = copy.deepcopy(puzzle)
    t0 = time.perf_counter()
    solutions = solve_all(grid_all)  # or solve_all(grid_all, limit=2)

    # Also get a *trace* to the first solution
    first_solution, trace, stats = solve_first_with_trace(puzzle)

    arc_answer(puzzle, solutions, stats)
    arc_reason(trace)
    arc_check(puzzle, solutions)

