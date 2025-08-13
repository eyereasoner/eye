#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
sudoku.py — Simple Sudoku solver (DFS backtracking) with ARC output
───────────────────────────────────────────────────────────────────────

Description
-----------
A clean, self-contained 9×9 Sudoku solver that:
  • Solves a puzzle in place using depth-first search with backtracking.
  • Emits ARC-style sections:

    Answer
    ------
    - Prints the initial puzzle and the solved board.
    - Shows basic solver statistics (decisions, backtracks, placements/sec).

    Reason why
    ----------
    - Summarizes the algorithm and constraints enforced.
    - Prints a compact trace of the first N recursive steps:
        TRY r,c←n  → placing a number
        OK  r,c←n  → placement kept (going deeper)
        BT  r,c←n  → backtracked (undo placement)

    Check (harness)
    ---------------
    - Verifies the input is consistent (no duplicates ignoring zeros).
    - Verifies the final board satisfies all Sudoku rules.
    - Verifies all givens are preserved.
    - Optionally counts solutions (≤2) to detect uniqueness.

Board representation
--------------------
- A 9×9 Python list of lists (typing alias `Board`).
- Digits 1‒9 for filled cells; 0 for blanks.

API
---
solve(board, trace=None, stats=None) -> bool
print_board(board) -> None
"""

from __future__ import annotations
from dataclasses import dataclass
from typing import List, Tuple, Optional, Iterable
import copy
import time

Board = List[List[int]]

# ─────────────────────────────────────────────────────────────
# Pretty printing
# ─────────────────────────────────────────────────────────────

def print_board(board: Board) -> None:
    """Pretty-print a Sudoku board to the console (0 shown as '.')."""
    for i, row in enumerate(board):
        if i % 3 == 0 and i != 0:
            print("-" * 21)
        for j, val in enumerate(row):
            if j % 3 == 0 and j != 0:
                print("|", end=" ")
            print(val if val != 0 else ".", end=" ")
        print()

# ─────────────────────────────────────────────────────────────
# Core helpers
# ─────────────────────────────────────────────────────────────

def find_empty(board: Board) -> Optional[Tuple[int, int]]:
    """Return (row, col) of the next empty cell, or None if solved."""
    for r in range(9):
        for c in range(9):
            if board[r][c] == 0:
                return r, c
    return None

def is_valid(board: Board, num: int, pos: Tuple[int, int]) -> bool:
    """Check whether placing *num* at *pos* keeps the board valid."""
    r, c = pos
    # Row & column
    if any(board[r][i] == num for i in range(9) if i != c):
        return False
    if any(board[i][c] == num for i in range(9) if i != r):
        return False
    # 3×3 sub-grid
    br, bc = 3 * (r // 3), 3 * (c // 3)
    for i in range(br, br + 3):
        for j in range(bc, bc + 3):
            if (i, j) != pos and board[i][j] == num:
                return False
    return True

def is_consistent_puzzle(board: Board) -> bool:
    """Check the *given* puzzle has no contradictions (ignoring zeros)."""
    # Rows
    for r in range(9):
        seen = set()
        for c in range(9):
            v = board[r][c]
            if v == 0: continue
            if v in seen: return False
            seen.add(v)
    # Columns
    for c in range(9):
        seen = set()
        for r in range(9):
            v = board[r][c]
            if v == 0: continue
            if v in seen: return False
            seen.add(v)
    # Boxes
    for br in range(0, 9, 3):
        for bc in range(0, 9, 3):
            seen = set()
            for r in range(br, br+3):
                for c in range(bc, bc+3):
                    v = board[r][c]
                    if v == 0: continue
                    if v in seen: return False
                    seen.add(v)
    return True

def is_complete_and_valid(board: Board) -> bool:
    """Check the board is fully filled and each row/col/box has 1..9 exactly once."""
    # Rows
    for r in range(9):
        row = board[r]
        if set(row) != set(range(1, 10)): return False
    # Cols
    for c in range(9):
        col = [board[r][c] for r in range(9)]
        if set(col) != set(range(1, 10)): return False
    # Boxes
    for br in range(0, 9, 3):
        for bc in range(0, 9, 3):
            box = []
            for r in range(br, br+3):
                for c in range(bc, bc+3):
                    box.append(board[r][c])
            if set(box) != set(range(1, 10)): return False
    return True

# ─────────────────────────────────────────────────────────────
# Solver with trace & stats
# ─────────────────────────────────────────────────────────────

@dataclass
class Stats:
    decisions: int = 0    # count of "try a number" attempts
    backtracks: int = 0   # count of undo operations
    placements: int = 0   # count of successful placements kept

def solve(board: Board,
          trace: Optional[List[str]] = None,
          stats: Optional[Stats] = None) -> bool:
    """
    Solve *board* in place using DFS backtracking.
    - If `trace` is provided, append human-readable events.
    - If `stats` is provided, update counters.
    """
    spot = find_empty(board)
    if spot is None:
        return True
    r, c = spot

    for num in range(1, 10):
        if trace is not None:
            trace.append(f"TRY ({r+1},{c+1}) ← {num}")
        if stats is not None:
            stats.decisions += 1
        if is_valid(board, num, (r, c)):
            board[r][c] = num
            if trace is not None:
                trace.append(f"OK  ({r+1},{c+1}) ← {num}")
            if stats is not None:
                stats.placements += 1
            if solve(board, trace, stats):
                return True
            # backtrack
            board[r][c] = 0
            if trace is not None:
                trace.append(f"BT  ({r+1},{c+1}) ← {num}")
            if stats is not None:
                stats.backtracks += 1
        else:
            if trace is not None:
                trace.append(f"NO  ({r+1},{c+1}) ← {num}")
    return False

def count_solutions(board: Board, limit: int = 2) -> int:
    """Return the number of solutions up to `limit` (stops early if exceeded)."""
    b = copy.deepcopy(board)
    cnt = 0

    def dfs() -> bool:
        nonlocal cnt
        spot = find_empty(b)
        if spot is None:
            cnt += 1
            return cnt >= limit  # stop if we reached the cap
        r, c = spot
        for num in range(1, 10):
            if is_valid(b, num, (r, c)):
                b[r][c] = num
                if dfs():
                    return True
                b[r][c] = 0
        return False

    dfs()
    return cnt

# ─────────────────────────────────────────────────────────────
# ARC sections
# ─────────────────────────────────────────────────────────────

def arc_answer(puzzle: Board, solution: Board, stats: Stats) -> None:
    print("Answer")
    print("------")
    print("Puzzle:\n")
    print_board(puzzle)
    print("\nSolution:\n")
    print_board(solution)
    print("\nStats:")
    print(f"  decisions = {stats.decisions}")
    print(f"  placements = {stats.placements}")
    print(f"  backtracks = {stats.backtracks}")
    print()

def arc_reason(trace: List[str], max_lines: int = 120) -> None:
    print("Reason why")
    print("----------")
    print("Algorithm: recursive DFS with backtracking.\n"
          "Constraints: row/column/subgrid uniqueness; we try digits 1..9 in order.\n")
    print(f"Trace (first {max_lines} events):")
    for i, line in enumerate(trace[:max_lines], 1):
        print(f"{i:>3}. {line}")
    if len(trace) > max_lines:
        print(f"... ({len(trace) - max_lines} more events omitted)")
    print()

def arc_check(puzzle: Board, solution: Board) -> None:
    print("Check (harness)")
    print("---------------")
    # 1) Input consistency
    assert is_consistent_puzzle(puzzle), "Puzzle has contradictions in givens."
    # 2) Solution validity & completeness
    assert is_complete_and_valid(solution), "Solution is not a valid completed Sudoku."
    # 3) Givens preserved
    for r in range(9):
        for c in range(9):
            if puzzle[r][c] != 0:
                assert puzzle[r][c] == solution[r][c], f"Given at ({r+1},{c+1}) changed."
    # 4) (Optional) uniqueness check, report only
    sols = count_solutions(puzzle, limit=2)
    note = "unique" if sols == 1 else ("non-unique (≥2)" if sols >= 2 else "unsolved")
    print(f"Uniqueness: {note}")
    print("OK.\n")

# ─────────────────────────────────────────────────────────────
# Demo
# ─────────────────────────────────────────────────────────────

if __name__ == "__main__":
    example: Board = [
        [1, 0, 0, 8, 0, 4, 0, 0, 0],
        [0, 2, 0, 0, 0, 0, 4, 5, 6],
        [0, 0, 3, 2, 0, 5, 0, 0, 0],
        [0, 0, 0, 4, 0, 0, 8, 0, 5],
        [7, 8, 9, 0, 5, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 6, 2, 0, 3],
        [8, 0, 1, 0, 0, 0, 7, 0, 0],
        [0, 0, 0, 1, 2, 3, 0, 8, 0],
        [2, 0, 5, 0, 0, 0, 0, 0, 9],
    ]

    # Keep a pristine copy for the Answer section
    puzzle = copy.deepcopy(example)
    trace: List[str] = []
    stats = Stats()

    ok = solve(example, trace=trace, stats=stats)

    # ARC output
    arc_answer(puzzle, example if ok else puzzle, stats)
    arc_reason(trace, max_lines=120)
    arc_check(puzzle, example if ok else puzzle)

