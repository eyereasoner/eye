#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
hanoi.py — Towers of Hanoi (recursive) with ARC-style output
────────────────────────────────────────────────────────────────

Description
-----------
Generates the optimal move sequence for the classic Towers of Hanoi with `n`
disks and three pegs (source, auxiliary, target). Prints:

• Answer
  - The move list (truncated if long), final peg contents, and core stats.

• Reason why
  - Short proof sketch of optimality (T(n)=2·T(n−1)+1 ⇒ 2^n−1).
  - Useful invariants: only top disk moves; never place larger on smaller.
  - Per-disk move counts (disk k moves 2^(n−k) times).

• Check (harness)
  - Simulates the moves from the initial configuration and asserts:
      1) every step is legal,
      2) final state has all disks on target in correct order,
      3) move count equals 2^n−1 (optimality),
      4) per-disk move counts match theory.

Usage
-----
Edit `N`, `SRC`, `AUX`, `TGT` at the bottom or run as-is.
"""

from __future__ import annotations
from dataclasses import dataclass
from typing import List, Tuple, Dict

Move = Tuple[int, str, str]   # (disk, from_peg, to_peg)


# ─────────────────────────────────────────────────────────────
# Solver
# ─────────────────────────────────────────────────────────────
def hanoi_moves(n: int, src: str, aux: str, tgt: str) -> List[Move]:
    """Return optimal move list for n disks from src to tgt using aux."""
    if n <= 0:
        return []
    moves: List[Move] = []

    def rec(k: int, a: str, b: str, c: str) -> None:
        if k == 1:
            moves.append((1, a, c))
            return
        rec(k - 1, a, c, b)
        moves.append((k, a, c))
        rec(k - 1, b, a, c)

    rec(n, src, aux, tgt)
    return moves


# ─────────────────────────────────────────────────────────────
# Pretty helpers
# ─────────────────────────────────────────────────────────────
def show_moves(moves: List[Move], max_head: int = 40, max_tail: int = 10) -> None:
    """Print the move list, truncated to first/last parts if long."""
    total = len(moves)
    if total <= max_head + max_tail:
        for i, (d, a, b) in enumerate(moves, 1):
            print(f"{i:>3}. move disk {d}: {a} → {b}")
        return

    for i, (d, a, b) in enumerate(moves[:max_head], 1):
        print(f"{i:>3}. move disk {d}: {a} → {b}")
    print("  ...")
    start = total - max_tail + 1
    for idx, (d, a, b) in enumerate(moves[-max_tail:], start):
        print(f"{idx:>3}. move disk {d}: {a} → {b}")


def peg_snapshot(pegs: Dict[str, List[int]]) -> str:
    def fmt(stack: List[int]) -> str:
        # bottom ... top; show as [n … 2 1] with 1 the top
        return "[" + " ".join(map(str, stack)) + "]"
    return " | ".join(f"{name}:{fmt(pegs[name])}" for name in sorted(pegs))


# ─────────────────────────────────────────────────────────────
# ARC: Answer / Reason / Check
# ─────────────────────────────────────────────────────────────
def arc_answer(n: int, src: str, aux: str, tgt: str, moves: List[Move]) -> None:
    print("Answer")
    print("------")
    print(f"Problem: {n} disks, {src}→{tgt} using {aux}")
    print(f"Total moves: {len(moves)}  (expected 2^{n}-1 = {2**n - 1})")
    print("\nMove list:")
    show_moves(moves)
    print()


def arc_reason(n: int, src: str, aux: str, tgt: str, moves: List[Move]) -> None:
    print("Reason why")
    print("----------")
    print("Recurrence & optimality:")
    print("  To move n disks A→C with helper B:")
    print("    1) move n−1 A→B,  2) move disk n A→C,  3) move n−1 B→C.")
    print("  This yields T(n) = 2·T(n−1) + 1 with T(1)=1 ⇒ T(n)=2^n−1 (minimal).\n")

    # Per-disk move counts
    counts: Dict[int, int] = {}
    for d, _, _ in moves:
        counts[d] = counts.get(d, 0) + 1

    print("Per-disk move counts (theory: disk k moves 2^(n−k) times):")
    for d in range(1, n + 1):
        theory = 2 ** (n - d)
        actual = counts.get(d, 0)
        ok = "✓" if theory == actual else "✗"
        print(f"  disk {d}: {actual} (theory {theory}) {ok}")
    print()


def arc_check(n: int, src: str, aux: str, tgt: str, moves: List[Move]) -> None:
    print("Check (harness)")
    print("---------------")
    # Initial state: src has [n, n-1, ..., 1] (1 is top), others empty
    pegs: Dict[str, List[int]] = {src: list(range(n, 0, -1)), aux: [], tgt: []}

    # Simulate moves, verifying legality at each step
    step = 0
    for d, a, b in moves:
        step += 1
        assert a in pegs and b in pegs, f"Unknown peg at step {step}."
        assert pegs[a], f"Source peg {a} empty at step {step}."
        top = pegs[a][-1]
        assert top == d, f"Wrong disk moved at step {step}: expected top {top}, got {d}."
        if pegs[b]:
            assert pegs[b][-1] > d, f"Illegal move at step {step}: larger-on-smaller violation."
        pegs[b].append(pegs[a].pop())

    # Final state assertions
    assert not pegs[src] and not pegs[aux], "Source/aux pegs should be empty at the end."
    assert pegs[tgt] == list(range(n, 0, -1)), "Target peg not in correct order."

    # Minimality
    assert len(moves) == 2 ** n - 1, "Move count is not minimal (should be 2^n−1)."

    # Per-disk counts
    counts: Dict[int, int] = {}
    for d, _, _ in moves:
        counts[d] = counts.get(d, 0) + 1
    for d in range(1, n + 1):
        assert counts.get(d, 0) == 2 ** (n - d), f"Disk {d} moved wrong number of times."

    print("OK. Final pegs:", peg_snapshot(pegs))
    print()


# ─────────────────────────────────────────────────────────────
# Demo
# ─────────────────────────────────────────────────────────────
if __name__ == "__main__":
    N   = 8
    SRC = "A"
    AUX = "B"
    TGT = "C"

    MOVES = hanoi_moves(N, SRC, AUX, TGT)

    arc_answer(N, SRC, AUX, TGT, MOVES)
    arc_reason(N, SRC, AUX, TGT, MOVES)
    arc_check(N, SRC, AUX, TGT, MOVES)

