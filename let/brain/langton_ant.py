#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Langton's Ant — ARC (Answer / Reason / Check), self-contained

Usage
  python langton_ant_arc.py [steps] [refresh]
    • steps   – total number of simulation steps (default 11000)
    • refresh – how often to print a snapshot (default 100)

Rendering
  Plain ASCII, centered on the ant. Cells:
    '.' = white, 'o' = black, 'A' = ant (at current cell)

ARC layout
  • Answer — runs the simulation and prints periodic snapshots.
  • Reason why — explains the two turning rules.
  • Check (harness) — proves small invariants, including exact
    step-by-step reversibility (forward then reverse returns to the
    precise initial state), and parity of black-cell count.
"""

from __future__ import annotations
import sys
from dataclasses import dataclass
from typing import Dict, Tuple, List

# ──────────────────────────── Model ────────────────────────────
WHITE, BLACK = 0, 1
DIRS = ((0, -1), (1, 0), (0, 1), (-1, 0))  # N, E, S, W
Point = Tuple[int, int]

@dataclass
class Ant:
    x: int = 0
    y: int = 0
    h: int = 0  # heading index into DIRS (0=N)

    def turn_right(self) -> None:
        self.h = (self.h + 1) % 4

    def turn_left(self) -> None:
        self.h = (self.h - 1) % 4

    def step(self) -> None:
        dx, dy = DIRS[self.h]
        self.x += dx
        self.y += dy

    def backstep(self) -> None:
        dx, dy = DIRS[self.h]
        self.x -= dx
        self.y -= dy

    def copy(self) -> "Ant":
        return Ant(self.x, self.y, self.h)

class Grid:
    def __init__(self):
        self._cells: Dict[Point, int] = {}

    def color(self, p: Point) -> int:
        return self._cells.get(p, WHITE)

    def flip(self, p: Point) -> None:
        self._cells[p] = BLACK if self.color(p) == WHITE else WHITE

    def black_count(self) -> int:
        return sum(1 for v in self._cells.values() if v == BLACK)

    def copy(self) -> "Grid":
        g = Grid()
        g._cells = dict(self._cells)
        return g

# ───────────────────────── Rendering ──────────────────────────
def render(grid: Grid, ant: Ant, w: int = 49, h: int = 25) -> str:
    """Return an ASCII snapshot centred on the ant."""
    hw, hh = w // 2, h // 2
    rows: List[str] = []
    for dy in range(-hh, hh + 1):
        y = ant.y + dy
        row: List[str] = []
        for dx in range(-hw, hw + 1):
            x = ant.x + dx
            if (x, y) == (ant.x, ant.y):
                row.append('A')
            else:
                row.append('o' if grid.color((x, y)) else '.')
        rows.append(''.join(row))
    return '\n'.join(rows)

# ────────────────────── Dynamics (forward & reverse) ──────────────────────
def step_forward(grid: Grid, ant: Ant) -> None:
    """One Langton step: flip, turn (R on white, L on black), move forward."""
    pos = (ant.x, ant.y)
    if grid.color(pos) == WHITE:
        grid.flip(pos)       # WHITE→BLACK
        ant.turn_right()
    else:
        grid.flip(pos)       # BLACK→WHITE
        ant.turn_left()
    ant.step()

def step_reverse(grid: Grid, ant: Ant) -> None:
    """
    Exact inverse of step_forward.
    Given (grid_after, ant_after), recover (grid_before, ant_before).
      Forward: at p, flip(c), turn (R if c==WHITE else L) to h1, move to q.
      After: ant at q, heading h1; grid[p] is flipped.
      Reverse: p = q - DIRS[h1]; c' = grid[p] (after flip); c = 1 - c' (before flip).
               h0 = h1-1 if c==WHITE else h1+1 (undo the turn).
               Move ant back to p and unflip grid[p] to restore c.
    """
    # Undo move
    ant.backstep()
    p = (ant.x, ant.y)

    # Determine original colour before flip
    c_after = grid.color(p)
    c_before = WHITE if c_after == BLACK else BLACK

    # Undo turn
    if c_before == WHITE:      # forward turned right: h1 = h0 + 1 ⇒ h0 = h1 - 1
        ant.h = (ant.h - 1) % 4
    else:                      # forward turned left:  h1 = h0 - 1 ⇒ h0 = h1 + 1
        ant.h = (ant.h + 1) % 4

    # Undo flip
    grid.flip(p)

# ───────────────────────── Simulation loop ────────────────────────
def simulate(steps: int, refresh: int) -> None:
    grid, ant = Grid(), Ant()
    for step in range(steps + 1):
        if step % refresh == 0:
            print('-' * 49)
            print(f"Step {step:,}")
            print(render(grid, ant))
            print()
        if step == steps:
            break
        step_forward(grid, ant)

# ─────────────────────────────── ARC: Answer ───────────────────────────────
def print_answer(steps: int, refresh: int) -> None:
    print("Answer")
    print("======")
    simulate(steps, refresh)

# ───────────────────────────── ARC: Reason why ──────────────────────────────
def print_reason() -> None:
    print("\nReason why")
    print("==========")
    print("Langton’s Ant evolves on an infinite grid of black/white cells.")
    print("At each step the ant:")
    print("  1) Looks at the colour of its current cell.")
    print("  2) Turns 90° right on white, 90° left on black.")
    print("  3) Flips the colour of the current cell.")
    print("  4) Moves forward one cell.")
    print("These two simple rules yield chaotic transients followed by a")
    print("‘highway’—a repeating pattern that drifts off to infinity.")

# ─────────────────────────── ARC: Check (harness) ───────────────────────────
def print_check() -> None:
    print("\nCheck (harness)")
    print("===============")
    ok_all = True

    # A) One-step invertibility: forward then reverse is identity
    g0, a0 = Grid(), Ant()
    g1, a1 = g0.copy(), a0.copy()
    step_forward(g1, a1)
    step_reverse(g1, a1)
    ok_one = (g1._cells == g0._cells and (a1.x, a1.y, a1.h) == (a0.x, a0.y, a0.h))
    print(f"Forward then reverse (1 step) recovers exact state? {ok_one}")
    ok_all &= ok_one

    # B) Multi-step reversibility
    g2, a2 = Grid(), Ant()
    steps = 2000
    # forward
    history_black_parity: List[int] = []
    for t in range(steps):
        step_forward(g2, a2)
        history_black_parity.append(g2.black_count() & 1)
    # reverse
    for _ in range(steps):
        step_reverse(g2, a2)
    ok_multi = (g2._cells == {} and (a2.x, a2.y, a2.h) == (0, 0, 0))
    print(f"Forward {steps} then reverse {steps} returns to initial state? {ok_multi}")
    ok_all &= ok_multi

    # C) Parity invariant: black-count flips parity every step from all-white start
    ok_parity = all(par == ((t + 1) & 1) for t, par in enumerate(history_black_parity))
    print(f"Parity of black-cell count matches step index (from 0)? {ok_parity}")
    ok_all &= ok_parity

    # D) Determinism: two independent runs with same number of steps match exactly
    def run_n(n: int):
        g, a = Grid(), Ant()
        for _ in range(n):
            step_forward(g, a)
        return g._cells, (a.x, a.y, a.h)
    sA = run_n(5000)
    sB = run_n(5000)
    ok_det = (sA == sB)
    print(f"Deterministic evolution (same steps ⇒ same state)? {ok_det}")
    ok_all &= ok_det

    print(f"\nAll checks passed? {ok_all}")

# ────────────────────────────────── Main ──────────────────────────────────
if __name__ == "__main__":
    total_steps = int(sys.argv[1]) if len(sys.argv) > 1 else 11_000
    refresh_every = int(sys.argv[2]) if len(sys.argv) > 2 else 100
    print_answer(total_steps, refresh_every)
    print_reason()
    print_check()

