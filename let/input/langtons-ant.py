#!/usr/bin/env python3
"""
Langton's Ant
=============

A tiny terminal animation of the classic two-state Turing machine.
Watch how simple rules generate a highway after ~10 000 steps.

Controls
--------
• Pass a step count on the command line to change run length.
• Ctrl-C to quit early.
"""

from __future__ import annotations
import sys, time
from typing import Dict, Tuple

# cell states
WHITE, BLACK = 0, 1

# cardinal directions          N      E      S      W
DIRS: Tuple[Tuple[int, int], ...] = ((0, -1), (1, 0), (0, 1), (-1, 0))


class Ant:
    """Keeps position and heading (index into DIRS)."""

    def __init__(self, x: int, y: int, heading: int = 0):
        self.x, self.y, self.h = x, y, heading

    def turn_right(self): self.h = (self.h + 1) % 4
    def turn_left(self):  self.h = (self.h - 1) % 4
    def step(self):
        dx, dy = DIRS[self.h]
        self.x += dx; self.y += dy


class Grid:
    """Sparse infinite grid: dict maps (x, y) → state (0/1)."""

    def __init__(self): self.cells: Dict[Tuple[int, int], int] = {}

    def color(self, x: int, y: int) -> int:
        return self.cells.get((x, y), WHITE)

    def flip(self, x: int, y: int):
        self.cells[(x, y)] = BLACK if self.color(x, y) == WHITE else WHITE


# ─────────────────────────── rendering ───────────────────────────────────── #

def render(grid: Grid, ant: Ant, w: int = 41, h: int = 21) -> str:
    """Return an ASCII snapshot centred on the ant."""
    hw, hh = w // 2, h // 2
    out = []
    for y in range(-hh, hh + 1):
        row = []
        for x in range(-hw, hw + 1):
            if (x, y) == (ant.x, ant.y):
                row.append('A')                      # the ant
            else:
                row.append('#' if grid.color(x, y) else '.')
        out.append(''.join(row))
    return '\n'.join(out)


# ───────────────────────── simulation loop ───────────────────────────────── #

def simulate(steps: int = 11_000, delay: float = 0.02, refresh: int = 100):
    grid, ant = Grid(), Ant(0, 0, 0)
    for step in range(steps):
        if grid.color(ant.x, ant.y) == WHITE:        # Rule 1: on white
            grid.flip(ant.x, ant.y)
            ant.turn_right()
        else:                                        # Rule 2: on black
            grid.flip(ant.x, ant.y)
            ant.turn_left()
        ant.step()                                   # Rule 3: move fwd

        if step % refresh == 0:                      # redraw every N steps
            print("\x1b[H\x1b[J", end='')            # clear ANSI screen
            print(f"Langton's Ant  –  step {step:,}")
            print(render(grid, ant))
            time.sleep(delay)


if __name__ == "__main__":
    # Optional CLI argument: number of steps
    total_steps = int(sys.argv[1]) if len(sys.argv) > 1 else 11_000
    try:
        simulate(total_steps)
    except KeyboardInterrupt:
        print("\nSimulation interrupted by user.")
