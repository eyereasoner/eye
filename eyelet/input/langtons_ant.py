#!/usr/bin/env python3
"""
langtons_ant.py
===============

Plain-text Langton’s Ant.
Prints periodic snapshots instead of an ANSI live animation, making the
output readable in any log or terminal that supports only basic text.

Usage
-----
    python langtons_ant.py [steps] [refresh]

    • steps   – total number of simulation steps  (default 11000)
    • refresh – how often to print a snapshot     (default   100)
"""

from __future__ import annotations
import sys
from typing import Dict, Tuple

WHITE, BLACK = 0, 1
DIRS = ((0, -1), (1, 0), (0, 1), (-1, 0))      # N, E, S, W
Point = Tuple[int, int]


class Ant:
    def __init__(self, x: int = 0, y: int = 0, heading: int = 0):
        self.x, self.y, self.h = x, y, heading

    def turn_right(self): self.h = (self.h + 1) % 4
    def turn_left(self):  self.h = (self.h - 1) % 4
    def step(self):
        dx, dy = DIRS[self.h]
        self.x += dx
        self.y += dy


class Grid:
    def __init__(self):
        self._cells: Dict[Point, int] = {}

    def color(self, p: Point) -> int:
        return self._cells.get(p, WHITE)

    def flip(self, p: Point):
        self._cells[p] = BLACK if self.color(p) == WHITE else WHITE


# ───────────────────────── rendering ───────────────────────── #

def render(grid: Grid, ant: Ant, w: int = 41, h: int = 21) -> str:
    """Return an ASCII snapshot centred on the ant."""
    hw, hh = w // 2, h // 2
    rows = []
    for dy in range(-hh, hh + 1):
        y = ant.y + dy
        row = []
        for dx in range(-hw, hw + 1):
            x = ant.x + dx
            if (x, y) == (ant.x, ant.y):
                row.append('A')
            else:
                row.append('#' if grid.color((x, y)) else '.')
        rows.append(''.join(row))
    return '\n'.join(rows)


# ───────────────────── simulation loop ────────────────────── #

def simulate(steps: int, refresh: int):
    grid, ant = Grid(), Ant()
    for step in range(steps):
        pos = (ant.x, ant.y)

        # Langton rules
        if grid.color(pos) == WHITE:
            grid.flip(pos)
            ant.turn_right()
        else:
            grid.flip(pos)
            ant.turn_left()
        ant.step()

        # periodic snapshot
        if step % refresh == 0:
            print('-' * 60)
            print(f"Step {step:,}")
            print(render(grid, ant))
            print()                     # blank line after each frame


# ──────────────────────── CLI entry ───────────────────────── #

if __name__ == "__main__":
    total_steps = int(sys.argv[1]) if len(sys.argv) > 1 else 11_000
    refresh_every = int(sys.argv[2]) if len(sys.argv) > 2 else 100
    simulate(total_steps, refresh_every)
