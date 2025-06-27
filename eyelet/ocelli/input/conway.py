#!/usr/bin/env python3
"""
Conway’s Game of Life — ASCII Edition (Python)
---------------------------------------------
Run with optional CLI args:

    python life.py [rows] [cols] [generations]

Defaults: rows=25, cols=49, generations=100
"""

import random
import sys
import time
from typing import List

ALIVE = "O"
DEAD  = "."

# --------------------------------------------------------------------------- #
def initialize(rows: int, cols: int) -> List[List[str]]:
    """Return a 2-D list seeded with 20 % live cells."""
    return [
        [ALIVE if random.randrange(5) == 0 else DEAD for _ in range(cols)]
        for _ in range(rows)
    ]


def neighbors(grid: List[List[str]], r: int, c: int) -> int:
    """Count the eight toroidal neighbours of cell (r,c)."""
    rows, cols = len(grid), len(grid[0])
    cnt = 0
    for dr in (-1, 0, 1):
        for dc in (-1, 0, 1):
            if dr == 0 and dc == 0:
                continue
            nr = (r + dr) % rows
            nc = (c + dc) % cols
            if grid[nr][nc] == ALIVE:
                cnt += 1
    return cnt


def step(grid: List[List[str]]) -> List[List[str]]:
    """Compute one generation and return the new grid (ping-pong buffer)."""
    rows, cols = len(grid), len(grid[0])
    nxt = [[DEAD] * cols for _ in range(rows)]
    for r in range(rows):
        for c in range(cols):
            n = neighbors(grid, r, c)
            if grid[r][c] == ALIVE:
                nxt[r][c] = ALIVE if n in (2, 3) else DEAD
            else:
                nxt[r][c] = ALIVE if n == 3 else DEAD
    return nxt


def display(grid: List[List[str]], generation: int) -> None:
    # ANSI clear-screen; comment out if unwanted
    # sys.stdout.write("\033[H\033[J")
    print(f"Generation {generation}")
    for row in grid:
        print("".join(row))
    sys.stdout.flush()


# --------------------------------------------------------------------------- #
def main(argv: List[str]) -> None:
    rows  = int(argv[1]) if len(argv) > 1 else 25
    cols  = int(argv[2]) if len(argv) > 2 else 49
    gens  = int(argv[3]) if len(argv) > 3 else 100

    random.seed(0)
    grid = initialize(rows, cols)

    for gen in range(gens):
        display(grid, gen)
        grid = step(grid)
        # time.sleep(0.10)   # 100 ms delay; uncomment if you like

if __name__ == "__main__":
    main(sys.argv)

