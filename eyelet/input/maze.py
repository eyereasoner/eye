#!/usr/bin/env python3
"""
maze.py
=====================

Deterministic but “complex” maze generator using **randomised Prim’s
algorithm** with a fixed RNG seed.

• Still a *perfect* maze (exactly one path between any two cells).
• The constant seed (0) guarantees identical output for the same
  width/height, yet the maze *looks* random and richly tangled.

Usage
-----
    python maze.py [width] [height] [path_char] [wall_char]

    width, height  – odd integers (defaults 41 × 21)
    path_char      – single-byte glyph for passages (default ' ')
    wall_char      – single-byte glyph for walls    (default '#')
"""

from __future__ import annotations
import random, sys
from typing import List, Tuple

# --------------------------------------------------------------------------- #
#   Constants                                                                 #
# --------------------------------------------------------------------------- #

Cell      = Tuple[int, int]        # (col, row) in cell coordinates
Direction = Tuple[int, int]        # 2-cell step (dx, dy)
DIRS: List[Direction] = [(2, 0), (-2, 0), (0, 2), (0, -2)]   # E, W, S, N

_rng = random.Random(0)            # <<<  deterministic RNG


# --------------------------------------------------------------------------- #
#   Prim-style maze construction                                              #
# --------------------------------------------------------------------------- #

def carve_maze(width: int, height: int) -> List[List[int]]:
    """
    Return a 2-D grid of 0 = wall / 1 = passage built with Prim’s algorithm.

    Width and height *must* be odd so that cells and walls align neatly.
    """
    if width % 2 == 0 or height % 2 == 0:
        raise ValueError("Width and height must be odd.")

    # initialise: everything is wall
    grid = [[0] * width for _ in range(height)]

    def carve(x: int, y: int):           # knock a cell open
        grid[y][x] = 1

    # start in the centre for symmetry (could choose (1,1) just as well)
    start_x, start_y = width // 2, height // 2
    carve(start_x, start_y)

    frontier: List[Cell] = [(start_x, start_y)]

    while frontier:
        # choose a random frontier cell (deterministic because RNG is fixed)
        idx = _rng.randrange(len(frontier))
        x, y = frontier.pop(idx)

        # look two steps away to find uncarved neighbours
        _rng.shuffle(DIRS)          # shuffle DIRS for extra variety (still deterministic)
        for dx, dy in DIRS:
            nx, ny = x + dx, y + dy
            wx, wy = x + dx // 2, y + dy // 2   # wall between (x,y) and (nx,ny)

            if 1 <= nx < width - 1 and 1 <= ny < height - 1 and grid[ny][nx] == 0:
                # exactly one of the adjacent cells must already be carved
                neighbours = sum(grid[ny + dy2][nx + dx2] for dx2, dy2 in
                                 [(2, 0), (-2, 0), (0, 2), (0, -2)])
                if neighbours == 1:
                    carve(wx, wy)   # knock down interior wall
                    carve(nx, ny)
                    frontier.append((nx, ny))

    # carve deterministic entrance and exit
    grid[1][0] = 1                   # entry
    grid[height - 2][width - 1] = 1  # exit
    return grid


# --------------------------------------------------------------------------- #
#   Rendering                                                                 #
# --------------------------------------------------------------------------- #

def render(grid: List[List[int]], path_char: str = ' ', wall_char: str = '#'):
    """Print the grid to stdout."""
    for row in grid:
        print("".join(path_char if cell else wall_char for cell in row))


# --------------------------------------------------------------------------- #
#   CLI entry                                                                 #
# --------------------------------------------------------------------------- #

def main(argv):
    width  = int(argv[1]) if len(argv) > 1 else 41
    height = int(argv[2]) if len(argv) > 2 else 21
    path_ch = argv[3] if len(argv) > 3 else ' '
    wall_ch = argv[4] if len(argv) > 4 else '#'

    grid = carve_maze(width, height)
    render(grid, path_ch[0], wall_ch[0])


if __name__ == "__main__":
    main(sys.argv)

