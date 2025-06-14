#!/usr/bin/env python3
"""
maze.py
=======

Generate and print a rectangular “perfect” maze using the classic
recursive-backtracker (depth-first search with backtracking).

Usage
-----
    python maze.py [width] [height] [path_char] [wall_char]

    • width, height  – *odd* integers so walls & passages align (defaults 41×21)
    • path_char      – glyph for corridors          (default ' ')
    • wall_char      – glyph for walls             (default '#')

The script writes the maze to stdout, with an entrance on the left edge
and an exit on the right.
"""

from __future__ import annotations
import random, sys
from typing import List, Tuple


# ───────────────────────────── maze carving ───────────────────────────── #

Cell      = Tuple[int, int]       # (col,row) in cell coordinates
Direction = Tuple[int, int]       # (dx,dy)  in cell coordinates
DIRS: List[Direction] = [(1, 0), (-1, 0), (0, 1), (0, -1)]   # E, W, S, N


def carve_maze(width: int, height: int) -> List[List[int]]:
    """
    Return a 2-D grid of 0=wall / 1=passage.  Dimensions must be odd.
    """
    if width % 2 == 0 or height % 2 == 0:
        raise ValueError("Width and height must be odd.")

    w_cells, h_cells = width // 2, height // 2           # cell lattice size
    grid = [[0] * width for _ in range(height)]          # 0 means wall
    visited = [[False] * w_cells for _ in range(h_cells)]

    def carve(cell: Cell):
        cx, cy = cell
        grid[2 * cy + 1][2 * cx + 1] = 1                 # knock out cell

    # depth-first backtracker
    stack: List[Cell] = [(0, 0)]
    visited[0][0] = True
    carve((0, 0))

    while stack:
        cx, cy = stack[-1]
        neighbours = [
            (nx, ny)
            for dx, dy in DIRS
            if 0 <= (nx := cx + dx) < w_cells
            and 0 <= (ny := cy + dy) < h_cells
            and not visited[ny][nx]
        ]
        if neighbours:
            nx, ny = random.choice(neighbours)
            # remove the wall between (cx,cy) and (nx,ny)
            grid[cy + ny + 1][cx + nx + 1] = 1
            carve((nx, ny))
            visited[ny][nx] = True
            stack.append((nx, ny))
        else:
            stack.pop()

    # carve entrance & exit
    grid[1][0] = grid[height - 2][width - 1] = 1
    return grid


# ───────────────────────────── renderer ───────────────────────────── #

def render(grid: List[List[int]], path_char: str, wall_char: str):
    for row in grid:
        print("".join(path_char if cell else wall_char for cell in row))


# ───────────────────────────── CLI entry ──────────────────────────── #

def main(argv):
    width  = int(argv[1]) if len(argv) > 1 else 41
    height = int(argv[2]) if len(argv) > 2 else 21
    path_ch = argv[3] if len(argv) > 3 else ' '
    wall_ch = argv[4] if len(argv) > 4 else '#'

    grid = carve_maze(width, height)
    render(grid, path_ch[0], wall_ch[0])


if __name__ == "__main__":
    main(sys.argv)
