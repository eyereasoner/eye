#!/usr/bin/env python3
from collections import deque
from dataclasses import dataclass
import random

# --- constants ---------------------------------------------------------------
ROW = 25
COL = 49
OBSTACLE_PROB = 30               # % chance that a cell is an obstacle

# directions: up, right, down, left
D_ROW = (-1, 0, 1, 0)
D_COL = (0, 1, 0, -1)

# --- helpers -----------------------------------------------------------------
@dataclass(frozen=True)
class Point:
    row: int
    col: int


def is_valid(grid, visited, r, c):
    """Return True if (r,c) is inside the board, walkable, and unvisited."""
    return (
        0 <= r < ROW
        and 0 <= c < COL
        and grid[r][c] == 0          # walkable cell
        and not visited[r][c]
    )


def lee(grid, src: Point, dst: Point):
    """Lee-algorithm BFS.
    Marks the shortest path in-place and returns True if a path exists."""
    visited = [[False] * COL for _ in range(ROW)]
    dist    = [[-1] * COL for _ in range(ROW)]
    prev    = [[None] * COL for _ in range(ROW)]

    q = deque()
    q.append(src)
    visited[src.row][src.col] = True
    dist[src.row][src.col] = 0
    prev[src.row][src.col] = Point(-1, -1)     # sentinel

    # --- BFS ------------------------------------------------------------------
    while q:
        p = q.popleft()
        if p == dst:
            break
        for dr, dc in zip(D_ROW, D_COL):
            nr, nc = p.row + dr, p.col + dc
            if is_valid(grid, visited, nr, nc):
                q.append(Point(nr, nc))
                visited[nr][nc] = True
                dist[nr][nc] = dist[p.row][p.col] + 1
                prev[nr][nc] = p

    # --- back-trace -----------------------------------------------------------
    if not visited[dst.row][dst.col]:
        return False                  # no path

    cur = dst
    while cur != src:
        grid[cur.row][cur.col] = 2    # mark path
        cur = prev[cur.row][cur.col]

    grid[src.row][src.col] = 3        # start
    grid[dst.row][dst.col] = 4        # destination
    return True


def print_grid(grid):
    """Pretty-print the grid using the same glyphs as the C version."""
    lookup = {
        -1: '.',    # obstacle
         0: ' ',    # empty
         2: '+',    # path
         3: 'S',    # start
         4: 'D'     # destination
    }
    for row in grid:
        print("".join(lookup.get(cell, '?') for cell in row))


# --- main --------------------------------------------------------------------
def main():
    random.seed(0)                    # reproducible obstacles

    # generate grid
    grid = [
        [
            -1 if random.randrange(100) < OBSTACLE_PROB else 0
            for _ in range(COL)
        ]
        for _ in range(ROW)
    ]

    src = Point(0, 0)
    dst = Point(ROW - 1, COL - 1)
    grid[src.row][src.col] = 0        # ensure walkable
    grid[dst.row][dst.col] = 0

    if lee(grid, src, dst):
        print("Path found:\n")
        print_grid(grid)
    else:
        print("No path exists")


if __name__ == "__main__":
    main()

