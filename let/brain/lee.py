#!/usr/bin/env python3
"""
Lee routing (grid shortest path via BFS)

Based on your original lee.py (same constants, same random-grid generation),
now with:
  • Extensive comments that walk through the algorithm step-by-step
  • A structured result (LeeResult) so we can inspect distances/visited/trace
  • ARC-style console output sections: Answer / Reason why / Check (harness)

Key properties (unchanged algorithmically):
  - 4-neighborhood (up, right, down, left)
  - Obstacles are cells with value -1
  - BFS guarantees the found path (if any) is shortest in number of steps
  - We reconstruct the path via a prev[] back-pointer tree

Legend in the printed grid:
  '.' = obstacle
  ' ' = free cell
  '+' = shortest path
  'S' = source
  'D' = destination
"""

from collections import deque
from dataclasses import dataclass
import random
from typing import List, Optional, Tuple

# --- constants ---------------------------------------------------------------
ROW = 25
COL = 49
OBSTACLE_PROB = 30  # % chance that a cell is an obstacle

# directions: up, right, down, left (row/col deltas)
D_ROW = (-1, 0, 1, 0)
D_COL = (0, 1, 0, -1)


# --- types & helpers ---------------------------------------------------------

@dataclass(frozen=True)
class Point:
    row: int
    col: int

Grid = List[List[int]]                 # -1 obstacle; 0 free; (we'll mark path 2, start 3, dest 4)
Visited = List[List[bool]]
Dist = List[List[int]]
Prev = List[List[Optional[Point]]]

def in_bounds(r: int, c: int) -> bool:
    """True if (r,c) is inside the ROW×COL board."""
    return 0 <= r < ROW and 0 <= c < COL

def is_valid(grid: Grid, visited: Visited, r: int, c: int) -> bool:
    """
    Return True if (r,c) is a legal next cell:
      - inside the board
      - walkable (grid[r][c] == 0)
      - not visited yet
    """
    return in_bounds(r, c) and grid[r][c] == 0 and not visited[r][c]

def is_neighbor(a: Point, b: Point) -> bool:
    """4-neighbor (Manhattan distance = 1)."""
    return abs(a.row - b.row) + abs(a.col - b.col) == 1


# --- results container -------------------------------------------------------

@dataclass
class LeeResult:
    success: bool                    # True if a path exists
    path: List[Point]                # forward order: [src, ..., dst] (empty if no path)
    grid: Grid                       # grid with path marked (2), start (3), dest (4)
    dist: Dist                       # distance field (steps from src); -1 for unreachable
    visited: Visited                 # visited mask
    prev: Prev                       # backpointers
    expansions: int                  # how many cells were popped from the queue


# --- core algorithm (Lee BFS) -----------------------------------------------

def lee(grid: Grid, src: Point, dst: Point) -> LeeResult:
    """
    Classic Lee algorithm (breadth-first search on a grid) with backpointers.

    Returns a LeeResult with:
      - success flag
      - reconstructed *shortest* path (if any)
      - distance & visited arrays for reasoning / verification
      - the grid annotated with the path and endpoints

    Complexity:
      Time  O(ROW*COL) in the worst case
      Space O(ROW*COL) for dist/visited/prev
    """
    visited: Visited = [[False] * COL for _ in range(ROW)]
    dist: Dist = [[-1] * COL for _ in range(ROW)]
    prev: Prev = [[None] * COL for _ in range(ROW)]
    q: deque[Point] = deque()

    # Initialize BFS frontier at the source
    q.append(src)
    visited[src.row][src.col] = True
    dist[src.row][src.col] = 0
    prev[src.row][src.col] = Point(-1, -1)  # sentinel to mark the root
    expansions = 0

    # --- BFS wave expansion -------------------------------------------------
    #
    # We pop in FIFO order, ensuring that *the first time* we reach any cell,
    # we do so via a path with minimal number of steps (shortest path property).
    #
    while q:
        p = q.popleft()
        expansions += 1
        if p == dst:
            break  # we can stop early: BFS reached destination with minimal steps

        # Try 4-neighbors (up, right, down, left)
        for dr, dc in zip(D_ROW, D_COL):
            nr, nc = p.row + dr, p.col + dc
            if is_valid(grid, visited, nr, nc):
                q.append(Point(nr, nc))
                visited[nr][nc] = True
                dist[nr][nc] = dist[p.row][p.col] + 1
                prev[nr][nc] = p

    # If BFS never visited dst, no path exists
    if not visited[dst.row][dst.col]:
        return LeeResult(
            success=False,
            path=[],
            grid=grid,
            dist=dist,
            visited=visited,
            prev=prev,
            expansions=expansions,
        )

    # --- Back-trace the shortest path --------------------------------------
    path_rev: List[Point] = []
    cur = dst
    while cur != src:
        path_rev.append(cur)
        cur = prev[cur.row][cur.col]
        # Defensive: if prev is missing, something went wrong
        if cur is None or cur.row < 0:
            # This shouldn't happen with the BFS above.
            return LeeResult(False, [], grid, dist, visited, prev, expansions)
    path_rev.append(src)
    path = list(reversed(path_rev))

    # Annotate the grid for pretty output (non-destructive vis: mark only the path)
    # Keep start & dest glyphs distinct to increase readability.
    for p in path[1:-1]:
        grid[p.row][p.col] = 2  # path
    grid[src.row][src.col] = 3  # start
    grid[dst.row][dst.col] = 4  # destination

    return LeeResult(
        success=True,
        path=path,
        grid=grid,
        dist=dist,
        visited=visited,
        prev=prev,
        expansions=expansions,
    )


# --- presentation helpers ----------------------------------------------------

def print_grid(grid: Grid) -> None:
    """Pretty-print the grid using readable glyphs."""
    lookup = {
        -1: '.',  # obstacle
         0: ' ',  # empty
         2: '+',  # path
         3: 'S',  # start
         4: 'D',  # destination
    }
    for row in grid:
        print("".join(lookup.get(cell, '?') for cell in row))


def reason_text(res: LeeResult, src: Point, dst: Point, max_rows: int = 8) -> str:
    """
    Build an explanation of why the path is shortest and what the BFS did.
    Shows a few rows of the distance field for intuition.
    """
    lines = []
    lines.append("We use breadth-first search (Lee) on the 4-connected grid.")
    lines.append("BFS explores in 'waves' of increasing distance. The first time we reach a cell,")
    lines.append("we have a shortest path to it. Thus, when we first visit the destination,")
    lines.append("its dist equals the global shortest path length (in steps).")
    lines.append("")
    if res.success:
        lines.append(f"Destination first reached at distance {res.dist[dst.row][dst.col]} steps.")
    lines.append(f"Total expansions (queue pops): {res.expansions}.")
    lines.append("")
    lines.append("Snippet of the distance field (−1 means unreachable/obstacle):")
    show_rows = min(ROW, max_rows)
    sample = ["  " + " ".join(f"{res.dist[r][c]:>2d}" for c in range(COL)) for r in range(show_rows)]
    lines.extend(sample)
    if ROW > show_rows:
        lines.append(f"  … {ROW - show_rows} more rows …")
    return "\n".join(lines)


# --- verification (ARC “Check” harness) -------------------------------------

def check_harness(res: LeeResult, src: Point, dst: Point) -> None:
    """
    Re-verify correctness and minimality:

    1) Each consecutive pair on the path are 4-neighbors.
    2) No path cell is an obstacle (grid value -1).
    3) The counted path length equals the BFS distance to dst.
    4) Re-run a clean BFS distance check to confirm minimality.
    """
    assert in_bounds(src.row, src.col) and in_bounds(dst.row, dst.col), "Endpoints out of bounds."

    if not res.success:
        # If no path exists, ensure BFS dist reflects that
        assert res.dist[dst.row][dst.col] == -1, "No path but dst has non-negative distance."
        return

    # 1) Neighbor steps & 2) No obstacles on the path
    for a, b in zip(res.path, res.path[1:]):
        assert is_neighbor(a, b), f"Non-neighbor step in path: {a} -> {b}"
        assert res.grid[b.row][b.col] in (2, 4), "Path steps must lie on free cells (or D)."

    # 3) Path length == recorded BFS distance
    path_len = len(res.path) - 1
    assert path_len == res.dist[dst.row][dst.col], (
        f"Path length {path_len} != BFS dist {res.dist[dst.row][dst.col]}"
    )

    # 4) Independent minimality check: run a fresh BFS on a clean grid
    #    (treat -1 as blocked; anything else as free)
    clean_dist = [[-1] * COL for _ in range(ROW)]
    dq = deque([src])
    clean_dist[src.row][src.col] = 0
    while dq:
        p = dq.popleft()
        if p == dst:
            break
        for dr, dc in zip(D_ROW, D_COL):
            nr, nc = p.row + dr, p.col + dc
            if in_bounds(nr, nc) and res.grid[nr][nc] != -1 and clean_dist[nr][nc] == -1:
                clean_dist[nr][nc] = clean_dist[p.row][p.col] + 1
                dq.append(Point(nr, nc))
    assert clean_dist[dst.row][dst.col] == path_len, "Independent BFS found a different shortest length."


# --- main --------------------------------------------------------------------

def main():
    # Reproducible obstacles so runs are stable across executions
    random.seed(0)

    # Generate a random grid with OBSTACLE_PROB% obstacles
    grid: Grid = [
        [-1 if random.randrange(100) < OBSTACLE_PROB else 0 for _ in range(COL)]
        for _ in range(ROW)
    ]

    src = Point(0, 0)
    dst = Point(ROW - 1, COL - 1)

    # Ensure endpoints are walkable
    grid[src.row][src.col] = 0
    grid[dst.row][dst.col] = 0

    # Run Lee algorithm
    res = lee(grid, src, dst)

    # ----- ARC output -----
    print("Answer")
    print("------")
    if res.success:
        print("Path found!")
        print(f"Source:      {src}")
        print(f"Destination: {dst}")
        print(f"Length:      {len(res.path) - 1} steps")
        print()
        print("Grid with path (S=source, D=dest, +=path, .=obstacle)")
        print("-----------------------------------------------------")
        print_grid(res.grid)
    else:
        print("No path exists on this board.")
        print(f"Source:      {src}")
        print(f"Destination: {dst}")
        print()
        print("Grid (no path)")
        print("--------------")
        print_grid(res.grid)

    print()
    print("Reason why")
    print("----------")
    print(reason_text(res, src, dst, max_rows=8))
    print()

    print("Check (harness)")
    print("---------------")
    try:
        check_harness(res, src, dst)
        if res.success:
            print("OK: path steps are valid neighbors, avoid obstacles, and the length is provably minimal.")
        else:
            print("OK: no path found, and BFS confirms destination is unreachable.")
    except AssertionError as e:
        print("FAILED:", e)
        raise

if __name__ == "__main__":
    main()

