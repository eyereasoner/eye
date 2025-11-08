#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
maze.py — Deterministic “random-looking” maze via randomized Prim (ARC-ified)
────────────────────────────────────────────────────────────────────────────────

Description
-----------
Generates a *perfect* maze (exactly one simple path between any two open cells)
using **randomized Prim’s algorithm** driven by a fixed RNG seed. With a fixed
seed and given width/height, the maze is identical across runs, yet *looks*
richly tangled.

This script prints ARC-style sections:

• Answer
  - Renders the maze (ASCII), reports dimensions and core stats.

• Reason why
  - Explains the algorithm and shows a compact build trace of the first steps
    (frontier expansion events: from cell → knock wall → new cell).

• Check (harness)
  - Verifies: odd dimensions, exactly two boundary openings (entry/exit),
    reproducibility (same seed ⇒ same maze), connectivity, and *perfection*
    (open-cell graph is a single tree: edges = nodes − 1, connected).

Usage
-----
  python maze.py [width] [height] [path_char] [wall_char]

Defaults: width=41 (odd), height=21 (odd), path_char=' ', wall_char='#'.
"""

from __future__ import annotations
from dataclasses import dataclass
from typing import List, Tuple, Iterable, Optional
import random, sys

Cell      = Tuple[int, int]        # (x, y) indexes into grid[y][x]
Direction = Tuple[int, int]        # 2-cell step (dx, dy)
DIRS: List[Direction] = [(2, 0), (-2, 0), (0, 2), (0, -2)]   # E, W, S, N

# ─────────────────────────────────────────────────────────────
# Maze generation (randomized Prim with fixed seed)
# ─────────────────────────────────────────────────────────────
@dataclass
class CarveEvent:
    from_cell: Cell
    through_wall: Cell
    to_cell: Cell

def carve_maze(width: int,
               height: int,
               rng_seed: int = 0,
               record: Optional[List[CarveEvent]] = None) -> List[List[int]]:
    """
    Returns a 2-D grid of 0=wall / 1=passage.
    Odd width/height required. Deterministic given rng_seed.
    If `record` is provided, carve events are appended for tracing.
    """
    if width % 2 == 0 or height % 2 == 0:
        raise ValueError("Width and height must be odd.")

    rng = random.Random(rng_seed)

    # Initialize all walls
    grid = [[0] * width for _ in range(height)]
    def open_cell(x: int, y: int) -> None: grid[y][x] = 1

    # Start on an ODD–ODD cell so ±2 steps cover all odd lattice cells.
    # This ensures the interior ring (x=1, y=1, x=W-2, y=H-2) gets reached.
    x0, y0 = 1, 1
    open_cell(x0, y0)
    frontier: List[Cell] = [(x0, y0)]

    while frontier:
        x, y = frontier.pop(rng.randrange(len(frontier)))
        dirs = DIRS[:]; rng.shuffle(dirs)
        for dx, dy in dirs:
            nx, ny = x + dx, y + dy            # target cell (two away)
            wx, wy = x + dx // 2, y + dy // 2  # wall between
            if 1 <= nx < width - 1 and 1 <= ny < height - 1 and grid[ny][nx] == 0:
                # new cell must have exactly one opened neighbor (Prim condition)
                neighbours = 0
                for dx2, dy2 in DIRS:
                    xx, yy = nx + dx2, ny + dy2
                    if 0 <= xx < width and 0 <= yy < height:
                        neighbours += grid[yy][xx]
                if neighbours == 1:
                    open_cell(wx, wy)
                    open_cell(nx, ny)
                    frontier.append((nx, ny))
                    if record is not None:
                        record.append(CarveEvent((x, y), (wx, wy), (nx, ny)))

    # ---- Robust, deterministic door selection ----
    W, H = width, height
    candidates: List[Tuple[str, Cell]] = []

    # Left border candidates (touch interior x=1)
    for y in range(1, H-1):
        if grid[y][1] == 1:
            candidates.append(("L", (0, y)))
    # Top border candidates (touch interior y=1)
    for x in range(1, W-1):
        if grid[1][x] == 1:
            candidates.append(("T", (x, 0)))
    # Right border candidates (touch interior x=W-2)
    for y in range(1, H-1):
        if grid[y][W-2] == 1:
            candidates.append(("R", (W-1, y)))
    # Bottom border candidates (touch interior y=H-2)
    for x in range(1, W-1):
        if grid[H-2][x] == 1:
            candidates.append(("B", (x, H-1)))

    # With odd–odd start, candidates will be non-empty for practical sizes.
    if not candidates:
        # Extremely defensive fallback (should not happen now)
        entry, exit_ = (0, 1), (W-1, H-2)
    else:
        entry = candidates[0][1]
        # farthest candidate (Manhattan) as exit, different from entry
        def md(p: Cell, q: Cell) -> int: return abs(p[0]-q[0]) + abs(p[1]-q[1])
        exit_list = [pos for _side, pos in candidates if pos != entry]
        if exit_list:
            exit_ = max(exit_list, key=lambda p: md(p, entry))
        else:
            # Only one candidate: synthesize an opposite-side partner
            ex, ey = entry
            if ex == 0:  # left → find right
                y2 = next((y for y in range(H-2, 0, -1) if grid[y][W-2] == 1), ey)
                exit_ = (W-1, y2)
            elif ex == W-1:  # right → find left
                y2 = next((y for y in range(H-2, 0, -1) if grid[y][1] == 1), ey)
                exit_ = (0, y2)
            elif ey == 0:  # top → find bottom
                x2 = next((x for x in range(W-2, 0, -1) if grid[H-2][x] == 1), ex)
                exit_ = (x2, H-1)
            else:  # bottom → find top
                x2 = next((x for x in range(W-2, 0, -1) if grid[1][x] == 1), ex)
                exit_ = (x2, 0)

    # Carve the two doors
    grid[entry[1]][entry[0]] = 1
    grid[exit_[1]][exit_[0]] = 1

    return grid

# ─────────────────────────────────────────────────────────────
# Rendering
# ─────────────────────────────────────────────────────────────
def render(grid: List[List[int]], path_char: str = ' ', wall_char: str = '#') -> str:
    return "\n".join("".join(path_char if cell else wall_char for cell in row) for row in grid)

# ─────────────────────────────────────────────────────────────
# Graph helpers (for Check)
# ─────────────────────────────────────────────────────────────
def open_cells(grid: List[List[int]]) -> List[Cell]:
    return [(x, y) for y in range(len(grid)) for x in range(len(grid[0])) if grid[y][x] == 1]

def open_neighbours(grid: List[List[int]], x: int, y: int) -> Iterable[Cell]:
    W, H = len(grid[0]), len(grid)
    for dx, dy in ((1,0),(-1,0),(0,1),(0,-1)):
        xx, yy = x+dx, y+dy
        if 0 <= xx < W and 0 <= yy < H and grid[yy][xx] == 1:
            yield (xx, yy)

def count_edges(grid: List[List[int]]) -> int:
    deg2 = sum(sum(1 for _ in open_neighbours(grid, x, y)) for x, y in open_cells(grid))
    return deg2 // 2

def boundary_openings(grid: List[List[int]]) -> List[Cell]:
    W, H = len(grid[0]), len(grid)
    out = []
    for x in range(W):
        if grid[0][x] == 1: out.append((x,0))
        if grid[H-1][x] == 1: out.append((x,H-1))
    for y in range(H):
        if grid[y][0] == 1: out.append((0,y))
        if grid[y][W-1] == 1: out.append((W-1,y))
    # dedup
    uniq = []
    for c in out:
        if c not in uniq: uniq.append(c)
    return uniq

def bfs_component(grid: List[List[int]], start: Cell) -> int:
    from collections import deque
    seen = {start}
    q = deque([start])
    while q:
        x, y = q.popleft()
        for nb in open_neighbours(grid, x, y):
            if nb not in seen:
                seen.add(nb); q.append(nb)
    return len(seen)

# ─────────────────────────────────────────────────────────────
# ARC sections
# ─────────────────────────────────────────────────────────────
def arc_answer(grid: List[List[int]], trace: List[CarveEvent], path_char: str, wall_char: str) -> None:
    W, H = len(grid[0]), len(grid)
    print("Answer")
    print("------")
    print(render(grid, path_char, wall_char))
    print("\nStats:")
    print(f"  size = {W}×{H} (odd)")
    print(f"  open cells = {len(open_cells(grid))}")
    print(f"  edges between open cells = {count_edges(grid)}")

def arc_reason(trace: List[CarveEvent], max_events: int = 40) -> None:
    print("Reason why")
    print("----------")
    print("Randomized Prim with fixed seed. We grow a spanning tree of interior\n"
          "cells by repeatedly picking a frontier cell uniformly at random and\n"
          "opening a neighbor that has exactly one opened neighbor, knocking down\n"
          "the separating wall. Doors are then cut on borders that already touch\n"
          "interior passages, guaranteeing connectivity (and determinism).\n")
    if trace:
        print(f"First {min(max_events, len(trace))} carve events:")
        for i, ev in enumerate(trace[:max_events], 1):
            print(f"  {i:>3}. from {ev.from_cell}  →  knock {ev.through_wall}  →  open {ev.to_cell}")
        if len(trace) > max_events:
            print(f"  ... ({len(trace)-max_events} more)\n")
        else:
            print()
    else:
        print("(no events recorded)\n")

def arc_check(grid: List[List[int]], width: int, height: int, rng_seed: int) -> None:
    print("Check (harness)")
    print("---------------")
    W, H = len(grid[0]), len(grid)
    # (1) odd dims
    assert W % 2 == 1 and H % 2 == 1, "Dimensions must be odd."
    # (2) exactly two boundary openings, each adjacent to an interior passage
    openings = boundary_openings(grid)
    assert len(openings) == 2, f"Expected exactly 2 boundary openings, got {openings}"
    for (x, y) in openings:
        if   x == 0:        assert grid[y][1] == 1
        elif x == W-1:      assert grid[y][W-2] == 1
        elif y == 0:        assert grid[1][x] == 1
        elif y == H-1:      assert grid[H-2][x] == 1
    # (3) reproducibility
    g2 = carve_maze(width, height, rng_seed=rng_seed)
    assert g2 == grid, "Same seed must reproduce the same maze."
    # (4) connectivity & acyclicity (tree)
    nodes = len(open_cells(grid))
    edges = count_edges(grid)
    comp = bfs_component(grid, openings[0])   # BFS from one opening
    assert comp == nodes, "Open cells are not all connected."
    assert edges == nodes - 1, f"Not a tree: edges={edges}, nodes={nodes}"
    print("OK: odd dims, two valid doors, deterministic, connected & acyclic.\n")

# ─────────────────────────────────────────────────────────────
# CLI entry
# ─────────────────────────────────────────────────────────────
def main(argv):
    width  = int(argv[1]) if len(argv) > 1 else 41
    height = int(argv[2]) if len(argv) > 2 else 21
    path_ch = (argv[3] if len(argv) > 3 else ' ')[:1]
    wall_ch = (argv[4] if len(argv) > 4 else '#')[:1]
    seed = 0

    trace: List[CarveEvent] = []
    grid = carve_maze(width, height, rng_seed=seed, record=trace)

    arc_answer(grid, trace, path_ch, wall_ch)
    arc_reason(trace)
    arc_check(grid, width, height, seed)

if __name__ == "__main__":
    main(sys.argv)

