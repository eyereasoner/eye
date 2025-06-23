#!/usr/bin/env python3
"""
life.py
=======

A deterministic plain-text Game of Life.

Differences from the original dynamic version:
• The RNG is seeded with a constant (0).  
• Everything else (rules, toroidal edges, rendering) is unchanged.

Usage
-----
    python life.py [width] [height] [generations]
                   [density] [live_char] [dead_char]
                   [refresh]

    width, height   board size                    (80 × 24)
    generations     total generations to run      (200)
    density         initial live-cell fraction    (0.30)
    live_char       glyph for a live cell         ('#')
    dead_char       glyph for a dead cell         ('.')
    refresh         print every N generations     (10)
"""

from __future__ import annotations
import random, sys
from typing import List

# ────────────────────── deterministic seed ────────────────────── #
random.seed(0)                   # <— fixed seed makes the run repeatable

Board = List[List[int]]          # 0 = dead, 1 = live


def make_board(w: int, h: int, p: float) -> Board:
    """Return an h×w board seeded with live cells at probability p."""
    rnd = random.random
    return [[1 if rnd() < p else 0 for _ in range(w)] for _ in range(h)]


def count_neighbours(board: Board, x: int, y: int) -> int:
    h, w = len(board), len(board[0])
    total = 0
    for dy in (-1, 0, 1):
        for dx in (-1, 0, 1):
            if dx == dy == 0:
                continue
            total += board[(y + dy) % h][(x + dx) % w]
    return total


def step(board: Board) -> Board:
    """Compute one generation (toroidal surface)."""
    h, w = len(board), len(board[0])
    nxt = [[0] * w for _ in range(h)]
    for y in range(h):
        for x in range(w):
            n = count_neighbours(board, x, y)
            live = board[y][x]
            nxt[y][x] = 1 if (live and n in (2, 3)) or (not live and n == 3) else 0
    return nxt


def render(board: Board, live_ch: str, dead_ch: str) -> str:
    return '\n'.join(
        ''.join(live_ch if cell else dead_ch for cell in row)
        for row in board
    )


def simulate(w=80, h=24, gens=200, dens=0.30,
             live='#', dead='.', refresh=10):
    board = make_board(w, h, dens)
    for gen in range(gens + 1):
        if gen % refresh == 0:
            print('-' * w)
            print(f"Generation {gen}/{gens}")
            print(render(board, live, dead))
            print()
        board = step(board)


# ───────────────────────── CLI entry point ────────────────────── #

def main(argv):
    w   = int(argv[1]) if len(argv) > 1 else 80
    h   = int(argv[2]) if len(argv) > 2 else 24
    g   = int(argv[3]) if len(argv) > 3 else 200
    p   = float(argv[4]) if len(argv) > 4 else 0.30
    live = argv[5] if len(argv) > 5 else '#'
    dead = argv[6] if len(argv) > 6 else '.'
    ref  = int(argv[7]) if len(argv) > 7 else 10
    simulate(w, h, g, p, live[0], dead[0], ref)


if __name__ == "__main__":
    main(sys.argv)
