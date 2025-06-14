#!/usr/bin/env python3
"""
ulam-spiral.py
==============

Draws an Ulam spiral (prime spiral) in pure ASCII.

Usage
-----
    python ulam-spiral.py [size] [prime_char] [nonprime_char]

    • size          – odd side length of the square grid (default 101)
    • prime_char    – single character for primes      (default '#')
    • nonprime_char – character for composites & 1     (default '.')

The script prints the grid to stdout.  Use an odd size so the number 1
sits exactly at the centre.
"""

from __future__ import annotations
import sys
import math
from typing import List, Tuple

# ─────────────────── prime test ──────────────────── #

def is_prime(n: int) -> bool:
    if n < 2:                 return False
    if n in (2, 3):           return True
    if n % 2 == 0 or n % 3 == 0:
                               return False
    limit = int(math.isqrt(n))
    for f in range(5, limit + 1, 6):
        if n % f == 0 or n % (f + 2) == 0:
            return False
    return True


# ───────────────── spiral coordinates ────────────── #

def spiral_coords(size: int) -> List[Tuple[int, int]]:
    """
    Yield (x, y) grid coordinates for 1, 2, …, size² in square spiral.

    Coordinates are centred: (0, 0) is the grid centre (number 1).
    """
    x = y = 0
    dx, dy = 1, 0          # initial direction: right
    step_len = 1           # length of current segment
    steps_taken = 0
    segments = 0           # two segments → step_len increases

    for _ in range(size * size):
        yield x, y
        x += dx
        y += dy
        steps_taken += 1
        if steps_taken == step_len:
            # rotate direction 90° anticlockwise
            dx, dy = -dy, dx
            steps_taken = 0
            segments += 1
            if segments % 2 == 0:
                step_len += 1


# ───────────────────── renderer ──────────────────── #

def make_ulam_grid(size: int,
                   prime_char: str = '#',
                   nonprime_char: str = '.') -> List[str]:
    """Return a list of text rows representing the Ulam spiral."""
    half = size // 2
    grid = [[nonprime_char] * size for _ in range(size)]

    for n, (x, y) in enumerate(spiral_coords(size), start=1):
        row = half - y      # invert y so positive is up
        col = half + x
        grid[row][col] = prime_char if is_prime(n) else nonprime_char

    return ["".join(row) for row in grid]


def main(argv):
    size = int(argv[1]) if len(argv) > 1 else 101
    if size % 2 == 0:
        print("Size must be odd so that 1 is centred.", file=sys.stderr)
        sys.exit(1)

    prime_char    = argv[2] if len(argv) > 2 else '#'
    nonprime_char = argv[3] if len(argv) > 3 else '.'

    rows = make_ulam_grid(size, prime_char[0], nonprime_char[0])
    for line in rows:
        print(line)


if __name__ == "__main__":
    main(sys.argv)
