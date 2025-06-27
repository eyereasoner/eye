#!/usr/bin/env python3
"""
Ulam Prime Spiral (ASCII Edition)
Inspired by the ‘Prime Patterns’ spread in
“Mathematica” (Lees & Farndon, 2025).

What it does
------------
• Builds an outward square spiral of the integers 1, 2, 3, … up to N²
• Marks every prime with a chosen glyph (default '*')
• Prints the grid so you can see the striking diagonal bands of primes
"""

import math

# ── tweakables ─────────────────────────────────────────
SIZE       = 49        # grid width/height (odd number works best)
PRIME_CHAR = "+"       # how a prime is drawn
NONPRIME   = " "       # how a non-prime cell is drawn
# ------------------------------------------------------

def is_prime(n: int) -> bool:
    """Simple deterministic primality test for n ≤ 2 000 000^2 (fast enough here)."""
    if n < 2 or n % 2 == 0 and n != 2:
        return n == 2
    limit = int(math.isqrt(n))
    for f in range(3, limit + 1, 2):
        if n % f == 0:
            return False
    return True

def build_spiral(size: int) -> list[list[int]]:
    """Return a square list-of-lists filled with the spiral numbers."""
    grid = [[0] * size for _ in range(size)]
    x = y = size // 2              # start in the centre
    dx, dy = 1, 0                  # initial direction: right
    step_len = 1                   # how many steps in current leg
    num, steps_taken, legs_done = 1, 0, 0

    while True:
        grid[y][x] = num
        if num == size * size:
            break

        x += dx
        y += dy
        num += 1
        steps_taken += 1

        if steps_taken == step_len:
            # turn left (dx,dy) ↦ (-dy,dx)
            dx, dy = -dy, dx
            steps_taken = 0
            legs_done += 1
            if legs_done % 2 == 0:      # every two legs, expand step length
                step_len += 1
    return grid

def print_prime_spiral(size: int) -> None:
    grid = build_spiral(size)
    for row in grid:
        line = "".join(PRIME_CHAR if is_prime(n) else NONPRIME for n in row)
        print(line)

if __name__ == "__main__":
    print_prime_spiral(SIZE)

