#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Mandelbrot — ARC (Answer / Reason / Check), self-contained

Usage
  python mandelbrot.py [width] [height] [max_iter]
    • width, height — character grid (defaults: 80 × 40)
    • max_iter — iteration cap (default: 100)

Behavior
  Renders a plain-text (no color) Mandelbrot view centered at (-0.5, 0),
  spanning x ∈ [-2.0, 1.0], y ∈ [-1.2, 1.2], using an ASCII palette.

ARC layout
  • Answer — prints the ASCII picture using the provided (or default) params
  • Reason why — explains the escape-time algorithm and shading
  • Check (harness) — validates a few mathematical invariants:
      - known interior points don’t escape within the cap
      - obvious exterior points escape quickly
      - conjugate symmetry (c and conj(c) give equal escape counts)
      - image is symmetric across the real axis for this viewport
"""

from __future__ import annotations
import sys
from typing import List, Tuple

# ───────────────────────────── Core maths ─────────────────────────────
def mandelbrot(c: complex, max_iter: int) -> int:
    """Return the escape iteration count (0…max_iter, where max_iter means 'never escaped')."""
    z = 0j
    for n in range(max_iter):
        if (z.real*z.real + z.imag*z.imag) > 4.0:  # |z|^2 > 4 ⇒ |z| > 2
            return n
        z = z*z + c
    return max_iter

# ─────────────────────────── ASCII shading ────────────────────────────
PALETTE = " .:-=+*#%@"  # light → dark (10 shades)

def shade(n: int, max_iter: int) -> str:
    """Map iteration count to a single ASCII character (no ANSI colors)."""
    idx = int(n / max_iter * (len(PALETTE) - 1))
    return PALETTE[idx]

# ───────────────────────────── Rendering ──────────────────────────────
def render(width: int, height: int, max_iter: int) -> None:
    """Print an ASCII Mandelbrot view centered at (-0.5, 0)."""
    xmin, xmax = -2.0, 1.0
    ymin, ymax = -1.2, 1.2
    for j in range(height):
        # map row index to complex-plane y (top→bottom)
        y = ymax - (ymax - ymin) * j / (height - 1)
        row_chars: List[str] = []
        for i in range(width):
            # map column index to x
            x = xmin + (xmax - xmin) * i / (width - 1)
            n = mandelbrot(complex(x, y), max_iter)
            row_chars.append(shade(n, max_iter))
        print("".join(row_chars))

# helper for the harness: capture escape counts for a small grid
def sample_counts(width: int, height: int, max_iter: int) -> List[List[int]]:
    xmin, xmax = -2.0, 1.0
    ymin, ymax = -1.2, 1.2
    grid: List[List[int]] = []
    for j in range(height):
        y = ymax - (ymax - ymin) * j / (height - 1)
        row: List[int] = []
        for i in range(width):
            x = xmin + (xmax - xmin) * i / (width - 1)
            row.append(mandelbrot(complex(x, y), max_iter))
        grid.append(row)
    return grid

# ─────────────────────────────── ARC: Answer ───────────────────────────────
def print_answer(width: int, height: int, iters: int):
    print("Answer")
    print("======")
    print(f"ASCII Mandelbrot ({width}×{height}, max_iter={iters})\n")
    render(width, height, iters)

# ───────────────────────────── ARC: Reason why ──────────────────────────────
def print_reason():
    print("\nReason why")
    print("==========")
    print("We use the classic escape-time algorithm for the quadratic map z ↦ z² + c:")
    print("  • Start z₀ = 0; iterate zₙ₊₁ = zₙ² + c.")
    print("  • If |zₙ| > 2 at step n < max_iter, we say c escapes at n.")
    print("  • If no escape is seen within max_iter, we print the darkest shade.")
    print("Shading linearly maps the escape count n ∈ [0, max_iter] onto 10 ASCII levels.")

# ─────────────────────────── ARC: Check (harness) ───────────────────────────
def print_check():
    print("\nCheck (harness)")
    print("===============")
    ok_all = True
    cap = 500  # a comfortable cap for the harness

    # 1) Known interior points (main cardioid and 2-cycle bulb) never escape
    #    Main cardioid: c = μ/2 - μ²/4 with |μ| < 1
    def cardioid_point(r: float, theta: float) -> complex:
        μ = r * complex.__new__(complex, __import__("math").cos(theta), __import__("math").sin(theta))
        return μ/2 - (μ*μ)/4

    import math
    inside_samples = [
        0+0j,                                # center
        cardioid_point(0.6, 0.0),            # inside cardioid
        cardioid_point(0.9, math.pi/3),      # "
        -1+0j,                                # center of the period-2 bulb
        -1+0.1j, -1-0.1j                      # still in the bulb (radius 0.25)
    ]
    ok_inside = all(mandelbrot(c, cap) == cap for c in inside_samples)
    print(f"Interior samples don’t escape within cap? {ok_inside}")
    ok_all &= ok_inside

    # 2) Obvious exterior points escape (often in a few steps)
    outside_samples = [2+0j, 1+1j, 0.5+0j, 0.3+0.5j, -2.5+0j]
    ok_outside = all(mandelbrot(c, cap) < cap for c in outside_samples)
    print(f"Exterior samples escape before cap? {ok_outside}")
    ok_all &= ok_outside

    # 3) Conjugate symmetry: escape counts match for c and conj(c)
    probes = [complex(x/10, y/10) for x in range(-15, 16, 5) for y in range(-12, 13, 4)]
    ok_conj = all(mandelbrot(c, cap) == mandelbrot(complex(c.real, -c.imag), cap) for c in probes)
    print(f"Conjugate symmetry (c vs. conj(c)) holds? {ok_conj}")
    ok_all &= ok_conj

    # 4) Image symmetry across the real axis for our viewport
    grid = sample_counts(121, 81, 200)
    h = len(grid); w = len(grid[0])
    ok_sym = all(grid[j][i] == grid[h-1-j][i] for j in range(h) for i in range(w))
    print(f"Rendered grid symmetric across real axis? {ok_sym}")
    ok_all &= ok_sym

    # 5) Shading maps endpoints correctly
    ok_shade = (shade(0, 100) == PALETTE[0]) and (shade(100, 100) == PALETTE[-1])
    print(f"Shading endpoints (0 → '{PALETTE[0]}', max → '{PALETTE[-1]}') correct? {ok_shade}")
    ok_all &= ok_shade

    print(f"\nAll checks passed? {ok_all}")

# ─────────────────────────────────── Main ───────────────────────────────────
def main(argv: List[str]) -> None:
    width  = int(argv[1]) if len(argv) > 1 else 80
    height = int(argv[2]) if len(argv) > 2 else 40
    iters  = int(argv[3]) if len(argv) > 3 else 100
    print_answer(width, height, iters)
    print_reason()
    print_check()

if __name__ == "__main__":
    main(sys.argv)

