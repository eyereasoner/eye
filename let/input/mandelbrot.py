#!/usr/bin/env python3
"""
mandelbrot.py
=============

Plain-text Mandelbrot-set renderer (no colour codes).

Usage
-----
    python mandelbrot.py [width] [height] [max_iter]

    • width, height  – grid size in characters (defaults: 80 × 40)
    • max_iter       – iteration cap (default: 100)

The script prints a static picture centred at (-0.5, 0) spanning 3 × 2.4
units.  Adjust xmin/xmax or ymin/ymax in `render()` to zoom/pan.
"""

from __future__ import annotations
import sys


# ───────────────────── core maths ────────────────────── #

def mandelbrot(c: complex, max_iter: int) -> int:
    """Return the escape iteration count (0…max_iter)."""
    z = 0j
    for n in range(max_iter):
        if abs(z) > 2.0:
            return n
        z = z*z + c
    return max_iter


# ───────────────────── shading table ─────────────────── #

PALETTE = " .:-=+*#%@"          # 10 ASCII shades (light → dark)

def shade(n: int, max_iter: int) -> str:
    """Map iteration count to a single character (no colour codes)."""
    idx = int(n / max_iter * (len(PALETTE) - 1))
    return PALETTE[idx]


# ───────────────────── renderer ──────────────────────── #

def render(width: int, height: int, max_iter: int):
    """Print an ASCII Mandelbrot centred at (-0.5, 0)."""
    xmin, xmax = -2.0, 1.0
    ymin, ymax = -1.2, 1.2
    for j in range(height):
        y = ymax - (ymax - ymin) * j / (height - 1)
        row = []
        for i in range(width):
            x = xmin + (xmax - xmin) * i / (width - 1)
            n = mandelbrot(complex(x, y), max_iter)
            row.append(shade(n, max_iter))
        print("".join(row))


# ───────────────────── CLI entry point ────────────────── #

def main(argv):
    width  = int(argv[1]) if len(argv) > 1 else 80
    height = int(argv[2]) if len(argv) > 2 else 40
    iters  = int(argv[3]) if len(argv) > 3 else 100
    render(width, height, iters)


if __name__ == "__main__":
    main(sys.argv)

