#!/usr/bin/env python
"""
Chaos-Game Sierpiński (ASCII Edition)
Inspired by the ‘Fractals & Infinity’ spread in
“Mathematica” (Lees & Farndon, 2025).

The program:
  • runs the classic chaos game inside an equilateral triangle
  • maps the points onto a text grid
  • prints the grid as ASCII art
"""

import random
import math

# ── tweakables ────────────────────────────────────────────────
WIDTH      = 80     # columns of output
HEIGHT     = 40     # rows   of output
ITERATIONS = 50_000 # ≥ 20 000 gives a clear triangle
POINT_CHAR = "*"    # what to draw for a plotted point
# -------------------------------------------------------------

# 1. Triangle vertices (equilateral, side length = 1)
VERTICES = [
    (0.0, 0.0),
    (1.0, 0.0),
    (0.5, math.sin(math.radians(60)))  # ≈ (0.5, 0.866...)
]

# 2. Start at any point inside the triangle
x, y = 0.25, 0.5

# 3. Empty text canvas
canvas = [[" " for _ in range(WIDTH)] for _ in range(HEIGHT)]
max_y = VERTICES[2][1]  # highest y-coordinate (≈0.866)

# 4. Chaos game loop
for _ in range(ITERATIONS):
    vx, vy = random.choice(VERTICES)
    x = (x + vx) / 2
    y = (y + vy) / 2

    col = int(x * (WIDTH - 1))
    row = int((1 - y / max_y) * (HEIGHT - 1))  # invert y → row 0 is top
    canvas[row][col] = POINT_CHAR

# 5. Print the ASCII art
for row in canvas:
    print("".join(row))
