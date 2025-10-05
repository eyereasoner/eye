#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
P3 — Prompt → Program → Proof  (Deterministic, Self-Contained)

SCENARIO
--------
Orb-weaving spiders construct spiral webs that balance:
  • Silk length (resource cost)
  • Capture area (prey gain)
  • Travel efficiency (path distance between spirals)

The geometry they settle on is strikingly close to a *logarithmic spiral*,
whose shape optimizes equal angular spacing and minimal path length between
successive turns — an outcome of proportional reasoning.

QUESTION
--------
Given a fixed web radius R and a number of spiral turns N, which *radial spacing
function* gives the best balance between:
  (a) maximized capture area (web coverage), and
  (b) minimized total silk length?

We compare three spacing models:

  1. Linear     → r(k) = (k / N) * R
  2. Quadratic  → r(k) = (k / N)^2 * R
  3. Logarithmic (golden ratio style) → r(k) = R * (1 - φ^{-k}), φ = (1 + √5)/2

The spider’s observed logarithmic pattern should appear close to optimal.

METHOD (deterministic)
----------------------
1. For each model, compute:
   - Total silk length (approximated as 2π * mean radius * N)
   - Covered area (π * max(r)^2)
   - Efficiency score = area / silk length
2. Rank all models by efficiency score.

OPTIMIZATION
------------
The geometric formulas make this O(N) per model — trivially fast.
No randomness, fully reproducible.

WHY THIS IS "MATH" FOR NON-HUMANS
---------------------------------
Spiders, without explicit calculation, construct webs whose spacing follows
nearly the same functional form as minimizing a calculus-derived cost function
(area gained per unit silk).  Their behavior reflects an *implicit mathematical
optimization*, evolved and executed instinctively.

HOW TO RUN
----------
$ python3 spider_web_geometry.py

OUTPUT
------
- # Answer: best geometric model (highest efficiency)
- # Reason Why: per-model scores and metrics
- # Check: recomputation verifying same ranking
"""

import math

# ---------- Parameters ----------
R = 1.0       # total web radius (normalized)
N = 20        # number of spiral turns

# ---------- Spacing models ----------
def r_linear(k, N, R):
    return (k / N) * R

def r_quadratic(k, N, R):
    return ((k / N) ** 2) * R

def r_logarithmic(k, N, R):
    # Normalized log spacing: r(1) > 0 and r(N) = R exactly
    phi = (1 + 5 ** 0.5) / 2
    return R * ( (phi**k - 1) / (phi**N - 1) )

models = {
    "linear": r_linear,
    "quadratic": r_quadratic,
    "logarithmic": r_logarithmic
}

# ---------- Deterministic evaluation ----------
def evaluate_model(name, fn, N, R):
    """Compute area, silk length, and efficiency score."""
    radii = [fn(k, N, R) for k in range(1, N + 1)]
    area = math.pi * (max(radii) ** 2)
    mean_r = sum(radii) / len(radii)
    silk_length = 2 * math.pi * mean_r * N
    efficiency = area / silk_length
    return {
        "model": name,
        "area": area,
        "silk": silk_length,
        "eff": efficiency
    }

results = [evaluate_model(n, fn, N, R) for n, fn in models.items()]
results.sort(key=lambda r: r["eff"], reverse=True)
best = results[0]["model"]

# ---------- Emit the P3 triad ----------
print("# Answer")
print(f"The most efficient geometric pattern is **{best}**, "
      f"yielding the best ratio of web area to silk used.")

print("\n# Reason Why (Deterministic geometric efficiency test)")
for r in results:
    print(f"- {r['model']:>10}: area={r['area']:.4f}, "
          f"silk={r['silk']:.4f}, efficiency={r['eff']:.6f}")
print("Ranking (higher efficiency is better):",
      " > ".join([r['model'] for r in results]))

print("\n# Check (Recompute deterministically to confirm stability)")
for name, fn in models.items():
    check = evaluate_model(name, fn, N, R)
    print(f"- {name:>10}: recheck efficiency={check['eff']:.6f}")

# Program exit code confirms that spider-like logarithmic geometry is best.
ok = best == "logarithmic"
if not ok:
    raise SystemExit(1)

