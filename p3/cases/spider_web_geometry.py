#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
P3 — Prompt → Program → Proof  (Deterministic, Normalized, with Uniformity)

SCENARIO
--------
Orb-weaving spiders build spiral webs that juggle:
  • Capture area (benefit)
  • Silk used (cost to build/maintain)
  • Uniform coverage (avoid big gaps that let prey pass through)

QUESTION
--------
For a fixed outer radius R and number of spiral turns N, which *radial spacing*
is most efficient once we (1) normalize all models to reach the rim, and
(2) reward more uniform coverage?

MODELS (all satisfy r(N) = R exactly)
-------------------------------------
1) Linear:      r_k = (k/N) * R
2) Quadratic:   r_k = (k/N)^2 * R
3) Logarithmic: r_k = R * ( (φ^k - 1) / (φ^N - 1) ),  φ=(1+√5)/2   ← normalized

METRICS (deterministic)
-----------------------
- Spiral silk   S_spiral ≈ Σ 2π r_k                (circumference of each turn)
- Spoke travel  S_spoke  = Σ (r_k - r_{k-1}), r_0=0 (one monotone outward pass)
- Coverage gaps Δ_k = r_k - r_{k-1};  uniformity via CV = std(Δ)/mean(Δ) (lower better)
- Composite efficiency:
      score = (Area) / (S_spiral + α*S_spoke) * U,
  where Area = πR^2 (same for all when r_N = R),
        α = 0.25 (small spoke weight),
        U = 1 / (1 + CV)  (penalizes uneven spacing).

WHY THIS IS “MATH” FOR NON-HUMANS
---------------------------------
Without explicit arithmetic, spiders’ construction rules produce spacing close to
a logarithmic spiral — effectively **optimizing** area-per-silk while keeping
gaps even. That’s a real-world combinatorial/geometry optimization.

HOW TO RUN
----------
$ python3 spider_web_geometry_normalized.py

OUTPUT
------
- # Answer: best model by the composite efficiency score
- # Reason Why: per-model metrics (silk, uniformity, score)
- # Check: recompute to confirm determinism
"""

import math
from statistics import pstdev

# ---------------- Parameters ----------------
R = 1.0                 # outer radius (normalized)
N = 20                  # number of spiral turns
alpha_spoke = 0.25      # weight of spoke travel in total "cost"

# ---------------- Spacing models (all reach r_N = R) ----------------
def r_linear(k, N, R):
    return (k / N) * R

def r_quadratic(k, N, R):
    return ((k / N) ** 2) * R

def r_logarithmic(k, N, R):
    # Normalized so r(N) = R exactly; multiplicative spacing ~ constant ratio.
    phi = (1 + 5 ** 0.5) / 2
    return R * ((phi**k - 1) / (phi**N - 1))

MODELS = {
    "linear": r_linear,
    "quadratic": r_quadratic,
    "logarithmic": r_logarithmic,
}

# ---------------- Deterministic evaluation ----------------
def evaluate_model(name, fn, N, R, alpha=alpha_spoke):
    # Radii for turns 1..N, with r_0 = 0 for gap computations
    radii = [fn(k, N, R) for k in range(1, N + 1)]
    r0 = 0.0
    gaps = []
    prev = r0
    for r in radii:
        gaps.append(r - prev)
        prev = r

    # Spiral silk: sum of circumferences for each turn
    S_spiral = 2 * math.pi * sum(radii)

    # Spoke travel: monotone outward traversal (no returns)
    S_spoke = sum(gaps)  # equals R since r_N = R

    # Coverage uniformity: coefficient of variation of radial gaps (lower is better)
    mean_gap = sum(gaps) / len(gaps)
    sd_gap = pstdev(gaps)  # population stdev (deterministic)
    cv = (sd_gap / mean_gap) if mean_gap > 0 else 0.0
    U = 1.0 / (1.0 + cv)

    # Composite score: area per (silk + spoke) times uniformity
    area = math.pi * (R ** 2)
    cost = S_spiral + alpha * S_spoke
    score = (area / cost) * U

    return {
        "model": name,
        "silk_spiral": S_spiral,
        "spoke": S_spoke,
        "cv_gaps": cv,
        "uniformity": U,
        "score": score,
    }

results = [evaluate_model(n, fn, N, R) for n, fn in MODELS.items()]
results.sort(key=lambda r: r["score"], reverse=True)
best = results[0]["model"]

# ---------------- Emit the P3 triad ----------------
print("# Answer")
print(f"The most efficient pattern is **{best}** when we normalize to r(N)=R and reward uniform coverage.")

print("\n# Reason Why (Deterministic normalized geometry with uniformity)")
for r in results:
    print(f"- {r['model']:>11}: silk={r['silk_spiral']:.4f}, spoke={r['spoke']:.4f}, "
          f"CV_gaps={r['cv_gaps']:.4f}, U={r['uniformity']:.4f}, score={r['score']:.6f}")
print("Ranking (higher score is better):", " > ".join([r['model'] for r in results]))

print("\n# Check (Recompute deterministically to confirm stability)")
for name, fn in MODELS.items():
    chk = evaluate_model(name, fn, N, R)
    print(f"- {name:>11}: recheck score={chk['score']:.6f}")

# Exit nonzero if logarithmic isn’t best (what orb-weaver field data generally support).
ok = best == "logarithmic"
if not ok:
    raise SystemExit(1)

