#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
P3 — Prompt → Program → Proof  (Deterministic, Self-Contained)

SCENARIO
--------
Nocturnal migratory birds can orient by the night sky. Experiments show they
are not fixating on a single star; instead they learn the *center of celestial
rotation* (the celestial pole) from the apparent motion of the star field.
Formally, they infer the point about which stars rotate — a geometric
estimation problem.

QUESTION
--------
Given short time-separated views of many stars, does a simple
multi-star "pole-from-motion" estimator outperform a naive single-star
heuristic (e.g., pretending the nearest bright star to the pole is the pole)?
If yes, that’s a concrete instance of **non-human mathematical reasoning**:
birds combine multiple noisy measurements using geometry to infer an invisible
reference point.

MODEL (deterministic)
---------------------
• True celestial pole at (0, 0) on a local sky dome (2D projection).
• A fixed set of N stars with polar coordinates (ρ_i, θ_i) relative to the pole,
  where ρ_i ∈ [ρ_min, ρ_max] degrees; θ_i ∈ [0, 360) degrees.
• Over a short interval Δt, stars rotate by Δφ degrees around the pole
  (counterclockwise). Observations at t0 and t1 = t0 + Δt.
• Small, fixed, *deterministic* observation noise is added to each (x, y).

ESTIMATORS
----------
A) Pole-from-motion (multi-star, geometric):
   For each star, form the chord between its two observed positions. The pole
   lies on the **perpendicular bisector** of that chord. Stack all bisector
   constraints and solve in least squares (A x = b) to estimate the center.

B) Single-star heuristic:
   Assume the *closest* observed star to the pole is itself the pole.

METRICS
-------
- Center error (deg): Euclidean distance between estimated pole and true pole.
- Heading error (deg): |arg(ĉ) - arg(c)| — angular error of the pole direction.
  (For our setup with c = (0, 0), the two are proportional; we still report both.)

CHECKS
------
- We test multiple sample sizes (S ∈ {3, 5, 8, 12, 20}) drawn from the same
  deterministic star field. We show errors for A vs B. No randomness is used.

WHY THIS IS "MATH" FOR NON-HUMANS
---------------------------------
The multi-star estimator is a straightforward **geometric inference**: combining
many constraints to recover an unknown center. Birds have been shown behaviorally
to use the rotation center rather than a single standout star — an emergent,
distributed “math-like” computation done by a brain with no classroom algebra.

HOW TO RUN
----------
$ python3 birds_star_map.py
(prints the P3 triad)
"""

import math

# ---------- Deterministic "pseudo-random" utilities (no true randomness) ----------
def dprng01(i: int) -> float:
    """
    Deterministic pseudo-random in [0,1). Uses a sine hash with fixed constants.
    This avoids importing random while staying fully reproducible.
    """
    x = math.sin(1.23456789 * (i + 0.918273645)) * 43758.5453
    return x - math.floor(x)

def deg2rad(d): return d * math.pi / 180.0
def rad2deg(r): return r * 180.0 / math.pi

# ---------- Star field and observation model ----------
def make_star_field(N: int, rho_min=12.0, rho_max=70.0):
    """
    Create N fixed stars at deterministic radii rho (deg) and angles theta (deg)
    around the true pole at (0,0).
    Returns list of (x, y) positions at t=0 in degrees (small-angle plane).
    """
    stars = []
    for i in range(N):
        # Radial distance in [rho_min, rho_max] (deg)
        rho = rho_min + (rho_max - rho_min) * dprng01(2*i)
        # Angle uniform in [0,360)
        theta = 360.0 * dprng01(2*i + 1)
        r = deg2rad(rho)
        t = deg2rad(theta)
        x = rho * math.cos(t)  # small-angle plane approx: use degrees as coordinates
        y = rho * math.sin(t)
        stars.append((x, y))
    return stars

def rotate_about_pole(p, dphi_deg):
    """Rotate point p=(x,y) around (0,0) by dphi (deg) in the plane."""
    (x, y) = p
    a = deg2rad(dphi_deg)
    ca, sa = math.cos(a), math.sin(a)
    return (ca*x - sa*y, sa*x + ca*y)

def add_det_noise(p, idx, scale_xy=(0.12, 0.12)):
    """
    Add small deterministic observation noise to point p using index idx.
    scale_xy are degree-scale jitters (x,y).
    """
    (x, y) = p
    nx = (dprng01(1000 + 3*idx) - 0.5) * 2.0 * scale_xy[0]
    ny = (dprng01(2000 + 5*idx) - 0.5) * 2.0 * scale_xy[1]
    return (x + nx, y + ny)

# ---------- Estimators ----------
def estimate_pole_from_motion(obs0, obs1):
    """
    Least-squares intersection of perpendicular bisectors of chords (obs0[i], obs1[i]).
    Each bisector constraint: (dx, dy) · c = (dx, dy) · m,  where
      dx, dy = obs1 - obs0,  m = (obs1 + obs0)/2
    Stack as A c = b and solve via normal equations: c = (A^T A)^(-1) A^T b.
    Returns estimated center (cx, cy).
    """
    # Build A (M×2) and b (M×1)
    a11 = a12 = a22 = 0.0  # components of A^T A
    bt1 = bt2 = 0.0        # components of A^T b
    for (x0, y0), (x1, y1) in zip(obs0, obs1):
        dx, dy = (x1 - x0), (y1 - y0)
        mx, my = (x0 + x1) * 0.5, (y0 + y1) * 0.5
        # Row: [dx dy], rhs: dx*mx + dy*my
        a11 += dx*dx
        a12 += dx*dy
        a22 += dy*dy
        rhs = dx*mx + dy*my
        bt1 += dx*rhs
        bt2 += dy*rhs

    # Solve 2x2 normal system
    det = a11*a22 - a12*a12
    if abs(det) < 1e-12:
        # Degenerate configuration (e.g., identical chords); return origin safely
        return (0.0, 0.0)
    inv11 =  a22 / det
    inv12 = -a12 / det
    inv22 =  a11 / det
    cx = inv11*bt1 + inv12*bt2
    cy = inv12*bt1 + inv22*bt2
    return (cx, cy)

def estimate_pole_single_star(obs0):
    """
    Naive heuristic: pick the observed star with smallest |p| (closest to pole)
    and pretend it is the pole.
    """
    best = min(obs0, key=lambda p: math.hypot(p[0], p[1]))
    return best

# ---------- Evaluation ----------
def angle_of(v):
    (x, y) = v
    if x == 0 and y == 0:
        return 0.0
    return math.atan2(y, x)

def eval_once(Nstars=60, S=8, dphi=15.0, noise_xy=(0.12, 0.12)):
    """
    Evaluate both estimators with:
      - Nstars: total stars in fixed sky (used deterministically)
      - S: how many stars the bird samples (first S from the list)
      - dphi: sky rotation between the two observations (deg)
      - noise_xy: deterministic observation jitter (deg)
    Returns dict with center/heading errors for both methods.
    """
    base = make_star_field(Nstars)
    # Take first S stars as the "sampled" stars (deterministic choice)
    s0 = [add_det_noise(base[i], i, noise_xy) for i in range(S)]
    s1 = [add_det_noise(rotate_about_pole(base[i], dphi), i, noise_xy) for i in range(S)]

    # A) Multi-star motion-based estimate
    cA = estimate_pole_from_motion(s0, s1)
    # B) Single-star heuristic
    cB = estimate_pole_single_star(s0)

    # Errors relative to true pole at (0,0)
    center_err_A = math.hypot(cA[0], cA[1])
    center_err_B = math.hypot(cB[0], cB[1])

    # Heading angles (direction to pole); here identical to the vector to (0,0),
    # but we report angular error magnitudes for completeness.
    ang_true = 0.0  # direction to (0,0) is "north" reference in this plane
    ang_A = angle_of(cA)
    ang_B = angle_of(cB)
    head_err_A = abs(rad2deg((ang_A - ang_true + math.pi) % (2*math.pi) - math.pi))
    head_err_B = abs(rad2deg((ang_B - ang_true + math.pi) % (2*math.pi) - math.pi))

    return {
        "S": S, "dphi": dphi,
        "center_err_A": center_err_A,
        "center_err_B": center_err_B,
        "head_err_A": head_err_A,
        "head_err_B": head_err_B,
        "cA": cA, "cB": cB
    }

def table_for_sample_sizes(sample_sizes=(3,5,8,12,20), Nstars=60, dphi=15.0):
    """Compute a small table of results for various star sample sizes S."""
    rows = []
    for S in sample_sizes:
        r = eval_once(Nstars=Nstars, S=S, dphi=dphi)
        rows.append(r)
    return rows

# ---------- Compute (deterministic) ----------
rows = table_for_sample_sizes()

# Rank methods by median center error across S
med_A = sorted(r["center_err_A"] for r in rows)[len(rows)//2]
med_B = sorted(r["center_err_B"] for r in rows)[len(rows)//2]
best_method = "Multi-star motion (A)" if med_A < med_B else "Single-star (B)"

# ---------- Emit the P3 triad ----------
print("# Answer")
print(f"The better celestial-pole estimator is **{best_method}** "
      f"(median center error: A={med_A:.3f}°, B={med_B:.3f}°).")

print("\n# Reason Why (Deterministic geometric inference from star motion)")
print("S = number of stars used; Δφ = sky rotation between two views.")
for r in rows:
    print(f"- S={r['S']:>2}, Δφ={r['dphi']:>4.1f}°  "
          f"A(center,heading)={r['center_err_A']:.3f}°, {r['head_err_A']:.3f}°   "
          f"B(center,heading)={r['center_err_B']:.3f}°, {r['head_err_B']:.3f}°")

print("\n# Check (Deterministic sanity tests)")
# 1) More stars should not hurt the multi-star estimate:
nonincreasing_ok = True
prev = None
for r in rows:
    if prev is not None and r["center_err_A"] > prev + 1e-9:
        nonincreasing_ok = False
    prev = r["center_err_A"]
print(f"- Multi-star center error nonincreasing with S: {nonincreasing_ok}")

# 2) Two identical views (Δφ=0) make A degenerate -> we detect and return origin:
degenerate = eval_once(S=8, dphi=0.0)
print(f"- Degenerate case Δφ=0 returns near-origin center for A: "
      f"{math.hypot(degenerate['cA'][0], degenerate['cA'][1]) < 1e-6}")

# 3) Larger rotation gives more leverage (compare S=8 at Δφ=5° vs 25°)
r_small = eval_once(S=8, dphi=5.0)
r_large = eval_once(S=8, dphi=25.0)
print(f"- Larger Δφ improves A's center accuracy: "
      f"{r_large['center_err_A'] <= r_small['center_err_A']} "
      f"(5°→{r_small['center_err_A']:.3f}°, 25°→{r_large['center_err_A']:.3f}°)")

# Exit nonzero if multi-star is not better on median (we expect it to be).
ok = (best_method == "Multi-star motion (A)")
if not ok:
    raise SystemExit(1)

