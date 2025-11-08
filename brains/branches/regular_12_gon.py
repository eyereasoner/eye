#!/usr/bin/env python3
"""
Regular 12-gon side length (ARC-ified)
======================================

Goal
----
Compute the side length s of a regular 12-gon inscribed in the unit circle (r = 1),
without imports, and print a clear explanation plus an **Answer / Reason why / Check (harness)** trio.

Facts
-----
• A side is a chord, so for central angle θ = 2π/n the chord length is:
      s = 2 r sin(θ/2).
• For n=12 and r=1: θ = 2π/12 = π/6 and s = 2 sin(π/12) = 2 sin(15°).
• Using angle subtraction (45°−30°),
      sin(15°) = (√6 − √2) / 4  ⇒  s = (√6 − √2) / 2.
• Equivalent closed form: s = √(2 − √3).

No imports are used; we implement π, sqrt, and sin (Taylor) ourselves.
"""

# -----------------------------
# Minimal numeric “toolbox”
# -----------------------------

# Enough precision for our needs
pi = 3.141592653589793

def sqrt(x: float) -> float:
    """Newton–Raphson sqrt with fixed iterations; no imports required."""
    if x == 0.0:
        return 0.0
    g = x / 2.0 if x >= 1.0 else 1.0  # decent initial guess for all x>0
    for _ in range(25):
        g = 0.5 * (g + x / g)
    return g

def sin(x: float) -> float:
    """
    sin(x) by Taylor series around 0:
      sin x = x − x^3/3! + x^5/5! − ...
    Good accuracy for |x| ≤ π with ~10–12 terms; here x = π/12 ≈ 0.2618.
    """
    # wrap to [-π, π] (helps accuracy)
    two_pi = 2.0 * pi
    while x >  pi: x -= two_pi
    while x < -pi: x += two_pi

    term = x
    res  = 0.0
    k    = 1        # current odd power/factorial index
    sgn  = 1.0
    for _ in range(12):  # 12 terms is overkill here; still cheap
        res += sgn * term
        # next term: multiply by x^2 / [(k+1)(k+2)] and flip sign
        term = term * x * x / ((k + 1) * (k + 2))
        k   += 2
        sgn *= -1.0
    return res

# -----------------------------
# Geometry-specific computation
# -----------------------------

n = 12          # dodecagon
r = 1.0         # unit circle

theta      = 2.0 * pi / n          # central angle = π/6
half_theta = theta / 2.0           # π/12

# Numeric via chord formula + Taylor sine
s_numeric = 2.0 * r * sin(half_theta)

# Closed-form 1:  s = (√6 − √2)/2
s_closed1 = (sqrt(6.0) - sqrt(2.0)) / 2.0

# Closed-form 2 (equivalent): s = √(2 − √3)
s_closed2 = sqrt(2.0 - sqrt(3.0))

# -----------------------------
# ARC output
# -----------------------------

print("Answer")
print("------")
print("Side length of a regular 12-gon in the unit circle:")
print("  Exact forms:  s = (√6 − √2) / 2  =  √(2 − √3)")
print(f"  Numeric:     s ≈ {s_numeric:.15f}")
print(f"  Closed-form: (√6 − √2)/2 ≈ {s_closed1:.15f}")
print(f"  Also:        √(2 − √3)  ≈ {s_closed2:.15f}")
print()

print("Reason why")
print("----------")
print("A side is a chord subtending central angle θ = 2π/n. Bisecting yields")
print("sin(θ/2) = (s/2)/r, hence s = 2 r sin(θ/2). For n=12, r=1 ⇒ s = 2 sin(π/12).")
print("Using sin(45°−30°) with standard exact values gives sin(15°) = (√6 − √2)/4,")
print("so s = 2⋅sin(15°) = (√6 − √2)/2, which equals √(2 − √3).")
print()

print("Check (harness)")
print("---------------")
# 1) Consistency of two closed forms
eps = 1e-12
diff_closed = abs(s_closed1 - s_closed2)
print(f"Closed-form equivalence: |(√6−√2)/2 − √(2−√3)| = {diff_closed:.3e}")
assert diff_closed < 1e-12, "Closed forms should be numerically equal."

# 2) Numeric (Taylor) vs exact
diff_num = abs(s_numeric - s_closed1)
print(f"Taylor vs exact: |s_numeric − s_exact| = {diff_num:.3e}")
assert diff_num < 1e-12, "Numeric sine should match the exact value closely."

# 3) Angle sanity: θ=π/6 and θ/2=π/12
pi_over_6  = pi / 6.0
pi_over_12 = pi / 12.0
assert abs(theta - pi_over_6)  < 1e-15
assert abs(half_theta - pi_over_12) < 1e-15
print("Angle checks passed (θ=π/6, θ/2=π/12).")

# 4) Optional alternative chord identity: s = √(2 − 2 cos θ) with cos θ = √3/2
cos_pi_over_6 = sqrt(3.0) / 2.0
s_alt = sqrt(2.0 - 2.0 * cos_pi_over_6)
print(f"Chord identity check: √(2−2cosθ) ≈ {s_alt:.15f}")
assert abs(s_alt - s_closed1) < 1e-12

print("All checks passed ✔")

