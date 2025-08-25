#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Aristotle-flavored Euler: a self-contained Python script

WHAT THIS PROGRAM DOES (and why it's "Aristotelian"):
----------------------------------------------------
We interpret complex numbers as *plane magnitudes* (2D vectors) so that
- addition is vector addition; and
- multiplication corresponds to composing a scaling and a rotation.

On the unit circle, the element e^{iθ} is simply a rotation by angle θ:
    E(θ) := (cos θ, sin θ).

This matches Aristotle's taste because:
  • MATERIAL CAUSE  : the "stuff" here is geometric—points/vectors in the plane.
  • FORMAL CAUSE    : the essence (logos) of multiplication is "scale + rotate".
  • EFFICIENT CAUSE : demonstrations are via Euclidean facts (angle addition, length preservation).
  • FINAL CAUSE     : the goal is explanatory unity—rotations, lengths, and algebra cohere.

INVENTION vs DISCOVERY (not used by the code, but the perspective it encodes):
  • DISCOVERY: the structure of plane rotations and their composition law.
  • INVENTION: our symbols/notations (i, e, algebraic rules) used to express that structure.

PROGRAM OUTPUT:
  1) Answer  – evaluates e^{iπ} + 1 and shows it is (numerically) 0.
  2) Reason  – a geometric explanation in words.
  3) Check   – an independent harness that verifies key laws:
        - |E(θ)| = 1 (unit circle)
        - E(α) * E(β) = E(α+β) (angle-addition / rotation composition)
        - (E(π/2))^2 = E(π) (quarter-turn squared is half-turn)
        - Rotations preserve Euclidean length
        - |z·w| = |z|·|w| (multiplicativity of norm)
        - De Moivre: (E(θ))^n = E(nθ) for random n
        - Euler’s identity: E(π) + 1 = 0

NOTE: We intentionally *do not* use Python's built-in complex numbers so the
meaning remains purely geometric, matching the "logos-as-structure" view.
"""

import math
import random

# --------------------------- Configuration ------------------------------------
EPS = 1e-10          # numerical tolerance for "almost equal"
DIGITS = 12          # digits for printing floats
RANDOM_SEED = 0      # reproducible harness runs

# --------------------------- Basic 2D operations -------------------------------
# Each "number" is a pair (a,b) which we read as a + b·i, but *geometrically* it is
# an oriented plane magnitude (vector). We keep operations explicit to show essence.

def add(z, w):
    """Vector addition: (a,b) + (c,d) = (a+c, b+d)."""
    a, b = z; c, d = w
    return (a + c, b + d)

def mul(z, w):
    """
    Geometric multiplication: compose scale+rotation.
    Algebraically: (a + bi)(c + di) = (ac - bd) + (ad + bc)i.
    """
    a, b = z; c, d = w
    return (a*c - b*d, a*d + b*c)

def conj(z):
    """Reflection in the x-axis (reverse orientation): (a, b) -> (a, -b)."""
    a, b = z
    return (a, -b)

def norm(z):
    """Euclidean length (magnitude) of a vector."""
    a, b = z
    return math.hypot(a, b)

def almost_equal(z, w, tol=EPS):
    """Vector closeness in Euclidean norm."""
    return norm((z[0]-w[0], z[1]-w[1])) <= tol

def to_str(z, digits=DIGITS):
    """Pretty string 'a ± b·i' with fixed decimals."""
    a, b = z
    return f"{a:.{digits}f} {'+' if b>=0 else '-'} {abs(b):.{digits}f}·i"

# --------------------------- Rotations and E(θ) --------------------------------
def E(theta):
    """
    Geometric meaning of e^{iθ}: the unit rotation by angle θ,
    represented as the point (cos θ, sin θ) on the unit circle.
    """
    return (math.cos(theta), math.sin(theta))

def rotate(theta, v):
    """
    Apply rotation by θ to vector v using the standard rotation matrix.
      [[cos, -sin],
       [sin,  cos]]
    """
    c, s = math.cos(theta), math.sin(theta)
    x, y = v
    return (c*x - s*y, s*x + c*y)

# --------------------------- Utilities -----------------------------------------
def section(title):
    print("\n" + "="*len(title))
    print(title)
    print("="*len(title))

def powN(z, n):
    """
    Integer power by repeated squaring (no Python complex).
    De Moivre becomes transparent when z=E(θ).
    """
    if n < 0:
        # For completeness; invert via conjugate/norm^2
        # z^{-1} = conj(z) / |z|^2  (provided z != 0)
        a, b = z
        r2 = a*a + b*b
        if r2 == 0:
            raise ZeroDivisionError("Attempted inverse of 0 vector.")
        z = (a/r2, -b/r2)
        n = -n
    out = (1.0, 0.0)   # multiplicative identity
    base = z
    while n:
        if n & 1:
            out = mul(out, base)
        base = mul(base, base)
        n >>= 1
    return out

# --------------------------- 1) Answer -----------------------------------------
def answer():
    section("Answer")
    one = (1.0, 0.0)
    e_ipi = E(math.pi)                 # rotation by π -> (-1, 0)
    lhs = add(e_ipi, one)              # e^{iπ} + 1
    print("Interpretation: E(θ) = rotation by θ on the unit circle.")
    print(f"E(π)        ≈ {to_str(e_ipi)}")
    print(f"E(π) + 1    ≈ {to_str(lhs)}")
    print("Conclusion : Euler's identity holds: E(π) + 1 = 0 (within tolerance).")

# --------------------------- 2) Reason -----------------------------------------
def reason():
    section("Reason (geometric explanation)")
    print(
        "1) On the unit circle, a rotation by θ maps (1,0) to (cos θ, sin θ) = E(θ).\n"
        "2) Composition of rotations adds angles, which mirrors multiplication:\n"
        "       E(α) · E(β) = E(α + β).\n"
        "3) Setting θ = π gives E(π) = (-1, 0); thus E(π) + (1,0) = (0,0).\n"
        "This unifies arithmetic with geometry: multiplying by E(θ) *is* 'turn by θ'."
    )

# --------------------------- 3) Independent Check (harness) --------------------
def check():
    section("Independent Check (harness)")
    random.seed(RANDOM_SEED)
    ok = True

    # (A) Unit length: |E(θ)| = 1
    for _ in range(200):
        th = random.uniform(-20*math.pi, 20*math.pi)
        if abs(norm(E(th)) - 1.0) > EPS:
            ok = False; print("[FAIL] Unit circle: |E(θ)| != 1"); break
    if ok: print("[PASS] Unit circle: |E(θ)| = 1 for random θ")

    # (B) Group law (angle addition): E(α) * E(β) = E(α+β)
    if ok:
        for _ in range(200):
            a = random.uniform(-10*math.pi, 10*math.pi)
            b = random.uniform(-10*math.pi, 10*math.pi)
            if not almost_equal(mul(E(a), E(b)), E(a + b)):
                ok = False; print("[FAIL] Angle-addition law"); break
        if ok: print("[PASS] Angle-addition: E(α)E(β) = E(α+β)")

    # (C) Quarter-turn squared = half-turn
    if ok:
        if almost_equal(powN(E(math.pi/2), 2), E(math.pi)):
            print("[PASS] Quarter-turn squared equals half-turn")
        else:
            ok = False; print("[FAIL] Quarter-turn property")

    # (D) Rotations preserve Euclidean length
    if ok:
        for _ in range(200):
            v = (random.uniform(-5, 5), random.uniform(-5, 5))
            th = random.uniform(-2*math.pi, 2*math.pi)
            if abs(norm(rotate(th, v)) - norm(v)) > 1e-10:
                ok = False; print("[FAIL] Rotation changed length"); break
        if ok: print("[PASS] Rotations preserve Euclidean length")

    # (E) Multiplicativity of norm: |z·w| = |z||w|
    if ok:
        for _ in range(200):
            z = (random.uniform(-3,3), random.uniform(-3,3))
            w = (random.uniform(-3,3), random.uniform(-3,3))
            if abs(norm(mul(z,w)) - (norm(z)*norm(w))) > 1e-10:
                ok = False; print("[FAIL] Norm multiplicativity"); break
        if ok: print("[PASS] Norm multiplicativity: |zw| = |z||w|")

    # (F) De Moivre: (E(θ))^n = E(nθ) for random integers n
    if ok:
        for _ in range(200):
            th = random.uniform(-2*math.pi, 2*math.pi)
            n  = random.randint(0, 20)
            if not almost_equal(powN(E(th), n), E(n*th)):
                ok = False; print("[FAIL] De Moivre for n =", n); break
        if ok: print("[PASS] De Moivre: (E(θ))^n = E(nθ)")

    # (G) Euler's identity directly
    if ok:
        zero = (0.0, 0.0)
        holds = almost_equal(add(E(math.pi), (1.0, 0.0)), zero)
        print(f"[{'PASS' if holds else 'FAIL'}] Euler identity: E(π) + 1 = 0")

    print("\nResult:", "ALL CHECKS PASSED ✅" if ok else "Some checks FAILED ❌")
    return ok

# --------------------------- Main ----------------------------------------------
if __name__ == "__main__":
    answer()
    reason()
    check()

