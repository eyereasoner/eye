#!/usr/bin/env python3
"""
Aristotelian view of Euler's identity: treat complex numbers as plane magnitudes
(2D vectors) where multiplication = compose rotations+scalings. On the unit
circle, e^{iθ} is the rotation by angle θ: (cos θ, sin θ).

Program outputs:
1) Answer: evaluates e^{iπ} + 1 = 0 using the rotation view.
2) Reason: short geometric explanation.
3) Check: independent harness verifying the rotation model's laws.
"""

import math
import random

# ---------- 2D vectors interpreted as "a + b i" without using Python complex ----------
def add(z, w):
    a, b = z; c, d = w
    return (a + c, b + d)

def mul(z, w):
    # (a + bi)(c + di) = (ac - bd) + (ad + bc)i
    a, b = z; c, d = w
    return (a*c - b*d, a*d + b*c)

def conj(z): 
    a,b = z; return (a, -b)

def norm(z):
    a, b = z
    return math.hypot(a, b)

def almost_equal(z, w, tol=1e-12):
    return norm((z[0]-w[0], z[1]-w[1])) <= tol

def to_str(z, digits=10):
    a, b = z
    return f"{a:.{digits}f} {'+' if b>=0 else '-'} {abs(b):.{digits}f}·i"

# ---------- Geometric meanings ----------
def E(theta):
    """e^{iθ} as a unit rotation: point on the unit circle (cos θ, sin θ)."""
    return (math.cos(theta), math.sin(theta))

def rotate(theta, v):
    """Apply rotation by θ to vector v using the matrix [[cos,-sin],[sin,cos]]."""
    c, s = math.cos(theta), math.sin(theta)
    x, y = v
    return (c*x - s*y, s*x + c*y)

# ---------- Pretty printing ----------
def section(title):
    print("\n" + "="*len(title))
    print(title)
    print("="*len(title))

# ---------- 1) Answer ----------
def answer():
    section("Answer")
    z = E(math.pi)                # rotation by π (a half-turn)
    one = (1.0, 0.0)
    lhs = add(z, one)             # e^{iπ} + 1
    print("Interpretation: e^{iθ} is rotation by θ on the unit circle.")
    print(f"e^{ { 'iπ' } }  ≈  {to_str(z)}")
    print(f"e^{ { 'iπ' } } + 1 ≈ {to_str(lhs)}")
    print("Conclusion: e^{iπ} + 1 = 0 (within numerical tolerance).")

# ---------- 2) Reason ----------
def reason():
    section("Reason (geometric)")
    print(
        "On the unit circle, a rotation by θ maps (1,0) to (cos θ, sin θ).\n"
        "Composition of rotations adds angles, which matches multiplication:\n"
        "   E(α)·E(β) = E(α+β).\n"
        "Setting θ = π gives E(π) = (-1, 0); hence E(π) + (1,0) = (0,0)."
    )

# ---------- 3) Independent Check (harness) ----------
def check():
    section("Independent Check (harness)")
    ok = True
    tol = 1e-10

    # (A) Unit length: |E(θ)| = 1
    for _ in range(100):
        th = random.uniform(-10*math.pi, 10*math.pi)
        if abs(norm(E(th)) - 1.0) > tol:
            ok = False; print("[FAIL] |E(θ)| != 1"); break
    else:
        print("[PASS] Unit circle: |E(θ)| = 1 for random θ")

    # (B) Group law: E(α) * E(β) = E(α+β)
    if ok:
        for _ in range(100):
            a = random.uniform(-4*math.pi, 4*math.pi)
            b = random.uniform(-4*math.pi, 4*math.pi)
            if not almost_equal(mul(E(a), E(b)), E(a + b), tol):
                ok = False; print("[FAIL] E(α)E(β) != E(α+β)"); break
        if ok: print("[PASS] Angle-addition law holds")

    # (C) Quarter-turn squared = half-turn: E(π/2)^2 = E(π)
    if ok:
        q2 = mul(E(math.pi/2), E(math.pi/2))
        if almost_equal(q2, E(math.pi), tol):
            print("[PASS] Quarter-turn squared equals half-turn")
        else:
            ok = False; print("[FAIL] Quarter-turn property")

    # (D) Rotation preserves ordinary lengths in the plane
    if ok:
        for _ in range(100):
            v = (random.uniform(-3,3), random.uniform(-3,3))
            th = random.uniform(-2*math.pi, 2*math.pi)
            if abs(norm(rotate(th, v)) - norm(v)) > 1e-10:
                ok = False; print("[FAIL] Rotation changed vector length"); break
        if ok: print("[PASS] Rotations preserve Euclidean length")

    # (E) Euler's identity numerically
    if ok:
        zero = (0.0, 0.0)
        holds = almost_equal(add(E(math.pi), (1.0, 0.0)), zero, tol)
        print(f"[{'PASS' if holds else 'FAIL'}] Euler identity: E(π) + 1 = 0")

    print("\nResult:", "ALL CHECKS PASSED ✅" if ok else "Some checks FAILED ❌")
    return ok

if __name__ == "__main__":
    answer()
    reason()
    check()

