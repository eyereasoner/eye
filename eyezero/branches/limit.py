#!/usr/bin/env python3
"""
Limits mini-demo — ARC output (pure Python, no imports)
======================================================

We mirror:
  1) limit(sin(x)/x, x, oo)
  2) limit(1/x, x, 0)              # two-sided
  3) limit(1/x, x, 0, dir='-')     # left-hand

Conventions
-----------
• sin is implemented by a Taylor series with reduction mod 2π.
• “Answer” reports the mathematical limit (real analysis).
• “Check (harness)” evaluates carefully chosen sequences to illustrate behavior.
"""

# ---------------- tiny math: sin ----------------

PI = 3.1415926535897932384626433832795028841971
TWO_PI = 2.0 * PI

def _mod_2pi(x: float) -> float:
    y = x % TWO_PI
    if y > PI:
        y -= TWO_PI
    return y

def sin_real(x: float) -> float:
    y = _mod_2pi(x)
    term = y
    s = term
    y2 = y*y
    k = 1
    while True:
        term *= -y2 / ((2*k)*(2*k+1))
        s_old = s
        s += term
        if abs(s - s_old) < 1e-16 or k > 200:
            break
        k += 1
    return s

def fmt(x):  # nice float fmt
    return f"{x:.12g}"

# ---------------- Case 1 ----------------

def case1():
    print("Case 1: limit(sin(x)/x, x, oo)")
    print("Answer     : 0")
    print("Reason why : |sin x| ≤ 1 ⇒ |sin x|/|x| ≤ 1/|x| → 0 as x→∞ (Squeeze Theorem).")

    # Check (harness): sample large x; also show the bound 1/x
    print("Check (harness):")
    xs = [10.0, 100.0, 1_000.0, 10_000.0]
    print("   x           sin(x)/x          bound 1/x")
    for x in xs:
        val = sin_real(x) / x
        bnd = 1.0 / x
        print(f"  {fmt(x):>8}   {fmt(val):>14}   {fmt(bnd):>12}")
    print()

# ---------------- Case 2 (right-hand by default) ----------------

def case2():
    print("Case 2: limit(1/x, x, 0)   # default dir='+' (right-hand)")
    print("Answer     : +∞")
    print("Reason why : as x→0⁺, 1/x is positive and unbounded above.")
    print("Note       : the two-sided limit does not exist; specify dir='-'/dir='+' to be explicit.")

    # Check (harness): approach 0 from the right with x_n = 10^{-n}
    print("Check (harness):")
    print("   n        x_n (0⁺)     1/x_n")
    for n in range(1, 7):
        xp = 10.0**(-n)
        print(f"  {n:>2}   {fmt(xp):>12}  {fmt(1.0/xp):>12}")
    print()

# ---------------- Case 3 (left-hand) ----------------

def case3():
    print("Case 3: limit(1/x, x, 0, dir='-')")
    print("Answer     : −∞")
    print("Reason why : for x<0 near 0, 1/x is negative and unbounded below; magnitude → ∞.")

    # Check (harness): x_n = −10^{-n}
    print("Check (harness):")
    print("   n        x_n (0⁻)     1/x_n")
    for n in range(1, 7):
        x = -10.0**(-n)
        print(f"  {n:>2}   {fmt(x):>12}  {fmt(1.0/x):>12}")
    print()

# ---------------- driver ----------------

if __name__ == "__main__":
    print("Answer / Reason why / Check (harness)")
    print("======================================\n")
    case1()
    case2()
    case3()

