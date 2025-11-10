#!/usr/bin/env python3
"""
Equation solving mini-demo — ARC output (pure Python, no imports)
================================================================

We mirror:
  1) solve(x**2 - 1, x)
  2) solve(x**2 + 1, x)
  3) solve(x**2 - 3*x + 2, x)
  4) solve(cos(x)*(1 - 2*sin(x)), x)
  5) solve(2**(2*x) - 5*2**(x + 1) + 16, x)
  6) linsolve([3x + 2y − z − 1, 2x − 2y + 4z + 2, −x + y/2 − z], x, y, z)
  7) nonlinsolve([x*y − 1, 4x**2 + y**2 − 5], x, y)

For trig we give **general solutions**; for algebraic/exponential we list concrete roots.
Each case also runs a small **check harness** that substitutes the solution(s).

Conventions
-----------
• **Real vs complex:** We give real roots when the equation is real and has real
  solutions; for `x**2+1` we return the complex pair `±i`.

How we “check”
--------------
For each case we independently verify by substitution:
• algebraic/exponential: plug solutions back and show residuals are 0 (or ~0),
• trig: evaluate using small Taylor implementations of sin/cos,
• linear/nonlinear systems: substitute and show vector residuals (0, 0, …).

Implementation notes
--------------------
• No imports; tiny `sin_real`/`cos_real` use Taylor series with reduction modulo 2π.
• We use simple formatting and tiny helpers; everything else is plain arithmetic.
"""

# ---------------- tiny trig (for checks) ----------------

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

def cos_real(x: float) -> float:
    y = _mod_2pi(x)
    term = 1.0
    s = term
    y2 = y*y
    k = 1
    while True:
        term *= -y2 / ((2*k-1)*(2*k))
        s_old = s
        s += term
        if abs(s - s_old) < 1e-16 or k > 200:
            break
        k += 1
    return s

def close(a, b, tol=1e-10):
    if isinstance(a, complex) or isinstance(b, complex):
        return abs(a.real-b.real) <= tol and abs(a.imag-b.imag) <= tol
    return abs(a-b) <= tol

# ---------------- Case 1 ----------------

def case1():
    print("Case 1: solve(x**2 - 1, x)")
    roots = [-1, 1]
    print("Answer     :", roots)
    print("Reason why : factor x^2−1 = (x−1)(x+1) ⇒ roots x = ±1.")
    # Check
    ok = all(((r*r - 1) == 0) for r in roots)
    print("Check      : substitute →", [(r, r*r-1) for r in roots], " ; all zero?", ok)
    print()

# ---------------- Case 2 ----------------

def case2():
    print("Case 2: solve(x**2 + 1, x)")
    roots = [1j, -1j]
    print("Answer     :", roots)
    print("Reason why : x^2 = −1 ⇒ x = ±i (principal complex roots).")
    ok = all(((r*r + 1) == 0) for r in roots)
    print("Check      : substitute →", [(r, r*r+1) for r in roots], " ; all zero?", ok)
    print()

# ---------------- Case 3 ----------------

def case3():
    print("Case 3: solve(x**2 - 3*x + 2, x)")
    # factor: (x-1)(x-2)
    roots = [1, 2]
    print("Answer     :", roots)
    print("Reason why : x^2 − 3x + 2 = (x−1)(x−2) ⇒ x = 1, 2.")
    ok = all(((r*r - 3*r + 2) == 0) for r in roots)
    print("Check      : substitute →", [(r, r*r - 3*r + 2) for r in roots], " ; all zero?", ok)
    print()

# ---------------- Case 4 ----------------

def case4():
    print("Case 4: solve(cos(x)*(1 - 2*sin(x)), x)")
    print("Answer     : union of")
    print("             • cos(x)=0   ⇒  x = π/2 + kπ,  k∈ℤ")
    print("             • sin(x)=1/2 ⇒  x = π/6 + 2kπ  or  x = 5π/6 + 2kπ,  k∈ℤ")
    print("Reason why : product is zero iff one factor is zero; solve each trig equation.")
    # Check: test a few integers k
    ks = range(-2, 3)
    ok_cos = all(abs(cos_real(PI/2 + k*PI)) <= 1e-12 for k in ks)
    s1 = all(abs(sin_real(PI/6 + 2*k*PI) - 0.5) <= 1e-12 for k in ks)
    s2 = all(abs(sin_real(5*PI/6 + 2*k*PI) - 0.5) <= 1e-12 for k in ks)
    print(f"Check      : cos-branch ok? {ok_cos} ; sin-branch ok? {s1 and s2}")
    print()

# ---------------- Case 5 ----------------

def case5():
    print("Case 5: solve(2**(2*x) - 5*2**(x + 1) + 16, x)")
    print("Reason why : let t=2^x (>0). Then t^2 − 10t + 16 = 0 ⇒ t∈{8,2} ⇒ x∈{3,1}.")
    roots = [1, 3]
    print("Answer     :", roots)
    ok = all(close(2**(2*r) - 5*(2**(r+1)) + 16, 0.0) for r in roots)
    print("Check      : substitute →", [(r, 2**(2*r) - 5*(2**(r+1)) + 16) for r in roots], " ; all ≈0?", ok)
    print()

# ---------------- Case 6 ----------------

def case6():
    print("Case 6: linsolve([...], x, y, z)")
    print("System     :")
    print("  3x + 2y − z = 1")
    print("  2x − 2y + 4z = −2")
    print("  −x + (1/2)y − z = 0")
    # Solve quickly by substitution (from the third: x = y/2 − z)
    x = 1; y = -2; z = -2
    sol = (x, y, z)
    print("Answer     :", [sol])
    print("Reason why : substitute x=y/2−z into the first two, solve → (x,y,z)=(1,−2,−2).")
    # Check
    e1 = 3*x + 2*y - z - 1
    e2 = 2*x - 2*y + 4*z + 2
    e3 = -x + 0.5*y - z
    ok = (e1 == 0) and (e2 == 0) and (abs(e3) < 1e-15)
    print("Check      : residuals →", (e1, e2, e3), " ; all zero?", ok)
    print()

# ---------------- Case 7 ----------------

def case7():
    print("Case 7: nonlinsolve([x*y - 1, 4*x**2 + y**2 - 5], x, y)")
    print("Reason why : y=1/x (x≠0) ⇒ 4x^4 - 5x^2 + 1 = 0. Let u=x^2 ⇒ 4u^2-5u+1=0 ⇒ u∈{1,1/4}.")
    sols = [(1,1), (-1,-1), (1/2, 2), (-1/2, -2)]
    print("Answer     :", sols)
    ok = all(close(x*y,1.0) and close(4*x*x + y*y, 5.0) for (x,y) in sols)
    print("Check      : substitute →", [((x,y), (x*y-1, 4*x*x + y*y - 5)) for (x,y) in sols], " ; all ≈(0,0)?", ok)
    print()

# ---------------- driver ----------------

if __name__ == "__main__":
    print("Answer / Reason why / Check (harness)")
    print("======================================\n")
    case1()
    case2()
    case3()
    case4()
    case5()
    case6()
    case7()

