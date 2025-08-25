#!/usr/bin/env python3
"""
Derivative mini-suite — ARC output, pure Python (no imports)
===========================================================

(Updated)
• Case 2 now uses the proper 5-point central stencil for f'''.
• Case 3 now uses a robust diff_seq_value() to compose partials without
  lambda-capture bugs.

What this file does
-------------------
Reproduces the following snippets *by hand* and prints, for each:
  • Answer     – the derivative in closed form,
  • Reason why – which rule(s) justify it,
  • Check      – an independent numeric harness (finite differences).

Covered cases
-------------
1)  diff(cos(x), x)
2)  diff(x**4, x, x, x)
3)  diff(exp(x*y*z), x, y, y, z, z, z, z)
4)  diff((x + 1)*((x**2 + 2)*(x**3 + 3)), x)
5)  diff(x/x/x/x/x/x/x/x/x/x, x)                       # left-assoc
6)  diff(log∘log∘⋯∘log(x), x), 10 nested logs
7)  diff(x*x*x*x*x*x*x*x*x*x, x)
8)  Jacobian of [x**3 + 5*y**4 − 9*z, exp(x*y*z)] wrt [x,y,z]
9)  Hessian  of  x**3 + 5*y**4 − 9*z   wrt [x,y,z]
"""

# ────────────────────────── tiny real math ──────────────────────────

PI = 3.1415926535897932384626433832795028841971
TWO_PI = 2.0 * PI

def _mod_2pi(x):
    y = x % TWO_PI
    if y > PI:
        y -= TWO_PI
    return y

def exp_real(x: float) -> float:
    k, ax = 0, abs(x)
    while ax > 0.5:
        x *= 0.5
        ax *= 0.5
        k += 1
    term = 1.0
    s = 1.0
    n = 1
    while True:
        term *= x / n
        s_old = s
        s += term
        if abs(s - s_old) < 1e-16 or n > 200:
            break
        n += 1
    while k > 0:
        s *= s
        k -= 1
    return s

def ln_real(y: float) -> float:
    if y <= 0.0:
        raise ValueError("ln domain error")
    m, e = y, 0
    while m >= 2.0: m *= 0.5; e += 1
    while m < 1.0:  m *= 2.0; e -= 1
    t = m - 1.0
    ln_m = t - 0.5*t*t + (1/3)*t*t*t - 0.25*t*t*t*t
    LN2 = 0.6931471805599453094
    x = ln_m + e*LN2
    for _ in range(3):
        ex = exp_real(x)
        x -= (ex - y)/ex
    return x

def sin_real(x: float) -> float:
    y = _mod_2pi(x)
    term, s, y2, k = y, y, y*y, 1
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
    term, s, y2, k = 1.0, 1.0, y*y, 1
    while True:
        term *= -y2 / ((2*k-1)*(2*k))
        s_old = s
        s += term
        if abs(s - s_old) < 1e-16 or k > 200:
            break
        k += 1
    return s

# ────────────────────────── finite differences ──────────────────────────

def cd1(f, xs, i, h=1e-6):
    xs1 = xs[:]; xs1[i] += h
    xs2 = xs[:]; xs2[i] -= h
    return (f(*xs1) - f(*xs2)) / (2*h)

def cd2_mixed(f, xs, i, j, h=1e-4):
    xs_pp = xs[:]; xs_pp[i] += h; xs_pp[j] += h
    xs_pm = xs[:]; xs_pm[i] += h; xs_pm[j] -= h
    xs_mp = xs[:]; xs_mp[i] -= h; xs_mp[j] += h
    xs_mm = xs[:]; xs_mm[i] -= h; xs_mm[j] -= h
    return (f(*xs_pp) - f(*xs_pm) - f(*xs_mp) + f(*xs_mm)) / (4*h*h)

def cd2_pure(f, xs, i, h=1e-4):
    xs1 = xs[:]; xs1[i] += h
    xs2 = xs[:]; xs2[i] -= h
    return (f(*xs1) - 2*f(*xs) + f(*xs2)) / (h*h)

# NEW: stable composition of first-order partials (fixes the lambda bug)
def diff_seq_value(f, idxs, point, h=1e-5):
    """
    Evaluate ∂/∂x_{idxs[-1]} … ∂/∂x_{idxs[0]} f at `point`,
    composing central differences safely.
    """
    def g(*args): return f(*args)
    for axis in idxs:
        prev = g
        def make(prev_fn, ax):
            def wrapped(*args, prev_fn=prev_fn, ax=ax):
                return cd1(prev_fn, list(args), ax, h)
            return wrapped
        g = make(prev, axis)
    return g(*point)

# ────────────────────────── helpers ──────────────────────────

def close(a, b, tol=1e-8):
    return abs(a - b) <= tol * max(1.0, 1.0 + abs(a) + abs(b))

# ────────────────────────── CASES ──────────────────────────

def case1():
    print("Case 1: diff(cos(x), x)")
    print("Answer     : −sin(x)")
    print("Reason why : derivative of cos is −sin (definition via series or Euler’s formula).")
    x0 = 0.37
    lhs = (cos_real(x0 + 1e-6) - cos_real(x0 - 1e-6)) / (2e-6)
    rhs = -sin_real(x0)
    print("Check      : numeric d/dx cos(x) ≈ {:.12f},  −sin(x) = {:.12f},  ok? {}".format(lhs, rhs, close(lhs, rhs)))
    print()

def case2():
    print("Case 2: diff(x**4, x, x, x)")
    print("Answer     : 24·x")
    print("Reason why : power rule k times:  d/dx x^4=4x^3 → 12x^2 → 24x.")
    # 5-point central stencil for third derivative:
    # f'''(x) ≈ [f(x−2h) − 2f(x−h) + 2f(x+h) − f(x+2h)] / (2 h^3)
    def f(x): return x**4
    x0, h = 1.234, 1e-3
    lhs = (f(x0-2*h) - 2*f(x0-h) + 2*f(x0+h) - f(x0+2*h)) / (2*h**3)
    rhs = 24*x0
    print("Check      : numeric ≈ {:.12f},  24x = {:.12f},  ok? {}".format(lhs, rhs, close(lhs, rhs, 1e-7)))
    print()

def case3():
    print("Case 3: diff(exp(x*y*z), x, y, y, z, z, z, z)")
    print("Answer     : (48 x^3 y^2 + 52 x^4 y^3 z + 14 x^5 y^4 z^2 + x^6 y^5 z^3) · exp(x y z)")
    print("Reason why : after ∂y∂y we get x^2 z^2 e^{xyz}; then ∂x gives (2xz^2 + x^2 y z^3)e^{xyz};")
    print("             each ∂z maps P↦∂P/∂z + (xy)P; after 4× you arrive at the displayed polynomial.")
    x0, y0, z0 = 0.2, 0.3, 0.4
    def F(x, y, z): return exp_real(x*y*z)
    seq = [0, 1, 1, 2, 2, 2, 2]   # x,y,y,z,z,z,z
    lhs = diff_seq_value(F, seq, [x0, y0, z0], h=5e-5)
    poly = (48*(x0**3)*(y0**2) + 52*(x0**4)*(y0**3)*z0 +
            14*(x0**5)*(y0**4)*(z0**2) + (x0**6)*(y0**5)*(z0**3))
    rhs = poly * exp_real(x0*y0*z0)
    print("Check      : numeric ≈ {:.12f},  closed form ≈ {:.12f},  ok? {}".format(lhs, rhs, close(lhs, rhs, 2e-6)))
    print()

def case4():
    print("Case 4: diff((x + 1)*((x**2 + 2)*(x**3 + 3)), x)")
    print("Answer     : (x^2+2)(x^3+3) + (x+1)(5x^4 + 6x^2 + 6x)")
    print("Reason why : product rule: g=(x+1), h=(x^2+2)(x^3+3); g' h + g h', with h' by product rule.")
    def f(x):
        return (x+1)*((x*x + 2)*(x**3 + 3))
    def fprime_formula(x):
        return (x*x+2)*(x**3+3) + (x+1)*(5*x**4 + 6*x**2 + 6*x)
    x0 = -0.7
    h = 1e-6
    lhs = (f(x0+h) - f(x0-h)) / (2*h)
    rhs = fprime_formula(x0)
    print("Check      : numeric ≈ {:.12f},  closed form ≈ {:.12f},  ok? {}".format(lhs, rhs, close(lhs, rhs)))
    print()

def case5():
    print("Case 5: diff(x/x/x/x/x/x/x/x/x/x, x)   # left-associative")
    print("Answer     : −8 / x^9")
    print("Reason why : ((((x/x)/x)/…)/x) = x^(1−9) = x^(−8); derivative is −8 x^(−9).")
    def f(x):
        t = x/x
        for _ in range(8):
            t = t / x
        return t
    x0 = 1.7
    h = 1e-6
    lhs = (f(x0+h) - f(x0-h)) / (2*h)
    rhs = -8.0 / (x0**9)
    print("Check      : numeric ≈ {:.12f},  closed form ≈ {:.12f},  ok? {}".format(lhs, rhs, close(lhs, rhs)))
    print()

def case6():
    print("Case 6: diff(log(log(…log(x)…)), x) with 10 nested logs")
    print("Answer     : 1 / [ x · log(x) · log(log x) · … · log^∘9(x) ]")
    print("Reason why : chain rule: d/dx log(L) = L'/L; apply repeatedly down the chain.")
    def L3(x):
        return ln_real(ln_real(ln_real(x)))
    def dL3_formula(x):
        return 1.0 / (x * ln_real(x) * ln_real(ln_real(x)))
    x0 = exp_real(exp_real(2.0))
    h = 1e-6
    lhs = (L3(x0+h) - L3(x0-h)) / (2*h)
    rhs = dL3_formula(x0)
    print("Check(n=3) : numeric ≈ {:.12f},  formula ≈ {:.12f},  ok? {}".format(lhs, rhs, close(lhs, rhs, 1e-7)))
    print("Note       : n=10 follows by the same recurrence; numerically it would overflow doubles.")
    print()

def case7():
    print("Case 7: diff(x*x*x*x*x*x*x*x*x*x, x)")
    print("Answer     : 10·x^9")
    print("Reason why : product/power rule: d/dx x^10 = 10 x^9.")
    def f(x): return x**10
    x0, h = -1.1, 1e-6
    lhs = (f(x0+h) - f(x0-h)) / (2*h)
    rhs = 10*(x0**9)
    print("Check      : numeric ≈ {:.12f},  closed form ≈ {:.12f},  ok? {}".format(lhs, rhs, close(lhs, rhs)))
    print()

def case8():
    print("Case 8: Jacobian of [x^3 + 5y^4 − 9z,  exp(x y z)] wrt [x,y,z]")
    print("Answer     : [[3x^2, 20y^3, −9],  [y z e^{xyz},  x z e^{xyz},  x y e^{xyz}]]")
    print("Reason why : row 1 is gradient of a polynomial; row 2 by chain rule on exp(xyz).")
    def F(x, y, z):
        return [(x**3 + 5*y**4 - 9*z),
                exp_real(x*y*z)]
    x0, y0, z0 = 0.8, 0.6, -0.4
    h = 1e-6
    J_num = [[0.0]*3 for _ in range(2)]
    for j in range(3):
        args_p = [x0, y0, z0]; args_p[j] += h
        args_m = [x0, y0, z0]; args_m[j] -= h
        Fp = F(*args_p); Fm = F(*args_m)
        for i in range(2):
            J_num[i][j] = (Fp[i] - Fm[i]) / (2*h)
    e = exp_real(x0*y0*z0)
    J_clo = [[3*x0*x0,           20*y0*y0*y0,   -9.0],
             [y0*z0*e,           x0*z0*e,       x0*y0*e]]
    ok = all(close(J_num[i][j], J_clo[i][j], 1e-7) for i in range(2) for j in range(3))
    print("Check      : numeric J ≈ {},  closed J ≈ {},  ok? {}".format(J_num, J_clo, ok))
    print()

def case9():
    print("Case 9: Hessian of f=x^3 + 5y^4 − 9z  wrt [x,y,z]")
    print("Answer     : [[6x, 0, 0],  [0, 60y^2, 0],  [0, 0, 0]]")
    print("Reason why : second derivatives of monomials; cross terms vanish (separable).")
    def f(x, y, z): return x**3 + 5*y**4 - 9*z
    x0, y0, z0 = -0.3, 0.7, 0.2
    h = 1e-4
    H_num = [[0.0]*3 for _ in range(3)]
    for i in range(3):
        H_num[i][i] = cd2_pure(f, [x0,y0,z0], i, h)
    for i in range(3):
        for j in range(i+1,3):
            H_num[i][j] = H_num[j][i] = cd2_mixed(f, [x0,y0,z0], i, j, h)
    H_clo = [[6*x0, 0.0, 0.0],
             [0.0, 60*y0*y0, 0.0],
             [0.0, 0.0, 0.0]]
    ok = all(close(H_num[i][j], H_clo[i][j], 5e-6) for i in range(3) for j in range(3))
    print("Check      : numeric H ≈ {},  closed H ≈ {},  ok? {}".format(H_num, H_clo, ok))
    print()

# ────────────────────────── driver ──────────────────────────

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
    case8()
    case9()

