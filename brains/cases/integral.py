"""
Closed-form definite integrals (no imports) with "explain-and-check"
====================================================================

What this file does
-------------------
• Reproduces (symbolically) the results of:
      integrate(exp(-x**2), (x, -oo, oo))               -> sqrt(pi)
      integrate(1/(1 + x**2), (x, -oo, oo))             -> pi
      integrate(sin(x)**3, (x, 0, 1))                   -> 2/3 - cos(1) + (cos(1)**3)/3

• Adds a proof harness for each case:
    1) "Reason why" (derivation/sketch).
    2) Independent numeric check (Simpson’s rule on a finite interval, plus tail
       bounds or substitutions), using homegrown exp/sin/cos/sqrt — no imports.

Conventions & numeric details
-----------------------------
• We keep π as a literal double (PI), compute √π with a Newton solver, and implement:
    - sin/cos via range reduction to [-π, π] + Taylor series (alternating).
    - exp via argument halving: exp(t) = exp(t/2)^2 until |t| ≤ 0.5, then Taylor.
    - Simpson’s composite rule (even subintervals).
• Gaussian tail bound:  ∫_t^∞ e^{-x^2} dx ≤ e^{-t^2}/(2t) for t>0 (so we can truncate).
• For ∫ 1/(1+x^2), we show: x = tan θ  ⇒ dx = sec^2θ dθ and 1/(1+tan^2θ)=cos^2θ, so
  integrand·dx = 1 dθ and limits −∞..∞ map to −π/2..π/2, giving area π exactly.

"""

# --------------------------- Tiny numeric toolbox ---------------------------

PI = 3.1415926535897932384626433832795028841971
TWO_PI = 2.0 * PI

def reduce_to_pi(x):
    """Range-reduce x to [-π, π] using float modulo (no imports)."""
    y = x % TWO_PI   # -> [0, 2π)
    if y > PI:
        y -= TWO_PI
    return y

def sin_t(x):
    """sin(x) via Taylor on [-π, π]."""
    y = reduce_to_pi(x)
    term = y
    s = term
    y2 = y * y
    k = 1
    while True:
        term *= -y2 / ((2*k)*(2*k+1))
        s_old = s
        s += term
        if abs(s - s_old) < 1e-16:
            break
        k += 1
        if k > 200:
            break
    return s

def cos_t(x):
    """cos(x) via Taylor on [-π, π]."""
    y = reduce_to_pi(x)
    term = 1.0
    s = term
    y2 = y * y
    k = 1
    while True:
        term *= -y2 / ((2*k-1)*(2*k))
        s_old = s
        s += term
        if abs(s - s_old) < 1e-16:
            break
        k += 1
        if k > 200:
            break
    return s

def exp_small(t):
    """exp(t) for |t| ≤ 0.5 via Taylor."""
    term = 1.0
    s = 1.0
    n = 1
    while True:
        term *= t / n
        s_old = s
        s += term
        if abs(s - s_old) < 1e-16:
            break
        n += 1
        if n > 200:
            break
    return s

def exp_t(t):
    """exp(t) with argument halving to keep |t| small (stable for large |t|)."""
    # Reduce magnitude by halving until small
    if abs(t) <= 0.5:
        return exp_small(t)
    # exp(t) = exp(t/2)^2
    half = exp_t(t * 0.5)
    return half * half

def sqrt_newton(a):
    """Simple Newton sqrt (a>0)."""
    if a <= 0:
        return 0.0
    x = a if a >= 1 else 1.0
    for _ in range(40):
        x = 0.5 * (x + a / x)
    return x

def simpson(f, a, b, n):
    """Composite Simpson (n even)."""
    if n % 2 == 1:
        n += 1
    h = (b - a) / n
    s = f(a) + f(b)
    # odd indices
    tot = 0.0
    x = a + h
    for i in range(1, n, 2):
        tot += f(x)
        x += 2*h
    s += 4.0 * tot
    # even indices
    tot = 0.0
    x = a + 2*h
    for i in range(2, n, 2):
        tot += f(x)
        x += 2*h
    s += 2.0 * tot
    return s * h / 3.0

def fmt(x):
    """Clean numeric format."""
    return f"{x:.15g}"

# --------------------------- The three integrals ----------------------------

def case_gaussian():
    expr = "integrate(exp(-x**2), (x, -oo, oo))"
    rhs_symbolic = "sqrt(pi)"

    print("="*72)
    print("Case:", expr)
    print("Reason why:")
    print("  Let I = ∫_{-∞}^{∞} e^{-x^2} dx. Then I^2 = ∬_{R^2} e^{-(x^2+y^2)} dx dy.")
    print("  Switch to polar: r≥0, θ∈[0,2π): ∬ e^{-r^2} r dr dθ = 2π * (1/2)∫_0^∞ e^{-u} du = π.")
    print("  Hence I = √π. (Take the positive root since integrand ≥ 0.)")

    # Numeric check: truncate to [-L, L] with tail bound
    L = 6.0
    f = lambda x: exp_t(-(x*x))
    approx = simpson(f, -L, L, n=1200)
    tail = exp_t(-(L*L)) / (2.0 * L)    # upper bound on each tail; two tails total:
    tail_total = 2.0 * tail
    sqrt_pi = sqrt_newton(PI)
    err = abs(approx - sqrt_pi)

    print("Check (numbers):")
    print("  Simpson on [-6,6]:", fmt(approx))
    print("  √π :", fmt(sqrt_pi))
    print("  |Δ|:", fmt(err))
    print("  Tail bound 2 * e^{-L^2}/(2L) with L=6:", fmt(tail_total), "(negligible)")
    print(f"{expr} = {rhs_symbolic}")

def case_cauchy():
    expr = "integrate(1/(1 + x**2), (x, -oo, oo))"
    rhs_symbolic = "pi"

    print("="*72)
    print("Case:", expr)
    print("Reason why:")
    print("  Substitute x = tan θ ⇒ dx = sec^2θ dθ and 1/(1+tan^2θ) = cos^2θ.")
    print("  Then integrand·dx = cos^2θ · sec^2θ dθ = 1 dθ.")
    print("  As x goes from −∞ to ∞, θ runs from −π/2 to π/2, so the area is π.")

    # Numeric check 1: direct Simpson on a large symmetric interval + tail bound
    # The tails satisfy ∫_{T}^{∞} 1/(1+x^2) dx = π/2 - arctan T ≤ 1/T.
    T = 1000.0
    f = lambda x: 1.0 / (1.0 + x*x)
    approx_trunc = simpson(f, -T, T, n=20000)
    tail_bound = 2.0 / T
    err1 = abs(approx_trunc - PI)

    # Numeric check 2: transformed integral becomes ∫_{-π/2}^{π/2} 1 dθ = π exactly;
    # we just report that as a consistency check with our PI constant.
    print("Check (numbers):")
    print("  Simpson on [-1000,1000]:", fmt(approx_trunc))
    print("  π :", fmt(PI))
    print("  |Δ|:", fmt(err1), "  (tail ≤", fmt(tail_bound), ")")
    print(f"{expr} = {rhs_symbolic}")

def case_sin_cubed():
    expr = "integrate(sin(x)**3, (x, 0, 1))"
    rhs_symbolic = "2/3 - cos(1) + cos(1)**3/3"

    print("="*72)
    print("Case:", expr)
    print("Reason why:")
    print("  Use sin^3 x = (3 sin x − sin 3x)/4.")
    print("  ∫ sin^3 x dx = -3/4 cos x + (1/12) cos 3x + C.")
    print("  Evaluate on [0,1]: 2/3 − cos(1) + (1/3)cos^3(1).")

    # Numeric check against Simpson
    f = lambda x: (sin_t(x))**3
    approx = simpson(f, 0.0, 1.0, n=2000)
    cos1 = cos_t(1.0)
    closed = (2.0/3.0) - cos1 + (cos1*cos1*cos1)/3.0
    err = abs(approx - closed)

    print("Check (numbers):")
    print("  Simpson on [0,1]:", fmt(approx))
    print("  Closed form with cos:", fmt(closed))
    print("  |Δ|:", fmt(err))
    print(f"{expr} = {rhs_symbolic}")

# --------------------------- Driver ---------------------------

if __name__ == "__main__":
    cases = [
        "integrate(exp(-x**2), (x, -oo, oo))",
        "integrate(1/(1 + x**2), (x, -oo, oo))",
        "integrate(sin(x)**3, (x, 0, 1))",
    ]

    # Run in the given order with explanations and checks
    case_gaussian()
    case_cauchy()
    case_sin_cubed()

