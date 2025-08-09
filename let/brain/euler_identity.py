# ============================================
# Euler's Identity — the "reason why" + proof harness
# ============================================
# This script:
#   • Prints a clean, step-by-step explanation that e^{iπ} + 1 = 0.
#   • Verifies under the hood (no imports) that e^{ix} = cos x + i sin x for several x,
#     and specifically that e^{iπ} + 1 ≈ 0 within tight tolerance.
#
# Two classic "reasons why":
# --------------------------------------------
# (A) Power-series identity:
#     exp(z)   = Σ_{n≥0}  z^n / n!
#     cos(x)   = Σ_{k≥0} (-1)^k x^{2k} / (2k)!
#     sin(x)   = Σ_{k≥0} (-1)^k x^{2k+1} / (2k+1)!
#   Put z = i x. Then
#     exp(i x) = Σ (i x)^n / n!
#               = Σ i^{2k} x^{2k}/(2k)! + Σ i^{2k+1} x^{2k+1}/(2k+1)!
#               = Σ (-1)^k x^{2k}/(2k)! + i Σ (-1)^k x^{2k+1}/(2k+1)!
#               = cos x + i sin x.
#   Plug x = π: exp(iπ) = cos π + i sin π = -1 + i·0 = -1 ⇒ exp(iπ)+1=0.
#
# (B) Differential-equation uniqueness:
#   f(x) = exp(i x) satisfies f'(x) = i f(x), f(0)=1.
#   g(x) = cos x + i sin x also satisfies g'(x) = i g(x), g(0)=1.
#   The IVP y'=i y, y(0)=1 has a unique solution, so f≡g, hence exp(i x)=cos x + i sin x,
#   and again exp(iπ) = -1.
#
# Numeric note: we’ll approximate series with ~30 terms and wrap angles to [-π, π]
# for stability. Tolerance is set to 1e-12.
#
# ============================================
# Minimal complex + series toolbox (no imports)
# ============================================

PI = 3.141592653589793

def complex_add(a, b):
    """Add complex numbers a=(ar,ai), b=(br,bi)."""
    return (a[0] + b[0], a[1] + b[1])

def complex_mul(a, b):
    """Multiply complex numbers a*b."""
    return (a[0]*b[0] - a[1]*b[1], a[0]*b[1] + a[1]*b[0])

def complex_scale(a, s):
    """Scale complex a by real s."""
    return (a[0]*s, a[1]*s)

def abs_real(x):
    return -x if x < 0.0 else x

def almost_equal_complex(a, b, atol=1e-12, rtol=1e-12):
    """
    Robust complex equality:
      |Δreal| <= atol + rtol*max(|a.real|, |b.real|)
      |Δimag| <= atol + rtol*max(|a.imag|, |b.imag|)
    Avoids taking a sqrt of tiny numbers (which can amplify error).
    """
    dx = a[0] - b[0]
    dy = a[1] - b[1]
    dx = abs_real(dx)
    dy = abs_real(dy)
    ar = abs_real(a[0]); br = abs_real(b[0])
    ai = abs_real(a[1]); bi = abs_real(b[1])
    return (dx <= atol + rtol*(ar if ar > br else br)) and \
           (dy <= atol + rtol*(ai if ai > bi else bi))

def sqrt_real(x):
    """Newton-Raphson for sqrt(x) with x>=0 (no imports)."""
    if x == 0.0:
        return 0.0
    g = x if x > 1.0 else 1.0
    for _ in range(25):
        g = 0.5*(g + x/g)
    return g

def wrap_angle(x):
    """Reduce x to [-π, π] to help Taylor convergence."""
    two_pi = 2.0*PI
    while x > PI:
        x -= two_pi
    while x < -PI:
        x += two_pi
    return x

def sin_taylor(x, terms=30):
    """sin(x) via Maclaurin series with incremental term update."""
    x = wrap_angle(x)
    term = x  # first term x^(1)/1!
    s = 0.0
    sign = 1.0
    n = 1.0  # current factorial index for denominator bookkeeping
    # We'll build terms by multiplying by x^2 / ((k+1)(k+2)) and flipping sign.
    for _ in range(terms):
        s += sign*term
        # next odd power / factorial
        term = term * x * x / ((n+1.0)*(n+2.0))
        sign = -sign
        n += 2.0
    return s

def cos_taylor(x, terms=30):
    """cos(x) via Maclaurin series with incremental term update."""
    x = wrap_angle(x)
    term = 1.0  # first term x^0 / 0!
    s = 0.0
    sign = 1.0
    n = 0.0
    for _ in range(terms):
        s += sign*term
        term = term * x * x / ((n+1.0)*(n+2.0))
        sign = -sign
        n += 2.0
    return s

def exp_complex_series(z, terms=30):
    """exp(z) = Σ z^n / n! using iterative term update."""
    s = (1.0, 0.0)     # sum starts at 1
    term = (1.0, 0.0)  # term for n=0
    for n in range(1, terms):
        # term_n = term_{n-1} * z / n
        term = complex_scale(complex_mul(term, z), 1.0/n)
        s = complex_add(s, term)
    return s

# ============================================
# Proof harness (silent on success)
# ============================================

def proof_harness():
    """
    Verify numerically that exp(i x) ≈ cos x + i sin x for several x,
    and that exp(iπ) + 1 ≈ 0.
    """
    i = (0.0, 1.0)
    xs = [0.0, 1.0, PI/3, PI/2, PI, -PI/2, 2.0]
    for x in xs:
        z = complex_scale(i, x)                    # i*x
        lhs = exp_complex_series(z, terms=40)      # e^{ix}
        rhs = (cos_taylor(x, 40), sin_taylor(x, 40))
        assert almost_equal_complex(lhs, rhs, 1e-12), f"exp(ix)≠cos+isin at x={x}: {lhs} vs {rhs}"

    # Check Euler's identity specifically: e^{iπ} + 1 = 0
    zpi = complex_scale(i, PI)
    e_ipi = exp_complex_series(zpi, terms=45)
    left = complex_add(e_ipi, (1.0, 0.0))
    assert almost_equal_complex(left, (0.0, 0.0), 1e-12), f"Euler identity off: {left}"

# ============================================
# Program output — the “reason why”
# ============================================

print("============================================")
print("Euler’s Identity — why e^{iπ} + 1 = 0")
print("============================================\n")

print("Reason A (power series):")
print("  exp(z) = Σ z^n/n!,  cos x = Σ (-1)^k x^{2k}/(2k)!,  sin x = Σ (-1)^k x^{2k+1}/(2k+1)!")
print("  Put z = i x. Then exp(i x) splits into even + odd parts, using i^2 = -1:")
print("    exp(i x) = cos x + i sin x.")
print("  With x = π:  exp(iπ) = cos π + i sin π = -1 + 0·i, hence exp(iπ) + 1 = 0.\n")

print("Reason B (ODE uniqueness):")
print("  f(x)=exp(i x) solves f' = i f with f(0)=1.")
print("  g(x)=cos x + i sin x also solves g' = i g with g(0)=1.")
print("  The initial-value problem has a unique solution ⇒ f ≡ g ⇒ exp(i x)=cos x + i sin x ⇒ exp(iπ)=-1.\n")

print("Conclusion:")
print("  Both arguments give exp(iπ) + 1 = 0. This links e, i, π, 1, and 0 in a single identity. □\n")

# Run harness
if __name__ == "__main__":
    proof_harness()
    print("Proof harness passed: exp(i x)≈cos x+i sin x for multiple x, and e^{iπ}+1≈0.")

