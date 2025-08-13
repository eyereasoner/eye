#!/usr/bin/env python3
# ============================================================
# Euler’s Identity — ARC-ified proof + deterministic harness
# ============================================================
# What you’ll see:
#   • Answer      — one-line claim.
#   • Reason why  — two classic derivations in plain language.
#   • Check       — a no-imports numeric harness that verifies
#                   e^{ix} = cos x + i sin x for several x and
#                   that e^{iπ} + 1 ≈ 0 within tight tolerance.
# ============================================================

# -----------------------------
# Minimal complex/series toolkit
# -----------------------------
PI = 3.141592653589793

def abs_real(x: float) -> float:
    return -x if x < 0.0 else x

def complex_add(a, b):
    return (a[0] + b[0], a[1] + b[1])

def complex_mul(a, b):
    return (a[0]*b[0] - a[1]*b[1], a[0]*b[1] + a[1]*b[0])

def complex_scale(a, s: float):
    return (a[0]*s, a[1]*s)

def almost_equal_complex(a, b, atol=1e-12, rtol=1e-12) -> bool:
    """
    Robust per-component check:
      |Δre| <= atol + rtol*max(|a.re|, |b.re|)
      |Δim| <= atol + rtol*max(|a.im|, |b.im|)
    """
    dx = abs_real(a[0] - b[0])
    dy = abs_real(a[1] - b[1])
    mr = (abs_real(a[0]) if abs_real(a[0]) > abs_real(b[0]) else abs_real(b[0]))
    mi = (abs_real(a[1]) if abs_real(a[1]) > abs_real(b[1]) else abs_real(b[1]))
    return (dx <= atol + rtol*mr) and (dy <= atol + rtol*mi)

def wrap_angle(x: float) -> float:
    """Map x to [-π, π] (helps Taylor convergence)."""
    two_pi = 2.0*PI
    while x > PI:
        x -= two_pi
    while x < -PI:
        x += two_pi
    return x

def sin_taylor(x: float, terms=32) -> float:
    x = wrap_angle(x)
    term = x      # x^1/1!
    s = 0.0
    sign = 1.0
    n = 1.0       # current odd factorial index
    for _ in range(terms):
        s += sign*term
        term = term * x * x / ((n+1.0)*(n+2.0))
        sign = -sign
        n += 2.0
    return s

def cos_taylor(x: float, terms=32) -> float:
    x = wrap_angle(x)
    term = 1.0    # x^0/0!
    s = 0.0
    sign = 1.0
    n = 0.0       # current even factorial index
    for _ in range(terms):
        s += sign*term
        term = term * x * x / ((n+1.0)*(n+2.0))
        sign = -sign
        n += 2.0
    return s

def exp_complex_series(z, terms=32):
    """exp(z) = Σ z^n / n! with iterative term update (no factorial storage)."""
    s = (1.0, 0.0)     # n=0
    term = (1.0, 0.0)  # z^0/0!
    for n in range(1, terms):
        term = complex_scale(complex_mul(term, z), 1.0/n)
        s = complex_add(s, term)
    return s

# -------------------
# Proof/harness runner
# -------------------
def run_harness():
    """
    Check exp(i x) ≈ cos x + i sin x on several angles and
    quantify the worst per-component error; then check Euler’s identity.
    """
    i = (0.0, 1.0)
    xs = [0.0, 0.5, 1.0, PI/6, PI/4, PI/3, PI/2, PI, -PI/2, -2.0]
    worst_re = 0.0
    worst_im = 0.0
    for x in xs:
        z = complex_scale(i, x)
        lhs = exp_complex_series(z, terms=48)
        rhs = (cos_taylor(x, 48), sin_taylor(x, 48))
        # Track max absolute component error
        er = abs_real(lhs[0] - rhs[0])
        ei = abs_real(lhs[1] - rhs[1])
        if er > worst_re: worst_re = er
        if ei > worst_im: worst_im = ei
        assert almost_equal_complex(lhs, rhs, 1e-12, 1e-12), f"exp(ix)≠cos+isin at x={x}: {lhs} vs {rhs}"

    # Specific check: e^{iπ} + 1 = 0
    zpi = complex_scale(i, PI)
    e_ipi = exp_complex_series(zpi, terms=56)
    left = complex_add(e_ipi, (1.0, 0.0))
    # Residual (max component magnitude)
    resid = (abs_real(left[0]) if abs_real(left[0]) > abs_real(left[1]) else abs_real(left[1]))
    assert almost_equal_complex(left, (0.0, 0.0), 1e-12, 1e-12), f"Euler identity off by {left}"

    return {"worst_re": worst_re, "worst_im": worst_im, "e_ipi_plus_1_resid": resid}

# ============
# ARC sections
# ============
print("Answer")
print("------")
print("e^{iπ} + 1 = 0.")
print()

print("Reason why")
print("----------")
print("A) Power series:")
print("   exp(z)=Σ z^n/n!,  cos x=Σ (-1)^k x^{2k}/(2k)!,  sin x=Σ (-1)^k x^{2k+1}/(2k+1)!")
print("   Put z=ix. Even/odd terms separate, using i^2=-1, giving exp(ix)=cos x + i sin x.")
print("   With x=π: exp(iπ)=cos π + i sin π = -1 + 0·i ⇒ exp(iπ)+1=0.")
print()
print("B) ODE uniqueness:")
print("   f(x)=exp(ix) solves f'=i f, f(0)=1.  g(x)=cos x + i sin x also solves g'=i g, g(0)=1.")
print("   The IVP y'=i y, y(0)=1 has a unique solution, hence f≡g and exp(ix)=cos x + i sin x.")
print("   Therefore exp(iπ)=-1. □")
print()

print("Check (harness)")
print("---------------")
stats = run_harness()
print("Validated exp(ix)=cos x + i sin x on multiple angles; Euler’s identity holds numerically.")
print(f"Worst component error over tests:  |ΔRe|≈{stats['worst_re']:.2e}, |ΔIm|≈{stats['worst_im']:.2e}")
print(f"Residual for e^(iπ)+1: ≈{stats['e_ipi_plus_1_resid']:.2e}  (target 0) ✓")

