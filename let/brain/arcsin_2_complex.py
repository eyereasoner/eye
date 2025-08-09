# ============================================
# Why arcsin(2) is complex — the "reason why" + proof harness
# ============================================
# This script:
#   • Prints a clean, self-contained explanation of why arcsin(2) ∉ ℝ.
#   • Runs a small "proof harness" under the hood to confirm that arcsin(x)
#     is non-real for x > 1 using the analytic continuation formula.
#   • Uses NO imports; implements sqrt, ln, and basic complex arithmetic manually.
#
# -------------------------------------------------------
# Core idea in one line:
# -------------------------------------------------------
# The real sine function satisfies |sin(y)| ≤ 1 for all real y.
# Therefore, if arcsin(2) were real, we would have sin(y) = 2 for some real y,
# which is impossible. The arcsin function for |x| > 1 is defined via
# complex-analytic continuation:
#
#   arcsin(z) = -i * ln( i z + sqrt(1 - z^2) )
#
# For real x > 1, the term (1 - x^2) is negative, so its square root is imaginary,
# and the logarithm's argument is complex — hence arcsin(x) is complex.
#
# -------------------------------------------------------
# Reasoning in steps:
# -------------------------------------------------------
# 1) On ℝ, the sine function's range is [-1, 1]:
#       For all real y, -1 ≤ sin(y) ≤ 1.
#
# 2) If arcsin(2) were a real number θ, then sin(θ) = 2, which is impossible.
#    Therefore arcsin(2) cannot be real.
#
# 3) In complex analysis, arcsin is defined via the inverse sine identity:
#       arcsin(z) = -i * ln( i z + sqrt(1 - z^2) )
#    where sqrt and ln are taken as complex functions.
#
# 4) For x > 1 (real), we have 1 - x^2 < 0, so:
#       sqrt(1 - x^2) = i * sqrt(x^2 - 1)   (pure imaginary)
#    This makes the quantity inside the ln complex, hence the ln is complex,
#    and multiplying by -i preserves its complex nature.
#
# 5) In fact, for x > 1 real:
#       arcsin(x) = π/2 - i * ln( x + sqrt(x^2 - 1) )
#    which has a real part π/2 and a nonzero imaginary part — definitely non-real.
#
# Conclusion: arcsin(2) is complex.
#
# -------------------------------------------------------
# Minimal numeric toolbox (no imports)
# -------------------------------------------------------

PI = 3.141592653589793

def sqrt_real(x):
    """Square root for real x ≥ 0."""
    if x == 0.0:
        return 0.0
    g = x if x > 1.0 else 1.0
    for _ in range(25):
        g = 0.5 * (g + x / g)
    return g

def complex_add(a, b):
    """Add two complex numbers a=(ar,ai), b=(br,bi)."""
    return (a[0] + b[0], a[1] + b[1])

def complex_mul(a, b):
    """Multiply two complex numbers."""
    return (a[0]*b[0] - a[1]*b[1], a[0]*b[1] + a[1]*b[0])

def complex_sqrt(z):
    """
    Principal branch of complex sqrt.
    Formula: sqrt(z) = sqrt((|z|+x)/2) + sign(y)*i*sqrt((|z|-x)/2)
    """
    x, y = z
    r = sqrt_real(x*x + y*y)
    if r == 0.0:
        return (0.0, 0.0)
    u = sqrt_real((r + x)/2.0)
    v = sqrt_real((r - x)/2.0)
    if y < 0:
        v = -v
    return (u, v)

def complex_ln(z):
    """
    Principal branch of complex natural log: ln(z) = ln|z| + i·arg(z)
    """
    x, y = z
    modulus = sqrt_real(x*x + y*y)
    if modulus == 0.0:
        raise ValueError("Logarithm undefined for 0.")
    # argument in range (-pi, pi]
    arg = 0.0
    if x > 0:
        arg = atan2_approx(y, x)
    elif x < 0:
        if y >= 0:
            arg = atan2_approx(y, x) + PI
        else:
            arg = atan2_approx(y, x) - PI
    else:  # x == 0
        arg = PI/2 if y > 0 else -PI/2
    return (math_ln(modulus), arg)

def atan2_approx(y, x):
    """
    Approximate atan2(y, x) using a series expansion.
    Good enough for small harness checks (not production-precise).
    """
    if x > 0:
        return atan_taylor(y/x)
    elif x < 0 and y >= 0:
        return atan_taylor(y/x) + PI
    elif x < 0 and y < 0:
        return atan_taylor(y/x) - PI
    elif x == 0 and y > 0:
        return PI/2
    elif x == 0 and y < 0:
        return -PI/2
    else:
        return 0.0

def atan_taylor(t):
    """
    Approximate atan(t) using a few terms of the Taylor series.
    Converges for |t| ≤ 1; for |t| > 1, use atan(t) = pi/2 - atan(1/t).
    """
    if t == 0.0:
        return 0.0
    if t > 1.0:
        return PI/2 - atan_taylor(1.0/t)
    if t < -1.0:
        return -PI/2 - atan_taylor(1.0/t)
    # 5-term Taylor series for atan(t)
    t2 = t*t
    return t - (t*t2)/3 + (t*t2*t2)/5 - (t*t2*t2*t2)/7 + (t*t2*t2*t2*t2)/9

def math_ln(x):
    """Natural log for positive real x using iterative improvement."""
    # Rough guess using change of base from log2
    # Start guess g = log2(x) ~ via shifting
    import sys
    g = 0.0
    y = x
    while y > 2.0:
        y /= 2.0
        g += 0.6931471805599453  # ln(2)
    while y < 1.0:
        y *= 2.0
        g -= 0.6931471805599453
    # Newton iterations on f(h) = exp(h) - y0
    h = 0.0
    y0 = y
    for _ in range(20):
        eh = exp_taylor(h)
        h -= (eh - y0)/eh
    return g + h

def exp_taylor(t):
    """Approximate e^t with 10-term Taylor series."""
    term = 1.0
    res = 1.0
    for n in range(1, 11):
        term *= t / n
        res += term
    return res

def arcsin_complex(z):
    """
    arcsin(z) = -i * ln( i z + sqrt(1 - z^2) ), using our complex routines.
    z is a tuple (real, imag).
    """
    i_z = (-z[1], z[0])  # i*z
    z_sq = (z[0]*z[0] - z[1]*z[1], 2*z[0]*z[1])
    one_minus_zsq = (1 - z_sq[0], -z_sq[1])
    sqrt_part = complex_sqrt(one_minus_zsq)
    inside = complex_add(i_z, sqrt_part)
    ln_val = complex_ln(inside)
    # Multiply by -i:
    return (ln_val[1], -ln_val[0])

# -------------------------------------------------------
# Proof harness
# -------------------------------------------------------

def proof_harness():
    """
    Check that arcsin(x) for real x > 1 yields a nonzero imaginary part.
    """
    test_values = [1.1, 2.0, 3.0, 10.0]
    for x in test_values:
        val = arcsin_complex((x, 0.0))
        assert val[1] != 0.0, f"Imaginary part is zero for x={x}, got {val}"
    return True

# -------------------------------------------------------
# Program output: the proof, step by step
# -------------------------------------------------------

print("============================================")
print("Why arcsin(2) is complex — the 'reason why'")
print("============================================\n")

print("1) For any real y, -1 ≤ sin(y) ≤ 1.")
print("   So sin(y) = 2 has no real solution — arcsin(2) cannot be real.\n")

print("2) In complex analysis, arcsin is defined as:")
print("     arcsin(z) = -i · ln( i z + sqrt(1 - z²) )\n")

print("3) For z = 2 (real), we have 1 - z² = 1 - 4 = -3 < 0.")
print("   So sqrt(1 - z²) = i·sqrt(3), which is purely imaginary.")
print("   This makes the argument of ln complex, hence arcsin(2) is complex.\n")

print("4) In fact, for any real x > 1:")
print("     arcsin(x) = π/2 - i · ln( x + sqrt(x² - 1) )")
print("   which has nonzero imaginary part.\n")

print("Conclusion:")
print("   arcsin(2) is complex (has a nonzero imaginary part).\n")

# -------------------------------------------------------
# Run the proof harness (silent on success)
# -------------------------------------------------------

if __name__ == "__main__":
    proof_harness()
    print("Proof harness passed for x = 1.1, 2.0, 3.0, 10.0: all non-real arcsin(x) confirmed.")

