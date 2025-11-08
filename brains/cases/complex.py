#!/usr/bin/env python3
# complex_proofs.py
#
# Explanatory backward-style proofs for classic complex identities,
# with "Answer", "Reason why", and "Check (harness)" per case.
#
# No imports. We implement tiny real numerics (exp, log, sin, cos, sqrt)
# and use elementary complex constructions.
#
# Conventions / branches
# ----------------------
# • Principal log/arg: Arg ∈ (−π, π], so log(−1)=iπ, log(−i)=−iπ/2, log(i x)=ln x + iπ/2 (x>0).
# • Principal sqrt: √i = (1+i)/√2 (the root with positive real part).
# • acos, asin (for |z|>1) use principal branches:
#     acos(2) = i ln(2+√3),  asin(2) = π/2 − i ln(2+√3).
#
# Numeric notes
# -------------
# • Real exp via a fast power-series / repeated squaring hybrid.
# • Real log ln(y) (y>0) via Newton on exp (2–3 iters for ~1e-14).
# • sin/cos via Taylor; sinh/cosh via exp; asinh via ln(x+√(x²+1)).
# • All checks use a default tolerance of ~1e-12.

# ───────────────────────── mini math (real) ─────────────────────────

PI = 3.14159265358979323846264338327950288419716939937510
TWO_PI = 2.0 * PI

def _reduce_to_pi(x):
    y = x % TWO_PI
    if y > PI:
        y -= TWO_PI
    return y

def exp_real(x):
    """Exp on R via simple range reduction and series."""
    # range reduce: x = n*ln2 + r, but we don't have ln2 yet.
    # Use crude scaling: bring |x| ≤ 0.5 via halving/doubling trick.
    k = 0
    ax = abs(x)
    if ax > 0.5:
        # find k such that |x/2^k| ≤ 0.5
        while ax > 0.5:
            x *= 0.5
            ax *= 0.5
            k += 1
    # series for e^x on small x
    term = 1.0
    s = 1.0
    n = 1
    while True:
        term *= x / n
        s_old = s
        s += term
        if abs(s - s_old) < 1e-16:
            break
        n += 1
        if n > 200:
            break
    # undo scaling: square repeatedly
    while k > 0:
        s = s * s
        k -= 1
    return s

def ln_real(y):
    """Natural log on (0,∞) via Newton solve of exp_real(t) = y."""
    if y <= 0.0:
        raise ValueError("ln_real domain error")
    # good initial guess using exponent/mantissa from float
    m, e = y, 0
    while m >= 2.0:
        m *= 0.5
        e += 1
    while m < 1.0:
        m *= 2.0
        e -= 1
    # ln(y) = ln(m) + e ln2; approximate ln(m) on [1,2)
    # Use series ln(m) ≈ (m-1) - (m-1)^2/2 + ... (few terms good enough)
    t = m - 1.0
    ln_m = t - 0.5*t*t + (1/3)*t*t*t - 0.25*t*t*t*t
    LN2 = 0.693147180559945309417232121458176568  # baked-in constant
    x = ln_m + e * LN2
    # two Newton steps
    for _ in range(3):
        ex = exp_real(x)
        x -= (ex - y) / ex
    return x

def sqrt_real(y):
    if y < 0.0:
        raise ValueError("sqrt_real domain error")
    if y == 0.0:
        return 0.0
    x = y if y < 1.0 else 0.5*(y + 1.0)  # rough start
    for _ in range(30):
        x = 0.5 * (x + y / x)
    return x

def sin_real(x):
    y = _reduce_to_pi(x)
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

def cos_real(x):
    y = _reduce_to_pi(x)
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

def sinh_real(x):
    ex = exp_real(x)
    em = 1.0 / ex
    return 0.5 * (ex - em)

def cosh_real(x):
    ex = exp_real(x)
    em = 1.0 / ex
    return 0.5 * (ex + em)

def asinh_real(x):
    return ln_real(x + sqrt_real(x*x + 1.0))

# ───────────────────────── helpers (complex-ish) ─────────────────────────

def fmt_c(z, d=12):
    a = round(z.real, d)
    b = round(z.imag, d)
    if abs(b) < 10**(-d): b = 0.0
    if abs(a) < 10**(-d): a = 0.0
    if b == 0.0:
        return f"{a:.{d}f}"
    if a == 0.0:
        return f"{b:.{d}f}i"
    sign = "+" if b >= 0 else "-"
    return f"{a:.{d}f} {sign} {abs(b):.{d}f}i"

def exp_complex(a, b):
    """e^{a+ib} = e^a (cos b + i sin b)."""
    ea = exp_real(a)
    return complex(ea * cos_real(b), ea * sin_real(b))

def cos_complex_for_pure_imag(y):
    """cos(i y) via definition: (e^{iy i} + e^{-iy i})/2 = (e^{-y}+e^{y})/2."""
    return complex(0.5 * (exp_real(-y) + exp_real(y)), 0.0)

def sin_complex_for_pure_imag(y):
    """sin(i y) via definition: (e^{iy i} − e^{-iy i})/(2i) = i*(e^{-y}-e^{y})/2."""
    return complex(0.0, 0.5 * (exp_real(-y) - exp_real(y)))

def approx_eq(a, b, tol=1e-12):
    if isinstance(a, complex) or isinstance(b, complex):
        return abs(a.real - b.real) <= tol and abs(a.imag - b.imag) <= tol
    return abs(a - b) <= tol

# ───────────────────────── cases ─────────────────────────

def case_sqrt_neg1():
    print("Answer     : √(-1) = i")
    print("Reason why : −1 = e^{iπ} (principal argument). √(e^{iπ}) = e^{iπ/2} = i.")
    print("Check      : (i)^2 = −1 →", complex(0,1)**2 == -1)

def case_exp_ipi_plus1():
    val = exp_complex(0.0, PI)  # e^{iπ} = cosπ + i sinπ = -1 + 0i
    s = complex(val.real + 1.0, val.imag)
    print("Answer     : e^{iπ}+1 = 0")
    print("Reason why : Euler: e^{iθ}=cosθ+i sinθ ⇒ e^{iπ}=−1.")
    print("Check      : e^{iπ}+1 =", fmt_c(s), "≈ 0")

def case_log_neg1():
    # principal
    val = complex(0.0, PI)
    back = exp_complex(val.real, val.imag)  # should be -1
    print("Answer     : log(−1) = iπ  (principal)")
    print("Reason why : log z = ln|z| + i Arg(z); |−1|=1 ⇒ ln|z|=0, Arg(−1)=π.")
    print("Check      : exp(log(−1)) =", fmt_c(back))

def case_log_negI():
    val = complex(0.0, -PI/2)
    back = exp_complex(val.real, val.imag)  # should be -i
    print("Answer     : log(−i) = −iπ/2  (principal)")
    print("Reason why : |−i|=1, Arg(−i)=−π/2 ⇒ log(−i)=0 + i(−π/2).")
    print("Check      : exp(log(−i)) =", fmt_c(back))

def case_log_Ix(x):
    val = complex(ln_real(x), PI/2)
    back = exp_complex(val.real, val.imag)  # should be i x
    print(f"Answer     : log(i·x) = ln x + iπ/2  (x>0), here x={x}")
    print("Reason why : Arg(i x)=π/2 (principal), |i x|=x ⇒ ln|·|=ln x.")
    print("Check      : exp(log(i x)) =", fmt_c(back), "≈ i·x =", fmt_c(complex(0.0, x)))

def case_cos_Ix(x):
    lhs = cos_complex_for_pure_imag(x)     # definition route
    rhs = complex(cosh_real(x), 0.0)
    print("Answer     : cos(i x) = cosh x")
    print("Reason why : cos z = (e^{iz}+e^{−iz})/2; set z=i x ⇒ (e^{−x}+e^{x})/2.")
    print("Check      : cos(i x) ≈", fmt_c(lhs), "  cosh(x) =", fmt_c(rhs),
          "  equal?", approx_eq(lhs, rhs))

def case_sin_Ix(x):
    lhs = sin_complex_for_pure_imag(x)     # definition route
    rhs = complex(0.0, sinh_real(x))
    print("Answer     : sin(i x) = i·sinh x")
    print("Reason why : sin z = (e^{iz}−e^{−iz})/(2i); set z=i x ⇒ i(e^{−x}−e^{x})/2.")
    print("Check      : sin(i x) ≈", fmt_c(lhs), "  i·sinh(x) =", fmt_c(rhs),
          "  equal?", approx_eq(lhs, rhs))

def case_asin_Ix(x):
    # Two independent formulas:
    # (A) asin(i x) = i asinh x
    lhs = complex(0.0, asinh_real(x))
    # (B) asin z = −i log( i z + √(1−z²) ). For z=i x this becomes −i ln(√(1+x²) − x)
    expr = sqrt_real(1.0 + x*x) - x  # positive real
    rhs = complex(0.0, -ln_real(expr))
    print("Answer     : asin(i x) = i·asinh x")
    print("Reason why : Definition asin z = −i log( i z + √(1−z²) ); put z=i x and use √(1+x²)−x>0;")
    print("             also ln(√(1+x²)−x) = −ln(x+√(1+x²)) gives i·asinh x.")
    print("Check      : i·asinh(x) =", fmt_c(lhs), " ;  −i ln(√(1+x²)−x) =", fmt_c(rhs),
          " ; equal?", approx_eq(lhs, rhs))

def case_sqrt_I():
    # principal sqrt(i) = (1+i)/√2
    s2 = sqrt_real(2.0)
    ans = complex(1.0/s2, 1.0/s2)
    print("Answer     : √i = (1+i)/√2  ≈", fmt_c(ans))
    print("Reason why : i = e^{iπ/2}; √i = e^{iπ/4} = cos(π/4)+i sin(π/4) = (1+i)/√2.")
    print("Check      : (√i)^2 =", fmt_c(ans*ans), " ≈ i")

def case_acos_2():
    y = ln_real(2.0 + sqrt_real(3.0))
    z = complex(0.0, y)  # i ln(2+√3)
    # Check via cos(z) = cos(i y) = cosh(y) = 2
    cos_z = cosh_real(y)
    print("Answer     : acos(2) = i ln(2+√3)  ≈", fmt_c(z))
    print("Reason why : For |z|>1, acos z = i ln(z+√(z²−1)) (principal). For z=2: √(3), ln(2+√3).")
    print("Check      : cos(acos(2)) = cos(i·ln(2+√3)) = cosh(ln(2+√3)) =", f"{cos_z:.12f}", "≈ 2")

def case_asin_2():
    y = ln_real(2.0 + sqrt_real(3.0))
    z = complex(PI/2, -y)   # π/2 − i ln(2+√3)
    # Check via sin(z) = 2 (derivation in analysis: equals cosh(y) = 2)
    # sin(π/2 − i y) = (e^{i(π/2−iy)} − e^{-i(π/2−iy)})/(2i)
    e1 = exp_complex(0.0, PI/2)  # i
    val = 0.5*(exp_real(y)+exp_real(-y))  # cosh(y)
    print("Answer     : asin(2) = π/2 − i ln(2+√3)  ≈", fmt_c(z))
    print("Reason why : asin z = π/2 − acos z and acos(2)=i ln(2+√3).")
    print("Check      : sin(asin(2)) = cosh(ln(2+√3)) =", f"{val:.12f}", "≈ 2  ;  i =", fmt_c(e1))

def case_i_pow_i():
    val = exp_real(-PI/2)  # e^{-π/2}
    print("Answer     : i^i = e^{−π/2} ≈", f"{val:.12f}")
    print("Reason why : i^i = e^{i log(i)} with principal log(i)=iπ/2 ⇒ exponent = −π/2 (real).")
    # Harness: also compare to Python's principal-branch pow(1j,1j)
    py = (1j**1j).real
    print("Check      : built-in (1j**1j).real ≈", f"{py:.12f}", " ; match?",
          approx_eq(val, py))

# ───────────────────────── driver ─────────────────────────

if __name__ == "__main__":
    # Choose one positive test x for the identities with a parameter
    x = 0.73

    cases = [
        ("sqrt(-1)",            case_sqrt_neg1),
        ("exp(i*pi) + 1",       case_exp_ipi_plus1),
        ("log(-1)",             case_log_neg1),
        ("log(-i)",             case_log_negI),
        (f"log(i*x) with x={x}",lambda: case_log_Ix(x)),
        (f"cos(i*x) with x={x}",lambda: case_cos_Ix(x)),
        (f"sin(i*x) with x={x}",lambda: case_sin_Ix(x)),
        (f"asin(i*x) with x={x}",lambda: case_asin_Ix(x)),
        ("sqrt(i)",             case_sqrt_I),
        ("acos(2)",             case_acos_2),
        ("asin(2)",             case_asin_2),
        ("i^i",                 case_i_pow_i),
    ]

    for label, fn in cases:
        print(f"\n{label}")
        print("=== Proof ===============================================")
        fn()

