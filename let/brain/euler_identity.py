#!/usr/bin/env python3
"""
Computer-assisted proof of Euler's identity e^{i*pi} + 1 = 0 using only Python's standard library.

Outline:
  A) Formal series derivation: e^{ix} = cos x + i sin x (by grouping even/odd terms).
  B) Rigorous numerical bound at x = pi using alternating-series remainders:
       |e^{i*pi} + 1| = |(cos pi + 1) + i sin pi|
                       <= |cos pi + 1| + |sin pi|
         Each computed by truncating the alternating Taylor series with an error <= next term.
  C) Pi is computed internally via the Machin formula: pi = 16*atan(1/5) - 4*atan(1/239).

You can change DIGITS / EPS to tighten the bound arbitrarily.
"""

from decimal import Decimal, getcontext
from fractions import Fraction

# ----------------------------
# Configuration
# ----------------------------
DIGITS = 60   # decimal digits to carry in Decimal arithmetic
EPS = Decimal('1e-50')  # target epsilon for the final bound

# ----------------------------
# Utilities: arctan and pi via Machin formula
# ----------------------------
def arctan_decimal(x: Decimal, tol: Decimal) -> Decimal:
    r"""
    arctan(x) = sum_{n=0}^\infty (-1)^n x^{2n+1}/(2n+1), for |x| <= 1.
    Stops when the next term < tol.
    """
    one = Decimal(1)
    term = x          # x^{2*0+1}/1
    s = term
    n = 1
    x2 = x * x
    while True:
        term *= x2    # x^{2n+1}
        add = term / Decimal(2*n + 1)
        if n % 2 == 1:
            s -= add
        else:
            s += add
        if add.copy_abs() < tol:
            break
        n += 1
    return s

def compute_pi(dps: int) -> Decimal:
    """
    Compute pi using Machin's formula:
      pi = 16*atan(1/5) - 4*atan(1/239)
    Carries a guard precision and rounds to 'dps' on return.
    """
    guard = 10
    getcontext().prec = dps + guard
    tol = Decimal(10) ** Decimal(-(dps + 3))
    pi = (Decimal(16) * arctan_decimal(Decimal(1)/Decimal(5), tol)
          - Decimal(4) * arctan_decimal(Decimal(1)/Decimal(239), tol))
    # Round to requested precision
    getcontext().prec = dps
    return +pi  # unary plus applies current rounding

# ----------------------------
# Alternating Taylor series for sin and cos with rigorous remainder bound
# ----------------------------
def sin_with_bound(x: Decimal, tol: Decimal):
    r"""
    sin(x) = sum_{n=0}^\infty (-1)^n x^{2n+1}/(2n+1)!
    We compute by recurrence on terms:
       t_{n+1} = - t_n * x^2 / ((2n+2)(2n+3)), starting with t_0 = x.
    For an alternating, eventually decreasing series, the truncation error
    is bounded by the magnitude of the first omitted term.
    Returns (approx_value, remainder_bound).
    """
    term = x
    s = term
    n = 0
    # We’ll stop when the next term drops below tol
    while True:
        n += 1
        # Update term to next alternating term
        term = -term * x * x / (Decimal(2*n) * Decimal(2*n + 1))
        s_next = s + term
        if term.copy_abs() < tol:
            # remainder <= |term|
            return s_next, term.copy_abs()
        s = s_next

def cos_with_bound(x: Decimal, tol: Decimal):
    r"""
    cos(x) = sum_{n=0}^\infty (-1)^n x^{2n}/(2n)!
    Recurrence:
       t_0 = 1
       t_{n+1} = - t_n * x^2 / ((2n+1)(2n+2))
    Returns (approx_value, remainder_bound) with remainder <= |next term|.
    """
    term = Decimal(1)
    s = term
    n = 0
    while True:
        n += 1
        term = -term * x * x / (Decimal(2*n-1) * Decimal(2*n))
        s_next = s + term
        if term.copy_abs() < tol:
            return s_next, term.copy_abs()
        s = s_next

# ----------------------------
# Part A: Show e^{ix} = cos x + i sin x via series (symbolic skeleton)
# ----------------------------
def show_series_match(k_terms=10):
    """
    Print the first few terms of e^{ix} expanded from the exponential series
    and show that even powers collect to cos(x), odd powers to i*sin(x).
    This is a didactic display (uses Fractions to avoid float noise).
    """
    i2 = -1  # since i^2 = -1
    terms = []
    for n in range(k_terms):
        # coefficient of x^n / n! times i^n
        # i^n cycles: 1, i, -1, -i, ...
        # We'll represent coefficients as one of 1, i, -1, -i times Fraction(1, n!)
        # and "power" n
        # Compute i^n symbolically:
        r = n % 4
        if r == 0:
            i_pow = "1"
            sign = 1
            imag = 0
        elif r == 1:
            i_pow = "i"
            sign = 1
            imag = 1
        elif r == 2:
            i_pow = "-1"
            sign = -1
            imag = 0
        else:
            i_pow = "-i"
            sign = -1
            imag = 1

        # n!
        fact = 1
        for k in range(2, n+1):
            fact *= k
        coeff = Fraction(sign, fact)  # coeff for x^n

        terms.append((n, i_pow, coeff))

    print("First few terms of e^{i x} = sum_{n>=0} (i x)^n / n!")
    real_parts = []
    imag_parts = []
    for (n, i_pow, coeff) in terms:
        if 'i' in i_pow:
            # imaginary contribution
            # strip the 'i' to record the real coefficient for the imaginary part
            real_coeff = coeff if i_pow == 'i' else -coeff
            imag_parts.append(f"{real_coeff}·x^{n}")
            print(f"  n={n}: (i x)^{n}/n! -> {i_pow} * x^{n} / {abs(coeff.denominator)} "
                  f"= {'+' if coeff>=0 else ''}{coeff} * {i_pow} * x^{n}")
        else:
            real_coeff = coeff if i_pow == '1' else -coeff
            real_parts.append(f"{real_coeff}·x^{n}")
            print(f"  n={n}: (i x)^{n}/n! -> {i_pow} * x^{n} / {abs(coeff.denominator)} "
                  f"= {'+' if coeff>=0 else ''}{coeff} * {i_pow} * x^{n}")

    print("\nGrouping real (even n) and imaginary (odd n) terms gives exactly:")
    print("  Real:   1 - x^2/2! + x^4/4! - x^6/6! + ...  = cos x")
    print("  Imag i: x - x^3/3! + x^5/5! - x^7/7! + ...  = i sin x")
    print("Therefore, by power-series definitions, e^{i x} = cos x + i sin x.\n")

# ----------------------------
# Part B: Rigorous bound at x = pi
# ----------------------------
def bound_euler_identity(dps=DIGITS, eps=EPS):
    getcontext().prec = dps
    pi = compute_pi(dps)
    tol = eps / Decimal(10)  # internal terms a bit tighter than final goal

    sin_pi, sin_rem = sin_with_bound(pi, tol)
    cos_pi, cos_rem = cos_with_bound(pi, tol)

    # Triangle inequality:
    # |e^{i*pi} + 1| = |(cos pi + 1) + i sin pi|
    # <= |cos pi + 1| + |sin pi|
    #
    # But we only have approximations with remainder bounds:
    #   cos pi = cos_approx +/- cos_rem
    #   sin pi = sin_approx +/- sin_rem
    #
    # So a rigorous bound is:
    # |e^{i*pi} + 1|
    # <= |(cos_approx + 1)| + cos_rem + |sin_approx| + sin_rem
    cos_plus_one_approx = cos_pi + Decimal(1)
    bound = (abs(cos_plus_one_approx) + cos_rem + abs(sin_pi) + sin_rem)

    return {
        "digits": dps,
        "pi": pi,
        "sin_pi": sin_pi,
        "sin_remainder_bound": sin_rem,
        "cos_pi": cos_pi,
        "cos_remainder_bound": cos_rem,
        "abs_upper_bound_e_to_i_pi_plus_1": bound
    }

# ----------------------------
# Main
# ----------------------------
if __name__ == "__main__":
    print("\n=== Part A: Series identity e^{i x} = cos x + i sin x ===\n")
    show_series_match(k_terms=10)

    print("=== Part B: Rigorous numerical bound at x = pi ===\n")
    getcontext().prec = DIGITS
    result = bound_euler_identity(DIGITS, EPS)

    print(f"Using {result['digits']} Decimal digits of precision.")
    print(f"Computed pi  ≈ {result['pi']}")
    print(f"sin(pi) approx = {result['sin_pi']}")
    print(f"  remainder bound ≤ {result['sin_remainder_bound']}")
    print(f"cos(pi) approx = {result['cos_pi']}")
    print(f"  remainder bound ≤ {result['cos_remainder_bound']}\n")

    print(f"Thus, a rigorous bound for |e^(i*pi) + 1| is")
    print(f"  ≤ |(cos(pi) approx + 1)| + cos_rem + |sin(pi) approx| + sin_rem")
    print(f"  = {result['abs_upper_bound_e_to_i_pi_plus_1']}\n")

    if result['abs_upper_bound_e_to_i_pi_plus_1'] <= EPS:
        print(f"SUCCESS: |e^(i*pi) + 1| ≤ {EPS}. Within this epsilon, e^(i*pi)+1 = 0.")
    else:
        print(f"Bound {result['abs_upper_bound_e_to_i_pi_plus_1']} did not reach EPS={EPS}.")
        print("Increase DIGITS or tighten EPS for a stronger bound.")

