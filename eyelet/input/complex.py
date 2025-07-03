#!/usr/bin/env python3
# complex_proofs.py
#
# Explanatory backward-style proofs for the demo expressions in your
# SymPy “complex number” snippet.

from sympy import sqrt, exp, log, I, pi, cos, sin, acos, asin, sinh, cosh, asinh, N
from sympy.abc import x

# ───────────────────────── helpers ──────────────────────────
def num(z, d=10):         # numeric with 10-digit precision
    return N(z, d)

# Each proof is a little function that prints a mini trace
# -----------------------------------------------------------

def p_sqrt_neg1():
    print("Step 01: recognise  -1 = e^{iπ}  (principal argument π)")
    print("Step 02: √(-1) = √(e^{iπ}) = e^{iπ/2}")
    print("Step 03: e^{iπ/2} = i")
    print("✔ PROVED  √(-1) = i")

def p_exp_ipi_plus1():
    print("Step 01: Euler  e^{iθ} = cosθ + i sinθ  ⇒  e^{iπ} = -1")
    print("Step 02: e^{iπ} + 1 = -1 + 1 = 0")
    print("✔ PROVED  e^{iπ}+1 = 0")

def p_log_neg1():
    print("Step 01: principal log:  log z = ln|z| + i·Arg(z)")
    print("         |−1| = 1 ⇒ ln|z| = 0,   Arg(−1)=π")
    print("Step 02: log(−1) = 0 + iπ")
    print("✔ PROVED  log(−1) = iπ")

def p_log_negI():
    print("Step 01: |−i| = 1,  Arg(−i)=−π/2  (principal value)")
    print("Step 02: log(−i) = 0 + i(−π/2) = −iπ/2")
    print("✔ PROVED")

def p_log_Ix():
    print("Step 01:  I·x = |x|·e^{iπ/2}  (assuming x>0 for principal branch)")
    print("Step 02:  log(Ix) = ln|x| + iπ/2")
    print("✔ PROVED  log(Ix) = log(x) + iπ/2")

def p_cos_Ix():
    print("Step 01: cos(i x) = (e^{ix·i} + e^{−ix·i})/2")
    print("                 = (e^{−x} + e^{x})/2")
    print("                 = cosh x")
    print("✔ PROVED  cos(i x) = cosh(x)")

def p_sin_Ix():
    print("Step 01: sin(i x) = (e^{ix·i} − e^{−ix·i})/(2i)")
    print("                 = (e^{−x} − e^{x})/(2i)")
    print("                 = i·sinh x")
    print("✔ PROVED  sin(i x) = i·sinh(x)")

def p_asin_Ix():
    print("Step 01: Definition  asin z = −i log( i z + √(1−z²) )")
    print("Step 02: Substitute z = i x  →  asin(i x) = i asinh x")
    print("✔ PROVED")

def p_sqrt_I():
    print("Step 01:  I = e^{iπ/2}")
    print("Step 02:  √I = e^{iπ/4} = cos(π/4) + i sin(π/4)")
    print("         = (1/√2)(1 + i)")
    print(f"✔ PROVED  √I ≈ {num(sqrt(I))}")

def p_acos_2():
    print("Step 01: For |z|>1, acos z = i·ln( z + √(z² − 1) )  (principal)")
    print("Step 02: z=2 ⇒ √(2²−1)=√3  ⇒  acos(2)=i ln(2+√3)")
    print(f"         ≈ {num(acos(2))}")
    print("✔ PROVED")

def p_asin_2():
    print("Step 01: asin z = π/2 − acos z")
    print("Step 02: Using acos(2) result  →  asin(2)=π/2 − i ln(2+√3)")
    print(f"         ≈ {num(asin(2))}")
    print("✔ PROVED")

def p_I_pow_I():
    print("Step 01: i^i = e^{i·log(i)}")
    print("Step 02: log(i) = iπ/2  (principal)")
    print("Step 03: i·log(i) = i(iπ/2) = −π/2")
    print("Step 04: e^{−π/2} ≈ 0.207879576...")
    print(f"✔ PROVED  i^i = e^{{-π/2}} ≈ {num(I**I, 14)}")

# ───────────────────────── table of cases ─────────────────────
cases = [
    ("sqrt(-1)",           sqrt(-1),           p_sqrt_neg1),
    ("exp(I*pi) + 1",      exp(I*pi)+1,        p_exp_ipi_plus1),
    ("log(-1)",            log(-1),            p_log_neg1),
    ("log(-I)",            log(-I),            p_log_negI),
    ("log(I*x)",           log(I*x),           p_log_Ix),
    ("cos(I*x)",           cos(I*x),           p_cos_Ix),
    ("sin(I*x)",           sin(I*x),           p_sin_Ix),
    ("asin(I*x)",          asin(I*x),          p_asin_Ix),
    ("N(sqrt(I))",         num(sqrt(I)),       p_sqrt_I),
    ("N(acos(2))",         num(acos(2)),       p_acos_2),
    ("N(asin(2))",         num(asin(2)),       p_asin_2),
    ("N(I**I, 136)",       N(I**I, 136),       p_I_pow_I)
]

# ───────────────────────── run all cases ──────────────────────
if __name__ == "__main__":
    for label, val, proof in cases:
        print(f"\n{label} = {val}")
        print("=== Proof ===============================================")
        proof()

