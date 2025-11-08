# ============================================
# Explain-and-Check: Bell/CHSH vs Local Realism
# ============================================
# Setup (CHSH):
#   Two parties, Alice and Bob, space-like separated.
#   Alice chooses setting A0 or A1; Bob chooses B0 or B1.
#   Outcomes are ±1.
#
# Local-Realist Hidden Variables (LRHV) assumptions:
#   • Realism: for each hidden state λ, outcomes are predetermined:
#         A0(λ), A1(λ) ∈ {±1},   B0(λ), B1(λ) ∈ {±1}.
#   • Locality: Alice's A_i(λ) is independent of Bob’s choice (and vice versa).
#   • Measurement independence: distribution ρ(λ) doesn't depend on the choices.
#
# CHSH expression:
#   S =  E(A0 B0) + E(A0 B1) + E(A1 B0) - E(A1 B1)
#
# Reason why LRHV ⇒ |S| ≤ 2 (the “why” in one line of algebra):
#   For any fixed λ (deterministic assignment),
#     s(λ) = A0B0 + A0B1 + A1B0 - A1B1
#          = A0(B0+B1) + A1(B0−B1).
#   Since B0±B1 ∈ {−2, 0, 2}, we have s(λ) ∈ {−2, 2} ⇒ |s(λ)| ≤ 2.
#   Averaging over λ (any ρ) cannot increase the bound (linearity/convexity),
#   hence   |S| ≤ 2   for all local-realist models.  □
#
# Quantum prediction for the singlet |Ψ−⟩:
#   E_QM(â, b̂) = −cos(θ_a − θ_b).
#   Choose standard CHSH angles (degrees):
#     A0=  0°,  A1= 90°,  B0= 45°,  B1= −45°.
#   Then:
#     E(A0,B0)=−cos(−45°)=−√2/2
#     E(A0,B1)=−cos( 45°)=−√2/2
#     E(A1,B0)=−cos( 45°)=−√2/2
#     E(A1,B1)=−cos(135°)= +√2/2
#   So   S_QM = (−√2/2) + (−√2/2) + (−√2/2) − (+√2/2) = −2√2,
#   hence |S_QM| = 2√2 ≈ 2.828 > 2.  □
#
# This script prints that story and CHECKS both sides:
#   1) Enumerate all deterministic LRHV assignments (16 total) ⇒ max |S| = 2.
#   2) Compute the quantum S with a small cos() and show |S| ≈ 2√2.
#
# No imports. No user input.

# -----------------------------
# Minimal math (no imports)
# -----------------------------
PI = 3.141592653589793

def sqrt_real(x):
    if x == 0.0:
        return 0.0
    g = x if x > 1.0 else 1.0
    for _ in range(25):
        g = 0.5*(g + x/g)
    return g

def wrap_pi(x):
    # reduce to [-pi, pi] for series stability
    two = 2.0*PI
    while x >  PI: x -= two
    while x < -PI: x += two
    return x

def cos_taylor(x, terms=30):
    # Maclaurin with incremental update
    x = wrap_pi(x)
    term = 1.0
    s = 0.0
    sign = 1.0
    n = 0.0
    for _ in range(terms):
        s += sign*term
        term = term * x * x / ((n+1.0)*(n+2.0))
        sign = -sign
        n += 2.0
    return s

def deg2rad(d): return d * (PI/180.0)
def absf(x): return -x if x < 0.0 else x

# -----------------------------
# LRHV side: enumerate all deterministic assignments
# -----------------------------
# Deterministic model = a 4-tuple (A0,A1,B0,B1) with each ∈ {−1,+1}.
# For each, s = A0B0 + A0B1 + A1B0 − A1B1, then S = sum_λ ρ(λ) s(λ).
# The maximum of a linear function over a simplex is at an extreme point,
# so checking all 16 assignments is sufficient to prove |S| ≤ 2.

def chsh_s_for_assignment(A0, A1, B0, B1):
    return A0*B0 + A0*B1 + A1*B0 - A1*B1

def enumerate_lrhv_extremes():
    values = []
    for A0 in (-1, +1):
        for A1 in (-1, +1):
            for B0 in (-1, +1):
                for B1 in (-1, +1):
                    s = chsh_s_for_assignment(A0, A1, B0, B1)
                    values.append(s)
    return values

# -----------------------------
# Quantum side: CHSH with singlet angles
# -----------------------------
# E_QM(a,b) = -cos(a-b). Angles in degrees:
A0_deg, A1_deg = 0.0,   90.0
B0_deg, B1_deg = 45.0, -45.0

def E_qm(deg_a, deg_b):
    return -cos_taylor(deg2rad(deg_a - deg_b))

def chsh_qm():
    e00 = E_qm(A0_deg, B0_deg)
    e01 = E_qm(A0_deg, B1_deg)
    e10 = E_qm(A1_deg, B0_deg)
    e11 = E_qm(A1_deg, B1_deg)
    S = e00 + e01 + e10 - e11
    return S, (e00, e01, e10, e11)

# -----------------------------
# EXPLAIN — the “reason why”
# -----------------------------
print("============================================")
print("Bell/CHSH — local realism vs quantum predictions")
print("============================================\n")

print("Local-realist hidden variables (LRHV) imply the CHSH bound:")
print("  For any deterministic λ:  s(λ) = A0B0 + A0B1 + A1B0 − A1B1")
print("                          = A0(B0+B1) + A1(B0−B1) ∈ {−2, 2}.")
print("  Averaging over λ cannot exceed 2 ⇒ |S| ≤ 2 in all LRHV models.  □\n")

print("Quantum prediction for the singlet (E = −cos angle-difference):")
print(f"  Angles: A0={A0_deg}°, A1={A1_deg}°, B0={B0_deg}°, B1={B1_deg}°")
print("  This gives |S| = 2√2 ≈ 2.828, violating the LRHV bound.  □\n")

# -----------------------------
# CHECK — LRHV enumeration & QM value
# -----------------------------
vals = enumerate_lrhv_extremes()
max_abs_lrhv = max(absf(v) for v in vals)
distinct_vals = sorted(set(vals))

S_qm, (e00, e01, e10, e11) = chsh_qm()
two_sqrt2 = 2.0*sqrt_real(2.0)

print("Classical (LRHV) check by enumeration:")
print(f"  Distinct s(λ) values over 16 assignments: {distinct_vals}")
print(f"  Max |s(λ)| = {max_abs_lrhv}  ⇒  any convex average obeys |S| ≤ 2.\n")

print("Quantum (singlet) check:")
print(f"  E(A0,B0)={e00:.12f}, E(A0,B1)={e01:.12f}, E(A1,B0)={e10:.12f}, E(A1,B1)={e11:.12f}")
print(f"  S_QM = {S_qm:.12f},   2√2 ≈ {two_sqrt2:.12f}\n")

# Assertions / harness
tol = 1e-12
assert max_abs_lrhv <= 2, "LRHV enumeration should never exceed 2."
assert absf(absf(S_qm) - two_sqrt2) <= 1e-9, "Quantum CHSH should equal 2√2 (within tolerance)."
assert absf(S_qm) > 2.0, "Quantum value must violate the LRHV bound."

print("Harness:")
print("  • LRHV extreme points yield s ∈ {−2, 2} ⇒ |S| ≤ 2 for any LRHV theory. ✓")
print("  • Quantum singlet with standard angles gives |S| = 2√2 > 2. ✓")
print("Conclusion:")
print("  The CHSH/Bell experiment rules out local-realist hidden-variable assignments.")
print("  Nature violates |S| ≤ 2; experiments find |S| ≈ 2.8, in line with quantum mechanics. □")

