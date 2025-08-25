#!/usr/bin/env python3
# ============================================
# Pythagorean Theorem — Similarity Proof (ARC-ified)
# ============================================
# What this program does (two layers):
#   LAYER 1 — EXPLANATION (printed):
#     • Prints a clear, step-by-step “reason why” the Pythagorean Theorem holds,
#       using Euclid’s similarity argument with an altitude to the hypotenuse.
#
#   LAYER 2 — VERIFICATION (silent harness with a short summary):
#     • Encodes the similarity relations as formulas:
#         p = b^2 / c,   q = a^2 / c,   p + q = c
#         p/b = b/c,     q/a = a/c
#         h = (a b)/c,   h^2 = p q
#         a^2 + b^2 = c^2
#       and checks them numerically across many triangles (multiple scales).
#     • If anything is inconsistent, raises an AssertionError; otherwise prints a compact
#       success report with worst-case absolute residuals.
#
# Constraints:
#   • NO imports. We implement sqrt() ourselves (Newton–Raphson).
#   • NO user input and NO worked examples; this is a proof + internal verification.
#
# -------------------------------------------------------
# ASCII DIAGRAM (schematic; not to scale)
# -------------------------------------------------------
#
#   A-----p-----D----------q------------B        (AB = c)
#    \          |                     /
#     \         |                   /
#      \        |                 /
#       \       |               /
#        \      |             /
#         \     |           /
#          \    |         /
#           \   |       /
#            \  |     /
#             \ |   /
#              \| /
#               C
#
# Segments on AB: AD = p, DB = q  (so p + q = c)
# Legs: AC = b (left slanted), BC = a (right slanted), and CD ⟂ AB
#
# -------------------------------------------------------
# GEOMETRIC REASONING (how the identities arise)
# -------------------------------------------------------
# 1) Similarity (AA):
#     • ΔACD ~ ΔABC (share angle A, both right-angled).
#     • ΔCBD ~ ΔABC (share angle B, both right-angled).
# 2) Read ratios from similarity:
#     • At A:  p / b = b / c  ⇒  b² = c·p  ⇒  p = b² / c.
#     • At B:  q / a = a / c  ⇒  a² = c·q  ⇒  q = a² / c.
# 3) Partition: p + q = c (D splits AB).
# 4) Altitude length & geometric mean:
#     • h = (a b)/c  (area ½ab = ½ch).
#     • h² = p q     (from ΔACD ~ ΔCBD).
# 5) Add the equalities:  a² + b² = c·p + c·q = c(p+q) = c².
#    This is the Pythagorean Theorem.
#
# Numeric notes:
#   • We use floating arithmetic and an absolute tolerance (1e-11).
#   • The proof is exact; tolerance only addresses round-off from sqrt().
# ============================================

# ============================================
# Minimal numeric toolbox (no imports)
# ============================================

def sqrt(x: float) -> float:
    """Newton–Raphson sqrt; assumes x >= 0."""
    if x == 0.0:
        return 0.0
    g = x if x >= 1.0 else 1.0
    for _ in range(25):
        g = 0.5 * (g + x / g)
    return g

def absf(x: float) -> float:
    return -x if x < 0.0 else x

def almost_equal(x: float, y: float, tol: float = 1e-11) -> bool:
    return absf(x - y) <= tol

# ============================================
# Geometry helpers (mirror the proof)
# ============================================

def hypotenuse(a: float, b: float) -> float:
    """c from legs a, b (used to feed similarity identities)."""
    return sqrt(a*a + b*b)

def derive_p_q(a: float, b: float):
    """Formulas from similarity: p = b²/c, q = a²/c; returns (p, q, c)."""
    c = hypotenuse(a, b)
    if c == 0.0:
        raise ValueError("Degenerate triangle (a=b=0) is not allowed.")
    return (b*b)/c, (a*a)/c, c

def altitude(a: float, b: float, c: float) -> float:
    """h = (a b)/c from equal-area argument."""
    return (a*b) / c

def check_all(a: float, b: float, tol: float = 1e-11) -> tuple:
    """
    Verify every identity used in the proof for a,b>0.
    Returns the worst absolute residual among all checks (for stats).
    """
    assert a > 0.0 and b > 0.0, "Leg lengths must be positive."

    p, q, c = derive_p_q(a, b)
    h = altitude(a, b, c)

    res = []

    # 1) Core similarity equalities
    res.append(absf(b*b - c*p))   # b^2 = c p
    res.append(absf(a*a - c*q))   # a^2 = c q

    # 2) Partition of hypotenuse
    res.append(absf((p + q) - c)) # p + q = c

    # 3) Ratio forms (same content as #1)
    res.append(absf((p / b) - (b / c)))  # p/b = b/c
    res.append(absf((q / a) - (a / c)))  # q/a = a/c

    # 4) Altitude & geometric mean
    res.append(absf((h*h) - (p*q)))      # h^2 = p q

    # 5) Area consistency
    res.append(absf(0.5*a*b - 0.5*c*h))  # ½ab = ½ch

    # 6) Pythagoras
    res.append(absf(a*a + b*b - c*c))    # a^2 + b^2 = c^2

    # All must be below tolerance
    for r in res:
        assert r <= tol, f"Identity residual {r:.3e} exceeds tolerance {tol:.1e}"

    return max(res), c

# ============================================
# Harness (grid of (a,b) and several scales)
# ============================================

def run_harness() -> dict:
    """
    Exercise the identities over many triangles and scales.
    Returns a dict with summary stats (tested, scales, worst residual).
    """
    max_leg = 12
    scales = [0.5, 1.0, 1.7, 3.0]
    tested = 0
    worst = 0.0

    for s in scales:
        for A in range(1, max_leg + 1):
            for B in range(1, max_leg + 1):
                a = s * float(A)
                b = s * float(B)
                w, _c = check_all(a, b)
                tested += 1
                if w > worst:
                    worst = w

    return {"tested": tested, "scales": len(scales), "worst": worst}

# ============================================
# ARC output
# ============================================

print("Answer")
print("------")
print("For any right triangle with legs a, b and hypotenuse c,")
print("the Pythagorean Theorem holds:  a² + b² = c².")
print()

print("Reason why")
print("----------")
print("Drop the altitude from the right angle to the hypotenuse, splitting it into")
print("segments of lengths p and q. By AA similarity:")
print("  p/b = b/c  ⇒  p = b²/c,      q/a = a/c  ⇒  q = a²/c,      and  p + q = c.")
print("Adding c·p and c·q gives  b² + a² = c(p+q) = c².")
print("The altitude satisfies  h = (ab)/c  and  h² = pq, consistent with the similarity.")
print()

print("Check (harness)")
print("---------------")
summary = run_harness()
print(f"Cases tested: {summary['tested']}  across {summary['scales']} scale factors.")
print(f"Worst absolute residual among all identities: {summary['worst']:.3e}")
print("All similarity relations and Pythagoras verified. ✔")

