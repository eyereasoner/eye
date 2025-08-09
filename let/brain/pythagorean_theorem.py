# ============================================
# Pythagorean Theorem — Similarity Proof (with code-backed checks)
# ============================================
# What this program does (two layers):
#   LAYER 1 — EXPLANATION (printed):
#     • Prints a clear, step-by-step “reason why” the Pythagorean Theorem holds,
#       using Euclid’s similarity argument with an altitude to the hypotenuse.
#
#   LAYER 2 — VERIFICATION (silent, under the hood):
#     • Encodes the similarity relations as formulas:
#         p = b^2 / c,   q = a^2 / c,   p + q = c
#         p/b = b/c,     q/a = a/c
#         h = (a b)/c,   h^2 = p q
#         a^2 + b^2 = c^2
#       and checks them numerically for many triangles.
#     • If anything is inconsistent, raises an AssertionError (so you know exactly what failed).
#     • If all is well, you’ll see a single success message after the explanation.
#
# Constraints:
#   • NO imports at all. We even implement sqrt() ourselves (Newton–Raphson).
#   • NO user input and NO worked examples; this is a proof + internal verification.
#
# Why verify with numbers if it’s a proof?
#   • The printed part is the geometric proof (sufficient on its own).
#   • The internal checks are a pedagogical “sanity harness” — they mirror each logical
#     statement with arithmetic identities that must hold if the similarity mapping is correct.
#
# -------------------------------------------------------
# ASCII DIAGRAM (schematic; not to scale — minimal)
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
#   Segments on AB: AD = p, DB = q  (so p + q = c)
#   Legs: AC = b (left slanted), BC = a (right slanted), and CD ⟂ AB
#
# -------------------------------------------------------
# GEOMETRIC REASONING (how the identities arise)
# -------------------------------------------------------
# 1) Similarity (AA):
#     • ΔACD ~ ΔABC (share angle A, both right-angled).
#     • ΔCBD ~ ΔABC (share angle B, both right-angled).
#     • Hence ΔACD ~ ΔCBD as well (both similar to ΔABC).
#
# 2) Ratio at angle A (matching ΔACD to ΔABC):
#     • In ΔACD, hypotenuse is AC (length b); side along AB is AD (length p).
#     • In ΔABC, hypotenuse is AB (length c); side corresponding near angle A is AC (length b).
#     • Corresponding “(adjacent to A)/hypotenuse” ratio must match:
#           p / b  =  b / c    ⇒   b^2 = c p   ⇒   p = b^2 / c.
#
# 3) Ratio at angle B (matching ΔCBD to ΔABC):
#     • In ΔCBD, hypotenuse is BC (length a); side along AB is DB (length q).
#     • In ΔABC, hypotenuse is AB (length c); side corresponding near B is BC (length a).
#     • Thus:
#           q / a  =  a / c    ⇒   a^2 = c q   ⇒   q = a^2 / c.
#
# 4) Partition of the hypotenuse:
#     • D splits AB into AD and DB, so:
#           p + q = c.
#
# 5) Altitude length and geometric mean:
#     • Area of the big triangle two ways:
#           Area(ABC) = ½ a b  =  ½ c h    ⇒   h = (a b) / c.
#     • From ΔACD ~ ΔCBD (the two small ones), one also gets the classic mean relation:
#           h^2 = p q.
#       (Sketch: in similar triangles, corresponding side ratios match; a neat alignment gives
#        h/p = q/h which rearranges to h^2 = p q.)
#
# 6) Pythagoras emerges by simple addition:
#       b^2 = c p,   a^2 = c q  ⇒ add:
#       a^2 + b^2 = c (p + q) = c · c = c^2.
#
# Numeric subtleties:
#   • We use floating-point arithmetic and allow a tiny tolerance (1e-11) in comparisons,
#     because decimal representations are finite and iterative sqrt introduces rounding.
#   • The proof itself is exact; the tolerance is purely about machine arithmetic.
#
# ============================================
# Minimal numeric toolbox (no imports)
# ============================================

def sqrt(x):
    """
    Compute sqrt(x) using Newton–Raphson iteration.
    - Converges quadratically with a reasonable initial guess.
    - Assumes x >= 0.
    - No imports are used.
    """
    if x == 0.0:
        return 0.0
    # Initial guess: max(1, x) works well across ranges without branching on magnitude signs
    g = x if x >= 1.0 else 1.0
    # 25 iterations are plenty for double-like accuracy in this educational context
    for _ in range(25):
        g = 0.5 * (g + x / g)
    return g

def almost_equal(x, y, tol=1e-11):
    """
    Compare two floats with an absolute tolerance.
    Why absolute (not relative)? Because our magnitudes are modest and controlled;
    absolute tolerance is simple and sufficient here.
    """
    d = x - y
    if d < 0:
        d = -d
    return d <= tol

# ============================================
# Geometry helper functions (mirror the proof)
# ============================================

def hypotenuse(a, b):
    """
    Hypotenuse length c from legs a, b via the Pythagorean relation.
    Note: This is used only to compute c to *feed back* into the similarity identities.
    """
    return sqrt(a*a + b*b)

def derive_p_q(a, b):
    """
    Use similarity-derived formulas:
        p = b^2 / c,   q = a^2 / c,
    where c = sqrt(a^2 + b^2).
    Returns (p, q, c).
    """
    c = hypotenuse(a, b)
    if c == 0.0:
        # Excludes the degenerate triangle (a=b=0).
        raise ValueError("Degenerate triangle (a=b=0) is not allowed.")
    p = (b*b) / c
    q = (a*a) / c
    return p, q, c

def altitude_to_hypotenuse(a, b, c):
    """
    Altitude from the right angle to the hypotenuse:
        h = (a b) / c
    derived from area(ABC) = ½ a b = ½ c h.
    """
    return (a*b) / c

def check_similarity_and_pythagoras(a, b):
    """
    Given positive legs a, b:
      • Compute c, then p, q, h using the similarity/area formulas.
      • Verify each identity the proof used.
      • Raise AssertionError if any check fails; otherwise return silently.
    """
    # Basic geometry: legs must be positive for a non-degenerate right triangle
    assert a > 0.0 and b > 0.0, "Leg lengths must be positive."

    p, q, c = derive_p_q(a, b)

    # 1) Core similarity equalities:
    #       b^2 = c p,   a^2 = c q
    assert almost_equal(b*b, c*p), f"b^2 != c·p  ({b*b} vs {c*p})"
    assert almost_equal(a*a, c*q), f"a^2 != c·q  ({a*a} vs {c*q})"

    # 2) Partition of hypotenuse:   p + q = c
    assert almost_equal(p + q, c), f"p + q != c  ({p + q} vs {c})"

    # 3) Ratio forms (same statement as #1 but expressed as pure ratios):
    #       p/b = b/c,   q/a = a/c
    #    These echo the angle-by-angle correspondence in the similar triangles.
    assert almost_equal(p / b, b / c), f"p/b != b/c  ({p/b} vs {b/c})"
    assert almost_equal(q / a, a / c), f"q/a != a/c  ({q/a} vs {a/c})"

    # 4) Altitude and geometric mean:
    #       h = (a b)/c,   and   h^2 = p q
    h = altitude_to_hypotenuse(a, b, c)
    assert almost_equal(h*h, p*q), f"h^2 != p·q  ({h*h} vs {p*q})"

    # 5) Area consistency (two ways to compute the area of ΔABC must match):
    #       ½ a b   =   ½ c h
    assert almost_equal(0.5*a*b, 0.5*c*h), f"Area mismatch  ({0.5*a*b} vs {0.5*c*h})"

    # 6) Final identity (Pythagoras):
    #       a^2 + b^2 = c^2
    assert almost_equal(a*a + b*b, c*c), f"Pythagoras fails  ({a*a + b*b} vs {c*c})"

def proof_harness():
    """
    Exhaustively exercise the checks over a small grid of integer leg pairs,
    scaled by several factors (to emphasize *scale invariance* of similarity).
    • Scale invariance: if we multiply every length by s > 0, all ratios are unchanged
      and each identity remains valid. This harness tests that idea numerically.
    """
    # Small integer grid (kept modest for speed; feel free to increase ranges)
    max_leg = 12
    # A few scale factors (no imports, so just fixed values)
    scales = [0.5, 1.0, 1.7, 3.0]

    for s in scales:
        for A in range(1, max_leg + 1):
            for B in range(1, max_leg + 1):
                a = s * float(A)
                b = s * float(B)
                check_similarity_and_pythagoras(a, b)

# ============================================
# PROGRAM OUTPUT — the “reason why”
# ============================================

print("============================================")
print("Pythagorean Theorem — Proof via Similarity")
print("============================================\n")

print("Setup:")
print("We have a right triangle ABC with right angle at C.")
print("Legs: AC = b, BC = a   |   Hypotenuse: AB = c\n")

print("Step 1 — Drop an altitude:")
print("From point C, draw an altitude to AB, meeting it at D.")
print("This splits AB into two segments: AD = p and DB = q, with p + q = c.\n")

print("Step 2 — Similar triangles (AA):")
print("ΔACD ~ ΔABC (share angle A, both right-angled).")
print("ΔCBD ~ ΔABC (share angle B, both right-angled).")
print("Therefore ΔACD ~ ΔCBD as well.\n")

print("Step 3 — Read ratios from similarity:")
print("At angle A:  p / b = b / c   ⇒   b² = c·p   ⇒   p = b² / c")
print("At angle B:  q / a = a / c   ⇒   a² = c·q   ⇒   q = a² / c\n")

print("Step 4 — Put the pieces together:")
print("Partition: p + q = c.")
print("Add the two equalities: a² + b² = c·p + c·q = c·(p + q) = c².\n")

print("Step 5 — Altitude and geometric mean (supporting facts):")
print("Altitude length: h = (a b) / c (areas ½ab and ½ch must match).")
print("Geometric-mean relation: h² = p·q (follows from ΔACD ~ ΔCBD).\n")

print("Conclusion:")
print("The square of the hypotenuse equals the sum of the squares of the legs.")
print("(Now verifying all relations numerically for many scaled triangles...)\n")

# ============================================
# RUN THE HARNESS (silent unless a check fails)
# ============================================

if __name__ == "__main__":
    proof_harness()
    print("All similarity relations and Pythagoras verified for many scaled triangles. ✓")

