# ============================================
# Explain-and-Check: Russell's Paradox
# ============================================
# Naïve Comprehension (informal schema):
#   “For any condition φ(x), there exists a set R = { x | φ(x) }.”
#
# Take φ(x) := x ∉ x. Then define:
#   R := { x | x ∉ x }     # "the set of all sets that do not contain themselves"
#
# The critical question:
#   Is R ∈ R ?
#
# Reason why this breaks naïve set theory (classically):
#   • Suppose R ∈ R. Then by definition of R, we must have R ∉ R. Contradiction.
#   • Suppose R ∉ R. Then R satisfies the condition for membership in itself,
#     so R ∈ R. Contradiction.
#   In symbols, letting r abbreviate “R ∈ R”, naïve comprehension forces:
#       r  ≡  ¬r
#   which is impossible in two-valued classical logic.
#
# Takeaway:
#   Russell’s paradox marks an “outer limit of reason” for naïve comprehension.
#   Modern set theories (e.g., ZF/ZFC) *restrict* comprehension (Separation/Replacement)
#   precisely to avoid this contradiction.
#
# This file prints that story and checks that r ≡ ¬r is unsatisfiable.

# -----------------------------
# EXPLAIN — the “reason why”
# -----------------------------

print("============================================")
print("Russell’s Paradox — outer limits of naïve comprehension")
print("============================================\n")

print("Setup:")
print("  R := { x | x ∉ x }   (the set of all sets that do not contain themselves)")
print("Question:")
print("  Is R ∈ R ?  (abbreviate r := 'R ∈ R')\n")

print("Reason why it explodes in classical logic:")
print("  • If r is True (R ∈ R), then by definition of R, R ∉ R — contradiction.")
print("  • If r is False (R ∉ R), then R meets its own membership condition, so R ∈ R — contradiction.")
print("  Therefore r ≡ ¬r has no classical solution.\n")

print("Philosophical upshot:")
print("  Naïve comprehension overreaches; restricting it (as in ZF/ZFC) avoids the paradox.")
print("  Without such restrictions, this is an 'outer limit of reason' for the system.\n")

# -----------------------------
# CHECK — brute-force classical test
# -----------------------------

def models_for_r_equals_not_r():
    """Try r ∈ {False, True} for the constraint r == (not r). Expect no solutions."""
    sols = []
    for r in (False, True):
        if r == (not r):
            sols.append(r)
    return sols

solutions = models_for_r_equals_not_r()

print("Check (classical two-valued):")
if not solutions:
    print("  ✓ Unsatisfiable: no truth value r makes r ≡ ¬r hold.")
else:
    print("  ✗ Unexpected satisfying assignment(s):", solutions)

print("\nConclusion:")
print("  Russell’s construction forces r ≡ ¬r under naïve comprehension,")
print("  so classical consistency fails — a precise 'outer limit of reason.' □")

