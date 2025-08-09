# ============================================
# Explain-and-Check: The Liar as "outer limits of reason"
# ============================================
# L := "This sentence is false."
#
# Naïve classical reading (no exotic machinery):
#   Treat the truth predicate for this one sentence as collapsing to its value:
#   True(L) ≡ L. Then the liar schema becomes:
#       L  ≡  ¬L
#
# Reason why this marks a limit in classical logic:
#   • If L is True, then the right-hand side says L is False. Contradiction.
#   • If L is False, then the right-hand side says L is True. Contradiction.
#   There is no consistent classical assignment. The sentence is *undecidable* here
#   not because it is subtle, but because the assumptions force a clash.
#
# Takeaway:
#   Without changing the rules (e.g., restricting self-reference, stratifying truth,
#   or altering the logic), a paradox like the liar just sits at the boundary:
#   an "outer limit of reason" for the chosen system.
#
# This file prints that story and *checks* that no classical assignment satisfies L ≡ ¬L.

# -----------------------------
# EXPLAIN — the “reason why”
# -----------------------------

print("============================================")
print("The Liar — outer limits of reason (classical logic)")
print("============================================\n")

print("Sentence:")
print('  L := "This sentence is false."')
print("Naïve classical schema:")
print("  L ≡ ¬L\n")

print("Reason why it goes off the rails:")
print("  • Assume L is True  ⇒  ¬L is False  ⇒  L is False.  (Contradiction)")
print("  • Assume L is False ⇒  ¬L is True   ⇒  L is True.   (Contradiction)")
print("  No consistent truth value exists under these assumptions.\n")

print("Philosophical upshot (without changing the system):")
print("  We can frankly admit the paradox marks an 'outer limit of reason' *for this setup*—")
print("  a place where our ordinary, two-valued, self-referential talk breaks itself.\n")

# -----------------------------
# CHECK — brute-force classical test
# -----------------------------

def classical_models_for_liar():
    """
    Try both classical truth values for L in the constraint L == (not L).
    Return a list of satisfying assignments (should be empty).
    """
    models = []
    for L in (False, True):
        if L == (not L):
            models.append(L)
    return models

models = classical_models_for_liar()

print("Check (classical two-valued):")
if not models:
    print("  ✓ No satisfying assignment exists for L ≡ ¬L. (Unsatisfiable)")
else:
    print("  ✗ Unexpected satisfying assignment(s):", models)

print("\nConclusion:")
print("  In plain classical logic, the liar forces contradiction either way—")
print("  a precise, technical sense in which it lives at the 'outer limits of reason.' □")

