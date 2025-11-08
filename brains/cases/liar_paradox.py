# ============================================
# The Liar Paradox as "outer limits of reason"
# ============================================
# Problem setup:
#   L := "This sentence is false."
#
# Naïve classical reading (no exotic machinery):
#   - Treat the truth predicate for this very sentence transparently:
#       True(L) ≡ L
#   - Then the liar schema collapses to the constraint:
#       L  ≡  ¬L
#
# Classical logic assumptions in play:
#   (A1) Bivalence: every sentence is either True or False (exhaustive and exclusive).
#   (A2) Non-contradiction: no sentence is both True and False.
#   (A3) Truth-functional ≡ (iff):  (P ≡ Q) is True iff P and Q have the same truth value.
#   (A4) Negation ¬ flips truth values:  ¬True = False,  ¬False = True.
#
# Why this marks a limit in this setup:
#   Under (A1–A4) and the schema L ≡ ¬L, every possible classical assignment for L
#   leads to contradiction; thus the constraints are unsatisfiable. The sentence
#   does not receive a consistent classical truth value without changing the rules
#   (e.g., restricting self-reference, stratifying truth, paraconsistency, many-valued logics).
#
# This file prints the story with explicit steps and *checks* unsatisfiability.

# -----------------------------
# FORMAL CORE
# -----------------------------

def liar_constraint(L: bool) -> bool:
    """Return the truth of the constraint L ≡ ¬L under classical two-valued logic."""
    return (L and (not L)) or ((not L) and L)  # equivalent to (L == (not L))

def classical_models_for_liar():
    """
    Try both classical truth values for L in the constraint L ≡ ¬L.
    Return a list of satisfying assignments (should be empty).
    """
    models = []
    for L in (False, True):
        if liar_constraint(L):
            models.append(L)
    return models

def explain_case(assumed_value: bool) -> list[str]:
    """
    Produce a step-by-step explanation for the assumed classical value of L.
    """
    steps = []
    s = "True" if assumed_value else "False"
    ns = "False" if assumed_value else "True"
    steps.append(f"Assume L is {s}.")
    steps.append("Given the schema L ≡ ¬L, the right-hand side must match the left-hand side.")
    steps.append(f"So ¬L must be {s}. But by classical negation, ¬L is {ns}.")
    steps.append(f"Contradiction: ¬L cannot be both {s} and {ns}.")
    if assumed_value:
        steps.append("Equivalently: from L and (L ≡ ¬L) we infer ¬L; hence L ∧ ¬L.")
    else:
        steps.append("Equivalently: from ¬L and (L ≡ ¬L) we infer L; hence L ∧ ¬L.")
    return steps

# -----------------------------
# OUTPUT — ANSWER / REASON / CHECK
# -----------------------------

line = "=" * 60
print(line)
print("The Liar — outer limits of reason (classical logic)")
print(line + "\n")

print('Sentence:')
print('  L := "This sentence is false."\n')

print("Naïve classical schema:")
print("  True(L) ≡ L  ⇒  L ≡ ¬L\n")

print("Answer:")
print("  No classical truth-value assignment satisfies L ≡ ¬L.")
print("  Under the stated assumptions, the liar sentence has no consistent classical truth value.\n")

print("Reason why:")
for step in explain_case(True):
    print("  • " + step)
print()
for step in explain_case(False):
    print("  • " + step)
print("\n  Takeaway:")
print("  Without changing the rules (e.g., restricting self-reference, stratifying truth,")
print("  or altering the logic), the liar sits at an 'outer limit of reason' for this system.\n")

print("Check (harness):")
models = classical_models_for_liar()
print("  Trying L ∈ {False, True} against the constraint (L == (not L))...")
if not models:
    print("  ✓ Unsatisfiable: no assignment makes L ≡ ¬L true.")
else:
    print("  ✗ Unexpected satisfying assignment(s):", models)

# Optional: show the tiny truth table explicitly
print("\n  Truth table for L and (L == (not L)):")
print("    L      not L    (L == not L)")
for L in (False, True):
    print(f"    {str(L):5}  {str((not L)):6}  {str(L == (not L))}")
print("\nConclusion:")
print("  In plain classical logic, assuming L ≡ ¬L forces contradiction either way—")
print("  a precise, technical sense in which the liar marks an 'outer limit of reason.' □")

