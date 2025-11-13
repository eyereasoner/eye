# ============================================
# The Barber Paradox as "outer limits of reason"
# ============================================
# Story:
#   In a certain town there is a barber who shaves all and only those
#   who do not shave themselves. Does the barber shave himself?
#
# Formalization:
#   Domain: people in the town. One individual constant b = "the barber".
#   Predicate S(x, y): "x shaves y".
#   Barber condition (axiom):
#       ∀y [ S(b, y) ↔ ¬S(y, y) ]      # all-and-only-those who do NOT shave themselves
#   Question:
#       S(b, b) ?
#
# Key instance (instantiate y := b):
#       S(b, b) ↔ ¬S(b, b)
#   Let p abbreviate the proposition p := S(b, b).
#
# Classical logic assumptions in play:
#   (A1) Bivalence: every proposition is either True or False (exhaustive & exclusive).
#   (A2) Non-contradiction: no proposition is both True and False.
#   (A3) Truth-functional biconditional: (P ↔ Q) is True iff P and Q have the same truth value.
#   (A4) Classical negation: ¬True = False and ¬False = True.
#
# Why this marks a limit *for this setup*:
#   From the axiom we get p ↔ ¬p. Under (A1–A4), both classical assignments for p explode:
#     • If p = True, then ¬p = True, contradiction.
#     • If p = False, then ¬p = True, so p must be True, contradiction.
#   Hence p ↔ ¬p is unsatisfiable in two-valued classical logic.
#
# Modern (consistency-preserving) reactions (context, not used in code):
#   - Disallow such self-referential "all-and-only" definitions (typing, domains, stratification).
#   - Clarify the intended domain/relations to block self-application.
#
# This file prints the story and *checks* that p ↔ ¬p is unsatisfiable.

# -----------------------------
# FORMAL CORE
# -----------------------------

def barber_constraint(p: bool) -> bool:
    """Truth of the constraint p ↔ ¬p in classical two-valued logic."""
    # Equivalent to (p == (not p)); expanded to mirror the logical form.
    return (p and (not p)) or ((not p) and p)

def classical_models_for_barber():
    """
    Try both classical truth values for p in p ↔ ¬p.
    Return a list of satisfying assignments (should be empty).
    """
    models = []
    for p in (False, True):
        if barber_constraint(p):
            models.append(p)
    return models

def explain_case(assumed_value: bool) -> list[str]:
    """
    Step-by-step reasoning from an assumed classical value of p = S(b,b).
    """
    steps = []
    if assumed_value:
        steps.append("Assume p is True (S(b,b)).")
        steps.append("From ∀y [S(b,y) ↔ ¬S(y,y)], instantiating y := b gives S(b,b) ↔ ¬S(b,b).")
        steps.append("Thus from p we infer ¬p. Contradiction (p ∧ ¬p).")
    else:
        steps.append("Assume p is False (¬S(b,b)).")
        steps.append("From S(b,b) ↔ ¬S(b,b), the right-hand side says ¬p, which matches the left.")
        steps.append("But then the biconditional forces p. Contradiction (p ∧ ¬p).")
    steps.append("Hence no classical truth value makes p ↔ ¬p true.")
    return steps

# -----------------------------
# OUTPUT — ANSWER / REASON / CHECK
# -----------------------------

line = "=" * 60
print(line)
print("The Barber Paradox — outer limits of reason (classical logic)")
print(line + "\n")

print("Setup:")
print("  Predicate S(x,y): 'x shaves y'.  Constant b: 'the barber'.")
print("  Barber condition (axiom):  ∀y [ S(b,y) ↔ ¬S(y,y) ]")
print("  Abbreviation: p := S(b,b)\n")

print("Key instance (y := b):")
print("  S(b,b) ↔ ¬S(b,b)   i.e.,   p ↔ ¬p\n")

print("Answer:")
print("  No classical truth-value assignment satisfies p ↔ ¬p.")
print("  Under the barber axiom with two-valued classical logic, p has no consistent value.\n")

print("Reason why:")
for step in explain_case(True):
    print("  • " + step)
print()
for step in explain_case(False):
    print("  • " + step)
print("\n  Takeaway:")
print("  Taken at face value, the barber condition self-destructs in classical logic.")
print("  Consistent formalisms avoid it by blocking self-application (typing, domain tweaks,")
print("  or curbing 'all-and-only' definitions).\n")

print("Check (harness):")
models = classical_models_for_barber()
print("  Trying p ∈ {False, True} against the constraint (p == (not p))...")
if not models:
    print("  ✓ Unsatisfiable: no assignment makes p ↔ ¬p true.")
else:
    print("  ✗ Unexpected satisfying assignment(s):", models)

# Optional: tiny truth table
print("\n  Truth table for p and (p == (not p)):")
print("    p      not p    (p == not p)")
for p in (False, True):
    print(f"    {str(p):5}  {str((not p)):6}  {str(p == (not p))}")
print("\nConclusion:")
print("  The barber axiom forces p ↔ ¬p at p = S(b,b), so there is no classical model —")
print("  a precise 'outer limit of reason' for this setup. □")

