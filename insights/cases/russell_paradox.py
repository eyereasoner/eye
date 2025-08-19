# ============================================
# Russell's Paradox as "outer limits of reason"
# ============================================
# Naïve Comprehension (informal schema):
#   For any condition φ(x), there exists a set R = { x | φ(x) }.
#
# Specialize φ(x) := x ∉ x. Define:
#   R := { x | x ∉ x }     # "the set of all sets that do not contain themselves"
#   Let r abbreviate the proposition “R ∈ R”.
#
# Classical logic + naïve comprehension force the constraint:
#   r  ≡  ¬r
#
# Assumptions in play:
#   (A1) Naïve comprehension: { x | φ(x) } exists for any condition φ.
#   (A2) Bivalence: every proposition is either True or False (exhaustive & exclusive).
#   (A3) Non-contradiction: no proposition is both True and False.
#   (A4) Truth-functional ≡ (iff): (P ≡ Q) is True iff P and Q have the same truth value.
#   (A5) Classical negation: ¬True = False and ¬False = True.
#
# Why this marks a limit *for this setup*:
#   Given (A1–A5), both possible classical assignments for r collapse to contradiction.
#   Hence r ≡ ¬r is unsatisfiable in two-valued classical logic; naïve comprehension
#   overgenerates and breaks consistency.
#
# Modern fix (context, not used in code):
#   ZF/ZFC restrict comprehension (via Separation/Replacement), thereby blocking the construction.
#
# This file prints the story and *checks* that r ≡ ¬r is unsatisfiable.

# -----------------------------
# FORMAL CORE
# -----------------------------

def russell_constraint(r: bool) -> bool:
    """Truth of the constraint r ≡ ¬r in classical two-valued logic."""
    # Equivalent to (r == (not r)); written longhand to mirror the logical form.
    return (r and (not r)) or ((not r) and r)

def classical_models_for_russell():
    """
    Try both classical truth values for r in the constraint r ≡ ¬r.
    Return a list of satisfying assignments (should be empty).
    """
    models = []
    for r in (False, True):
        if russell_constraint(r):
            models.append(r)
    return models

def explain_case(assumed_value: bool) -> list[str]:
    """
    Step-by-step explanation starting from an assumed classical value of r.
    """
    steps = []
    s = "True" if assumed_value else "False"
    ns = "False" if assumed_value else "True"

    if assumed_value:
        steps.append("Assume r is True, i.e., R ∈ R.")
        steps.append("By the definition of R = { x | x ∉ x }, membership in R requires ¬(R ∈ R).")
        steps.append("So from r we infer ¬r. Contradiction (r ∧ ¬r).")
    else:
        steps.append("Assume r is False, i.e., R ∉ R.")
        steps.append("But then R satisfies its own membership condition (x ∉ x), so R ∈ R.")
        steps.append("So from ¬r we infer r. Contradiction (r ∧ ¬r).")

    steps.append(f"In classical terms: r must be {s} to match ¬r, but ¬r is {ns} by negation.")
    return steps

# -----------------------------
# OUTPUT — ANSWER / REASON / CHECK
# -----------------------------

line = "=" * 60
print(line)
print("Russell’s Paradox — outer limits of naïve comprehension (classical logic)")
print(line + "\n")

print("Setup:")
print("  R := { x | x ∉ x }   (the set of all sets that do not contain themselves)")
print("  Abbreviation: r := 'R ∈ R'\n")

print("Naïve comprehension + classical reading yield:")
print("  r ≡ ¬r\n")

print("Answer:")
print("  No classical truth-value assignment satisfies r ≡ ¬r.")
print("  Under naïve comprehension and two-valued classical logic, r has no consistent value.\n")

print("Reason why:")
for step in explain_case(True):
    print("  • " + step)
print()
for step in explain_case(False):
    print("  • " + step)
print("\n  Takeaway:")
print("  Naïve comprehension overreaches; restricting it (as in ZF/ZFC) avoids the paradox.")
print("  Without such restrictions, Russell’s construction marks an 'outer limit of reason' for this system.\n")

print("Check (harness):")
models = classical_models_for_russell()
print("  Trying r ∈ {False, True} against the constraint (r == (not r))...")
if not models:
    print("  ✓ Unsatisfiable: no assignment makes r ≡ ¬r true.")
else:
    print("  ✗ Unexpected satisfying assignment(s):", models)

# Optional: show the tiny truth table explicitly
print("\n  Truth table for r and (r == (not r)):")
print("    r      not r    (r == not r)")
for r in (False, True):
    print(f"    {str(r):5}  {str((not r)):6}  {str(r == (not r))}")
print("\nConclusion:")
print("  Russell’s construction forces r ≡ ¬r from naïve comprehension,")
print("  so classical consistency fails — a precise 'outer limit of reason.' □")

