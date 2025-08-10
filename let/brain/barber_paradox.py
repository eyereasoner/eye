# ============================================
# Explain-and-Check: The Barber Paradox
# ============================================
# Story:
#   In a certain town there is a barber who shaves all and only those
#   who do not shave themselves. Does the barber shave himself?
#
# Formalization (one individual b = the barber; predicate S(x,y) = "x shaves y"):
#   Axiom (the barber condition):
#       ∀y  [ S(b, y) ↔ ¬S(y, y) ]         (all and only those who do NOT shave themselves)
#
#   Question: S(b, b) ?
#
# Reason why this explodes in classical logic:
#   Instantiating y := b in the axiom gives:
#       S(b, b) ↔ ¬S(b, b).
#   Now check the two classical possibilities for p := S(b, b):
#     • If p is True, then the right side says ¬p is True, contradiction.
#     • If p is False, then the right side says ¬p is True, so p must be True, contradiction.
#   No classical truth value for p satisfies p ↔ ¬p.
#
# Takeaway:
#   The barber setup, taken at face value with ordinary (two-valued) logic,
#   sits at an *outer limit of reason* for that system. Consistent formalisms avoid
#   it by rejecting such self-referential definitions (e.g., restricting comprehension,
#   clarifying the domain, or disallowing the "only those" clause when it self-applies).
#
# This file prints that story and checks (brute force) that p ↔ ¬p is unsatisfiable.

# -----------------------------
# EXPLAIN — the “reason why”
# -----------------------------

print("============================================")
print("The Barber Paradox — outer limits of reason")
print("============================================\n")

print("Setup:")
print("  Predicate S(x,y): 'x shaves y'. Let b be the barber.")
print("  Barber condition:  ∀y [ S(b,y) ↔ ¬S(y,y) ]\n")

print("Key instance (y := b):")
print("  S(b,b) ↔ ¬S(b,b)\n")

print("Reason why it contradicts classical logic:")
print("  • If S(b,b) is True, then ¬S(b,b) is True — impossible.")
print("  • If S(b,b) is False, then ¬S(b,b) is True, so S(b,b) must be True — impossible.")
print("  No classical assignment satisfies S(b,b) ↔ ¬S(b,b).\n")

print("Philosophical upshot:")
print("  Without changing the rules, this definition self-destructs;")
print("  it marks an 'outer limit of reason' for plain two-valued self-reference.\n")

# -----------------------------
# CHECK — brute-force classical test
# -----------------------------

def models_for_p_equiv_not_p():
    """
    Try p ∈ {False, True} under the constraint p == (not p).
    Return list of satisfying assignments (should be empty).
    """
    sols = []
    for p in (False, True):
        if p == (not p):
            sols.append(p)
    return sols

solutions = models_for_p_equiv_not_p()

print("Check (classical two-valued):")
if not solutions:
    print("  ✓ Unsatisfiable: no truth value p makes p ↔ ¬p hold.")
else:
    print("  ✗ Unexpected satisfying assignment(s):", solutions)

print("\nConclusion:")
print("  The barber condition forces p ↔ ¬p at p = S(b,b), so there’s no classical model.")
print("  That’s precisely how the paradox lives at the 'outer limits of reason.' □")

