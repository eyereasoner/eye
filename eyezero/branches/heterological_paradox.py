# ============================================
# Explain-and-Check: Grelling–Nelson (heterological) paradox
# ============================================
# Background:
#   • Call an adjective AUTOL﻿OGICAL if it applies to itself as a word.
#       e.g., “English” is autological (the word is an English word).
#   • Call an adjective HETEROLOGICAL if it does NOT apply to itself.
#       e.g., “long” is (arguably) not long as a word ⇒ heterological.
#
# The paradoxical question:
#   Is the adjective “heterological” itself heterological?
#
# Minimal formalization (one-word self-application):
#   Let Applies(w) mean: “the adjective w applies to itself.”
#   Define  Het(w)  :=  ¬Applies(w).
#
#   Now take the specific word h := "heterological".
#   Two obvious constraints hold when we talk *about the same predicate* “heterological”:
#
#     (D)  Het(h)  =  ¬Applies(h)                      # definition of heterological
#     (S)  Applies(h)  =  Het(h)                        # because “heterological” applies to a word
#                                                       # exactly when that word is heterological;
#                                                       # specialize it to the word “heterological” itself
#
#   Combine (D) and (S):    Het(h) = ¬Applies(h)  and  Applies(h) = Het(h)
#                       ⇒   Het(h) = ¬Het(h).
#
#   That’s a straight L ↔ ¬L contradiction in classical two-valued logic.
#
# Takeaway:
#   The Grelling–Nelson setup locates an “outer limit of reason” for this kind of
#   unrestricted self-application in a two-valued setting. Avoiding the paradox
#   requires changing something (e.g., stratifying the language, restricting the
#   self-application schema, or moving to non-classical semantics).
#
# This file prints that story and CHECKS that the constraints (D)&(S) are unsatisfiable.

# -----------------------------
# EXPLAIN — the “reason why”
# -----------------------------

print("============================================")
print("Grelling–Nelson (heterological) — explain and check")
print("============================================\n")

print("Definitions:")
print("  Autological(w)  : the adjective w applies to itself.")
print("  Heterological(w): not Autological(w).\n")

print('Set h := "heterological".  Two constraints when talking about this very predicate:')
print("  (D)  Het(h) = ¬Applies(h)                   # definition of heterological")
print("  (S)  Applies(h) = Het(h)                    # 'heterological' applies to a word")
print("                                               # exactly when that word is heterological\n")

print("Reason why it explodes in classical logic:")
print("  From (D) and (S) we get  Het(h) = ¬Applies(h)  and  Applies(h) = Het(h),")
print("  hence  Het(h) = ¬Het(h).  No two-valued assignment satisfies that.  □\n")

print("Philosophical upshot:")
print("  As stated, the heterological notion self-destructs under plain self-application.")
print("  To avoid the clash, you must restrict the schema or change the semantics.\n")

# -----------------------------
# CHECK — brute-force classical test
# -----------------------------
# We model the two unknown booleans:
#   h := Het(h)          ∈ {False, True}
#   a := Applies(h)      ∈ {False, True}
#
# Constraints to satisfy:
#   (D) h == (not a)
#   (S) a == h
# If any (h,a) satisfies both, we found a classical model; otherwise unsatisfiable.

def satisfying_assignments():
    sols = []
    for h in (False, True):
        for a in (False, True):
            D = (h == (not a))  # definition
            S = (a == h)        # “applies” equals “is heterological” when the predicate is 'heterological'
            if D and S:
                sols.append((h, a))
    return sols

solutions = satisfying_assignments()

print("Check (classical two-valued):")
if not solutions:
    print("  ✓ Unsatisfiable: no (h, a) satisfies (D) and (S).")
else:
    print("  ✗ Unexpected satisfying assignment(s):", solutions)

print("\nConclusion:")
print("  The Grelling–Nelson heterological question reduces to h ≡ ¬h for h := Het('heterological').")
print("  In classical logic this has no model — a clean example of an 'outer limit of reason.' □")

