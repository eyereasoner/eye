# ============================================
# Disjunction Elimination (∨-Elim) — realistic, exhaustive example
# ============================================
# Scenario:
#   Today is either a WEEKDAY or a WEEKEND (mutually exclusive and exhaustive).
#   If it's a WEEKDAY, the museum is open today.
#   If it's a WEEKEND, the museum is open today.
#   Therefore, the museum is open today.
#
# Propositions:
#   Wd := "Today is a weekday"
#   We := "Today is a weekend"
#   Op := "The museum is open today"
#
# Premises (formal):
#   (P1)  Wd ∨ We                # exhaustive disjunction: every day is one or the other
#   (P2)  Wd → Op                 # if weekday, open
#   (P3)  We → Op                 # if weekend, open
#
# Goal (conclusion):
#   (C)   Op
#
# The “reason why” (Disjunction Elimination / Proof by Cases):
#   1) From (P1), exactly one case holds: Weekday or Weekend (exhaustive & exclusive).
#   2) CASE Weekday:  assume Wd. By (P2), infer Op.
#   3) CASE Weekend:  assume We. By (P3), infer Op.
#   4) Both cases yield Op, so we conclude Op unconditionally.   (∨-Elim)
#
# Notes:
#   • This is the standard rule schema:
#         From  (A ∨ B),  (A → C),  (B → C)  infer  C.
#     Here A := Wd, B := We, C := Op.
#   • We explicitly model the *exhaustiveness* by enforcing exactly-one-of {Wd, We}
#     in the proof harness below.
#
# No imports. No user input. Educational comments included.

# ---------------------------------
# Program output: the “reason why”
# ---------------------------------

print("============================================")
print("Disjunction Elimination — realistic, exhaustive example")
print("============================================\n")

print("Propositions:")
print("  Wd: Today is a weekday")
print("  We: Today is a weekend")
print("  Op: The museum is open today\n")

print("Premises:")
print("  (P1) Wd ∨ We            # exhaustive: it’s either weekday or weekend")
print("  (P2) Wd → Op            # if weekday, open")
print("  (P3) We → Op            # if weekend, open\n")

print("Derivation (proof by cases):")
print("  From (P1), exactly one of Wd or We holds (mutually exclusive & exhaustive).")
print("   • Case Weekday: assume Wd.  From (P2), infer Op.")
print("   • Case Weekend: assume We.  From (P3), infer Op.")
print("  Since both cases yield Op, conclude Op.   (∨-Elim)\n")

print("Conclusion:")
print("  (C) Op — The museum is open today.\n")

# ---------------------------------
# Proof harness (semantic check)
# ---------------------------------
# We check that for every truth assignment where:
#   • exactly one of {Wd, We} is true (exhaustive & exclusive),
#   • and (P2) and (P3) hold,
# the conclusion Op must be true.
#
# Propositional semantics:
#   implication p→q is (¬p) ∨ q

def implies(p, q):
    return (not p) or q

def xor(a, b):
    return (a and not b) or (b and not a)

def harness():
    models_scanned = 0
    satisfying = 0
    for Wd in (False, True):
        for We in (False, True):
            # Enforce exhaustive & exclusive split of the days
            if not xor(Wd, We):
                continue

            for Op in (False, True):
                models_scanned += 1

                P1 = (Wd or We)        # holds automatically if xor holds
                P2 = implies(Wd, Op)
                P3 = implies(We, Op)

                if P1 and P2 and P3:
                    satisfying += 1
                    # Then Op must be True (by ∨-Elim structure)
                    assert Op, "Counterexample: premises hold but Op is false."

    return models_scanned, satisfying

if __name__ == "__main__":
    total, sat = harness()
    print(f"Harness: scanned {total} exhaustive/exclusive day-cases; {sat} satisfy (P1)&(P2)&(P3).")
    print("All satisfying cases make Op True.  ∨-Elimination validated on a realistic, exhaustive split. ✓")

