#!/usr/bin/env python3
# ============================================================
# Disjunction Elimination (∨-Elim) — ARC-ified, exhaustive case
# ============================================================
# You’ll see:
#   • Answer      — the conclusion.
#   • Reason why  — the case split argument (proof by cases).
#   • Check       — a small semantic harness that exhaustively
#                   verifies the rule under an exclusive Weekday/Weekend split.
# No imports. Deterministic output.
# ============================================================

# -------------
# Core semantics
# -------------
def implies(p: bool, q: bool) -> bool:
    """Material implication p → q == (¬p) ∨ q."""
    return (not p) or q

def xor(a: bool, b: bool) -> bool:
    """Exclusive-or: exactly one is true."""
    return (a and not b) or (b and not a)

# ----------------------
# Exhaustive rule check
# ----------------------
def harness():
    """
    Enumerate all models where exactly one of {Wd, We} holds.
    Count those where (Wd ∨ We) ∧ (Wd→Op) ∧ (We→Op) is true and assert Op.
    """
    models_scanned = 0
    satisfying = 0
    for Wd in (False, True):
        for We in (False, True):
            if not xor(Wd, We):
                continue  # enforce exclusive & exhaustive split
            for Op in (False, True):
                models_scanned += 1
                P1 = (Wd or We)
                P2 = implies(Wd, Op)
                P3 = implies(We, Op)
                if P1 and P2 and P3:
                    satisfying += 1
                    assert Op, "Counterexample: premises hold but Op is false."
    return models_scanned, satisfying

# ============
# ARC sections
# ============
print("Answer")
print("------")
print("(C) Op — The museum is open today.")
print()

print("Reason why")
print("----------")
print("From (P1) Wd ∨ We, exactly one case holds (exclusive & exhaustive).")
print("• Case Weekday:  assume Wd. From (P2) Wd→Op, infer Op.")
print("• Case Weekend:  assume We. From (P3) We→Op, infer Op.")
print("Both cases yield Op, so Op holds unconditionally.  (∨-Elimination)")
print()

print("Check (harness)")
print("---------------")
total, sat = harness()
print(f"Scanned {total} exclusive day-cases; {sat} satisfy (P1)∧(P2)∧(P3).")
print("All satisfying cases make Op True.  ∨-Elimination validated. ✓")

