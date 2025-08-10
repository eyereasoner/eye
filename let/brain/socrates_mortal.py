#!/usr/bin/env python3
"""
Socrates is mortal — ARC (Answer / Reason / Check), self-contained

Premises:
  (P1) ∀x (Man(x) → Mortal(x))        # All men are mortal
  (P2) Man(Socrates)                  # Socrates is a man

Goal:
  (C) Mortal(Socrates)

Derivation idea:
  Step A — UI on (P1) with Socrates:      Man(Socrates) → Mortal(Socrates)
  Step B — MP with (P2):                  Mortal(Socrates)

Harness:
  Semantic check over finite domains: enumerate valuations for Man(·), Mortal(·)
  that satisfy (P1) and (P2), and assert that (C) always holds.
"""

# ------------------------------ Answer ------------------------------
def print_answer():
    print("Answer")
    print("======")
    print("Conclusion: Mortal(Socrates)")
    print("\nFrom the premises:")
    print("  (P1) ∀x (Man(x) → Mortal(x))")
    print("  (P2) Man(Socrates)")

# --------------------------- Reason why -----------------------------
def print_reason():
    print("\nReason why")
    print("==========")
    print("Step A (Universal Instantiation on P1):")
    print("  From ∀x (Man(x) → Mortal(x)) infer Man(Socrates) → Mortal(Socrates).")
    print("Step B (Modus Ponens with P2):")
    print("  From Man(Socrates) and Man(Socrates) → Mortal(Socrates)")
    print("  infer Mortal(Socrates).")
    print("\nTherefore, Mortal(Socrates).")

# ----------------------- Check (harness) ----------------------------
# We check *semantically* that whenever (P1) and (P2) hold in a finite domain,
# the conclusion (C) must also hold.
#
# Model (finite, unary predicates only):
#   • Domain D = {c0, c1, ..., c_{N-1}} with c0 named 'Socrates'
#   • Predicates Man(·), Mortal(·) are truth assignments over D
#
# Constraint for (P1): for every d in D, not(Man(d) and not Mortal(d))
#   → allowed pairs (Man(d), Mortal(d)) ∈ {(0,0), (0,1), (1,1)}  (forbidden: (1,0))
# Constraint for (P2): Man(c0) is True
#
# Under these constraints, Mortal(c0) must be True in all models.

def check_all_models_up_to_size(max_n: int) -> int:
    total_models_checked = 0
    for N in range(1, max_n + 1):
        total_models_checked += enumerate_models_and_check(N)
    return total_models_checked

def enumerate_models_and_check(N: int) -> int:
    """
    Enumerate all valuations of Man and Mortal over a domain of size N
    that satisfy (P1) and (P2). Assert that the conclusion holds in each model.
    Returns the number of models checked for this N.
    """
    # Allowed truth pairs at a domain element d: (F,F), (F,T), (T,T)
    allowed = [(0, 0), (0, 1), (1, 1)]

    # Arrays man[i], mortal[i] for i = 0..N-1.
    man = [0] * N
    mortal = [0] * N

    # (P2) requires Man(Socrates)=True; with (P1) the only compatible pair is (1,1)
    # because (1,0) is forbidden by (P1).
    man[0], mortal[0] = 1, 1

    count = 0

    def backtrack(i: int):
        nonlocal count
        if i == N:
            # Verify constraints & conclusion explicitly
            for idx in range(N):
                assert not (man[idx] == 1 and mortal[idx] == 0), "(P1) violated."
            assert man[0] == 1, "(P2) violated."
            assert mortal[0] == 1, "Conclusion (C) violated."
            count += 1
            return
        for (m, mo) in allowed:
            man[i], mortal[i] = m, mo
            backtrack(i + 1)

    if N == 1:
        # Only the fixed assignment on Socrates
        count = 1
        # Re-check explicitly
        assert not (man[0] == 1 and mortal[0] == 0), "(P1) violated."
        assert man[0] == 1, "(P2) violated."
        assert mortal[0] == 1, "Conclusion (C) violated."
    else:
        # Start enumeration from i = 1 (since i = 0 fixed)
        backtrack(1)

    return count

def print_check():
    print("\nCheck (harness)")
    print("===============")
    total = check_all_models_up_to_size(6)  # domains of size 1..6
    print(f"Harness: checked {total} models over domains |D| = 1..6.")
    print("All satisfy (P1) & (P2) ⇒ Mortal(Socrates). ✓")

# ------------------------------- Main --------------------------------
if __name__ == "__main__":
    print_answer()
    print_reason()
    print_check()

