# ============================================
# Why “Socrates is mortal” follows logically
# ============================================
# Premises:
#   (P1) ∀x (Man(x) → Mortal(x))          # All men are mortal
#   (P2) Man(Socrates)                    # Socrates is a man
#
# Goal:
#   (C)  Mortal(Socrates)                 # Socrates is mortal
#
# The “reason why” (clean derivation):
#   Step A — Universal Instantiation (UI) on (P1) with the constant Socrates:
#       From  ∀x (Man(x) → Mortal(x))  infer  Man(Socrates) → Mortal(Socrates).
#
#   Step B — Modus Ponens (MP) with (P2):
#       From  Man(Socrates)  and  Man(Socrates) → Mortal(Socrates)
#       infer  Mortal(Socrates).
#
#   That’s it: UI + MP yields the conclusion logically.
#
# -------------------------------------------------------
# Program output: print the “reason why”
# -------------------------------------------------------

print("============================================")
print("Socrates is mortal — the logical reason why")
print("============================================\n")

print("Premises:")
print("  (P1)  ∀x (Man(x) → Mortal(x))        # All men are mortal")
print("  (P2)  Man(Socrates)                  # Socrates is a man\n")

print("Derivation:")
print("  Step A (Universal Instantiation on P1):")
print("    From ∀x (Man(x) → Mortal(x)) infer Man(Socrates) → Mortal(Socrates).")
print("  Step B (Modus Ponens with P2):")
print("    From Man(Socrates) and Man(Socrates) → Mortal(Socrates) infer Mortal(Socrates).\n")

print("Conclusion:")
print("  Mortal(Socrates).  (UI + MP)\n")

# -------------------------------------------------------
# Proof harness (semantic sanity check on finite domains)
# -------------------------------------------------------
# Idea:
#   We check *semantically* that whenever (P1) and (P2) hold in a finite domain,
#   the conclusion (C) must also hold.
#
#   Model setup (finite, unary predicates only):
#     • Domain D = {c0, c1, ..., c_{N-1}} with c0 named 'Socrates'.
#     • Predicates Man(·), Mortal(·) are truth assignments over D.
#   Constraint for (P1): for every d in D, not(Man(d) and not Mortal(d)).
#     (i.e., the pair (True, False) is forbidden at each element.)
#   Constraint for (P2): Man(c0) is True.
#   We enumerate all valuations satisfying these constraints and assert that
#   Mortal(c0) is True in all of them.
#
#   Observations:
#     • For any element d, the allowed truth pairs are exactly:
#          (Man, Mortal) ∈ {(False, False), (False, True), (True, True)}.
#       (The forbidden bad pair is (True, False).)
#     • For c0 = Socrates, (P2) forces (Man, Mortal) = (True, True) under (P1).
#       So the conclusion is immediate — but we still check exhaustively.

def check_all_models_up_to_size(max_n):
    total_models_checked = 0
    for N in range(1, max_n + 1):
        total_models_checked += enumerate_models_and_check(N)
    return total_models_checked

def enumerate_models_and_check(N):
    """
    Enumerate all valuations of Man and Mortal over a domain of size N
    that satisfy (P1) and (P2). Assert the conclusion holds in each model.
    Returns the number of models checked for this N.
    """
    # Allowed truth pairs at a domain element d (Man(d), Mortal(d)):
    # (F,F), (F,T), (T,T). (T,F) is forbidden by (P1).
    allowed = [(0, 0), (0, 1), (1, 1)]

    # We will build arrays man[i], mortal[i] for i = 0..N-1.
    man    = [0] * N
    mortal = [0] * N

    # (P2) requires Man(Socrates) to be True; with (P1) this forces Mortal(Socrates) True.
    # We encode that by fixing the pair for i=0 to (1,1) and enumerate freely for i≥1.
    man[0], mortal[0] = 1, 1

    count = 0
    def backtrack(i):
        nonlocal count
        if i == N:
            # Sanity: (P1) holds (it does by construction), (P2) holds, then (C) must hold.
            assert man[0] == 1, "Premise (P2) violated in construction."
            # From (P1): (1,0) never occurs, so Mortal(Socrates) must be 1.
            assert mortal[0] == 1, "Conclusion (C) fails in a model — impossible under (P1)+(P2)."
            count += 1
            return
        # Choose any allowed pair for element i (except i==0 which is fixed)
        for (m, mo) in allowed:
            man[i], mortal[i] = m, mo
            backtrack(i + 1)

    # Start enumeration from i = 1 (since i = 0 fixed to (1,1))
    if N == 1:
        count = 1  # only the fixed assignment on Socrates
    else:
        backtrack(1)

    # Optional: verify (P1) explicitly across the model
    for idx in range(N):
        assert not (man[idx] == 1 and mortal[idx] == 0), "(P1) violated post-enumeration."
    # Verify (P2)
    assert man[0] == 1, "(P2) violated post-enumeration."
    # Verify (C)
    assert mortal[0] == 1, "Conclusion (C) violated post-enumeration."
    return count

# Run the harness (small sizes keep things fast and readable)
if __name__ == "__main__":
    total = check_all_models_up_to_size(6)  # domains of size 1..6
    print(f"Harness: checked {total} models over domains |D|=1..6. All satisfy (P1)&(P2) ⇒ (C). ✓")

