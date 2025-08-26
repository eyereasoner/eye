#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Aristotle & the Barber Paradox: impossibility vs repair
-------------------------------------------------------

Philosophical framing (Aristotelian):
- A good definition must express a thing's *logos* (essence) without
  self-destruction; contradictions violate the Principle of Non-Contradiction (PNC).
- The naive barber clause quantifies over *all* townspeople, including the barber
  himself, which makes the definition *impredicative* (self-referential).
- Substituting x = barber yields S(b,b) <-> ¬S(b,b): impossible in any model.

Aristotelian remedy (non-circular essence):
- Restrict the role's scope:  ∀x ≠ b:  S(b,x) ↔ ¬S(x,x)
  Now the definition ranges over *others*, avoiding the self-application that
  caused the contradiction. The barber's self-shaving can be specified separately
  (or left free) without breaking the clause.

Program outputs:
1) Answer  – states the impossibility of the naive clause and the coherent repair.
2) Reason  – explains the contradiction and the Aristotelian restriction.
3) Check   – harness that:
     (A) Proves the naive clause is unsatisfiable (both branches fail).
     (B) Randomly builds towns; shows the restricted clause is satisfiable.
     (C) Shows the "Russell set" shadow R∈R ↔ ¬(R∈R) is also unsatisfiable.
"""

import random
from typing import Dict, List, Set, Tuple

# --------------------------- Pretty printing -----------------------------------
def section(title: str):
    print("\n" + "="*len(title))
    print(title)
    print("="*len(title))

# --------------------------- Core logic predicates ------------------------------
# We model shaving as a boolean relation S(a, x): "a shaves x".
# The paradoxical clause is:
#   (NAIVE)  ∀x: S(b, x)  ⇔  ¬S(x, x)
# Substituting x=b yields S(b,b) ⇔ ¬S(b,b) → contradiction.
#
# The Aristotelian repair restricts the domain to avoid vicious circularity:
#   (REPAIRED)  ∀x ≠ b: S(b, x)  ⇔  ¬S(x, x)

def naive_barber_consistent(shaves_self: bool) -> bool:
    """
    Given a truth-value for S(b,b), check if the NAIVE clause can be satisfied.
    Clause at x=b becomes: S(b,b) ⇔ ¬S(b,b), which is false for both True/False.
    """
    sbb = shaves_self
    return (sbb == (not sbb))  # always False

def repaired_barber_model(people: List[str], barber: str, self_shaves: Set[str]):
    """
    Construct a model for the REPAIRED clause:
        For all x ≠ barber: S(barber, x) ⇔ (x not in self_shaves)
    Return:
        - S: dictionary mapping (a, x) -> bool for a single 'a' = barber
        - holds: True iff repaired clause holds
    The value S(barber, barber) is left free; we set it to any value (e.g., False).
    """
    S: Dict[Tuple[str, str], bool] = {}
    for x in people:
        if x != barber:
            S[(barber, x)] = (x not in self_shaves)
    # choose any value for S(barber, barber) (clause doesn't constrain it)
    S[(barber, barber)] = False
    # verify repaired clause
    holds = all(S[(barber, x)] == (x not in self_shaves) for x in people if x != barber)
    return S, holds

# --------------------------- 1) Answer -----------------------------------------
def answer():
    section("Answer")
    print("The naive barber definition is IMPOSSIBLE (no model):")
    print("   ∀x: S(b,x) ⇔ ¬S(x,x)  ⇒  at x=b: S(b,b) ⇔ ¬S(b,b).")
    print("Aristotelian repair (non-circular essence) is CONSISTENT:")
    print("   ∀x ≠ b: S(b,x) ⇔ ¬S(x,x)  (scope excludes the barber).")

# --------------------------- 2) Reason -----------------------------------------
def reason():
    section("Reason (Aristotelian diagnosis)")
    print(
        "A definition must state a non-circular logos (essence). The naive clause "
        "quantifies over *all* townspeople, including the barber himself, which "
        "forces S(b,b) ⇔ ¬S(b,b) and violates the Principle of Non-Contradiction (PNC).\n"
        "Restricting the scope to x ≠ b removes the vicious self-application. "
        "The role is then grounded in prior facts (who self-shaves) and is satisfiable."
    )

# --------------------------- 3) Independent Check (harness) --------------------
def check():
    section("Independent Check (harness)")
    ok = True

    # (A) Show NAIVE clause unsatisfiable by case split on S(b,b)
    print("[A] Naive clause at x=b forces S(b,b) ⇔ ¬S(b,b):")
    for val in [False, True]:
        sat = naive_barber_consistent(val)
        print(f"    Try S(b,b) = {val:5}:  biconditional is {sat}  -> CONTRADICTION")
        if sat:
            ok = False
    if ok:
        print("    [PASS] Both branches fail; naive clause has no model")

    # (B) Random towns satisfy the REPAIRED clause
    if ok:
        print("\n[B] Repaired clause holds across random towns:")
        rng = random.Random(0)
        names = ["Barber", "Alice", "Bob", "Cara", "Dion", "Eve", "Fay", "Gus", "Hiro", "Ivy", "Jax"]
        for trial in range(10):
            # pick a random town size and cast of people
            town = names[: rng.randint(2, min(8, len(names)))]
            b = town[0]  # first person is the barber
            # choose random self-shavers
            self_shaves = {p for p in town if rng.random() < 0.5}
            S, holds = repaired_barber_model(town, b, self_shaves)
            if not holds:
                ok = False
                print("[FAIL] Repaired clause failed on trial", trial)
                break
            # spot-check: for x ≠ b, S(b,x) equals (x not in self_shaves)
            mismatches = [x for x in town if x != b and S[(b,x)] != (x not in self_shaves)]
            if mismatches:
                ok = False
                print("[FAIL] Mismatch for", mismatches)
                break
            print(f"    [PASS] Trial {trial}: town={town}, self_shaves={sorted(self_shaves)}")

    # (C) Russell set shadow: R∈R ⇔ ¬(R∈R) is unsatisfiable (both branches fail)
    if ok:
        print("\n[C] Russell set shadow: r := (r ∉ r)  is impossible:")
        for r_in_r in [False, True]:
            consistent = (r_in_r == (not r_in_r))
            print(f"    Try (R∈R) = {r_in_r:5}:  biconditional is {consistent}  -> CONTRADICTION")
            if consistent:
                ok = False
        if ok:
            print("    [PASS] Both branches fail; no assignment satisfies R∈R ⇔ ¬(R∈R)")

    print("\nResult:", "ALL CHECKS PASSED ✅" if ok else "Some checks FAILED ❌")
    return ok

# --------------------------- Main ----------------------------------------------
if __name__ == "__main__":
    answer()
    reason()
    check()

