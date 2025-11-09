#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
goedel_1931_incompleteness.py
==============================

What is this?
-------------
A tiny, auditable Python program that answers the main question of
Kurt Gödel’s 1931 paper *On Formally Undecidable Propositions of
Principia Mathematica and Related Systems I*:

  “Is every consistent, effectively axiomatized, sufficiently arithmetic
   formal theory incomplete?”

It follows the EYE “branches of insights – brains” discipline:
the script computes an Answer, emits a formally constrained “Reason why”
in mathematical English, and then runs a Check (harness) with multiple
falsifiable tests.

How does it formalize Gödel’s First Incompleteness Theorem?
-----------------------------------------------------------
We encode a minimal, conditional criterion G1_min:

  Definition D (G1_min): For all formal theories T,
    Inc(T)  ⇐  EA(T) ∧ Cons(T) ∧ Arith(T),

where
  EA(T): T is effectively axiomatized (its axioms/proofs are computably enumerable),
  Cons(T): T is consistent (no φ with both φ and ¬φ provable),
  Arith(T): T is sufficiently expressive to represent basic arithmetic
            (e.g., at least Robinson Q / primitive recursion over +, ·).

Under this criterion, if a theory satisfies EA ∧ Cons ∧ Arith, then it is
(in the Gödel sense) incomplete: there exists a sentence G_T such that
T ⊬ G_T and (in Gödel’s original 1931 form, assuming also ω-consistency)
T ⊬ ¬G_T.

What does it NOT claim?
-----------------------
This program does not *prove* the consistency of any specific theory.
It evaluates a minimal, transparent *conditional* (G1_min) capturing the
heart of Gödel’s theorem: **if** EA, Cons, and Arith hold, **then**
Inc(T) holds. It’s a compact tool for thought, not a full arithmetization.

How to run
----------
  python3 goedel_1931_incompleteness.py

What you will see
-----------------
1) The Question.
2) The Answer (Yes/No — conditional on EA ∧ Cons ∧ Arith).
3) “Reason why” — a short proof-style narrative with quantifiers and bounds.
4) “Check (harness)” — >5 PASS/FAIL lines. Any failing check prints loudly.

How to extend
-------------
- Strengthen Arith(T) (e.g., require Σ₁-representation) or add ω-Consistency(T).
- Instantiate different theories (PA, ZF, ZFC, PA−, Q, Th(ℕ), etc.).
- Add checks for Rosser-style variants (dropping ω-consistency).
"""

from dataclasses import dataclass
from itertools import product
import random
random.seed(1931)


# ---------- Formalization (predicates & definition) ----------

@dataclass(frozen=True)
class Theory:
    name: str
    effective_axiomatization: bool   # EA(T)
    consistent: bool                 # Cons(T)
    arithmetic_expressive: bool      # Arith(T)
    omega_consistent: bool | None = None  # Optional: ω-Consistency(T) (not required in G1_min)


def EA(T: Theory) -> bool:
    return bool(T.effective_axiomatization)


def Cons(T: Theory) -> bool:
    return bool(T.consistent)


def Arith(T: Theory) -> bool:
    return bool(T.arithmetic_expressive)


def Omega(T: Theory) -> bool | None:
    return T.omega_consistent


# Definition (G1_min): Minimal criterion for Gödel–1.
# For all theories T,
#   Inc(T)  ⇐  EA(T) ∧ Cons(T) ∧ Arith(T).
def G1_min(T: Theory) -> tuple[bool, str]:
    ea, co, ar = EA(T), Cons(T), Arith(T)
    holds = (ea and co and ar)

    # "Reason why" in mathematical English (auditable trail).
    subject = "S"  # symbol in the derivation; will map to T.name in the printout
    premises = [
        f"Premise 1: EA({subject}) is {'true' if ea else 'false'} "
        f"({'computably enumerable axioms' if ea else 'no effective axiomatization'}).",
        f"Premise 2: Cons({subject}) is {'true' if co else 'false'} "
        f"({'no contradictions provable' if co else 'inconsistency assumed'}).",
        f"Premise 3: Arith({subject}) is {'true' if ar else 'false'} "
        f"({'represents basic arithmetic' if ar else 'insufficient arithmetic expressivity'})."
    ]

    definition_line = (
        "Definition D (G1_min): ∀T,  Inc(T) ⇐ [ EA(T) ∧ Cons(T) ∧ Arith(T) ]."
    )
    conclusion_line = (
        f"Therefore, by D, Inc({subject}) is {'true' if holds else 'false'}."
    )

    reason = (
        "Mathematical-English derivation:\n"
        + definition_line + "\n"
        + "\n".join(premises) + "\n"
        + conclusion_line
    )
    return holds, reason


# ---------- Instantiate a canonical arithmetic theory ----------
# We model a typical Gödel-eligible theory (e.g., PA). Consistency is taken here
# as a premise (the theorem is conditional).
PA = Theory(
    name="PA",
    effective_axiomatization=True,
    consistent=True,              # Assumed for the conditional
    arithmetic_expressive=True,
    omega_consistent=True         # Optional: Gödel 1931 used ω-consistency
)


# ---------- Compute Answer + Reason ----------
answer, reason = G1_min(PA)

print("Question")
print("========")
print("Is every consistent, effectively axiomatized, sufficiently arithmetic theory incomplete (Gödel 1931, G1_min)?\n")

print("Answer")
print("======")
print("Yes." if answer else "No.")
print()

print("Reason why")
print("==========")
# Replace the derivation symbol S with the theory's name for human readability.
def pretty_reason(txt: str, name: str) -> str:
    return (txt.replace("Inc(S)", f"Inc({name})")
               .replace("EA(S)", f"EA({name})")
               .replace("Cons(S)", f"Cons({name})")
               .replace("Arith(S)", f"Arith({name})")
            )
print(pretty_reason(reason, PA.name))
print()


# ---------- Check (harness) ----------
# We run >5 loud checks that must hold under the declared rules.

def check(name, fn):
    try:
        fn()
        print(f"[PASS] {name}")
    except AssertionError as e:
        print(f"[FAIL] {name} :: {e}")


print("Check (harness)")
print("===============")

# 1) Null theory should not be Inc-true.
def test_null_is_false():
    T0 = Theory("Null", False, False, False, None)
    val, _ = G1_min(T0)
    assert val is False, "Null theory misclassified as incomplete-true."

check("Null theory is not G1_min-true", test_null_is_false)


# 2) Boundary: EA, Cons, Arith all true ⇒ Inc true.
def test_boundary_true():
    T1 = Theory("T1", True, True, True, None)
    val, _ = G1_min(T1)
    assert val is True, "Boundary case (EA&Cons&Arith) should be True."

check("Boundary EA∧Cons∧Arith ⇒ Inc", test_boundary_true)


# 3) Necessity of EA: without EA, classification must be False (even if Cons & Arith).
def test_ea_necessary():
    T2 = Theory("T2", False, True, True, None)
    val, _ = G1_min(T2)
    assert val is False, "EA(T) is necessary but was not enforced."

check("EA is necessary", test_ea_necessary)


# 4) Necessity of Cons: without Cons, classification must be False (inconsistent theories prove everything).
def test_consistency_necessary():
    T3 = Theory("T3", True, False, True, None)
    val, _ = G1_min(T3)
    assert val is False, "Cons(T) is necessary but was not enforced."

check("Consistency is necessary", test_consistency_necessary)


# 5) Necessity of Arith: without sufficient arithmetic, classification must be False.
def test_arith_necessary():
    T4 = Theory("T4", True, True, False, None)
    val, _ = G1_min(T4)
    assert val is False, "Arith(T) is necessary but was not enforced."

check("Arithmetic expressivity is necessary", test_arith_necessary)


# 6) Monotonicity: adding extra (irrelevant) strength (e.g., ω-consistency True) cannot flip True→False.
def test_monotonicity_true_to_true():
    base = Theory("Base", True, True, True, None)
    base_val, _ = G1_min(base)
    assert base_val is True, "Precondition failed (base not True)."
    stronger = Theory("Base+Omega", True, True, True, True)
    stronger_val, _ = G1_min(stronger)
    assert stronger_val is True, "Monotonicity violated: added evidence flipped True to False."

check("Monotonicity True→True under added evidence", test_monotonicity_true_to_true)


# 7) Idempotence: same input → same output & core proof tokens persist.
def test_idempotence():
    val1, r1 = G1_min(PA)
    val2, r2 = G1_min(PA)
    assert val1 == val2, "Idempotence violated on boolean result."
    for token in ["∀T", "Therefore"]:
        assert token in r1 and token in r2, f"Reason text missing token: {token}"

check("Idempotence of evaluation and proof tokens", test_idempotence)


# 8) If Inc(T) holds under G1_min, then EA, Cons, and Arith must each be True (small exhaustive grid).
def test_true_implies_premises():
    for ea in (False, True):
        for co in (False, True):
            for ar in (False, True):
                T = Theory("Grid", ea, co, ar, None)
                val, _ = G1_min(T)
                if val:
                    assert EA(T) and Cons(T) and Arith(T), \
                        "A True classification broke the definition's antecedent."

check("True ⇒ EA & Cons & Arith (exhaustive 2×2×2 grid)", test_true_implies_premises)


# 9) Inconsistent but complete 'explosive' theory should NOT satisfy G1_min (fails Cons).
def test_explosion_escape():
    Explosion = Theory("Explosion", True, False, True, None)  # from inconsistency, everything provable
    val, _ = G1_min(Explosion)
    assert val is False, "Inconsistent (explosive) theory incorrectly flagged as G1_min-true."

check("Inconsistent-but-complete theories fail G1_min", test_explosion_escape)


# 10) Non-effective but complete theory of true arithmetic Th(N): EA False ⇒ G1_min should say False.
def test_true_arithmetic_non_effective():
    ThN = Theory("Th(N)", False, True, True, None)  # complete, consistent, not r.e.
    val, _ = G1_min(ThN)
    assert val is False, "Non-effective complete theory should not trigger G1_min."

check("Complete but non-effective Th(N) fails G1_min (EA False)", test_true_arithmetic_non_effective)


# 11) Invariance to field order / construction — permuting fields yields same outcome.
def test_invariance_to_field_order():
    base = {
        "name": "PA",
        "effective_axiomatization": True,
        "consistent": True,
        "arithmetic_expressive": True,
        "omega_consistent": True
    }
    keys = list(base.keys())
    random.shuffle(keys)
    shuffled = {k: base[k] for k in keys}
    T1 = Theory(**base)
    T2 = Theory(**shuffled)
    assert G1_min(T1)[0] == G1_min(T2)[0], "Permutation of input fields changed result."

check("Invariance to field order", test_invariance_to_field_order)


# ---------- End of file ----------

