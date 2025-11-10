#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
road_to_reality.py
==================

What is this?
-------------
A tiny, auditable Python program that answers a central question of
Roger Penrose’s *The Road to Reality* (2004):

  “Is physical reality fundamentally governed by deep, objective
   mathematical structures (a strong mathematical realism)?”

It follows the EYE “branches of insights – brains” discipline:
the script computes an Answer, emits a formally constrained “Reason why”
in mathematical English, and then runs a Check (harness) with multiple
falsifiable tests.

How does it formalize “mathematical realism”?
---------------------------------------------
We encode a minimal, falsifiable criterion M_min:

  Definition D (M_min): For all worlds x,
    R(x)  ⇐  L(x) ∧ S(x) ∧ ( C(x) + P(x) + E(x) ≥ 2 ),

where
  L(x): mathematical lawfulness — the world’s dynamics instantiate precise,
        mathematical laws (not mere regularities-by-accident),
  S(x): structural principles — symmetry/geometry/topology centrally constrain
        lawful forms,
  C(x): consilience — one mathematical framework unifies/explains diverse domains,
  P(x): predictive success — the framework yields novel, confirmed predictions,
  E(x): elegance/compressibility — simple axioms/generators compress wide ranges
        of phenomena (short descriptions vs brute lists).

Under this criterion, if {mathematical lawfulness, structural principles} hold,
and at least two among {consilience, predictive success, elegance} hold, then
R(x) (strong mathematical realism) is affirmed.

What does it NOT claim?
-----------------------
It does not adjudicate open issues (e.g., quantum gravity, non-computability,
or consciousness). It evaluates a transparent, conditional abstraction aligned
with Penrose’s emphasis on objective, discoverable mathematical structure.
It is a tool for thought, not a replacement for physics.

How to run
----------
  python3 road_to_reality.py

What you will see
-----------------
1) The Question.
2) The Answer (Yes/No).
3) “Reason why” — a short proof-style narrative with quantifiers and bounds.
4) “Check (harness)” — >5 PASS/FAIL lines. Any failing check prints loudly.

How to extend
-------------
- Tighten M_min (e.g., require all of {C, P, E}).
- Grade predicates (e.g., MDL-based elegance scores rather than booleans).
- Instantiate alternative “worlds” for comparison (instrumentalist/anti-realist variants).
"""

from dataclasses import dataclass
import random
random.seed(2004)


# ---------- Formalization (predicates & definition) ----------

@dataclass(frozen=True)
class World:
    laws_mathematical: bool          # L(x): governed by precise mathematical laws
    structural_principles: bool      # S(x): symmetry/geometry/topology central
    consilience: bool                # C(x): cross-domain unification
    predictive_success: bool         # P(x): novel predictions borne out
    elegance_compressibility: bool   # E(x): simple axioms compress phenomena


def L(x: World) -> bool:
    return bool(x.laws_mathematical)


def S(x: World) -> bool:
    return bool(x.structural_principles)


def C(x: World) -> bool:
    return bool(x.consilience)


def P(x: World) -> bool:
    return bool(x.predictive_success)


def E(x: World) -> bool:
    return bool(x.elegance_compressibility)


def at_least_two(triple: tuple[bool, bool, bool]) -> bool:
    return sum(1 for t in triple if t) >= 2


# Definition (M_min): Minimal criterion for strong mathematical realism.
# For all worlds x,
#   R(x)  ⇐  L(x) ∧ S(x) ∧ ( C(x) + P(x) + E(x) ≥ 2 ).
def M_min(x: World) -> tuple[bool, str]:
    l, s, c, p, e = L(x), S(x), C(x), P(x), E(x)
    k = at_least_two((c, p, e))
    holds = (l and s and k)

    # Build "Reason why" in mathematical English (auditable trail).
    subject = "W"  # symbol in the derivation; mapped to World_Penrose below
    premises_lines = [
        f"Premise 1: L({subject}) is {'true' if l else 'false'} (mathematical lawfulness {'present' if l else 'absent'}).",
        f"Premise 2: S({subject}) is {'true' if s else 'false'} (structural principles {'central' if s else 'not central'}).",
        f"Premise 3: C({subject}) is {'true' if c else 'false'} (consilience {'present' if c else 'absent'}).",
        f"Premise 4: P({subject}) is {'true' if p else 'false'} (predictive success {'present' if p else 'absent'}).",
        f"Premise 5: E({subject}) is {'true' if e else 'false'} (elegance/compressibility {'present' if e else 'absent'}).",
        f"Therefore among {{C({subject}), P({subject}), E({subject})}} we have "
        f"{sum([c, p, e])} truths → condition (C+P+E ≥ 2) is {'met' if k else 'not met'}."
    ]

    definition_line = (
        "Definition D (M_min): ∀x,  R(x) ⇐ [ L(x) ∧ S(x) ∧ ( C(x)+P(x)+E(x) ≥ 2 ) ]."
    )
    conclusion_line = (
        f"Therefore, by D, R({subject}) is {'true' if holds else 'false'}."
    )

    reason = (
        "Mathematical-English derivation:\n"
        + definition_line + "\n"
        + "\n".join(premises_lines) + "\n"
        + conclusion_line
    )
    return holds, reason


# ---------- Instantiate the subject world ----------
# Penrose-style stance: mathematical lawfulness + structural principles,
# with strong consilience, predictive success, and elegance.
World_Penrose = World(
    laws_mathematical=True,
    structural_principles=True,
    consilience=True,
    predictive_success=True,
    elegance_compressibility=True
)


# ---------- Compute Answer + Reason ----------
answer, reason = M_min(World_Penrose)

print("Question")
print("========")
print("Is physical reality fundamentally governed by deep mathematical structures (M_min)?\n")

print("Answer")
print("======")
print("Yes." if answer else "No.")
print()

print("Reason why")
print("==========")
# Replace the derivation symbol W with World_Penrose in the printed proof narrative.
print(
    reason.replace("R(W)", "R(World_Penrose)")
          .replace("L(W)", "L(World_Penrose)")
          .replace("S(W)", "S(World_Penrose)")
          .replace("C(W)", "C(World_Penrose)")
          .replace("P(W)", "P(World_Penrose)")
          .replace("E(W)", "E(World_Penrose)")
)
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

# 1) Null world should not satisfy R.
def test_null_is_false():
    x = World(False, False, False, False, False)
    val, _ = M_min(x)
    assert val is False, "Null world misclassified as M_min-true."

check("Null world is not M_min-true", test_null_is_false)


# 2) Boundary: L & S true, and exactly two among {C, P, E} true ⇒ R true.
def test_boundary_two_of_three_is_true():
    x = World(True, True, True, False, True)  # C=True, E=True, P=False
    val, _ = M_min(x)
    assert val is True, "Boundary case (two-of-three with L & S) should be True."

check("Boundary two-of-three with L & S holds", test_boundary_two_of_three_is_true)


# 3) Necessity of L: without mathematical lawfulness, R must be False even if others hold.
def test_lawfulness_is_necessary():
    x = World(False, True, True, True, True)
    val, _ = M_min(x)
    assert val is False, "L(x) is necessary but was not enforced."

check("Mathematical lawfulness (L) is necessary", test_lawfulness_is_necessary)


# 4) Necessity of S: without structural principles, R must be False even if others hold.
def test_structural_principles_are_necessary():
    x = World(True, False, True, True, True)
    val, _ = M_min(x)
    assert val is False, "S(x) is necessary but was not enforced."

check("Structural principles (S) are necessary", test_structural_principles_are_necessary)


# 5) Monotonicity: adding capabilities cannot flip True → False.
def test_monotonicity_true_to_true():
    base = World(True, True, True, True, False)  # already True: L,S and {C,P}
    base_val, _ = M_min(base)
    assert base_val is True, "Precondition failed (base not True)."
    supersets = [
        World(True, True, True, True, True),
        World(True, True, True, True, True)
    ]
    for sup in supersets:
        sup_val, _ = M_min(sup)
        assert sup_val is True, "Monotonicity violated: added evidence flipped True to False."

check("Monotonicity True→True under added evidence", test_monotonicity_true_to_true)


# 6) Idempotence: same input → same output & proof tokens persist.
def test_idempotence():
    val1, r1 = M_min(World_Penrose)
    val2, r2 = M_min(World_Penrose)
    assert val1 == val2, "Idempotence violated on boolean result."
    for token in ["∀x", "≥ 2", "Therefore"]:
        assert token in r1 and token in r2, f"Reason text missing token: {token}"

check("Idempotence of evaluation and proof tokens", test_idempotence)


# 7) If R(x) is True, then L and S must be True and count(C,P,E) ≥ 2 (exhaustive small grid).
def test_true_implies_premises():
    for l in (False, True):
        for s in (False, True):
            for c in (False, True):
                for p in (False, True):
                    for e in (False, True):
                        x = World(l, s, c, p, e)
                        val, _ = M_min(x)
                        if val:
                            assert L(x) and S(x) and at_least_two((C(x), P(x), E(x))), \
                                "A True classification broke the definition's antecedent."

check("True ⇒ L & S & (C+P+E ≥ 2) (exhaustive small grid)", test_true_implies_premises)


# 8) Minimality wrt necessary parts on World_Penrose: remove pieces → False.
def test_minimality_removals():
    # Remove L
    x1 = World(False, True, True, True, True)
    assert M_min(x1)[0] is False, "Removing L should falsify R."

    # Remove S
    x2 = World(True, False, True, True, True)
    assert M_min(x2)[0] is False, "Removing S should falsify R."

    # Keep L & S but reduce two-of-three to one-of-three
    x3 = World(True, True, True, False, False)  # only C True
    assert M_min(x3)[0] is False, "One-of-three should be insufficient."

check("Minimality of necessary conditions", test_minimality_removals)


# 9) Invariance to field order (dict shuffle → same result).
def test_invariance_to_field_order():
    base = {
        "laws_mathematical": True,
        "structural_principles": True,
        "consilience": True,
        "predictive_success": True,
        "elegance_compressibility": True
    }
    keys = list(base.keys())
    random.shuffle(keys)
    shuffled = {k: base[k] for k in keys}
    x1 = World(**base)
    x2 = World(**shuffled)
    assert M_min(x1)[0] == M_min(x2)[0], "Permutation of input fields changed result."

check("Invariance to field order", test_invariance_to_field_order)


# 10) Two-of-three requirement: with L,S True but only one of {C,P,E} True ⇒ False.
def test_two_of_three_requirement():
    x = World(True, True, False, True, False)  # only P True
    val, _ = M_min(x)
    assert val is False, "Two-of-three requirement not enforced."

check("Two-of-three requirement enforced", test_two_of_three_requirement)


# ---------- End of file ----------

