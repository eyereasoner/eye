#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
reality_and_its_order.py
========================

What is this?
-------------
A tiny, auditable Python program that answers a central question of
Werner Heisenberg’s *Reality and Its Order* (English trans. 2019 of a 1941/42 essay):

  “Should we conceive reality as an ordered, stratified structure that
   requires complementary descriptions (quantum/classical etc.) rather
   than a single universal classical picture?”

It follows the EYE “branches of insights – brains” discipline:
the script computes an Answer, emits a formally constrained “Reason why”
in mathematical English, and then runs a Check (harness) with multiple
falsifiable tests.

How does it formalize Heisenberg’s stance?
------------------------------------------
We encode a minimal, falsifiable criterion H_min for “ordered, complementary reality”:

  Definition D (H_min): For all worlds x,
    H(x)  ⇐  L(x) ∧ Q(x) ∧ ( Cl(x) + Nr(x) + Co(x) ≥ 2 ),

where
  L(x): there exist ≥2 autonomous levels/orders of description (stratification),
  Q(x): quantum contextuality/complementarity constrains what can be ascribed,
  Cl(x): classical limit/correspondence holds in an appropriate domain,
  Nr(x): nonreduction — higher-level regularities are not trivially derivable
         from lower-level microdynamics,
  Co(x): cross-level coherence — overlapping domains do not contradict;
         bridge principles relate levels.

Under this criterion, if the world exhibits multiple levels (L) and quantum
constraints (Q), and at least two among {classical limit, nonreduction,
cross-level coherence}, then H(world) holds.

What does it NOT claim?
-----------------------
It does not settle metaphysics or reconstruct Heisenberg’s full argument.
It evaluates a transparent, testable abstraction aligned with themes of
levels, complementarity, correspondence, and nonreduction. It is a tool
for thought, not a historical-critical edition.

How to run
----------
  python3 reality_and_its_order.py

What you will see
-----------------
1) The Question.
2) The Answer (Yes/No).
3) “Reason why” — a short proof-style narrative with quantifiers and bounds.
4) “Check (harness)” — >5 PASS/FAIL lines. Any failing check prints loudly.

How to extend
-------------
- Tighten the threshold (e.g., require all of {Cl, Nr, Co}).
- Refine predicates (graded contextuality; quantitative coherence tests).
- Compare different hypothetical “worlds” by instantiating more systems.
- Add domain-specific checks (e.g., stronger correspondence constraints).

Attributions
------------
- High-level framing reflects Heisenberg’s essay about knowledge, reality,
  levels/orders of description, complementarity, and correspondence.
"""

from dataclasses import dataclass
from itertools import product
import random
random.seed(1942)


# ---------- Formalization (predicates & definition) ----------

@dataclass(frozen=True)
class World:
    level_count: int            # number of autonomous descriptive orders (≥2 ⇒ stratification)
    quantum_contextuality: bool # Q: measurement/context limits simultaneous properties (complementarity)
    classical_limit: bool       # Cl: correspondence/classical regime exists
    nonreduction: bool          # Nr: higher-level regularities not trivially reducible
    cross_level_coherence: bool # Co: bridge principles align overlapping domains


def L(x: World) -> bool:
    """Stratification predicate: at least two levels/orders of description."""
    return x.level_count >= 2


def Q(x: World) -> bool:
    """Quantum contextuality/complementarity predicate."""
    return bool(x.quantum_contextuality)


def Cl(x: World) -> bool:
    """Classical limit (correspondence) predicate."""
    return bool(x.classical_limit)


def Nr(x: World) -> bool:
    """Nonreduction predicate."""
    return bool(x.nonreduction)


def Co(x: World) -> bool:
    """Cross-level coherence predicate."""
    return bool(x.cross_level_coherence)


def at_least_two(triple: tuple[bool, bool, bool]) -> bool:
    return sum(bool(t) for t in triple) >= 2


# Definition (H_min): Minimal criterion for an ordered, complementary reality.
# For all worlds x,
#   H(x)  ⇐  L(x) ∧ Q(x) ∧ ( Cl(x) + Nr(x) + Co(x) ≥ 2 ).
def H_min(x: World) -> tuple[bool, str]:
    l, q, cl, nr, co = L(x), Q(x), Cl(x), Nr(x), Co(x)
    k = at_least_two((cl, nr, co))
    holds = (l and q and k)

    # Build "Reason why" in mathematical English (auditable trail).
    subject = "W"  # symbol in the derivation; mapped to World in the printout
    premises_lines = [
        f"Premise 1: L({subject}) is {'true' if l else 'false'} because level_count({subject}) = {x.level_count} {'≥' if l else '<'} 2.",
        f"Premise 2: Q({subject}) is {'true' if q else 'false'} (quantum contextuality {'present' if q else 'absent'}).",
        f"Premise 3: Cl({subject}) is {'true' if cl else 'false'} (classical-limit correspondence {'holds' if cl else 'does not hold'}).",
        f"Premise 4: Nr({subject}) is {'true' if nr else 'false'} (nonreduction {'present' if nr else 'absent'}).",
        f"Premise 5: Co({subject}) is {'true' if co else 'false'} (cross-level coherence {'present' if co else 'absent'}).",
        f"Therefore among {{Cl({subject}), Nr({subject}), Co({subject})}} we have "
        f"{sum([cl, nr, co])} truths → condition (Cl+Nr+Co ≥ 2) is {'met' if k else 'not met'}."
    ]

    definition_line = (
        "Definition D (H_min): ∀x,  H(x) ⇐ [ L(x) ∧ Q(x) ∧ ( Cl(x)+Nr(x)+Co(x) ≥ 2 ) ]."
    )
    conclusion_line = (
        f"Therefore, by D, H({subject}) is {'true' if holds else 'false'}."
    )

    reason = (
        "Mathematical-English derivation:\n"
        + definition_line + "\n"
        + "\n".join(premises_lines) + "\n"
        + conclusion_line
    )
    return holds, reason


# ---------- Instantiate the subject world ----------
# Heisenberg-style stance: multiple levels; quantum complementarity; a classical limit;
# nonreduction and cross-level coherence both acknowledged.
World_H = World(
    level_count=3,              # e.g., classical domain, quantum domain, biological/psychological order
    quantum_contextuality=True, # complementarity/context dependence
    classical_limit=True,       # correspondence principle (classical regimes exist)
    nonreduction=True,          # not all macroscopics trivially deduced from microdynamics
    cross_level_coherence=True  # bridge principles maintain consistency across overlaps
)


# ---------- Compute Answer + Reason ----------
answer, reason = H_min(World_H)

print("Question")
print("========")
print("Should we conceive reality as ordered/stratified and complementary (H_min)?\n")

print("Answer")
print("======")
print("Yes." if answer else "No.")
print()

print("Reason why")
print("==========")
# Replace the derivation symbol W with World_H in the printed proof narrative.
print(
    reason.replace("H(W)", "H(World_H)")
          .replace("L(W)", "L(World_H)")
          .replace("Q(W)", "Q(World_H)")
          .replace("Cl(W)", "Cl(World_H)")
          .replace("Nr(W)", "Nr(World_H)")
          .replace("Co(W)", "Co(World_H)")
          .replace("level_count(W)", "level_count(World_H)")
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

# 1) Null world should not satisfy H.
def test_null_is_false():
    x = World(1, False, False, False, False)   # L=False, Q=False, others False
    val, _ = H_min(x)
    assert val is False, "Null world misclassified as H_min-true."

check("Null world is not H_min-true", test_null_is_false)


# 2) Boundary: L true, Q true, and exactly two among {Cl, Nr, Co} true ⇒ H true.
def test_boundary_two_of_three_is_true():
    x = World(2, True, True, False, True)  # Cl=True, Nr=False, Co=True
    val, _ = H_min(x)
    assert val is True, "Boundary case (two-of-three with L & Q) should be True."

check("Boundary two-of-three with L & Q holds", test_boundary_two_of_three_is_true)


# 3) Necessity of L: without stratification, H must be False even if others hold.
def test_stratification_is_necessary():
    x = World(1, True, True, True, True)  # L=False; others True
    val, _ = H_min(x)
    assert val is False, "L(x) is necessary but was not enforced."

check("Stratification (L) is necessary", test_stratification_is_necessary)


# 4) Necessity of Q: without quantum constraints, H must be False even if others hold.
def test_quantum_is_necessary():
    x = World(3, False, True, True, True)  # Q=False; others True
    val, _ = H_min(x)
    assert val is False, "Q(x) is necessary but was not enforced."

check("Quantum contextuality (Q) is necessary", test_quantum_is_necessary)


# 5) Monotonicity: adding capabilities cannot flip True → False.
def test_monotonicity_true_to_true():
    base = World(2, True, True, True, False)  # already True: L,Q,Cl,Nr
    base_val, _ = H_min(base)
    assert base_val is True, "Precondition failed (base not True)."
    supersets = [
        World(3, True, True, True, True),
        World(4, True, True, True, True)
    ]
    for sup in supersets:
        sup_val, _ = H_min(sup)
        assert sup_val is True, "Monotonicity violated: added evidence flipped True to False."

check("Monotonicity True→True under added evidence", test_monotonicity_true_to_true)


# 6) Idempotence: same input → same output & proof tokens persist.
def test_idempotence():
    val1, r1 = H_min(World_H)
    val2, r2 = H_min(World_H)
    assert val1 == val2, "Idempotence violated on boolean result."
    for token in ["∀x", "≥ 2", "Therefore"]:
        assert token in r1 and token in r2, f"Reason text missing token: {token}"

check("Idempotence of evaluation and proof tokens", test_idempotence)


# 7) If H(x) is True, then L and Q must be True and count(Cl,Nr,Co) ≥ 2 (small exhaustive grid).
def test_true_implies_premises():
    for lc in range(0, 4):
        for q in (False, True):
            for cl in (False, True):
                for nr in (False, True):
                    for co in (False, True):
                        x = World(lc, q, cl, nr, co)
                        val, _ = H_min(x)
                        if val:
                            assert L(x) and Q(x) and at_least_two((Cl(x), Nr(x), Co(x))), \
                                "A True classification broke the definition's antecedent."

check("True ⇒ L & Q & (Cl+Nr+Co ≥ 2) (exhaustive small grid)", test_true_implies_premises)


# 8) Minimality wrt necessary parts on World_H: remove pieces → False.
def test_minimality_removals():
    # Remove L by dropping level_count to 1
    x1 = World(1, True, True, True, True)
    assert H_min(x1)[0] is False, "Removing L should falsify H."

    # Keep L & Q but reduce two-of-three to one-of-three
    x2 = World(3, True, True, False, False)  # only Cl True
    assert H_min(x2)[0] is False, "One-of-three should be insufficient."

    # Keep L but remove Q
    x3 = World(3, False, True, True, False)
    assert H_min(x3)[0] is False, "Removing Q should falsify H."

check("Minimality of necessary conditions", test_minimality_removals)


# 9) Invariance to field order (dict shuffle → same result).
def test_invariance_to_field_order():
    base = {
        "level_count": 3,
        "quantum_contextuality": True,
        "classical_limit": True,
        "nonreduction": True,
        "cross_level_coherence": True
    }
    keys = list(base.keys())
    random.shuffle(keys)
    shuffled = {k: base[k] for k in keys}
    x1 = World(**base)
    x2 = World(**shuffled)
    assert H_min(x1)[0] == H_min(x2)[0], "Permutation of input fields changed result."

check("Invariance to field order", test_invariance_to_field_order)


# 10) If none of {Cl,Nr,Co} hold, H must be False even with L & Q True.
def test_two_of_three_requirement():
    x = World(3, True, False, False, False)  # L,Q True; 0-of-3
    val, _ = H_min(x)
    assert val is False, "Two-of-three requirement not enforced."

check("Two-of-three requirement enforced", test_two_of_three_requirement)


# ---------- End of file ----------

