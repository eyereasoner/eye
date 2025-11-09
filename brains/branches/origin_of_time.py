#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
origin_of_time.py
=================

What is this?
-------------
A tiny, auditable Python program that answers a central question of
Thomas Hertog’s *On the Origin of Time* (2023): “Do the fundamental laws of
physics co-emerge and evolve with the universe rather than being timeless and
fixed?” It follows the EYE “branches of insights – brains” discipline: the
script computes an Answer, emits a formally constrained “Reason why” in
mathematical English, and then runs a Check (harness) with multiple falsifiable
tests.

How does it formalize the question?
-----------------------------------
We use a minimal, falsifiable criterion E_min for “evolving laws”:

  Definition D (E_min): For all systems x,
    E(x)  ⇐  R(x) ∧ ( H(x) + F(x) + ¬U(x) ≥ 2 ),

where
  R(x): regime variability — at least 2 distinct dynamical regimes with
        different effective descriptions (counts ≥ 2),
  H(x): history dependence — present laws/effective rules depend (even partly)
        on earlier states/conditions (top-down, relational influence),
  F(x): selection/feedback — feedback that stabilizes regularities (e.g., a
        selection-like mechanism / constraint that prunes possibilities),
  U(x): uniqueness-timelessness — a single, timeless, context-free law (U=True)
        vs non-unique/contextual (U=False).

Under this criterion, if the world exhibits multiple dynamical regimes (R) and
at least two among {history dependence, selection/feedback, non-uniqueness}
then E(world) holds.

What does it NOT claim?
-----------------------
It does not adjudicate the ultimate truth of any cosmological theory. It
evaluates a *minimal, transparent criterion* capturing the book’s thesis in a
testable abstraction. It’s a tool for thought, not a verdict on metaphysics.

How to run
----------
  python3 origin_of_time.py

What you will see
-----------------
1) The Question.
2) The Answer (Yes/No).
3) “Reason why” — a short proof-style narrative with quantifiers and bounds.
4) “Check (harness)” — >5 PASS/FAIL lines. Any failing check prints loudly.

How to extend
-------------
- Tighten the threshold (e.g., require all of {H, F, ¬U}).
- Refine predicates (e.g., gradient of history-dependence vs boolean).
- Compare different hypothetical cosmologies by instantiating more systems.
- Add domain-specific checks (e.g., robustness under coarse-graining).

Attributions
------------
- High-level framing inspired by Hertog’s presentation of a top-down,
  relational cosmology in which effective laws co-emerge and evolve with the
  universe’s history. This program abstracts that stance into a crisp,
  falsifiable criterion.
"""

from dataclasses import dataclass
from itertools import product
import random
random.seed(42)


# ---------- Formalization (predicates & definition) ----------

@dataclass(frozen=True)
class Cosmos:
    # Simple capability flags and counts to stand in for richer structure.
    regime_count: int           # number of distinct effective regimes (≥2 ⇒ nontrivial variability)
    history_dependence: bool    # H: present effective rules depend on prior states/boundary data
    feedback_selection: bool    # F: feedback/selection prunes possibilities & stabilizes regularities
    timeless_unique_law: bool   # U: True if a single timeless, context-free law is assumed


def R(x: Cosmos) -> bool:
    """Regime variability predicate."""
    return x.regime_count >= 2


def H(x: Cosmos) -> bool:
    """History dependence predicate."""
    return bool(x.history_dependence)


def F(x: Cosmos) -> bool:
    """Selection/feedback predicate."""
    return bool(x.feedback_selection)


def U(x: Cosmos) -> bool:
    """Timeless uniqueness predicate."""
    return bool(x.timeless_unique_law)


def at_least_two(triple: tuple[bool, bool, bool]) -> bool:
    return sum(bool(t) for t in triple) >= 2


# Definition (E_min): Minimal criterion for evolving laws.
# For all systems x,
#   E(x)  ⇐  R(x) ∧ ( H(x) + F(x) + ¬U(x) ≥ 2 ).
def E_min(x: Cosmos) -> tuple[bool, str]:
    r, h, f, u = R(x), H(x), F(x), U(x)
    k = at_least_two((h, f, (not u)))
    holds = (r and k)

    # Build "Reason why" in mathematical English (auditable trail).
    subject = "W"  # name used in the derivation (we'll map to World below)
    premises_lines = [
        f"Premise 1: R({subject}) is {'true' if r else 'false'} because regime_count({subject}) = {x.regime_count} {'≥' if r else '<'} 2.",
        f"Premise 2: H({subject}) is {'true' if h else 'false'} (history dependence {'present' if h else 'absent'}).",
        f"Premise 3: F({subject}) is {'true' if f else 'false'} (selection/feedback {'present' if f else 'absent'}).",
        f"Premise 4: ¬U({subject}) is {'true' if (not u) else 'false'} (timeless uniqueness is {'rejected' if (not u) else 'assumed'}).",
        f"Therefore among {{H({subject}), F({subject}), ¬U({subject})}} we have "
        f"{sum([h, f, (not u)])} truths → condition (H+F+¬U ≥ 2) is {'met' if k else 'not met'}."
    ]

    definition_line = (
        "Definition D (E_min): ∀x,  E(x) ⇐ [ R(x) ∧ ( H(x)+F(x)+¬U(x) ≥ 2 ) ]."
    )
    conclusion_line = (
        f"Therefore, by D, E({subject}) is {'true' if holds else 'false'}."
    )

    reason = (
        "Mathematical-English derivation:\n"
        + definition_line + "\n"
        + "\n".join(premises_lines) + "\n"
        + conclusion_line
    )
    return holds, reason


# ---------- Instantiate the subject world ----------
# This "World" encodes the Hertog-style stance: multiple regimes, history dependence,
# selection-like feedback, and rejection of a single timeless unique law.
World = Cosmos(
    regime_count=3,            # e.g., early quantum-gravity regime, radiation era, structure era (≥2)
    history_dependence=True,   # top-down/relational influence from boundary/initial conditions
    feedback_selection=True,   # selection/feedback prunes/stabilizes effective regularities
    timeless_unique_law=False  # laws not taken as one timeless unique rule
)


# ---------- Compute Answer + Reason ----------
answer, reason = E_min(World)

print("Question")
print("========")
print("Do the fundamental laws of physics co-emerge and evolve with the universe (E_min)?\n")

print("Answer")
print("======")
print("Yes." if answer else "No.")
print()

print("Reason why")
print("==========")
# Replace subject symbol W with World in the printed proof narrative.
print(
    reason.replace("E(W)", "E(World)")
          .replace("R(W)", "R(World)")
          .replace("H(W)", "H(World)")
          .replace("F(W)", "F(World)")
          .replace("U(W)", "U(World)")
          .replace("regime_count(W)", "regime_count(World)")
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

# 1) Null cosmos should not satisfy E.
def test_null_is_false():
    x = Cosmos(1, False, False, True)   # R=False, H=F=False, U=True
    val, _ = E_min(x)
    assert val is False, "Null cosmos misclassified as evolving-law True."

check("Null cosmos is not E_min-true", test_null_is_false)


# 2) Boundary: R true and exactly two among {H, F, ¬U} true => E true.
def test_boundary_two_of_three_is_true():
    x = Cosmos(2, True, False, False)   # H=True, F=False, U=False → {H,¬U} = 2 truths
    val, _ = E_min(x)
    assert val is True, "Boundary case (two-of-three with R) should be True."

check("Boundary two-of-three with R holds", test_boundary_two_of_three_is_true)


# 3) Necessity of R: without R, classification must be False even if others hold.
def test_regime_variability_is_necessary():
    x = Cosmos(1, True, True, False)    # R=False; others make two truths but should fail
    val, _ = E_min(x)
    assert val is False, "R(x) is necessary but was not enforced."

check("Regime variability (R) is necessary", test_regime_variability_is_necessary)


# 4) Monotonicity: adding capabilities cannot flip True -> False.
def test_monotonicity_true_to_true():
    base = Cosmos(2, True, True, False)  # R True; H,F True; U False → True
    base_val, _ = E_min(base)
    assert base_val is True, "Precondition failed (base not True)."
    supersets = [
        Cosmos(3, True, True, False),
        Cosmos(5, True, True, False)
    ]
    for sup in supersets:
        sup_val, _ = E_min(sup)
        assert sup_val is True, "Monotonicity violated: added evidence flipped True to False."

check("Monotonicity True→True under added evidence", test_monotonicity_true_to_true)


# 5) Idempotence: same input → same output & core tokens persist in the proof text.
def test_idempotence():
    val1, r1 = E_min(World)
    val2, r2 = E_min(World)
    assert val1 == val2, "Idempotence violated on boolean result."
    for token in ["∀x", "≥ 2", "Therefore"]:
        assert token in r1 and token in r2, f"Reason text missing token: {token}"

check("Idempotence of evaluation and proof tokens", test_idempotence)


# 6) If E(x) is True, then R must be True and count(H,F,¬U) ≥ 2 (small exhaustive grid).
def test_true_implies_premises():
    for rc in range(0, 4):
        for h in (False, True):
            for f in (False, True):
                for u in (False, True):
                    x = Cosmos(rc, h, f, u)
                    val, _ = E_min(x)
                    if val:
                        assert R(x) and at_least_two((H(x), F(x), (not U(x)))), \
                            "A True classification broke the definition's antecedent."

check("True ⇒ R & (H+F+¬U ≥ 2) (exhaustive small grid)", test_true_implies_premises)


# 7) Minimality w.r.t necessary parts on World: remove any necessary part → False.
def test_minimality_removals():
    # Remove R by dropping regimes to 1
    x1 = Cosmos(1, True, True, False)
    assert E_min(x1)[0] is False, "Removing R should falsify E."

    # Keep R but reduce two-of-three to one-of-three
    x2 = Cosmos(3, True, False, True)  # only H True; ¬U False (since U True)
    assert E_min(x2)[0] is False, "One-of-three should be insufficient."

    # Keep R but assume timeless uniqueness and drop F
    x3 = Cosmos(3, True, False, True)  # mirrors x2 explicitly
    assert E_min(x3)[0] is False, "Timeless uniqueness with weak support should fail."

check("Minimality of necessary conditions", test_minimality_removals)


# 8) Invariance to construction order (dict shuffle → same result).
def test_invariance_to_field_order():
    base = {
        "regime_count": 3,
        "history_dependence": True,
        "feedback_selection": True,
        "timeless_unique_law": False
    }
    keys = list(base.keys())
    random.shuffle(keys)
    shuffled = {k: base[k] for k in keys}
    x1 = Cosmos(**base)
    x2 = Cosmos(**shuffled)
    assert E_min(x1)[0] == E_min(x2)[0], "Permutation of input fields changed result."

check("Invariance to field order", test_invariance_to_field_order)


# ---------- End of file ----------

