#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
light_eaters.py
===============

What is this?
-------------
A tiny, auditable Python program that answers the main question at the heart of
Zoë Schlanger’s *The Light Eaters*: “Are plants intelligent?” It follows the
EYE “branches of insights – brains” discipline: the script computes an Answer,
emits a formally constrained “Reason why” in mathematical English, and then
runs a Check (harness) with multiple falsifiable tests.

How does it define “intelligence”?
----------------------------------
We use a minimal, falsifiable criterion I_min:

  Definition D (I_min): For all systems x,
    I(x)  ⇐  S(x) ∧ G(x) ∧ ( M(x) + N(x) + C(x) ≥ 2 ),

where
  S(x): multimodal sensing (at least 2 distinct modalities),
  G(x): goal-directed adaptation (actions that improve a utility proxy),
  M(x): memory (internal state persists and guides later behavior),
  N(x): networked internal signaling/integration,
  C(x): communication that alters other organisms’ behavior.

Under this criterion, the program shows I(Plants) is True and prints a
mathematical-English derivation explaining why.

What does it NOT claim?
-----------------------
It does not assert consciousness or human-like cognition. It evaluates a
minimum, transparent criterion that many embodied systems could satisfy.
It’s a compact, testable abstraction — a tool for thought — not a verdict
on philosophical debates.

How to run
----------
  python3 light_eaters.py

What you will see
-----------------
1) The Question.
2) The Answer (Yes/No).
3) “Reason why” — a short proof-style narrative with quantifiers and bounds.
4) “Check (harness)” — >5 PASS/FAIL lines. Any failing check will print loudly.

How to extend
-------------
- Tweak the thresholds (e.g., require ≥3 of {M,N,C}).
- Add or refine capability predicates.
- Replace “Plants” with other systems to compare outcomes.
- Add domain-specific checks (e.g., conservation laws, monotonicity under noise).

Attributions
------------
- Main question context drawn from Schlanger’s *The Light Eaters* (2024).
- The program’s structure is inspired by the EYE “branches of insights – brains”
  style: compute an answer, state reasons, run checks.
"""

from dataclasses import dataclass
from itertools import product
import random
random.seed(7)


# ---------- Formalization (premises & definition) ----------

@dataclass(frozen=True)
class System:
    # Capabilities (booleans, except modalities_count is an int).
    modalities_count: int                 # number of distinct sensed exogenous variables
    goal_directed_adaptation: bool        # actions that reliably increase a utility proxy (e.g., light/fitness proxy)
    memory: bool                          # internal state persists and influences later decisions (priming/circadian)
    networked_signaling: bool             # internal integration across parts (e.g., electrical/chemical long-distance)
    communication: bool                   # alters other organisms' behavior (e.g., VOCs to recruit predators)


def S(x: System) -> bool:
    """Multimodal sensing predicate."""
    return x.modalities_count >= 2


def G(x: System) -> bool:
    """Goal-directed adaptation predicate."""
    return bool(x.goal_directed_adaptation)


def M(x: System) -> bool:
    return bool(x.memory)


def N(x: System) -> bool:
    return bool(x.networked_signaling)


def C(x: System) -> bool:
    return bool(x.communication)


def at_least_two(triple: tuple[bool, bool, bool]) -> bool:
    return sum(bool(t) for t in triple) >= 2


# Definition (I_min): Minimal Intelligence
# For all systems x,
#   I(x)  ⇐  S(x) ∧ G(x) ∧ ( M(x) + N(x) + C(x) ≥ 2 ).
def I_min(x: System) -> tuple[bool, str]:
    s, g, m, n, c = S(x), G(x), M(x), N(x), C(x)
    twoof = at_least_two((m, n, c))

    holds = (s and g and twoof)

    # Build "Reason why" in mathematical English (auditable trail).
    # Use P to denote the subject system when it's Plants; otherwise generic x.
    subject = "P"  # name used in the derivation (we'll map to Plants in the header below)
    premises_lines = [
        f"Premise 1: S({subject}) is {'true' if s else 'false'} because modalities_count({subject}) = {x.modalities_count} {'≥' if s else '<'} 2.",
        f"Premise 2: G({subject}) is {'true' if g else 'false'} (goal-directed adaptation {'present' if g else 'absent'}).",
        f"Premise 3: Among {{M({subject}), N({subject}), C({subject})}} we have "
        f"{sum([m, n, c])} truths → condition (M+N+C ≥ 2) is {'met' if twoof else 'not met'}."
    ]

    definition_line = (
        "Definition D (I_min): ∀x,  I(x) ⇐ [ S(x) ∧ G(x) ∧ ( M(x)+N(x)+C(x) ≥ 2 ) ]."
    )
    conclusion_line = (
        f"Therefore, by D, I({subject}) is {'true' if holds else 'false'}."
    )

    reason = (
        "Mathematical-English derivation:\n"
        + definition_line + "\n"
        + "\n".join(premises_lines) + "\n"
        + conclusion_line
    )
    return holds, reason


# ---------- Instantiate the subject: Plants ----------
# We encode widely reported plant capacities at the level of *capability predicates*.
# (This program does not claim consciousness; it evaluates a minimal intelligence criterion.)
Plants = System(
    modalities_count=4,           # e.g., light, chemical, touch, sound/vibration (≥2)
    goal_directed_adaptation=True,# e.g., phototropism/hydrotropism/root foraging improve utility proxies
    memory=True,                  # e.g., priming & circadian entrainment
    networked_signaling=True,     # e.g., electrical/chemical long-distance signaling
    communication=True            # e.g., volatile organic compounds recruiting defenders
)


# ---------- Compute Answer + Reason ----------
answer, reason = I_min(Plants)

print("Question")
print("========")
print("Are plants intelligent (under the minimal criterion I_min)?\n")

print("Answer")
print("======")
print("Yes." if answer else "No.")
print()

print("Reason why")
print("==========")
# Replace subject symbol P with Plants in the printed proof narrative.
print(reason.replace("I(P)", "I(Plants)")
            .replace("S(P)", "S(Plants)")
            .replace("G(P)", "G(Plants)")
            .replace("M(P)", "M(Plants)")
            .replace("N(P)", "N(Plants)")
            .replace("C(P)", "C(Plants)")
            .replace("modalities_count(P)", "modalities_count(Plants)"))
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

# 1) Null system should not be intelligent.
def test_null_is_false():
    x = System(0, False, False, False, False)
    val, _ = I_min(x)
    assert val is False, "Null system misclassified as intelligent."

check("Null system is not intelligent", test_null_is_false)


# 2) Boundary: exactly two of {M,N,C} with S and G true => intelligent.
def test_boundary_two_of_three_is_true():
    x = System(2, True, True, False, True)  # M=True, N=False, C=True
    val, _ = I_min(x)
    assert val is True, "Boundary case (two-of-three) should be True."

check("Boundary two-of-three with S & G holds", test_boundary_two_of_three_is_true)


# 3) Necessity of S: without S, classification must be False even if others hold.
def test_sensing_is_necessary():
    x = System(1, True, True, True, True)  # S is False (1 < 2)
    val, _ = I_min(x)
    assert val is False, "S(x) is necessary but was not enforced."

check("Sensing (S) is necessary", test_sensing_is_necessary)


# 4) Monotonicity: adding capabilities cannot flip True -> False.
def test_monotonicity():
    base = System(2, True, True, True, False)  # already True
    base_val, _ = I_min(base)
    assert base_val is True, "Precondition failed (base not True)."
    supersets = [
        System(3, True, True, True, True),
        System(4, True, True, True, True)
    ]
    for sup in supersets:
        sup_val, _ = I_min(sup)
        assert sup_val is True, "Monotonicity violated: added evidence flipped True to False."

check("Monotonicity True→True under added evidence", test_monotonicity)


# 5) Idempotence: same input → same output & same conclusion truth value.
def test_idempotence():
    val1, r1 = I_min(Plants)
    val2, r2 = I_min(Plants)
    assert val1 == val2, "Idempotence violated on boolean result."
    # Don't require identical string bytes (could vary), but core tokens must exist.
    for token in ["∀x", "≥ 2", "Therefore"]:
        assert token in r1 and token in r2, f"Reason text missing token: {token}"

check("Idempotence of evaluation and core proof tokens", test_idempotence)


# 6) Contrapositive property for positives: if I(x) is True, then S and G must be True and (M+N+C ≥ 2).
def test_true_implies_premises():
    for mod in range(0, 5):
        for g in (False, True):
            for m in (False, True):
                for n in (False, True):
                    for c in (False, True):
                        x = System(mod, g, m, n, c)
                        val, _ = I_min(x)
                        if val:
                            assert S(x) and G(x) and at_least_two((M(x), N(x), C(x))), \
                                "A True classification broke the definition's antecedent."

check("True ⇒ S & G & two-of-three (exhaustive small grid)", test_true_implies_premises)


# 7) Minimality wrt necessary parts on Plants: remove any necessary part → False.
def test_minimality_removals():
    # Remove S by dropping modalities to 1
    x1 = System(1, True, True, True, True)
    assert I_min(x1)[0] is False, "Removing S should falsify I."

    # Remove G
    x2 = System(4, False, True, True, True)
    assert I_min(x2)[0] is False, "Removing G should falsify I."

    # Keep S and G but reduce two-of-three to one-of-three
    x3 = System(4, True, True, False, False)  # only M
    assert I_min(x3)[0] is False, "One-of-three should be insufficient."

check("Minimality of necessary conditions", test_minimality_removals)


# 8) Invariance to construction order (dict shuffle → same result).
def test_invariance_to_order():
    base = {
        "modalities_count": 4,
        "goal_directed_adaptation": True,
        "memory": True,
        "networked_signaling": True,
        "communication": True
    }
    keys = list(base.keys())
    random.shuffle(keys)
    shuffled = {k: base[k] for k in keys}
    x1 = System(**base)
    x2 = System(**shuffled)
    assert I_min(x1)[0] == I_min(x2)[0], "Permutation of input fields changed result."

check("Invariance to field order", test_invariance_to_order)


# ---------- End of file ----------

