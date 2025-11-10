#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
open_socrates.py
================

What is this?
-------------
A tiny, auditable Python program that answers a central question of
Agnes Callard’s *Open Socrates: The Case for a Philosophical Life* (2025):

  “Should one lead a Socratic life — one organized around dialogic inquiry,
   intellectual humility, rigorous self-examination, and willingness to be
   transformed by better reasons?”

It follows the EYE “branches of insights – brains” discipline:
the script computes an Answer, emits a formally constrained “Reason why”
in mathematical English, and then runs a Check (harness) with multiple
falsifiable tests.

How does it formalize the “Socratic life”?
------------------------------------------
We encode a minimal, falsifiable criterion S_min:

  Definition D (S_min): For all lives x,
    S(x)  ⇐  D(x) ∧ E(x) ∧ H(x) ∧ ( R(x) + T(x) + Q(x) ≥ 2 ),

where
  D(x): dialogic inquiry — engages others in genuine argumentative dialogue,
  E(x): ongoing self-examination — regularly tests one’s commitments,
  H(x): intellectual humility — avows and tracks one’s ignorance,
  R(x): openness to refutation — revises beliefs when shown better reasons,
  T(x): willingness to be transformed — updates projects/relationships/actions,
  Q(x): orientation to “big questions” — actively pursues normative/existential questions.

Under this criterion, if a life exhibits {dialogue, self-examination, humility}
and at least two among {refutation-openness, transformative willingness, big-question
orientation}, then it counts as “Socratic” (S(x)).

What does it NOT claim?
-----------------------
It does not rank all possible lives or claim that only Socratic lives are good.
It evaluates a transparent, testable abstraction aligned with themes of
dialogue, humility, admission of error, and life-changing inquiry.

How to run
----------
  python3 open_socrates.py

What you will see
-----------------
1) The Question.
2) The Answer (Yes/No).
3) “Reason why” — a short proof-style narrative with quantifiers and bounds.
4) “Check (harness)” — >5 PASS/FAIL lines. Any failing check prints loudly.

How to extend
-------------
- Tighten S_min (e.g., require all of {R, T, Q}).
- Grade predicates (e.g., frequency of dialogue; strength of update).
- Instantiate additional “lives” to compare patterns across contexts.

Attributions
------------
- High-level framing reflects *Open Socrates* on dialogic inquiry, humility,
  argument, and transformation. This program abstracts those themes into a crisp,
  falsifiable criterion for a “Socratic life.”
"""

from dataclasses import dataclass
from itertools import product
import random
random.seed(2025)


# ---------- Formalization (predicates & definition) ----------

@dataclass(frozen=True)
class Life:
    dialogic_inquiry: bool          # D(x): sustained argument with others
    self_examination: bool          # E(x): regular testing of commitments
    intellectual_humility: bool     # H(x): avowal/monitoring of ignorance
    refutation_openness: bool       # R(x): willingness to concede & revise
    transformative_willingness: bool# T(x): readiness to change life on reasons
    big_questions_focus: bool       # Q(x): pursuit of normative/existential questions


def D(x: Life) -> bool: return bool(x.dialogic_inquiry)
def E(x: Life) -> bool: return bool(x.self_examination)
def H(x: Life) -> bool: return bool(x.intellectual_humility)
def R(x: Life) -> bool: return bool(x.refutation_openness)
def T(x: Life) -> bool: return bool(x.transformative_willingness)
def Q(x: Life) -> bool: return bool(x.big_questions_focus)

def at_least_two(triple: tuple[bool, bool, bool]) -> bool:
    return sum(1 for t in triple if t) >= 2


# Definition (S_min): Minimal criterion for a Socratic life.
# For all lives x,
#   S(x)  ⇐  D(x) ∧ E(x) ∧ H(x) ∧ ( R(x) + T(x) + Q(x) ≥ 2 ).
def S_min(x: Life) -> tuple[bool, str]:
    d, e, h, r, t, q = D(x), E(x), H(x), R(x), T(x), Q(x)
    k = at_least_two((r, t, q))
    holds = (d and e and h and k)

    # Build "Reason why" in mathematical English (auditable trail).
    subject = "L"  # symbol in the derivation; mapped to Example below
    premises_lines = [
        f"Premise 1: D({subject}) is {'true' if d else 'false'} (dialogic inquiry {'present' if d else 'absent'}).",
        f"Premise 2: E({subject}) is {'true' if e else 'false'} (self-examination {'present' if e else 'absent'}).",
        f"Premise 3: H({subject}) is {'true' if h else 'false'} (intellectual humility {'present' if h else 'absent'}).",
        f"Premise 4: R({subject}) is {'true' if r else 'false'} (refutation-openness {'present' if r else 'absent'}).",
        f"Premise 5: T({subject}) is {'true' if t else 'false'} (transformative willingness {'present' if t else 'absent'}).",
        f"Premise 6: Q({subject}) is {'true' if q else 'false'} (big-questions focus {'present' if q else 'absent'}).",
        f"Therefore among {{R({subject}), T({subject}), Q({subject})}} we have "
        f"{sum([r, t, q])} truths → condition (R+T+Q ≥ 2) is {'met' if k else 'not met'}."
    ]

    definition_line = (
        "Definition D (S_min): ∀x,  S(x) ⇐ [ D(x) ∧ E(x) ∧ H(x) ∧ ( R(x)+T(x)+Q(x) ≥ 2 ) ]."
    )
    conclusion_line = (
        f"Therefore, by D, S({subject}) is {'true' if holds else 'false'}."
    )

    reason = (
        "Mathematical-English derivation:\n"
        + definition_line + "\n"
        + "\n".join(premises_lines) + "\n"
        + conclusion_line
    )
    return holds, reason


# ---------- Instantiate the subject life ----------
# “Example” encodes the Socratic stance emphasized in the book: dialogue, humility,
# self-examination, plus (at least) two of {refutation-openness, transformative willingness, big questions}.
Example = Life(
    dialogic_inquiry=True,
    self_examination=True,
    intellectual_humility=True,
    refutation_openness=True,
    transformative_willingness=True,
    big_questions_focus=True
)


# ---------- Compute Answer + Reason ----------
answer, reason = S_min(Example)

print("Question")
print("========")
print("Should one lead a Socratic life (S_min)?\n")

print("Answer")
print("======")
print("Yes." if answer else "No.")
print()

print("Reason why")
print("==========")
# Replace the derivation symbol L with Example in the printed proof narrative.
print(
    reason.replace("S(L)", "S(Example)")
          .replace("D(L)", "D(Example)")
          .replace("E(L)", "E(Example)")
          .replace("H(L)", "H(Example)")
          .replace("R(L)", "R(Example)")
          .replace("T(L)", "T(Example)")
          .replace("Q(L)", "Q(Example)")
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

# 1) Null life should not satisfy S.
def test_null_is_false():
    x = Life(False, False, False, False, False, False)
    val, _ = S_min(x)
    assert val is False, "Null life misclassified as S_min-true."

check("Null life is not S_min-true", test_null_is_false)


# 2) Boundary: D,E,H true and exactly two among {R,T,Q} true ⇒ S true.
def test_boundary_two_of_three_is_true():
    x = Life(True, True, True, True, False, True)  # R=True, Q=True, T=False
    val, _ = S_min(x)
    assert val is True, "Boundary case (two-of-three with D,E,H) should be True."

check("Boundary two-of-three with D,E,H holds", test_boundary_two_of_three_is_true)


# 3) Necessity of D: without dialogue, S must be False even if others hold.
def test_dialogue_is_necessary():
    x = Life(False, True, True, True, True, True)
    val, _ = S_min(x)
    assert val is False, "D(x) is necessary but was not enforced."

check("Dialogic inquiry (D) is necessary", test_dialogue_is_necessary)


# 4) Necessity of E: without self-examination, S must be False.
def test_self_examination_is_necessary():
    x = Life(True, False, True, True, True, True)
    val, _ = S_min(x)
    assert val is False, "E(x) is necessary but was not enforced."

check("Self-examination (E) is necessary", test_self_examination_is_necessary)


# 5) Necessity of H: without humility, S must be False.
def test_humility_is_necessary():
    x = Life(True, True, False, True, True, True)
    val, _ = S_min(x)
    assert val is False, "H(x) is necessary but was not enforced."

check("Intellectual humility (H) is necessary", test_humility_is_necessary)


# 6) Monotonicity: adding capabilities cannot flip True → False.
def test_monotonicity_true_to_true():
    base = Life(True, True, True, True, False, True)  # already True: D,E,H + {R,Q}
    base_val, _ = S_min(base)
    assert base_val is True, "Precondition failed (base not True)."
    supersets = [
        Life(True, True, True, True, True, True),
        Life(True, True, True, True, True, True)
    ]
    for sup in supersets:
        sup_val, _ = S_min(sup)
        assert sup_val is True, "Monotonicity violated: added evidence flipped True to False."

check("Monotonicity True→True under added evidence", test_monotonicity_true_to_true)


# 7) Idempotence: same input → same output & proof tokens persist.
def test_idempotence():
    val1, r1 = S_min(Example)
    val2, r2 = S_min(Example)
    assert val1 == val2, "Idempotence violated on boolean result."
    for token in ["∀x", "≥ 2", "Therefore"]:
        assert token in r1 and token in r2, f"Reason text missing token: {token}"

check("Idempotence of evaluation and proof tokens", test_idempotence)


# 8) If S(x) is True, then D,E,H must be True and count(R,T,Q) ≥ 2 (small exhaustive grid).
def test_true_implies_premises():
    for d in (False, True):
        for e in (False, True):
            for h in (False, True):
                for r in (False, True):
                    for t in (False, True):
                        for q in (False, True):
                            x = Life(d, e, h, r, t, q)
                            val, _ = S_min(x)
                            if val:
                                assert D(x) and E(x) and H(x) and at_least_two((R(x), T(x), Q(x))), \
                                    "A True classification broke the definition's antecedent."

check("True ⇒ D & E & H & (R+T+Q ≥ 2) (exhaustive small grid)", test_true_implies_premises)


# 9) Minimality wrt necessary parts on Example: remove a necessary part → False.
def test_minimality_removals():
    x1 = Life(True, True, True, False, True, True)  # remove R only (still ≥2 via T,Q) → True
    assert S_min(x1)[0] is True, "Control failed: still two-of-three should remain True."
    x2 = Life(True, True, False, True, True, True)  # remove H → False
    assert S_min(x2)[0] is False, "Removing H should falsify S."
    x3 = Life(True, False, True, True, True, False)  # remove E → False
    assert S_min(x3)[0] is False, "Removing E should falsify S."

check("Minimality of necessary conditions", test_minimality_removals)


# 10) Two-of-three requirement: with D,E,H True but only one of {R,T,Q} True ⇒ False.
def test_two_of_three_requirement():
    x = Life(True, True, True, True, False, False)  # only R True
    val, _ = S_min(x)
    assert val is False, "Two-of-three requirement not enforced."

check("Two-of-three requirement enforced", test_two_of_three_requirement)


# ---------- End of file ----------

