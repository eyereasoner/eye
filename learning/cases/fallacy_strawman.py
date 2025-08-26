#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Strawman Fallacy — ARC (Answer / Reason / Check), self-contained

Intuition (toy detector)
  An argument A commits a strawman when it attacks a *misrepresentation* of the
  opponent’s claim rather than the claim itself. Two common misreps:
    • OVERSTATEMENT: attacking a stronger/extreme version the opponent didn’t claim.
    • TOPIC SHIFT: attacking something unrelated to the opponent’s claim.

Encoding
  opponent_claim(A) = C_op              – what the opponent actually proposed
  attacked_target(A) = C_tar            – what the arg actually attacks
  overstates(X, Y)                      – X ⇒ Y but not vice versa (X is stronger than Y)
  unrelated(X, Y)                       – X and Y are about different topics
  equivalent(X, Y)                      – interchangeable paraphrases (no strawman if attacked)

Detection
  strawman(A) if attacked_target(A) ≠ opponent_claim(A) AND
                 [ overstates(attacked, opponent) OR unrelated(attacked, opponent) ]
  but NOT if equivalent(attacked, opponent).
"""

from typing import Dict, List, Set, Tuple

# ───────────────────────────── Example arguments ─────────────────────────────
sentences: Dict[str, str] = {
    "Arg1": "They say we should regulate emissions; but banning all cars is absurd!",
    "Arg2": "My opponent wants background checks; confiscating all guns violates rights!",
    "Arg3": "They propose trimming defense waste; abolishing the military is reckless!",
    "Arg4": "They want to raise property tax; here’s why that harms renters.",
}

# What the opponent actually claimed (for each argument)
opponent_claim: Dict[str, str] = {
    "Arg1": "regulate_emissions",
    "Arg2": "background_checks",
    "Arg3": "cut_defense_waste",
    "Arg4": "raise_property_tax",
}

# What the argument *actually attacks*
attacked_target: Dict[str, str] = {
    "Arg1": "ban_all_cars",       # stronger than regulate_emissions
    "Arg2": "confiscate_all_guns",# stronger than background_checks
    "Arg3": "abolish_military",   # unrelated to 'cut waste'
    "Arg4": "raise_property_tax", # addresses the real claim (no strawman)
}

# Background relations
overstate_pairs: Set[Tuple[str, str]] = {
    ("ban_all_cars", "regulate_emissions"),
    ("confiscate_all_guns", "background_checks"),
    # Note: 'abolish_military' is not modeled as overstate of 'cut_defense_waste'; it's a topic shift.
}

unrelated_pairs: Set[Tuple[str, str]] = {
    ("abolish_military", "cut_defense_waste"),
    ("cut_defense_waste", "abolish_military"),
}

equivalent_pairs: Set[Tuple[str, str]] = set()
# add paraphrase pairs later with equivalent_pairs.add(("raise_property_tax","increase_property_tax"))

# ───────────────────────────── Inference helpers ─────────────────────────────
def equivalent(x: str, y: str) -> bool:
    return (x, y) in equivalent_pairs or (y, x) in equivalent_pairs or x == y

def overstates(x: str, y: str) -> bool:
    return (x, y) in overstate_pairs

def unrelated(x: str, y: str) -> bool:
    return (x, y) in unrelated_pairs or (y, x) in unrelated_pairs

def detect_strawman(aid: str) -> Tuple[bool, List[str]]:
    """Return (is_strawman, reasons)."""
    op = opponent_claim[aid]
    tar = attacked_target[aid]
    if equivalent(tar, op):
        return (False, ["attacks the actual (or equivalent) claim"])
    if tar != op and (overstates(tar, op) or unrelated(tar, op)):
        reasons = []
        if overstates(tar, op):
            reasons.append(f"attacks an OVERSTATEMENT of the claim: '{tar}' ⊃ '{op}'")
        if unrelated(tar, op):
            reasons.append(f"attacks an UNRELATED target: '{tar}' vs '{op}'")
        return (True, reasons)
    # Different but neither overstatement nor marked unrelated → conservative: not flagged
    if tar != op:
        return (False, [f"attacks a different but not-proven-misrepresentative target: '{tar}' vs '{op}'"])
    return (False, ["attacks the actual claim"])

# ────────────────────────────────── ARC: Answer ─────────────────────────────
def print_answer() -> None:
    print("Answer")
    print("======")
    results: Dict[str, bool] = {}

    for aid, text in sentences.items():
        op = opponent_claim[aid]
        tar = attacked_target[aid]
        is_sm, reasons = detect_strawman(aid)

        print(f"\n=== {aid}")
        print(f"Text:        {text}")
        print(f"Opponent:    {op}")
        print(f"Attacked:    {tar}")

        print("Analysis:")
        for r in reasons:
            print("  - " + r)

        print("Result:", "STRAW MAN" if is_sm else "no strawman detected")
        results[aid] = is_sm

    print("\nSummary")
    for aid in sorted(sentences):
        print(f"  {aid}: {'strawman' if results[aid] else 'ok'}")

# ───────────────────────────────── ARC: Reason why ──────────────────────────
def print_reason() -> None:
    print("\nReason why")
    print("==========")
    print("We flag strawman when the attacked target differs from the opponent’s claim")
    print("AND is either a stronger/extreme version (overstatement) or off-topic.")
    print("Attacking an equivalent paraphrase, or the original claim itself, is not a strawman.")
    print("Examples:")
    print("  • Arg1/Arg2: stronger claims ('ban all cars', 'confiscate all guns') replace modest proposals.")
    print("  • Arg3: topic shift from 'cut waste' to 'abolish the military'.")
    print("  • Arg4: directly addresses the actual claim (no strawman).")

# ─────────────────────────────── ARC: Check (harness) ───────────────────────
def print_check() -> None:
    print("\nCheck (harness)")
    print("===============")
    ok_all = True

    # 1) Expected classifications
    expected = {"Arg1": True, "Arg2": True, "Arg3": True, "Arg4": False}
    ok_cls = True
    for aid, want in expected.items():
        got, _ = detect_strawman(aid)
        if got != want:
            ok_cls = False
            print(f"  MISMATCH {aid}: got {got}, want {want}")
    print(f"Expected classifications hold? {ok_cls}")
    ok_all &= ok_cls

    # 2) Soundness: if we change Arg1 to attack the actual claim, the flag should clear
    saved_tar = attacked_target["Arg1"]
    attacked_target["Arg1"] = opponent_claim["Arg1"]
    cleared, _ = detect_strawman("Arg1")
    print(f"Arg1 clears when it attacks the actual claim? {not cleared}")
    attacked_target["Arg1"] = saved_tar
    ok_all &= (not cleared)

    # 3) Soundness: if we mark Arg2's target as equivalent paraphrase, the flag should clear
    equivalent_pairs.add( (attacked_target["Arg2"], opponent_claim["Arg2"]) )
    cleared2, _ = detect_strawman("Arg2")
    print(f"Arg2 clears when target is marked equivalent? {not cleared2}")
    equivalent_pairs.clear()
    ok_all &= (not cleared2)

    # 4) Specificity: if Arg3’s relation isn’t overstate or unrelated, we don’t flag (conservative)
    #    Temporarily remove unrelated marker for Arg3
    unrelated_pairs_backup = set(unrelated_pairs)
    for p in [("abolish_military","cut_defense_waste"), ("cut_defense_waste","abolish_military")]:
        unrelated_pairs.discard(p)
    conservative, _ = detect_strawman("Arg3")
    print(f"Without relation evidence, Arg3 not flagged (conservative)? {not conservative}")
    unrelated_pairs.update(unrelated_pairs_backup)
    ok_all &= (not conservative)

    # 5) Determinism / idempotence
    a = detect_strawman("Arg4")
    b = detect_strawman("Arg4")
    print(f"Deterministic (same inputs ⇒ same result)? {a == b}")
    ok_all &= (a == b)

    print(f"\nAll checks passed? {ok_all}")

# ─────────────────────────────────── Main ───────────────────────────────────
if __name__ == "__main__":
    print_answer()
    print_reason()
    print_check()

