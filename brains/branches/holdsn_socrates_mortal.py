#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
README (plain text)
===================
Purpose
-------
A tiny, self-contained “branch of insights” (in the spirit of
https://eyereasoner.github.io/eye/brains/) that performs the classic inference
**“Socrates is mortal”**. It showcases the Hayes–Menzel trick: treat predicates
and rules as *names* (intensions) and use a small fixed family of predicates to
apply them—here `Holds₁` for class membership and `Holds₂` for subclass
inclusion—so the core remains **first-order**.

Core idea (Hayes–Menzel, in friendly terms)
-------------------------------------------
- Predicates like `Human` and `Mortal` are **named objects** (think URIs).
- `Holds₁(S, a)` means “individual `a` is in the extension of the class named
  by `S`”.
- `Holds₂(SubClassOf, P, Q)` means “class `P` is included in class `Q`”.
- We compute the *closure* of facts under subclass propagation:
    if `Holds₂(SubClassOf, P, Q)` and `Holds₁(P, a)`, then infer `Holds₁(Q, a)`.

Typical question (printed by the program)
-----------------------------------------
Given:
- Facts: `Human(Socrates)`, `Human(Plato)`, `Greek(Socrates)`, `Immortal(Zeus)`.
- Rules: `Greek ⊆ Human` and `Human ⊆ Mortal`.

(1) Is Socrates mortal?  
(2) Which classes does Socrates belong to after closure?

What the program prints
-----------------------
1) **Model**  — domain, named classes, facts, and subclass rules.  
2) **Question** — the two items above.  
3) **Answer** — “Yes/No” and Socrates’s class set after closure.  
4) **Reason why** — a short mathematical-English explanation, plus the
   (deterministic) Kleene chain of membership closure.  
5) **Check (harness)** — 12 deterministic tests ensuring the inference and
   closure properties behave as expected.

How to run
----------
    python3 holdsn_socrates_mortal.py

No external dependencies; output is deterministic.
"""

from __future__ import annotations

from typing import Dict, Iterable, List, Set, Tuple

# =========================================================
# Model: individuals, class names (intensions), Holds₁/₂
# =========================================================

# Deterministic domain of individuals
D: Tuple[str, ...] = ("Socrates", "Plato", "Zeus")

# Namespace for names (URIs/strings as intensions)
EX = "ex:"

# ----------------- Unary: class names → member sets -----------------
# EXT1 maps a class-name (intension) to its extension (members)
EXT1: Dict[str, Set[str]] = {}

def define_class(name: str, members: Iterable[str]) -> str:
    """Register a named class with its (sorted) extension of members."""
    EXT1[name] = set(sorted(members))
    return name

def Holds1(cname: str, a: str) -> bool:
    """Holds₁(C, a): individual a is in the extension of the class named by C."""
    return a in EXT1.get(cname, set())

# Our class names (intensions)
Human     = define_class(EX + "Human",     ["Socrates", "Plato"])
Mortal    = define_class(EX + "Mortal",    [])          # intentionally empty; will be inferred
Greek     = define_class(EX + "Greek",     ["Socrates"])
Immortal  = define_class(EX + "Immortal",  ["Zeus"])

CLASSES: Tuple[str, ...] = (Greek, Human, Mortal, Immortal)

# ----------------- Binary: SubClassOf relation -----------------
# EXT2 maps relation names to sets of pairs (extensions)
EXT2: Dict[str, Set[Tuple[str, str]]] = {}

def define_relation(name: str, pairs: Iterable[Tuple[str, str]]) -> str:
    """Register a named binary relation with its extension (set of ordered pairs)."""
    EXT2[name] = {(a, b) for (a, b) in pairs}
    return name

def Holds2(rname: str, x: str, y: str) -> bool:
    """Holds₂(R, x, y): the pair ⟨x,y⟩ is in the extension of relation-name R."""
    return (x, y) in EXT2.get(rname, set())

SubClassOf = define_relation(EX + "SubClassOf", [
    (Greek, Human),    # Greeks are humans
    (Human, Mortal),   # Humans are mortal
    # (Immortal, Mortal) — intentionally *absent* (Zeus won’t be inferred mortal)
])

# =========================================================
# Closure machinery: subclass transitive closure + Kleene LFP
# =========================================================

def subclass_transitive_closure() -> Set[Tuple[str, str]]:
    """
    Compute the (non-reflexive) transitive closure of SubClassOf over known class names.
    If P⊆Q and Q⊆R, include P⊆R.
    """
    base = set(EXT2[SubClassOf])
    changed = True
    while changed:
        changed = False
        # Attempt to add P⊆R whenever P⊆Q and Q⊆R are present
        additions: Set[Tuple[str, str]] = set()
        for (p, q1) in sorted(base):
            for (q2, r) in sorted(base):
                if q1 == q2 and (p, r) not in base:
                    additions.add((p, r))
        if additions:
            base |= additions
            changed = True
    return base

SUBCLOS: Set[Tuple[str, str]] = subclass_transitive_closure()

def kleene_membership_closure(max_steps: int = 64) -> Tuple[Set[Tuple[str, str]], List[Set[Tuple[str, str]]]]:
    """
    Compute the least fixed point of membership under subclass propagation.
      Universe of facts: {(C, a) | C∈CLASSES, a∈D}.
      Operator F(S) = base_members ∪ { (Q, a) | (P, a)∈S and (P⊆Q)∈SUBCLOS }.
    Return (LFP, chain).
    """
    base_members: Set[Tuple[str, str]] = set()
    for C in CLASSES:
        for a in sorted(EXT1.get(C, set())):
            base_members.add((C, a))

    chain: List[Set[Tuple[str, str]]] = [set()]
    S: Set[Tuple[str, str]] = set()
    for _ in range(max_steps):
        S_next = set(base_members)
        for (P, a) in S:
            for (X, Y) in sorted(SUBCLOS):
                if P == X:
                    S_next.add((Y, a))
        chain.append(S_next)
        if S_next == S:
            return S_next, chain
        S = S_next
    # Should converge well before this
    return S, chain

# =========================================================
# Pretty printers (deterministic)
# =========================================================

def fmt_membership_pairs(S: Iterable[Tuple[str, str]]) -> str:
    seq = list(sorted(S))
    return "∅" if not seq else "{" + ", ".join(f"{c}({a})" for (c, a) in seq) + "}"

def fmt_classes_of(a: str, LFP: Set[Tuple[str, str]]) -> str:
    Cs = sorted({c for (c, x) in LFP if x == a})
    return "∅" if not Cs else "{" + ", ".join(Cs) + "}"

def fmt_chain(chain: List[Set[Tuple[str, str]]], limit: int = 6) -> str:
    parts = [fmt_membership_pairs(S) for S in chain[:limit]]
    if len(chain) > limit:
        parts.append("…")
    return " ⊆ ".join(parts)

# =========================================================
# The Branch: Model → Question → Answer → Reason why
# =========================================================

def print_model() -> None:
    print("Model")
    print("=====")
    print(f"Individuals D = {list(D)}")
    print()
    print("Signature")
    print("---------")
    print("• Holds₁(C, a): a ∈ extension of the class named by C (C is an *intension*).")
    print("• Holds₂(SubClassOf, P, Q): the class named P is included in the class named Q.")
    print()
    print("Named classes and base facts")
    print("----------------------------")
    for C in CLASSES:
        print(f"- {C:<12}: members = {sorted(EXT1.get(C, set()))}")
    print()
    print("Subclass rules")
    print("--------------")
    print("• " + ", ".join([f"{p.split(':')[1]} ⊆ {q.split(':')[1]}" for (p, q) in sorted(EXT2[SubClassOf])]))
    print()

def print_question() -> None:
    print("Question")
    print("========")
    print("Given facts Human(Socrates), Human(Plato), Greek(Socrates), Immortal(Zeus)")
    print("and rules Greek ⊆ Human and Human ⊆ Mortal:")
    print("(1) Is Socrates mortal?")
    print("(2) Which classes does Socrates belong to after closure?")
    print()

def compute_answer() -> Tuple[Set[Tuple[str, str]], List[Set[Tuple[str, str]]], bool, List[str]]:
    LFP, chain = kleene_membership_closure()
    is_socrates_mortal = ("ex:Mortal", "Socrates") in LFP
    socrates_classes = sorted([c for (c, a) in LFP if a == "Socrates"])
    return LFP, chain, is_socrates_mortal, socrates_classes

def print_answer(is_socrates_mortal: bool, socrates_classes: List[str]) -> None:
    print("Answer")
    print("======")
    print(f"Is Socrates mortal? {'Yes' if is_socrates_mortal else 'No'}")
    print("Classes(Socrates) after closure =", "{" + ", ".join(socrates_classes) + "}" if socrates_classes else "∅")
    print()

def print_reason(LFP: Set[Tuple[str, str]], chain: List[Set[Tuple[str, str]]]) -> None:
    print("Reason why")
    print("==========")
    print("We use a first-order core with named classes and two fixed predicates:")
    print("  • Holds₁(C,a) for membership, and Holds₂(SubClassOf,P,Q) for subclass.")
    print("From the rules Greek ⊆ Human and Human ⊆ Mortal, we infer Greek ⊆ Mortal by transitivity.")
    print("Since Greek(Socrates) (and also Human(Socrates)), subclass propagation yields Mortal(Socrates).")
    print()
    print("Kleene membership closure from base facts yields the ascending chain:")
    print("  " + fmt_chain(chain, limit=6))
    print(f"which stabilizes at LFP = {fmt_membership_pairs(LFP)}.")
    print()

# =========================================================
# Check (harness) — deterministic, ≥ 12 tests
# =========================================================

class CheckFailure(AssertionError):
    pass

def check(cond: bool, msg: str) -> None:
    if not cond:
        raise CheckFailure(msg)

def run_checks(LFP: Set[Tuple[str, str]], chain: List[Set[Tuple[str, str]]]) -> List[str]:
    notes: List[str] = []

    # 1) SubClassOf contains the intended rules
    check(Holds2(SubClassOf, Greek, Human), "Expected Greek ⊆ Human.")
    check(Holds2(SubClassOf, Human, Mortal), "Expected Human ⊆ Mortal.")
    notes.append("PASS 1: SubClassOf contains Greek⊆Human and Human⊆Mortal.")

    # 2) Transitive closure includes Greek ⊆ Mortal
    check((Greek, Mortal) in SUBCLOS, "Transitive closure should have Greek ⊆ Mortal.")
    notes.append("PASS 2: Transitive subclass includes Greek⊆Mortal.")

    # 3) First Kleene step adds at least the base facts
    base = {(C, a) for C in CLASSES for a in EXT1.get(C, set())}
    check(base.issubset(chain[1]), "First Kleene step must include base facts.")
    notes.append("PASS 3: Kleene step 1 contains all base facts.")

    # 4) Chain is ascending and stabilizes at LFP
    for i in range(len(chain) - 1):
        check(chain[i].issubset(chain[i+1]), "Chain must be ascending.")
    check(chain[-1] == LFP, "Chain must stabilize at LFP.")
    notes.append("PASS 4: Chain is ascending and stabilizes.")

    # 5) The main inference: Socrates is mortal
    check(("ex:Mortal", "Socrates") in LFP, "Socrates should be inferred mortal.")
    notes.append("PASS 5: Socrates is mortal is derived.")

    # 6) Plato also becomes mortal by Human ⊆ Mortal
    check(("ex:Mortal", "Plato") in LFP, "Plato should be inferred mortal.")
    notes.append("PASS 6: Plato is also inferred mortal.")

    # 7) Zeus remains not mortal (no Immortal ⊆ Mortal rule)
    check(("ex:Mortal", "Zeus") not in LFP, "Zeus must not be inferred mortal.")
    notes.append("PASS 7: Zeus is not inferred mortal (as intended).")

    # 8) Socrates has exactly the expected classes
    expected_socrates = {Greek, Human, Mortal}
    actual_socrates = {c for (c, a) in LFP if a == "Socrates"}
    check(actual_socrates == expected_socrates, f"Socrates classes mismatch: {actual_socrates}")
    notes.append("PASS 8: Socrates classes after closure are {Greek, Human, Mortal}.")

    # 9) Deterministic pretty-printing of membership set
    s1 = fmt_membership_pairs(LFP)
    s2 = fmt_membership_pairs(set(sorted(LFP)))
    check(s1 == s2, "Pretty-printer must be deterministic.")
    notes.append("PASS 9: Deterministic formatting is stable.")

    # 10) Idempotence of closure: applying one more step yields no change
    L2, _ = kleene_membership_closure()
    check(L2 == LFP, "Kleene closure should be a fixed point.")
    notes.append("PASS 10: Closure is idempotent (fixed point).")

    # 11) No spurious individuals/classes appear
    for (c, a) in LFP:
        check(c in CLASSES and a in D, "All derived facts must use known classes and individuals.")
    notes.append("PASS 11: No out-of-vocabulary facts were created.")

    # 12) Expected total number of facts (for this tiny model)
    # Base: Human={Socrates,Plato} (2), Greek={Socrates} (1), Immortal={Zeus} (1) → 4
    # Derived: Mortal={Socrates,Plato} (2) → total 6
    check(len(LFP) == 6, f"Expected 6 membership facts after closure, got {len(LFP)}.")
    notes.append("PASS 12: Total number of membership facts is exactly 6.")

    return notes

# =========================================================
# Main orchestration
# =========================================================

def main() -> None:
    print_model()
    print_question()

    LFP, chain, is_socrates_mortal, socrates_classes = compute_answer()
    print_answer(is_socrates_mortal, socrates_classes)
    print_reason(LFP, chain)

    print("Check (harness)")
    print("===============")
    try:
        notes = run_checks(LFP, chain)
    except CheckFailure as e:
        print("FAIL:", e)
        raise
    else:
        for line in notes:
            print(line)

if __name__ == "__main__":
    main()

