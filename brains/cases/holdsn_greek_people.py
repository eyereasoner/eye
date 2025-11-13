#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
README (plain text)
===================
Purpose
-------
A small “branch of insights” (in the spirit of https://eyereasoner.github.io/eye/brains/)
that *quantifies over predicates* while keeping a first-order core via the
Hayes–Menzel idea. Predicates (classes) are **named objects** (intensions like
"ex:Greek"), and we use fixed predicates:

  • Holds₁(C, a)   — membership: a is in the extension of class-name C
  • Holds₂(SubClassOf, P, Q) — subclass: class-name P’s extension is included in Q’s

Quantifying over predicates = quantifying over **names** P drawn from a finite
vocabulary `CLASSES`. This keeps everything first-order, while the behavior still
looks second-order (ranging over predicates).

Typical Question
----------------
Given base facts and subclass rules:

  Base facts:
    Greek(Socrates), Philosopher(Socrates), Philosopher(Plato),
    Human(Plato), Human(Aristotle), Olympian(Zeus).

  Rules:
    Greek ⊆ Human, Philosopher ⊆ Human, Human ⊆ Mortal.

(1) ∃P [ P is a class-name ∧ P(Socrates) ∧ P ⊆ Mortal ] ?   (give witnesses)  
(2) ∀P [ (P ⊆ Human ∧ P(Socrates)) → Mortal(Socrates) ] ?  
(3) List all classes that Socrates belongs to after closure.

What the program prints
-----------------------
1) **Model**  — individuals, class names, base facts, subclass rules.  
2) **Question** — the three items above.  
3) **Answer** — Yes/No + witnesses for (1); truth-value for (2); the set for (3).  
4) **Reason why** — mathematical-English explanation with a tiny closure chain.  
5) **Check (harness)** — 12 deterministic tests (closure, quantifiers, monotonicity).

How to run
----------
    python3 holdsn_greek_people.py

No external dependencies; deterministic execution and output.
"""

from __future__ import annotations
from typing import Dict, Iterable, List, Set, Tuple

# =========================================================
# Model: individuals, class names (intensions), Holds₁/₂
# =========================================================

# Deterministic domain of individuals
D: Tuple[str, ...] = ("Socrates", "Plato", "Aristotle", "Zeus")

# Namespace for names (URIs/strings as intensions)
EX = "ex:"

# ---------- Unary: class names → member sets ----------
EXT1: Dict[str, Set[str]] = {}

def define_class(name: str, members: Iterable[str]) -> str:
    """Register a named class with its (sorted) extension of members."""
    EXT1[name] = set(sorted(members))
    return name

def Holds1(cname: str, a: str) -> bool:
    """Holds₁(C, a): individual a is in the extension of the class named by C."""
    return a in EXT1.get(cname, set())

# Class (predicate) names (intensions)
Greek       = define_class(EX + "Greek",       ["Socrates"])
Philosopher = define_class(EX + "Philosopher", ["Socrates", "Plato"])
Human       = define_class(EX + "Human",       ["Plato", "Aristotle"])  # Socrates derived via subclass
Mortal      = define_class(EX + "Mortal",      [])                      # to be derived
Olympian    = define_class(EX + "Olympian",    ["Zeus"])

# Universe of predicate *names* we will quantify over
CLASSES: Tuple[str, ...] = (Greek, Philosopher, Human, Mortal, Olympian)

# ---------- Binary: SubClassOf relation ----------
EXT2: Dict[str, Set[Tuple[str, str]]] = {}

def define_relation(name: str, pairs: Iterable[Tuple[str, str]]) -> str:
    """Register a named binary relation with its extension (set of ordered pairs)."""
    EXT2[name] = {(a, b) for (a, b) in pairs}
    return name

def Holds2(rname: str, x: str, y: str) -> bool:
    """Holds₂(R, x, y): the pair ⟨x,y⟩ is in the extension of relation-name R."""
    return (x, y) in EXT2.get(rname, set())

SubClassOf = define_relation(EX + "SubClassOf", [
    (Greek,       Human),
    (Philosopher, Human),
    (Human,       Mortal),
    # (Olympian, Mortal) — intentionally absent (Zeus is not inferred mortal)
])

# =========================================================
# Subclass transitive closure & membership closure (Kleene)
# =========================================================

def subclass_transitive_closure() -> Set[Tuple[str, str]]:
    """
    Compute the (non-reflexive) transitive closure of SubClassOf over known class names:
      if P⊆Q and Q⊆R, include P⊆R.
    """
    base = set(EXT2[SubClassOf])
    changed = True
    while changed:
        changed = False
        add: Set[Tuple[str, str]] = set()
        for (p, q1) in sorted(base):
            for (q2, r) in sorted(base):
                if q1 == q2 and (p, r) not in base:
                    add.add((p, r))
        if add:
            base |= add
            changed = True
    return base

SUBCLOS: Set[Tuple[str, str]] = subclass_transitive_closure()

def subclass_leq(P: str, Q: str) -> bool:
    """Reflexive-transitive subclass check: P ⊆* Q  (reflexive on names)."""
    return P == Q or (P, Q) in SUBCLOS or (P, Q) in EXT2[SubClassOf]

def kleene_membership_closure(max_steps: int = 64) -> Tuple[Set[Tuple[str, str]], List[Set[Tuple[str, str]]]]:
    """
    Least fixed point of membership under subclass propagation.
      Universe of facts: {(C, a) | C∈CLASSES, a∈D}.
      F(S) = base_members ∪ { (Q, a) | (P, a)∈S and P ⊆* Q }.
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
            for Q in CLASSES:
                if subclass_leq(P, Q):
                    S_next.add((Q, a))
        chain.append(S_next)
        if S_next == S:
            return S_next, chain
        S = S_next
    return S, chain  # defensive (should converge quickly)

# =========================================================
# Quantification over predicate *names*
# =========================================================

def exists_P_such_that_member_and_subclass_to_mortal(a: str, LFP: Set[Tuple[str, str]]) -> List[str]:
    """Return all predicate-names P with P(a) and P ⊆* Mortal (witnesses for ∃P …)."""
    return [P for P in CLASSES if (P, a) in LFP and subclass_leq(P, Mortal)]

def forall_P_human_implies_mortal_for(a: str, LFP: Set[Tuple[str, str]]) -> bool:
    """Check ∀P: (P ⊆* Human ∧ P(a)) → Mortal(a)."""
    for P in CLASSES:
        if subclass_leq(P, Human) and (P, a) in LFP:
            if (Mortal, a) not in LFP:
                return False
    return True

# =========================================================
# Pretty helpers (deterministic)
# =========================================================

def local(name: str) -> str:
    """Drop 'ex:' for pretty printing."""
    return name.split(":", 1)[1] if ":" in name else name

def fmt_membership(S: Iterable[Tuple[str, str]]) -> str:
    seq = list(sorted(S))
    return "∅" if not seq else "{" + ", ".join(f"{local(c)}({a})" for (c, a) in seq) + "}"

def fmt_classes_of(a: str, LFP: Set[Tuple[str, str]]) -> str:
    Cs = sorted({local(c) for (c, x) in LFP if x == a})
    return "∅" if not Cs else "{" + ", ".join(Cs) + "}"

def fmt_chain(chain: List[Set[Tuple[str, str]]], limit: int = 6) -> str:
    parts = [fmt_membership(S) for S in chain[:limit]]
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
        print(f"- {local(C):<12}: members = {sorted(EXT1.get(C, set()))}")
    print()
    print("Subclass rules")
    print("--------------")
    print("• " + ", ".join([f"{local(p)} ⊆ {local(q)}" for (p, q) in sorted(EXT2[SubClassOf])]))
    print()

def print_question() -> None:
    print("Question")
    print("========")
    print("(1) ∃P  [ P is a class-name ∧ P(Socrates) ∧ P ⊆ Mortal ] ?  (list witnesses)")
    print("(2) ∀P  [ (P ⊆ Human ∧ P(Socrates)) → Mortal(Socrates) ] ?")
    print("(3) What are the classes of Socrates after closure?")
    print()

def compute_answer():
    LFP, chain = kleene_membership_closure()
    witnesses = exists_P_such_that_member_and_subclass_to_mortal("Socrates", LFP)
    universal = forall_P_human_implies_mortal_for("Socrates", LFP)
    classes_socrates = sorted({local(C) for (C, a) in LFP if a == "Socrates"})
    return LFP, chain, witnesses, universal, classes_socrates

def print_answer(witnesses: List[str], universal: bool, classes_socrates: List[str]) -> None:
    print("Answer")
    print("======")
    print("(1) Exists P ?  " + ("Yes" if witnesses else "No"))
    if witnesses:
        print("    Witnesses:", "{" + ", ".join(local(w) for w in sorted(witnesses)) + "}")
    print("(2) For all P ? " + ("Yes" if universal else "No"))
    print("(3) Classes(Socrates) =", "{" + ", ".join(classes_socrates) + "}" if classes_socrates else "∅")
    print()

def print_reason(LFP: Set[Tuple[str, str]], chain: List[Set[Tuple[str, str]]]) -> None:
    print("Reason why")
    print("==========")
    print("We treat predicates as *names* and mediate application with fixed predicates:")
    print("  • Holds₁(C,a) for membership, Holds₂(SubClassOf,P,Q) for subclass.")
    print("Subclass rules Greek⊆Human and Philosopher⊆Human, together with Human⊆Mortal,")
    print("yield (by transitivity) Greek⊆Mortal and Philosopher⊆Mortal. Since Greek(Socrates)")
    print("and Philosopher(Socrates), the existential (1) holds with witnesses {Greek, Philosopher}")
    print("(and also the trivial witness Mortal, since Mortal(Socrates) will hold in the closure).")
    print("For (2), whenever P⊆Human and P(Socrates), subclass propagation implies Mortal(Socrates).")
    print()
    print("Kleene membership closure from base facts yields the ascending chain:")
    print("  " + fmt_chain(chain, limit=5))
    print(f"which stabilizes at LFP = {fmt_membership(LFP)}.")
    print()

# =========================================================
# Check (harness) — deterministic, ≥ 12 tests
# =========================================================

class CheckFailure(AssertionError):
    pass

def check(cond: bool, msg: str) -> None:
    if not cond:
        raise CheckFailure(msg)

def run_checks(LFP: Set[Tuple[str, str]], chain: List[Set[Tuple[str, str]]], witnesses: List[str], universal: bool) -> List[str]:
    notes: List[str] = []

    # 1) SubClassOf contains intended rules
    check(Holds2(SubClassOf, Greek, Human), "Expected Greek ⊆ Human.")
    check(Holds2(SubClassOf, Philosopher, Human), "Expected Philosopher ⊆ Human.")
    check(Holds2(SubClassOf, Human, Mortal), "Expected Human ⊆ Mortal.")
    notes.append("PASS 1: Intended subclass rules present.")

    # 2) Transitive closure includes Greek⊆Mortal and Philosopher⊆Mortal
    check((Greek, Mortal) in SUBCLOS and (Philosopher, Mortal) in SUBCLOS, "Transitive closure should include P⊆Mortal.")
    notes.append("PASS 2: Transitive subclass closure correct.")

    # 3) First Kleene step contains all base facts
    base = {(C, a) for C in CLASSES for a in EXT1.get(C, set())}
    check(base.issubset(chain[1]), "First Kleene step must include base facts.")
    notes.append("PASS 3: Base facts included in step 1.")

    # 4) Chain is ascending and stabilizes at LFP
    for i in range(len(chain) - 1):
        check(chain[i].issubset(chain[i+1]), "Chain must be ascending.")
    check(chain[-1] == LFP, "Chain must stabilize at LFP.")
    notes.append("PASS 4: Kleene chain is ascending and stabilizes.")

    # 5) Main derived facts: Human(Socrates) and Mortal(Socrates)
    check(("ex:Human", "Socrates") in LFP, "Human(Socrates) must be derived.")
    check(("ex:Mortal", "Socrates") in LFP, "Mortal(Socrates) must be derived.")
    notes.append("PASS 5: Human(Socrates) and Mortal(Socrates) derived.")

    # 6) Zeus not mortal (no Olympian⊆Mortal rule)
    check(("ex:Mortal", "Zeus") not in LFP, "Zeus should not be inferred mortal.")
    notes.append("PASS 6: Zeus is not inferred mortal.")

    # 7) Witnesses for ∃P include Greek and Philosopher (and Mortal)
    wloc = {local(w) for w in witnesses}
    check({"Greek", "Philosopher"}.issubset(wloc), "Greek/Philosopher should witness ∃P.")
    check("Mortal" in wloc, "Mortal should also be a (trivial) witness.")
    notes.append("PASS 7: Existential witnesses are present.")

    # 8) ∀P statement holds
    check(universal is True, "Universal statement should be true.")
    notes.append("PASS 8: Universal statement verified.")

    # 9) Classes(Socrates) are exactly {Greek, Philosopher, Human, Mortal}
    soc = {local(C) for (C, a) in LFP if a == "Socrates"}
    check(soc == {"Greek", "Philosopher", "Human", "Mortal"}, f"Socrates classes mismatch: {soc}")
    notes.append("PASS 9: Socrates’s classes are correct after closure.")

    # 10) Deterministic pretty-print
    s1 = fmt_membership(LFP)
    s2 = fmt_membership(set(sorted(LFP)))
    check(s1 == s2, "Pretty-printer must be deterministic.")
    notes.append("PASS 10: Deterministic formatting stable.")

    # 11) No out-of-vocabulary symbols appear
    for (c, a) in LFP:
        check(c in CLASSES and a in D, "All derived facts must use known classes/individuals.")
    notes.append("PASS 11: No out-of-vocabulary facts.")

    # 12) Expected total number of membership facts:
    # Base: Greek{S} (1), Philosopher{S,P} (2), Human{P,A} (2), Olympian{Z}(1) → 6
    # Derived: Human{S} (1), Mortal{S,P,A} (3) → +4  (Plato already Human)
    # Total = 10
    check(len(LFP) == 10, f"Expected 10 membership facts, got {len(LFP)}.")
    notes.append("PASS 12: Total number of membership facts is 10.")

    return notes

# =========================================================
# Main orchestration
# =========================================================

def main() -> None:
    print_model()
    print_question()

    LFP, chain, witnesses, universal, classes_socrates = compute_answer()
    print_answer(witnesses, universal, classes_socrates)
    print_reason(LFP, chain)

    print("Check (harness)")
    print("===============")
    try:
        notes = run_checks(LFP, chain, witnesses, universal)
    except CheckFailure as e:
        print("FAIL:", e)
        raise
    else:
        for line in notes:
            print(line)

if __name__ == "__main__":
    main()

