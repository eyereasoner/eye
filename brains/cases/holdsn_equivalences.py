#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
README (plain text)
===================
This single-file Python program is a “branch of insights” in the sense of
https://eyereasoner.github.io/eye/brains/ : it computes a clear **Answer** to a
clear **Question**, emits a mathematically phrased **Reason why**, and runs a
self-verifying **Check (harness)** with multiple assertions that can fail loudly.

Question
--------
“Among the named binary relations on a small domain, which ones are
**equivalence relations** (reflexive, symmetric, transitive)?”

Core Idea (Hayes–Menzel)
------------------------
We render “higher-order-looking” quantification over predicates as **first-order**
by treating predicate *names* (intensions, e.g., URIs) as ordinary objects and
mediating application via a fixed predicate schema `Holds₂`. The **extension** of
a relation-name `r` is the set of ordered pairs ⟨a,b⟩ such that `Holds2(r,a,b)`.

What the program prints
-----------------------
1) **Model** — a description of the domain, signature, and the named relations.
2) **Answer** — the set of relation names that are equivalence relations.
3) **Reason why** — a short mathematical-English justification for each relation
   (positive proof outline or counter-witness).
4) **Check (harness)** — 10 robust checks covering extensional vs intensional
   equality, property definitions, and expected examples/counterexamples.

How to run
----------
    python3 holdsn_equivalence.py

No external dependencies; deterministic execution.
"""
from __future__ import annotations

from typing import Iterable, Tuple, Dict, Set, List

# -------------------------
# Model: domain and Holds₂
# -------------------------

# Use a deterministic, fixed ordering for the domain
D: Tuple[str, ...] = ("Alice", "Bob", "Carol", "Dave")

# Relation names (intensions) look like Web URIs for concreteness
EX = "ex:"  # namespace prefix

# Extension store: for each relation-name r, a set of (a,b) in D×D such that Holds2(r,a,b)
EXT: Dict[str, Set[Tuple[str, str]]] = {}

def define_relation(name: str, pairs: Iterable[Tuple[str, str]]) -> str:
    """Register a relation (its *extension*); return its name (the *intension*)."""
    EXT[name] = {(a, b) for (a, b) in pairs}
    return name

# Deterministic helpers
def ord_dom(domain: Iterable[str]) -> Tuple[str, ...]:
    """Return a deterministic ordering of the domain (lexicographic)."""
    return tuple(sorted(domain))

def identity_pairs(domain: Iterable[str]) -> Set[Tuple[str, str]]:
    dom = ord_dom(domain)
    return {(x, x) for x in dom}

def complete_pairs(domain: Iterable[str]) -> Set[Tuple[str, str]]:
    dom = ord_dom(domain)
    return {(x, y) for x in dom for y in dom}

# Define relations (intensions) with their extensions
equals        = define_relation(EX + "equals", identity_pairs(D))                # identity relation
sameTeam      = define_relation(EX + "sameTeam", {
    ("Alice", "Alice"), ("Alice", "Bob"), ("Bob", "Alice"), ("Bob", "Bob"),      # team 1 = {Alice, Bob}
    ("Carol", "Carol"),                                                          # team 2 = {Carol}
    ("Dave", "Dave")                                                             # team 3 = {Dave}
})
coMember      = define_relation(EX + "coMember", EXT[EX + "sameTeam"].copy())    # same extension, different name
coworker      = define_relation(EX + "coworker", {                               # symmetric but not transitive
    ("Alice", "Bob"), ("Bob", "Alice"),
    ("Bob", "Carol"), ("Carol", "Bob")
})
likes         = define_relation(EX + "likes", {                                  # arbitrary, typically neither sym nor trans
    ("Alice", "Bob"), ("Alice", "Carol"),
    ("Bob", "Alice"),
    ("Carol", "Dave")
})
ancestorOf    = define_relation(EX + "ancestorOf", {                             # transitive (as given), not symmetric, not reflexive
    ("Alice", "Bob"), ("Bob", "Carol"), ("Alice", "Carol")
})
universalRel  = define_relation(EX + "universalRel", complete_pairs(D))          # everything relates to everything
emptyRel      = define_relation(EX + "emptyRel", set())                          # nothing relates to anything

RELATIONS: List[str] = [equals, sameTeam, coMember, coworker, likes, ancestorOf, universalRel, emptyRel]

# --------------------------
# First-order core: Holds₂
# --------------------------

def Holds2(r: str, a: str, b: str) -> bool:
    """Application mediated by a fixed first-order predicate symbol Holds2(r,a,b)."""
    return (a, b) in EXT.get(r, set())

# ----------------------------
# Derived FO properties on r
# ----------------------------

def Reflexive(r: str, domain: Iterable[str] = D) -> bool:
    dom = ord_dom(domain)
    return all(Holds2(r, x, x) for x in dom)

def Symmetric(r: str, domain: Iterable[str] = D) -> bool:
    dom = ord_dom(domain)
    return all((not Holds2(r, x, y)) or Holds2(r, y, x) for x in dom for y in dom)

def Transitive(r: str, domain: Iterable[str] = D) -> bool:
    dom = ord_dom(domain)
    return all((not (Holds2(r, x, y) and Holds2(r, y, z))) or Holds2(r, x, z)
               for x in dom for y in dom for z in dom)

def Equivalence(r: str, domain: Iterable[str] = D) -> bool:
    dom = ord_dom(domain)
    return Reflexive(r, dom) and Symmetric(r, dom) and Transitive(r, dom)

def SubsetEq(r: str, s: str, domain: Iterable[str] = D) -> bool:
    dom = ord_dom(domain)
    return all((not Holds2(r, x, y)) or Holds2(s, x, y) for x in dom for y in dom)

def ExtEq2(r: str, s: str, domain: Iterable[str] = D) -> bool:
    dom = ord_dom(domain)
    return SubsetEq(r, s, dom) and SubsetEq(s, r, dom)

# ------------------------------
# Pretty helpers (deterministic)
# ------------------------------

def fmt_pairs(pairs: Iterable[Tuple[str, str]]) -> str:
    seq = sorted(pairs)  # deterministic order for display
    return ", ".join(f"⟨{a},{b}⟩" for (a, b) in seq) if seq else "∅"

def sample_pairs(r: str, k: int = 6) -> str:
    pairs = sorted(EXT.get(r, set()))
    return ", ".join(f"⟨{a},{b}⟩" for (a, b) in pairs[:k]) + (" …" if len(pairs) > k else "")

# ------------------------------
# The Branch: Answer & Reason
# ------------------------------

def answer_equivalences(relset: Iterable[str], domain: Iterable[str] = D) -> List[str]:
    """Quantify over relation *names* (intensions) and return those that are equivalence relations."""
    dom = ord_dom(domain)
    return [r for r in relset if Equivalence(r, dom)]

def reason_for_equivalence(r: str, domain: Iterable[str] = D) -> str:
    """Produce a short mathematical-English justification that Equivalence(r) holds on domain."""
    dom = ord_dom(domain)
    missing_reflexives = [(x, x) for x in dom if not Holds2(r, x, x)]
    asymmetries = [(x, y) for x in dom for y in dom if Holds2(r, x, y) and not Holds2(r, y, x)]
    trans_violations = [(x, y, z) for x in dom for y in dom for z in dom
                        if Holds2(r, x, y) and Holds2(r, y, z) and not Holds2(r, x, z)]
    if missing_reflexives or asymmetries or trans_violations:
        if missing_reflexives:
            return (f"Not reflexive because for x∈D we require Holds₂({r},x,x); "
                    f"but missing {fmt_pairs(missing_reflexives)}.")
        if asymmetries:
            (x, y) = asymmetries[0]
            return (f"Not symmetric because Holds₂({r},{x},{y}) holds but Holds₂({r},{y},{x}) fails.")
        (x, y, z) = trans_violations[0]
        return (f"Not transitive because Holds₂({r},{x},{y}) and Holds₂({r},{y},{z}) hold "
                f"but Holds₂({r},{x},{z}) fails.")
    return (f"{r} is an equivalence on D because:\n"
            f"  • Reflexive: ∀x∈D, Holds₂({r},x,x).\n"
            f"  • Symmetric: ∀x,y∈D, Holds₂({r},x,y) ⇒ Holds₂({r},y,x).\n"
            f"  • Transitive: ∀x,y,z∈D, (Holds₂({r},x,y) ∧ Holds₂({r},y,z)) ⇒ Holds₂({r},x,z).")

# ------------------------------
# Check (harness)
# ------------------------------

class CheckFailure(AssertionError):
    pass

def check(condition: bool, message: str) -> None:
    if not condition:
        raise CheckFailure(message)

def run_checks() -> List[str]:
    """Return a list of human-readable PASS lines; raise on failure."""
    notes: List[str] = []
    # 1) Extensional equality is symmetric and detects coMember ≡ sameTeam but names differ (intension ≠ extension)
    check(ExtEq2(sameTeam, coMember), "Expected extensional equality of sameTeam and coMember.")
    check(sameTeam != coMember, "Expected intensional inequality (different names) for same extension.")
    notes.append("PASS 1: ExtEq₂(sameTeam, coMember) and sameTeam ≠ coMember (intension ≠ extension).")

    # 2) equals is exactly the identity relation
    check(EXT[equals] == identity_pairs(D), "equals should be the identity extension.")
    notes.append("PASS 2: equals has the identity extension on D.")

    # 3) Any equivalence is reflexive
    for r in RELATIONS:
        if Equivalence(r):
            check(Reflexive(r), f"{r} marked equivalence but not reflexive.")
    notes.append("PASS 3: Every r with Equivalence(r) is reflexive.")

    # 4) coworker is not transitive (classic counterexample: Alice–Bob, Bob–Carol but not Alice–Carol)
    check(Holds2(coworker, "Alice", "Bob") and Holds2(coworker, "Bob", "Carol")
          and not Holds2(coworker, "Alice", "Carol"), "coworker should fail transitivity.")
    check(not Transitive(coworker), "coworker must not be transitive.")
    notes.append("PASS 4: coworker is symmetric but not transitive.")

    # 5) ancestorOf is transitive but not symmetric nor reflexive
    check(Transitive(ancestorOf) and not Symmetric(ancestorOf) and not Reflexive(ancestorOf),
          "ancestorOf should be transitive only.")
    notes.append("PASS 5: ancestorOf is transitive, neither symmetric nor reflexive.")

    # 6) subset relation from extensional equality: ExtEq2(r,s) ⇒ SubsetEq(r,s) ∧ SubsetEq(s,r)
    for r in RELATIONS:
        for s in RELATIONS:
            if ExtEq2(r, s):
                check(SubsetEq(r, s) and SubsetEq(s, r), "ExtEq2 implies mutual SubsetEq.")
    notes.append("PASS 6: ExtEq₂ implies mutual SubsetEq for all pairs in scope.")

    # 7) universalRel is an equivalence (it is the total relation)
    check(Equivalence(universalRel), "universalRel should be an equivalence.")
    notes.append("PASS 7: universalRel is an equivalence on D.")

    # 8) emptyRel is not reflexive and thus not an equivalence
    check(not Reflexive(emptyRel) and not Equivalence(emptyRel),
          "emptyRel must fail reflexivity/equivalence.")
    notes.append("PASS 8: emptyRel fails reflexivity and equivalence.")

    # 9) likes is not symmetric (witness Alice→Carol but not Carol→Alice)
    check(Holds2(likes, "Alice", "Carol") and not Holds2(likes, "Carol", "Alice"),
          "likes should be non-symmetric.")
    check(not Symmetric(likes), "likes must not be symmetric.")
    notes.append("PASS 9: likes is not symmetric.")

    # 10) Characterization check: Equivalences in RELATIONS are exactly {equals, sameTeam, coMember, universalRel}
    eqs = set(answer_equivalences(RELATIONS))
    expected = {equals, sameTeam, coMember, universalRel}
    check(eqs == expected, f"Expected equivalences {expected}, got {eqs}")
    notes.append("PASS 10: Set of equivalence relations matches expectation.")
    return notes

# ------------------------------
# Model description (printed first)
# ------------------------------

def print_model_description() -> None:
    print("Model")
    print("=====")
    # Domain
    print(f"Domain D = {list(D)}")
    print()
    # Signature
    print("Signature")
    print("---------")
    print("• A fixed predicate Holds₂(r,a,b) on objects r,a,b (first-order).")
    print("• For each relation-name r (an intension, e.g., a URI-like string),")
    print("  its extension EXT[r] ⊆ D×D determines where Holds₂(r,·,·) is true.")
    print()
    # Named relations
    print("Named relations (intensions) with brief descriptions")
    print("----------------------------------------------------")
    def row(name: str, blurb: str) -> None:
        sz = len(EXT[name])
        samp = sample_pairs(name, 5)
        print(f"- {name:<18} ({sz:>2} pairs): {blurb}. Sample: {samp}")
    row(equals,       "identity on D")
    row(sameTeam,     "equivalence: teams {Alice,Bob}, {Carol}, {Dave}")
    row(coMember,     "same extension as ex:sameTeam (different name)")
    row(coworker,     "symmetric but not transitive")
    row(likes,        "neither symmetric nor transitive in general")
    row(ancestorOf,   "transitive but not reflexive nor symmetric")
    row(universalRel, "total relation D×D (an equivalence)")
    row(emptyRel,     "empty relation ∅ (not an equivalence)")
    print()

# ------------------------------
# Main: compute + explain + check
# ------------------------------

def main() -> None:
    # 0) Model description first
    print_model_description()

    # 1) Answer
    eqs = answer_equivalences(RELATIONS)
    print("Answer")
    print("======")
    print("Equivalence relations on D =", list(D), "are:")
    for r in eqs:
        print(f"  - {r}")
    print()

    # 2) Reason why
    print("Reason why")
    print("==========")
    for r in RELATIONS:
        expl = reason_for_equivalence(r)
        print(f"- {expl}")
    print()

    # 3) Check (harness)
    print("Check (harness)")
    print("===============")
    try:
        notes = run_checks()
    except CheckFailure as e:
        print("FAIL:", e)
        raise
    else:
        for line in notes:
            print(line)

if __name__ == "__main__":
    main()

