#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
README (plain text)
===================
Purpose
-------
This CASE module is a “branch of insights” in the sense of
https://eyereasoner.github.io/eye/brains/ :

  • It computes a clear **Answer** to a clear **Question**,
  • emits a mathematically phrased **Reason why**,
  • and runs a self-verifying **Check (harness)** with multiple assertions.

Question
--------
“Among the named binary relations on a small domain, which ones are
**equivalence relations** (reflexive, symmetric, transitive)?”

Core Idea (Hayes–Menzel)
------------------------
We render “higher-order-looking” quantification over predicates as **first-order**
by treating predicate *names* (intensions, e.g., URIs) as ordinary objects and
mediating application via a fixed predicate schema

    ex:holds2(R,x,y)

The **extension** of a relation-name `R` is the set of ordered pairs ⟨a,b⟩ such
that `holds2(R,a,b)` holds. Quantifying over “relations” becomes quantifying
over **names** in the NAME-sort; the underlying logic is still first-order.

What this CASE prints
---------------------
1) **Model** — a description of the domain, signature, and the named relations.
2) **Answer** — the set of relation names that are equivalence relations.
3) **Reason why** — a short mathematical-English justification for each
   relation (positive proof-outline or a counter-witness).
4) **Check (harness)** — robust checks covering extensional vs intensional
   equality, relation properties, and expected examples/counterexamples.

How this uses EyeZero
---------------------
  • We populate an EyeZero program `PROGRAM` with all `ex:holds2` facts
    corresponding to the relation extensions.
  • We let EyeZero’s bottom-up engine compute the least fixpoint (no rules
    here, so it just re-collects the facts) and cross-check it against our
    extensional store.
  • The property predicates (Reflexive, Symmetric, etc.) are defined
    extensionally (set-based) in Python, as in the original script; this is
    natural for such “global” properties over finite domains.

How to run
----------
Standalone:

    python3 equivalences.py

Via EyeZero runner:

    python3 eyezero.py equivalences.py

Output
------
Model → Question → Answer → Reason why → Check (harness)
"""

from __future__ import annotations

from typing import Iterable, Tuple, Dict, Set, List

from eyezero import (
    Var, Atom, Clause, atom, fact,
    solve_topdown, solve_bottomup, match_against_facts,
    NAME, IND, Signature, deref, local,
)

# ─────────────────────────────────────────────────────────────────────────────
# Model: domain, relation-names, and holds₂ facts
# ─────────────────────────────────────────────────────────────────────────────

# Use a deterministic, fixed ordering for the domain
D: Tuple[str, ...] = ("Alice", "Bob", "Carol", "Dave")

# Relation names (intensions) look like Web URIs for concreteness
EX = "ex:"

# Fixed predicate for application: holds2(R,x,y)
Holds2 = EX + "holds2"

# Extensional store: for each relation-name r, a set of (a,b) in D×D such that Holds2(r,a,b)
EXT: Dict[str, Set[Tuple[str, str]]] = {}

# EyeZero program (facts) and signature
PROGRAM: List[Clause] = []
SIGNATURE: Signature = {
    Holds2: (NAME, IND, IND),
}


def identity_pairs(domain: Iterable[str]) -> Set[Tuple[str, str]]:
    """Identity relation on domain: {(x,x) | x∈D}."""
    dom = tuple(sorted(domain))
    return {(x, x) for x in dom}


def complete_pairs(domain: Iterable[str]) -> Set[Tuple[str, str]]:
    """Total relation on domain: D×D."""
    dom = tuple(sorted(domain))
    return {(x, y) for x in dom for y in dom}


def define_relation(name: str, pairs: Iterable[Tuple[str, str]]) -> str:
    """
    Register a relation (its *extension*) and populate EyeZero's PROGRAM with
    holds2 facts; return its name (the *intension*).
    """
    spairs = {(a, b) for (a, b) in pairs}  # ensure set, remove duplicates
    EXT[name] = spairs
    for (a, b) in spairs:
        PROGRAM.append(fact(Holds2, name, a, b))
    return name


# Define relations (intensions) with their extensions
equals = define_relation(EX + "equals", identity_pairs(D))  # identity relation

sameTeam = define_relation(
    EX + "sameTeam",
    {
        # team 1 = {Alice, Bob}
        ("Alice", "Alice"),
        ("Alice", "Bob"),
        ("Bob", "Alice"),
        ("Bob", "Bob"),
        # team 2 = {Carol}
        ("Carol", "Carol"),
        # team 3 = {Dave}
        ("Dave", "Dave"),
    },
)

coMember = define_relation(
    EX + "coMember",
    EXT[EX + "sameTeam"].copy(),  # same extension as sameTeam, different name
)

coworker = define_relation(
    EX + "coworker",
    {
        # symmetric but not transitive
        ("Alice", "Bob"),
        ("Bob", "Alice"),
        ("Bob", "Carol"),
        ("Carol", "Bob"),
    },
)

likes = define_relation(
    EX + "likes",
    {
        # arbitrary; typically neither symmetric nor transitive
        ("Alice", "Bob"),
        ("Alice", "Carol"),
        ("Bob", "Alice"),
        ("Carol", "Dave"),
    },
)

ancestorOf = define_relation(
    EX + "ancestorOf",
    {
        # transitive as given, not symmetric, not reflexive
        ("Alice", "Bob"),
        ("Bob", "Carol"),
        ("Alice", "Carol"),
    },
)

universalRel = define_relation(EX + "universalRel", complete_pairs(D))  # D×D
emptyRel = define_relation(EX + "emptyRel", set())                      # ∅

RELATIONS: List[str] = [
    equals,
    sameTeam,
    coMember,
    coworker,
    likes,
    ancestorOf,
    universalRel,
    emptyRel,
]

# ─────────────────────────────────────────────────────────────────────────────
# First-order core: Holds₂ as a fixed predicate (intension → extension)
# ─────────────────────────────────────────────────────────────────────────────

def Holds2_py(r: str, a: str, b: str) -> bool:
    """
    Application mediated by a fixed first-order predicate symbol Holds2(r,a,b).

    Here we use the extensional store EXT as the source of truth. EyeZero's
    bottom-up engine is fed the same facts (via PROGRAM), and the harness
    checks that both views coincide.
    """
    return (a, b) in EXT.get(r, set())

# ─────────────────────────────────────────────────────────────────────────────
# Derived properties of a relation-name r (over finite D)
# ─────────────────────────────────────────────────────────────────────────────

def Reflexive(r: str, domain: Iterable[str] = D) -> bool:
    dom = tuple(sorted(domain))
    return all(Holds2_py(r, x, x) for x in dom)


def Symmetric(r: str, domain: Iterable[str] = D) -> bool:
    dom = tuple(sorted(domain))
    return all(
        (not Holds2_py(r, x, y)) or Holds2_py(r, y, x)
        for x in dom for y in dom
    )


def Transitive(r: str, domain: Iterable[str] = D) -> bool:
    dom = tuple(sorted(domain))
    return all(
        (not (Holds2_py(r, x, y) and Holds2_py(r, y, z))) or Holds2_py(r, x, z)
        for x in dom for y in dom for z in dom
    )


def Equivalence(r: str, domain: Iterable[str] = D) -> bool:
    dom = tuple(sorted(domain))
    return Reflexive(r, dom) and Symmetric(r, dom) and Transitive(r, dom)


def SubsetEq(r: str, s: str, domain: Iterable[str] = D) -> bool:
    dom = tuple(sorted(domain))
    return all(
        (not Holds2_py(r, x, y)) or Holds2_py(s, x, y)
        for x in dom for y in dom
    )


def ExtEq2(r: str, s: str, domain: Iterable[str] = D) -> bool:
    dom = tuple(sorted(domain))
    return SubsetEq(r, s, dom) and SubsetEq(s, r, dom)

# ─────────────────────────────────────────────────────────────────────────────
# Pretty helpers (deterministic)
# ─────────────────────────────────────────────────────────────────────────────

def fmt_pairs_plain(pairs: Iterable[Tuple[str, str]]) -> str:
    """
    Deterministic pretty-print for lists/sets of pairs, *without* curly braces,
    used in Reason-why explanations.
    """
    seq = sorted(pairs)
    return ", ".join(f"⟨{a},{b}⟩" for (a, b) in seq) if seq else "∅"


def sample_pairs(r: str, k: int = 6) -> str:
    pairs = sorted(EXT.get(r, set()))
    head = ", ".join(f"⟨{a},{b}⟩" for (a, b) in pairs[:k])
    return head + (" …" if len(pairs) > k else "")

# ─────────────────────────────────────────────────────────────────────────────
# The Branch: Answer & Reason
# ─────────────────────────────────────────────────────────────────────────────

def answer_equivalences(relset: Iterable[str], domain: Iterable[str] = D) -> List[str]:
    """
    Quantify over relation *names* (intensions) and return those that are
    equivalence relations on the finite domain D.
    """
    dom = tuple(sorted(domain))
    return [r for r in relset if Equivalence(r, dom)]


def reason_for_equivalence(r: str, domain: Iterable[str] = D) -> str:
    """
    Produce a short mathematical-English justification that Equivalence(r)
    holds on D, or else a specific counter-example.
    """
    dom = tuple(sorted(domain))

    missing_reflexives = [(x, x) for x in dom if not Holds2_py(r, x, x)]
    asymmetries = [
        (x, y) for x in dom for y in dom
        if Holds2_py(r, x, y) and not Holds2_py(r, y, x)
    ]
    trans_violations = [
        (x, y, z) for x in dom for y in dom for z in dom
        if Holds2_py(r, x, y) and Holds2_py(r, y, z) and not Holds2_py(r, x, z)
    ]

    if missing_reflexives or asymmetries or trans_violations:
        if missing_reflexives:
            return (
                f"Not reflexive because for x∈D we require Holds₂({r},x,x); "
                f"but missing {fmt_pairs_plain(missing_reflexives)}."
            )
        if asymmetries:
            (x, y) = asymmetries[0]
            return (
                f"Not symmetric because Holds₂({r},{x},{y}) holds but "
                f"Holds₂({r},{y},{x}) fails."
            )
        (x, y, z) = trans_violations[0]
        return (
            f"Not transitive because Holds₂({r},{x},{y}) and Holds₂({r},{y},{z}) hold "
            f"but Holds₂({r},{x},{z}) fails."
        )

    return (
        f"{r} is an equivalence on D because:\n"
        f" • Reflexive: ∀x∈D, Holds₂({r},x,x).\n"
        f" • Symmetric: ∀x,y∈D, Holds₂({r},x,y) ⇒ Holds₂({r},y,x).\n"
        f" • Transitive: ∀x,y,z∈D, (Holds₂({r},x,y) ∧ Holds₂({r},y,z)) ⇒ Holds₂({r},x,z)."
    )

# ─────────────────────────────────────────────────────────────────────────────
# Model + Question
# ─────────────────────────────────────────────────────────────────────────────

def print_model() -> None:
    print("Model")
    print("=====")
    print(f"Domain D = {list(D)}\n")

    print("Signature")
    print("---------")
    print("• A fixed predicate ex:holds2(R,x,y) on objects R,x,y (first-order).")
    print("• For each relation-name R (an intension, e.g., a URI-like string),")
    print("  its extension EXT[R] ⊆ D×D determines where holds2(R,·,·) is true.\n")

    print("Named relations (intensions) with brief descriptions")
    print("----------------------------------------------------")

    def row(name: str, blurb: str) -> None:
        sz = len(EXT[name])
        samp = sample_pairs(name, 5)
        print(f"- {name:<18} ({sz:>2} pairs): {blurb}.\n  Sample: {samp}")

    row(equals,       "identity on D")
    row(sameTeam,     "equivalence: teams {Alice,Bob}, {Carol}, {Dave}")
    row(coMember,     "same extension as ex:sameTeam (different name)")
    row(coworker,     "symmetric but not transitive")
    row(likes,        "neither symmetric nor transitive in general")
    row(ancestorOf,   "transitive but neither reflexive nor symmetric")
    row(universalRel, "total relation D×D (an equivalence)")
    row(emptyRel,     "empty relation ∅ (not an equivalence)")
    print()


def print_question() -> None:
    print("Question")
    print("========")
    print("Q1) Among the named relations R, which R are equivalence relations")
    print("    on D (reflexive, symmetric, transitive)? [meta classification]\n")

# ─────────────────────────────────────────────────────────────────────────────
# Queries (for EyeZero runner)
# ─────────────────────────────────────────────────────────────────────────────

def run_queries():
    eqs = answer_equivalences(RELATIONS)
    # Engine tag is "meta" since classification is done extensionally here;
    # EyeZero’s engine is used in the harness to cross-check the extensional store.
    return [("Q1", "meta", eqs, "n/a")]

# ─────────────────────────────────────────────────────────────────────────────
# Answers + Reason
# ─────────────────────────────────────────────────────────────────────────────

def print_answer(res1) -> None:
    print("Answer")
    print("======")
    tag1, engine1, eqs, _ = res1
    nice = ", ".join(sorted(local(r) for r in eqs))
    print(f"{tag1}) Engine: {engine1} → Equivalence relations on D are:")
    for r in sorted(eqs):
        print(f"  - {local(r)} ({r})")
    print()


def print_reason(eng1: str, eng2: str) -> None:
    # eng2 is unused here; we keep the signature for compatibility with eyezero.
    print("Reason why")
    print("==========")
    print("• Each relation-name R denotes a binary relation on D via holds₂(R,x,y).")
    print("  Properties Reflexive/Symmetric/Transitive/Equivalence are defined")
    print("  **extensionally** over this finite D.")
    print("• The classification:")
    print("    - equals, sameTeam, coMember, universalRel are equivalences;")
    print("    - coworker is symmetric but not transitive;")
    print("    - ancestorOf is transitive but not reflexive nor symmetric;")
    print("    - likes is neither symmetric nor transitive in general;")
    print("    - emptyRel is not reflexive.")
    print("• EyeZero’s bottom-up engine is fed the same holds₂ facts as EXT and the")
    print("  harness checks that both views coincide; all higher-level properties")
    print("  are then computed from this shared extension.\n")

# ─────────────────────────────────────────────────────────────────────────────
# Check (harness)
# ─────────────────────────────────────────────────────────────────────────────

class CheckFailure(AssertionError):
    pass


def check(condition: bool, message: str) -> None:
    if not condition:
        raise CheckFailure(message)


def run_checks() -> List[str]:
    """
    Return a list of human-readable PASS lines; raise on failure.

    We both:
      • Check the extensional properties (as in the original script), and
      • Cross-check that EyeZero’s bottom-up view of holds₂ matches EXT.
    """
    notes: List[str] = []

    # 0) Bottom-up closure matches our extensional store exactly
    facts, _ = solve_bottomup(PROGRAM, SIGNATURE)
    holds_facts = facts.get(Holds2, set())
    ext_union = {(r, a, b) for r, pairs in EXT.items() for (a, b) in pairs}
    check(holds_facts == ext_union, "Bottom-up holds2 facts do not match EXT.")
    notes.append("PASS 0: Bottom-up holds₂ facts exactly match extensional store.")

    # 1) Extensional equality is symmetric and detects coMember ≡ sameTeam
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
    check(
        Holds2_py(coworker, "Alice", "Bob")
        and Holds2_py(coworker, "Bob", "Carol")
        and not Holds2_py(coworker, "Alice", "Carol"),
        "coworker should fail transitivity with Alice–Bob–Carol."
    )
    check(not Transitive(coworker), "coworker must not be transitive.")
    notes.append("PASS 4: coworker is symmetric but not transitive.")

    # 5) ancestorOf is transitive but not symmetric nor reflexive
    check(
        Transitive(ancestorOf) and not Symmetric(ancestorOf) and not Reflexive(ancestorOf),
        "ancestorOf should be transitive only (not reflexive nor symmetric)."
    )
    notes.append("PASS 5: ancestorOf is transitive, neither symmetric nor reflexive.")

    # 6) subset relation from extensional equality: ExtEq2(r,s) ⇒ SubsetEq(r,s) ∧ SubsetEq(s,r)
    for r in RELATIONS:
        for s in RELATIONS:
            if ExtEq2(r, s):
                check(SubsetEq(r, s) and SubsetEq(s, r), "ExtEq₂ implies mutual SubsetEq.")
    notes.append("PASS 6: ExtEq₂ implies mutual SubsetEq for all pairs in scope.")

    # 7) universalRel is an equivalence (it is the total relation)
    check(Equivalence(universalRel), "universalRel should be an equivalence.")
    notes.append("PASS 7: universalRel is an equivalence on D.")

    # 8) emptyRel is not reflexive and thus not an equivalence
    check(not Reflexive(emptyRel) and not Equivalence(emptyRel),
          "emptyRel must fail reflexivity/equivalence.")
    notes.append("PASS 8: emptyRel fails reflexivity and equivalence.")

    # 9) likes is not symmetric (witness Alice→Carol but not Carol→Alice)
    check(Holds2_py(likes, "Alice", "Carol") and not Holds2_py(likes, "Carol", "Alice"),
          "likes should be non-symmetric.")
    check(not Symmetric(likes), "likes must not be symmetric.")
    notes.append("PASS 9: likes is not symmetric.")

    # 10) Characterization check: Equivalences in RELATIONS are exactly {equals, sameTeam, coMember, universalRel}
    eqs = set(answer_equivalences(RELATIONS))
    expected = {equals, sameTeam, coMember, universalRel}
    check(eqs == expected, f"Expected equivalences {expected}, got {eqs}")
    notes.append("PASS 10: Set of equivalence relations matches expectation.")

    return notes

# ─────────────────────────────────────────────────────────────────────────────
# Main
# ─────────────────────────────────────────────────────────────────────────────

def main() -> None:
    print_model()
    print_question()
    (res1,) = run_queries()
    print_answer(res1)
    print_reason(res1[1], "n/a")

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

