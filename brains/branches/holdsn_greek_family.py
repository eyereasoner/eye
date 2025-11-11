#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
README (plain text)
===================
Purpose
-------
A small “branch of insights” (in the spirit of https://eyereasoner.github.io/eye/brains/)
that *quantifies over binary predicates* while keeping a first-order core via the
Hayes–Menzel idea. Binary predicates (relations) are **named objects** (intensions
like "ex:ParentOf"), and we use a single fixed predicate style:

  • Holds₂(R, a, b)           — membership: ⟨a,b⟩ is in the extension of relation-name R
  • Holds₂(SubRelOf, P, Q)    — inclusion:   extension(P) ⊆ extension(Q) (over *names* P, Q)

Quantifying over predicates = quantifying over **relation names** drawn from a finite
vocabulary RELS. This keeps everything first-order while the behavior still looks
second-order.

Realistic facts (historical)
----------------------------
Individuals D = {Sophroniscus, Socrates, Lamprocles, Ariston, Plato, Nicomachus, Aristotle}.

  FatherOf:   Sophroniscus→Socrates, Socrates→Lamprocles, Ariston→Plato, Nicomachus→Aristotle
  ParentOf:   same four pairs (different name)
  TeacherOf:  Socrates→Plato (historically accurate)
  AncestorOf: transitive closure of ParentOf (so Sophroniscus→Lamprocles holds)

Rules over relation *names*:
  FatherOf ⊆ ParentOf,   ParentOf ⊆ AncestorOf.   (but TeacherOf ⊄ AncestorOf)

Typical Question (this program prints)
--------------------------------------
(1) ∃R [ R(Socrates, Lamprocles) ∧ R ⊆ AncestorOf ] ?   (list witnesses R)
(2) ∀R [ (R ⊆ ParentOf ∧ R(Socrates, y)) → AncestorOf(Socrates, y) ] ?  (true/false)
(3) What are all pairs in AncestorOf after closure?

How to run
----------
    python3 holdsn_greek_family.py

No external dependencies; deterministic execution and output.
"""

from __future__ import annotations
from typing import Dict, Iterable, List, Set, Tuple, Optional

# =========================================================
# Model: individuals, relation names (intensions), Holds₂
# =========================================================

# Deterministically ordered individuals (historically plausible)
D: Tuple[str, ...] = (
    "Sophroniscus",  # father of Socrates
    "Socrates",
    "Lamprocles",    # son of Socrates
    "Ariston",       # father of Plato
    "Plato",
    "Nicomachus",    # father of Aristotle
    "Aristotle"
)

# Namespace for names (URIs/strings as intensions)
EX = "ex:"

# ---------- Binary: relation names → set of pairs (their extensions) ----------
EXT2: Dict[str, Set[Tuple[str, str]]] = {}

def define_relation(name: str, pairs: Iterable[Tuple[str, str]]) -> str:
    """Register a named binary relation with its extension (a set of ordered pairs)."""
    EXT2[name] = {(a, b) for (a, b) in pairs}
    return name

def Holds2(rname: str, x: str, y: str) -> bool:
    """Holds₂(R, x, y) — ⟨x,y⟩ ∈ extension of the relation named by R."""
    return (x, y) in EXT2.get(rname, set())

# Base relations (realistic)
FatherOf   = define_relation(EX + "FatherOf", [
    ("Sophroniscus", "Socrates"),
    ("Socrates",     "Lamprocles"),
    ("Ariston",      "Plato"),
    ("Nicomachus",   "Aristotle"),
])
ParentOf   = define_relation(EX + "ParentOf", [
    ("Sophroniscus", "Socrates"),
    ("Socrates",     "Lamprocles"),
    ("Ariston",      "Plato"),
    ("Nicomachus",   "Aristotle"),
])
TeacherOf  = define_relation(EX + "TeacherOf", [
    ("Socrates",     "Plato"),
])

# Build AncestorOf as the *transitive closure* of ParentOf (non-reflexive).
def transitive_closure(E: Set[Tuple[str, str]]) -> Set[Tuple[str, str]]:
    closure = set(E)
    changed = True
    while changed:
        changed = False
        add: Set[Tuple[str, str]] = set()
        for (x, y1) in sorted(closure):
            for (y2, z) in sorted(closure):
                if y1 == y2 and (x, z) not in closure:
                    add.add((x, z))
        if add:
            closure |= add
            changed = True
    return closure

AncestorOf = define_relation(EX + "AncestorOf", transitive_closure(EXT2[ParentOf]))

# Universe of relation *names* we quantify over
RELS: Tuple[str, ...] = (FatherOf, ParentOf, TeacherOf, AncestorOf)

# ---------- Meta-relation over relation names: SubRelOf ----------
# Holds₂(SubRelOf, P, Q) means: extension(P) ⊆ extension(Q).
SubRelOf = define_relation(EX + "SubRelOf", [
    (FatherOf, ParentOf),
    (ParentOf, AncestorOf),
    # TeacherOf deliberately NOT included in AncestorOf chain
])

def subrel_transitive_closure() -> Set[Tuple[str, str]]:
    """Compute (non-reflexive) transitive closure over relation-names."""
    base = set(EXT2[SubRelOf])
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

SUBREL_CLOS: Set[Tuple[str, str]] = subrel_transitive_closure()

def subrel_leq(P: str, Q: str) -> bool:
    """Reflexive-transitive inclusion on relation-names: P ⊆* Q."""
    return P == Q or (P, Q) in EXT2[SubRelOf] or (P, Q) in SUBREL_CLOS

# =========================================================
# Membership closure (Kleene): propagate along SubRelOf
# =========================================================
# Facts are triples (R, a, b) meaning Holds₂(R, a, b).
# Operator F(S) = base_facts ∪ { (Q,a,b) | (P,a,b)∈S ∧ (P ⊆* Q) }.

def kleene_membership_closure(max_steps: int = 64) -> Tuple[Set[Tuple[str, str, str]], List[Set[Tuple[str, str, str]]]]:
    base: Set[Tuple[str, str, str]] = set()
    for R in RELS:
        for (a, b) in sorted(EXT2[R]):
            base.add((R, a, b))

    chain: List[Set[Tuple[str, str, str]]] = [set()]
    S: Set[Tuple[str, str, str]] = set()
    for _ in range(max_steps):
        S_next = set(base)
        for (P, a, b) in S:
            for Q in RELS:
                if subrel_leq(P, Q):
                    S_next.add((Q, a, b))
        chain.append(S_next)
        if S_next == S:
            return S_next, chain
        S = S_next
    return S, chain  # defensive (should converge quickly)

# =========================================================
# Quantification over relation *names*
# =========================================================

def exists_R_member_and_subrel_to(a: str, b: str, Target: str, LFP: Set[Tuple[str, str, str]]) -> List[str]:
    """Return all relation-names R with R(a,b) and R ⊆* Target (witnesses for ∃R …)."""
    return [R for (R, x, y) in LFP if x == a and y == b and subrel_leq(R, Target)]

def forall_R_parent_implies_ancestor(x: str, LFP: Set[Tuple[str, str, str]]) -> bool:
    """Check ∀R∀y: (R ⊆ ParentOf ∧ R(x,y)) → AncestorOf(x,y)."""
    for (R, a, b) in LFP:
        if a != x:
            continue
        if subrel_leq(R, ParentOf) and (AncestorOf, a, b) not in LFP:
            return False
    return True

# =========================================================
# Pretty helpers (deterministic)
# =========================================================

def local(name: str) -> str:
    """Drop 'ex:' for display."""
    return name.split(":", 1)[1] if ":" in name else name

def fmt_pairs(R: Iterable[Tuple[str, str]]) -> str:
    seq = list(sorted(R))
    return "∅" if not seq else "{" + ", ".join(f"⟨{a},{b}⟩" for (a, b) in seq) + "}"

def fmt_facts(S: Iterable[Tuple[str, str, str]]) -> str:
    seq = list(sorted(S))
    return "∅" if not seq else "{" + ", ".join(f"{local(R)}({a},{b})" for (R, a, b) in seq) + "}"

def fmt_chain(chain: List[Set[Tuple[str, str, str]]], limit: int = 5) -> str:
    parts = [fmt_facts(S) for S in chain[:limit]]
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
    print("• Holds₂(R, a, b): ⟨a,b⟩ ∈ extension of the relation named by R (R is an *intension*).")
    print("• Holds₂(SubRelOf, P, Q): extension of P is included in extension of Q (P,Q are names).")
    print()
    print("Named relations (intensions) and their pairs")
    print("-------------------------------------------")
    for R, blurb in [
        (FatherOf,  "biological father relation"),
        (ParentOf,  "parent relation (same pairs as FatherOf here)"),
        (TeacherOf, "teaching relation (unrelated to ancestry)"),
        (AncestorOf,"transitive closure of ParentOf (non-reflexive)"),
    ]:
        print(f"- {local(R):<12}: {fmt_pairs(EXT2[R])} — {blurb}")
    print()
    print("Inclusion rules over relation *names*")
    print("-------------------------------------")
    subs = [f"{local(p)} ⊆ {local(q)}" for (p,q) in sorted(EXT2[SubRelOf])]
    print("• " + ", ".join(subs))
    print()

def print_question() -> None:
    print("Question")
    print("========")
    print("(1) ∃R  [ R(Socrates,Lamprocles) ∧ R ⊆ AncestorOf ] ?  (list witnesses)")
    print("(2) ∀R  [ (R ⊆ ParentOf ∧ R(Socrates,y)) → AncestorOf(Socrates,y) ] ?")
    print("(3) What are all pairs in AncestorOf after closure?")
    print()

def compute_answer():
    LFP, chain = kleene_membership_closure()
    witnesses = exists_R_member_and_subrel_to("Socrates", "Lamprocles", AncestorOf, LFP)
    universal = forall_R_parent_implies_ancestor("Socrates", LFP)
    ancestor_pairs_after = sorted({(a,b) for (R,a,b) in LFP if R == AncestorOf})
    return LFP, chain, witnesses, universal, ancestor_pairs_after

def print_answer(witnesses: List[str], universal: bool, ancestor_pairs_after: List[Tuple[str,str]]) -> None:
    print("Answer")
    print("======")
    print("(1) Exists R ?  " + ("Yes" if witnesses else "No"))
    if witnesses:
        print("    Witnesses:", "{" + ", ".join(local(w) for w in sorted(set(witnesses))) + "}")
    print("(2) For all R ? " + ("Yes" if universal else "No"))
    print("(3) AncestorOf =", fmt_pairs(ancestor_pairs_after))
    print()

def print_reason(LFP: Set[Tuple[str, str, str]], chain: List[Set[Tuple[str, str, str]]]) -> None:
    print("Reason why")
    print("==========")
    print("We treat binary predicates as *names* and mediate application with a fixed predicate:")
    print("  • Holds₂(R,a,b) for membership, plus Holds₂(SubRelOf,P,Q) for inclusion of relation-names.")
    print("From FatherOf ⊆ ParentOf and ParentOf ⊆ AncestorOf we get FatherOf ⊆ AncestorOf by transitivity.")
    print("Since FatherOf(Socrates,Lamprocles), the existential in (1) holds with witnesses {FatherOf, ParentOf, AncestorOf}.")
    print("For (2), whenever R ⊆ ParentOf, any R(Socrates,y) also lies in ParentOf and thus in AncestorOf.")
    print()
    print("Kleene membership closure from base facts yields the ascending chain:")
    print("  " + fmt_chain(chain, limit=5))
    print(f"which stabilizes at LFP = {fmt_facts(LFP)}.")
    print()

# =========================================================
# Check (harness) — deterministic, ≥ 12 tests
# =========================================================

class CheckFailure(AssertionError):
    pass

def check(cond: bool, msg: str) -> None:
    if not cond:
        raise CheckFailure(msg)

def run_checks(LFP: Set[Tuple[str, str, str]], chain: List[Set[Tuple[str, str, str]]],
               witnesses: List[str], universal: bool) -> List[str]:
    notes: List[str] = []

    # 1) Inclusion rules present
    check((FatherOf, ParentOf)  in EXT2[SubRelOf], "Expected FatherOf ⊆ ParentOf.")
    check((ParentOf, AncestorOf) in EXT2[SubRelOf], "Expected ParentOf ⊆ AncestorOf.")
    notes.append("PASS 1: Intended inclusion rules present.")

    # 2) Transitive inclusion: FatherOf ⊆ AncestorOf via closure
    check(subrel_leq(FatherOf, AncestorOf), "FatherOf should be ⊆* AncestorOf.")
    notes.append("PASS 2: Transitive inclusion FatherOf ⊆* AncestorOf holds.")

    # 3) First Kleene step contains all base facts
    base = {(R,a,b) for R in RELS for (a,b) in EXT2[R]}
    check(base.issubset(chain[1]), "First Kleene step must include base facts.")
    notes.append("PASS 3: Kleene step 1 contains base facts.")

    # 4) Chain is ascending and stabilizes
    for i in range(len(chain) - 1):
        check(chain[i].issubset(chain[i+1]), "Kleene chain must be ascending.")
    check(chain[-1] == LFP, "Kleene chain must stabilize at LFP.")
    notes.append("PASS 4: Chain is ascending and stabilizes.")

    # 5) AncestorOf pairs include the transitive link (Sophroniscus,Lamprocles)
    check(("Sophroniscus","Lamprocles") in EXT2[AncestorOf], "AncestorOf must include (Sophroniscus,Lamprocles).")
    notes.append("PASS 5: AncestorOf has the expected transitive pair.")

    # 6) Existential witnesses for R(Socrates,Lamprocles) ∧ R ⊆ AncestorOf
    wset = set(witnesses)
    check(FatherOf in wset and ParentOf in wset and AncestorOf in wset, "Witnesses should include FatherOf, ParentOf, AncestorOf.")
    check(TeacherOf not in wset, "TeacherOf should not be a witness (not ⊆ AncestorOf).")
    notes.append("PASS 6: Existential witnesses correct.")

    # 7) Universal statement holds (for Socrates as parent)
    check(universal is True, "Universal statement should be true.")
    notes.append("PASS 7: Universal statement verified.")

    # 8) Propagation: any (FatherOf,a,b) ends up as (AncestorOf,a,b) in LFP
    for (a,b) in EXT2[FatherOf]:
        check((AncestorOf, a, b) in LFP, "FatherOf facts must propagate to AncestorOf.")
    notes.append("PASS 8: Inclusion propagation works.")

    # 9) No spurious propagation from TeacherOf to AncestorOf
    for (a,b) in EXT2[TeacherOf]:
        check((AncestorOf, a, b) not in LFP, "TeacherOf facts must NOT propagate to AncestorOf.")
    notes.append("PASS 9: Unrelated relations do not propagate.")

    # 10) LFP is a fixed point for inclusion propagation
    L2, _ = kleene_membership_closure()
    check(L2 == LFP, "Closure should be idempotent (fixed point).")
    notes.append("PASS 10: Closure is idempotent.")

    # 11) Deterministic formatting
    s1 = fmt_facts(LFP); s2 = fmt_facts(set(sorted(LFP)))
    check(s1 == s2, "Pretty-printer must be deterministic.")
    notes.append("PASS 11: Deterministic formatting is stable.")

    # 12) Count facts: base + propagated (unique triples)
    # Base counts: FatherOf(4) + ParentOf(4) + TeacherOf(1) + AncestorOf(5) = 14
    # Propagation doesn’t add *new* triples beyond these (it only ensures they’re present).
    check(len(LFP) == 14, f"Expected exactly 14 membership triples in LFP, got {len(LFP)}.")
    notes.append("PASS 12: LFP fact count is exactly 14.")

    return notes

# =========================================================
# Main orchestration
# =========================================================

def main() -> None:
    print_model()
    print_question()

    LFP, chain, witnesses, universal, ancestor_pairs_after = compute_answer()
    print_answer(witnesses, universal, ancestor_pairs_after)
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

