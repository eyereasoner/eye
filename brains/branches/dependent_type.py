#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Dependent Type — Good : Profession → Type
ARC (Answer / Reason / Check), self-contained

What this mirrors
  • A minimal type universe with a meta-type Profession.
  • A dependent type constructor Good : Profession → Type that
    automatically creates GoodP for each profession P.
  • Two “excellence” rules:
      - Good Cobbler: if a cobbler produces a shoe of quality Excellent
      - Good Physician: if a physician treats a case with outcome Recovered
  • A small world (alice the cobbler, bob the physician) and proofs.

Output layout
  • Answer — shows the generated Good-types and inferred good professionals.
  • Reason why — explains the dependent-type reading and rules.
  • Check — verifies constructor scope, rule soundness (positive + negative),
    idempotence, and consistency of the `good_of` link.
"""

from __future__ import annotations
from collections import defaultdict
from dataclasses import dataclass
from typing import Dict, List, Set, Tuple, Union, Iterable

# ============================ 0) Core universe ==============================

class Type:
    """
    Minimal RDF/N3-like class:
      - name     … a short identifier
      - kind     … optional meta-type it belongs to (e.g., Profession)
      - good_of  … for auto-generated Good-types, which profession they depend on
    """
    def __init__(self, name: str, *, kind: 'Type' | None = None, good_of: 'Type' | None = None):
        self.name = name
        self.kind = kind
        self.good_of = good_of

    def __repr__(self) -> str:
        return self.name

TYPES: Dict[str, Type] = {}

def new_type(name: str, *, kind: Type | None = None, good_of: Type | None = None) -> Type:
    t = Type(name, kind=kind, good_of=good_of)
    TYPES[name] = t
    return t

# Meta/meta types & basic domain
Type_      = new_type("Type")                 # top type (for completeness)
Profession = new_type("Profession", kind=None)

# Professions
Cobbler   = new_type("Cobbler",   kind=Profession)
Physician = new_type("Physician", kind=Profession)

# Domain notions each profession cares about
Quality   = new_type("Quality")
Excellent = new_type("Excellent")             # instance of Quality (simplified)
Shoe      = new_type("Shoe")

Outcome   = new_type("Outcome")
Recovered = new_type("Recovered")             # instance of Outcome (simplified)

# Property names (as strings for this toy model)
PRODUCES   = "produces"  # cobbler ––> shoe
QUALITY    = "quality"   # shoe ––> quality
TREATS     = "treats"    # physician ––> case
OUTCOME    = "outcome"   # case ––> outcome
PATIENT_OF = "patientOf" # convenience inverse

# Individuals
@dataclass(eq=False)
class Individual:
    name: str
    types: Set[Type]
    props: Dict[str, Set[Union['Individual', Type]]]

    def __init__(self, name: str):
        self.name = name
        self.types = set()
        self.props = defaultdict(set)

    def add_type(self, t: Type) -> None:
        self.types.add(t)

    def add_prop(self, prop: str, value: Union['Individual', Type]) -> None:
        self.props[prop].add(value)

    def has_type(self, t: Type) -> bool:
        return t in self.types

    def __repr__(self) -> str:
        return self.name

# ======================= 1) Dependent type constructor ======================

GOOD_TYPES: Dict[Type, Type] = {}  # P ↦ GoodP

def create_good_types() -> None:
    """For every P of kind Profession, create GoodP with good_of=P."""
    for t in list(TYPES.values()):
        if t.kind is Profession and t not in GOOD_TYPES:
            GOOD_TYPES[t] = new_type(f"Good{t.name}", good_of=t)

create_good_types()

# ============================ 2) Excellence rules ===========================

def infer_good_professionals(individuals: Iterable[Individual]) -> None:
    """
    Forward-chaining of two rules:
      • Good Cobbler:
          if ind : Cobbler
          and ∃ shoe . ind PRODUCES shoe ∧ shoe QUALITY Excellent
          then ind : GoodCobbler
      • Good Physician:
          if ind : Physician
          and ∃ case . ind TREATS case ∧ case OUTCOME Recovered
          then ind : GoodPhysician
    """
    for ind in individuals:
        # Good Cobbler
        if ind.has_type(Cobbler):
            for shoe in ind.props.get(PRODUCES, []):
                if isinstance(shoe, Individual) and (Excellent in shoe.props.get(QUALITY, [])):
                    ind.add_type(GOOD_TYPES[Cobbler])

        # Good Physician
        if ind.has_type(Physician):
            for case in ind.props.get(TREATS, []):
                if isinstance(case, Individual) and (Recovered in case.props.get(OUTCOME, [])):
                    ind.add_type(GOOD_TYPES[Physician])

# =============================== 3) World ===================================

# A cobbler who makes excellent shoes
alice  = Individual("alice")
shoe_1 = Individual("shoe₁")
alice.add_type(Cobbler)
shoe_1.add_type(Shoe)
shoe_1.add_prop(QUALITY, Excellent)
alice.add_prop(PRODUCES, shoe_1)

# A physician who cures patients
bob     = Individual("bob")
case_1  = Individual("case₁")
bob.add_type(Physician)
case_1.add_prop(OUTCOME, Recovered)
case_1.add_prop(PATIENT_OF, bob)
bob.add_prop(TREATS, case_1)

WORLD: List[Individual] = [alice, shoe_1, bob, case_1]
infer_good_professionals(WORLD)

# =============================== ARC: Answer ================================

def print_answer() -> None:
    print("Answer")
    print("======")

    print("Good-types generated by Good : Profession → Type")
    for P, GP in sorted(GOOD_TYPES.items(), key=lambda kv: kv[0].name):
        print(f"  {P.name} ↦ {GP.name}   (good_of={GP.good_of.name})")

    print("\nInferred ‘good’ professionals:")
    any_good = False
    for ind in WORLD:
        goodies = [t.name for t in ind.types if t in GOOD_TYPES.values()]
        if goodies:
            any_good = True
            print(f"  • {ind.name} → {', '.join(sorted(goodies))}")
    if not any_good:
        print("  (none)")

# ============================ ARC: Reason why ===============================

def print_reason() -> None:
    print("\nReason why")
    print("==========")
    print("We treat Profession as a meta-type and define a dependent constructor")
    print("Good : Profession → Type that automatically yields GoodP for each P.")
    print("Then two domain rules identify when a professional counts as ‘good’:")
    print("  • Cobblers are good if they produce at least one shoe that is Excellent.")
    print("  • Physicians are good if they treat at least one case with outcome Recovered.")
    print("This mirrors the idea that ‘good-at(P)’ depends on P (not just ‘good’ ∧ P).")

# ============================ ARC: Check (harness) ==========================

def print_check() -> None:
    print("\nCheck (harness)")
    print("===============")
    ok_all = True

    # A) Constructor scope: Good-types exist exactly for professions
    all_professions = {t for t in TYPES.values() if t.kind is Profession}
    good_targets = set(GOOD_TYPES.keys())
    only_for_prof = good_targets == all_professions
    links_ok = all(GOOD_TYPES[P].good_of is P for P in all_professions)
    print(f"Good-types exist for every (and only) Profession? {only_for_prof}")
    print(f"Each GoodP has good_of = P?                     {links_ok}")
    ok_all &= only_for_prof and links_ok

    # B) Positive rule applications in WORLD
    got_good_cobbler   = GOOD_TYPES[Cobbler]   in alice.types
    got_good_physician = GOOD_TYPES[Physician] in bob.types
    print(f"Alice inferred as GoodCobbler?                 {got_good_cobbler}")
    print(f"Bob inferred as GoodPhysician?                 {got_good_physician}")
    ok_all &= got_good_cobbler and got_good_physician

    # C) Negative cases (no false positives)
    #   Charlie is a cobbler but makes a shoe with no Excellent quality.
    charlie = Individual("charlie")
    charlie_shoe = Individual("shoe₂")
    charlie.add_type(Cobbler)
    charlie.add_prop(PRODUCES, charlie_shoe)  # shoe₂ has no QUALITY Excellent

    #   Dana is a physician but treats a case that did NOT recover.
    dana = Individual("dana")
    case_2 = Individual("case₂")
    dana.add_type(Physician)
    dana.add_prop(TREATS, case_2)            # case₂ has no OUTCOME Recovered

    test_world = [charlie, charlie_shoe, dana, case_2]
    infer_good_professionals(test_world)

    no_bad_good_cobbler   = GOOD_TYPES[Cobbler]   not in charlie.types
    no_bad_good_physician = GOOD_TYPES[Physician] not in dana.types
    print(f"No GoodCobbler without Excellent shoe?         {no_bad_good_cobbler}")
    print(f"No GoodPhysician without Recovered case?       {no_bad_good_physician}")
    ok_all &= no_bad_good_cobbler and no_bad_good_physician

    # D) Idempotence: running inference again doesn’t change results
    before = {ind.name: set(ind.types) for ind in WORLD}
    infer_good_professionals(WORLD)
    after  = {ind.name: set(ind.types) for ind in WORLD}
    idem = before == after
    print(f"Inference is idempotent on WORLD?              {idem}")
    ok_all &= idem

    # E) Consistency: any GoodP on an individual implies that individual has P
    consistent = True
    for ind in WORLD + test_world:
        for t in ind.types:
            if t.good_of is not None:  # this is a GoodP
                if t.good_of not in ind.types:
                    consistent = False
                    break
        if not consistent:
            break
    print(f"Any GoodP implies base profession P?           {consistent}")
    ok_all &= consistent

    print(f"\nAll checks passed? {ok_all}")

# ================================= Main =====================================

if __name__ == "__main__":
    print_answer()
    print_reason()
    print_check()

