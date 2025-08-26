#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Slide 32 (Pat Hayes talk) — ARC (Answer / Reason / Check), self-contained

Facts:
  :Ghent a :City .

Rule:
  { ?x a :City } => { ?x a :HumanCommunity } .

Query (tautology pattern):
  { ?S a ?C } => { ?S a ?C } .
"""

from typing import Set, Tuple, List

Triple = Tuple[str, str, str]  # (s, p, o), with 'a' as rdf:type

# ------------------------------- Data --------------------------------
FACTS: Set[Triple] = {
    (":Ghent", "a", ":City"),
}

# --------------------------- Derivation ------------------------------
def apply_rule_once(facts: Set[Triple]) -> int:
    """Apply  { ?x a :City } => { ?x a :HumanCommunity }  once. Return #new facts."""
    added = 0
    new_facts = set()
    for (s, p, o) in facts:
        if p == "a" and o == ":City":
            concl = (s, "a", ":HumanCommunity")
            if concl not in facts:
                new_facts.add(concl)
    for f in new_facts:
        facts.add(f)
        added += 1
    return added

def closure(start: Set[Triple]) -> Set[Triple]:
    """Forward-chain to a fixed point (trivial here but future-proof)."""
    facts = set(start)
    while apply_rule_once(facts):
        pass
    return facts

def query_types(facts: Set[Triple]) -> List[str]:
    """Answer the query { ?S a ?C }: list all type assertions."""
    return sorted(f"{s} a {o}" for (s, p, o) in facts if p == "a")

# ------------------------------ ARC ---------------------------------
def print_answer():
    print("Answer")
    print("======")
    derived = closure(FACTS)
    answers = query_types(derived)
    print("# Derived type assertions (?S a ?C)")
    for line in answers:
        print(line)
    # spotlight the intended new fact
    if (":Ghent", "a", ":HumanCommunity") in derived:
        print("\nResult: :Ghent is inferred to be a :HumanCommunity.")

def print_reason():
    print("\nReason why")
    print("==========")
    print("We use one universal rule:")
    print("  { ?x a :City } => { ?x a :HumanCommunity }")
    print("Instantiating with ?x = :Ghent and using the fact ':Ghent a :City' yields")
    print("  ':Ghent a :HumanCommunity'. The query { ?S a ?C } lists all such type facts.")

def print_check():
    print("\nCheck (harness)")
    print("===============")
    ok_all = True

    # 1) Expected two types for :Ghent after closure
    d = closure(FACTS)
    want = {
        (":Ghent", "a", ":City"),
        (":Ghent", "a", ":HumanCommunity"),
    }
    has_want = want.issubset(d)
    print(f"Contains expected {{:Ghent a :City, :Ghent a :HumanCommunity}}? {has_want}")
    ok_all &= has_want

    # 2) Idempotence: closing again adds nothing
    d2 = closure(d)
    idem = (d2 == d)
    print(f"Closure is a fixed point? {idem}")
    ok_all &= idem

    # 3) Generalization test: any City becomes HumanCommunity
    test = {
        (":A", "a", ":City"),
        (":B", "a", ":City"),
        (":C", "a", ":Thing"),
    }
    dtest = closure(test)
    gen_ok = ((":A", "a", ":HumanCommunity") in dtest and
              (":B", "a", ":HumanCommunity") in dtest and
              (":C", "a", ":HumanCommunity") not in dtest)
    print(f"Rule applies to all and only City-instances? {gen_ok}")
    ok_all &= gen_ok

    # 4) Query answers equal all 'a'-triples in the closure
    qlines = set(query_types(d))
    just_types = {f"{s} a {o}" for (s, p, o) in d if p == "a"}
    query_ok = (qlines == just_types)
    print(f"Query { '{ ?S a ?C }' } returns exactly the type facts? {query_ok}")
    ok_all &= query_ok

    print(f"\nAll checks passed? {ok_all}")

# ------------------------------- Main --------------------------------
if __name__ == "__main__":
    print_answer()
    print_reason()
    print_check()

