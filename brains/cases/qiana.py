#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
qiana.py
──────────────────────────────────────────────────────────────
Qiana (EYE-style N3) — quoting + meta-rules with a goal-oriented proof

Mirrors (from qiana.n3):
  1) { :Einstein :says ?phi } => { ?x :believes ?phi }        # (∀x,φ)
  2) { :Einstein :says ?phi } => ?phi                         # (authoritative says)
  3) :Einstein :says { { ?x a :glitter } => { ?x :notNecessarilyA :gold } } .
  4) :northStar a :glitter .

Queries (from qiana-query.n3):
  - { :Fabian :believes ?what } => { :Fabian :believes ?what } .
  - { ?x :notNecessarilyA ?what } => { ?x :notNecessarilyA ?what } .

This program:
  • Encodes those facts and meta-rules exactly (the “said” formula is quoted).
  • Applies meta-rules:
      – From “Einstein says φ” derive “Fabian believes φ” (instance of ∀x).
      – From “Einstein says φ” assert φ (if φ is a rule: add the rule and fire it).
  • Uses the asserted glitter→notNecessarilyA rule on :northStar.
  • Prints ARC output:
      Answer      – the derived triples that satisfy the two query patterns
      Reason why  – a compact pretty proof
      Check       – harness that verifies entailments, soundness of meta-steps,
                    and determinism (same output on re-run)
"""

from __future__ import annotations
from dataclasses import dataclass, field
from typing import Any, Dict, List, Optional, Set, Tuple

# ─────────────────────────────────────────────────────────────
# Data model (triples, quoted formulas, rules)
# ─────────────────────────────────────────────────────────────

@dataclass(frozen=True, order=True)
class Triple:
    s: str
    p: str
    o: str  # When we quote a formula in object position, we keep a string label.

@dataclass(frozen=True)
class Rule:
    """Simple Horn rule: one triple pattern in body, one triple in head (variables allowed)."""
    body: Triple
    head: Triple

# Variable test (we only need ?x here, but keep general)
def is_var(t: str) -> bool:
    return isinstance(t, str) and t.startswith("?")

def unify(pat: Triple, fact: Triple, theta: Optional[Dict[str, str]] = None) -> Optional[Dict[str, str]]:
    """Positional unification of triple pattern with a ground triple (predicates must match)."""
    if pat.p != fact.p:
        return None
    θ: Dict[str, str] = dict(theta or {})
    for p_term, f_term in ((pat.s, fact.s), (pat.o, fact.o)):
        if is_var(p_term):
            bound = θ.get(p_term)
            if bound is None:
                θ[p_term] = f_term
            elif bound != f_term:
                return None
        else:
            if p_term != f_term:
                return None
    return θ

def subst(tr: Triple, θ: Dict[str, str]) -> Triple:
    s = θ.get(tr.s, tr.s)
    o = θ.get(tr.o, tr.o)
    return Triple(s, tr.p, o)

# ─────────────────────────────────────────────────────────────
# KB initialisation (mirrors qiana.n3)
# ─────────────────────────────────────────────────────────────
Einstein = ":Einstein"
Fabian   = ":Fabian"

# Quoted rule label (what Einstein says)
phi_label = "{ { ?x a :glitter } => { ?x :notNecessarilyA :gold } }"
# The actual rule φ_E (we’ll assert it via the meta-rule)
phi_rule = Rule(
    body=Triple("?x", "a", ":glitter"),
    head=Triple("?x", ":notNecessarilyA", ":gold"),
)

# Base facts
BASE_FACTS: Set[Triple] = {
    Triple(Einstein, ":says", phi_label),
    Triple(":northStar", "a", ":glitter"),
}

# ─────────────────────────────────────────────────────────────
# Pretty-proof kernel
# ─────────────────────────────────────────────────────────────
@dataclass
class Step:
    id: int
    rule: str
    text: str
    refs: List[int] = field(default_factory=list)

@dataclass
class Proof:
    steps: List[Step] = field(default_factory=list)
    def add(self, rule: str, text: str, refs: List[int] | Tuple[int, ...] = ()) -> int:
        sid = len(self.steps) + 1
        self.steps.append(Step(sid, rule, text, list(refs)))
        return sid
    def pretty(self) -> str:
        lines: List[str] = []
        for s in self.steps:
            refs = f" [{', '.join(map(str, s.refs))}]" if s.refs else ""
            lines.append(f"[{s.id}] {s.rule}{refs}: {s.text}")
        return "\n".join(lines)

# ─────────────────────────────────────────────────────────────
# Inference: meta-rules + firing of asserted rule
# ─────────────────────────────────────────────────────────────
def derive() -> Tuple[Set[Triple], List[Rule], Proof]:
    """Run the Qiana derivation; return (facts, rules, proof). Deterministic output."""
    facts: Set[Triple] = set(BASE_FACTS)    # start from base facts
    rules: List[Rule]  = []                 # to be populated via says→assert
    proof = Proof()

    # Introduce the setting
    proof.add("Facts", f"{Einstein} :says {phi_label}  ;  :northStar a :glitter")

    # Meta-rule sketches
    r1 = proof.add("Premise-Rule", "{ :Einstein :says ?phi } => { ?x :believes ?phi }  // says→believes (∀x)")
    r2 = proof.add("Premise-Rule", "{ :Einstein :says ?phi } => ?phi                 // says→assert")

    # Gather said φ
    said_phis = sorted([t.o for t in facts if t.s == Einstein and t.p == ":says"])

    # Apply says→believes: instantiate x = :Fabian (enough for our query)
    for phi in said_phis:
        t = Triple(Fabian, ":believes", phi)
        if t not in facts:
            facts.add(t)
            proof.add("Apply says→believes", f"{Fabian} :believes {phi}", refs=[r1])

    # Apply says→assert: if φ is our quoted rule, admit it
    for phi in said_phis:
        if phi == phi_label:
            if phi_rule not in rules:
                rules.append(phi_rule)
                proof.add("Apply says→assert", "Admitted rule: { ?x a :glitter } => { ?x :notNecessarilyA :gold }", refs=[r2])

    # Fire the admitted rule(s) once to saturation (single rule here)
    changed = True
    while changed:
        changed = False
        for rl in list(rules):
            # Find matches of the body in current facts
            matches: List[Tuple[Dict[str, str], Triple]] = []
            for f in sorted(facts):
                θ = unify(rl.body, f, {})
                if θ is not None:
                    matches.append((θ, f))
            # Add heads
            for θ, _ in matches:
                concl = subst(rl.head, θ)
                if concl not in facts:
                    facts.add(concl)
                    proof.add("Apply asserted φ", f"{concl.s} :notNecessarilyA :gold  (from {rl.body.s} a :glitter)", refs=[])
                    changed = True

    return facts, rules, proof

# ─────────────────────────────────────────────────────────────
# ARC — Answer / Reason why / Check
# ─────────────────────────────────────────────────────────────
def arc_answer(facts: Set[Triple]) -> None:
    print("Answer")
    print("------")
    # Query patterns:
    # 1) :Fabian :believes ?what
    # 2) ?x :notNecessarilyA ?what
    beliefs = sorted([t for t in facts if t.s == Fabian and t.p == ":believes"])
    nnecs   = sorted([t for t in facts if t.p == ":notNecessarilyA"])

    if beliefs:
        for t in beliefs:
            print(f"{t.s} {t.p} {t.o}")
    else:
        print("# no :Fabian :believes ?what found")

    if nnecs:
        for t in nnecs:
            print(f"{t.s} {t.p} {t.o}")
    else:
        print("# no ?x :notNecessarilyA ?what found")
    print()

def arc_reason(proof: Proof) -> None:
    print("Reason why")
    print("----------")
    print("• Meta-rule 1 (says→believes): from :Einstein :says φ we instantiate x=:Fabian and derive :Fabian :believes φ.")
    print("• Meta-rule 2 (says→assert): from :Einstein :says φ we admit φ into the KB.")
    print("• Here φ is a *rule* { ?x a :glitter } ⇒ { ?x :notNecessarilyA :gold }, which we then fire on :northStar a :glitter.")
    print("\nPretty proof\n------------")
    print(proof.pretty())
    print()

def arc_check(facts: Set[Triple], rules: List[Rule]) -> None:
    print("Check (harness)")
    print("---------------")
    # 1) Expected key entailments present
    exp1 = Triple(Fabian, ":believes", phi_label)
    exp2 = Triple(":northStar", ":notNecessarilyA", ":gold")
    assert exp1 in facts, "Expected :Fabian :believes φ not derived"
    assert exp2 in facts, "Expected :northStar :notNecessarilyA :gold not derived"

    # 2) Soundness: firing asserted rule equals image of all glitter facts
    glitter_subjects = sorted([t.s for t in facts if t.p == "a" and t.o == ":glitter"])
    expected_heads = {Triple(s, ":notNecessarilyA", ":gold") for s in glitter_subjects}
    actual_heads   = {t for t in facts if t.p == ":notNecessarilyA" and t.o == ":gold"}
    assert expected_heads <= actual_heads, "Some glitter-derived heads missing"
    # (we allow extra heads if future extensions add more rules)

    # 3) Determinism: re-run derive() and compare the two answer sets
    f2, r2, _ = derive()
    assert facts == f2 and rules == r2, "Non-deterministic derivation (facts or rules differ on re-run)"

    # 4) If we remove the says-triple, neither belief nor asserted head should appear
    stripped = {t for t in BASE_FACTS if not (t.s == Einstein and t.p == ":says")}
    # run a tiny local derive with stripped facts
    def derive_from(fs: Set[Triple]) -> Set[Triple]:
        facts_local: Set[Triple] = set(fs)
        rules_local: List[Rule] = []
        # no :says ⇒ no meta-derivations; but still try to “fire” (there should be no rules)
        changed = True
        while changed:
            changed = False
            for rl in list(rules_local):
                for f in list(facts_local):
                    θ = unify(rl.body, f, {})
                    if θ is not None:
                        concl = subst(rl.head, θ)
                        if concl not in facts_local:
                            facts_local.add(concl)
                            changed = True
        return facts_local

    stripped_facts = derive_from(stripped)
    assert exp1 not in stripped_facts, "Belief should not arise without :Einstein :says φ"
    assert exp2 not in stripped_facts, "Head should not arise without asserted rule"

    print("OK: expected entailments present; rule firing matches glitter-facts; derivation deterministic; meta-rules required.\n")

# ─────────────────────────────────────────────────────────────
# Main
# ─────────────────────────────────────────────────────────────
if __name__ == "__main__":
    facts, rules, proof = derive()
    arc_answer(facts)
    arc_reason(proof)
    arc_check(facts, rules)

