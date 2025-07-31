#!/usr/bin/env python3
"""
Generic explicit-step proof builder for rule-based worlds (testcase-10)
======================================================================

Scenario (ODRL Test Conflicts, testcase-10):
  - Permission to read if Student OR Employee.
  - Prohibition to read if Student AND Employee.

Expected global activation state: Ambiguous
  Some worlds permit the action, while other worlds prohibit it.

This program uses a generic, reusable proof kernel for finite boolean "worlds".
You can reuse the builder by changing:
  - the antecedent predicates that imply Permission,
  - the antecedent predicates whose conjunction implies Prohibition,
  - the action/target labels.

No external libraries required.
"""

from __future__ import annotations
from dataclasses import dataclass, field
from typing import Any, Dict, Iterable, List, Optional, Sequence, Tuple

# -----------------------------------------------------------------------------
# Tiny logic: terms, formulas, and pretty-printers (generic)
# -----------------------------------------------------------------------------

@dataclass(frozen=True)
class Term:
    name: str
    def __str__(self) -> str: return self.name

@dataclass(frozen=True)
class Atom:
    pred: str
    args: Tuple[Any, ...]
    def pretty(self) -> str:
        def fmt(a: Any) -> str:
            return a if isinstance(a, str) else str(a)
        return f"{self.pred}(" + ", ".join(fmt(x) for x in self.args) + ")"

@dataclass(frozen=True)
class And:
    left: Any
    right: Any
    def pretty(self) -> str:
        L = self.left.pretty() if hasattr(self.left, "pretty") else str(self.left)
        R = self.right.pretty() if hasattr(self.right, "pretty") else str(self.right)
        return f"({L} ∧ {R})"

@dataclass(frozen=True)
class Imp:
    ant: Any
    cons: Any
    def pretty(self) -> str:
        A = self.ant.pretty() if hasattr(self.ant, "pretty") else str(self.ant)
        C = self.cons.pretty() if hasattr(self.cons, "pretty") else str(self.cons)
        return f"({A} → {C})"

@dataclass(frozen=True)
class ForAll:
    var: Term
    body: Any  # usually an Imp with var occurrences
    def pretty(self) -> str:
        return f"∀{self.var}. {self.body.pretty()}"

# -----------------------------------------------------------------------------
# Proof kernel (generic, reusable)
# -----------------------------------------------------------------------------

@dataclass
class Conclusion:
    kind: str
    payload: Any
    def pretty(self) -> str:
        k, p = self.kind, self.payload
        if k == "formula":       return p.pretty()
        if k == "world-def":     return f"{p[0]} := {p[1]}"
        if k == "world-fact":    return f"{p[0]}[{p[1]}] = {p[2]}"
        if k == "choose-world":  return f"{p[0]} := {p[1]}"
        if k == "classification":return f"Global activation state = {p}"
        return str(p)

@dataclass
class Step:
    id: int
    rule: str
    premises: List[int]
    conclusion: Conclusion
    notes: Optional[str] = None

@dataclass
class Proof:
    steps: List[Step] = field(default_factory=list)
    def add(self, rule: str, premises: List[int], conclusion: Conclusion, notes: Optional[str] = None) -> int:
        sid = len(self.steps) + 1
        self.steps.append(Step(sid, rule, premises, conclusion, notes))
        return sid
    def pretty(self) -> str:
        lines = []
        for s in self.steps:
            prem = f" [{', '.join(map(str, s.premises))}]" if s.premises else ""
            note = f"  // {s.notes}" if s.notes else ""
            lines.append(f"[{s.id}] {s.rule}{prem}: {s.conclusion.pretty()}{note}")
        return "\n".join(lines)

# -----------------------------------------------------------------------------
# World model and evaluation (generic for boolean predicates)
# -----------------------------------------------------------------------------

World = Dict[str, bool]

def enumerate_worlds(predicates: Sequence[str]) -> List[World]:
    """All boolean assignments over the given predicate names."""
    preds = list(predicates)
    n = len(preds)
    worlds: List[World] = []
    for mask in range(1 << n):
        w = {preds[i]: bool((mask >> i) & 1) for i in range(n)}
        worlds.append(w)
    return worlds

def world_str(w: World) -> str:
    return "(" + ", ".join(f"{k}={'T' if v else 'F'}" for k, v in sorted(w.items())) + ")"

# -----------------------------------------------------------------------------
# Reusable builders for testcase-10-like structures
# -----------------------------------------------------------------------------

def forall_perm_rule(var: Term, antecedent_pred: str, action: str, target: str) -> ForAll:
    return ForAll(var, Imp(Atom(antecedent_pred, (var,)), Atom("Perm", (action, target, var))))

def forall_proh_rule_conj(var: Term, antecedents: Sequence[str], action: str, target: str) -> ForAll:
    assert len(antecedents) >= 2, "Use at least a binary conjunction for the prohibition antecedent."
    conj = Atom(antecedents[0], (var,))
    for p in antecedents[1:]:
        conj = And(conj, Atom(p, (var,)))
    return ForAll(var, Imp(conj, Atom("Proh", (action, target, var))))

def eval_perm_proh_in_world(w: World,
                            perm_ants: Sequence[str],
                            proh_ants: Sequence[str]) -> Tuple[bool, bool, bool]:
    perm = any(w.get(p, False) for p in perm_ants)
    proh = all(w.get(p, False) for p in proh_ants)
    conflict = perm and proh
    return perm, proh, conflict

def classify_worlds(worlds: Sequence[World],
                    perm_ants: Sequence[str],
                    proh_ants: Sequence[str]) -> str:
    exists_perm = exists_proh = TrueConflictEverywhere = False
    any_conflict = True
    # Evaluate each world
    per_flags = []
    proh_flags = []
    conf_flags = []
    for w in worlds:
        p, f, c = eval_perm_proh_in_world(w, perm_ants, proh_ants)
        per_flags.append(p); proh_flags.append(f); conf_flags.append(c)
    exists_perm = any(per_flags)
    exists_proh = any(proh_flags)
    conflict_always = all(conf_flags)
    if conflict_always: return "https://w3id.org/force/compliance-report#Conflict"
    if exists_perm and exists_proh: return "https://w3id.org/force/compliance-report#Ambiguous"
    if exists_proh and not exists_perm: return "https://w3id.org/force/compliance-report#Prohibited"
    if exists_perm and not exists_proh: return "https://w3id.org/force/compliance-report#Permitted"
    return "https://w3id.org/force/compliance-report#NoConflict"

# -----------------------------------------------------------------------------
# Generic proof builder for "Perm if any antecedent" and "Proh if all antecedents"
# -----------------------------------------------------------------------------

def build_ambiguous_proof(perm_antecedents: Sequence[str],
                          proh_antecedents: Sequence[str],
                          action: str,
                          target: str,
                          universe_name: str = "W",
                          w_perm_name: str = "w_perm",
                          w_proh_name: str = "w_proh") -> Tuple[Proof, str]:
    """
    Build an explicit proof:
      Premises → Define universe → Choose witnesses → UE + Modus Ponens → ∧-Intro → Classification.
    """
    proof = Proof()
    var = Term("w")

    # [1..k] Permission premises: ∀w. Antecedent_i(w) → Perm(action,target,w)
    perm_prem_steps: List[int] = []
    for ant in perm_antecedents:
        s = proof.add("Premise", [], Conclusion("formula", forall_perm_rule(var, ant, action, target)),
                      notes=f"If {ant}(w) then Perm({action},{target},w)")
        perm_prem_steps.append(s)

    # [k+1] Prohibition premise: ∀w. (∧ antecedents)(w) → Proh(action,target,w)
    proh_step = proof.add("Premise", [],
                          Conclusion("formula", forall_proh_rule_conj(var, proh_antecedents, action, target)),
                          notes=f"If {' ∧ '.join(proh_antecedents)}(w) then Proh({action},{target},w)")

    # [..] Define the universe of worlds
    preds = sorted(set(list(perm_antecedents) + list(proh_antecedents)))
    worlds = enumerate_worlds(preds)
    proof.add("Universe-Def", [], Conclusion("world-def", (universe_name, f"{{ all assignments over {preds} }}")),
              notes=f"{len(worlds)} worlds total: " + ", ".join(world_str(w) for w in worlds))

    # Pick a permitted-only witness: first antecedent true, others false
    w_perm = {p: (p == perm_antecedents[0]) for p in preds}
    # Pick a prohibited witness: all proh antecedents true (and thus also permits via at least one perm antecedent here)
    w_proh = {p: (p in proh_antecedents) for p in preds}

    # [..] Choose witnesses
    s_wperm = proof.add("Choose-World", [], Conclusion("choose-world", (w_perm_name, world_str(w_perm))),
                        notes=f"{w_perm_name} ∈ {universe_name}")
    s_wproh = proof.add("Choose-World", [], Conclusion("choose-world", (w_proh_name, world_str(w_proh))),
                        notes=f"{w_proh_name} ∈ {universe_name}")

    # Facts for witnesses (Student/Employee truth values)
    for name, w in [(w_perm_name, w_perm), (w_proh_name, w_proh)]:
        for k, v in sorted(w.items()):
            proof.add("World-Fact", [], Conclusion("world-fact", (name, k, "T" if v else "F")))

    # Derive Perm at w_perm from corresponding premise
    # Use the premise whose antecedent we set True in w_perm
    perm_ant_for_wperm = perm_antecedents[0]
    proof.add("Universal-Elim + MP", [perm_prem_steps[0], s_wperm],
              Conclusion("formula", Atom("Perm", (action, target, w_perm_name))),
              notes=f"From {perm_ant_for_wperm}({w_perm_name})")

    # Derive Proh at w_proh from prohibition premise, using conjunction of all proh antecedents
    proof.add("Universal-Elim + MP", [proh_step, s_wproh],
              Conclusion("formula", Atom("Proh", (action, target, w_proh_name))),
              notes=f"From {' ∧ '.join(a + '(' + w_proh_name + ')' for a in proh_antecedents)}")

    # (Optional) Also show Perm holds in w_proh (since at least one perm antecedent is true there)
    # Pick the first perm antecedent present in w_proh to witness Perm
    for i, ant in enumerate(perm_antecedents):
        if ant in proh_antecedents:  # in testcase-10 both Student and Employee imply permission
            proof.add("Universal-Elim + MP", [perm_prem_steps[i], s_wproh],
                      Conclusion("formula", Atom("Perm", (action, target, w_proh_name))),
                      notes=f"From {ant}({w_proh_name})")
            break

    # Classification (computed over all worlds)
    result_uri = classify_worlds(worlds, perm_antecedents, proh_antecedents)
    proof.add("Classification", [], Conclusion("classification", result_uri),
              notes="∃ permitted-only world and ∃ prohibited world; not all worlds are conflicts")

    return proof, result_uri

# -----------------------------------------------------------------------------
# Demonstration: testcase-10 plugged into the generic builder
# -----------------------------------------------------------------------------

def main() -> None:
    action = "odrl:read"
    target = "ex:resourceX"
    perm_antecedents = ["Student", "Employee"]         # P if Student; P if Employee
    proh_antecedents = ["Student", "Employee"]         # F if Student ∧ Employee

    proof, result_uri = build_ambiguous_proof(perm_antecedents, proh_antecedents, action, target)

    # 1) Always print the expected URI first
    print(result_uri)

    # 2) Pretty, numbered proof with explicit steps
    print("\n=== Pretty Proof ===\n")
    print(proof.pretty())

if __name__ == "__main__":
    main()

