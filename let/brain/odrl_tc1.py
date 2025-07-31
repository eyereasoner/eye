#!/usr/bin/env python3
"""
Generic explicit-step proof builder for unconditional Permission vs Prohibition
==============================================================================

Testcase-1 summary:
  - Unconditional Permission: ex:alice may odrl:read ex:resourceX.
  - Unconditional Prohibition: ex:alice must-not odrl:read ex:resourceX.
  - No refinements/constraints => both rules apply in every possible world.

Therefore, in *every* world w: Perm(read,w) ∧ Proh(read,w) ⇒ Global state: Conflict.

This program provides:
  - A tiny, reusable proof kernel (terms, formulas, explicit steps).
  - A generic builder `build_unconditional_conflict_proof` that works for any
    (assignee, action, target) triple with unconditional Permission & Prohibition.

No external libraries required.
"""

from __future__ import annotations
from dataclasses import dataclass, field
from typing import Any, List, Optional, Tuple

# -----------------------------------------------------------------------------
# Tiny logic: terms, formulas, pretty-printers (generic)
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
        def fmt(x: Any) -> str: return x if isinstance(x, str) else str(x)
        return f"{self.pred}(" + ", ".join(fmt(a) for a in self.args) + ")"

@dataclass(frozen=True)
class And:
    left: Any
    right: Any
    def pretty(self) -> str:
        L = self.left.pretty() if hasattr(self.left, "pretty") else str(self.left)
        R = self.right.pretty() if hasattr(self.right, "pretty") else str(self.right)
        return f"({L} ∧ {R})"

@dataclass(frozen=True)
class ForAll:
    var: Term
    body: Any  # body may reference var (here: Perm/Proh(..., var))
    def pretty(self) -> str:
        return f"∀{self.var}. {self.body.pretty()}"

# -----------------------------------------------------------------------------
# Proof kernel (pretty output only)
# -----------------------------------------------------------------------------

@dataclass
class Conclusion:
    kind: str
    payload: Any
    def pretty(self) -> str:
        k, p = self.kind, self.payload
        if k == "formula":        return p.pretty()
        if k == "universe-def":   return f"{p[0]} := {p[1]}"
        if k == "choose-world":   return f"{p[0]} ∈ {p[1]} (arbitrary)"
        if k == "classification": return f"Global activation state = {p}"
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
# Reusable builders for unconditional premises
# -----------------------------------------------------------------------------

def forall_perm_unconditional(var: Term, assignee: str, action: str, target: str) -> ForAll:
    # Unconditional: holds for all worlds (no antecedent)
    return ForAll(var, Atom("Perm", (assignee, action, target, var)))

def forall_proh_unconditional(var: Term, assignee: str, action: str, target: str) -> ForAll:
    # Unconditional: holds for all worlds (no antecedent)
    return ForAll(var, Atom("Proh", (assignee, action, target, var)))

# -----------------------------------------------------------------------------
# Report URIs
# -----------------------------------------------------------------------------

REPORT_URIS = {
    "Conflict":   "https://w3id.org/force/compliance-report#Conflict",
    "Ambiguous":  "https://w3id.org/force/compliance-report#Ambiguous",
    "Prohibited": "https://w3id.org/force/compliance-report#Prohibited",
    "Permitted":  "https://w3id.org/force/compliance-report#Permitted",
    "NoConflict": "https://w3id.org/force/compliance-report#NoConflict",
}

# -----------------------------------------------------------------------------
# Generic proof builder for unconditional Permission vs Prohibition
# -----------------------------------------------------------------------------

def build_unconditional_conflict_proof(assignee: str, action: str, target: str,
                                       universe_name: str = "W",
                                       witness_name: str = "w0") -> Tuple[Proof, str]:
    """
    Build an explicit proof that unconditional Permission and Prohibition
    on the same (assignee, action, target) yield Conflict.
    """
    proof = Proof()
    w = Term("w")

    # [1] Unconditional Permission premise
    perm_prem = forall_perm_unconditional(w, assignee, action, target)
    s1 = proof.add("Premise", [], Conclusion("formula", perm_prem),
                   notes="Unconditional Permission: holds in every world")

    # [2] Unconditional Prohibition premise
    proh_prem = forall_proh_unconditional(w, assignee, action, target)
    s2 = proof.add("Premise", [], Conclusion("formula", proh_prem),
                   notes="Unconditional Prohibition: holds in every world")

    # [3] Define the universe of worlds
    s3 = proof.add("Universe-Def", [], Conclusion("universe-def", (universe_name, "all possible worlds")),
                   notes="No constraints/refinements restrict applicability")

    # [4] Choose an arbitrary world w0 ∈ W
    s4 = proof.add("Arbitrary-World", [s3], Conclusion("choose-world", (witness_name, universe_name)),
                   notes="World chosen arbitrarily; proof must not depend on which one")

    # [5] Universal-Elim on Permission at w0
    inst_perm = Atom("Perm", (assignee, action, target, witness_name))
    s5 = proof.add("Universal-Elim", [s1, s4], Conclusion("formula", inst_perm),
                   notes="Instantiate unconditional Permission at w0")

    # [6] Universal-Elim on Prohibition at w0
    inst_proh = Atom("Proh", (assignee, action, target, witness_name))
    s6 = proof.add("Universal-Elim", [s2, s4], Conclusion("formula", inst_proh),
                   notes="Instantiate unconditional Prohibition at w0")

    # [7] ∧-Introduction: Perm ∧ Proh at w0
    conj = And(inst_perm, inst_proh)
    s7 = proof.add("And-Intro", [s5, s6], Conclusion("formula", conj),
                   notes="Clash at the arbitrary world")

    # [8] ∀-Introduction: generalize clash to all worlds
    general = ForAll(w, And(
        Atom("Perm", (assignee, action, target, w)),
        Atom("Proh", (assignee, action, target, w))
    ))
    s8 = proof.add("ForAll-Intro", [s7], Conclusion("formula", general),
                   notes="Since w0 was arbitrary, the conjunction holds in all worlds")

    # [9] Classification: Conflict
    result_uri = REPORT_URIS["Conflict"]
    proof.add("Classification", [s8], Conclusion("classification", result_uri),
              notes="Perm and Proh hold in every possible world")

    return proof, result_uri

# -----------------------------------------------------------------------------
# Demonstration: instantiate for testcase-1
# -----------------------------------------------------------------------------

def main() -> None:
    assignee = "ex:alice"
    action   = "odrl:read"
    target   = "ex:resourceX"

    proof, result_uri = build_unconditional_conflict_proof(assignee, action, target)

    # 1) Expected URI first
    print(result_uri)

    # 2) Pretty, numbered proof
    print("\n=== Pretty Proof ===\n")
    print(proof.pretty())

if __name__ == "__main__":
    main()

