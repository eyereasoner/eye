#!/usr/bin/env python3
"""
Generic explicit-step proof builder: Unconditional Obligation vs Prohibition
===========================================================================

Testcase-3 summary:
  - Unconditional Obligation: ex:alice must odrl:read ex:resourceX.
  - Unconditional Prohibition: ex:alice must-not odrl:read ex:resourceX.
  - No refinements/conditions ⇒ both rules apply in every world.

Deontic axiom used (generic, reusable):
  (Ax) O(a) ⇒ P(a)

Therefore, in every world w:
  O(read, w) and F(read, w). From O(read, w) we also infer P(read, w),
  giving P(read, w) ∧ F(read, w) ⇒ Global state: Conflict.

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
    args: Tuple[Any, ...]  # e.g., (assignee, action, target, world)
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
    body: Any  # usually an Atom referring to var
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
        if k == "deontic-axiom":  return "O(a) ⇒ P(a)"
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

def forall_obl_unconditional(var: Term, assignee: str, action: str, target: str) -> ForAll:
    # Unconditional: holds for all worlds
    return ForAll(var, Atom("Obl", (assignee, action, target, var)))

def forall_proh_unconditional(var: Term, assignee: str, action: str, target: str) -> ForAll:
    # Unconditional: holds for all worlds
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
# Generic proof builder: Unconditional Obligation vs Prohibition
# -----------------------------------------------------------------------------

def build_unconditional_obligation_vs_prohibition_conflict_proof(
    assignee: str,
    action: str,
    target: str,
    universe_name: str = "W",
    witness_name: str = "w0"
) -> Tuple[Proof, str]:
    """
    Construct an explicit proof that:
      ∀w O(a) and ∀w F(a)  ⇒  ∀w (P(a) ∧ F(a))  ⇒ Conflict.
    Uses a generic deontic axiom O(a) ⇒ P(a).
    """
    proof = Proof()
    w = Term("w")

    # [1] Unconditional Obligation premise
    obl_prem = forall_obl_unconditional(w, assignee, action, target)
    s1 = proof.add("Premise", [], Conclusion("formula", obl_prem),
                   notes="Unconditional Obligation: holds in every world")

    # [2] Unconditional Prohibition premise
    proh_prem = forall_proh_unconditional(w, assignee, action, target)
    s2 = proof.add("Premise", [], Conclusion("formula", proh_prem),
                   notes="Unconditional Prohibition: holds in every world")

    # [3] Universe of worlds
    s3 = proof.add("Universe-Def", [], Conclusion("universe-def", (universe_name, "all possible worlds")),
                   notes="No constraints/refinements restrict applicability")

    # [4] Choose arbitrary world w0 ∈ W
    s4 = proof.add("Arbitrary-World", [s3], Conclusion("choose-world", (witness_name, universe_name)),
                   notes="World chosen arbitrarily; argument must not depend on which one")

    # [5] UE on Obligation at w0
    inst_obl = Atom("Obl", (assignee, action, target, witness_name))
    s5 = proof.add("Universal-Elim", [s1, s4], Conclusion("formula", inst_obl),
                   notes="Instantiate unconditional Obligation at w0")

    # [6] Deontic axiom O⇒P (generic)
    s6 = proof.add("Deontic-Axiom", [], Conclusion("deontic-axiom", None),
                   notes="From obligation, permission follows")

    # [7] Deontic-Apply: from O(a,w0) infer P(a,w0)
    inst_perm = Atom("Perm", (assignee, action, target, witness_name))
    s7 = proof.add("Deontic-Apply", [s6, s5], Conclusion("formula", inst_perm),
                   notes="Apply O⇒P at w0")

    # [8] UE on Prohibition at w0
    inst_proh = Atom("Proh", (assignee, action, target, witness_name))
    s8 = proof.add("Universal-Elim", [s2, s4], Conclusion("formula", inst_proh),
                   notes="Instantiate unconditional Prohibition at w0")

    # [9] ∧-Introduction: Perm ∧ Proh at w0
    conj = And(inst_perm, inst_proh)
    s9 = proof.add("And-Intro", [s7, s8], Conclusion("formula", conj),
                   notes="Clash at the arbitrary world")

    # [10] ∀-Introduction: generalize clash to all worlds
    general = ForAll(w, And(
        Atom("Perm", (assignee, action, target, w)),
        Atom("Proh", (assignee, action, target, w))
    ))
    s10 = proof.add("ForAll-Intro", [s9], Conclusion("formula", general),
                    notes="Since w0 was arbitrary, the conjunction holds in all worlds")

    # [11] Classification: Conflict
    result_uri = REPORT_URIS["Conflict"]
    proof.add("Classification", [s10], Conclusion("classification", result_uri),
              notes="Permission and Prohibition co-hold in every world")

    return proof, result_uri

# -----------------------------------------------------------------------------
# Demonstration: instantiate for testcase-3
# -----------------------------------------------------------------------------

def main() -> None:
    assignee = "ex:alice"
    action   = "odrl:read"
    target   = "ex:resourceX"

    proof, result_uri = build_unconditional_obligation_vs_prohibition_conflict_proof(
        assignee=assignee, action=action, target=target,
        universe_name="W", witness_name="w0"
    )

    # 1) Expected URI first
    print(result_uri)

    # 2) Pretty, numbered proof
    print("\n=== Pretty Proof ===\n")
    print(proof.pretty())

if __name__ == "__main__":
    main()

