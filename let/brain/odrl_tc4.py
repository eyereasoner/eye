#!/usr/bin/env python3
"""
Generic explicit-step proof builder: Permission-with-Duty vs Prohibition
=======================================================================

Testcase-4 summary:
  - Permission to odrl:use ex:resourceX with a duty to ex:signContract (assigner ex:bob, target ex:contract).
  - Prohibition to ex:signContract (assigner ex:bob, target ex:contract).
  - Intuition: Exercising the permission requires fulfilling the duty (signing), yet signing is prohibited.

This program provides:
  - A tiny reusable proof kernel (terms, formulas, explicit steps).
  - A generic builder `build_duty_prohibited_conflict_proof` that works for any
    (assignee, perm_action, perm_target, duty_action, duty_assigner, duty_target)
    paired with a prohibition on the same duty action/assigner/target.

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
    body: Any
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
        if k == "duty-obligation":return f"PermWithDuty(…, {p}) ⇒ Obl({p})"
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
# Reusable builders for premises
# -----------------------------------------------------------------------------

def forall_permission_with_duty(var: Term,
                                assignee: str,
                                perm_action: str,
                                perm_target: str,
                                duty_action: str,
                                duty_assigner: str,
                                duty_target: str) -> ForAll:
    """
    ∀w. PermWithDuty(assignee, perm_action, perm_target,
                     duty_action, duty_assigner, duty_target, w)
    """
    return ForAll(var, Atom("PermWithDuty", (assignee, perm_action, perm_target,
                                             duty_action, duty_assigner, duty_target, var)))

def forall_prohibition_on_duty(var: Term,
                               assignee: str,
                               duty_action: str,
                               duty_assigner: str,
                               duty_target: str) -> ForAll:
    """ ∀w. Proh(assignee, duty_action, duty_assigner, duty_target, w) """
    return ForAll(var, Atom("Proh", (assignee, duty_action, duty_assigner, duty_target, var)))

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
# Generic proof builder: Permission-with-Duty vs Prohibition on the duty
# -----------------------------------------------------------------------------

def build_duty_prohibited_conflict_proof(
    assignee: str,
    perm_action: str,
    perm_target: str,
    duty_action: str,
    duty_assigner: str,
    duty_target: str,
    universe_name: str = "W",
    witness_name: str = "w0",
    show_O_implies_P: bool = True
) -> Tuple[Proof, str]:
    """
    Construct an explicit proof that:
      ∀w PermWithDuty(perm, duty)  and  ∀w Proh(duty)
      together entail a clash on the duty action in every world.

    Rules used (generic):
      - Duty-Induces-Obligation: from PermWithDuty(…, duty, w) infer Obl(duty, w).
      - (optional) Deontic O⇒P: from Obl(duty, w) infer Perm(duty, w).
    """
    proof = Proof()
    w = Term("w")

    # [1] Premise: Permission-with-duty (unconditional apart from the duty itself)
    pmd_prem = forall_permission_with_duty(w, assignee, perm_action, perm_target,
                                           duty_action, duty_assigner, duty_target)
    s1 = proof.add("Premise", [], Conclusion("formula", pmd_prem),
                   notes="Permission with attached duty holds in every world")

    # [2] Generic rule: Duty-Induces-Obligation (we record the rule then derive a universal)
    duty_sig = f"{assignee}, {duty_action}, {duty_assigner}, {duty_target}, w"
    s2 = proof.add("Duty-Induces-Obligation", [s1],
                   Conclusion("duty-obligation", duty_sig),
                   notes="To exercise the permission, the duty must be fulfilled")

    # [3] Derived universal: ∀w. Obl(assignee, duty_action, duty_assigner, duty_target, w)
    obl_univ = ForAll(w, Atom("Obl", (assignee, duty_action, duty_assigner, duty_target, w)))
    s3 = proof.add("Derive-Universal", [s2], Conclusion("formula", obl_univ),
                   notes="Obligation on the duty induced by the permission-with-duty")

    # [4] Premise: Prohibition on the duty (unconditional)
    proh_prem = forall_prohibition_on_duty(w, assignee, duty_action, duty_assigner, duty_target)
    s4 = proof.add("Premise", [], Conclusion("formula", proh_prem),
                   notes="Unconditional Prohibition on the duty action")

    # [5] Universe of worlds (unrestricted)
    s5 = proof.add("Universe-Def", [], Conclusion("universe-def", (universe_name, "all possible worlds")),
                   notes="No external conditions restrict applicability")

    # [6] Choose an arbitrary world w0 ∈ W
    s6 = proof.add("Arbitrary-World", [s5], Conclusion("choose-world", (witness_name, universe_name)),
                   notes="World chosen arbitrarily; argument must not depend on which one")

    # [7] UE: Obl(duty) at w0
    inst_obl = Atom("Obl", (assignee, duty_action, duty_assigner, duty_target, witness_name))
    s7 = proof.add("Universal-Elim", [s3, s6], Conclusion("formula", inst_obl),
                   notes="Instantiate the induced obligation on the duty at w0")

    # (optional) O⇒P to show Permit & Prohibit explicitly on the duty
    if show_O_implies_P:
        s8 = proof.add("Deontic-Axiom", [], Conclusion("deontic-axiom", None),
                       notes="From obligation, permission follows (O⇒P)")
        inst_perm = Atom("Perm", (assignee, duty_action, duty_assigner, duty_target, witness_name))
        s9 = proof.add("Deontic-Apply", [s8, s7], Conclusion("formula", inst_perm),
                       notes="Apply O⇒P at w0")
        perm_step_id_for_conj = s9
    else:
        perm_step_id_for_conj = s7  # use Obl directly in the conjunction (O ∧ F)

    # [10] UE: Proh(duty) at w0
    inst_proh = Atom("Proh", (assignee, duty_action, duty_assigner, duty_target, witness_name))
    s10 = proof.add("Universal-Elim", [s4, s6], Conclusion("formula", inst_proh),
                    notes="Instantiate the prohibition on the duty at w0")

    # [11] ∧-Introduction: (Perm|Obl) ∧ Proh on the duty at w0
    left_atom = (Atom("Perm", (assignee, duty_action, duty_assigner, duty_target, witness_name))
                 if show_O_implies_P else
                 Atom("Obl", (assignee, duty_action, duty_assigner, duty_target, witness_name)))
    conj = And(left_atom, inst_proh)
    s11 = proof.add("And-Intro", [perm_step_id_for_conj, s10], Conclusion("formula", conj),
                    notes="Clash at the arbitrary world on the duty action")

    # [12] ∀-Introduction: generalize clash to all worlds
    general = ForAll(w, And(
        left_atom.__class__(*left_atom.__dict__.values()) if hasattr(left_atom, "__dict__") else left_atom,
        Atom("Proh", (assignee, duty_action, duty_assigner, duty_target, w))
    ))
    s12 = proof.add("ForAll-Intro", [s11], Conclusion("formula", general),
                    notes="Since w0 was arbitrary, the clash holds in all worlds")

    # [13] Classification: Conflict
    result_uri = REPORT_URIS["Conflict"]
    proof.add("Classification", [s12], Conclusion("classification", result_uri),
              notes="The duty is simultaneously (Perm|Obl) and Proh in every world")

    return proof, result_uri

# -----------------------------------------------------------------------------
# Demonstration: instantiate for testcase-4
# -----------------------------------------------------------------------------

def main() -> None:
    assignee      = "ex:alice"
    perm_action   = "odrl:use"
    perm_target   = "ex:resourceX"
    duty_action   = "ex:signContract"
    duty_assigner = "ex:bob"
    duty_target   = "ex:contract"

    proof, result_uri = build_duty_prohibited_conflict_proof(
        assignee=assignee,
        perm_action=perm_action,
        perm_target=perm_target,
        duty_action=duty_action,
        duty_assigner=duty_assigner,
        duty_target=duty_target,
        universe_name="W",
        witness_name="w0",
        show_O_implies_P=True  # set False if you prefer O ∧ F instead of P ∧ F
    )

    # 1) Expected URI first
    print(result_uri)

    # 2) Pretty, numbered proof
    print("\n=== Pretty Proof ===\n")
    print(proof.pretty())

if __name__ == "__main__":
    main()

