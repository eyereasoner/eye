#!/usr/bin/env python3
"""
Testcase-9 with a generic, explicit proof generator (pretty output only)
=======================================================================

Scenario (testcase-9):
  - Permission:  odrl:read ex:resourceX for all dates d in [2025-01-01, 2025-12-31].
  - Prohibition: odrl:read ex:resourceX for all dates d in (-∞, 2026-01-01).

Expected global activation state: Conflict
  Because the permission window is fully contained in the prohibition window.

No external libraries required.
"""

from __future__ import annotations
from dataclasses import dataclass, field
from datetime import date, timedelta
from typing import List, Optional, Tuple, Union, Any

# -----------------------------------------------------------------------------
# Interval utilities over dates
# -----------------------------------------------------------------------------

DateInterval = Tuple[Optional[date], bool, Optional[date], bool]  # (l, lc, u, uc)

def interval_from_op(op: str, value: date) -> DateInterval:
    if op == "lt":
        return (None, False, value, False)
    if op == "lteq":
        return (None, False, value, True)
    if op == "gt":
        return (value, False, None, False)
    if op == "gteq":
        return (value, True, None, False)
    if op == "eq":
        return (value, True, value, True)
    raise ValueError(f"Unsupported operator: {op}")

def interval_intersection(a: DateInterval, b: DateInterval) -> Optional[DateInterval]:
    (al, alc, au, auc) = a
    (bl, blc, bu, buc) = b

    # Lower bound
    if al is None:
        nl, nlc = bl, blc
    elif bl is None:
        nl, nlc = al, alc
    else:
        if al > bl:
            nl, nlc = al, alc
        elif bl > al:
            nl, nlc = bl, blc
        else:
            nl, nlc = al, alc and blc

    # Upper bound
    if au is None:
        nu, nuc = bu, buc
    elif bu is None:
        nu, nuc = au, auc
    else:
        if au < bu:
            nu, nuc = au, auc
        elif bu < au:
            nu, nuc = bu, buc
        else:
            nu, nuc = au, auc and buc

    # Empty?
    if nl is not None and nu is not None:
        if nl > nu:
            return None
        if nl == nu and not (nlc and nuc):
            return None

    return (nl, nlc, nu, nuc)

def interval_to_str(iv: DateInterval) -> str:
    (l, lc, u, uc) = iv
    l_s = "-∞" if l is None else l.isoformat()
    u_s = "+∞" if u is None else u.isoformat()
    return f"{'[' if lc else '('}{l_s}, {u_s}{']' if uc else ')'}"

def pick_witness(iv: DateInterval) -> date:
    (l, lc, u, uc) = iv
    if l is not None:
        return l if lc else (l + timedelta(days=1))
    # Else choose just before upper bound (which must exist)
    assert u is not None
    return u if uc else (u - timedelta(days=1))

# -----------------------------------------------------------------------------
# Tiny logic: terms, predicates, formulas
# -----------------------------------------------------------------------------

@dataclass(frozen=True)
class Term:
    name: str
    def __str__(self) -> str:
        return self.name

@dataclass(frozen=True)
class Atom:
    pred: str                     # e.g., "Perm" or "Proh"
    args: Tuple[Union[str, Term, date], ...]
    def pretty(self) -> str:
        def fmt(a: Union[str, Term, date]) -> str:
            if isinstance(a, Term): return str(a)
            if isinstance(a, date): return a.isoformat()
            return str(a)
        return f"{self.pred}(" + ", ".join(fmt(a) for a in self.args) + ")"

@dataclass(frozen=True)
class And:
    left: Union[Atom, 'And']
    right: Union[Atom, 'And']
    def pretty(self) -> str:
        return f"({self.left.pretty()} ∧ {self.right.pretty()})"

@dataclass(frozen=True)
class ForAllInInterval:
    var: Term
    interval: DateInterval
    body: Atom
    def pretty(self) -> str:
        return f"∀{self.var}∈{interval_to_str(self.interval)} {self.body.pretty()}"

# -----------------------------------------------------------------------------
# Proof kernel: steps and proof object (pretty output only)
# -----------------------------------------------------------------------------

@dataclass
class Conclusion:
    kind: str
    payload: Any

    def pretty(self) -> str:
        k = self.kind
        p = self.payload
        if k == "formula":
            return p.pretty()
        if k == "interval-eq":
            (left_a, left_b, result) = p
            return f"{interval_to_str(left_a)} ∩ {interval_to_str(left_b)} = {interval_to_str(result)}"
        if k == "membership":
            (val, interval) = p
            v = val if isinstance(val, str) else (val.isoformat() if isinstance(val, date) else str(val))
            return f"{v} ∈ {interval_to_str(interval)}"
        if k == "classification":
            return f"Global activation state = {p}"
        if k == "interval-def":
            (name, iv) = p
            return f"{name} := {interval_to_str(iv)}"
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
            prem = f" [{', '.join(map(str,s.premises))}]" if s.premises else ""
            note = f"  // {s.notes}" if s.notes else ""
            lines.append(f"[{s.id}] {s.rule}{prem}: {s.conclusion.pretty()}{note}")
        return "\n".join(lines)

# -----------------------------------------------------------------------------
# Testcase-9 policy encoding
# -----------------------------------------------------------------------------

# Permission window: [2025-01-01, 2025-12-31]
PERM_IV = interval_intersection(
    interval_from_op("gteq", date.fromisoformat("2025-01-01")),
    interval_from_op("lteq", date.fromisoformat("2025-12-31")),
)
assert PERM_IV is not None

# Prohibition window: (-∞, 2026-01-01)
PROH_IV = interval_from_op("lt", date.fromisoformat("2026-01-01"))

# Universal premises
d = Term("d")
perm_body = Atom("Perm", ("odrl:read", "ex:resourceX", d))
proh_body = Atom("Proh", ("odrl:read", "ex:resourceX", d))
PERM_PREMISE = ForAllInInterval(d, PERM_IV, perm_body)
PROH_PREMISE = ForAllInInterval(d, PROH_IV, proh_body)

# -----------------------------------------------------------------------------
# Classification helper
# -----------------------------------------------------------------------------

REPORT_URIS = {
    "Conflict":   "https://w3id.org/force/compliance-report#Conflict",
    "Ambiguous":  "https://w3id.org/force/compliance-report#Ambiguous",
    "Prohibited": "https://w3id.org/force/compliance-report#Prohibited",
    "Permitted":  "https://w3id.org/force/compliance-report#Permitted",
    "NoConflict": "https://w3id.org/force/compliance-report#NoConflict",
}

def classify_from_intervals(perm_iv: DateInterval, proh_iv: DateInterval) -> Tuple[str, Optional[DateInterval]]:
    inter = interval_intersection(perm_iv, proh_iv)
    if inter is None:
        return (REPORT_URIS["NoConflict"], None)
    return (REPORT_URIS["Conflict"], inter)

# -----------------------------------------------------------------------------
# Build the explicit proof
# -----------------------------------------------------------------------------

def build_proof_for_testcase9() -> Tuple[Proof, str]:
    proof = Proof()

    # [1] Premise: ∀d ∈ PERM_IV Perm(...)
    s1 = proof.add("Premise", [], Conclusion("formula", PERM_PREMISE),
                   notes="Permission premise over dates")

    # [2] Premise: ∀d ∈ PROH_IV Proh(...)
    s2 = proof.add("Premise", [], Conclusion("formula", PROH_PREMISE),
                   notes="Prohibition premise over dates")

    # [3] Interval-Intersection: PERM_IV ∩ PROH_IV = INTER
    inter = interval_intersection(PERM_IV, PROH_IV)
    assert inter is not None
    s3 = proof.add("Interval-Intersection", [], Conclusion("interval-eq", (PERM_IV, PROH_IV, inter)),
                   notes="Compute overlap of activation windows")

    # [4] Define I := INTER
    s4 = proof.add("Definition", [s3], Conclusion("interval-def", ("I", inter)),
                   notes="Name the overlap interval as I")

    # [5] Choose: pick d0 ∈ I  (witness)
    d0 = pick_witness(inter)
    s5 = proof.add("Choose", [s4], Conclusion("membership", (d0, inter)),
                   notes=f"Witness date d0 = {d0.isoformat()}")

    # [6] Universal-Elim on [1] with d0: Perm(..., d0)
    inst_perm = Atom("Perm", ("odrl:read", "ex:resourceX", d0))
    s6 = proof.add("Universal-Elim", [s1, s5], Conclusion("formula", inst_perm),
                   notes="Instantiate permission at d0")

    # [7] Universal-Elim on [2] with d0: Proh(..., d0)
    inst_proh = Atom("Proh", ("odrl:read", "ex:resourceX", d0))
    s7 = proof.add("Universal-Elim", [s2, s5], Conclusion("formula", inst_proh),
                   notes="Instantiate prohibition at d0")

    # [8] And-Intro: Perm(..., d0) ∧ Proh(..., d0)
    conj = And(inst_perm, inst_proh)
    s8 = proof.add("And-Intro", [s6, s7], Conclusion("formula", conj),
                   notes="Clash at the witness date")

    # [9] ForAll-Intro over d ∈ I: ∀d ∈ I (Perm ∧ Proh)
    general = ForAllInInterval(d, inter, And(Atom("Perm", ("odrl:read","ex:resourceX", d)),
                                             Atom("Proh", ("odrl:read","ex:resourceX", d))))
    s9 = proof.add("ForAll-Intro", [s8], Conclusion("formula", general),
                   notes="Generalize the clash over all dates in the overlap I")

    # [10] Classification: Conflict
    result_uri, _ = classify_from_intervals(PERM_IV, PROH_IV)
    proof.add("Classification", [s9], Conclusion("classification", result_uri),
              notes="Overlap equals permission window ⇒ standing clash")

    return proof, result_uri

# -----------------------------------------------------------------------------
# Main
# -----------------------------------------------------------------------------

def main() -> None:
    proof, result_uri = build_proof_for_testcase9()

    # 1) Always print expected URI first
    print(result_uri)

    # 2) Pretty proof only (no JSON)
    print("\n=== Pretty Proof ===\n")
    print(proof.pretty())

if __name__ == "__main__":
    main()

