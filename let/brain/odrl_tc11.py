#!/usr/bin/env python3
"""
Permission with refinement. Obligation with conflicting refinement
==================================================================

This script encodes the two ODRL policies:

- policy11a: Permission to pay with refinement payAmount > 10 EUR
- policy11b: Obligation to pay with refinement payAmount < 10 EUR

Deontic reading used (goal-oriented proof leverages this axiom schema):
  O( pay < T )  ⇒  F( pay > T )
where O = Obligation, F = Prohibition.

Output (on conflict):
  https://w3id.org/force/compliance-report#Conflict

It also prints a goal-oriented proof explanation.
"""

from decimal import Decimal, ROUND_HALF_UP
from dataclasses import dataclass
from typing import Optional, Tuple
import argparse

# Interval abstraction: (lower, lower_closed, upper, upper_closed), None for ±∞
Interval = Tuple[Optional[Decimal], bool, Optional[Decimal], bool]

def interval_from_op(op: str, value: Decimal) -> Interval:
    if op == "lt":
        return (None, False, value, False)    # (-inf, value)
    if op == "le":
        return (None, False, value, True)     # (-inf, value]
    if op == "gt":
        return (value, False, None, False)    # (value, +inf)
    if op == "ge":
        return (value, True, None, False)     # [value, +inf)
    if op == "eq":
        return (value, True, value, True)     # [value, value]
    raise ValueError(f"Unsupported operator: {op}")

def interval_intersection(a: Interval, b: Interval) -> Optional[Interval]:
    (al, alc, au, auc) = a
    (bl, blc, bu, buc) = b

    # New lower
    if al is None:
        nl, nlc = bl, blc
    elif bl is None:
        nl, nlc = al, alc
    else:
        if al > bl:
            nl, nlc = al, alc
        elif bl > al:
            nl, nlc = bl, blc
        else:  # equal
            nl, nlc = al, alc and blc

    # New upper
    if au is None:
        nu, nuc = bu, buc
    elif bu is None:
        nu, nuc = au, auc
    else:
        if au < bu:
            nu, nuc = au, auc
        elif bu < au:
            nu, nuc = bu, buc
        else:  # equal
            nu, nuc = au, auc and buc

    # Empty?
    if nl is not None and nu is not None:
        if nl > nu:
            return None
        if nl == nu and not (nlc and nuc):
            return None

    return (nl, nlc, nu, nuc)

def pick_point(interval: Interval, decimals: int = 2) -> Decimal:
    """
    Deterministically pick a witness inside a non-empty interval.
    We assume currency-like values and use a 0.01 step.
    """
    (l, lc, u, uc) = interval
    step = Decimal("1").scaleb(-decimals)  # 0.01 for 2 decimals

    def quant(x: Decimal) -> Decimal:
        q = Decimal("1").scaleb(-decimals)
        return x.quantize(q, rounding=ROUND_HALF_UP)

    if l is None and u is None:
        return Decimal("0.00")
    if l is None:
        # choose u - step (or u exactly if closed)
        if uc:
            return quant(u)
        else:
            return quant(u - step)
    if u is None:
        # choose l + step (or l exactly if closed)
        if lc:
            return quant(l)
        else:
            return quant(l + step)
    # both bounds finite
    if l == u:
        # interval must be closed (we checked before), choose the bound
        return quant(l)
    # open/closed handling
    candidate = l if lc else (l + step)
    # ensure candidate < u (or <= if uc)
    if candidate < u or (candidate == u and uc):
        return quant(candidate)
    # fallback: try just below upper
    candidate = u if uc else (u - step)
    return quant(candidate)

@dataclass(frozen=True)
class Refinement:
    left_operand: str        # e.g., "odrl:payAmmount"
    operator: str            # "lt", "gt", ...
    value: Decimal
    unit: str                # e.g., "dbpedia:Euro"

    def as_interval(self) -> Interval:
        return interval_from_op(self.operator, self.value)

@dataclass(frozen=True)
class Rule:
    kind: str                # "permission", "obligation", "prohibition"
    assignee: str            # e.g., "ex:alice"
    action: str              # e.g., "odrl:pay"
    refinement: Refinement
    target: str              # e.g., "ex:resourceX"

def implied_prohibition_from_obligation(ob: Rule) -> Rule:
    """
    Deontic mapping used:
      O(pay < T)  ⇒ F(pay > T)
      O(pay ≤ T)  ⇒ F(pay > T)
      O(pay > T)  ⇒ F(pay < T)
      O(pay ≥ T)  ⇒ F(pay < T)
      O(pay = T)  ⇒ F(pay ≠ T) — approximated here via F(pay > T) for direct clash
    """
    op = ob.refinement.operator
    val = ob.refinement.value
    if op in ("lt", "le"):
        p_op = "gt"; p_val = val
    elif op in ("gt", "ge"):
        p_op = "lt"; p_val = val
    elif op == "eq":
        p_op = "gt"; p_val = val
    else:
        raise ValueError(f"Unsupported obligation operator: {op}")

    prohib_ref = Refinement(
        left_operand=ob.refinement.left_operand,
        operator=p_op,
        value=p_val,
        unit=ob.refinement.unit,
    )
    return Rule(
        kind="prohibition",
        assignee=ob.assignee,
        action=ob.action,
        refinement=prohib_ref,
        target=ob.target,
    )

@dataclass
class ConflictResult:
    conflict: bool
    witness_amount: Optional[Decimal] = None
    explanation: Optional[str] = None

def rules_conflict_by_amount(permission: Rule, prohibition: Rule) -> ConflictResult:
    """
    Conflict criterion:
      ∃ amount a  such that  Perm(a) ∧ Prohib(a)
    (for same assignee, action, target, leftOperand, unit)
    """
    if not (
        permission.kind == "permission" and
        prohibition.kind == "prohibition" and
        permission.assignee == prohibition.assignee and
        permission.action == prohibition.action and
        permission.target == prohibition.target and
        permission.refinement.left_operand == prohibition.refinement.left_operand and
        permission.refinement.unit == prohibition.refinement.unit
    ):
        return ConflictResult(False)

    inter = interval_intersection(
        permission.refinement.as_interval(),
        prohibition.refinement.as_interval()
    )
    if inter is None:
        return ConflictResult(False)

    witness = pick_point(inter)
    return ConflictResult(True, witness_amount=witness)

def build_goal_oriented_proof(permission: Rule, obligation: Rule, witness: Decimal) -> str:
    """
    Construct a concise goal-oriented proof explanation.

    Goal G:
      Show Conflict, i.e., ∃a Perm(a) ∧ Prohib(a)

    Strategy:
      1) From the obligation derive a prohibition (deontic axiom).
      2) Instantiate a concrete witness amount 'a' within the overlapping region.
      3) Show both Perm(a) and Prohib(a) hold for the same agent/action/target.
    """
    T = obligation.refinement.value
    currency = obligation.refinement.unit.split(":")[-1]  # "Euro"
    a = f"{witness:.2f}"

    steps = []
    steps.append("Goal G: ∃a  (Perm(pay,a) ∧ Prohib(pay,a))  for the same subject, action, and target.")
    steps.append("")
    steps.append("Policy facts:")
    steps.append(f"  (P1) Permission: pay > {T} {currency}  (policy11a).")
    steps.append(f"  (P2) Obligation: pay < {T} {currency}  (policy11b).")
    steps.append("")
    steps.append("Deontic axiom (used as a rule):")
    steps.append(f"  (D) From O(pay < {T} {currency}) infer F(pay > {T} {currency}).")
    steps.append("")
    steps.append("Subgoals and derivation:")
    steps.append(f"  (S1) From (P2) and (D) derive Prohib(pay > {T} {currency}).")
    steps.append(f"  (S2) Choose witness amount a := {a} {currency} with a > {T} (witness is in the overlap).")
    steps.append(f"  (S3) From (P1) and a > {T}, derive Perm(pay,a).")
    steps.append(f"  (S4) From (S1) and a > {T}, derive Prohib(pay,a).")
    steps.append("  (C) From (S3) and (S4), Perm(pay,a) ∧ Prohib(pay,a). Therefore ∃a … holds.")
    steps.append("Conclusion: Goal G is satisfied ⇒ Conflict.")
    return "\n".join(steps)

def make_policies():
    # ex:policy11a — Permission to pay > 10.00 EUR
    policy11a = Rule(
        kind="permission",
        assignee="ex:alice",
        action="odrl:pay",
        refinement=Refinement(
            left_operand="odrl:payAmmount",  # preserving the spelling from the snippet
            operator="gt",
            value=Decimal("10.00"),
            unit="dbpedia:Euro",
        ),
        target="ex:resourceX",
    )
    # ex:policy11b — Obligation to pay < 10.00 EUR
    policy11b = Rule(
        kind="obligation",
        assignee="ex:alice",
        action="odrl:pay",
        refinement=Refinement(
            left_operand="odrl:payAmmount",
            operator="lt",
            value=Decimal("10.00"),
            unit="dbpedia:Euro",
        ),
        target="ex:resourceX",
    )
    return policy11a, policy11b

def main() -> None:
    parser = argparse.ArgumentParser(description="Detect conflict and show a goal-oriented proof.")

    permission, obligation = make_policies()
    implied_prohib = implied_prohibition_from_obligation(obligation)

    result = rules_conflict_by_amount(permission, implied_prohib)

    if result.conflict:
        print("https://w3id.org/force/compliance-report#Conflict")
        print("\n--- Goal-Oriented Proof Explanation ---\n")
        proof = build_goal_oriented_proof(permission, obligation, result.witness_amount)
        print(proof)
    else:
        print("https://w3id.org/force/compliance-report#NoConflict")
        print("\n(No conflict found; no proof to display.)")

if __name__ == "__main__":
    main()

