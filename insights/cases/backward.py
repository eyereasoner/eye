#!/usr/bin/env python3
"""
backward.py
───────────────────────────────────────────────────────────────
Description
-----------
Python rewrite of the N3 backward-rule example
(from https://www.w3.org/2000/10/swap/doc/tutorial-1.pdf, p.17).

- Triples are (S, P, O)
- Variables are strings starting with "?X"
- Rules are (rule_id, HEAD, [BODY_ATOMS...])
- Facts are triples
- Built-in supported: math:greaterThan

Implements the rule:
  { ?X :moreInterestingThan ?Y } <= { ?X math:greaterThan ?Y } .

We run two queries:
  1) (5, ":moreInterestingThan", 3)   — succeeds
  2) (3, ":moreInterestingThan", 5)   — fails (with explanation)

The program prints ARC sections per query:
  • Answer — YES/NO plus the grounded instance (if any)
  • Reason why — a readable proof tree or a failure explanation tree
  • Check (harness) — sanity tests for unification, built-ins, and proof soundness
"""

from itertools import count
from dataclasses import dataclass, field
from typing import Any, Dict, Iterable, List, Optional, Tuple

Triple = Tuple[Any, str, Any]
Subst = Dict[str, Any]

# -----------------------------------------------------------------------------
# Knowledge base (extend freely)
# -----------------------------------------------------------------------------

RULES: List[Tuple[str, Triple, List[Triple]]] = [
    (
        "R-moreInteresting",
        ("?X", ":moreInterestingThan", "?Y"),
        [("?X", "math:greaterThan", "?Y")],
    ),
]

FACTS: List[Triple] = [
    # Add ground facts here if you want, e.g.:
    # ("alice", ":likes", "chess"),
]

# -----------------------------------------------------------------------------
# Utilities: variables, substitution, unification
# -----------------------------------------------------------------------------

def is_var(t: Any) -> bool:
    return isinstance(t, str) and t.startswith("?")

def walk(t: Any, theta: Subst) -> Any:
    """Follow substitutions to a representative."""
    while is_var(t) and t in theta:
        t = theta[t]
    return t

def subst_term(t: Any, theta: Subst) -> Any:
    return walk(t, theta)

def subst_triple(tr: Triple, theta: Subst) -> Triple:
    s, p, o = tr
    return (subst_term(s, theta), p, subst_term(o, theta))

def unify(a: Any, b: Any, theta: Subst) -> Optional[Subst]:
    """Unify two terms (not triples)."""
    a, b = walk(a, theta), walk(b, theta)
    if a == b:
        return theta
    if is_var(a):
        theta2 = dict(theta); theta2[a] = b
        return theta2
    if is_var(b):
        theta2 = dict(theta); theta2[b] = a
        return theta2
    return None  # both non-vars and unequal

def unify_triples(pat: Triple, fact: Triple, theta: Subst) -> Optional[Subst]:
    """Unify two triples positionally (predicates must match exactly)."""
    s1, p1, o1 = pat
    s2, p2, o2 = fact
    if p1 != p2:
        return None
    theta = unify(s1, s2, theta)
    if theta is None:
        return None
    theta = unify(o1, o2, theta)
    return theta

# -----------------------------------------------------------------------------
# Standardize-apart for rules (prevents variable capture across applications)
# -----------------------------------------------------------------------------

_fresh = count(1)

def std_apart_rule(rule: Tuple[str, Triple, List[Triple]]) -> Tuple[str, Triple, List[Triple]]:
    rid, head, body = rule
    n = next(_fresh)
    mapping: Dict[str, str] = {}

    def rn(t: Any) -> Any:
        if is_var(t):
            if t not in mapping:
                mapping[t] = f"{t}_{n}"
            return mapping[t]
        return t

    def rn_triple(tr: Triple) -> Triple:
        s, p, o = tr
        return (rn(s), p, rn(o))

    return rid, rn_triple(head), [rn_triple(b) for b in body]

# -----------------------------------------------------------------------------
# Built-ins
# -----------------------------------------------------------------------------

def is_builtin(pred: str) -> bool:
    return pred == "math:greaterThan"

def eval_builtin(goal: Triple) -> Tuple[bool, str]:
    s, p, o = goal
    if p == "math:greaterThan":
        if is_var(s) or is_var(o):
            return False, "not ground"
        try:
            ok = s > o
        except Exception as e:
            return False, f"error: {e}"
        return ok, f"{s} > {o}"
    return False, f"unknown builtin {p}"

# -----------------------------------------------------------------------------
# Proof objects
# -----------------------------------------------------------------------------

@dataclass
class ProofNode:
    goal: Triple
    kind: str                        # "fact", "builtin", or "rule"
    detail: Optional[str] = None     # fact tuple for "fact"; rule id for "rule"; explanation for "builtin"
    children: List["ProofNode"] = field(default_factory=list)

def pp_proof(node: ProofNode, indent: int = 0) -> None:
    pad = "  " * indent
    if node.kind == "rule":
        print(f"{pad}Goal: {node.goal}  via {node.detail}")
        for ch in node.children:
            pp_proof(ch, indent + 1)
        print(f"{pad}✓ Proven: {node.goal}")
    elif node.kind == "fact":
        print(f"{pad}✓ fact {node.detail}")
    elif node.kind == "builtin":
        print(f"{pad}✓ builtin {node.goal} ({node.detail})")
    else:
        print(f"{pad}? {node.goal}")

# -----------------------------------------------------------------------------
# Failure objects
# -----------------------------------------------------------------------------

@dataclass
class Failure:
    goal: Triple
    reason: str
    children: List["Failure"] = field(default_factory=list)

def pp_failure(node: Failure, indent: int = 0) -> None:
    pad = "  " * indent
    print(f"{pad}✗ {node.goal}  — {node.reason}")
    for ch in node.children:
        pp_failure(ch, indent + 1)

# -----------------------------------------------------------------------------
# Backward-chaining prover (success)
# -----------------------------------------------------------------------------

def prove(goal: Triple, theta: Subst) -> Iterable[Tuple[Subst, ProofNode]]:
    """
    Prove `goal` under substitution `theta`.
    Yields (extended_subst, proof_node) for each successful derivation.
    """
    g = subst_triple(goal, theta)
    s, p, o = g

    # 1) Built-ins
    if is_builtin(p):
        ok, reason = eval_builtin(g)
        if ok:
            yield theta, ProofNode(goal=g, kind="builtin", detail=reason)
        return

    # 2) Facts
    for fact in FACTS:
        if fact[1] != p:
            continue
        theta2 = unify_triples(g, fact, dict(theta))
        if theta2 is not None:
            yield theta2, ProofNode(goal=g, kind="fact", detail=str(fact))

    # 3) Rules
    for rule in RULES:
        rid, head, body = std_apart_rule(rule)
        if head[1] != p:
            continue
        theta_head = unify_triples(g, head, dict(theta))
        if theta_head is None:
            continue

        # Prove the body in sequence with backtracking
        def prove_body(goals: List[Triple], th: Subst) -> Iterable[Tuple[Subst, List[ProofNode]]]:
            if not goals:
                yield th, []
                return
            first, rest = goals[0], goals[1:]
            for th1, proof_first in prove(first, th):
                for th2, proof_rest in prove_body(rest, th1):
                    yield th2, [proof_first] + proof_rest

        for th_final, children in prove_body(body, theta_head):
            yield th_final, ProofNode(goal=g, kind="rule", detail=rid, children=children)

# -----------------------------------------------------------------------------
# Diagnosis (why a goal is NOT provable)
# -----------------------------------------------------------------------------

def diagnose(goal: Triple, theta: Subst) -> Failure:
    """
    Return a Failure tree explaining why `goal` cannot be proven from the KB.
    If `goal` is actually provable, this function isn't called (caller checks first).
    """
    g = subst_triple(goal, theta)
    s, p, o = g

    # Built-in predicate
    if is_builtin(p):
        ok, reason = eval_builtin(g)
        if not ok:
            return Failure(goal=g, reason=f"builtin {p} failed: {reason}")
        # (shouldn't happen: caller only calls on failure)
        return Failure(goal=g, reason=f"unexpected: builtin {p} succeeded")

    # Facts quick check
    for fact in FACTS:
        if fact[1] != p:
            continue
        if unify_triples(g, fact, dict(theta)) is not None:
            return Failure(goal=g, reason="unexpected: matched a fact")

    # Gather applicable rules by predicate
    rules_p = [r for r in RULES if r[1][1] == p]
    if not rules_p:
        return Failure(goal=g, reason=f"no facts and no rules with head predicate {p}")

    # Examine each rule
    rule_failures: List[Failure] = []
    any_head_unified = False

    for rule in rules_p:
        rid, head, body = std_apart_rule(rule)
        theta_head = unify_triples(g, head, dict(theta))
        if theta_head is None:
            rule_failures.append(Failure(goal=g, reason=f"rule {rid} not applicable (head doesn't unify)"))
            continue
        any_head_unified = True

        # Try to satisfy body sequentially; report the first failing subgoal
        th_curr = theta_head
        body_ok = True
        body_fail_children: List[Failure] = []
        for sub in body:
            found = False
            for th_next, _proof in prove(sub, th_curr):
                th_curr = th_next
                found = True
                break
            if not found:
                body_ok = False
                body_fail_children.append(diagnose(sub, th_curr))
                rule_failures.append(Failure(goal=g, reason=f"rule {rid} body not provable", children=body_fail_children))
                break

        if body_ok:
            return Failure(goal=g, reason=f"unexpected: rule {rid} would actually prove the goal")

    if not any_head_unified:
        return Failure(goal=g, reason="no applicable rules unify with the goal's arguments")

    return Failure(goal=g, reason="all applicable rules' bodies failed", children=rule_failures)

# -----------------------------------------------------------------------------
# Proof verification (for the harness)
# -----------------------------------------------------------------------------

def verify_proof(node: ProofNode) -> bool:
    """Check that each leaf is either a known fact or a true built-in."""
    if node.kind == "builtin":
        ok, _ = eval_builtin(node.goal)
        return ok
    if node.kind == "fact":
        return any(unify_triples(node.goal, f, {}) is not None for f in FACTS)
    if node.kind == "rule":
        return all(verify_proof(ch) for ch in node.children)
    return False

# -----------------------------------------------------------------------------
# ARC sections per query
# -----------------------------------------------------------------------------

def arc_answer(goal: Triple, answers: List[Tuple[Subst, ProofNode]]) -> None:
    print("Answer")
    print("------")
    if answers:
        theta, _ = answers[0]
        inst = subst_triple(goal, theta)
        print(f"YES — instance: {inst}")
    else:
        print("NO — not derivable from the rule base.")
    print()

def arc_reason(goal: Triple, answers: List[Tuple[Subst, ProofNode]]) -> None:
    print("Reason why")
    print("----------")
    if answers:
        print("Proof tree:")
        pp_proof(answers[0][1])
    else:
        print("Failure explanation:")
        pp_failure(diagnose(goal, {}))
    print()

def arc_check(goal: Triple, answers: List[Tuple[Subst, ProofNode]]) -> None:
    print("Check (harness)")
    print("---------------")
    if answers:
        # Verify the proof tree we printed
        ok = verify_proof(answers[0][1])
        assert ok, "Proof tree failed verification."
        # Re-run: there must be at least one successful derivation
        assert any(True for _ in prove(goal, {})), "Second run found no proofs."
    else:
        # Ensure truly not provable
        assert not any(True for _ in prove(goal, {})), "Unexpected proof found on re-run."
        # If it's a builtin-based rule, the builtin should actually fail
        s, p, o = goal
        if p == ":moreInterestingThan":
            # Under our single rule, failure ⇔ !(s > o)
            if not (is_var(s) or is_var(o)):
                ok, reason = eval_builtin((s, "math:greaterThan", o))
                assert not ok, "Builtin unexpectedly true in failing case."
    print("OK.")
    print()

# -----------------------------------------------------------------------------
# Run both demo queries with ARC sections
# -----------------------------------------------------------------------------

def run_query(goal: Triple, show_all: bool = False) -> None:
    answers = list(prove(goal, {}))
    arc_answer(goal, answers)
    arc_reason(goal, answers)
    arc_check(goal, answers)

# -----------------------------------------------------------------------------
# Demo
# -----------------------------------------------------------------------------

if __name__ == "__main__":
    # Succeeds via rule + builtin
    print("=== Query 1 === (should succeed)")
    run_query((5, ":moreInterestingThan", 3))

    # Fails; you'll get an explanation
    print("=== Query 2 === (should fail)")
    run_query((3, ":moreInterestingThan", 5))

