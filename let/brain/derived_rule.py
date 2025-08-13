#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Robust backward-chainer with rule generation and variable bindings — ARC-ified

What this does:
---------------
1) Start with ground facts like
       (:Minka a :Cat)
       (:Charly a :Dog)
2) A *meta rule* says:
       if (?x a :Cat) then generate a *rule*
           Rgen-from-cat:
               if (?y a :Dog) then (:test :is true)
   In words: the existence of (some) cat enables a rule that any dog implies :test is true.
3) We *generate* ordinary rules from meta rules by unifying meta-premises with facts.
4) We run a *backward chainer* to prove the query (:test :is true), recording a trace.

ARC output:
-----------
- Answer:   lists the generated rules and the first successful proof’s substitution(s)
- Reason why: explains the meta-rule → rule instantiation and prints the proof trace
- Check (harness): re-derives rules, checks unification & variable bindings, and
                   re-proves the query to ensure consistency

Notes:
------
- Variables are strings beginning with '?' (e.g., "?x", "?y").
- Unification is tuple-wise and only binds variables (no occurs-check needed for these flat triples).
- The backward prover returns the *first* solution (like your original), using a simple loop-guard.
"""

from __future__ import annotations
from dataclasses import dataclass
from typing import Dict, Iterable, Iterator, List, Optional, Set, Tuple, FrozenSet

# ─────────────────────────────────────────────────────────────
# Facts
# ─────────────────────────────────────────────────────────────

facts: Set[Tuple[str, str, str]] = {
    (":Minka",  "a", ":Cat"),
    (":Charly", "a", ":Dog"),
}

# ─────────────────────────────────────────────────────────────
# Meta rules (unchanged in intent)
# ─────────────────────────────────────────────────────────────

meta_rules = [
    {
        "id": "R1-cat-makes-rule",
        "premises": [
            ("?x", "a", ":Cat"),
        ],
        "gen_rule": {
            "id": "Rgen-from-cat",
            "premises": [
                ("?y", "a", ":Dog"),
            ],
            "conclusions": [
                (":test", ":is", "true"),
            ],
        },
    }
]

# Ordinary rules (start empty; will be generated from meta_rules)
rules: List[Dict] = []

# ─────────────────────────────────────────────────────────────
# Unification and substitution
# ─────────────────────────────────────────────────────────────

def is_var(t) -> bool:
    return isinstance(t, str) and t.startswith("?")

def unify(pat: Tuple[str, str, str], fact: Tuple[str, str, str], θ: Dict[str, str]) -> Optional[Dict[str, str]]:
    """
    Unify a triple pattern with a ground triple fact.
    Only variables on the pattern side may bind.
    """
    θ = dict(θ)
    for p, f in zip(pat, fact):
        if is_var(p):
            if p in θ and θ[p] != f:
                return None
            θ[p] = f
        elif p != f:
            return None
    return θ

def subst(term, θ: Dict[str, str]):
    """Apply a substitution to a triple (or atom); tuples recursively."""
    if isinstance(term, tuple):
        return tuple(subst(x, θ) for x in term)
    return θ.get(term, term)

# ─────────────────────────────────────────────────────────────
# Rule generation from meta rules
# ─────────────────────────────────────────────────────────────

def derive_rules(facts: Set[Tuple[str,str,str]], meta_rules: List[Dict]) -> List[Dict]:
    """
    For each meta rule:
      - find all θ that satisfy every meta-premise against the facts
      - instantiate the embedded gen_rule with θ
      - add origin and avoid duplicates
    """
    out: List[Dict] = []
    for mr in meta_rules:
        # build all consistent θ that prove every meta-premise
        combos: List[Dict[str,str]] = [ {} ]
        for prem in mr["premises"]:
            next_combos: List[Dict[str,str]] = []
            for f in facts:
                θ = unify(prem, f, {})
                if θ is None:
                    continue
                for c in combos:
                    # merge if consistent
                    ok = True
                    merged = dict(c)
                    for k, v in θ.items():
                        if k in merged and merged[k] != v:
                            ok = False
                            break
                        merged[k] = v
                    if ok:
                        next_combos.append(merged)
            combos = next_combos

        # instantiate gen_rule with each θ
        for θ in combos:
            gr = mr["gen_rule"]
            new_rule = {
                "id": gr["id"],
                "premises": [subst(p, θ) for p in gr["premises"]],
                "conclusions": [subst(c, θ) for c in gr["conclusions"]],
                "origin": mr["id"],
            }
            if new_rule not in out:
                out.append(new_rule)
    return out

# Generate ordinary rules once (like your top-level code)
rules = derive_rules(facts, meta_rules)

# ─────────────────────────────────────────────────────────────
# Backward prover (first solution; with a simple loop guard)
# ─────────────────────────────────────────────────────────────

def indent(d: int) -> str:
    return "  " * d

@dataclass
class Proof:
    goal: Tuple[str,str,str]
    success: bool
    theta: Dict[str,str]
    trace: List[str]
    used_rule_ids: List[str]

def bc(goal: Tuple[str,str,str],
       θ: Dict[str,str],
       depth: int,
       seen: FrozenSet[Tuple[str, Tuple[str,str,str]]],
       trace: List[str],
       used_rules: List[str]) -> Iterator[Dict[str,str]]:
    """
    Prove a single triple `goal` under substitution θ.
    Try facts first, then rules (left-to-right). Yield the FIRST solution upward.
    Loop guard `seen` tracks (rule_id, instantiated_head) pairs.
    """
    goal_inst = subst(goal, θ)
    trace.append(f"{indent(depth)}prove {goal_inst}")

    # 1) Try matching facts
    for f in sorted(facts):
        θ2 = unify(goal_inst, f, {})
        if θ2 is not None:
            θ_total = {**θ, **θ2}
            trace.append(f"{indent(depth)}✓ fact {f}")
            yield θ_total
            return

    # 2) Try rules
    for r in rules:
        for head in r["conclusions"]:
            θ_head = unify(head, goal_inst, {})
            if θ_head is None:
                continue
            key = (r["id"], subst(head, θ_head))
            if key in seen:
                continue
            trace.append(f"{indent(depth)}→ via {r['id']}")
            θ_new = {**θ, **θ_head}

            # Prove premises sequentially
            def prove_seq(i: int, θ_curr: Dict[str,str]) -> Iterator[Dict[str,str]]:
                if i == len(r["premises"]):
                    used_rules.append(r["id"])
                    yield θ_curr
                    return
                prem = r["premises"][i]
                for θ_next in bc(prem, θ_curr, depth+1, seen | frozenset({key}), trace, used_rules):
                    yield from prove_seq(i+1, θ_next)

            for θ_fin in prove_seq(0, θ_new):
                yield θ_fin
                return

def prove(query: Tuple[str,str,str]) -> Proof:
    trace: List[str] = []
    used: List[str] = []
    for θ in bc(query, {}, 0, frozenset(), trace, used):
        return Proof(goal=query, success=True, theta=θ, trace=trace, used_rule_ids=used)
    return Proof(goal=query, success=False, theta={}, trace=trace, used_rule_ids=[])

# ─────────────────────────────────────────────────────────────
# ARC output sections
# ─────────────────────────────────────────────────────────────

def print_answer(query: Tuple[str,str,str], pr: Proof) -> None:
    print("Answer")
    print("------")
    print("Generated ordinary rules (from meta rules):")
    if rules:
        for i, r in enumerate(rules, 1):
            print(f"  {i}. {r['id']} (origin: {r['origin']})")
            print(f"     premises   : {r['premises']}")
            print(f"     conclusions: {r['conclusions']}")
    else:
        print("  (none)")
    print()

    print(f"Query: {query}")
    if pr.success:
        print("Result: PROVED")
        if pr.theta:
            print(f"Substitution (bindings): {pr.theta}")
        else:
            print("(no variable bindings)")
        if pr.used_rule_ids:
            print(f"Rules used: {pr.used_rule_ids}")
    else:
        print("Result: NOT PROVED")
    print()

def print_reason(pr: Proof, max_lines: int = 40) -> None:
    print("Reason why")
    print("----------")
    print("Meta → rule:")
    print("  R1-cat-makes-rule: if (?x a :Cat) then generate Rgen-from-cat:")
    print("    Rgen-from-cat: premises [(?y a :Dog)] ⇒ conclusions [(:test :is true)].")
    print("  With (:Minka a :Cat) in facts, this rule is admitted.")
    print()
    print("Backward proof (first lines):")
    for line in pr.trace[:max_lines]:
        print("  " + line)
    if len(pr.trace) > max_lines:
        print(f"  … {len(pr.trace) - max_lines} more steps …")
    print()

# ─────────────────────────────────────────────────────────────
# Check (harness)
# ─────────────────────────────────────────────────────────────

def check_harness(query: Tuple[str,str,str], pr: Proof) -> None:
    """
    Re-verify:
      1) Rule generation: meta-premises unify with facts and produce exactly Rgen-from-cat,
         whose head matches the query and whose premise is (?y a :Dog).
      2) The proof succeeds and binds ?y to a subject that is in fact a :Dog.
      3) Re-running the prover yields a (the same) success.
    """
    # 1) Rule generation sanity
    assert any(mr["id"] == "R1-cat-makes-rule" for mr in meta_rules)
    # meta-premise unifies with (:Minka a :Cat)
    θ_cat = unify(("?x","a",":Cat"), tuple(next(iter([f for f in facts if f[2]==":Cat"]))), {})
    assert θ_cat is not None and θ_cat["?x"] == ":Minka"
    # generated rule present
    rgen = [r for r in rules if r["id"] == "Rgen-from-cat"]
    assert len(rgen) == 1, "Expected exactly one Rgen-from-cat."
    r = rgen[0]
    assert r["conclusions"] == [(":test",":is","true")]
    assert r["premises"] == [("?y","a",":Dog")]

    # 2) Proof succeeded and ?y is a dog
    assert pr.success, "Prover did not succeed."
    y = pr.theta.get("?y")
    assert y is not None, "Expected a binding for ?y."
    assert (y, "a", ":Dog") in facts, f"{y} is not known to be a :Dog."

    # 3) Re-run
    pr2 = prove(query)
    assert pr2.success, "Second run failed unexpectedly."
    assert pr2.theta.get("?y") == y, "Different binding across runs."

# ─────────────────────────────────────────────────────────────
# Main
# ─────────────────────────────────────────────────────────────

def main():
    query = (":test", ":is", "true")
    pr = prove(query)

    # ----- ARC output -----
    print_answer(query, pr)
    print_reason(pr, max_lines=40)

    print("Check (harness)")
    print("---------------")
    try:
        check_harness(query, pr)
        print("OK: rule generation, proof steps, and bindings all verified.")
    except AssertionError as e:
        print("FAILED:", e)
        raise

if __name__ == "__main__":
    main()

