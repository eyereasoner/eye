#!/usr/bin/env python3
"""
Slide 33 (Pat Hayes talk) — OWL allValuesFrom restriction as meta-rule
---------------------------------------------------------------------

Input (mirrored):

@prefix owl: <http://www.w3.org/2002/07/owl#>.
@prefix :    <http://example.org/#>.

# OWL restriction
:aaa owl:onProperty :bbb.
:aaa owl:allValuesFrom :ccc.

# Given 'local' rule (instance schema):
{ :xxx :bbb ?y. } => { ?y a :ccc. }.

# OWL allValuesFrom as a meta-rule (schematic):
{
  ?a owl:onProperty ?b.
  ?a owl:allValuesFrom ?c.
  { ?x ?b ?y. } => { ?y a ?c. }.
} => {
  { ?x a ?a. ?x ?b ?y. } => { ?y a ?c. }.
  ?x a ?a.
}.

Intuition:
- If class :aaa restricts property :bbb to values in :ccc,
- and we (already) have a rule saying “from :xxx :bbb ?y infer ?y a :ccc”,
- then we may conclude the specialized rule requiring ‘:xxx a :aaa’
  AND conclude the classification ‘:xxx a :aaa’ outright.
- This should entail  :xxx a :aaa .

Query:
{ ?S a ?C }  =>  { ?S a ?C } .
"""

from __future__ import annotations
from dataclasses import dataclass, field
from typing import Any, List, Optional, Set, Tuple

# -----------------------------------------------------------------------------
# Tiny RDF-ish store
# -----------------------------------------------------------------------------

Triple = Tuple[str, str, str]  # (s, p, o)

FACTS: Set[Triple] = {
    (":aaa", "owl:onProperty", ":bbb"),
    (":aaa", "owl:allValuesFrom", ":ccc"),
}

@dataclass(frozen=True)
class Rule:
    """One-antecedent / one-consequent triple rule (sufficient for this example)."""
    body: Triple
    head: Triple
    label: str = ""

# Given rule from the example:  { :xxx :bbb ?y } => { ?y a :ccc } .
GIVEN_RULE = Rule(
    body=(":xxx", ":bbb", "?y"),
    head=("?y", "a", ":ccc"),
    label="R_given"
)

# -----------------------------------------------------------------------------
# Pretty-proof kernel (same style as earlier answers)
# -----------------------------------------------------------------------------

@dataclass
class Conclusion:
    kind: str
    payload: Any
    def pretty(self) -> str:
        return self.payload if isinstance(self.payload, str) else str(self.payload)

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
        out = []
        for s in self.steps:
            prem = f" [{', '.join(map(str, s.premises))}]" if s.premises else ""
            note = f" // {s.notes}" if s.notes else ""
            out.append(f"[{s.id}] {s.rule}{prem}: {s.conclusion.pretty()}{note}")
        return "\n".join(out)

# -----------------------------------------------------------------------------
# Helper: simple symbolic unification for the meta-rule check
# -----------------------------------------------------------------------------

def is_var(t: str) -> bool:
    return isinstance(t, str) and t.startswith("?")

def unify_pair(x: str, y: str, env: dict) -> bool:
    """Unify two terms under env (very small subset: variables start with '?')."""
    if is_var(x) and is_var(y):
        # tie variables together
        xv = env.get(x, x)
        yv = env.get(y, y)
        if is_var(xv) and is_var(yv) and xv != yv:
            env[yv] = xv  # union-find lite
            return True
        if xv == yv:
            return True
        return False
    if is_var(x):
        xv = env.get(x, None)
        if xv is None:
            env[x] = y
            return True
        return xv == y
    if is_var(y):
        yv = env.get(y, None)
        if yv is None:
            env[y] = x
            return True
        return yv == x
    return x == y

def unify_triple(pat: Triple, tgt: Triple, env: dict) -> bool:
    return (
        unify_pair(pat[0], tgt[0], env) and
        unify_pair(pat[1], tgt[1], env) and
        unify_pair(pat[2], tgt[2], env)
    )

# -----------------------------------------------------------------------------
# Apply the OWL meta-rule effect:
# If we have  a:onProperty b  and  a:allValuesFrom c
# and a rule  { x b y } => { y a c } ,
# then conclude:
#   (i)  new rule { x a a . x b y } => { y a c }    (we record it)
#   (ii) x a a                                   (we assert it)
# -----------------------------------------------------------------------------

def apply_allValuesFrom_meta(FACTS: Set[Triple], rules: List[Rule], proof: Proof) -> None:
    s_intro = proof.add(
        "Premise-Rule (meta)",
        [],
        Conclusion("text",
                   "{ ?a owl:onProperty ?b. ?a owl:allValuesFrom ?c. { ?x ?b ?y. } => { ?y a ?c. }. }"
                   " => { { ?x a ?a. ?x ?b ?y. } => { ?y a ?c. }. ?x a ?a. }."),
        notes="OWL allValuesFrom internalization"
    )

    # Collect all (a,b,c) restrictions
    restrictions = []
    for (s1,p1,o1) in FACTS:
        if p1 == "owl:onProperty":
            a, b = s1, o1
            # find matching allValuesFrom for same a
            for (s2,p2,o2) in FACTS:
                if s2 == a and p2 == "owl:allValuesFrom":
                    restrictions.append((a,b,o2))  # (a,b,c)

    # For each restriction and each candidate rule, see if it matches the schema
    for (a,b,c) in restrictions:
        for r in rules:
            # Need: body is (x, b, y) and head is (y, a, c)
            env = {}
            ok_body = unify_triple(r.body, ("?x", b, "?y"), env)
            ok_head = unify_triple(r.head, ("?y", "a", c), env)
            if ok_body and ok_head:
                # Derived specialized rule (we'll record a textual form)
                specialized_rule_text = (
                    f"{{ {env.get('?x', '?x')} a {a}. {env.get('?x', '?x')} {b} {env.get('?y', '?y')}. }} "
                    f"=> {{ {env.get('?y', '?y')} a {c}. }}."
                )
                proof.add("Derive-Rule", [s_intro],
                          Conclusion("text", specialized_rule_text),
                          notes=f"from restriction ({a} onProperty {b}; allValuesFrom {c}) and {r.label}")

                # Derived class assertion: x a a
                x_term = env.get("?x", "?x")
                fact = (x_term, "a", a)
                if fact not in FACTS:
                    FACTS.add(fact)
                proof.add("Classify", [s_intro],
                          Conclusion("text", f"{x_term} a {a}"),
                          notes="OWL allValuesFrom ⇒ membership")

# -----------------------------------------------------------------------------
# Driver (query answering + proof)
# -----------------------------------------------------------------------------

def main():
    proof = Proof()

    # [1] Facts
    proof.add(
        "Facts",
        [],
        Conclusion("text", ":aaa owl:onProperty :bbb ; :aaa owl:allValuesFrom :ccc")
    )

    # [2] Given 'local' rule
    proof.add(
        "Given-Rule",
        [],
        Conclusion("text", "{ :xxx :bbb ?y. } => { ?y a :ccc. }  (R_given)")
    )

    # Apply OWL meta-rule internalization
    apply_allValuesFrom_meta(FACTS, [GIVEN_RULE], proof)

    # Query: { ?S a ?C } ⇒ list all rdf:type assertions
    answers = sorted([f"{s} a {o}" for (s,p,o) in FACTS if p == "a"])

    proof.add("Answer", [], Conclusion("text", " ; ".join(answers)),
              notes="All entailed type assertions")

    # Print results
    print("# Derived type assertions (?S a ?C)")
    for line in answers:
        print(line)

    print("\n=== Pretty Proof ===\n")
    print(proof.pretty())

if __name__ == "__main__":
    main()

