#!/usr/bin/env python3
"""
REST-path (9 steps, width 3) — self-contained forward derivation + goal-oriented proof

We mirror the chain of N3 rules in path-9-3.n3:

  Step 1 (seed):
    { ?a1 ex:rel1 ?b1. }
    =>
    { ?request http:methodName "GET".
      ?request http:requestURI ?a1.
      ?request http:resp ?resp.
      ?resp    http:body ?b1.
      ?a1 ex:rel2 ?b1.
      ?a2 ex:rel2 ?b2.
      ?a3 ex:rel2 ?b3. }     # a2,b2,a3,b3 are conclusion-only (existential) quick vars

  Steps k=2..9 (propagate triples for 3 lanes and emit a GET each time):
    { ?a1 ex:relk ?b1. ?a2 ex:relk ?b2. ?a3 ex:relk ?b3. }
    =>
    { ?request http:methodName "GET".
      ?request http:requestURI ?a1.
      ?request http:resp ?resp.
      ?resp    http:body ?b1.
      ?a1 ex:rel{k+1} ?b1.
      ?a2 ex:rel{k+1} ?b2.
      ?a3 ex:rel{k+1} ?b3. }

  Final head (at k=9) additionally asserts:
      ?a1 ex:relGoal ?b1.

We start from a single seed fact:
    :a1 ex:rel1 :b1 .
"""

from __future__ import annotations
from dataclasses import dataclass, field
from typing import Any, Dict, List, Optional, Set, Tuple
import itertools

# --------------------------------------------------------------------------------------
# Basic RDF-like store for this toy
# --------------------------------------------------------------------------------------

Triple = Tuple[str, str, str]  # (s, p, o)

class KB:
    def __init__(self):
        self.triples: Set[Triple] = set()
        self._gens = itertools.count(1)

    def add(self, s: str, p: str, o: str):
        self.triples.add((s, p, o))

    def fresh(self, stem: str) -> str:
        return f"_:${stem}{next(self._gens)}"

    def ask(self, s=None, p=None, o=None):
        for (S, P, O) in self.triples:
            if (s is None or s == S) and (p is None or p == P) and (o is None or o == O):
                yield (S, P, O)

    def has(self, s=None, p=None, o=None) -> bool:
        return next(self.ask(s, p, o), None) is not None

# --------------------------------------------------------------------------------------
# Pretty-proof kernel (same style as in earlier answers)
# --------------------------------------------------------------------------------------

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
        lines = []
        for s in self.steps:
            prem = f" [{', '.join(map(str, s.premises))}]" if s.premises else ""
            note = f" // {s.notes}" if s.notes else ""
            lines.append(f"[{s.id}] {s.rule}{prem}: {s.conclusion.pretty()}{note}")
        return "\n".join(lines)

# --------------------------------------------------------------------------------------
# Rules (inlined from path-9-3.n3, generalized)
# --------------------------------------------------------------------------------------

def apply_step1(kb: KB, a1: str, b1: str, proof: Proof) -> Tuple[List[Tuple[str,str]], Tuple[str,str,str,str]]:
    """
    Step 1: from a single rel1 pair, emit GET and produce 3 lanes at rel2.
    Returns:
      - lanes at rel2: [(a1,b1), (a2,b2), (a3,b3)]
      - the (request, resp, uri, body) identifiers for the GET emitted here
    """
    req, resp = kb.fresh("req"), kb.fresh("resp")
    a2, b2 = kb.fresh("a"), kb.fresh("b")
    a3, b3 = kb.fresh("a"), kb.fresh("b")

    kb.add(req, "http:methodName", '"GET"')
    kb.add(req, "http:requestURI", a1)
    kb.add(req, "http:resp", resp)
    kb.add(resp, "http:body", b1)

    kb.add(a1, "ex:rel2", b1)
    kb.add(a2, "ex:rel2", b2)
    kb.add(a3, "ex:rel2", b3)

    proof.add("Step-1 (rel1→rel2)", [],
              Conclusion("text", f"emit GET(req={req}, uri={a1}, body={b1}); create lanes: "
                                 f"(a1,b1)=({a1},{b1}), (a2,b2)=({a2},{b2}), (a3,b3)=({a3},{b3})"),
              notes="Conclusion-only vars existentially created")
    return [(a1,b1),(a2,b2),(a3,b3)], (req, resp, a1, b1)

def apply_mid_step(kb: KB, k: int, lanes_k: List[Tuple[str,str]], proof: Proof) -> Tuple[List[Tuple[str,str]], Tuple[str,str,str,str]]:
    """
    Steps 2..8: relk → rel{k+1} for 3 lanes, with a GET on lane-1.
    """
    assert len(lanes_k) == 3
    a1,b1 = lanes_k[0]

    req, resp = kb.fresh("req"), kb.fresh("resp")
    kb.add(req, "http:methodName", '"GET"')
    kb.add(req, "http:requestURI", a1)
    kb.add(req, "http:resp", resp)
    kb.add(resp, "http:body", b1)

    lanes_next = []
    for (ai, bi) in lanes_k:
        kb.add(ai, f"ex:rel{k+1}", bi)
        lanes_next.append((ai, bi))

    proof.add(f"Step-{k} (rel{k}→rel{k+1})", [],
              Conclusion("text", f"emit GET(req={req}, uri={a1}, body={b1}); promote 3 lanes to rel{k+1}"))
    return lanes_next, (req, resp, a1, b1)

def apply_final_step(kb: KB, lanes9: List[Tuple[str,str]], proof: Proof) -> Tuple[List[Tuple[str,str]], Tuple[str,str,str,str]]:
    """
    Step 9: rel9 → rel10 for 3 lanes, also assert relGoal on lane-1.
    """
    assert len(lanes9) == 3
    a1,b1 = lanes9[0]

    req, resp = kb.fresh("req"), kb.fresh("resp")
    kb.add(req, "http:methodName", '"GET"')
    kb.add(req, "http:requestURI", a1)
    kb.add(req, "http:resp", resp)
    kb.add(resp, "http:body", b1)

    lanes10 = []
    for (ai, bi) in lanes9:
        kb.add(ai, "ex:rel10", bi)
        lanes10.append((ai, bi))

    kb.add(a1, "ex:relGoal", b1)

    proof.add("Step-9 (rel9→rel10 + relGoal)", [],
              Conclusion("text", f"emit GET(req={req}, uri={a1}, body={b1}); promote to rel10; assert relGoal({a1},{b1})"))
    return lanes10, (req, resp, a1, b1)

# --------------------------------------------------------------------------------------
# Driver: assemble the chain and print a proof
# --------------------------------------------------------------------------------------

def main():
    kb = KB()
    proof = Proof()

    # Facts / seed (see note above about initial.n3)
    a1, b1 = ":a1", ":b1"
    kb.add(a1, "ex:rel1", b1)
    s_f = proof.add("Facts", [], Conclusion("text", f"{a1} ex:rel1 {b1}"))

    # Goal
    s_g = proof.add("Goal", [], Conclusion("text", "{ ?S ex:relGoal ?O }"))

    # Rule sketch (from path-9-3.n3)
    proof.add("Premise-Rule", [],
              Conclusion("text",
                         "Chain of 9 GET-emitting rules: rel1→rel2 (create 3 lanes), then relk→rel{k+1} for k=2..9; final also asserts relGoal"),
              notes="Each step copies 3 lanes and emits a GET on lane-1")

    # Apply steps
    lanes2, _ = apply_step1(kb, a1, b1, proof)                    # rel2
    lanes3, _ = apply_mid_step(kb, 2, lanes2, proof)               # rel3
    lanes4, _ = apply_mid_step(kb, 3, lanes3, proof)               # rel4
    lanes5, _ = apply_mid_step(kb, 4, lanes4, proof)               # rel5
    lanes6, _ = apply_mid_step(kb, 5, lanes5, proof)               # rel6
    lanes7, _ = apply_mid_step(kb, 6, lanes6, proof)               # rel7
    lanes8, _ = apply_mid_step(kb, 7, lanes7, proof)               # rel8
    lanes9, _ = apply_mid_step(kb, 8, lanes8, proof)               # rel9
    lanes10, _= apply_final_step(kb, lanes9, proof)                # rel10 + relGoal

    # Answers (all relGoal facts + the emitted GET requests)
    goals = sorted(kb.ask(p="ex:relGoal"), key=lambda t: (t[0], t[1], t[2]))
    gets  = sorted(kb.ask(p="http:methodName"), key=lambda t: t[0])  # list requests by id

    # Present concise results
    print("# Derived relGoal facts")
    for s,_,o in goals:
        print(f"{s} ex:relGoal {o}")

    print("\n# Emitted GET requests (skolem ids)")
    for (req,_,_) in gets:
        uri = next((o for (S,P,O) in kb.ask(s=req, p="http:requestURI")), "?")
        body= next((o for (S,P,O) in kb.ask(s=next((o for (S,P,O) in kb.ask(s=req, p="http:resp")), None), p="http:body")), "?")
        print(f"{req}: GET {uri}  → body {body}")

    # Pretty proof
    print("\n=== Pretty Proof ===\n")
    print(proof.pretty())

if __name__ == "__main__":
    main()

