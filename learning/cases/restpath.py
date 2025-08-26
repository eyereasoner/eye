#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
restpath.py
───────────────────────────────────────────────────────────────────────────────
Description
-----------
Self-contained forward derivation + goal-oriented proof for the REST-path
example (9 steps, width 3), aligned with the N3 in your message.

Seed:
  <x> ex:rel1 <y> .

Rule chain:
  • Step 1: from rel1 produce 3 lanes at rel2 and emit a GET.
  • Steps 2..8: relk → rel{k+1} (k=2..8), emit a GET each time (lane-1).
  • Final step (k=9): rel9 → rel10, emit a GET, and assert  <x> ex:relGoal <y> .

ARC output:
  • Answer      — derived relGoal facts and all emitted GETs
  • Reason why  — tidy, numbered proof trace (Facts, Goal, rule sketch, 9 steps)
  • Check       — verifies: 9 GETs, 3 lanes at each level 2..10, goal is
                  exactly <x> ex:relGoal <y>, and GET linking integrity
"""

from __future__ import annotations
from dataclasses import dataclass, field
from typing import Any, Dict, List, Optional, Set, Tuple, Iterable
import itertools

Triple = Tuple[str, str, str]  # (s, p, o)

# ─────────────────────────────────────────────────────────────
# Tiny RDF-like KB
# ─────────────────────────────────────────────────────────────
class KB:
    def __init__(self):
        self.triples: Set[Triple] = set()
        self._gens = itertools.count(1)

    def add(self, s: str, p: str, o: str):
        self.triples.add((s, p, o))

    def fresh(self, stem: str) -> str:
        return f"_:${stem}{next(self._gens)}"

    def ask(self, s: str | None = None, p: str | None = None, o: str | None = None) -> Iterable[Triple]:
        for (S, P, O) in self.triples:
            if (s is None or s == S) and (p is None or p == P) and (o is None or o == O):
                yield (S, P, O)

    def has(self, s: str | None = None, p: str | None = None, o: str | None = None) -> bool:
        return next(self.ask(s, p, o), None) is not None

# ─────────────────────────────────────────────────────────────
# Pretty proof scaffold
# ─────────────────────────────────────────────────────────────
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

# ─────────────────────────────────────────────────────────────
# Rule applications (mirror the N3)
# ─────────────────────────────────────────────────────────────
def apply_step1(kb: KB, a1: str, b1: str, proof: Proof) -> List[Tuple[str,str]]:
    # emit GET on lane-1 and create 3 lanes at rel2
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
              Conclusion("text", f"emit GET(req={req}, uri={a1}, body={b1}); "
                                 f"create lanes: (a1,b1)=({a1},{b1}), (a2,b2)=({a2},{b2}), (a3,b3)=({a3},{b3})"),
              notes="Existential a2,b2,a3,b3")
    return [(a1,b1),(a2,b2),(a3,b3)]

def apply_mid_step(kb: KB, k: int, lanes_k: List[Tuple[str,str]], proof: Proof) -> List[Tuple[str,str]]:
    # relk → rel{k+1} for the 3 lanes; emit GET on lane-1
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
    return lanes_next

def apply_final_step(kb: KB, lanes9: List[Tuple[str,str]], proof: Proof) -> List[Tuple[str,str]]:
    # rel9 → rel10; emit GET; also assert relGoal on lane-1
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
    return lanes10

# ─────────────────────────────────────────────────────────────
# Pipeline orchestration
# ─────────────────────────────────────────────────────────────
def run_pipeline() -> Tuple[KB, Proof]:
    kb = KB()
    proof = Proof()

    # Seed fact matches your N3
    a1, b1 = "<x>", "<y>"
    kb.add(a1, "ex:rel1", b1)
    proof.add("Facts", [], Conclusion("text", f"{a1} ex:rel1 {b1}"))

    # Goal sketch
    proof.add("Goal", [], Conclusion("text", "{ <x> ex:relGoal <y> }"))

    # Chain sketch
    proof.add("Premise-Rule", [],
              Conclusion("text",
                         "Chain: rel1→rel2 (create 3 lanes), then relk→rel{k+1} for k=2..9; final also asserts relGoal"),
              notes="Each step emits a GET on lane-1")

    # Apply the 9 GET-emitting steps
    lanes2  = apply_step1(kb, a1, b1, proof)     # rel2
    lanes3  = apply_mid_step(kb, 2, lanes2, proof)
    lanes4  = apply_mid_step(kb, 3, lanes3, proof)
    lanes5  = apply_mid_step(kb, 4, lanes4, proof)
    lanes6  = apply_mid_step(kb, 5, lanes5, proof)
    lanes7  = apply_mid_step(kb, 6, lanes6, proof)
    lanes8  = apply_mid_step(kb, 7, lanes7, proof)
    lanes9  = apply_mid_step(kb, 8, lanes8, proof)
    _lanes10 = apply_final_step(kb, lanes9, proof)

    return kb, proof

def list_gets(kb: KB) -> List[Tuple[str,str,str]]:
    """Return [(req, uri, body)] sorted by req id."""
    gets = sorted(kb.ask(p="http:methodName"), key=lambda t: t[0])
    out: List[Tuple[str,str,str]] = []
    for (req, _, _) in gets:
        uri  = next((o for (_,_,o) in kb.ask(s=req, p="http:requestURI")), "?")
        resp = next((o for (_,_,o) in kb.ask(s=req, p="http:resp")), None)
        body = next((o for (_,_,o) in kb.ask(s=resp, p="http:body")), "?") if resp else "?"
        out.append((req, uri, body))
    return out

# ─────────────────────────────────────────────────────────────
# ARC sections
# ─────────────────────────────────────────────────────────────
def arc_answer(kb: KB) -> None:
    print("Answer")
    print("------")
    goals = sorted(kb.ask(p="ex:relGoal"))
    for s,_,o in goals:
        print(f"{s} ex:relGoal {o}")
    gets = list_gets(kb)
    print(f"\nEmitted GET requests: {len(gets)}")
    for req, uri, body in gets:
        print(f"  {req}: GET {uri}  → body {body}")
    print()

def arc_reason(proof: Proof) -> None:
    print("Reason why")
    print("----------")
    print(proof.pretty())
    print()

def arc_check(kb: KB, proof: Proof) -> None:
    print("Check (harness)")
    print("---------------")
    # (1) Exactly one relGoal and it is (<x>,<y>)
    goals = list(kb.ask(p="ex:relGoal"))
    assert len(goals) == 1 and goals[0] == ("<x>", "ex:relGoal", "<y>"), f"Unexpected ex:relGoal facts: {goals}"

    # (2) Exactly 9 GETs (one per step)
    gets = list_gets(kb)
    assert len(gets) == 9, f"Expected 9 GETs, found {len(gets)}"
    for _, uri, body in gets:
        assert uri == "<x>" and body == "<y>", "GET should use lane-1 URI <x> and body <y>"

    # (3) For each level k=2..10 there are exactly 3 lanes; and lane-1 is (<x>,<y>)
    for k in range(2, 11):
        triples = list(kb.ask(p=f"ex:rel{k}"))
        assert len(triples) == 3, f"Expected 3 lanes at rel{k}, found {len(triples)}"
        assert ("<x>", f"ex:rel{k}", "<y>") in triples, f"Missing lane-1 at rel{k}"

    # (4) Proof structure: Facts + Goal + Premise-Rule + 9 steps = 12 lines
    assert len(proof.steps) == 12, f"Unexpected number of proof steps: {len(proof.steps)}"
    assert proof.steps[0].rule.startswith("Facts"), "First proof step should record Facts"
    assert proof.steps[1].rule == "Goal", "Second proof step should record Goal"
    assert "Premise-Rule" in proof.steps[2].rule, "Third step should sketch the rule chain"
    for i, s in enumerate(proof.steps[3:], start=1):
        expect = f"Step-{i}"
        assert s.rule.startswith(expect), f"Expected {expect}, got {s.rule}"

    # (5) GET triples link correctly
    for req, _, body in gets:
        assert kb.has(s=req, p="http:methodName", o='"GET"')
        assert kb.has(s=req, p="http:requestURI", o="<x>")
        resp = next((o for (_,_,o) in kb.ask(s=req, p="http:resp")), None)
        assert resp is not None and kb.has(s=resp, p="http:body", o=body), "Response/body linkage broken"

    print("OK: goal fact, GET count/linking, lane counts per level, and proof shape verified.\n")

# ─────────────────────────────────────────────────────────────
# Main
# ─────────────────────────────────────────────────────────────
if __name__ == "__main__":
    kb, proof = run_pipeline()
    arc_answer(kb)
    arc_reason(proof)
    arc_check(kb, proof)

