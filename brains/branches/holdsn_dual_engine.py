#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
README (plain text)
===================
Purpose
-------
A universal “branch” (in the spirit of https://eyereasoner.github.io/eye/brains/)
that supports **both** bottom-up and top-down reasoning, and **automatically chooses**
between them per query. It showcases the Hayes–Menzel idea: relations like
`ex:ParentOf` are **names** (intensions), and application is mediated by a fixed
predicate

    ex:holds2(R, x, y)   (read: ⟨x,y⟩ is in the extension of relation-name R)

so that “quantifying over predicates” is actually quantifying over **names**.

What’s inside
-------------
- A **top-down engine** (SLD) with proper **unification** and **standardize-apart**
  (fresh variables per clause use).
- A lightweight **bottom-up engine** that computes the least fixed point (Kleene)
  for `ex:holds2` together with inclusion over relation-names.
- An **auto-chooser** that picks an engine based on the query:
  * enumerate-heavy / unbound `AncestorOf(X,Y)` → bottom-up
  * ground/mostly-bound goals → top-down
  * (It will fall back if a search hits a step threshold.)

Model (realistic facts)
-----------------------
Individuals D = {Sophroniscus, Socrates, Lamprocles, Ariston, Plato, Nicomachus, Aristotle}

Relations (names as strings):
  ex:FatherOf   : Sophroniscus→Socrates, Socrates→Lamprocles, Ariston→Plato, Nicomachus→Aristotle
  ex:ParentOf   : (same four pairs)
  ex:TeacherOf  : Socrates→Plato
  ex:AncestorOf : derived (no base pairs)

Inclusion over relation *names*:
  ex:SubRelOf   : FatherOf ⊆ ParentOf,  ParentOf ⊆ AncestorOf

Program (rules over fixed predicates)
-------------------------------------
1) holds2(AncestorOf, X, Y) :- holds2(ParentOf, X, Y).
2) holds2(AncestorOf, X, Z) :- holds2(ParentOf, X, Y), holds2(AncestorOf, Y, Z).
3) leq_strict(P, Q)        :- SubRelOf(P, Q).
4) leq_strict(P, Q)        :- SubRelOf(P, R), leq_strict(R, Q).
5) leq(P, P).     (reflexive over relation-names)
6) leq(P, Q)                :- leq_strict(P, Q).
7) holds2(Q, X, Y)          :- leq_strict(P, Q), holds2(P, X, Y).   (lifting)

Typical questions (auto-chosen engine)
--------------------------------------
Q1  List all (X,Y) with holds2(AncestorOf, X, Y).     → bottom-up (enumeration)
Q2  ∃R: holds2(R, Socrates, Lamprocles) ∧ leq(R, AncestorOf) ? (witnesses) → top-down
Q3  ∀R,y: (leq(R, ParentOf) ∧ holds2(R, Socrates, y)) → holds2(AncestorOf, Socrates, y) ? → mixed

How to run
----------
    python3 holdsn_dual_engine.py

Output
------
Model → Question → Answer (with chosen engine labels) → Reason why → Check (harness)

Deterministic output; no external dependencies.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Dict, Iterable, List, Optional, Set, Tuple, Union
from collections import defaultdict

# =========================================================
# Shared: names, model, and rule vocabulary
# =========================================================

D: Tuple[str, ...] = (
    "Sophroniscus",
    "Socrates",
    "Lamprocles",
    "Ariston",
    "Plato",
    "Nicomachus",
    "Aristotle",
)

EX = "ex:"
FatherOf   = EX + "FatherOf"
ParentOf   = EX + "ParentOf"
TeacherOf  = EX + "TeacherOf"
AncestorOf = EX + "AncestorOf"
SubRelOf   = EX + "SubRelOf"
LeqStrict  = EX + "leq_strict"
Leq        = EX + "leq"
Holds2     = EX + "holds2"

# =========================================================
# Program (facts + rules) in a Prolog-like syntax
# =========================================================

class Var:
    __slots__ = ("name","id")
    _c = 0
    def __init__(self, name: str): Var._c+=1; self.name=name; self.id=Var._c
    def __repr__(self): return f"Var({self.name}_{self.id})"
    def __hash__(self): return hash(self.id)
    def __eq__(self, o): return isinstance(o, Var) and self.id==o.id

Term = Union[str, Var]

@dataclass
class Atom:
    pred: str
    args: List[Term]

@dataclass
class Clause:
    head: Atom
    body: List[Atom]

def atom(pred: str, *args: Term) -> Atom: return Atom(pred, list(args))
def fact(pred: str, *args: Term) -> Clause: return Clause(atom(pred,*args), [])

PROGRAM: List[Clause] = []

# Base holds2 facts
for a,b in [
    ("Sophroniscus","Socrates"),
    ("Socrates","Lamprocles"),
    ("Ariston","Plato"),
    ("Nicomachus","Aristotle"),
]:
    PROGRAM += [fact(Holds2, FatherOf, a, b),
                fact(Holds2, ParentOf, a, b)]
PROGRAM.append(fact(Holds2, TeacherOf, "Socrates", "Plato"))

# SubRelOf facts (over relation-names)
PROGRAM += [
    fact(SubRelOf, FatherOf, ParentOf),
    fact(SubRelOf, ParentOf, AncestorOf),
]

# Rules over names: leq_strict / leq
P,Q,R = Var("P"),Var("Q"),Var("R")
PROGRAM.append(Clause(atom(LeqStrict,P,Q), [atom(SubRelOf,P,Q)]))
P,Q,R = Var("P"),Var("Q"),Var("R")
PROGRAM.append(Clause(atom(LeqStrict,P,Q), [atom(SubRelOf,P,R), atom(LeqStrict,R,Q)]))
P = Var("P")
PROGRAM.append(Clause(atom(Leq,P,P), []))                      # reflexive
P,Q = Var("P"),Var("Q")
PROGRAM.append(Clause(atom(Leq,P,Q), [atom(LeqStrict,P,Q)]))

# Ancestor rules (top-down form, via holds2)
X,Y = Var("X"),Var("Y")
PROGRAM.append(Clause(atom(Holds2, AncestorOf, X, Y),
                      [atom(Holds2, ParentOf, X, Y)]))
X,Y,Z = Var("X"),Var("Y"),Var("Z")
PROGRAM.append(Clause(atom(Holds2, AncestorOf, X, Z),
                      [atom(Holds2, ParentOf, X, Y),
                       atom(Holds2, AncestorOf, Y, Z)]))

# Lifting along inclusion of names
P,Q,X,Y = Var("P"),Var("Q"),Var("X"),Var("Y")
PROGRAM.append(Clause(atom(Holds2, Q, X, Y),
                      [atom(LeqStrict, P, Q), atom(Holds2, P, X, Y)]))

# Index by predicate
PROG_BY: Dict[str, List[Clause]] = defaultdict(list)
for c in PROGRAM: PROG_BY[c.head.pred].append(c)

# =========================================================
# Top-down (SLD) engine with standardize-apart + unification
# =========================================================

def deref(t: Term, s: Dict[Var,Term]) -> Term:
    while isinstance(t, Var) and t in s: t = s[t]
    return t

def unify(t1: Term, t2: Term, s: Dict[Var,Term]) -> Optional[Dict[Var,Term]]:
    t1 = deref(t1,s); t2 = deref(t2,s)
    if t1 == t2: return s
    if isinstance(t1, Var): s[t1]=t2; return s
    if isinstance(t2, Var): s[t2]=t1; return s
    return None

def unify_lists(xs: List[Term], ys: List[Term], s: Dict[Var,Term]) -> Optional[Dict[Var,Term]]:
    if len(xs)!=len(ys): return None
    for a,b in zip(xs,ys):
        s = unify(a,b,s)
        if s is None: return None
    return s

def standardize_apart(cl: Clause) -> Clause:
    m: Dict[Var,Var] = {}
    def f(t: Term) -> Term:
        if isinstance(t,Var):
            if t not in m: m[t] = Var(t.name)
            return m[t]
        return t
    def fa(a: Atom) -> Atom: return Atom(a.pred, [f(t) for t in a.args])
    return Clause(fa(cl.head), [fa(a) for a in cl.body])

def apply_s(a: Atom, s: Dict[Var,Term]) -> Atom:
    return Atom(a.pred, [deref(t,s) for t in a.args])

def solve_topdown(goals: List[Atom], step_limit: int = 10000) -> Tuple[List[Dict[Var,Term]], int]:
    answers: List[Dict[Var,Term]] = []
    steps = 0
    def dfs(gs: List[Atom], s: Dict[Var,Term]) -> None:
        nonlocal steps
        if steps > step_limit: return
        if not gs: answers.append(s.copy()); return
        g = gs[0]
        for cl in PROG_BY.get(g.pred, []):
            c = standardize_apart(cl)
            steps += 1
            g1 = apply_s(g, s)
            h1 = apply_s(c.head, s)
            s2 = unify_lists(h1.args, g1.args, s.copy())
            if s2 is None: continue
            dfs(c.body + gs[1:], s2)
    dfs(goals, {})
    return answers, steps

# =========================================================
# Bottom-up engine (Kleene LFP over holds2 + name inclusion)
# =========================================================

def bottomup_closure() -> Tuple[Dict[str, Set[Tuple[Term,...]]], int]:
    """Compute all derived facts for holds2 / leq_strict / leq using naive iteration.
       Returns (facts_by_pred, rounds)."""
    facts: Dict[str, Set[Tuple[Term,...]]] = defaultdict(set)
    # seed with ground base facts
    for cl in PROGRAM:
        if not cl.body and all(isinstance(t, str) for t in cl.head.args):
            facts[cl.head.pred].add(tuple(cl.head.args))

    # collect the set of relation *names* we know about
    RELS: Set[str] = set()
    for (r, x, y) in list(facts[Holds2]):          # ← fixed: iterate triples
        if isinstance(r, str):
            RELS.add(r)
    for (p, q) in list(facts[SubRelOf]):
        if isinstance(p, str) and isinstance(q, str):
            RELS.update([p, q])

    rounds = 0
    changed = True
    while changed:
        rounds += 1
        changed = False

        # 1) leq_strict from SubRelOf and (iterated) transitive closure
        for (p, q) in list(facts[SubRelOf]):
            if (p, q) not in facts[LeqStrict]:
                facts[LeqStrict].add((p, q)); changed = True
        for (p, r) in list(facts[LeqStrict]):
            for (x, q) in list(facts[LeqStrict]):
                if r == x and (p, q) not in facts[LeqStrict]:
                    facts[LeqStrict].add((p, q)); changed = True

        # 2) leq is reflexive on RELS plus leq_strict
        for r in RELS:
            if (r, r) not in facts[Leq]:
                facts[Leq].add((r, r)); changed = True
        for (p, q) in list(facts[LeqStrict]):
            if (p, q) not in facts[Leq]:
                facts[Leq].add((p, q)); changed = True

        # 3) holds2(AncestorOf,x,y) :- holds2(ParentOf,x,y)
        for (r, x, y) in list(facts[Holds2]):
            if r == ParentOf and (AncestorOf, x, y) not in facts[Holds2]:
                facts[Holds2].add((AncestorOf, x, y)); changed = True

        # 4) holds2(AncestorOf,x,z) :- holds2(ParentOf,x,y), holds2(AncestorOf,y,z)
        par = {(x, y) for (r, x, y) in facts[Holds2] if r == ParentOf}
        anc = {(y, z) for (r, y, z) in facts[Holds2] if r == AncestorOf}
        for (x, y) in par:
            for (y2, z) in anc:
                if y == y2 and (AncestorOf, x, z) not in facts[Holds2]:
                    facts[Holds2].add((AncestorOf, x, z)); changed = True

        # 5) lifting: holds2(Q,x,y) :- leq_strict(P,Q), holds2(P,x,y)
        inc = list(facts[LeqStrict])
        holds = list(facts[Holds2])
        for (p, q) in inc:
            for (r, x, y) in holds:
                if p == r and (q, x, y) not in facts[Holds2]:
                    facts[Holds2].add((q, x, y)); changed = True

    return facts, rounds

def solve_bottomup(goals: List[Atom]) -> Tuple[List[Dict[Var,Term]], int]:
    """Answer non-ground goals by matching against bottom-up computed facts."""
    facts, rounds = bottomup_closure()
    # Build a join over goals
    sols: List[Dict[Var,Term]] = [dict()]
    def match_atom(a: Atom, s: Dict[Var,Term]) -> List[Dict[Var,Term]]:
        pred = a.pred
        tuples = facts[pred]
        outs: List[Dict[Var,Term]] = []
        for tpl in tuples:
            if len(tpl) != len(a.args): continue
            s2 = s.copy()
            ok = True
            for arg, val in zip(a.args, tpl):
                # unify variable/constant with constant (bottom-up facts are ground)
                if isinstance(arg, Var):
                    cur = deref(arg, s2)
                    if isinstance(cur, Var):
                        s2[cur] = val
                    else:
                        if cur != val: ok=False; break
                else:
                    if arg != val: ok=False; break
            if ok: outs.append(s2)
        return outs
    for a in goals:
        new_sols: List[Dict[Var,Term]] = []
        for s in sols:
            new_sols.extend(match_atom(a, s))
        sols = new_sols
    return sols, rounds

# =========================================================
# Auto chooser + helpers
# =========================================================

def is_var(t: Term) -> bool: return isinstance(t, Var)

def choose_engine(goals: List[Atom]) -> str:
    """Heuristic:
       - If any goal is holds2(AncestorOf, X, Y) with a variable → bottom-up.
       - If any goal is fully unbound (all args variables) → bottom-up.
       - Otherwise → top-down."""
    for g in goals:
        if g.pred == Holds2 and len(g.args)==3 and g.args[0]==AncestorOf and (is_var(g.args[1]) or is_var(g.args[2])):
            return "bottomup"
        if all(is_var(t) for t in g.args):
            return "bottomup"
    return "topdown"

def ask(goals: List[Atom], step_limit: int = 10000, fallback_threshold: int = 4000) -> Tuple[str, List[Dict[Var,Term]], int]:
    engine = choose_engine(goals)
    if engine == "topdown":
        sols, steps = solve_topdown(goals, step_limit=step_limit)
        if steps > fallback_threshold:
            # Fallback to bottom-up if the search exploded
            sols, rounds = solve_bottomup(goals)
            engine = "bottomup"
            return engine, sols, rounds
        return engine, sols, steps
    else:
        sols, rounds = solve_bottomup(goals)
        return engine, sols, rounds

# =========================================================
# Pretty helpers
# =========================================================

def local(name: str) -> str: return name.split(":",1)[1] if ":" in name else name

def fmt_ans_substs(varlist: List[Var], solns: List[Dict[Var,Term]]) -> str:
    if not solns: return "∅"
    rows = []
    for s in solns:
        vals = []
        for v in varlist:
            t = deref(v, s)
            vals.append(t if isinstance(t,str) else f"?{v.name}")
        rows.append("(" + ", ".join(str(x) for x in vals) + ")")
    # determinize
    rows = sorted(set(rows))
    return "{" + ", ".join(rows) + "}"

def fmt_pairs(pairs: Iterable[Tuple[str,str]]) -> str:
    seq = sorted(pairs)
    return "∅" if not seq else "{" + ", ".join(f"⟨{a},{b}⟩" for (a,b) in seq) + "}"

# =========================================================
# Branch: Model → Question → Answer → Reason why
# =========================================================

def print_model() -> None:
    print("Model")
    print("=====")
    print(f"Individuals D = {list(D)}\n")
    print("Fixed predicates")
    print("----------------")
    print("• ex:holds2(R,x,y)   — application (⟨x,y⟩ in extension of relation-name R)")
    print("• ex:SubRelOf(P,Q)   — inclusion over relation *names*")
    print("• ex:leq_strict / ex:leq   — ⊆* with/without reflex on names\n")
    print("Named relations (with facts)")
    print("----------------------------")
    print("FatherOf  =", fmt_pairs([("Sophroniscus","Socrates"), ("Socrates","Lamprocles"), ("Ariston","Plato"), ("Nicomachus","Aristotle")]))
    print("ParentOf  =", "same pairs (different name)")
    print("TeacherOf =", fmt_pairs([("Socrates","Plato")]))
    print("AncestorOf = derived only (no base facts)\n")
    print("Inclusions over names: FatherOf ⊆ ParentOf, ParentOf ⊆ AncestorOf\n")

def print_question() -> None:
    print("Question")
    print("========")
    print("Q1) List all (X,Y) with holds2(AncestorOf,X,Y).   [auto engine]")
    print("Q2) ∃R: holds2(R,Socrates,Lamprocles) ∧ leq(R,AncestorOf) ?  [auto engine]")
    print("Q3) ∀R,y: (leq(R,ParentOf) ∧ holds2(R,Socrates,y)) → holds2(AncestorOf,Socrates,y) ?  [auto engine]")
    print()

def run_queries():
    # Q1
    X,Y = Var("X"), Var("Y")
    eng1, sols1, metric1 = ask([atom(Holds2, AncestorOf, X, Y)])
    # Format pairs
    pairs = []
    for s in sols1:
        x = deref(X,s); y = deref(Y,s)
        if isinstance(x,str) and isinstance(y,str): pairs.append((x,y))
    pairs = sorted(set(pairs))

    # Q2
    R = Var("R")
    eng2, sols2, metric2 = ask([atom(Holds2, R, "Socrates", "Lamprocles"),
                                atom(Leq,   R, AncestorOf)])
    witnesses = sorted({deref(R,s) for s in sols2 if isinstance(deref(R,s), str)})

    # Q3 (universal by enumeration with the auto-chooser per conjunct)
    # Holds iff no counterexample exists
    ok = True
    RELS = [FatherOf, ParentOf, TeacherOf, AncestorOf]
    for r in RELS:
        for y in D:
            # if leq(r,ParentOf) ∧ holds2(r,S,y) then must have holds2(AncestorOf,S,y)
            _, condbind, _ = ask([atom(Leq, r, ParentOf), atom(Holds2, r, "Socrates", y)])
            if condbind:
                eng3, need, _ = ask([atom(Holds2, AncestorOf, "Socrates", y)])
                if not need:
                    ok = False
                    break
        if not ok: break

    return (("Q1", eng1, pairs, metric1),
            ("Q2", eng2, witnesses, metric2),
            ("Q3", "mixed", ok, 0))

def print_answer(res1, res2, res3) -> None:
    print("Answer")
    print("======")
    tag1, eng1, pairs, metric1 = res1
    tag2, eng2, witnesses, metric2 = res2
    tag3, eng3, ok, _ = res3
    print(f"{tag1}) Engine: {eng1} → AncestorOf =", fmt_pairs(pairs))
    print(f"{tag2}) Engine: {eng2} → Witness relation-names R = " + ("∅" if not witnesses else "{" + ", ".join(sorted(local(w) for w in witnesses)) + "}"))
    print(f"{tag3}) Engine: {eng3} → Universal statement holds: {'Yes' if ok else 'No'}\n")

def print_reason(eng1, eng2) -> None:
    print("Reason why")
    print("==========")
    print("• Heuristics:")
    print("  - If a query would enumerate many answers (e.g., AncestorOf(X,Y) with variables),")
    print("    we pick **bottom-up** and compute the closure once.")
    print("  - If a query is ground or tightly bound (few variables), we pick **top-down**")
    print("    to avoid materializing unrelated facts.")
    print("  - If a top-down search exceeds a step threshold, we **fallback** to bottom-up.")
    print(f"• In this run: Q1 used {eng1}; Q2 used {eng2}; Q3 checks each needed conjunct with the same policy.")
    print("• Both engines operate over the same Hayes–Menzel core (names as intensions + fixed holds₂),")
    print("  so their answers coincide; the harness below checks this equivalence.\n")

# =========================================================
# Check (harness) — both engines agree, properties hold
# =========================================================

class CheckFailure(AssertionError): pass
def check(c: bool, msg: str): 
    if not c: raise CheckFailure(msg)

def run_checks() -> List[str]:
    notes: List[str] = []

    # Expected Ancestor pairs
    expected = {
        ("Sophroniscus","Socrates"),
        ("Sophroniscus","Lamprocles"),
        ("Socrates","Lamprocles"),
        ("Ariston","Plato"),
        ("Nicomachus","Aristotle"),
    }

    # 1) Bottom-up enumerates expected AncestorOf
    X,Y = Var("X"),Var("Y")
    sols_bu, _ = solve_bottomup([atom(Holds2, AncestorOf, X, Y)])
    anc_bu = {(deref(X,s), deref(Y,s)) for s in sols_bu}
    check(anc_bu == expected, "Bottom-up AncestorOf enumeration mismatch.")
    notes.append("PASS 1: Bottom-up AncestorOf enumeration is correct.")

    # 2) Top-down enumerates the same AncestorOf
    sols_td, _ = solve_topdown([atom(Holds2, AncestorOf, X, Y)])
    anc_td = {(deref(X,s), deref(Y,s)) for s in sols_td}
    check(anc_td == expected, "Top-down AncestorOf enumeration mismatch.")
    notes.append("PASS 2: Top-down AncestorOf enumeration is correct.")

    # 3) Engines agree on existential witnesses
    R = Var("R")
    bu_w, _ = solve_bottomup([atom(Holds2, R, "Socrates","Lamprocles"), atom(Leq, R, AncestorOf)])
    td_w, _ = solve_topdown([atom(Holds2, R, "Socrates","Lamprocles"), atom(Leq, R, AncestorOf)])
    w1 = {deref(R,s) for s in bu_w}; w2 = {deref(R,s) for s in td_w}
    check(w1 == w2 == {FatherOf, ParentOf, AncestorOf}, "Witness set mismatch between engines.")
    notes.append("PASS 3: Engines agree on existential witnesses.")

    # 4) Universal property holds (via auto ask)
    ok = True
    RELS = [FatherOf, ParentOf, TeacherOf, AncestorOf]
    for r in RELS:
        for y in D:
            _, cond, _ = ask([atom(Leq, r, ParentOf), atom(Holds2, r, "Socrates", y)])
            if cond and not ask([atom(Holds2, AncestorOf, "Socrates", y)])[1]:
                ok = False
                break
        if not ok: break
    check(ok, "Universal property failed.")
    notes.append("PASS 4: Universal property verified.")

    # 5) Negative: TeacherOf does not entail AncestorOf
    check(not ask([atom(Holds2, AncestorOf, "Socrates", "Plato")])[1], "TeacherOf leaked into AncestorOf.")
    notes.append("PASS 5: Unrelated relation does not leak.")

    # 6) Auto-chooser picks bottom-up for AncestorOf(X,Y) and top-down for ground goal
    e1, _, _ = ask([atom(Holds2, AncestorOf, Var("X"), Var("Y"))])
    e2, _, _ = ask([atom(Holds2, AncestorOf, "Socrates", "Lamprocles")])
    check(e1=="bottomup" and e2=="topdown", "Auto engine choice not as expected.")
    notes.append("PASS 6: Engine chooser behaves as intended.")

    # 7) Deterministic formatting
    s1 = fmt_pairs(sorted(expected)); s2 = fmt_pairs(sorted(list(expected)))
    check(s1==s2, "Determinism of pretty-printer failed.")
    notes.append("PASS 7: Pretty-printing deterministic.")

    # 8) leq is reflexive for all relation names
    for r in [FatherOf,ParentOf,TeacherOf,AncestorOf]:
        check(ask([atom(Leq, r, r)])[1], f"Reflexivity of leq failed for {local(r)}.")
    notes.append("PASS 8: leq reflexivity holds for all relation-names.")

    # 9) leq_strict is transitive (spot-check)
    check(ask([atom(LeqStrict, FatherOf, AncestorOf)])[1], "leq_strict transitivity failed.")
    notes.append("PASS 9: leq_strict transitivity holds.")

    # 10) Top-down standardize-apart sanity (repeat success)
    ok1 = ask([atom(Holds2, AncestorOf, "Sophroniscus", "Lamprocles")])[1]
    ok2 = ask([atom(Holds2, AncestorOf, "Sophroniscus", "Lamprocles")])[1]
    check(ok1 and ok2, "Repeated top-down query should remain true.")
    notes.append("PASS 10: Standardize-apart avoids capture across proofs.")

    # 11) Bottom-up closure idempotence (two runs identical)
    f1,_ = bottomup_closure(); f2,_ = bottomup_closure()
    check(f1[Holds2]==f2[Holds2], "Bottom-up closure not idempotent.")
    notes.append("PASS 11: Bottom-up closure is stable.")

    # 12) Both engines return no AncestorOf reflexives
    for x in D:
        check(not ask([atom(Holds2, AncestorOf, x, x)])[1], "Unexpected reflexive ancestor derived.")
    notes.append("PASS 12: No reflexive AncestorOf facts.")

    return notes

# =========================================================
# Main
# =========================================================

def main() -> None:
    print_model()
    print_question()
    res1, res2, res3 = run_queries()
    print_answer(res1, res2, res3)
    print_reason(res1[1], res2[1])

    print("Check (harness)")
    print("===============")
    try:
        notes = run_checks()
    except CheckFailure as e:
        print("FAIL:", e)
        raise
    else:
        for n in notes: print(n)

if __name__ == "__main__":
    main()

