#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
README (plain text)
===================
Purpose
-------
This branch cleanly separates:
  • a reusable **ENGINE LIBRARY** (generic top-down SLD + generic bottom-up LFP), and
  • a concrete **CASE** that instantiates the Hayes–Menzel style (predicate *names*
    as intensions + fixed ex:holds2/3 application) with facts and rules.

You can extend or replace the CASE without touching the engines. The bottom-up engine
is generic Datalog (function-free) and supports “unsafe” rules like leq(P,P). via a
small signature that tells the engine which argument positions are relation-names vs
individuals (so it knows which domain to use when grounding head-only variables).

Where to edit
-------------
• Add or modify **facts** under:  >>> USER SECTION: FACTS
• Add or modify **rules** under:  >>> USER SECTION: RULES
• If you add new predicates, extend the **signature** in the CASE section so the
  bottom-up engine knows argument sorts.

What it prints
--------------
Model → Question → Answer (with chosen engine labels) → Reason why → Check (harness)

Run
---
    python3 holdsn_dual_engine.py

Deterministic output; no external dependencies.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Dict, Iterable, List, Optional, Set, Tuple, Union
from collections import defaultdict, deque
import itertools

# ╔═══════════════════════════════════════════════════════════════════════════╗
# ║                           ENGINE LIBRARY (generic)                        ║
# ╚═══════════════════════════════════════════════════════════════════════════╝

# ---------------------------
# Core data model (generic)
# ---------------------------

class Var:
    """Logic variable with identity; standardized-apart per rule application."""
    __slots__ = ("name","id")
    _c = 0
    def __init__(self, name: str):
        Var._c += 1
        self.name = name
        self.id = Var._c
    def __repr__(self): return f"Var({self.name}_{self.id})"
    def __hash__(self): return hash(self.id)
    def __eq__(self, o): return isinstance(o, Var) and self.id == o.id

Term = Union[str, Var]  # constants are plain strings

@dataclass
class Atom:
    pred: str
    args: List[Term]

@dataclass
class Clause:
    """Head :- Body1, Body2, ...   (Body=[] encodes a fact)."""
    head: Atom
    body: List[Atom]

def atom(pred: str, *args: Term) -> Atom: return Atom(pred, list(args))
def fact(pred: str, *args: Term) -> Clause: return Clause(atom(pred,*args), [])

# ---------------------------
# Unification utilities
# ---------------------------

def deref(t: Term, s: Dict[Var,Term]) -> Term:
    while isinstance(t, Var) and t in s: t = s[t]
    return t

def unify(t1: Term, t2: Term, s: Dict[Var,Term]) -> Optional[Dict[Var,Term]]:
    t1 = deref(t1,s); t2 = deref(t2,s)
    if t1 == t2: return s
    if isinstance(t1, Var): s[t1] = t2; return s
    if isinstance(t2, Var): s[t2] = t1; return s
    return None

def unify_lists(xs: List[Term], ys: List[Term], s: Dict[Var,Term]) -> Optional[Dict[Var,Term]]:
    if len(xs) != len(ys): return None
    for a,b in zip(xs,ys):
        s = unify(a,b,s)
        if s is None: return None
    return s

def standardize_apart(cl: Clause) -> Clause:
    """Freshen variables in a clause for each use (avoid capture)."""
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

# ---------------------------
# Top-down SLD (generic)
# ---------------------------

def index_by_pred(program: List[Clause]) -> Dict[str, List[Clause]]:
    idx: Dict[str, List[Clause]] = defaultdict(list)
    for c in program: idx[c.head.pred].append(c)
    return idx

def solve_topdown(program: List[Clause],
                  goals: List[Atom],
                  step_limit: int = 10000) -> Tuple[List[Dict[Var,Term]], int]:
    """Generic top-down SLD (depth-first)."""
    prog_by = index_by_pred(program)
    answers: List[Dict[Var,Term]] = []
    steps = 0
    def dfs(gs: List[Atom], s: Dict[Var,Term]) -> None:
        nonlocal steps
        if steps > step_limit: return
        if not gs: answers.append(s.copy()); return
        g = gs[0]
        for cl in prog_by.get(g.pred, []):
            c = standardize_apart(cl)
            steps += 1
            g1 = apply_s(g, s)
            h1 = apply_s(c.head, s)
            s2 = unify_lists(h1.args, g1.args, s.copy())
            if s2 is None: continue
            dfs(c.body + gs[1:], s2)
    dfs(goals, {})
    return answers, steps

# ---------------------------
# Bottom-up LFP (generic)
# ---------------------------
# We support “unsafe” rules (head-only variables) *if* the CASE provides a
# signature that marks argument positions as NAME (intension) or IND (individual).
# The engine uses these domains to ground head-only variables.

NAME, IND = "NAME", "IND"  # argument sorts

Signature = Dict[str, Tuple[str, ...]]  # pred -> tuple of sorts per argument

def _collect_domains(facts: Dict[str, Set[Tuple[str,...]]],
                     sig: Signature) -> Tuple[Set[str], Set[str]]:
    names: Set[str] = set()
    inds:  Set[str] = set()
    for p, rows in facts.items():
        if p not in sig: continue
        sorts = sig[p]
        for row in rows:
            for val, sort in zip(row, sorts):
                if sort == NAME: names.add(val)
                elif sort == IND: inds.add(val)
    return names, inds

def _ground_head_from_domains(head: Atom,
                              subst: Dict[Var,Term],
                              sig: Signature,
                              name_dom: Set[str],
                              ind_dom: Set[str]) -> Iterable[Tuple[str,...]]:
    """Given a head atom and a (possibly partial) substitution, yield ground tuples.
       For any head variable not bound by subst, iterate over the appropriate domain
       using the signature (NAME vs IND); enforce equalities if the same Var repeats."""
    sorts = sig.get(head.pred, tuple(NAME for _ in head.args))  # default NAME
    # Prepare per-position candidate domains or fixed constants
    positions: List[List[str]] = []
    # Track repeated variables to enforce equality
    var_to_pos: Dict[Var, int] = {}

    for i, (t, sort) in enumerate(zip(head.args, sorts)):
        t = deref(t, subst)
        if isinstance(t, str):
            positions.append([t])
        elif isinstance(t, Var):
            # choose domain by sort
            dom = name_dom if sort == NAME else ind_dom
            positions.append(sorted(dom))
            var_to_pos[t] = i
        else:
            raise RuntimeError("Unexpected term kind")

    # Iterate cartesian product and filter assignments that violate repeated-var equalities
    for combo in itertools.product(*positions):
        ok = True
        # enforce equality if same Var occurs multiple times in head
        seen: Dict[Var, str] = {}
        for i, (t, _sort) in enumerate(zip(head.args, sorts)):
            t = deref(t, subst)
            if isinstance(t, Var):
                if t in seen:
                    if seen[t] != combo[i]: ok = False; break
                else:
                    seen[t] = combo[i]
        if ok:
            yield tuple(combo)

def solve_bottomup(program: List[Clause], sig: Signature) -> Tuple[Dict[str, Set[Tuple[str,...]]], int]:
    """
    Generic bottom-up LFP:
      - Start from all EDB facts (empty body, fully ground).
      - Iterate rules; for each rule, join current facts for its body atoms.
      - For head-only variables (unsafe rules), ground them using NAME/IND domains
        inferred from current facts via the signature.
    Returns: (facts_by_pred, rounds)
    """
    facts: Dict[str, Set[Tuple[str,...]]] = defaultdict(set)

    # Seed explicit ground facts
    for cl in program:
        if not cl.body and all(isinstance(t, str) for t in cl.head.args):
            facts[cl.head.pred].add(tuple(cl.head.args))

    rounds = 0
    changed = True
    while changed:
        rounds += 1
        changed = False

        # Current domains from known facts
        name_dom, ind_dom = _collect_domains(facts, sig)

        # Apply each clause in program order for determinism
        for cl in program:
            if not cl.body:
                # If head has variables (unsafe fact like leq(P,P).), ground from domains
                if not all(isinstance(t, str) for t in cl.head.args):
                    for tpl in _ground_head_from_domains(cl.head, {}, sig, name_dom, ind_dom):
                        if tpl not in facts[cl.head.pred]:
                            facts[cl.head.pred].add(tpl); changed = True
                # else (fully ground fact) already seeded above
                continue

            # Evaluate body via joins over current facts
            partials: List[Dict[Var,Term]] = [dict()]
            safe_substs: List[Dict[Var,Term]] = []
            for b in cl.body:
                new_partials: List[Dict[Var,Term]] = []
                rows = facts.get(b.pred, set())
                for s in partials:
                    b1 = apply_s(b, s)
                    for row in rows:
                        if len(row) != len(b1.args): continue
                        s2 = s.copy()
                        ok = True
                        for arg, val in zip(b1.args, row):
                            s2 = unify(arg, val, s2)
                            if s2 is None: ok=False; break
                        if ok: new_partials.append(s2)
                partials = new_partials
                if not partials: break
            else:
                safe_substs = partials

            if not safe_substs:
                continue

            # For each substitution, produce head tuples; if head has extra vars, ground them from domains
            for s in safe_substs:
                head = apply_s(cl.head, s)
                if all(isinstance(t, str) for t in head.args):
                    tpl = tuple(head.args)  # fully ground head
                    if tpl not in facts[head.pred]:
                        facts[head.pred].add(tpl); changed = True
                else:
                    # Some head vars not bound by body: ground using signature domains
                    for tpl in _ground_head_from_domains(head, {}, sig, name_dom, ind_dom):
                        if tpl not in facts[head.pred]:
                            facts[head.pred].add(tpl); changed = True

    return facts, rounds

# ╔═══════════════════════════════════════════════════════════════════════════╗
# ║                           CASE: Hayes–Menzel demo                         ║
# ╚═══════════════════════════════════════════════════════════════════════════╝

# ------------------------------------------------------------
# Names (constants) and predicate symbols used in this *case*
# ------------------------------------------------------------

D: Tuple[str, ...] = (  # individuals, for printing & quantification in the harness
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

# -------------------------------
# Predicate signature (important)
# -------------------------------
# Tell the bottom-up engine which positions are relation-names (NAME) vs individuals (IND).
SIGNATURE: Signature = {
    Holds2:     (NAME, IND, IND),
    SubRelOf:   (NAME, NAME),
    LeqStrict:  (NAME, NAME),
    Leq:        (NAME, NAME),
}

# -----------------------------------
# Program (facts + rules) for the case
# -----------------------------------

PROGRAM: List[Clause] = []

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# USER SECTION: FACTS
# Base holds2 facts (extensions of named relations)
for a,b in [
    ("Sophroniscus","Socrates"),
    ("Socrates","Lamprocles"),
    ("Ariston","Plato"),
    ("Nicomachus","Aristotle"),
]:
    PROGRAM += [fact(Holds2, FatherOf, a, b),
                fact(Holds2, ParentOf, a, b)]
PROGRAM.append(fact(Holds2, TeacherOf, "Socrates", "Plato"))

# Inclusions over relation *names* (intensions)
PROGRAM += [
    fact(SubRelOf, FatherOf, ParentOf),    # FatherOf ⊆ ParentOf
    fact(SubRelOf, ParentOf, AncestorOf),  # ParentOf ⊆ AncestorOf
]
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# USER SECTION: RULES
# (Use Var("X") etc.; the engine will standardize-apart each use.)

# Inclusion closure over names
P,Q,R = Var("P"),Var("Q"),Var("R")
PROGRAM.append(Clause(atom(LeqStrict,P,Q), [atom(SubRelOf,P,Q)]))
P,Q,R = Var("P"),Var("Q"),Var("R")
PROGRAM.append(Clause(atom(LeqStrict,P,Q), [atom(SubRelOf,P,R), atom(LeqStrict,R,Q)]))

# Reflexive closure over names — “unsafe” but the engine grounds it from NAME-domain
P = Var("P")
PROGRAM.append(Clause(atom(Leq,P,P), []))  # will yield leq(n,n) for all relation-names n known so far

# Leq includes leq_strict
P,Q = Var("P"),Var("Q")
PROGRAM.append(Clause(atom(Leq,P,Q), [atom(LeqStrict,P,Q)]))

# AncestorOf via holds2
X,Y = Var("X"),Var("Y")
PROGRAM.append(Clause(atom(Holds2, AncestorOf, X, Y),
                      [atom(Holds2, ParentOf, X, Y)]))
X,Y,Z = Var("X"),Var("Y"),Var("Z")
PROGRAM.append(Clause(atom(Holds2, AncestorOf, X, Z),
                      [atom(Holds2, ParentOf, X, Y),
                       atom(Holds2, AncestorOf, Y, Z)]))

# Lifting along inclusion between relation-names
P,Q,X,Y = Var("P"),Var("Q"),Var("X"),Var("Y")
PROGRAM.append(Clause(atom(Holds2, Q, X, Y),
                      [atom(LeqStrict, P, Q), atom(Holds2, P, X, Y)]))
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# -------------------------
# Helpers for pretty output
# -------------------------

def local(name: str) -> str: return name.split(":",1)[1] if ":" in name else name

def fmt_pairs(pairs: Iterable[Tuple[str,str]]) -> str:
    seq = sorted(pairs)
    return "∅" if not seq else "{" + ", ".join(f"⟨{a},{b}⟩" for (a,b) in seq) + "}"

def fmt_set(names: Iterable[str]) -> str:
    seq = sorted(local(n) for n in set(names))
    return "∅" if not seq else "{" + ", ".join(seq) + "}"

# ------------------------------------
# Auto-chooser (case-specific heuristic)
# ------------------------------------

def is_var(t: Term) -> bool: return isinstance(t, Var)

def choose_engine(goals: List[Atom]) -> str:
    """
    Heuristic for this CASE:
      - If we see holds2(AncestorOf, X?, Y?) with variables → bottom-up (enumeration).
      - If any goal is all-variables (fully unbound) → bottom-up.
      - Otherwise → top-down.
    """
    for g in goals:
        if g.pred == Holds2 and len(g.args)==3 and g.args[0]==AncestorOf and (is_var(g.args[1]) or is_var(g.args[2])):
            return "bottomup"
        if all(is_var(t) for t in g.args):
            return "bottomup"
    return "topdown"

def ask(goals: List[Atom], step_limit: int = 10000, fallback_threshold: int = 4000) -> Tuple[str, List[Dict[Var,Term]], int]:
    engine = choose_engine(goals)
    if engine == "topdown":
        sols, steps = solve_topdown(PROGRAM, goals, step_limit=step_limit)
        if steps > fallback_threshold:
            # safety fallback for pathological cases
            facts, _ = solve_bottomup(PROGRAM, SIGNATURE)
            sols = _match_against_facts(goals, facts)
            engine = "bottomup"
            return engine, sols, 0
        return engine, sols, steps
    else:
        facts, rounds = solve_bottomup(PROGRAM, SIGNATURE)
        sols = _match_against_facts(goals, facts)
        return engine, sols, rounds

def _match_against_facts(goals: List[Atom], facts: Dict[str, Set[Tuple[str,...]]]) -> List[Dict[Var,Term]]:
    """Conjunctive query evaluation over ground facts (used for bottom-up answers)."""
    sols: List[Dict[Var,Term]] = [dict()]
    for a in goals:
        new_sols: List[Dict[Var,Term]] = []
        rows = facts.get(a.pred, set())
        for s in sols:
            for row in rows:
                if len(row) != len(a.args): continue
                s2 = s.copy()
                ok = True
                for arg, val in zip(a.args, row):
                    # unify var/const with ground value
                    if isinstance(arg, Var):
                        cur = deref(arg, s2)
                        if isinstance(cur, Var):
                            s2[cur] = val
                        else:
                            if cur != val: ok=False; break
                    else:
                        if arg != val: ok=False; break
                if ok: new_sols.append(s2)
        sols = new_sols
        if not sols: break
    return sols

# --------------------
# Branch: presentation
# --------------------

def print_model() -> None:
    print("Model")
    print("=====")
    print(f"Individuals D = {list(D)}\n")
    print("Fixed predicates (signature)")
    print("----------------------------")
    print("• ex:holds2(R,x,y)   — application (⟨x,y⟩ ∈ ext(R)); sorts: (NAME, IND, IND)")
    print("• ex:SubRelOf(P,Q)   — inclusion over relation *names*; sorts: (NAME, NAME)")
    print("• ex:leq_strict / ex:leq   — ⊆* with/without reflex on names; sorts: (NAME, NAME)\n")
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
    # Q1: enumerate AncestorOf pairs
    Xv, Yv = Var("X"), Var("Y")
    eng1, sols1, m1 = ask([atom(Holds2, AncestorOf, Xv, Yv)])
    anc_pairs = sorted({(deref(Xv,s), deref(Yv,s)) for s in sols1})  # type: ignore

    # Q2: witness relation-names R
    Rv = Var("R")
    eng2, sols2, m2 = ask([atom(Holds2, Rv, "Socrates", "Lamprocles"),
                           atom(Leq,   Rv, AncestorOf)])
    witnesses = sorted({deref(Rv,s) for s in sols2 if isinstance(deref(Rv,s), str)})

    # Q3: universal property check by enumeration per (R,y)
    ok = True
    for R in [FatherOf, ParentOf, TeacherOf, AncestorOf]:
        for y in D:
            _, cond, _ = ask([atom(Leq, R, ParentOf), atom(Holds2, R, "Socrates", y)])
            if cond:
                if not ask([atom(Holds2, AncestorOf, "Socrates", y)])[1]:
                    ok = False; break
        if not ok: break

    return (("Q1", eng1, anc_pairs, m1),
            ("Q2", eng2, witnesses, m2),
            ("Q3", "mixed", ok, 0))

def print_answer(res1, res2, res3) -> None:
    print("Answer")
    print("======")
    tag1, eng1, pairs, _ = res1
    tag2, eng2, wits, _  = res2
    tag3, eng3, ok, _    = res3
    print(f"{tag1}) Engine: {eng1} → AncestorOf =", fmt_pairs(pairs))
    print(f"{tag2}) Engine: {eng2} → Witness relation-names R = " + (fmt_set(wits) if wits else "∅"))
    print(f"{tag3}) Engine: {eng3} → Universal statement holds: {'Yes' if ok else 'No'}\n")

def print_reason(eng1, eng2) -> None:
    print("Reason why")
    print("==========")
    print("• Engines are generic; the CASE supplies only facts, rules, and a signature.")
    print("• Top-down SLD does goal-directed proof search with standardize-apart.")
    print("• Bottom-up LFP derives all consequences; the signature lets it safely")
    print("  ground head-only variables (e.g., leq(P,P).) from the correct DOMAINS:")
    print("  NAME-domain from name-positions; IND-domain from individual-positions.")
    print("• Auto-chooser (case-specific) sends enumerative AncestorOf(X,Y) to bottom-up,")
    print("  and bound/ground queries to top-down.\n")

# -------------------
# Check (harness)
# -------------------

class CheckFailure(AssertionError): pass
def check(c: bool, msg: str):
    if not c: raise CheckFailure(msg)

def run_checks() -> List[str]:
    notes: List[str] = []

    expected = {
        ("Sophroniscus","Socrates"),
        ("Sophroniscus","Lamprocles"),
        ("Socrates","Lamprocles"),
        ("Ariston","Plato"),
        ("Nicomachus","Aristotle"),
    }

    # 1) Bottom-up enumerates expected AncestorOf
    facts, _ = solve_bottomup(PROGRAM, SIGNATURE)
    X, Y = Var("X"), Var("Y")
    bu = _match_against_facts([atom(Holds2, AncestorOf, X, Y)], facts)
    anc_bu = {(deref(X, s), deref(Y, s)) for s in bu}
    check(anc_bu == expected, "Bottom-up AncestorOf enumeration mismatch.")
    notes.append("PASS 1: Bottom-up AncestorOf enumeration is correct.")

    # 2) Top-down enumerates the same AncestorOf
    td, _ = solve_topdown(PROGRAM, [atom(Holds2, AncestorOf, X, Y)])
    anc_td = {(deref(X, s), deref(Y, s)) for s in td}
    check(anc_td == expected, "Top-down AncestorOf enumeration mismatch.")
    notes.append("PASS 2: Top-down AncestorOf enumeration is correct.")

    # 3) Engines agree on existential witnesses
    R = Var("R")
    bu_w = _match_against_facts([atom(Holds2, R, "Socrates", "Lamprocles"),
                                 atom(Leq,   R, AncestorOf)], facts)
    td_w, _ = solve_topdown(PROGRAM, [atom(Holds2, R, "Socrates", "Lamprocles"),
                                      atom(Leq,   R, AncestorOf)])
    w1 = {deref(R, s) for s in bu_w}
    w2 = {deref(R, s) for s in td_w}
    check(w1 == w2 == {FatherOf, ParentOf, AncestorOf}, "Witness set mismatch.")
    notes.append("PASS 3: Engines agree on existential witnesses.")

    # 4) Universal property holds
    ok = True
    for r in [FatherOf, ParentOf, TeacherOf, AncestorOf]:
        for y in D:
            cond = ask([atom(Leq, r, ParentOf), atom(Holds2, r, "Socrates", y)])[1]
            if cond and not ask([atom(Holds2, AncestorOf, "Socrates", y)])[1]:
                ok = False; break
        if not ok: break
    check(ok, "Universal property failed.")
    notes.append("PASS 4: Universal property verified.")

    # 5) TeacherOf does not entail AncestorOf
    check(not ask([atom(Holds2, AncestorOf, "Socrates", "Plato")])[1],
          "TeacherOf leaked into AncestorOf.")
    notes.append("PASS 5: TeacherOf does not entail AncestorOf.")

    # 6) No reflexive AncestorOf
    for x in D:
        check(not ask([atom(Holds2, AncestorOf, x, x)])[1],
              "Unexpected reflexive AncestorOf fact.")
    notes.append("PASS 6: No reflexive AncestorOf facts.")

    # 7) Auto-chooser: bottom-up for enumerate; top-down for ground
    e1, _, _ = ask([atom(Holds2, AncestorOf, Var("X"), Var("Y"))])
    e2, _, _ = ask([atom(Holds2, AncestorOf, "Socrates", "Lamprocles")])
    check(e1 == "bottomup" and e2 == "topdown", "Engine chooser mismatch.")
    notes.append("PASS 7: Engine chooser behaves as intended.")

    # 8) leq reflexive for all relation names
    for r in [FatherOf, ParentOf, TeacherOf, AncestorOf]:
        check(ask([atom(Leq, r, r)])[1], f"Reflexivity of leq failed for {local(r)}.")
    notes.append("PASS 8: leq reflexivity holds.")

    # 9) leq_strict transitivity (FatherOf ⊆ AncestorOf via ParentOf)
    check(ask([atom(LeqStrict, FatherOf, AncestorOf)])[1],
          "leq_strict transitivity failed.")
    notes.append("PASS 9: leq_strict transitivity holds.")

    # 10) Standardize-apart: repeat top-down query is stable
    ok1 = ask([atom(Holds2, AncestorOf, "Sophroniscus", "Lamprocles")])[1]
    ok2 = ask([atom(Holds2, AncestorOf, "Sophroniscus", "Lamprocles")])[1]
    check(ok1 and ok2, "Repeated top-down query should remain true.")
    notes.append("PASS 10: Standardize-apart avoids capture across proofs.")

    # 11) Bottom-up closure idempotence
    f1, _ = solve_bottomup(PROGRAM, SIGNATURE)
    f2, _ = solve_bottomup(PROGRAM, SIGNATURE)
    check(f1[Holds2] == f2[Holds2], "Bottom-up closure not idempotent.")
    notes.append("PASS 11: Bottom-up closure is stable.")

    # 12) Pretty-printer determinism
    s1 = fmt_pairs(sorted(expected))
    s2 = fmt_pairs(sorted(list(expected)))
    check(s1 == s2, "Pretty-printer determinism failed.")
    notes.append("PASS 12: Pretty printing deterministic.")

    return notes

# -------------------
# Main branch runner
# -------------------

def main() -> None:
    print_model()       # prints "Model" + contents
    print_question()    # prints "Question" + contents
    res1, res2, res3 = run_queries()
    print_answer(res1, res2, res3)    # prints "Answer" + contents
    print_reason(res1[1], res2[1])    # prints "Reason why" + contents

    print("Check (harness)")
    print("===============")
    try:
        for note in run_checks():
            print(note)
    except CheckFailure as e:
        print("FAIL:", e)
        raise

if __name__ == "__main__":
    main()

