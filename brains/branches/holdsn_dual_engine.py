#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
README (plain text)
===================
Name
----
holdsn_dual_engine.py

Purpose
-------
Generic reasoning engine + runner that separates:
  • ENGINE LIBRARY: generic top-down SLD, generic bottom-up LFP with signatures,
    unification, standardize-apart, and utilities.
  • CASE MODULE: a separate file that declares facts, rules, signature, and
    presentation (Model → Question → Answer → Reason → Check).

Usage
-----
  python holdsn_dual_engine.py bus/greek_family.py
  python holdsn_dual_engine.py             # defaults to bus/greek_family.py

How a CASE module should look
-----------------------------
It should `from holdsn_dual_engine import ...` and define:

  D: Tuple[str,...]                 # individuals (for printing / quantification)
  SIGNATURE: Dict[str, Tuple[str,...]]  # NAME/IND sorts per predicate argument
  PROGRAM: List[Clause]             # facts + rules (using Var/Atom/Clause)

  # Presentation + execution hooks:
  print_model() -> None
  print_question() -> None
  run_queries() -> Tuple[Any, Any, Any]          # returns tuples like (Q1, engine, answer, metric)
  print_answer(res1, res2, res3) -> None
  print_reason(eng1, eng2) -> None
  run_checks() -> List[str]         # returns list of "PASS ..." notes, raises on failure

The CASE can use:
  Var, Atom, Clause, atom, fact,
  solve_topdown, solve_bottomup, match_against_facts,
  NAME, IND, Signature, deref,
  local, fmt_pairs, fmt_set
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Dict, Iterable, List, Optional, Set, Tuple, Union
from collections import defaultdict
import importlib.util, sys, os, itertools

# ---------------------------------------------------------------------------
# Export our module under the expected name so a CASE can `import holdsn_dual_engine`
# even when this file is running as __main__.
# ---------------------------------------------------------------------------
sys.modules['holdsn_dual_engine'] = sys.modules[__name__]

# ╔═══════════════════════════════════════════════════════════════════════════╗
# ║                           ENGINE LIBRARY (generic)                        ║
# ╚═══════════════════════════════════════════════════════════════════════════╝

# ---------------------------
# Core data model
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

NAME, IND = "NAME", "IND"  # argument sorts
Signature = Dict[str, Tuple[str, ...]]  # pred -> sorts per argument

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
    """Yield ground head tuples; for head-only vars, range over NAME/IND domains."""
    sorts = sig.get(head.pred, tuple(NAME for _ in head.args))  # default NAME
    positions: List[List[str]] = []
    seen: Dict[Var, str] = {}
    for t, sort in zip(head.args, sorts):
        t = deref(t, subst)
        if isinstance(t, str):
            positions.append([t])
        else:
            dom = name_dom if sort == NAME else ind_dom
            positions.append(sorted(dom))
    for combo in itertools.product(*positions):
        ok = True
        # enforce equality of repeated head vars
        i = 0
        for t, _sort in zip(head.args, sorts):
            t = deref(t, subst)
            if isinstance(t, Var):
                if t in seen and seen[t] != combo[i]:
                    ok = False; break
                seen[t] = combo[i]
            i += 1
        if ok:
            yield tuple(combo)

def solve_bottomup(program: List[Clause], sig: Signature) -> Tuple[Dict[str, Set[Tuple[str,...]]], int]:
    """
    Generic bottom-up LFP:
      - EDB = facts with empty body & fully ground head (seed).
      - Iterate rules; join body against current facts.
      - Head-only vars grounded from NAME/IND domains via signature.
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
        name_dom, ind_dom = _collect_domains(facts, sig)

        for cl in program:
            if not cl.body:
                # Unsafe fact (e.g., leq(P,P).) → ground from domains
                if not all(isinstance(t, str) for t in cl.head.args):
                    for tpl in _ground_head_from_domains(cl.head, {}, sig, name_dom, ind_dom):
                        if tpl not in facts[cl.head.pred]:
                            facts[cl.head.pred].add(tpl); changed = True
                continue

            # Conjunctive join over current facts for the body
            partials: List[Dict[Var,Term]] = [dict()]
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

            if not partials:
                continue

            # Produce head tuples
            for s in partials:
                head = apply_s(cl.head, s)
                if all(isinstance(t, str) for t in head.args):
                    tpl = tuple(head.args)
                    if tpl not in facts[head.pred]:
                        facts[head.pred].add(tpl); changed = True
                else:
                    for tpl in _ground_head_from_domains(head, {}, sig, name_dom, ind_dom):
                        if tpl not in facts[head.pred]:
                            facts[head.pred].add(tpl); changed = True

    return facts, rounds

# ---------------------------
# Utility helpers (exported)
# ---------------------------

def match_against_facts(goals: List[Atom], facts: Dict[str, Set[Tuple[str,...]]]) -> List[Dict[Var,Term]]:
    """Conjunctive query evaluation over ground facts (used after bottom-up)."""
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
                    if isinstance(arg, Var):
                        cur = deref(arg, s2)
                        if isinstance(cur, Var): s2[cur] = val
                        else:
                            if cur != val: ok=False; break
                    else:
                        if arg != val: ok=False; break
                if ok: new_sols.append(s2)
        sols = new_sols
        if not sols: break
    return sols

def local(name: str) -> str: return name.split(":",1)[1] if ":" in name else name

def fmt_pairs(pairs: Iterable[Tuple[str,str]]) -> str:
    seq = sorted(pairs)
    return "∅" if not seq else "{" + ", ".join(f"⟨{a},{b}⟩" for (a,b) in seq) + "}"

def fmt_set(names: Iterable[str]) -> str:
    seq = sorted(local(n) for n in set(names))
    return "∅" if not seq else "{" + ", ".join(seq) + "}"

# ╔═══════════════════════════════════════════════════════════════════════════╗
# ║                                 RUNNER                                   ║
# ╚═══════════════════════════════════════════════════════════════════════════╝

def _load_case_module(path: str):
    spec = importlib.util.spec_from_file_location("case_module", path)
    if spec is None or spec.loader is None:
        raise RuntimeError(f"Cannot load case module from {path}")
    mod = importlib.util.module_from_spec(spec)
    # Make engine imports available during module execution
    sys.modules['holdsn_dual_engine'] = sys.modules[__name__]
    spec.loader.exec_module(mod)  # type: ignore
    return mod

def main():
    # Default case path: bus/greek_family.py (relative to this script)
    script_dir = os.path.dirname(os.path.abspath(__file__))
    default_case = os.path.join(script_dir, "bus", "greek_family.py")

    case_path = sys.argv[1] if len(sys.argv) > 1 else default_case
    if not os.path.isabs(case_path):
        case_path = os.path.join(script_dir, case_path)

    case = _load_case_module(case_path)

    # Orchestration — the CASE prints its own headers
    case.print_model()
    case.print_question()
    res1, res2, res3 = case.run_queries()
    case.print_answer(res1, res2, res3)
    case.print_reason(res1[1], res2[1])

    print("Check (harness)")
    print("===============")
    try:
        for note in case.run_checks():
            print(note)
    except Exception as e:
        print("FAIL:", e)
        raise

if __name__ == "__main__":
    main()

