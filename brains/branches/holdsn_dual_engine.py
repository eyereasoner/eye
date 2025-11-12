#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
README
======
Purpose
-------
Generic reasoning engine + runner that separates:
  • ENGINE LIBRARY:
      - Top-down **tabled** evaluation (SLG-style via relevant-subprogram LFP),
      - Bottom-up LFP with NAME/IND signatures (supports unsafe rules like leq(P,P).),
      - Unification, standardize-apart, utilities.
  • CASE MODULE:
      A separate file that declares facts, rules, signature, and presentation
      (Model → Question → Answer → Reason → Check).

Why “tabling via relevant LFP”?
-------------------------------
For function-free Horn programs (Datalog), SLG tabling for a query q returns the
least fixpoint of the **relevant** subprogram reachable from q’s predicates.
We implement tabling by:
  1) finding the predicate dependency closure from the query (relevant preds),
  2) computing a bottom-up LFP **only on that subprogram**,
  3) answering the query by conjunctive matching over those tabled facts.

This yields SLG-like termination on recursion/left recursion, without the
complexity of explicit subgoal tables and answer resolution.

IMPORTANT nuance (unsafe rules like leq(P,P).)
----------------------------------------------
Rules with *head-only variables* (a.k.a. unsafe) need a domain to ground from.
We use the predicate signature (NAME/IND) plus a **seed domain** collected from:
  • ALL ground facts in the whole program (not only the relevant subprogram), and
  • The constants that appear in the *query goals*.
This guarantees that head-only variables like P in leq(P,P). range over all
relation *names* that your case actually uses (e.g., TeacherOf), even when the
relevant subprogram itself does not mention them.

Usage
-----
  python holdsn_dual_engine.py greek_family.py
  python holdsn_dual_engine.py                 # defaults to ./greek_family.py

CASE module contract
--------------------
A CASE module should import from this file:

  from holdsn_dual_engine import (
      Var, Atom, Clause, atom, fact,
      solve_topdown, solve_bottomup, match_against_facts,
      NAME, IND, Signature, deref,
      local, fmt_pairs, fmt_set,
  )

and define:

  D: Tuple[str,...]
  SIGNATURE: Dict[str, Tuple[str,...]]
  PROGRAM: List[Clause]

  # Presentation + execution hooks:
  print_model() -> None
  print_question() -> None
  run_queries() -> Tuple[Any, Any, Any]
  print_answer(res1, res2, res3) -> None
  print_reason(eng1, eng2) -> None
  run_checks() -> List[str]   # raise on failure

Deterministic output; no external dependencies.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Dict, Iterable, List, Optional, Set, Tuple, Union
from collections import defaultdict, deque
import importlib.util, sys, os, itertools, inspect

# Make this module importable as 'holdsn_dual_engine' even when run as __main__.
sys.modules['holdsn_dual_engine'] = sys.modules[__name__]

# ╔═══════════════════════════════════════════════════════════════════════════╗
# ║                           ENGINE LIBRARY (generic)                        ║
# ╚═══════════════════════════════════════════════════════════════════════════╝

# ----------------------------------------------------------------------------
# Core data model
# ----------------------------------------------------------------------------

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

# ----------------------------------------------------------------------------
# Unification utilities
# ----------------------------------------------------------------------------

def deref(t: Term, s: Dict[Var,Term]) -> Term:
    """Dereference a term through a substitution map s."""
    while isinstance(t, Var) and t in s: t = s[t]
    return t

def unify(t1: Term, t2: Term, s: Dict[Var,Term]) -> Optional[Dict[Var,Term]]:
    """Unify two terms under s; returns extended s or None if clash."""
    t1 = deref(t1,s); t2 = deref(t2,s)
    if t1 == t2: return s
    if isinstance(t1, Var): s[t1] = t2; return s
    if isinstance(t2, Var): s[t2] = t1; return s
    return None

def unify_lists(xs: List[Term], ys: List[Term], s: Dict[Var,Term]) -> Optional[Dict[Var,Term]]:
    """Pairwise unification for argument lists."""
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
    """Apply a substitution to an atom."""
    return Atom(a.pred, [deref(t,s) for t in a.args])

# ----------------------------------------------------------------------------
# Bottom-up LFP (generic)
# ----------------------------------------------------------------------------

NAME, IND = "NAME", "IND"  # argument sorts
Signature = Dict[str, Tuple[str, ...]]  # pred -> sorts per argument

def _collect_domains(facts: Dict[str, Set[Tuple[str,...]]],
                     sig: Signature) -> Tuple[Set[str], Set[str]]:
    """
    Compute NAME/IND domains from *current* facts, guided by the signature.
    Only ground facts contribute.
    """
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
    """
    Yield ground head tuples. For head-only vars, range over NAME/IND domains.

    IMPORTANT: equality between repeated head variables is enforced *per tuple*.
    """
    sorts = sig.get(head.pred, tuple(NAME for _ in head.args))  # default NAME
    positions: List[List[str]] = []

    # Candidate constants for each head position (constants fixed; vars get a domain)
    for t, sort in zip(head.args, sorts):
        t = deref(t, subst)
        if isinstance(t, str):
            positions.append([t])
        else:
            dom = name_dom if sort == NAME else ind_dom
            positions.append(sorted(dom))

    # Iterate products; enforce equality of repeated head variables PER COMBINATION
    for combo in itertools.product(*positions):
        seen: Dict[Var, str] = {}   # IMPORTANT: per-combination
        ok = True
        for i, (t, _sort) in enumerate(zip(head.args, sorts)):
            t = deref(t, subst)
            if isinstance(t, Var):
                prev = seen.get(t)
                if prev is None:
                    seen[t] = combo[i]
                elif prev != combo[i]:
                    ok = False; break
        if ok:
            yield tuple(combo)

def solve_bottomup(program: List[Clause],
                   sig: Signature,
                   seed_name_domain: Optional[Set[str]] = None,
                   seed_ind_domain: Optional[Set[str]] = None) -> Tuple[Dict[str, Set[Tuple[str,...]]], int]:
    """
    Generic bottom-up LFP:
      - EDB = facts with empty body & fully ground head (seed).
      - Iterate rules; join body against current facts.
      - Head-only vars grounded from NAME/IND domains via signature.
    Seed domains (if provided) are UNIONed into the computed domains each round,
    so unsafe heads can still range over all intended constants.
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

        # Current domains + optional seeds
        names, inds = _collect_domains(facts, sig)
        if seed_name_domain: names |= set(seed_name_domain)
        if seed_ind_domain:  inds  |= set(seed_ind_domain)

        for cl in program:
            if not cl.body:
                # Unsafe fact (e.g., leq(P,P).) → ground from domains
                if not all(isinstance(t, str) for t in cl.head.args):
                    for tpl in _ground_head_from_domains(cl.head, {}, sig, names, inds):
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
                    for tpl in _ground_head_from_domains(head, {}, sig, names, inds):
                        if tpl not in facts[head.pred]:
                            facts[head.pred].add(tpl); changed = True

    return facts, rounds

# ----------------------------------------------------------------------------
# Utilities
# ----------------------------------------------------------------------------

def match_against_facts(goals: List[Atom], facts: Dict[str, Set[Tuple[str,...]]]) -> List[Dict[Var,Term]]:
    """Conjunctive query evaluation over ground facts (used after LFP)."""
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

# ----------------------------------------------------------------------------
# Top-down with tabling (variant via relevant-subprogram LFP)
# ----------------------------------------------------------------------------

def _dependency_graph(program: List[Clause]) -> Dict[str, Set[str]]:
    """Predicate dependency graph: head.pred -> set(body.pred)."""
    g: Dict[str, Set[str]] = defaultdict(set)
    for c in program:
        h = c.head.pred
        for b in c.body:
            g[h].add(b.pred)
        g.setdefault(h, g.get(h, set()))  # ensure key exists
    return g

def _reachable_preds(program: List[Clause], goal_preds: Set[str]) -> Set[str]:
    """BFS over predicate dependency graph starting from goal predicates."""
    g = _dependency_graph(program)
    reach: Set[str] = set()
    q = deque(goal_preds)
    while q:
        p = q.popleft()
        if p in reach: continue
        reach.add(p)
        for q2 in g.get(p, set()):
            if q2 not in reach: q.append(q2)
    return reach

def _filter_program(program: List[Clause], rel_preds: Set[str]) -> List[Clause]:
    """Keep only clauses whose head predicate is in rel_preds."""
    return [c for c in program if c.head.pred in rel_preds]

def _collect_seed_domains_from_program(program: List[Clause],
                                       sig: Signature) -> Tuple[Set[str], Set[str]]:
    """
    Scan ALL ground facts in the ORIGINAL program (not just the relevant subprogram)
    and collect NAME/IND constants according to the signature.
    This supplies the seed domain for unsafe heads in the tabled LFP.
    """
    names: Set[str] = set()
    inds:  Set[str] = set()
    for cl in program:
        if cl.body:  # only facts here
            continue
        head = cl.head
        if head.pred not in sig:
            continue
        sorts = sig[head.pred]
        # Only ground heads contribute (EDB)
        if any(isinstance(t, Var) for t in head.args):
            continue
        for val, sort in zip(head.args, sorts):
            if isinstance(val, str):
                if sort == NAME: names.add(val)
                elif sort == IND: inds.add(val)
    return names, inds

def _collect_seed_domains_from_goals(goals: List[Atom],
                                     sig: Signature) -> Tuple[Set[str], Set[str]]:
    """Collect NAME/IND constants from the query goals themselves (ground positions)."""
    names: Set[str] = set()
    inds:  Set[str] = set()
    for g in goals:
        sorts = sig.get(g.pred)
        if not sorts:  # unknown signature → skip
            continue
        for val, sort in zip(g.args, sorts):
            if isinstance(val, str):
                if sort == NAME: names.add(val)
                elif sort == IND: inds.add(val)
    return names, inds

def solve_topdown(program: List[Clause],
                  goals: List[Atom],
                  step_limit: int = 10000) -> Tuple[List[Dict[Var,Term]], int]:
    """
    Top-down with tabling (variant):
      1) Compute predicate dependency closure reachable from the goal predicates.
      2) Compute an LFP on this **relevant subprogram**, using a seed NAME/IND
         domain from the WHOLE program + the goals (so unsafe heads like leq(P,P). see
         all relation names, e.g., TeacherOf).
      3) Answer the conjunctive query by matching against the tabled facts.

    Returns: (solutions, metric) where metric is the number of LFP rounds.
    """
    # 0) Obtain signature from the caller's module (CASE). If missing, default NAMEs.
    sig: Signature = {}
    try:
        caller_globals = inspect.currentframe().f_back.f_globals  # type: ignore
        if 'SIGNATURE' in caller_globals:
            sig = caller_globals['SIGNATURE']  # type: ignore
    except Exception:
        pass

    # If signature is empty, default all predicate positions to NAME (best effort).
    if not sig:
        preds = {c.head.pred for c in program}
        for p in preds:
            # guess arity from first clause with predicate p
            arity = len(next(c for c in program if c.head.pred==p).head.args)
            sig[p] = tuple(NAME for _ in range(arity))  # type: ignore

    # 1) Relevant subprogram
    goal_preds = {g.pred for g in goals}
    rel = _reachable_preds(program, goal_preds)
    subprog = _filter_program(program, rel)

    # 2) Seed NAME/IND domains from the WHOLE program + the GOALS
    names_prog, inds_prog   = _collect_seed_domains_from_program(program, sig)
    names_goals, inds_goals = _collect_seed_domains_from_goals(goals, sig)
    seed_names = names_prog | names_goals
    seed_inds  = inds_prog  | inds_goals

    # Compute tabled facts for relevant preds only, using the seeded domains
    facts, rounds = solve_bottomup(subprog, sig,
                                   seed_name_domain=seed_names,
                                   seed_ind_domain=seed_inds)

    # 3) Conjunctive answer over tabled facts
    sols = match_against_facts(goals, facts)
    return sols, rounds

# ╔═══════════════════════════════════════════════════════════════════════════╗
# ║                                   RUNNER                                 ║
# ╚═══════════════════════════════════════════════════════════════════════════╝

def _load_case_module(path: str):
    """Load a CASE module from a file path so it can import this engine."""
    spec = importlib.util.spec_from_file_location("case_module", path)
    if spec is None or spec.loader is None:
        raise RuntimeError(f"Cannot load case module from {path}")
    mod = importlib.util.module_from_spec(spec)
    # Make engine imports available during module execution.
    sys.modules['holdsn_dual_engine'] = sys.modules[__name__]
    spec.loader.exec_module(mod)  # type: ignore
    return mod

def main():
    # Default case path: ./greek_family.py (sibling of this file)
    script_dir = os.path.dirname(os.path.abspath(__file__))
    default_case = os.path.join(script_dir, "greek_family.py")

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

