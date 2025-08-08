#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
===============================================================================
Horn Reasoner (Iterative, Goal-Oriented, with Proofs + Memoization)
===============================================================================

WHAT THIS IS
------------
A tiny but robust Horn-clause backward-chaining reasoner that:
  • evaluates queries using *iterative* SLD resolution (no Python recursion),
  • supports a few arithmetic built-ins: geq/2, sub/3, add/3,
  • prints a human-readable, goal-oriented proof tree,
  • standardizes variables apart on every rule application,
  • uses memoization for fib/2 (and supports optional pre-seeding for speed).

It’s intentionally small and hackable so you can adapt it to your own demos.

WHY ITERATIVE?
--------------
Earlier naive implementations tend to blow the Python call stack (especially
with deep recursion like fibonacci). This engine keeps its own explicit stack
of “states” (choice points), so there’s no Python recursion in the search or
proof printing. That makes it stable for large or deep trees.

HORN LOGIC TL;DR
----------------
A Horn clause is either a fact or a rule:
  • fact:    head.
  • rule:    head :- body1, body2, ..., bodyK.

To *prove a query*, backward-chaining turns the query into a goal list and
tries to solve it by repeatedly selecting a goal and matching it against rule
heads, pushing the rule’s body goals in place of the selected goal.

DATA MODEL (TERMS, RULES, KB)
-----------------------------
Terms
  • Const(value)         : ground value (int, str, etc.)
  • Var(name, id)        : a logic variable; *id* is a globally unique integer
                           used as the identity in substitutions.
  • Struct(functor,args) : a predicate term, e.g., Struct("human", (Const("socrates"),))

Rules & KB
  • Rule(head, body)     : body is a tuple of Struct or Builtin goals.
  • KB(rules)            : ordered list of rules (order matters for DFS).

Standardize-Apart
  • Every time a rule is chosen, we clone it and assign **fresh variable ids**
    to avoid variable collisions between independent uses of the rule.

SUBSTITUTIONS & UNIFICATION
---------------------------
Substitution s : Dict[var_id -> term]. We never bind by name—only by *id*.
  • walk(t, s)  : iteratively dereference variables (with path compression).
  • unify(a,b,s): iterative Robinson unification; mutates/returns s or None.
Notes:
  • No occurs-check (fine for these demos, simpler/faster).
  • Path compression makes dereferencing near O(1) amortized.

BUILT-INS
---------
A Builtin goal wraps a Python function that:
  • inspects its (possibly partially instantiated) args,
  • yields **delta substitutions** of the form {var_id: term}.
We then merge each delta into the current substitution via unify().

Provided built-ins:
  • geq(X,Y)    : succeeds when both are ground integers and X >= Y.
  • sub(X,Y,Z)  : Z = X - Y (can bind Z if variable; checks consistency if const).
  • add(X,Y,Z)  : Z = X + Y (same behavior for Z).

PROOF TREE
----------
We build a tree of ProofNode objects as the search proceeds:
  • Every selected goal becomes a node.
  • If it matches a rule, we annotate the node with the rule and the delta θ.
  • Built-ins show as “Builtin: name(args)”.
  • Success leaves print “✓ success”.
We also provide an *iterative* printer to avoid recursion depth issues.

SEARCH ENGINE (ITERATIVE SLD)
-----------------------------
We maintain an explicit stack of State(goals, subst, parent_node, depth).
Loop:
  1) Pop a state:
     - If goals empty → success: emit a solution & bubble success markers.
  2) Take the first goal:
     - If Builtin: run it, push continuations for each yielded delta.
     - If Struct:
         (a) Try memo hit (for fib/2): if found, push continuation and
             **skip expanding rules** (prioritized instant answer).
         (b) Otherwise, for each matching rule head:
               - standardize-apart the rule,
               - unify the selected goal with the rule head,
               - push a new state with the rule body prepended.
Notes:
  • DFS order follows the order of rules in KB.
  • MAX_DEPTH guards against runaway search (mostly for debugging).

FIBONACCI, MEMOIZATION & PRE-SEEDING
------------------------------------
Naive fib via Horn rules is exponential on the *first* encounter. Memoization
makes *subsequent* calls instant, but the *first* run still explodes.

We provide:
  • A simple memo table fib_table: Dict[int, int].
  • A “memo hit” shortcut for fib/2 that is **prioritized** (skip rule expansion).
  • preseed_fib(N): optional DP fill of fib_table[0..N] to make big queries
    (e.g., fib(30,_)) instant on the first try, while keeping rules for small
    proofs (e.g., fib(6,_)).

DEMO KNOWLEDGE BASE
-------------------
1) Socrates syllogism
   human('socrates').
   mortal(X) :- human(X).

2) Fibonacci
   fib(0, 0).
   fib(1, 1).
   fib(N,F) :- geq(N,2),
               sub(N,1,N1), sub(N,2,N2),
               fib(N1,F1), fib(N2,F2),
               add(F1,F2,F).

USAGE
-----
Run the file directly. It:
  • Pre-seeds fib up to 30.
  • Proves mortal('socrates').
  • Proves fib(6, F) with a full proof (F = 8).
  • Answers fib(30, Fbig) instantly via memo hit.

CONFIG KNOBS
------------
  TRACE       : set True to see built-in calls & simple tracing.
  MAX_DEPTH   : depth guard for the iterative search (safety during debugging).

EXTENDING THE SYSTEM
--------------------
Add facts/rules:
  • Create Structs and append Rule(...) objects to the KB’s rules list.
  • Rule bodies can include Structs (predicates) and Builtins.

Add new built-ins:
  • Write a function fn(args, subst) -> iterable of {var_id: term} dicts.
  • Wrap with Builtin(name, args, fn) and use in rule bodies.
  • Always bind the **actual variables** from args: look them up, walk() them,
    and return {that_var.id: Const(...)} deltas.

Change goal selection or search strategy:
  • This engine is deterministic left-to-right DFS. You can implement a different
    scheduler by changing how states are pushed/popped (stack → queue for BFS).

LIMITATIONS & NOTES
-------------------
  • No occurs-check. For typical Horn demos this is fine; add it if you intend
    to construct cyclic terms.
  • No cut (!), no negation-as-failure, no disjunction in heads.
  • True *tabled* evaluation (SLG/OLDT) is not implemented here; for general
    recursive programs with heavy mutual recursion, consider adding a producer/
    consumer worklist and answer memo sets per subgoal variant.
  • Rule order matters (DFS). Put base facts before recursive rules when that
    helps pruning or readability.

TROUBLESHOOTING
---------------
  • “Runs forever” on fib(30,_):
      - Make sure preseed_fib(30) is called before the query.
      - Ensure the memo-hit path **continues** without also expanding rules.
  • “No solutions”:
      - Check that your goal functor/arity matches a rule head.
      - Ensure built-ins are binding the *original* variables (by id).
  • “RecursionError”:
      - This engine avoids Python recursion, so that shouldn’t happen; if you
        see it, it likely comes from an external print or logging recursion.

CREDITS
-------
  • Classic logic programming ideas (SLD resolution, unification).
  • Implementation choices (iterative unification, path compression, standardize
    apart, memo hit short-circuit) are tailored to keep the demo simple,
    predictable, and easy to extend.
"""

from __future__ import annotations
from dataclasses import dataclass, field
from typing import Any, Dict, Iterable, List, Optional, Tuple, Union
import itertools

# ===========================
# Config
# ===========================

TRACE = False        # set True to see built-in activity
MAX_DEPTH = 200000   # safety cap for search depth

def tprint(*a, **k):
    if TRACE:
        print(*a, **k)

# ===========================
# Terms
# ===========================

VarId = int

@dataclass(frozen=True)
class Var:
    """Logic variable (human name + unique id)."""
    name: str
    id: VarId

def V(name: str, counter: itertools.count) -> Var:
    return Var(name, next(counter))

@dataclass(frozen=True)
class Const:
    value: Any

@dataclass(frozen=True)
class Struct:
    functor: str
    args: Tuple['Term', ...] = field(default_factory=tuple)

Term = Union[Var, Const, Struct]
Subst = Dict[VarId, Term]  # variable-id -> term

def is_var(t: Term) -> bool: return isinstance(t, Var)
def is_const(t: Term) -> bool: return isinstance(t, Const)
def is_struct(t: Term) -> bool: return isinstance(t, Struct)

def show(t: Term) -> str:
    if isinstance(t, Var):   return f"{t.name}_{t.id}"
    if isinstance(t, Const): return repr(t.value) if isinstance(t.value, str) else str(t.value)
    if isinstance(t, Struct):
        if not t.args: return t.functor
        return f"{t.functor}(" + ", ".join(show(a) for a in t.args) + ")"
    return str(t)

# ===========================
# Dereferencing & Unification (iterative)
# ===========================

def walk(t: Term, s: Subst) -> Term:
    """
    Iteratively follow variable links in s until a non-variable (or unbound var)
    is reached. Path compression flattens chains. Cycle-safe.
    """
    path: List[VarId] = []
    while isinstance(t, Var) and t.id in s:
        nxt = s[t.id]
        if isinstance(nxt, Var) and nxt.id == t.id:  # self-loop guard
            break
        path.append(t.id)
        t = nxt
        if isinstance(t, Var) and t.id in path:      # cycle guard
            break
    for vid in path:
        s[vid] = t
    return t

def unify(a: Term, b: Term, s: Subst) -> Optional[Subst]:
    """
    Iterative Robinson unification (no occurs-check, fine for this demo).
    Mutates/returns `s` on success, or None on failure.
    """
    stack: List[Tuple[Term, Term]] = [(a, b)]
    while stack:
        x, y = stack.pop()
        x = walk(x, s); y = walk(y, s)
        if x is y:
            continue
        if is_var(x):
            s[x.id] = y
            continue
        if is_var(y):
            s[y.id] = x
            continue
        if is_const(x) and is_const(y):
            if x.value != y.value:
                return None
            continue
        if is_struct(x) and is_struct(y) and x.functor == y.functor and len(x.args) == len(y.args):
            for xa, ya in zip(x.args, y.args):
                stack.append((xa, ya))
            continue
        return None
    return s

# ===========================
# Built-ins: geq/2, sub/3, add/3
# ===========================

def expect_int(t: Term, s: Subst) -> Optional[int]:
    tw = walk(t, s)
    return tw.value if isinstance(tw, Const) and isinstance(tw.value, int) else None

@dataclass(frozen=True)
class Builtin:
    """Built-in goal wrapper; fn returns zero or more delta bindings {var_id: term}."""
    name: str
    args: Tuple[Term, ...]
    fn: Any  # (args, subst) -> Iterable[Dict[VarId, Term]]

def bi_geq(args: Tuple[Term, ...], s: Subst) -> Iterable[Dict[VarId, Term]]:
    X, Y = args
    x = expect_int(X, s); y = expect_int(Y, s)
    tprint(f"[geq] {x} >= {y}")
    if x is not None and y is not None and x >= y:
        tprint("[geq] ✓")
        yield {}

def bi_sub(args: Tuple[Term, ...], s: Subst) -> Iterable[Dict[VarId, Term]]:
    X, Y, Z = args
    x = expect_int(X, s); y = expect_int(Y, s)
    Zw = walk(Z, s)
    tprint(f"[sub] {x} - {y} -> Z={show(Zw)}")
    if x is None or y is None:
        tprint("[sub] ✗ non-ground")
        return
    target = Const(x - y)
    if isinstance(Zw, Var):
        tprint(f"[sub] bind {show(Zw)} = {target.value}")
        yield {Zw.id: target}
    elif isinstance(Zw, Const) and Zw.value == target.value:
        tprint("[sub] ✓ consistent")
        yield {}

def bi_add(args: Tuple[Term, ...], s: Subst) -> Iterable[Dict[VarId, Term]]:
    X, Y, Z = args
    x = expect_int(X, s); y = expect_int(Y, s)
    Zw = walk(Z, s)
    tprint(f"[add] {x} + {y} -> Z={show(Zw)}")
    if x is None or y is None:
        tprint("[add] ✗ non-ground")
        return
    target = Const(x + y)
    if isinstance(Zw, Var):
        tprint(f"[add] bind {show(Zw)} = {target.value}")
        yield {Zw.id: target}
    elif isinstance(Zw, Const) and Zw.value == target.value:
        tprint("[add] ✓ consistent")
        yield {}

def p_geq(x,y):    return Builtin("geq", (x,y), bi_geq)
def p_sub(x,y,z):  return Builtin("sub", (x,y,z), bi_sub)
def p_add(x,y,z):  return Builtin("add", (x,y,z), bi_add)

# ===========================
# Rules, KB, standardize-apart
# ===========================

@dataclass(frozen=True)
class Rule:
    head: Struct
    body: Tuple[Union[Struct, Builtin], ...] = field(default_factory=tuple)
    def __str__(self):
        if not self.body: return f"{show(self.head)}."
        body = ", ".join(show(g) if isinstance(g, Struct) else f"{g.name}(...)" for g in self.body)
        return f"{show(self.head)} :- {body}."

@dataclass
class KB:
    rules: List[Rule]

def standardize_apart(rule: Rule, fresh: itertools.count) -> Rule:
    """
    Clone `rule` with brand-new variable ids so different uses never alias.
    """
    mapping: Dict[VarId, Var] = {}
    def rterm(t: Term) -> Term:
        if isinstance(t, Var):
            if t.id not in mapping:
                mapping[t.id] = V(t.name, fresh)
            return mapping[t.id]
        if isinstance(t, Struct):
            return Struct(t.functor, tuple(rterm(a) for a in t.args))
        return t
    head = rterm(rule.head)
    body: List[Union[Struct, Builtin]] = []
    for g in rule.body:
        if isinstance(g, Builtin):
            body.append(Builtin(g.name, tuple(rterm(a) for a in g.args), g.fn))
        else:
            body.append(rterm(g))
    return Rule(head, tuple(body))

# ===========================
# Proof tree (iterative print)
# ===========================

@dataclass
class ProofNode:
    goal: Union[Struct, Builtin, Struct]  # Struct or Builtin or 'true'
    depth: int
    parent: Optional['ProofNode'] = None
    rule_used: Optional[Rule] = None
    theta: Optional[Subst] = None
    children: List['ProofNode'] = field(default_factory=list)
    success: bool = False
    note: Optional[str] = None

def print_proof_iterative(root: ProofNode):
    """Print the tree iteratively to avoid recursion limits."""
    stack: List[Tuple[ProofNode, bool]] = [(root, True)]
    while stack:
        node, entering = stack.pop()
        ind = "  " * node.depth
        if entering:
            if isinstance(node.goal, Builtin):
                print(f"{ind}- Builtin: {node.goal.name}(" + ", ".join(show(a) for a in node.goal.args) + ")")
            else:
                note = f" [{node.note}]" if node.note else ""
                print(f"{ind}- Goal: {show(node.goal)}{note}")
            if node.rule_used is not None:
                print(f"{ind}  using rule: {node.rule_used}")
            if node.theta:
                binds = [f"{vid}->{show(val)}" for vid, val in node.theta.items()]
                if binds:
                    print(f"{ind}  theta: " + ", ".join(binds))
            stack.append((node, False))
            for child in reversed(node.children):
                stack.append((child, True))
        else:
            if node.success:
                print(f"{ind}  ✓ success")

# ===========================
# Tabling (memo) for fib/2
# ===========================

fib_table: Dict[int, int] = {}  # N -> F

def memo_lookup(goal: Struct, s: Subst) -> Optional[int]:
    if goal.functor != "fib" or len(goal.args) != 2:
        return None
    n = expect_int(goal.args[0], s)
    if n is None:
        return None
    return fib_table.get(n)

def memo_store(goal: Struct, s: Subst):
    if goal.functor != "fib" or len(goal.args) != 2:
        return
    n = expect_int(goal.args[0], s)
    f = expect_int(goal.args[1], s)
    if n is not None and f is not None:
        fib_table[n] = f

def preseed_fib(nmax: int):
    """
    Optional speedup: load a small DP table for fib 0..nmax ahead of time.
    This makes big fib queries instant but keeps the rule for small proofs.
    """
    if nmax < 0: return
    fib_table[0] = 0
    if nmax >= 1: fib_table[1] = 1
    for k in range(2, nmax + 1):
        fib_table[k] = fib_table[k-1] + fib_table[k-2]

# ===========================
# Iterative SLD engine (no recursion)
# ===========================

@dataclass
class State:
    goals: List[Union[Struct, Builtin]]
    subst: Subst
    parent_node: ProofNode
    depth: int

def solve_iter(kb: KB, query: Struct, max_depth: int = MAX_DEPTH):
    """
    Iterative depth-first SLD using an explicit stack.
    Yields (subst, proof_root) for each solution.
    """
    fresh = itertools.count(1_000_000)    # fresh ids for rule instances
    root = ProofNode(goal=Struct("true", ()), depth=0, parent=None)
    stack: List[State] = [State(goals=[query], subst={}, parent_node=root, depth=0)]

    while stack:
        st = stack.pop()
        if st.depth > max_depth:
            continue

        if not st.goals:
            leaf = ProofNode(goal=Struct("true", ()), depth=st.depth, parent=st.parent_node, success=True)
            st.parent_node.children.append(leaf)
            p = st.parent_node
            while p is not None and not p.success:
                p.success = True
                p = p.parent
            yield st.subst, root
            continue

        first, *rest = st.goals

        # Built-in goals
        if isinstance(first, Builtin):
            tprint(f"\n[prove] builtin {first.name} @ depth {st.depth}")
            node = ProofNode(goal=first, depth=st.depth, parent=st.parent_node)
            st.parent_node.children.append(node)
            for delta in first.fn(first.args, st.subst):
                ns = dict(st.subst)
                ok = True
                for vid, term in delta.items():
                    ns = unify(Var("_", vid), term, ns)
                    if ns is None:
                        ok = False
                        break
                if not ok:
                    continue
                stack.append(State(goals=rest, subst=ns, parent_node=node, depth=st.depth + 1))
            continue

        # Regular goals (Struct)
        if isinstance(first, Struct):
            # 1) PRIORITIZED memoization for fib/2 — if hit, skip rule expansion
            cached = memo_lookup(first, st.subst)
            if cached is not None:
                node = ProofNode(goal=first, depth=st.depth, parent=st.parent_node, note="memo hit")
                st.parent_node.children.append(node)
                ns = dict(st.subst)
                if unify(first.args[1], Const(cached), ns) is not None:
                    stack.append(State(goals=rest, subst=ns, parent_node=node, depth=st.depth + 1))
                    continue  # <<< key: do NOT also expand rules when memo resolves it

            # 2) Try matching rules
            for rule in kb.rules:
                if rule.head.functor != first.functor or len(rule.head.args) != len(first.args):
                    continue
                r = standardize_apart(rule, fresh)
                theta = unify(first, r.head, dict(st.subst))
                if theta is None:
                    continue
                delta_only = {vid: val for vid, val in theta.items()
                              if vid not in st.subst or st.subst[vid] is not theta[vid]}
                node = ProofNode(goal=first, depth=st.depth, parent=st.parent_node,
                                 rule_used=r, theta=delta_only)
                st.parent_node.children.append(node)
                new_goals = list(r.body) + rest
                stack.append(State(goals=new_goals, subst=theta, parent_node=node, depth=st.depth + 1))
                # Opportunistic store if fib/2 became ground
                memo_store(first, theta)

    return  # no solutions

# ===========================
# Build demo KB
# ===========================

def kb_demo() -> KB:
    fresh = itertools.count(1)
    X  = V("X", fresh)
    N  = V("N", fresh); F  = V("F", fresh)
    N1 = V("N1", fresh); N2 = V("N2", fresh)
    F1 = V("F1", fresh); F2 = V("F2", fresh)

    rules: List[Rule] = []

    # -- Socrates syllogism --
    rules.append(Rule(Struct("human", (Const("socrates"),))))
    rules.append(Rule(Struct("mortal", (X,)), (Struct("human", (X,)),)))

    # -- Fibonacci --
    # Base cases
    rules.append(Rule(Struct("fib", (Const(0), Const(0)))))
    rules.append(Rule(Struct("fib", (Const(1), Const(1)))))
    # Recursive case:
    rules.append(Rule(
        Struct("fib", (N, F)),
        (
            p_geq(N, Const(2)),         # guard: only expand if N >= 2
            p_sub(N, Const(1), N1),
            p_sub(N, Const(2), N2),
            Struct("fib", (N1, F1)),
            Struct("fib", (N2, F2)),
            p_add(F1, F2, F),
        )
    ))
    return KB(rules)

# ===========================
# Runner
# ===========================

def run_query(kb: KB, query: Struct, max_solutions: int = 1):
    print("\n" + "="*72)
    print(f"QUERY: {show(query)}")
    print("-"*72)

    found = 0
    for subst, proof_root in solve_iter(kb, query, MAX_DEPTH):
        found += 1
        qvars = [a for a in query.args if isinstance(a, Var)]
        if qvars:
            for v in qvars:
                print(f"{v.name} = {show(walk(v, subst))}")
        else:
            print("true.")
        print("\nProof:")
        for child in proof_root.children:
            print_proof_iterative(child)
        if found >= max_solutions:
            break

    if not found:
        print("No solutions.")
    print("="*72 + "\n")

def main():
    kb = kb_demo()

    # Demo 1: Socrates is mortal
    run_query(kb, Struct("mortal", (Const("socrates"),)))

    # Demo 2: Fibonacci (small) — full rule-based proof
    fresh = itertools.count(10_000_000)
    F = V("F", fresh)
    run_query(kb, Struct("fib", (Const(0), F)))

    # Demo 3: Fibonacci (small) — full rule-based proof
    fresh = itertools.count(10_000_000)
    F = V("F", fresh)
    run_query(kb, Struct("fib", (Const(1), F)))

    # Demo 4: Fibonacci (small) — full rule-based proof
    fresh = itertools.count(10_000_000)
    F = V("F", fresh)
    run_query(kb, Struct("fib", (Const(6), F)))

    # Pre-seed Fibonacci table so big queries are instant (still keep rules for small proofs).
    preseed_fib(5000)

    # Demo 5: Fibonacci (big) — instant via memo/preseed, no deep search
    Fbig = V("Fbig", fresh)
    run_query(kb, Struct("fib", (Const(91), Fbig)))

    # Demo 6: Fibonacci (big) — instant via memo/preseed, no deep search
    Fbig = V("Fbig", fresh)
    run_query(kb, Struct("fib", (Const(283), Fbig)))

    # Demo 7: Fibonacci (big) — instant via memo/preseed, no deep search
    Fbig = V("Fbig", fresh)
    run_query(kb, Struct("fib", (Const(3674), Fbig)))

if __name__ == "__main__":
    main()

