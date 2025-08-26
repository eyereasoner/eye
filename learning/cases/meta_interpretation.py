#!/usr/bin/env python3
r"""
Meta-interpretation — faithful Python translation of:
  https://raw.githubusercontent.com/eyereasoner/arvol/refs/heads/main/input/meta-interpretation.pl

Program (condensed from the Prolog source):

  :- op(1200, xfx, :+).

  mi([], []).
  mi([G|Gs], []) :- head_body_(G, Goals, Gs), mi(Goals, []).

  head_body_(mi([], []), Rs, Rs).
  head_body_(mi([G|Gs], []), [head_body_(G, Goals, Gs), mi(Goals, [])|Rs], Rs).
  head_body_(head_body_(Head, Goals0, Goals), Rs, Rs) :- head_body_(Head, Goals0, Goals).

  head_body_(factorial(0, s(0)), Rs, Rs).
  head_body_(factorial(s(N), F), [factorial(N, F1), prod(s(N), F1, F)|Rs], Rs).

  head_body_(prod(0, _, 0), Rs, Rs).
  head_body_(prod(s(N), M, P), [prod(N, M, K), sum(K, M, P)|Rs], Rs).

  head_body_(sum(0, M, M), Rs, Rs).
  head_body_(sum(s(N), M, s(K)), [sum(N, M, K)|Rs], Rs).

  % query:
  true :+ mi([mi([factorial(s(s(s(s(s(0))))), _)], [])], []).

What this Python does
---------------------
• Implements symbolic terms (variables, functors, and lists), a tiny unifier, and the same clause set.
• Executes the Prolog *meta* logic: an outer `mi/2` that interprets an inner `mi/2`, which in turn interprets
  `factorial/2`, `prod/3`, and `sum/3` over Peano numbers.
• Produces ARC-style console sections:
    - Answer: the computed value of F for factorial(5, F) (as Peano and as an integer)
    - Reason why: which rules fired and how the meta-step rewrites proceed
    - Check (harness): re-runs the derivation, verifies goal exhaustion and that F converts to 120
"""

from __future__ import annotations
from dataclasses import dataclass
from typing import Any, Dict, Iterable, List, Optional, Sequence, Tuple, Union

# ----------------------------
# Term representation
# ----------------------------

_unique_id = 0
def _next_id() -> int:
    global _unique_id
    _unique_id += 1
    return _unique_id

@dataclass(frozen=True)
class Var:
    name: str
    id: int
    def __repr__(self) -> str:
        return f"{self.name}_{self.id}"

def V(name: str) -> Var:
    return Var(name, _next_id())

@dataclass(frozen=True)
class Fun:
    functor: str
    args: Tuple[Any, ...]  # args can be Fun, Var, or Python lists (for goal lists)
    def __repr__(self) -> str:
        if not self.args:
            return self.functor
        return f"{self.functor}({', '.join(map(_term_str, self.args))})"

Term = Union[Var, Fun, List[Any], str]  # variables, compounds, Python lists (for goal lists), atoms as strings

def _term_str(t: Term) -> str:
    if isinstance(t, list):
        return "[" + ", ".join(map(_term_str, t)) + "]"
    return repr(t)

# Convenience atoms
ZERO = Fun("0", ())

def S(x: Term) -> Fun:
    return Fun("s", (x,))

def peano(n: int) -> Term:
    t: Term = ZERO
    for _ in range(n):
        t = S(t)
    return t

def peano_to_int(t: Term) -> int:
    count = 0
    while isinstance(t, Fun) and t.functor == "s" and len(t.args) == 1:
        t = t.args[0]
        count += 1
    if t != ZERO:
        raise ValueError("Not a canonical Peano numeral")
    return count

# ----------------------------
# Unification
# ----------------------------

Subst = Dict[Var, Term]

def deref(x: Term, s: Subst) -> Term:
    while isinstance(x, Var) and x in s:
        x = s[x]
    return x

def occurs(v: Var, t: Term, s: Subst) -> bool:
    t = deref(t, s)
    if v == t:
        return True
    if isinstance(t, Var):
        return False
    if isinstance(t, list):
        return any(occurs(v, ti, s) for ti in t)
    if isinstance(t, Fun):
        return any(occurs(v, ti, s) for ti in t.args)
    return False

def unify(a: Term, b: Term, s: Subst) -> Optional[Subst]:
    a = deref(a, s)
    b = deref(b, s)
    if a == b:
        return s
    if isinstance(a, Var):
        if occurs(a, b, s):  # (light occurs-check)
            return None
        s[a] = b
        return s
    if isinstance(b, Var):
        if occurs(b, a, s):
            return None
        s[b] = a
        return s
    # Lists
    if isinstance(a, list) and isinstance(b, list):
        if len(a) != len(b):
            return None
        for x, y in zip(a, b):
            if (s := unify(x, y, s)) is None:
                return None
        return s
    # Functors
    if isinstance(a, Fun) and isinstance(b, Fun):
        if a.functor != b.functor or len(a.args) != len(b.args):
            return None
        for x, y in zip(a.args, b.args):
            if (s := unify(x, y, s)) is None:
                return None
        return s
    return None

def apply_subst(t: Term, s: Subst) -> Term:
    t = deref(t, s)
    if isinstance(t, Var):
        return t
    if isinstance(t, Fun):
        return Fun(t.functor, tuple(apply_subst(ai, s) for ai in t.args))
    if isinstance(t, list):
        return [apply_subst(ai, s) for ai in t]
    return t  # atoms (strings) are immutable

# ----------------------------
# Object program as Python rules
# ----------------------------

# The meta-interpreter rewrites goal lists. We mirror the Prolog clause order.

def expand_goal(goal: Fun, s: Subst, trace: List[str]) -> Tuple[List[Fun], Subst]:
    """
    Given a single goal (a Fun), return (body_goals, updated_subst).
    'body_goals' are the goals that replace 'goal' (following the Prolog head_body_/3).
    """
    goal = apply_subst(goal, s)

    # ---- mi/2 (meta) ----
    if goal.functor == "mi" and len(goal.args) == 2:
        goals_list, second = goal.args
        assert isinstance(goals_list, list), "mi/2 expects a list of goals as first arg"
        assert second == [], "mi/2 second argument must be [] in this program"
        if not goals_list:
            trace.append("mi([], [])  ⇒  []")
            return [], s
        # [G|Gs]
        G = goals_list[0]
        Gs = goals_list[1:]
        bodyG, s = expand_goal(G, s, trace)  # compute 'Goals' (body of G)
        new_mi = Fun("mi", (bodyG + Gs, []))
        trace.append(f"mi([G|Gs], [])  ⇒  mi(Goals, []) where Goals = body({G}) ++ Gs")
        return [new_mi], s

    # ---- head_body_(head_body_(...)) "reified" case is inlined by expand_goal above ----

    # ---- factorial/2 ----
    # Base: factorial(0, s(0)).
    pat = Fun("factorial", (ZERO, S(ZERO)))
    s_try = s.copy()
    if unify(goal, pat, s_try) is not None:
        trace.append("factorial(0, s(0))  ⇒  []")
        return [], s_try

    # Rec: factorial(s(N), F) :- factorial(N, F1), prod(s(N), F1, F).
    N, F = V("N"), V("F")
    pat = Fun("factorial", (S(N), F))
    s_try = s.copy()
    if unify(goal, pat, s_try) is not None:
        F1 = V("F1")
        body = [
            Fun("factorial", (N, F1)),
            Fun("prod",      (S(N), F1, F)),
        ]
        body = [apply_subst(b, s_try) for b in body]
        trace.append("factorial(s(N), F)  ⇒  [factorial(N, F1), prod(s(N), F1, F)]")
        return body, s_try

    # ---- prod/3 ----
    # Base: prod(0, _, 0).
    M_ = V("_")
    pat = Fun("prod", (ZERO, M_, ZERO))
    s_try = s.copy()
    if unify(goal, pat, s_try) is not None:
        trace.append("prod(0, _, 0)  ⇒  []")
        return [], s_try

    # Rec: prod(s(N), M, P) :- prod(N, M, K), sum(K, M, P).
    N, M, P = V("N"), V("M"), V("P")
    pat = Fun("prod", (S(N), M, P))
    s_try = s.copy()
    if unify(goal, pat, s_try) is not None:
        K = V("K")
        body = [
            Fun("prod", (N, M, K)),
            Fun("sum",  (K, M, P)),
        ]
        body = [apply_subst(b, s_try) for b in body]
        trace.append("prod(s(N), M, P)  ⇒  [prod(N, M, K), sum(K, M, P)]")
        return body, s_try

    # ---- sum/3 ----
    # Base: sum(0, M, M).
    M = V("M")
    pat = Fun("sum", (ZERO, M, M))
    s_try = s.copy()
    if unify(goal, pat, s_try) is not None:
        trace.append("sum(0, M, M)  ⇒  []")
        return [], s_try

    # Rec: sum(s(N), M, s(K)) :- sum(N, M, K).
    N, M, K = V("N"), V("M"), V("K")
    pat = Fun("sum", (S(N), M, S(K)))
    s_try = s.copy()
    if unify(goal, pat, s_try) is not None:
        body = [Fun("sum", (N, M, K))]
        body = [apply_subst(b, s_try) for b in body]
        trace.append("sum(s(N), M, s(K))  ⇒  [sum(N, M, K)]")
        return body, s_try

    raise ValueError(f"No matching clause for goal: {goal}")

# ----------------------------
# Meta-interpretation loop
# ----------------------------

def solve(goals: List[Fun]) -> Tuple[Subst, List[str]]:
    """
    Deterministic rewriting (mirrors mi/2).
    At each step, take the leftmost goal and replace it by its 'body' (possibly empty).
    Stops when the goal list becomes empty.
    """
    s: Subst = {}
    trace: List[str] = []
    steps = 0

    while goals:
        g = goals.pop(0)
        body, s = expand_goal(g, s, trace)
        goals = body + goals  # replace g by its body
        steps += 1
        # (Safety) guard against runaway terms in case of editing mistakes:
        if steps > 20000:
            raise RuntimeError("Too many steps; likely non-terminating.")

    return s, trace

# ----------------------------
# Build the exact Prolog query
# ----------------------------

def make_query_for_factorial(n: int) -> Tuple[List[Fun], Var]:
    """
    Build the exact nested query from the Prolog file:
      mi([ mi([ factorial(s^n(0), F) ], []) ], [])
    and return (initial_goals, F).
    """
    F = V("F")
    inner_list = [Fun("factorial", (peano(n), F))]
    inner_mi = Fun("mi", (inner_list, []))
    outer_goals = [inner_mi]
    outer_mi = Fun("mi", (outer_goals, []))
    return [outer_mi], F

# ----------------------------
# ARC-style output + harness
# ----------------------------

def fmt_peano(t: Term, max_s: int = 50) -> str:
    """Compact pretty-printer: s^k(0) if many s/1 layers."""
    # Count s-layers without expanding gigantic strings
    try:
        k = peano_to_int(t)
        return f"s^{k}(0)"
    except Exception:
        # Fallback to structural repr with a cap
        s = repr(t)
        if len(s) > 200:
            return s[:200] + "..."
        return s

def reason_text(trace: List[str], n: int, k_first: int = 10) -> str:
    lines = []
    lines.append("We run the meta-interpreter `mi/2` exactly as in the Prolog program,")
    lines.append("rewriting the leftmost goal each step using `head_body_/3`:")
    for i, step in enumerate(trace[:k_first], 1):
        lines.append(f"  {i:>2}. {step}")
    if len(trace) > k_first:
        lines.append(f"  … {len(trace)-k_first} more rewrite steps …")
    lines.append("")
    lines.append("Rules used (in order of matching):")
    lines.append("  factorial(0, s(0)).")
    lines.append("  factorial(s(N), F)  →  factorial(N, F1), prod(s(N), F1, F).")
    lines.append("  prod(0, _, 0).")
    lines.append("  prod(s(N), M, P)    →  prod(N, M, K), sum(K, M, P).")
    lines.append("  sum(0, M, M).")
    lines.append("  sum(s(N), M, s(K))  →  sum(N, M, K).")
    lines.append("")
    lines.append(f"This yields factorial({n}) by repeated unfolding into `prod` and `sum` over Peano numbers.")
    return "\n".join(lines)

def check_harness(n: int) -> None:
    """Re-run, verify the goal list empties and F = 120 (for n=5)."""
    goals, F = make_query_for_factorial(n)
    s, _ = solve(goals)
    F_val = apply_subst(F, s)
    assert isinstance(F_val, Fun) and F_val.functor in {"s", "0"}, "F did not instantiate to a Peano numeral."
    k = peano_to_int(F_val)
    from math import factorial as fact
    assert k == fact(n), f"peano_to_int(F)={k} but factorial({n})={fact(n)}"
    # Also sanity-check re-running doesn't depend on destructive state
    goals2, F2 = make_query_for_factorial(n)
    s2, _ = solve(goals2)
    k2 = peano_to_int(apply_subst(F2, s2))
    assert k2 == k, "Second run produced a different result."

def main():
    n = 5  # the Prolog query uses s(s(s(s(s(0)))))
    goals, F = make_query_for_factorial(n)
    subst, trace = solve(goals)
    F_val = apply_subst(F, subst)
    k = peano_to_int(F_val)

    # ----- ARC output -----
    print("Answer")
    print("------")
    print("Query:")
    print("  true :+ mi([mi([factorial(s^5(0), F)], [])], []).")
    print("Result:")
    print(f"  F (Peano) : {fmt_peano(F_val)}")
    print(f"  F (int)   : {k}")
    print()

    print("Reason why")
    print("----------")
    print(reason_text(trace, n, k_first=12))
    print()

    print("Check (harness)")
    print("----------------")
    try:
        check_harness(n)
        print("OK: meta-derivation exhausted goals and F = 120 (matches Python math.factorial).")
    except AssertionError as e:
        print("FAILED:", e)
        raise

if __name__ == "__main__":
    main()

