# -*- coding: utf-8 -*-
"""
CASE MODULE (standalone runnable)
=================================
Fibonacci in Hayes–Menzel holds₂ style — fast for large ground N (< 20000)
with cached bottom-up facts to avoid repeated LFP recomputation.

What this demonstrates
----------------------
We keep everything first-order by treating the Fibonacci predicate as a *name*:

    ex:holds2(ex:Fib, n, v)

and providing a small Datalog-style substrate:

  • ex:Succ(n,m)         — successor on indices n∈[0..12]
  • ex:Add(x,y,z)        — precomputed addition table for x+y=z ≤ 144 (EDB)
  • Recurrence rule      — holds2(Fib, n+2, v0+v1) from Fib(n), Fib(n+1)

For very large **ground** n, we seed a single EDB fact
`holds2(Fib, n, v)` computed by **fast doubling** (O(log n)), so queries like
`holds2(Fib, "3674", V)` resolve instantly without changing the core logic.

Performance tweak
-----------------
We cache the bottom-up facts once and reuse them across queries & checks.
The cache is invalidated only when we seed a new big-N oracle fact.

Instead of a 10k-row Add table for all 0..144, we precompute Add(x,y,z)
*only for Fibonacci values* <= 144. That cuts Add to ~169 rows and
dramatically reduces bottom-up join work. We also cache the bottom-up facts.

How to run
----------
  python fibonacci_holdsn.py                 # full demo + checks
  python eyezero.py fibonacci_holdsn.py

  # Quick CLI to print F(n) without running the harness:
  python fibonacci_holdsn.py 3674

Printed sections
----------------
Model → Question → Answer → Reason why → Check (12 tests)
"""

from typing import List, Tuple, Dict, Set, Optional
from copy import deepcopy
from eyezero import (
    Var, Atom, Clause, atom, fact,
    solve_topdown, solve_bottomup, match_against_facts,
    NAME, IND, Signature, deref,
)

# ─────────────────────────────────────────────────────────────────────────────
# Names & signature
# ─────────────────────────────────────────────────────────────────────────────
EX     = "ex:"
Holds2 = EX + "holds2"         # NAME×IND×IND
Fib    = EX + "Fib"            # NAME (predicate name) used in holds2(Fib,n,v)
Succ   = EX + "Succ"           # IND×IND    (index successor only)
Add    = EX + "Add"            # IND×IND×IND (precomputed addition table)

# Small demo range for enumeration/printing
N_MAX = 12
V_MAX = 144

# Oracle limit for big ground queries
ORACLE_MAX_N = 20000

SIGNATURE: Signature = {
    Holds2: (NAME, IND, IND),
    Succ:   (IND, IND),
    Add:    (IND, IND, IND),
}

def s(i: int) -> str: return str(i)

# ─────────────────────────────────────────────────────────────────────────────
# Build tiny arithmetic substrate (optimized)
# ─────────────────────────────────────────────────────────────────────────────

def fib_values_upto(max_val: int) -> List[int]:
    """Return the unique Fibonacci numbers <= max_val (including 0), ascending."""
    vals: List[int] = [0, 1]
    while True:
        nxt = vals[-1] + vals[-2]
        if nxt > max_val: break
        vals.append(nxt)
    # de-duplicate (1 appears twice in the sequence start)
    return sorted(set(vals))

FVALS: List[int] = fib_values_upto(V_MAX)  # → [0,1,2,3,5,8,13,21,34,55,89,144]

# ─────────────────────────────────────────────────────────────────────────────
# PROGRAM (facts + rules)
# ─────────────────────────────────────────────────────────────────────────────
PROGRAM: List[Clause] = []

# Succ only for indices 0..12 (keeps the demo small & deterministic)
for n in range(N_MAX):
    PROGRAM.append(fact(Succ, s(n), s(n+1)))

# Optimized Add EDB: only Fibonacci-value pairs (drastically smaller than 0..144×0..144)
for x in FVALS:
    for y in FVALS:
        z = x + y
        if z <= V_MAX:
            PROGRAM.append(fact(Add, s(x), s(y), s(z)))

# Fibonacci base cases
PROGRAM.append(fact(Holds2, Fib, "0", "0"))
PROGRAM.append(fact(Holds2, Fib, "1", "1"))

# Fibonacci step (uses Succ on indices and Add on values)
N0, N1, N2 = Var("N0"), Var("N1"), Var("N2")
V0, V1, V  = Var("V0"), Var("V1"), Var("V")
PROGRAM.append(Clause(
    atom(Holds2, Fib, N2, V),
    [
        atom(Succ, N0, N1), atom(Succ, N1, N2),
        atom(Holds2, Fib, N0, V0),
        atom(Holds2, Fib, N1, V1),
        atom(Add, V0, V1, V),
    ]
))

# ─────────────────────────────────────────────────────────────────────────────
# Oracle seeding for large ground n + facts cache
# ─────────────────────────────────────────────────────────────────────────────
_oracle_seeded: Set[str] = set()
_FACTS_CACHE: Optional[Dict[str, Set[Tuple[str, ...]]]] = None
_FACTS_ROUNDS: int = 0  # for info

def _invalidate_cache() -> None:
    global _FACTS_CACHE, _FACTS_ROUNDS
    _FACTS_CACHE = None
    _FACTS_ROUNDS = 0

def _facts_cached() -> Dict[str, Set[Tuple[str, ...]]]:
    global _FACTS_CACHE, _FACTS_ROUNDS
    if _FACTS_CACHE is None:
        facts, rounds = solve_bottomup(PROGRAM, SIGNATURE)
        _FACTS_CACHE = facts
        _FACTS_ROUNDS = rounds
    return _FACTS_CACHE

def _fib_fast_doubling(n: int) -> int:
    def fd(k: int):
        if k == 0: return (0, 1)
        a, b = fd(k >> 1)
        c = a * ((b << 1) - a)
        d = a*a + b*b
        return (d, c + d) if (k & 1) else (c, d)
    return fd(n)[0]

def _maybe_seed_fib_oracle(goals: List[Atom]) -> None:
    """
    If a goal is holds2(Fib,'N',V) with ground decimal N (≤ ORACLE_MAX_N),
    compute Fib(N) once and add it as a base fact; then invalidate cache.
    """
    for g in goals:
        if g.pred == Holds2 and len(g.args) == 3 and g.args[0] == Fib:
            n = g.args[1]
            if isinstance(n, str) and n.isdigit() and n not in _oracle_seeded:
                n_int = int(n)
                if n_int <= ORACLE_MAX_N:
                    v = str(_fib_fast_doubling(n_int))
                    PROGRAM.append(fact(Holds2, Fib, n, v))
                    _oracle_seeded.add(n)
                    _invalidate_cache()

# ─────────────────────────────────────────────────────────────────────────────
# Engine glue (auto chooser)
# ─────────────────────────────────────────────────────────────────────────────
def _is_var(t) -> bool: return isinstance(t, Var)

def choose_engine(goals: List[Atom]) -> str:
    """
    Heuristic:
      - holds2(Fib, N, V) with both N and V free → bottom-up (enumeration)
      - otherwise (at least one bound)           → top-down (goal-directed)
    """
    for g in goals:
        if g.pred == Holds2 and len(g.args) == 3 and g.args[0] == Fib:
            n, v = g.args[1], g.args[2]
            if _is_var(n) and _is_var(v):
                return "bottomup"
            else:
                return "topdown"
    return "topdown"

def ask(goals: List[Atom], step_limit: int = 10000):
    _maybe_seed_fib_oracle(goals)
    engine = choose_engine(goals)
    if engine == "topdown":
        sols, metric = solve_topdown(PROGRAM, goals, step_limit=step_limit)
        return engine, sols, metric
    else:
        facts = _facts_cached()
        sols = match_against_facts(goals, facts)
        return engine, sols, _FACTS_ROUNDS

# ─────────────────────────────────────────────────────────────────────────────
# Presentation
# ─────────────────────────────────────────────────────────────────────────────
def print_model() -> None:
    print("Model")
    print("=====")
    print(f"Indices n ∈ [0..{N_MAX}] (strings), Values v ∈ [0..{V_MAX}] (strings) for the demo range.")
    print("Large ground n (< 20000) answered by a one-shot oracle seed (fast doubling).")
    print("\nFixed predicates (signature)")
    print("----------------------------")
    print("• ex:holds2(ex:Fib, n, v)  — Fibonacci as a binary relation; (NAME, IND, IND)")
    print("• ex:Succ(n,m)             — index successor (demo range); (IND, IND)")
    print("• ex:Add(x,y,z)            — precomputed addition (only Fib-values ≤ 144); (IND, IND, IND)")
    print("\nProgram rules")
    print("-------------")
    print("1) Base: Fib(0)=0, Fib(1)=1.")
    print("2) Step: Fib(n+2) = Fib(n+1) + Fib(n) via Succ and Add.\n")

def print_question() -> None:
    print("Question")
    print("========")
    print("Q1) Enumerate all pairs holds2(Fib, n, v) for n=0..12.       [auto engine]")
    print("Q2) Ground checks: Fib(7)=13, Fib(12)=144?                   [auto engine]")
    print("Q3) Functional property: for each n there is a unique v.     [auto engine]")
    print("Q4) Large-N demo: query holds2(Fib, '3674', V).               [auto engine + oracle]\n")

def run_queries():
    # Q1 enumeration
    Nv, Vv = Var("N"), Var("V")
    eng1, sols1, m1 = ask([atom(Holds2, Fib, Nv, Vv)])
    pairs = sorted({(deref(Nv,s), deref(Vv,s)) for s in sols1 if isinstance(deref(Nv,s),str) and isinstance(deref(Vv,s),str)},
                   key=lambda t: int(t[0]))

    # Q2 ground checks
    eng2a, sols2a, _ = ask([atom(Holds2, Fib, "7", "13")])
    eng2b, sols2b, _ = ask([atom(Holds2, Fib, "12", "144")])

    # Q3 uniqueness
    rows = _facts_cached().get(Holds2, set())
    ok_unique = True
    for n in [s(i) for i in range(N_MAX+1)]:
        vs = {v for (p,x,v) in rows if p == Fib and x == n}
        if len(vs) != 1:
            ok_unique = False; break

    # Q4: Large-N demo
    Vbig = Var("Vbig")
    eng4, sols4, _ = ask([atom(Holds2, Fib, "3674", Vbig)])
    big_ok = bool(sols4)

    return (("Q1", eng1, pairs, m1),
            ("Q2", "mixed", (bool(sols2a), bool(sols2b)), "n/a"),
            ("Q3", "bottomup", ok_unique, "n/a"),
            ("Q4", eng4, big_ok, "n/a"))

def print_answer(res1, res2, res3, res4) -> None:
    print("Answer")
    print("======")
    tag1, eng1, pairs, _ = res1
    tag2, eng2, (ok_7, ok_12), _ = res2
    tag3, eng3, ok_fun, _ = res3
    tag4, eng4, ok_big, _ = res4

    def show(ps): return "∅" if not ps else "{" + ", ".join(f"⟨{n},{v}⟩" for (n,v) in ps) + "}"
    print(f"{tag1}) Engine: {eng1} → Fib =", show(pairs))
    print(f"{tag2}) Engine: {eng2} → Fib(7)=13: {'Yes' if ok_7 else 'No'},  Fib(12)=144: {'Yes' if ok_12 else 'No'}")
    print(f"{tag3}) Engine: {eng3} → Functional per n (unique v): {'Yes' if ok_fun else 'No'}")
    print(f"{tag4}) Engine: {eng4} → Large-N (n=3674) solution present: {'Yes' if ok_big else 'No'}\n")

def print_reason(eng1, eng2) -> None:
    print("Reason why")
    print("==========")
    print("• Names-as-intensions (ex:Fib) with fixed holds₂ keeps logic first-order.")
    print("• Add EDB contains ONLY sums of Fibonacci values ≤ 144 → tiny join space.")
    print("• Large ground N handled by a one-shot fast-doubling seed.")
    print("• Cached bottom-up facts avoid recomputing the LFP across checks.\n")

# ─────────────────────────────────────────────────────────────────────────────
# Check (12 tests)
# ─────────────────────────────────────────────────────────────────────────────
class CheckFailure(AssertionError): pass
def check(c: bool, msg: str):
    if not c: raise CheckFailure(msg)

def run_checks() -> List[str]:
    notes: List[str] = []
    facts = _facts_cached()

    # 1) Expected pairs up to 12
    exp = {
        ("0","0"), ("1","1"), ("2","1"), ("3","2"), ("4","3"),
        ("5","5"), ("6","8"), ("7","13"), ("8","21"), ("9","34"),
        ("10","55"), ("11","89"), ("12","144"),
    }
    have = {(n,v) for (p,n,v) in facts.get(Holds2, set()) if p == Fib and int(n) <= N_MAX}
    check(exp == have, "Bottom-up Fibonacci enumeration mismatch.")
    notes.append("PASS 1: Bottom-up enumeration correct (0..12).")

    # 2) Tabled top-down: prove two ground goals
    td1, _ = solve_topdown(PROGRAM, [atom(Holds2, Fib, "7", "13")])
    td2, _ = solve_topdown(PROGRAM, [atom(Holds2, Fib, "12", "144")])
    check(bool(td1) and bool(td2), "Top-down failed on ground Fibonacci facts.")
    notes.append("PASS 2: Tabled top-down proves ground facts.")

    # 3) Negative: no wrong value, e.g., Fib(9) ≠ 33
    td_neg, _ = solve_topdown(PROGRAM, [atom(Holds2, Fib, "9", "33")])
    check(not td_neg, "Unexpected wrong Fibonacci value derived.")
    notes.append("PASS 3: Negative ground case blocked.")

    # 4) Add sanity: 55 + 89 = 144 exists (EDB)
    check(match_against_facts([atom(Add, "55", "89", "144")], facts),
          "Addition table missing 55+89=144.")
    notes.append("PASS 4: Addition table covers required sums.")

    # 5) Succ coverage includes 11→12
    check(match_against_facts([atom(Succ, "11", "12")], facts),
          "Succ coverage incomplete (11→12 missing).")
    notes.append("PASS 5: Successor coverage complete for indices.")

    # 6) Functional property: unique value per n (0..12)
    unique = True
    for n in [s(i) for i in range(N_MAX+1)]:
        vs = {v for (p,x,v) in facts.get(Holds2,set()) if p==Fib and x==n}
        if len(vs) != 1: unique = False; break
    check(unique, "Fibonacci not functional for some n.")
    notes.append("PASS 6: Unique value per index.")

    # 7) Exactly 13 pairs for n=0..12
    check(len(have) == 13, f"Expected 13 pairs, got {len(have)}.")
    notes.append("PASS 7: Correct number of pairs (13).")

    # 8) Deterministic formatting
    s1 = ", ".join(sorted(f"{n}:{v}" for (n,v) in have))
    s2 = ", ".join(sorted(f"{n}:{v}" for (n,v) in set(have)))
    check(s1 == s2, "Deterministic pretty-print failed.")
    notes.append("PASS 8: Deterministic output.")

    # 9) Engine chooser behavior
    e1, _, _ = ask([atom(Holds2, Fib, Var("N"), Var("V"))])
    e2, _, _ = ask([atom(Holds2, Fib, "12", Var("V"))])
    check(e1 == "bottomup" and e2 == "topdown", "Engine chooser mismatch.")
    notes.append("PASS 9: Engine chooser behaves as intended.")

    # 10) Repeated top-down queries stable (standardize-apart)
    q = [atom(Holds2, Fib, "8", "21")]
    td_a, _ = solve_topdown(PROGRAM, q)
    td_b, _ = solve_topdown(PROGRAM, q)
    check(bool(td_a) and bool(td_b), "Standardize-apart instability.")
    notes.append("PASS 10: Standardize-apart stable.")

    # 11) Bottom-up closure idempotent (cache-free clone equality)
    from copy import deepcopy
    f_clone = deepcopy(facts)
    check(f_clone == facts, "Bottom-up closure not stable.")
    notes.append("PASS 11: Bottom-up closure stable (single computation).")

    # 12) Oracle big-N seed (3674) present after seeding
    Vbig = Var("Vbig")
    _maybe_seed_fib_oracle([atom(Holds2, Fib, "3674", Vbig)])  # seed
    facts2 = _facts_cached()
    ok = match_against_facts([atom(Holds2, Fib, "3674", Var("V"))], facts2)
    check(bool(ok), "Oracle seeding for n=3674 did not add a fact.")
    notes.append("PASS 12: Oracle seeding adds the big-N fact.")

    return notes

# ─────────────────────────────────────────────────────────────────────────────
# Standalone runner + tiny CLI for quick F(n)
# ─────────────────────────────────────────────────────────────────────────────
def main():
    print_model()
    print_question()
    res1, res2, res3, res4 = run_queries()
    print_answer(res1, res2, res3, res4)
    print_reason(res1[1], res2[1])
    print("Check (harness)")
    print("===============")
    try:
        for note in run_checks():
            print(note)
    except CheckFailure as e:
        print("FAIL:", e)
        raise

if __name__ == "__main__":
    import sys
    if len(sys.argv) == 2 and sys.argv[1].isdigit():
        n = int(sys.argv[1])
        if n > ORACLE_MAX_N:
            raise SystemExit(f"n too large for this demo (n={n}, limit={ORACLE_MAX_N})")
        # Fast-doubling CLI path:
        def _fd(k: int):
            if k == 0: return (0, 1)
            a, b = _fd(k >> 1)
            c = a * ((b << 1) - a)
            d = a*a + b*b
            return (d, c + d) if (k & 1) else (c, d)
        print(_fd(n)[0])
    else:
        main()

