#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
CASE MODULE (standalone runnable)
=================================
Light Eaters (Plants) — in Hayes–Menzel "holds₁" style

What this demonstrates
----------------------
We re-express the minimal intelligence criterion from the original
light_eaters.py as **first-order rules over names (intensions)** using a fixed
application predicate:

    ex:holds1(R, x)      # "x ∈ extension(R)"

Base capability names (intensions):
  S(x): multimodal sensing (≥2 modalities)
  G(x): goal-directed adaptation
  M(x): memory
  N(x): networked internal signaling
  C(x): communication affecting others

We encode the *two-of-three* condition with pairwise witnesses:
  PairMN(x) :- M(x) ∧ N(x)
  PairMC(x) :- M(x) ∧ C(x)
  PairNC(x) :- N(x) ∧ C(x)
  TwoOf(x)  :- PairMN(x) ∨ PairMC(x) ∨ PairNC(x)

Minimal intelligence (I_min):
  Imin(x) :- S(x) ∧ G(x) ∧ TwoOf(x)

Everything is first-order: the “quantification over predicates” happens over
**names** (e.g., PairMN, PairMC, PairNC), and application remains `holds1`.

How to run
----------
  python light_eaters_holdsn.py
  python eyezero.py light_eaters_holdsn.py

Printed sections
----------------
Model → Question → Answer → Reason why → Check (harness)

Notes for readers
-----------------
- This CASE uses only unary application `holds1`, which your dual engine
  supports as any ordinary predicate (via the program & signature you see below).
- The harness creates **temporary individuals** (by pushing and popping facts)
  for boundary and negative tests; the base program remains simple and small.
"""

from typing import List, Tuple
from contextlib import contextmanager

from eyezero import (
    Var, Atom, Clause, atom, fact,
    solve_topdown, solve_bottomup, match_against_facts,
    NAME, IND, Signature, deref,
    # pretty helpers (optional)
    local,
)

# ─────────────────────────────────────────────────────────────────────────────
# Names & signature
# ─────────────────────────────────────────────────────────────────────────────
EX      = "ex:"
Holds1  = EX + "holds1"        # (NAME, IND)  — unary application: holds1(R, x)

# Capability names (relation names / intensions)
S       = EX + "S"
G       = EX + "G"
M       = EX + "M"
N       = EX + "N"
C       = EX + "C"

# Pairwise witnesses for the "two-of-three" condition
PairMN  = EX + "PairMN"
PairMC  = EX + "PairMC"
PairNC  = EX + "PairNC"

TwoOf   = EX + "TwoOf"         # disjunction over the three pairs
Imin    = EX + "Imin"          # the target predicate

# Individuals (strings)
Plants  = "Plants"

# Signature: only holds1 is needed here (NAME × IND)
SIGNATURE: Signature = {
    Holds1: (NAME, IND),
}

# ─────────────────────────────────────────────────────────────────────────────
# Program: facts + rules (Prolog-like)
# ─────────────────────────────────────────────────────────────────────────────
PROGRAM: List[Clause] = []

def h1(R, X) -> Clause:
    return fact(Holds1, R, X)

# Base facts for Plants (drawn from the light_eaters narrative)
PROGRAM += [
    h1(S, Plants),  # multimodal sensing (≥2 modalities)
    h1(G, Plants),  # goal-directed adaptation
    h1(M, Plants),  # memory
    h1(N, Plants),  # networked internal signaling
    h1(C, Plants),  # communication
]

# Rules: pairwise witnesses
X = Var("X")
PROGRAM += [
    Clause(atom(Holds1, PairMN, X), [atom(Holds1, M, X), atom(Holds1, N, X)]),
    Clause(atom(Holds1, PairMC, X), [atom(Holds1, M, X), atom(Holds1, C, X)]),
    Clause(atom(Holds1, PairNC, X), [atom(Holds1, N, X), atom(Holds1, C, X)]),
]

# TwoOf: any of the pairs suffices
X = Var("X")
PROGRAM += [
    Clause(atom(Holds1, TwoOf, X), [atom(Holds1, PairMN, X)]),
    Clause(atom(Holds1, TwoOf, X), [atom(Holds1, PairMC, X)]),
    Clause(atom(Holds1, TwoOf, X), [atom(Holds1, PairNC, X)]),
]

# Imin: S ∧ G ∧ TwoOf
X = Var("X")
PROGRAM += [
    Clause(atom(Holds1, Imin, X), [atom(Holds1, S, X), atom(Holds1, G, X), atom(Holds1, TwoOf, X)]),
]

# ─────────────────────────────────────────────────────────────────────────────
# Engine glue (tiny ask with a simple chooser)
# ─────────────────────────────────────────────────────────────────────────────
def _is_var(t) -> bool: return isinstance(t, Var)

def choose_engine(goals: List[Atom]) -> str:
    """
    Heuristic:
      - Enumeration with a free individual (holds1(R, Var)) → bottom-up is fine
      - Ground/mostly-bound checks → top-down
    This case is tiny; either engine works. We pick top-down for ground queries.
    """
    for g in goals:
        if g.pred == Holds1:
            if any(_is_var(a) for a in g.args):
                return "bottomup"
    return "topdown"

def ask(goals: List[Atom], step_limit: int = 10000):
    engine = choose_engine(goals)
    if engine == "topdown":
        sols, metric = solve_topdown(PROGRAM, goals, step_limit=step_limit)
        return engine, sols, metric
    else:
        facts, rounds = solve_bottomup(PROGRAM, SIGNATURE)
        sols = match_against_facts(goals, facts)
        return engine, sols, rounds

# ─────────────────────────────────────────────────────────────────────────────
# Utilities for temporary test facts (used by the harness)
# ─────────────────────────────────────────────────────────────────────────────
@contextmanager
def temp_facts(*cls: Clause):
    """
    Push some temporary facts/rules into PROGRAM for a 'with' block,
    then pop them to restore the base program.
    """
    start = len(PROGRAM)
    try:
        PROGRAM.extend(cls)
        yield
    finally:
        # remove anything appended after 'start'
        del PROGRAM[start:]

def has(goal: Atom) -> bool:
    return bool(ask([goal])[1])

# ─────────────────────────────────────────────────────────────────────────────
# Presentation
# ─────────────────────────────────────────────────────────────────────────────
def print_model() -> None:
    print("Model")
    print("=====")
    print(f"Individuals D = [{Plants!r}]  (tests will create temporary ones)")
    print("\nFixed predicate (signature)")
    print("---------------------------")
    print("• ex:holds1(R,x)  — unary application; sorts: (NAME, IND)")
    print("\nNamed capability relations (intensions)")
    print("---------------------------------------")
    print("S(x): multimodal sensing (≥2 modalities)")
    print("G(x): goal-directed adaptation")
    print("M(x): memory")
    print("N(x): networked signaling/integration")
    print("C(x): communication influencing other organisms")
    print("\nTwo-of-three witnesses")
    print("----------------------")
    print("PairMN(x) :- M(x) ∧ N(x)")
    print("PairMC(x) :- M(x) ∧ C(x)")
    print("PairNC(x) :- N(x) ∧ C(x)")
    print("TwoOf(x)  :- PairMN(x) ∨ PairMC(x) ∨ PairNC(x)")
    print("\nMinimal intelligence")
    print("--------------------")
    print("Imin(x)   :- S(x) ∧ G(x) ∧ TwoOf(x)\n")

def print_question() -> None:
    print("Question")
    print("========")
    print("Q1) Are plants intelligent under I_min (Imin(Plants))?        [auto engine]")
    print("Q2) Which pair-witnesses {MN, MC, NC} hold for Plants?         [auto engine]")
    print("Q3) Necessity: does ¬S prevent Imin for a system with G and two-of-three? [auto]\n")

def run_queries():
    # Q1: Is Imin(Plants)?
    eng1, sols1, _ = ask([atom(Holds1, Imin, Plants)])
    ans1 = bool(sols1)

    # Q2: Which pair witnesses hold?
    P = Plants
    pairs = []
    for (name, R) in [("MN", PairMN), ("MC", PairMC), ("NC", PairNC)]:
        if has(atom(Holds1, R, P)):
            pairs.append(name)
    eng2 = "topdown"  # small, but ground; top-down is fine

    # Q3: Necessity of S — fabricate a temporary individual X with G & (M,C) but no S
    Xid = "NoSense"
    with temp_facts(h1(G, Xid), h1(M, Xid), h1(C, Xid)):
        eng3, sols3, _ = ask([atom(Holds1, Imin, Xid)])
        ans3 = not bool(sols3)  # we expect that Imin(Xid) does not hold

    return (("Q1", eng1, ans1, "n/a"),
            ("Q2", eng2, sorted(pairs), "n/a"),
            ("Q3", eng3, ans3, "n/a"))

def print_answer(res1, res2, res3) -> None:
    print("Answer")
    print("======")
    tag1, eng1, ok1, _ = res1
    tag2, eng2, pairs, _ = res2
    tag3, eng3, ok3, _  = res3
    print(f"{tag1}) Engine: {eng1} → Imin(Plants): {'Yes' if ok1 else 'No'}")
    print(f"{tag2}) Engine: {eng2} → Pair-witnesses for Plants: " + ("∅" if not pairs else "{" + ", ".join(pairs) + "}"))
    print(f"{tag3}) Engine: {eng3} → Necessity (¬S ⇒ ¬Imin) holds: {'Yes' if ok3 else 'No'}\n")

def print_reason(eng1, eng2) -> None:
    print("Reason why")
    print("==========")
    print("• By rules: Imin(x) :- S(x) ∧ G(x) ∧ TwoOf(x).")
    print("• For Plants we have facts S(Plants), G(Plants), and all of M,N,C are true,")
    print("  so at least one of PairMN/PairMC/PairNC holds; hence TwoOf(Plants).")
    print("• Therefore Imin(Plants) holds. The necessity test shows that without S,")
    print("  even with G and two-of-three, Imin does not follow under these rules.\n")

# ─────────────────────────────────────────────────────────────────────────────
# Check (harness)
# ─────────────────────────────────────────────────────────────────────────────
class CheckFailure(AssertionError): pass
def check(c: bool, msg: str):
    if not c: raise CheckFailure(msg)

def run_checks() -> List[str]:
    notes: List[str] = []

    # 1) Plants intelligent
    td, _ = solve_topdown(PROGRAM, [atom(Holds1, Imin, Plants)])
    check(bool(td), "Imin(Plants) should hold.")
    notes.append("PASS 1: Imin(Plants) holds.")

    # 2) Pair witnesses present (all three given M,N,C)
    for R in (PairMN, PairMC, PairNC):
        check(has(atom(Holds1, R, Plants)), f"Missing pair witness {local(R)} for Plants.")
    notes.append("PASS 2: All pair-witnesses hold for Plants.")

    # 3) Necessity of S (no S → no Imin)
    Xid = "NoSense"
    with temp_facts(h1(G, Xid), h1(M, Xid), h1(C, Xid)):
        check(not has(atom(Holds1, Imin, Xid)), "Imin should not hold without S.")
    notes.append("PASS 3: Necessity of S verified.")

    # 4) Boundary: exactly two-of-three with S & G → Imin
    Yid = "Boundary"
    with temp_facts(h1(S, Yid), h1(G, Yid), h1(M, Yid), h1(C, Yid)):
        check(has(atom(Holds1, Imin, Yid)), "Boundary two-of-three failed to imply Imin.")
    notes.append("PASS 4: Two-of-three boundary implies Imin.")

    # 5) Monotonicity: adding capabilities preserves Imin
    Zid = "Mono"
    with temp_facts(h1(S, Zid), h1(G, Zid), h1(M, Zid), h1(C, Zid)):
        check(has(atom(Holds1, Imin, Zid)), "Precondition for monotonicity failed.")
        # Add N; still true
        with temp_facts(h1(N, Zid)):
            check(has(atom(Holds1, Imin, Zid)), "Monotonicity violated after adding N.")
    notes.append("PASS 5: Monotonicity True→True under added evidence.")

    # 6) Idempotence: repeating the query stable
    q = [atom(Holds1, Imin, Plants)]
    t1, _ = solve_topdown(PROGRAM, q)
    t2, _ = solve_topdown(PROGRAM, q)
    check(bool(t1) and bool(t2), "Repeated top-down query became unstable.")
    notes.append("PASS 6: Standardize-apart/idempotence stable.")

    # 7) Deterministic bottom-up enumeration of Imin (tiny program)
    facts, _ = solve_bottomup(PROGRAM, SIGNATURE)
    pairs = {(r, x) for (r, x) in facts.get(Holds1, set()) if r == Imin}
    s1 = sorted(pairs); s2 = sorted(set(pairs))
    check(s1 == s2, "Determinism of enumeration failed.")
    notes.append("PASS 7: Deterministic bottom-up output.")

    return notes

# ─────────────────────────────────────────────────────────────────────────────
# Standalone runner
# ─────────────────────────────────────────────────────────────────────────────
def main():
    print_model()
    print_question()
    r1, r2, r3 = run_queries()
    print_answer(r1, r2, r3)
    print_reason(r1[1], r2[1])
    print("Check (harness)")
    print("===============")
    try:
        for n in run_checks():
            print(n)
    except CheckFailure as e:
        print("FAIL:", e)
        raise

if __name__ == "__main__":
    main()

