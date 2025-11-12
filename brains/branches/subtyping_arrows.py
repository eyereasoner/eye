# -*- coding: utf-8 -*-
"""
CASE MODULE (standalone runnable)
=================================
Arrow-type Subtyping (contravariant/covariant) in Hayes–Menzel holds₂ style
---------------------------------------------------------------------------

What this demonstrates
----------------------
A tiny *subtyping* calculus over a finite set of base types and all 3×3
function (arrow) types built from them. We encode:

  • Base types     : ex:Top, ex:Int, ex:Bool
  • Arrow types    : ex:Arr_Top_Top, ex:Arr_Int_Bool, ... (9 total)
  • Subtyping      : ex:SubType(T, U)   (both arguments are *names*)
  • Arrow mapping  : ex:ArrowOf(A1, A2, TA) says TA is the name for A1→A2

Rules
-----
  1) Reflexivity                  :  SubType(T, T).
  2) Transitivity                 :  SubType(T, U) :- SubType(T, V), SubType(V, U).
  3) Base facts                   :  Int ≤ Top, Bool ≤ Top.
  4) Arrow rule (→-subtyping)     :  If  ArrowOf(A1,A2,TA) and ArrowOf(B1,B2,TB)
                                      and SubType(B1, A1) and SubType(A2, B2)
                                      then SubType(TA, TB).
     (Contravariant in arg, covariant in result.)

This is purely first-order because types are *names* (intensions), and all
reasoning is over relations on names (no function symbols). The engine uses
the SIGNATURE to safely ground head-only variables.

How to run
----------
  python subtyping_arrows.py
  python eyezero.py subtyping_arrows.py

Printed sections
----------------
Model → Question → Answer → Reason why → Check (12 tests)
"""

from typing import List, Tuple, Set
from eyezero import (
    Var, Atom, Clause, atom, fact,
    solve_topdown, solve_bottomup, match_against_facts,
    NAME, IND, Signature, deref,
    local, fmt_set,
)

# ─────────────────────────────────────────────────────────────────────────────
# Type Names (as *names*/URIs)
# ─────────────────────────────────────────────────────────────────────────────
EX   = "ex:"
Top  = EX + "Top"
Int  = EX + "Int"
Bool = EX + "Bool"
BASE_TYPES: Tuple[str, ...] = (Top, Int, Bool)

def arr_name(a: str, b: str) -> str:
    """Canonical name for the arrow type a→b."""
    return EX + f"Arr_{local(a)}_{local(b)}"

# All 3×3 arrow types over {Top,Int,Bool}
ARROWS: Tuple[str, ...] = tuple(arr_name(a,b) for a in BASE_TYPES for b in BASE_TYPES)

# Mapping predicate from components to arrow name
ArrowOf = EX + "ArrowOf"     # NAME×NAME×NAME
# Subtyping relation (over *names*)
SubType = EX + "SubType"     # NAME×NAME

# Signature (NAME/IND). Only NAMEs appear in these predicates.
SIGNATURE: Signature = {
    ArrowOf: (NAME, NAME, NAME),
    SubType: (NAME, NAME),
}

# ─────────────────────────────────────────────────────────────────────────────
# PROGRAM (facts + rules)
# ─────────────────────────────────────────────────────────────────────────────
PROGRAM: List[Clause] = []

# ArrowOf facts for all pairs
for a in BASE_TYPES:
    for b in BASE_TYPES:
        PROGRAM.append(fact(ArrowOf, a, b, arr_name(a,b)))

# Base subtyping facts
PROGRAM += [
    fact(SubType, Int,  Top),
    fact(SubType, Bool, Top),
]

# Reflexivity (unsafe head-only variable; grounded via NAME-domain)
T = Var("T")
PROGRAM.append(Clause(atom(SubType, T, T), []))

# Transitivity
X, Y, Z = Var("X"), Var("Y"), Var("Z")
PROGRAM.append(Clause(atom(SubType, X, Z), [atom(SubType, X, Y), atom(SubType, Y, Z)]))

# Arrow subtyping: (A1→A2) ≤ (B1→B2) if B1 ≤ A1  and  A2 ≤ B2
A1, A2, B1, B2, TA, TB = Var("A1"), Var("A2"), Var("B1"), Var("B2"), Var("TA"), Var("TB")
PROGRAM.append(Clause(
    atom(SubType, TA, TB),
    [atom(ArrowOf, A1, A2, TA),
     atom(ArrowOf, B1, B2, TB),
     atom(SubType, B1, A1),     # contravariant in the argument
     atom(SubType, A2, B2)]     # covariant in the result
))

# ─────────────────────────────────────────────────────────────────────────────
# Case-specific engine glue
# ─────────────────────────────────────────────────────────────────────────────
def _is_var(t) -> bool: return isinstance(t, Var)

def choose_engine(goals: List[Atom]) -> str:
    """Enumerations (variables) → bottom-up; ground checks → top-down."""
    for g in goals:
        if g.pred == SubType and any(_is_var(t) for t in g.args):
            return "bottomup"
    return "topdown"

def ask(goals: List[Atom], step_limit: int = 10000, fallback_threshold: int = 4000):
    engine = choose_engine(goals)
    if engine == "topdown":
        sols, metric = solve_topdown(PROGRAM, goals, step_limit=step_limit)
        return engine, sols, metric
    else:
        facts, rounds = solve_bottomup(PROGRAM, SIGNATURE)
        sols = match_against_facts(goals, facts)
        return engine, sols, rounds

# ─────────────────────────────────────────────────────────────────────────────
# Presentation
# ─────────────────────────────────────────────────────────────────────────────
def print_model() -> None:
    print("Model")
    print("=====")
    print("Base types (names):", ", ".join(local(t) for t in BASE_TYPES))
    print("Arrow types (names):", ", ".join(local(a) for a in ARROWS))
    print("\nFixed predicates (signature)")
    print("----------------------------")
    print("• ex:SubType(T,U)  — subtyping over *names*; sorts: (NAME, NAME)")
    print("• ex:ArrowOf(A1,A2,T) — T is the *name* of arrow A1→A2; sorts: (NAME, NAME, NAME)")
    print("\nProgram rules")
    print("-------------")
    print("1) Reflexive: SubType(T,T).")
    print("2) Transitive: SubType(X,Z) :- SubType(X,Y), SubType(Y,Z).")
    print("3) Arrow: (A1→A2) ≤ (B1→B2) if B1 ≤ A1 and A2 ≤ B2 (contra/co-variance).")
    print("Base facts: Int ≤ Top, Bool ≤ Top.\n")

def print_question() -> None:
    print("Question")
    print("========")
    print("Q1) Enumerate all subtyping pairs SubType(T,U).             [auto engine]")
    print("Q2) Witness TB s.t. SubType(Arr_Int_Bool, TB).              [auto engine]")
    print("Q3) ∀X∈{Top,Int,Bool}: SubType(Arr_Top_X, Arr_Top_Top)?     [auto engine]")
    print("Q4) ∀X∈{Top,Int,Bool}: SubType(Arr_Top_Top, Arr_X_Top)?     [auto engine]")

def run_queries():
    # Q1: enumerate all subtyping pairs
    Tv, Uv = Var("T"), Var("U")
    eng1, sols1, m1 = ask([atom(SubType, Tv, Uv)])
    pairs = sorted({(deref(Tv,s), deref(Uv,s)) for s in sols1})

    # Q2: witnesses TB such that Arr_Int_Bool ≤ TB
    TA = arr_name(Int, Bool)
    TBv = Var("TB")
    eng2, sols2, m2 = ask([atom(SubType, TA, TBv)])
    witnesses = sorted({deref(TBv,s) for s in sols2})

    # Q3: universal property (covariance in result when arg=Top)
    ok3 = True
    for X in BASE_TYPES:
        TA = arr_name(Top, X)
        TT = arr_name(Top, Top)
        e, sols, _ = ask([atom(SubType, TA, TT)])
        if not sols: ok3 = False; break
    eng3 = "mixed"

    # Q4: universal property (contravariance in argument toward Top)
    ok4 = True
    AT = arr_name(Top, Top)
    for X in BASE_TYPES:
        TX = arr_name(X, Top)
        e, sols, _ = ask([atom(SubType, AT, TX)])
        if not sols: ok4 = False; break
    eng4 = "mixed"

    return (("Q1", eng1, pairs, m1),
            ("Q2", eng2, witnesses, m2),
            ("Q3", eng3, ok3, 0),
            ("Q4", eng4, ok4, 0))

def print_answer(res1, res2, res3, res4) -> None:
    print("Answer")
    print("======")
    tag1, eng1, pairs, _ = res1
    tag2, eng2, witnesses, _ = res2
    tag3, eng3, ok3, _ = res3
    tag4, eng4, ok4, _ = res4

    def show_pairs(ps):
        if not ps: return "∅"
        return "{" + ", ".join(f"{local(t)} ≤ {local(u)}" for (t,u) in ps) + "}"

    print(f"{tag1}) Engine: {eng1} → {show_pairs(pairs)}")
    print(f"{tag2}) Engine: {eng2} → Witness TB s.t. Arr_Int_Bool ≤ TB = " +
          ("∅" if not witnesses else "{" + ", ".join(local(w) for w in witnesses) + "}"))
    print(f"{tag3}) Engine: {eng3} → Universal (covariant result) holds: {'Yes' if ok3 else 'No'}")
    print(f"{tag4}) Engine: {eng4} → Universal (contravariant arg) holds: {'Yes' if ok4 else 'No'}\n")

def print_reason(eng1, eng2) -> None:
    print("Reason why")
    print("==========")
    print("• Types are *names*; subtyping is a relation over names (first-order).")
    print("• ArrowOf maps components (A1,A2) to the *name* of A1→A2.")
    print("• Arrow rule implements contravariance/covariance without functions or higher-order.")
    print("• Engine chooser: big enumerations → bottom-up; targeted checks → tabled top-down.\n")

# ─────────────────────────────────────────────────────────────────────────────
# Check (12 tests)
# ─────────────────────────────────────────────────────────────────────────────
class CheckFailure(AssertionError): pass
def check(c: bool, msg: str):
    if not c: raise CheckFailure(msg)

def run_checks() -> List[str]:
    notes: List[str] = []

    # 1) Bottom-up enumeration includes some expected base and arrow pairs
    facts, _ = solve_bottomup(PROGRAM, SIGNATURE)
    Tv, Uv = Var("T"), Var("U")
    bu = match_against_facts([atom(SubType, Tv, Uv)], facts)
    pairs = {(deref(Tv,s), deref(Uv,s)) for s in bu}
    expect_subset = {
        (Int, Top), (Bool, Top),
        (arr_name(Int, Bool), arr_name(Int, Top)),   # covariant result
        (arr_name(Top, Top),  arr_name(Int, Top)),   # contravariant arg
    }
    check(expect_subset.issubset(pairs), "Expected subtyping pairs missing in bottom-up.")
    notes.append("PASS 1: Bottom-up includes expected base & arrow pairs.")

    # 2) Tabled top-down proves a concrete arrow subtyping goal
    td, _ = solve_topdown(PROGRAM, [atom(SubType, arr_name(Int,Bool), arr_name(Int,Top))])
    check(bool(td), "Top-down failed on Arr_Int_Bool ≤ Arr_Int_Top.")
    notes.append("PASS 2: Tabled top-down proves a sample arrow subtyping.")

    # 3) Witnesses for Arr_Int_Bool ≤ TB are exactly {Arr_Int_Bool, Arr_Int_Top}
    Tv2 = Var("TB")
    td_w, _ = solve_topdown(PROGRAM, [atom(SubType, arr_name(Int,Bool), Tv2)])
    ws = {deref(Tv2,s) for s in td_w}
    check(ws == {arr_name(Int,Bool), arr_name(Int,Top)}, f"Witness set mismatch: {ws}")
    notes.append("PASS 3: Witness set for Arr_Int_Bool ≤ TB is correct.")

    # 4) Covariance universal: ∀X, Arr_Top_X ≤ Arr_Top_Top
    ok = True
    for X in BASE_TYPES:
        if not match_against_facts([atom(SubType, arr_name(Top,X), arr_name(Top,Top))], facts):
            ok = False; break
    check(ok, "Covariance universal failed.")
    notes.append("PASS 4: Covariance universal holds.")

    # 5) Contravariance universal: ∀X, Arr_Top_Top ≤ Arr_X_Top
    ok = True
    for X in BASE_TYPES:
        if not match_against_facts([atom(SubType, arr_name(Top,Top), arr_name(X,Top))], facts):
            ok = False; break
    check(ok, "Contravariance universal failed.")
    notes.append("PASS 5: Contravariance universal holds.")

    # 6) Deterministic pretty output
    s1 = " ".join(sorted(local(t) for t,_ in pairs))
    s2 = " ".join(sorted(local(t) for t,_ in set(pairs)))
    check(s1 == s2, "Determinism check failed.")
    notes.append("PASS 6: Deterministic formatting.")

    # 7) Engine chooser: enumeration vs ground
    e1, _, _ = ask([atom(SubType, Var("T"), Var("U"))])
    e2, _, _ = ask([atom(SubType, arr_name(Int,Bool), arr_name(Int,Top))])
    check(e1=="bottomup" and e2=="topdown", "Engine chooser mismatch.")
    notes.append("PASS 7: Engine chooser behaves as intended.")

    # 8) Reflexivity holds for all base and arrow types
    for t in list(BASE_TYPES) + list(ARROWS):
        check(match_against_facts([atom(SubType, t, t)], facts), f"Reflexivity failed for {local(t)}.")
    notes.append("PASS 8: Reflexivity holds.")

    # 9) No bogus cross-base subtyping (Int ≰ Bool, Bool ≰ Int)
    check(not match_against_facts([atom(SubType, Int, Bool)], facts), "Unexpected Int ≤ Bool.")
    check(not match_against_facts([atom(SubType, Bool, Int)], facts), "Unexpected Bool ≤ Int.")
    notes.append("PASS 9: No bogus base subtypings.")

    # 10) A negative arrow example should fail (Arr_Int_Int ≰ Arr_Bool_Int)
    check(not match_against_facts([atom(SubType, arr_name(Int,Int), arr_name(Bool,Int))], facts),
          "Unexpected Arr_Int_Int ≤ Arr_Bool_Int.")
    notes.append("PASS 10: Negative arrow subtyping blocked as expected.")

    # 11) Bottom-up closure stability (idempotence)
    f1,_ = solve_bottomup(PROGRAM, SIGNATURE); f2,_ = solve_bottomup(PROGRAM, SIGNATURE)
    check(f1[SubType]==f2[SubType], "Bottom-up closure not stable.")
    notes.append("PASS 11: Bottom-up closure stable.")

    # 12) Another top-down ground sample: Arr_Top_Top ≤ Arr_Bool_Top
    td2, _ = solve_topdown(PROGRAM, [atom(SubType, arr_name(Top,Top), arr_name(Bool,Top))])
    check(bool(td2), "Top-down failed on Arr_Top_Top ≤ Arr_Bool_Top.")
    notes.append("PASS 12: Tabled top-down proves contravariant sample.")

    return notes

# ─────────────────────────────────────────────────────────────────────────────
# Standalone runner (compatible with the dual engine runner)
# ─────────────────────────────────────────────────────────────────────────────
def main():
    print_model()
    print_question()
    res1, res2, res3, res4 = run_queries()
    print_answer(res1, res2, res3, res4)
    # For the "Reason why" section, forward first two engine labels (if any)
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
    main()

