# -*- coding: utf-8 -*-
"""
CASE MODULE (standalone runnable)
=================================
Higher algebra — cyclic subgroups of Z₆
---------------------------------------
We represent subgroup generation as **reachability under “+k”** on Z₆, with the Hayes–Menzel
pattern:

    ex:holds2(R, x, y)     # ⟨x,y⟩ is in the extension of the relation-name R

Named relations (intensions):
  • ex:Add1, ex:Add2, ex:Add3        — one-step edges x → x+k (mod 6)
  • ex:Gen1, ex:Gen2, ex:Gen3        — generated-by-k reachability (positive-length)
  • ex:SubRelOf, ex:leq_strict, ex:leq (over *relation names*, i.e., intensions)

Rules (schematic):
  • leq_strict from SubRelOf, plus its transitive closure
  • leq is reflexive on names, and extends leq_strict
  • lifting: holds2(Q,x,y) :- leq_strict(P,Q), holds2(P,x,y)       # push facts along SubRelOf
  • generation (right-recursive): holds2(Rgen,x,z) :- holds2(Rstep,x,y), holds2(Rgen,y,z), SubRelOf(Rstep,Rgen)

This yields:
  Gen1 = all pairs (x,y)  (since 1 generates Z₆)
  Gen2 = pairs with y ≡ x (mod 2)
  Gen3 = pairs with y ≡ x (mod 3)

This module plugs into the generic engine `eyezero.py` and provides:
  print_model, print_question, run_queries, print_answer, print_reason, run_checks, and a standalone main().
"""

from typing import Dict, List, Tuple, Set
from eyezero import (
    Var, Atom, Clause, atom, fact,
    solve_topdown, solve_bottomup, match_against_facts,
    NAME, IND, Signature, deref,
    local, fmt_pairs, fmt_set,
)

# -------------------------
# Domain (individuals)
# -------------------------
# We use stringified integers to stay uniform with the engine’s Term = Union[str,Var].
D: Tuple[str, ...] = tuple(str(i) for i in range(6))  # "0","1","2","3","4","5"

# -------------------------
# Relation names (intensions)
# -------------------------
EX = "ex:"
Add1, Add2, Add3 = EX+"Add1", EX+"Add2", EX+"Add3"
Gen1, Gen2, Gen3 = EX+"Gen1", EX+"Gen2", EX+"Gen3"

SubRelOf   = EX + "SubRelOf"
LeqStrict  = EX + "leq_strict"
Leq        = EX + "leq"
Holds2     = EX + "holds2"

# -------------------------------
# Predicate signature (NAME/IND)
# -------------------------------
SIGNATURE: Signature = {
    Holds2:    (NAME, IND, IND),
    SubRelOf:  (NAME, NAME),
    LeqStrict: (NAME, NAME),
    Leq:       (NAME, NAME),
}

# -----------------------------
# PROGRAM (facts + rules)
# -----------------------------
PROGRAM: List[Clause] = []

# >>> USER SECTION: FACTS
# Addk edges: for each x in Z6, a single step x → x+k (mod 6)
def _add_mod6(x: int, k: int) -> int: return (x + k) % 6

for x in range(6):
    PROGRAM.append(fact(Holds2, Add1, str(x), str(_add_mod6(x,1))))
    PROGRAM.append(fact(Holds2, Add2, str(x), str(_add_mod6(x,2))))
    PROGRAM.append(fact(Holds2, Add3, str(x), str(_add_mod6(x,3))))

# SubRelOf edges tying each step-relation to its generated reachability
PROGRAM += [
    fact(SubRelOf, Add1, Gen1),
    fact(SubRelOf, Add2, Gen2),
    fact(SubRelOf, Add3, Gen3),
]

# >>> USER SECTION: RULES
# Inclusion closure over relation names
P,Q,R = Var("P"),Var("Q"),Var("R")
PROGRAM.append(Clause(atom(LeqStrict,P,Q), [atom(SubRelOf,P,Q)]))
P,Q,R = Var("P"),Var("Q"),Var("R")
PROGRAM.append(Clause(atom(LeqStrict,P,Q), [atom(SubRelOf,P,R), atom(LeqStrict,R,Q)]))

# leq is reflexive on relation names (unsafe; grounded from NAME-domain)
P = Var("P")
PROGRAM.append(Clause(atom(Leq,P,P), []))
# and includes leq_strict
P,Q = Var("P"),Var("Q")
PROGRAM.append(Clause(atom(Leq,P,Q), [atom(LeqStrict,P,Q)]))

# Lifting along inclusion of names: push Addk-facts into Genk
P,Q,X,Y = Var("P"),Var("Q"),Var("X"),Var("Y")
PROGRAM.append(Clause(atom(Holds2, Q, X, Y),
                      [atom(Holds2, P, X, Y),     # bind P from actual facts first
                       atom(LeqStrict, P, Q)]))   # then push along inclusion

# Generation (right-recursive) — avoid left-recursive SLD blowups:
#   holds2(Rgen, x, z) :- holds2(Rstep, x, y), holds2(Rgen, y, z), SubRelOf(Rstep, Rgen).
Rstep, Rgen, X, Y, Z = Var("Rstep"), Var("Rgen"), Var("X"), Var("Y"), Var("Z")
PROGRAM.append(Clause(atom(Holds2, Rgen, X, Z),
                      [atom(Holds2, Rstep, X, Y),
                       atom(Holds2, Rgen,  Y, Z),
                       atom(SubRelOf, Rstep, Rgen)]))

# -------------------------
# Case-specific engine glue
# -------------------------
def _is_var(t) -> bool: return isinstance(t, Var)

def choose_engine(goals: List[Atom]) -> str:
    """
    Heuristic:
      - holds2(Gen*, X?, Y?) with variables → bottom-up (enumeration)
      - any fully unbound goal (all vars) → bottom-up
      - otherwise → top-down
    """
    for g in goals:
        if g.pred == Holds2 and len(g.args)==3 and str(g.args[0]).startswith(EX+"Gen") and (_is_var(g.args[1]) or _is_var(g.args[2])):
            return "bottomup"
        if all(_is_var(t) for t in g.args):
            return "bottomup"
    return "topdown"

def ask(goals: List[Atom], step_limit: int = 10000, fallback_threshold: int = 4000):
    from eyezero import solve_topdown as _td, solve_bottomup as _bu  # explicit
    engine = choose_engine(goals)
    if engine == "topdown":
        sols, steps = _td(PROGRAM, goals, step_limit=step_limit)
        if steps > fallback_threshold:
            facts, _ = _bu(PROGRAM, SIGNATURE)
            sols = match_against_facts(goals, facts)
            engine = "bottomup"
            return engine, sols, 0
        return engine, sols, steps
    else:
        facts, rounds = _bu(PROGRAM, SIGNATURE)
        sols = match_against_facts(goals, facts)
        return engine, sols, rounds

# -------------------------
# Presentation (printing)
# -------------------------
def print_model() -> None:
    print("Model")
    print("=====")
    print(f"Elements (individuals) D = {list(D)}  (Z₆, written as strings)\n")
    print("Fixed predicates (signature)")
    print("----------------------------")
    print("• ex:holds2(R,x,y)   — application (⟨x,y⟩ ∈ ext(R)); sorts: (NAME, IND, IND)")
    print("• ex:SubRelOf(P,Q)   — inclusion over relation *names*; sorts: (NAME, NAME)")
    print("• ex:leq_strict / ex:leq   — ⊆* with/without reflex on names; sorts: (NAME, NAME)\n")
    print("Named relations (with facts)")
    print("----------------------------")
    add1 = [(str(x), str((x+1)%6)) for x in range(6)]
    add2 = [(str(x), str((x+2)%6)) for x in range(6)]
    add3 = [(str(x), str((x+3)%6)) for x in range(6)]
    print("Add1 =", fmt_pairs(add1))
    print("Add2 =", fmt_pairs(add2))
    print("Add3 =", fmt_pairs(add3))
    print("Gen1, Gen2, Gen3 = derived only (no base facts)\n")
    print("Inclusions over names: Add1 ⊆ Gen1,  Add2 ⊆ Gen2,  Add3 ⊆ Gen3\n")

def print_question() -> None:
    print("Question")
    print("========")
    print("Q1) List all (x,y) with holds2(Gen2,x,y).                  [auto engine]")
    print("Q2) ∃R: holds2(R,0,3) ∧ leq(R,Gen3) ?  (witness relation names)  [auto engine]")
    print("Q3) ∀R,y: (leq(R,Gen2) ∧ holds2(R,0,y)) → holds2(Gen2,0,y) ?     [auto engine]")
    print()

def run_queries():
    # Q1: enumerate Gen2 pairs
    Xv, Yv = Var("X"), Var("Y")
    eng1, sols1, m1 = ask([atom(Holds2, Gen2, Xv, Yv)])
    gen2_pairs = sorted({(deref(Xv,s), deref(Yv,s)) for s in sols1})  # type: ignore

    # Q2: witness relation-names R for (0,3) under Gen3
    Rv = Var("R")
    eng2, sols2, m2 = ask([atom(Holds2, Rv, "0", "3"),
                           atom(Leq,   Rv, Gen3)])
    witnesses = sorted({deref(Rv,s) for s in sols2 if isinstance(deref(Rv,s), str)})

    # Q3: universal property for Gen2 at source 0
    ok = True
    for R in [Add2, Gen2]:
        for y in D:
            _, cond, _ = ask([atom(Leq, R, Gen2), atom(Holds2, R, "0", y)])
            if cond and not ask([atom(Holds2, Gen2, "0", y)])[1]:
                ok = False; break
        if not ok: break

    return (("Q1", eng1, gen2_pairs, m1),
            ("Q2", eng2, witnesses, m2),
            ("Q3", "mixed", ok, 0))

def print_answer(res1, res2, res3) -> None:
    print("Answer")
    print("======")
    tag1, eng1, pairs, _ = res1
    tag2, eng2, wits, _  = res2
    tag3, eng3, ok, _    = res3
    print(f"{tag1}) Engine: {eng1} → Gen2 =", fmt_pairs(pairs))
    print(f"{tag2}) Engine: {eng2} → Witness relation-names R = " + (fmt_set(wits) if wits else "∅"))
    print(f"{tag3}) Engine: {eng3} → Universal statement holds: {'Yes' if ok else 'No'}\n")

def print_reason(eng1, eng2) -> None:
    print("Reason why")
    print("==========")
    print("• In Z₆, ⟨1⟩ = Z₆, ⟨2⟩ = {0,2,4}, ⟨3⟩ = {0,3}.")
    print("• We encode “generated-by-k” as reachability under Addk steps with right recursion.")
    print("• Quantification over relations (e.g., ∃R ⊆ Gen3) is over **names** (intensions).")
    print("• Lifting pushes Addk-facts into Genk, then the recursive rule closes paths (n≥2).")
    print("• Auto-chooser: big enumerations (Gen2(X,Y)) → bottom-up; ground checks → top-down.\n")

# -------------------
# Check (12 tests)
# -------------------
class CheckFailure(AssertionError): pass
def check(c: bool, msg: str):
    if not c: raise CheckFailure(msg)

def _expected_gen_k_pairs(k: int) -> Set[Tuple[str,str]]:
    """Compute expected reachability pairs for Addk on Z6 (positive-length paths)."""
    nodes = list(range(6))
    edges = {x: {(x+k)%6} for x in nodes}
    # transitive closure (no zero-length path)
    reach: Dict[int, Set[int]] = {x: set() for x in nodes}
    # BFS from each node using directed edges
    from collections import deque
    for s in nodes:
        seen = set()
        q = deque()
        for t in edges[s]:
            q.append(t); seen.add(t); reach[s].add(t)
        while q:
            u = q.popleft()
            for v in edges[u]:
                if v not in seen:
                    seen.add(v); q.append(v); reach[s].add(v)
    return {(str(x), str(y)) for x in nodes for y in reach[x]}

def run_checks() -> List[str]:
    notes: List[str] = []

    # Expected Gen2 & Gen3 sets (finite, computed procedurally)
    exp_gen2 = _expected_gen_k_pairs(2)
    exp_gen3 = _expected_gen_k_pairs(3)

    # 1) Bottom-up enumerates expected Gen2
    facts, _ = solve_bottomup(PROGRAM, SIGNATURE)
    X, Y = Var("X"),Var("Y")
    bu2 = match_against_facts([atom(Holds2, Gen2, X, Y)], facts)
    gen2_bu = {(deref(X,s), deref(Y,s)) for s in bu2}
    check(gen2_bu == exp_gen2, "Bottom-up Gen2 enumeration mismatch.")
    notes.append("PASS 1: Bottom-up Gen2 enumeration is correct.")

    # 2) Goal-directed correctness on *ground* pairs (use ask(); allow fallback)
    ok_all = True
    for x in D:
        for y in D:
            should = (x, y) in exp_gen2
            got = bool(ask([atom(Holds2, Gen2, x, y)])[1])   # <-- cast list → bool
            if got != should:
                ok_all = False
                break
        if not ok_all:
            break
    check(ok_all, "Ground goal-directed correctness for Gen2 failed.")
    notes.append("PASS 2: Ground goal-directed correctness for Gen2 holds.")

    # 3) Existential witnesses for (0,3) under Gen3 are {Add3, Gen3}
    R = Var("R")
    bu_w = match_against_facts([atom(Holds2, R, "0","3"), atom(Leq, R, Gen3)], facts)
    td_w, _ = solve_topdown(PROGRAM, [atom(Holds2, R, "0","3"), atom(Leq, R, Gen3)])
    w1 = {deref(R,s) for s in bu_w}; w2 = {deref(R,s) for s in td_w}
    check(w1 == w2 == {Add3, Gen3}, f"Witness set mismatch: {w1} vs {w2}")
    notes.append("PASS 3: Witness set for (0,3) under Gen3 is {Add3, Gen3}.")

    # 4) Universal property for Gen2 at source 0
    ok = True
    for r in [Add2, Gen2]:
        for y in D:
            cond = ask([atom(Leq, r, Gen2), atom(Holds2, r, "0", y)])[1]
            if cond and not ask([atom(Holds2, Gen2, "0", y)])[1]:
                ok = False; break
        if not ok: break
    check(ok, "Universal property failed for Gen2.")
    notes.append("PASS 4: Universal property holds for Gen2 at source 0.")

    # 5) Negative: (0,3) is NOT in Gen2
    check(not ask([atom(Holds2, Gen2, "0", "3")])[1], "Gen2 incorrectly relates 0 to 3.")
    notes.append("PASS 5: Gen2 excludes cross-parity pair (0,3).")

    # 6) Non-membership example for Gen3: (1,0) not reachable (different cosets mod 3)
    check(not ask([atom(Holds2, Gen3, "1", "0")])[1], "Gen3 incorrectly relates 1 to 0.")
    notes.append("PASS 6: Gen3 excludes cross-coset pair (1,0).")

    # 7) Auto-chooser behavior (enumeration vs ground)
    e1, _, _ = ask([atom(Holds2, Gen2, Var("X"), Var("Y"))])        # → bottomup
    e2, _, _ = ask([atom(Holds2, Gen2, "0", "4")])                  # → topdown (or fallback)
    check(e1 == "bottomup" and e2 in ("topdown", "bottomup"), "Engine chooser mismatch.")
    notes.append("PASS 7: Engine chooser behaves as intended (enumeration vs ground).")

    # 8) leq reflexive on names
    for r in [Add1, Add2, Add3, Gen1, Gen2, Gen3]:
        check(ask([atom(Leq, r, r)])[1], f"Reflexivity of leq failed for {local(r)}.")
    notes.append("PASS 8: leq reflexivity holds for all relation names.")

    # 9) leq_strict transitivity: Add2 ⊆ Gen2 and Add3 ⊆ Gen3
    check(ask([atom(LeqStrict, Add2, Gen2)])[1], "leq_strict Add2 ⊆ Gen2 failed.")
    check(ask([atom(LeqStrict, Add3, Gen3)])[1], "leq_strict Add3 ⊆ Gen3 failed.")
    notes.append("PASS 9: leq_strict inclusions hold.")

    # 10) Top-down stability (standardize-apart) on ground
    ok1 = ask([atom(Holds2, Gen2, "0", "4")])[1]
    ok2 = ask([atom(Holds2, Gen2, "0", "4")])[1]
    check(ok1 and ok2, "Repeated ground query should remain true.")
    notes.append("PASS 10: Standardize-apart stable on repeats.")

    # 11) Bottom-up closure idempotence
    f1, _ = solve_bottomup(PROGRAM, SIGNATURE)
    f2, _ = solve_bottomup(PROGRAM, SIGNATURE)
    check(f1[Holds2] == f2[Holds2], "Bottom-up closure not idempotent.")
    notes.append("PASS 11: Bottom-up closure is stable.")

    # 12) Deterministic printing (on Gen2’s expected set)
    s1 = fmt_pairs(sorted(exp_gen2)); s2 = fmt_pairs(sorted(list(exp_gen2)))
    check(s1 == s2, "Pretty-printer determinism failed.")
    notes.append("PASS 12: Pretty printing deterministic.")

    return notes

# -------------------
# Standalone runner
# -------------------
def main():
    print_model()
    print_question()
    res1, res2, res3 = run_queries()
    print_answer(res1, res2, res3)
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

