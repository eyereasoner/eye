# -*- coding: utf-8 -*-
"""
CASE MODULE (standalone runnable)
=================================
Graph reachability (directed) in the Hayes–Menzel style:
relation *symbols* are **names** (intensions), and application is a fixed predicate

    ex:holds2(R, x, y)   (read: ⟨x,y⟩ ∈ ext(R))

We define:
  - ex:Edge      — base directed edges
  - ex:Reach     — transitive closure of Edge (non-reflexive here)
  - ex:SubRelOf  — inclusion between relation *names* (intensions), with Edge ⊆ Reach

Rules:
  • leq_strict/2 builds transitive closure of SubRelOf (⊊*)
  • leq/2 adds reflexive closure over names
  • lifting: holds2(Q,x,y) :- leq_strict(P,Q), holds2(P,x,y)
  • reach transitivity (right-recursive): holds2(Reach,x,z) :- holds2(Edge,x,y), holds2(Reach,y,z)
    (length-1 paths are obtained by lifting Edge ⊆ Reach)
"""

from typing import Dict, List, Tuple, Set
from eyezero import (
    Var, Atom, Clause, atom, fact,
    solve_topdown, solve_bottomup, match_against_facts,
    NAME, IND, Signature, deref,
    local, fmt_pairs, fmt_set,
)

# -------------------------
# Names (constants) & preds
# -------------------------

# Graph nodes (individuals)
D: Tuple[str, ...] = ("A", "B", "C", "D", "E")

# Relation names (intensions)
EX = "ex:"
Edge      = EX + "Edge"
Reach     = EX + "Reach"
SubRelOf  = EX + "SubRelOf"
LeqStrict = EX + "leq_strict"
Leq       = EX + "leq"
Holds2    = EX + "holds2"

# -------------------------------
# Predicate signature (NAME/IND)
# -------------------------------

SIGNATURE: Signature = {
    Holds2:     (NAME, IND, IND),
    SubRelOf:   (NAME, NAME),
    LeqStrict:  (NAME, NAME),
    Leq:        (NAME, NAME),
}

# -----------------------------
# PROGRAM (facts + rules)
# -----------------------------
PROGRAM: List[Clause] = []

# >>> USER SECTION: FACTS
# Base directed edges
for a,b in [
    ("A","B"),
    ("B","C"),
    ("C","D"),
    ("B","D"),  # a shortcut
    # "E" is isolated (no outgoing/incoming edges)
]:
    PROGRAM.append(fact(Holds2, Edge, a, b))

# Inclusion between relation *names*: Edge ⊆ Reach
PROGRAM.append(fact(SubRelOf, Edge, Reach))

# >>> USER SECTION: RULES
# Inclusion closure over names (⊊*)
P,Q,R = Var("P"),Var("Q"),Var("R")
PROGRAM.append(Clause(atom(LeqStrict,P,Q), [atom(SubRelOf,P,Q)]))
P,Q,R = Var("P"),Var("Q"),Var("R")
PROGRAM.append(Clause(atom(LeqStrict,P,Q), [atom(SubRelOf,P,R), atom(LeqStrict,R,Q)]))

# Reflexive closure over names (unsafe; grounded from NAME-domain)
P = Var("P")
PROGRAM.append(Clause(atom(Leq,P,P), []))

# ≤ includes ⊊
P,Q = Var("P"),Var("Q")
PROGRAM.append(Clause(atom(Leq,P,Q), [atom(LeqStrict,P,Q)]))

# Lifting facts along inclusion of names:
#   if P ⊆* Q and (x,y) ∈ P then (x,y) ∈ Q
P,Q,X,Y = Var("P"),Var("Q"),Var("X"),Var("Y")
PROGRAM.append(Clause(atom(Holds2, Q, X, Y),
                      [atom(LeqStrict, P, Q), atom(Holds2, P, X, Y)]))

# Transitive closure for Reach using Edge as the *first* step (right recursion):
#   Reach(x,z) :- Edge(x,y), Reach(y,z)
# This avoids left-recursive blowups in top-down SLD for ground goals.
X,Y,Z = Var("X"),Var("Y"),Var("Z")
PROGRAM.append(Clause(atom(Holds2, Reach, X, Z),
                      [atom(Holds2, Edge,  X, Y),
                       atom(Holds2, Reach, Y, Z)]))
# NOTE: We do NOT add Reach(x,x) (no reflexive closure) to keep "non-reflexive reachability".

# -------------------------
# Case-specific engine glue
# -------------------------

def _is_var(t) -> bool: return isinstance(t, Var)

def choose_engine(goals: List[Atom]) -> str:
    """
    Heuristic:
      - holds2(Reach, X?, Y?) with variables → bottom-up (enumeration)
      - any fully unbound goal (all vars) → bottom-up
      - otherwise → top-down
    """
    for g in goals:
        if g.pred == Holds2 and len(g.args)==3 and g.args[0]==Reach and (_is_var(g.args[1]) or _is_var(g.args[2])):
            return "bottomup"
        if all(_is_var(t) for t in g.args):
            return "bottomup"
    return "topdown"

def ask(goals: List[Atom], step_limit: int = 10000, fallback_threshold: int = 4000):
    engine = choose_engine(goals)
    if engine == "topdown":
        sols, steps = solve_topdown(PROGRAM, goals, step_limit=step_limit)
        if steps > fallback_threshold:
            facts, _ = solve_bottomup(PROGRAM, SIGNATURE)
            sols = match_against_facts(goals, facts)
            engine = "bottomup"
            return engine, sols, 0
        return engine, sols, steps
    else:
        facts, rounds = solve_bottomup(PROGRAM, SIGNATURE)
        sols = match_against_facts(goals, facts)
        return engine, sols, rounds

# -------------------------
# Presentation (printing)
# -------------------------

def print_model() -> None:
    print("Model")
    print("=====")
    print(f"Nodes (individuals) D = {list(D)}\n")
    print("Fixed predicates (signature)")
    print("----------------------------")
    print("• ex:holds2(R,x,y)   — application (⟨x,y⟩ ∈ ext(R)); sorts: (NAME, IND, IND)")
    print("• ex:SubRelOf(P,Q)   — inclusion over relation *names*; sorts: (NAME, NAME)")
    print("• ex:leq_strict / ex:leq   — ⊆* with/without reflex on names; sorts: (NAME, NAME)\n")
    print("Named relations (with facts)")
    print("----------------------------")
    print("Edge   =", fmt_pairs([("A","B"), ("B","C"), ("C","D"), ("B","D")]))
    print("Reach  = derived only (no base facts)\n")
    print("Inclusions over names: Edge ⊆ Reach\n")

def print_question() -> None:
    print("Question")
    print("========")
    print("Q1) List all (X,Y) with holds2(Reach,X,Y).   [auto engine]")
    print("Q2) ∃R: holds2(R,A,D) ∧ leq(R,Reach) ?  [auto engine]")
    print("Q3) ∀R,y: (leq(R,Edge) ∧ holds2(R,A,y)) → holds2(Reach,A,y) ?  [auto engine]")
    print()

def run_queries():
    # Q1: enumerate Reach pairs
    Xv, Yv = Var("X"), Var("Y")
    eng1, sols1, m1 = ask([atom(Holds2, Reach, Xv, Yv)])
    reach_pairs = sorted({(deref(Xv,s), deref(Yv,s)) for s in sols1})  # type: ignore

    # Q2: witness relation-names R for (A,D)
    Rv = Var("R")
    eng2, sols2, m2 = ask([atom(Holds2, Rv, "A", "D"),
                           atom(Leq,   Rv, Reach)])
    witnesses = sorted({deref(Rv,s) for s in sols2 if isinstance(deref(Rv,s), str)})

    # Q3: universal property for node A
    ok = True
    for R in [Edge, Reach]:
        for y in D:
            _, cond, _ = ask([atom(Leq, R, Edge), atom(Holds2, R, "A", y)])
            if cond:
                if not ask([atom(Holds2, Reach, "A", y)])[1]:
                    ok = False; break
        if not ok: break

    return (("Q1", eng1, reach_pairs, m1),
            ("Q2", eng2, witnesses, m2),
            ("Q3", "mixed", ok, 0))

def print_answer(res1, res2, res3) -> None:
    print("Answer")
    print("======")
    tag1, eng1, pairs, _ = res1
    tag2, eng2, wits, _  = res2
    tag3, eng3, ok, _    = res3
    print(f"{tag1}) Engine: {eng1} → Reach =", fmt_pairs(pairs))
    print(f"{tag2}) Engine: {eng2} → Witness relation-names R = " + (fmt_set(wits) if wits else "∅"))
    print(f"{tag3}) Engine: {eng3} → Universal statement holds: {'Yes' if ok else 'No'}\n")

def print_reason(eng1, eng2) -> None:
    print("Reason why")
    print("==========")
    print("• Edge ⊆ Reach via SubRelOf; the lifting rule moves Edge-facts into Reach.")
    print("• The transitive rule Reach(x,z) :- Edge(x,y), Reach(y,z) closes paths of length ≥2.")
    print("• We keep Reach non-reflexive (no zero-length paths).")
    print("• Auto-chooser: enumeration of Reach(X,Y) → bottom-up; ground checks → top-down.\n")

# -------------------
# Check (12 tests)
# -------------------

class CheckFailure(AssertionError): pass
def check(c: bool, msg: str):
    if not c: raise CheckFailure(msg)

def run_checks() -> List[str]:
    notes: List[str] = []

    expected = {
        ("A","B"),
        ("A","C"),
        ("A","D"),
        ("B","C"),
        ("B","D"),
        ("C","D"),
    }

    # 1) Bottom-up enumerates expected Reach
    facts, _ = solve_bottomup(PROGRAM, SIGNATURE)
    X, Y = Var("X"),Var("Y")
    bu = match_against_facts([atom(Holds2, Reach, X, Y)], facts)
    reach_bu = {(deref(X,s), deref(Y,s)) for s in bu}
    check(reach_bu == expected, "Bottom-up Reach enumeration mismatch.")
    notes.append("PASS 1: Bottom-up Reach enumeration is correct.")

    # 2) Top-down enumerates the same Reach
    td, _ = solve_topdown(PROGRAM, [atom(Holds2, Reach, X, Y)])
    reach_td = {(deref(X,s), deref(Y,s)) for s in td}
    check(reach_td == expected, "Top-down Reach enumeration mismatch.")
    notes.append("PASS 2: Top-down Reach enumeration is correct.")

    # 3) Existential witnesses for (A,D) are exactly {Reach}
    R = Var("R")
    bu_w = match_against_facts([atom(Holds2, R, "A","D"), atom(Leq, R, Reach)], facts)
    td_w, _ = solve_topdown(PROGRAM, [atom(Holds2, R, "A","D"), atom(Leq, R, Reach)])
    w1 = {deref(R,s) for s in bu_w}; w2 = {deref(R,s) for s in td_w}
    check(w1 == w2 == {Reach}, f"Witness set mismatch: {w1} vs {w2}")
    notes.append("PASS 3: Witness set for (A,D) is {Reach}.")

    # 4) Universal property for node A
    ok = True
    for r in [Edge, Reach]:
        for y in D:
            # If r ≤ Edge and r(A,y) then Reach(A,y)
            cond = ask([atom(Leq, r, Edge), atom(Holds2, r, "A", y)])[1]
            if cond and not ask([atom(Holds2, Reach, "A", y)])[1]:
                ok = False; break
        if not ok: break
    check(ok, "Universal property failed for node A.")
    notes.append("PASS 4: Universal property holds for node A.")

    # 5) No reachability from E (isolated)
    X = Var("X")
    none_from_E = not ask([atom(Holds2, Reach, "E", X)])[1]
    check(none_from_E, "Unexpected reachability from isolated node E.")
    notes.append("PASS 5: No reachability from E.")

    # 6) Non-reflexive: no Reach(x,x)
    for v in D:
        check(not ask([atom(Holds2, Reach, v, v)])[1], f"Unexpected reflexive reach at {v}.")
    notes.append("PASS 6: Reach is non-reflexive.")

    # 7) Auto-chooser behavior
    e1, _, _ = ask([atom(Holds2, Reach, Var("X"), Var("Y"))])
    e2, _, _ = ask([atom(Holds2, Reach, "A", "D")])
    check(e1 == "bottomup" and e2 == "topdown", "Engine chooser mismatch.")
    notes.append("PASS 7: Engine chooser behaves as intended.")

    # 8) leq reflexive on names
    for r in [Edge, Reach]:
        check(ask([atom(Leq, r, r)])[1], f"Reflexivity of leq failed for {local(r)}.")
    notes.append("PASS 8: leq reflexivity holds for all relation names.")

    # 9) leq_strict transitivity: Edge ⊆ Reach
    check(ask([atom(LeqStrict, Edge, Reach)])[1],
          "leq_strict Edge ⊆ Reach failed.")
    notes.append("PASS 9: leq_strict Edge ⊆ Reach holds.")

    # 10) Top-down stability (standardize-apart)
    ok1 = ask([atom(Holds2, Reach, "A", "D")])[1]
    ok2 = ask([atom(Holds2, Reach, "A", "D")])[1]
    check(ok1 and ok2, "Repeated top-down query should remain true.")
    notes.append("PASS 10: Standardize-apart stable on repeats.")

    # 11) Bottom-up closure idempotence
    f1, _ = solve_bottomup(PROGRAM, SIGNATURE)
    f2, _ = solve_bottomup(PROGRAM, SIGNATURE)
    check(f1[Holds2] == f2[Holds2], "Bottom-up closure not idempotent.")
    notes.append("PASS 11: Bottom-up closure is stable.")

    # 12) Deterministic printing
    s1 = fmt_pairs(sorted(expected)); s2 = fmt_pairs(sorted(list(expected)))
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

