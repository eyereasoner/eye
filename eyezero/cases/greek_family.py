# -*- coding: utf-8 -*-
"""
CASE MODULE
===========
Greek family pedigree demonstrating the Hayes–Menzel idea:
relation *symbols* are **names** (intensions) and application is a fixed predicate:

    ex:holds2(R, x, y)   (read: ⟨x,y⟩ ∈ ext(R))

So “quantifying over predicates” becomes quantifying over **names** while staying first-order.

This module relies on the generic engine in eyezero.py and provides:
  - D, SIGNATURE, PROGRAM
  - presentation hooks (print_model/print_question/...)
  - a case-specific engine chooser + ask()
"""

from eyezero import (
    Var, Atom, Clause, atom, fact,
    solve_topdown, solve_bottomup, match_against_facts,
    NAME, IND, Signature, deref,
    local, fmt_pairs, fmt_set,
)

from typing import Dict, List, Tuple, Set

# -------------------------
# Names (constants) & preds
# -------------------------

D: Tuple[str, ...] = (
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
for a,b in [
    ("Sophroniscus","Socrates"),
    ("Socrates","Lamprocles"),
    ("Ariston","Plato"),
    ("Nicomachus","Aristotle"),
]:
    PROGRAM += [fact(Holds2, FatherOf, a, b),
                fact(Holds2, ParentOf, a, b)]
PROGRAM.append(fact(Holds2, TeacherOf, "Socrates", "Plato"))

PROGRAM += [
    fact(SubRelOf, FatherOf, ParentOf),
    fact(SubRelOf, ParentOf, AncestorOf),
]

# >>> USER SECTION: RULES
# Inclusion closure over names
P,Q,R = Var("P"),Var("Q"),Var("R")
PROGRAM.append(Clause(atom(LeqStrict,P,Q), [atom(SubRelOf,P,Q)]))
P,Q,R = Var("P"),Var("Q"),Var("R")
PROGRAM.append(Clause(atom(LeqStrict,P,Q), [atom(SubRelOf,P,R), atom(LeqStrict,R,Q)]))

# Reflexive closure over names (unsafe; grounded from NAME domain)
P = Var("P")
PROGRAM.append(Clause(atom(Leq,P,P), []))

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

# Lifting along inclusion between names
P,Q,X,Y = Var("P"),Var("Q"),Var("X"),Var("Y")
PROGRAM.append(Clause(atom(Holds2, Q, X, Y),
                      [atom(LeqStrict, P, Q), atom(Holds2, P, X, Y)]))

# -------------------------
# Case-specific engine glue
# -------------------------

def _is_var(t) -> bool: return isinstance(t, Var)

def choose_engine(goals: List[Atom]) -> str:
    """
    Heuristic:
      - holds2(AncestorOf, X?, Y?) with variables → bottom-up (enumeration)
      - any fully unbound goal (all vars) → bottom-up
      - otherwise → top-down
    """
    for g in goals:
        if g.pred == Holds2 and len(g.args)==3 and g.args[0]==AncestorOf and (_is_var(g.args[1]) or _is_var(g.args[2])):
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

    # Q3: universal property
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
    print("• Engines are generic; this CASE supplies only facts, rules, and a signature.")
    print("• Top-down SLD does goal-directed proof search with standardize-apart.")
    print("• Bottom-up LFP derives all consequences; the signature safely grounds")
    print("  head-only variables (e.g., leq(P,P).) from NAME/IND domains.")
    print("• Auto-chooser sends enumerative AncestorOf(X,Y) to bottom-up;")
    print("  bound/ground queries go top-down.\n")

# -------------------
# Check (12 tests)
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
    X, Y = Var("X"),Var("Y")
    bu = match_against_facts([atom(Holds2, AncestorOf, X, Y)], facts)
    anc_bu = {(deref(X,s), deref(Y,s)) for s in bu}
    check(anc_bu == expected, "Bottom-up AncestorOf enumeration mismatch.")
    notes.append("PASS 1: Bottom-up AncestorOf enumeration is correct.")

    # 2) Top-down enumerates the same AncestorOf
    td, _ = solve_topdown(PROGRAM, [atom(Holds2, AncestorOf, X, Y)])
    anc_td = {(deref(X,s), deref(Y,s)) for s in td}
    check(anc_td == expected, "Top-down AncestorOf enumeration mismatch.")
    notes.append("PASS 2: Top-down AncestorOf enumeration is correct.")

    # 3) Engines agree on witnesses
    R = Var("R")
    bu_w = match_against_facts([atom(Holds2, R, "Socrates","Lamprocles"), atom(Leq, R, AncestorOf)], facts)
    td_w, _ = solve_topdown(PROGRAM, [atom(Holds2, R, "Socrates","Lamprocles"), atom(Leq, R, AncestorOf)])
    w1 = {deref(R,s) for s in bu_w}; w2 = {deref(R,s) for s in td_w}
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

    # 7) Auto-chooser behavior
    e1, _, _ = ask([atom(Holds2, AncestorOf, Var("X"), Var("Y"))])
    e2, _, _ = ask([atom(Holds2, AncestorOf, "Socrates", "Lamprocles")])
    check(e1 == "bottomup" and e2 == "topdown", "Engine chooser mismatch.")
    notes.append("PASS 7: Engine chooser behaves as intended.")

    # 8) leq reflexive
    for r in [FatherOf, ParentOf, TeacherOf, AncestorOf]:
        check(ask([atom(Leq, r, r)])[1], f"Reflexivity of leq failed for {local(r)}.")
    notes.append("PASS 8: leq reflexivity holds.")

    # 9) leq_strict transitivity (FatherOf ⊆ AncestorOf via ParentOf)
    check(ask([atom(LeqStrict, FatherOf, AncestorOf)])[1],
          "leq_strict transitivity failed.")
    notes.append("PASS 9: leq_strict transitivity holds.")

    # 10) Standardize-apart stability
    ok1 = ask([atom(Holds2, AncestorOf, "Sophroniscus", "Lamprocles")])[1]
    ok2 = ask([atom(Holds2, AncestorOf, "Sophroniscus", "Lamprocles")])[1]
    check(ok1 and ok2, "Repeated top-down query should remain true.")
    notes.append("PASS 10: Standardize-apart avoids capture across proofs.")

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

