#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
README (plain text)
===================
Purpose
-------
A CASE module for `eyezero.py` that constructs the **Greek people** example

    Greek(Socrates)
    Philosopher(Socrates), Philosopher(Plato)
    Human(Plato), Human(Aristotle)
    Olympian(Zeus)
    Greek ⊆ Human, Philosopher ⊆ Human, Human ⊆ Mortal

using the Hayes–Menzel idea:

  • Class predicates like Greek, Human, Mortal are **names** (intensions).
  • Membership is mediated by a fixed unary predicate

        ex:holds1(C,x)   # "x is in the extension of the class named by C"

  • Subclassing is mediated by a fixed binary predicate

        ex:SubClassOf(C,D)   # "extension(C) ⊆ extension(D)"

Quantifying over predicates (e.g. “∃P P(Socrates) ∧ P ⊆ Mortal”) becomes
quantifying over **names** P in the NAME-sort; the underlying logic is still
first-order over a fixed vocabulary (holds1, SubClassOf, leq, ...).

Model
-----
Individuals (IND-sort, as strings):
  D = { "Socrates", "Plato", "Aristotle", "Zeus" }

Class names (NAME-sort, intensions):
  ex:Greek, ex:Philosopher, ex:Human, ex:Mortal, ex:Olympian

Base membership facts (via holds1):
  Greek(Socrates)
  Philosopher(Socrates), Philosopher(Plato)
  Human(Plato), Human(Aristotle)
  Mortal(–)   # empty initially
  Olympian(Zeus)

Subclass facts (via SubClassOf):
  Greek ⊆ Human
  Philosopher ⊆ Human
  Human ⊆ Mortal

Rules (first-order Horn clauses)
--------------------------------
We work with two sorts (NAME, IND) and predicates:

  ex:holds1(C,x)      # (NAME, IND)
  ex:SubClassOf(C,D)  # (NAME, NAME)
  ex:leq_strict(C,D)  # (NAME, NAME) – transitive closure
  ex:leq(C,D)         # (NAME, NAME) – reflexive-transitive closure

Rules:

  1) leq_strict(C,D) :- SubClassOf(C,D).
  2) leq_strict(C,D) :- SubClassOf(C,E), leq_strict(E,D).
  3) leq(C,C).
  4) leq(C,D) :- leq_strict(C,D).
  5) holds1(D,x) :- leq_strict(C,D), holds1(C,x).   # membership lifting

EyeZero’s bottom-up semantics computes the least fixed point of this program.

Typical questions
-----------------
Q1) ∃P: P(Socrates) ∧ P ⊆ Mortal ?        (list witness class-names)
Q2) ∀P: (P ⊆ Human ∧ P(Socrates)) → Mortal(Socrates) ?
Q3) List all classes that Socrates belongs to after closure.

How to run
----------
Standalone:

    python3 greek_people.py

Via EyeZero:

    python3 eyezero.py greek_people.py

Output
------
Model → Question → Answer → Reason why → Check (harness)
"""

from __future__ import annotations

from typing import List, Set, Tuple

from eyezero import (
    Var, Atom, Clause, atom, fact,
    solve_topdown, solve_bottomup, match_against_facts,
    NAME, IND, Signature, deref,
)

# ─────────────────────────────────────────────────────────────────────────────
# Names, signature, and program
# ─────────────────────────────────────────────────────────────────────────────

EX = "ex:"

# Application and meta-predicates
Holds1     = EX + "holds1"       # (NAME, IND) – class-name application
SubClassOf = EX + "SubClassOf"   # (NAME, NAME) – subclass over names
LeqStrict  = EX + "leq_strict"   # (NAME, NAME) – transitive closure of SubClassOf
Leq        = EX + "leq"          # (NAME, NAME) – reflexive-transitive closure

# Class names (intensions)
Greek       = EX + "Greek"
Philosopher = EX + "Philosopher"
Human       = EX + "Human"
Mortal      = EX + "Mortal"
Olympian    = EX + "Olympian"

ALL_CLASSES = [Greek, Philosopher, Human, Mortal, Olympian]

# Individuals
Socrates  = "Socrates"
Plato     = "Plato"
Aristotle = "Aristotle"
Zeus      = "Zeus"

D: Tuple[str, ...] = (Socrates, Plato, Aristotle, Zeus)

# Signature for EyeZero (NAME vs IND per argument position)
SIGNATURE: Signature = {
    Holds1:     (NAME, IND),
    SubClassOf: (NAME, NAME),
    LeqStrict:  (NAME, NAME),
    Leq:        (NAME, NAME),
}

# Program: facts + rules (Prolog-style)
PROGRAM: List[Clause] = []

def h1(C, x) -> Clause:
    return fact(Holds1, C, x)

# Base membership facts
PROGRAM += [
    h1(Greek,       Socrates),
    h1(Philosopher, Socrates),
    h1(Philosopher, Plato),
    h1(Human,       Plato),
    h1(Human,       Aristotle),
    h1(Olympian,    Zeus),
    # Mortal has no base members; all Mortal facts are derived.
]

# SubClassOf facts (name-level inclusions)
PROGRAM += [
    fact(SubClassOf, Greek,       Human),
    fact(SubClassOf, Philosopher, Human),
    fact(SubClassOf, Human,       Mortal),
]

# Rules: leq_strict = transitive closure of SubClassOf
C, Dv, Ev = Var("C"), Var("D"), Var("E")
PROGRAM += [
    Clause(atom(LeqStrict, C, Dv), [atom(SubClassOf, C, Dv)]),
]
C, Dv, Ev = Var("C"), Var("D"), Var("E")
PROGRAM += [
    Clause(atom(LeqStrict, C, Dv), [atom(SubClassOf, C, Ev), atom(LeqStrict, Ev, Dv)]),
]

# leq = reflexive + leq_strict
C = Var("C")
PROGRAM.append(Clause(atom(Leq, C, C), []))
C, Dv = Var("C"), Var("D")
PROGRAM.append(Clause(atom(Leq, C, Dv), [atom(LeqStrict, C, Dv)]))

# Membership lifting along subclass chains:
#   if C ⊆ D and x ∈ C then x ∈ D
C, Dv, X = Var("C"), Var("D"), Var("X")
PROGRAM.append(
    Clause(
        atom(Holds1, Dv, X),
        [atom(LeqStrict, C, Dv), atom(Holds1, C, X)],
    )
)

# ─────────────────────────────────────────────────────────────────────────────
# Engine glue (auto-chooser + ask)
# ─────────────────────────────────────────────────────────────────────────────

def _is_var(t) -> bool:
    return isinstance(t, Var)

def choose_engine(goals: List[Atom]) -> str:
    """
    Heuristic:
      • holds1(C, Socrates) with C free → bottom-up (enumeration).
      • fully unbound atomic goals      → bottom-up.
      • otherwise                       → top-down (tabled).
    """
    for g in goals:
        if g.pred == Holds1 and len(g.args) == 2 and g.args[1] == Socrates and _is_var(g.args[0]):
            return "bottomup"
        if all(_is_var(a) for a in g.args):
            return "bottomup"
    return "topdown"

def ask(goals: List[Atom], step_limit: int = 10000):
    """
    Ask the engine a conjunctive query.

    Returns: (engine_tag, solutions, metric)
      - engine_tag ∈ {"bottomup","topdown"}
      - metric is number of rounds/steps (we don't print it here).
    """
    engine = choose_engine(goals)
    if engine == "topdown":
        sols, steps = solve_topdown(PROGRAM, goals, step_limit=step_limit)
        return engine, sols, steps
    else:
        facts, rounds = solve_bottomup(PROGRAM, SIGNATURE)
        sols = match_against_facts(goals, facts)
        return engine, sols, rounds

# ─────────────────────────────────────────────────────────────────────────────
# Pretty helpers
# ─────────────────────────────────────────────────────────────────────────────

def local(name: str) -> str:
    """Drop 'ex:' for pretty printing."""
    return name.split(":", 1)[1] if ":" in name else name

def fmt_class_set(classes: Set[str]) -> str:
    if not classes:
        return "∅"
    return "{" + ", ".join(sorted(local(c) for c in classes)) + "}"

# ─────────────────────────────────────────────────────────────────────────────
# Model + Question
# ─────────────────────────────────────────────────────────────────────────────

def print_model() -> None:
    print("Model")
    print("=====")
    print(f"Individuals D = {list(D)}")
    print()
    print("Fixed predicates (signature)")
    print("----------------------------")
    print("• ex:holds1(C,x)     — unary application; sorts: (NAME, IND)")
    print("• ex:SubClassOf(C,D) — inclusion over class *names*; sorts: (NAME, NAME)")
    print("• ex:leq_strict / ex:leq — transitive / reflexive-transitive closure on names")
    print()
    print("Class names (intensions)")
    print("------------------------")
    print("Greek, Philosopher, Human, Mortal, Olympian")
    print()
    print("Base facts")
    print("----------")
    print("Membership:")
    print(f"  Greek({Socrates})")
    print(f"  Philosopher({Socrates}), Philosopher({Plato})")
    print(f"  Human({Plato}), Human({Aristotle})")
    print(f"  Olympian({Zeus})")
    print("Subclass:")
    print("  Greek ⊆ Human, Philosopher ⊆ Human, Human ⊆ Mortal")
    print()
    print("Rules")
    print("-----")
    print("1) leq_strict(C,D) :- SubClassOf(C,D).")
    print("2) leq_strict(C,D) :- SubClassOf(C,E), leq_strict(E,D).")
    print("3) leq(C,C).")
    print("4) leq(C,D) :- leq_strict(C,D).")
    print("5) holds1(D,x) :- leq_strict(C,D), holds1(C,x).")
    print()
    print("These rules propagate membership along subclass chains. Intuitively,")
    print("if x is Greek or Philosopher, and Greek/Philosopher ⊆ Human ⊆ Mortal,")
    print("then x is Human and Mortal as well.\n")

def print_question() -> None:
    print("Question")
    print("========")
    print(f"Q1) ∃P: holds1(P,{Socrates}) ∧ leq(P,Mortal) ?  (witness class-names)        [auto engine]")
    print(f"Q2) ∀P: (leq(P,Human) ∧ holds1(P,{Socrates})) → holds1(Mortal,{Socrates}) ?  [auto engine]")
    print(f"Q3) List all classes C with holds1(C,{Socrates}) after closure.             [auto engine]\n")

# ─────────────────────────────────────────────────────────────────────────────
# Queries
# ─────────────────────────────────────────────────────────────────────────────

def run_queries():
    # Q1: witnesses P with leq(P,Mortal) ∧ holds1(P,Socrates)
    P = Var("P")
    eng1, sols1, _ = ask([atom(Leq, P, Mortal), atom(Holds1, P, Socrates)])
    witnesses = {deref(P, s) for s in sols1 if isinstance(deref(P, s), str)}

    # Q2: Universal: ∀P ((P⊆Human ∧ P(Socrates)) → Mortal(Socrates))
    # Implemented by searching for a counterexample P.
    universal_ok = True
    for cname in ALL_CLASSES:
        _, cond_sols, _ = ask([atom(Leq, cname, Human), atom(Holds1, cname, Socrates)])
        if cond_sols:
            # antecedent holds; check consequent holds1(Mortal,Socrates)
            _, msols, _ = ask([atom(Holds1, Mortal, Socrates)])
            if not msols:
                universal_ok = False
                break

    # Q3: all classes Socrates belongs to
    Cvar = Var("C")
    eng3, sols3, _ = ask([atom(Holds1, Cvar, Socrates)])
    classes = {deref(Cvar, s) for s in sols3 if isinstance(deref(Cvar, s), str)}

    return (
        ("Q1", eng1, witnesses, "n/a"),
        ("Q2", "mixed", universal_ok, "n/a"),
        ("Q3", eng3, classes, "n/a"),
    )

# ─────────────────────────────────────────────────────────────────────────────
# Answers + Reason
# ─────────────────────────────────────────────────────────────────────────────

def print_answer(res1, res2, res3) -> None:
    print("Answer")
    print("======")
    tag1, eng1, witnesses, _ = res1
    tag2, eng2, universal_ok, _ = res2
    tag3, eng3, classes, _ = res3

    print(f"{tag1}) Engine: {eng1} → Witnesses P with leq(P,Mortal) and holds1(P,{Socrates}): {fmt_class_set(witnesses)}")
    print(f"{tag2}) Engine: {eng2} → ∀P ((P⊆Human ∧ P({Socrates})) ⇒ Mortal({Socrates})): {'Yes' if universal_ok else 'No'}")
    print(f"{tag3}) Engine: {eng3} → Classes C with holds1(C,{Socrates}): {fmt_class_set(classes)}\n")

def print_reason(eng1: str, eng2: str) -> None:
    # eng1, eng2 kept for compatibility with eyezero main; not used directly.
    print("Reason why")
    print("==========")
    print("• Class symbols (Greek, Philosopher, Human, Mortal, Olympian) are **names**.")
    print("  Application is via holds1(C,x); subclassing is via SubClassOf(C,D) and its")
    print("  closure leq/leq_strict over names.")
    print("• From the base facts and subclass rules we get:")
    print("    Greek ⊆ Human ⊆ Mortal")
    print("    Philosopher ⊆ Human ⊆ Mortal")
    print(f"  so Greek(Socrates) and Philosopher(Socrates) imply Human({Socrates}) and Mortal({Socrates}).")
    print("• Q1 finds all class-names P such that P(Socrates) and P ⊆ Mortal; these include")
    print("  Greek, Philosopher, and Mortal itself.")
    print("• Q2 checks a universal over class-names P: any P that is a subclass of Human")
    print(f"  and that contains {Socrates} forces Mortal({Socrates}). This follows because")
    print("  Human ⊆ Mortal, and membership lifts along subclass chains.")
    print("• Q3 simply enumerates all classes Socrates belongs to in the least fixed point,")
    print("  which are exactly Greek, Philosopher, Human, and Mortal.\n")

# ─────────────────────────────────────────────────────────────────────────────
# Check (harness)
# ─────────────────────────────────────────────────────────────────────────────

class CheckFailure(AssertionError):
    pass

def check(c: bool, msg: str) -> None:
    if not c:
        raise CheckFailure(msg)

def run_checks() -> List[str]:
    notes: List[str] = []

    # Bottom-up closure once, shared by several checks
    facts_bu, _ = solve_bottomup(PROGRAM, SIGNATURE)
    mem = facts_bu.get(Holds1, set())
    sub = facts_bu.get(SubClassOf, set())
    leq_s = facts_bu.get(LeqStrict, set())
    leq   = facts_bu.get(Leq, set())

    # Expected membership facts after closure
    expected_mem = {
        (Greek,       Socrates),
        (Philosopher, Socrates),
        (Philosopher, Plato),
        (Human,       Plato),
        (Human,       Aristotle),
        (Human,       Socrates),
        (Mortal,      Socrates),
        (Mortal,      Plato),
        (Mortal,      Aristotle),
        (Olympian,    Zeus),
    }

    # 1) SubClassOf contains intended rules
    intended_sub = {
        (Greek,       Human),
        (Philosopher, Human),
        (Human,       Mortal),
    }
    check(intended_sub.issubset(sub), "Intended subclass rules missing.")
    notes.append("PASS 1: Intended subclass rules present.")

    # 2) leq_strict contains transitive closure edges up to Mortal
    check((Greek, Mortal) in leq_s and (Philosopher, Mortal) in leq_s,
          "Transitive subclass closure to Mortal missing.")
    notes.append("PASS 2: leq_strict includes Greek⊆Mortal and Philosopher⊆Mortal.")

    # 3) leq is reflexive on all class names
    for cname in ALL_CLASSES:
        check((cname, cname) in leq, f"Reflexivity of leq failed for {local(cname)}.")
    notes.append("PASS 3: leq is reflexive on all class names.")

    # 4) leq(Human,Mortal) holds
    check((Human, Mortal) in leq, "leq(Human,Mortal) should hold.")
    notes.append("PASS 4: leq(Human,Mortal) via closure.")

    # 5) Membership closure matches expected 10 facts
    check(mem == expected_mem, f"Membership closure mismatch: got {mem}.")
    notes.append("PASS 5: Membership closure for holds1 is exactly the expected 10 facts.")

    # 6) Zeus is not Mortal
    check((Mortal, Zeus) not in mem, "Zeus should not be inferred Mortal.")
    notes.append("PASS 6: Zeus is not inferred Mortal.")

    # 7) Socrates is Human and Mortal
    check((Human, Socrates) in mem and (Mortal, Socrates) in mem,
          "Socrates should be Human and Mortal.")
    notes.append("PASS 7: Socrates is Human and Mortal via subclass propagation.")

    # 8) existence witnesses for Q1 include Greek, Philosopher, and Mortal
    P = Var("P")
    sols_q1_td, _ = solve_topdown(PROGRAM, [atom(Leq, P, Mortal), atom(Holds1, P, Socrates)])
    w_td = {deref(P, s) for s in sols_q1_td if isinstance(deref(P, s), str)}
    for c in (Greek, Philosopher, Mortal):
        check(c in w_td, f"{local(c)} should be a witness for ∃P.")
    notes.append("PASS 8: Greek, Philosopher, Mortal are witnesses for Q1.")

    # 9) Q2 universal holds
    universal_ok = True
    for cname in ALL_CLASSES:
        sols_leq, _ = solve_topdown(PROGRAM, [atom(Leq, cname, Human)])
        sols_mem_S, _ = solve_topdown(PROGRAM, [atom(Holds1, cname, Socrates)])
        if sols_leq and sols_mem_S:
            sols_mort_S, _ = solve_topdown(PROGRAM, [atom(Holds1, Mortal, Socrates)])
            if not sols_mort_S:
                universal_ok = False
                break
    check(universal_ok, "Universal property in Q2 failed.")
    notes.append("PASS 9: Universal property for subclasses of Human holds.")

    # 10) Classes of Socrates are exactly {Greek, Philosopher, Human, Mortal}
    classes_S = {C for (C, a) in mem if a == Socrates}
    expected_S = {Greek, Philosopher, Human, Mortal}
    check(classes_S == expected_S, f"Socrates class set mismatch: {classes_S}")
    notes.append("PASS 10: Classes(Socrates) = {Greek, Philosopher, Human, Mortal}.")

    # 11) Top-down and bottom-up agree on Socrates' classes
    Cvar = Var("C")
    sols_td_S, _ = solve_topdown(PROGRAM, [atom(Holds1, Cvar, Socrates)])
    td_set = {deref(Cvar, s) for s in sols_td_S if isinstance(deref(Cvar, s), str)}
    check(td_set == classes_S, "Top-down and bottom-up disagree on Socrates' classes.")
    notes.append("PASS 11: Top-down and bottom-up agree for Socrates.")

    # 12) Bottom-up closure is deterministic (two runs the same)
    facts_bu2, _ = solve_bottomup(PROGRAM, SIGNATURE)
    check(facts_bu2.get(Holds1, set()) == mem, "Bottom-up closure changed between runs.")
    notes.append("PASS 12: Bottom-up closure is stable and deterministic.")

    return notes

# ─────────────────────────────────────────────────────────────────────────────
# Main
# ─────────────────────────────────────────────────────────────────────────────

def main() -> None:
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

