#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
README (plain text)
===================
Purpose
-------
A small CASE module for `eyezero.py` that reconstructs the classic syllogism

    All humans are mortal.
    Socrates is a human.
    --------------------
    Therefore, Socrates is mortal.

in the **Hayes–Menzel** style, *and* adds a slightly “second-order-looking”
statement that still lives in first-order logic over two sorts (NAME, IND).

Class predicates like Human, Animal, Mortal are treated as **names**
(intensions), and application is mediated by a fixed unary predicate

    ex:holds1(C, x)      # "x is in the extension of class-name C"

Subclassing is also expressed at the *name level*:

    ex:SubClassOf(C, D)  # inclusion over class names (intensions)

Membership lifts along subclass chains via rules like:

    holds1(D, x) :- leq_strict(C, D), holds1(C, x).

Thus “quantification over predicates” (which class does Socrates belong to?)
is really quantification over **names**; the underlying logic is first-order
over a fixed vocabulary (holds1, SubClassOf, leq, ...).

What is modeled
---------------
Domain (individuals, as strings):
  • "Socrates"

Class names (intensions):
  • ex:Human, ex:Mammal, ex:Animal, ex:Mortal
  • ex:Greek, ex:Philosopher

Facts:
  • holds1(Human, Socrates).
  • holds1(Greek, Socrates).
  • holds1(Philosopher, Socrates).
  • SubClassOf(Human, Mammal).
  • SubClassOf(Mammal, Animal).
  • SubClassOf(Animal, Mortal).

Rules:
  1) leq_strict(C,D) :- SubClassOf(C,D).
  2) leq_strict(C,D) :- SubClassOf(C,E), leq_strict(E,D).  (transitive closure)
  3) leq(C,C).                                           (reflexive)
  4) leq(C,D) :- leq_strict(C,D).
  5) holds1(D,x) :- leq_strict(C,D), holds1(C,x).        (membership lifting)

Queries
-------
Q1) Which named classes C satisfy holds1(C,"Socrates")?
Q2) Is there some C with leq(C,Mortal) ∧ holds1(C,"Socrates")?  (witnesses)
Q3) Second-order-looking:

      ∀C: (leq(C,Mortal) ∧ holds1(C,Socrates)) → ∃x holds1(C,x)

    i.e. “every subclass of Mortal that contains Socrates is non-empty”.

This *looks* like quantification over predicates C, but in fact we quantify
over **class names** in the NAME sort, with application always via holds1.

How to run
----------
Standalone:

    python3 socrates_mortal.py

Via EyeZero:

    python3 eyezero.py socrates_mortal.py

What it prints
--------------
Model → Question → Answer → Reason why → Check (harness)

The harness runs several checks:
  • that leq behaves as a reflexive-transitive closure of SubClassOf,
  • that Socrates is in every superclass of Human,
  • that no “unrelated” class is derived,
  • that bottom-up and top-down engines of EyeZero agree,
  • and that the ∀C “subclass of Mortal” non-emptiness property holds.
"""

from __future__ import annotations

from typing import List, Set

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
Holds1    = EX + "holds1"      # (NAME, IND) – class-name application
SubClassOf = EX + "SubClassOf" # (NAME, NAME) – subclass over names
LeqStrict = EX + "leq_strict"  # (NAME, NAME) – transitive closure of SubClassOf
Leq       = EX + "leq"         # (NAME, NAME) – reflexive-transitive closure

# Class names (intensions)
Human       = EX + "Human"
Mammal      = EX + "Mammal"
Animal      = EX + "Animal"
Mortal      = EX + "Mortal"
Greek       = EX + "Greek"
Philosopher = EX + "Philosopher"

ALL_CLASSES = [Human, Mammal, Animal, Mortal, Greek, Philosopher]

# Individuals
Socrates = "Socrates"

# Signature for EyeZero (NAME vs IND per argument position)
SIGNATURE: Signature = {
    Holds1:    (NAME, IND),
    SubClassOf:(NAME, NAME),
    LeqStrict: (NAME, NAME),
    Leq:       (NAME, NAME),
}

# Program: facts + rules (Prolog-style)
PROGRAM: List[Clause] = []

def h1(C, x) -> Clause:
    return fact(Holds1, C, x)

# Base membership facts
PROGRAM += [
    h1(Human,       Socrates),
    h1(Greek,       Socrates),
    h1(Philosopher, Socrates),
]

# SubClassOf facts (name-level inclusions)
PROGRAM += [
    fact(SubClassOf, Human,  Mammal),
    fact(SubClassOf, Mammal, Animal),
    fact(SubClassOf, Animal, Mortal),
]

# Rules: leq_strict = transitive closure of SubClassOf
C, D, E = Var("C"), Var("D"), Var("E")
PROGRAM += [
    Clause(atom(LeqStrict, C, D), [atom(SubClassOf, C, D)]),
]
C, D, E = Var("C"), Var("D"), Var("E")
PROGRAM += [
    Clause(atom(LeqStrict, C, D), [atom(SubClassOf, C, E), atom(LeqStrict, E, D)]),
]

# leq = reflexive + leq_strict
C = Var("C")
PROGRAM.append(Clause(atom(Leq, C, C), []))
C, D = Var("C"), Var("D")
PROGRAM.append(Clause(atom(Leq, C, D), [atom(LeqStrict, C, D)]))

# Membership lifting along subclass chains:
#   if C ⊆ D and x ∈ C then x ∈ D
C, D, X = Var("C"), Var("D"), Var("X")
PROGRAM.append(
    Clause(
        atom(Holds1, D, X),
        [atom(LeqStrict, C, D), atom(Holds1, C, X)],
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
      • holds1(C, "Socrates") with C free  → bottom-up (enumeration).
      • fully unbound atomic goals         → bottom-up.
      • otherwise                          → top-down (tabled).
    """
    for g in goals:
        if g.pred == Holds1 and len(g.args) == 2 and g.args[1] == Socrates and _is_var(g.args[0]):
            return "bottomup"
        if all(_is_var(a) for a in g.args):
            return "bottomup"
    return "topdown"

def ask(goals: List[Atom], step_limit: int = 10000):
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
    print(f"Individuals D = ['{Socrates}']")
    print()
    print("Fixed predicates (signature)")
    print("----------------------------")
    print("• ex:holds1(C,x)     — unary application; sorts: (NAME, IND)")
    print("• ex:SubClassOf(C,D) — inclusion over class *names*; sorts: (NAME, NAME)")
    print("• ex:leq_strict / ex:leq — transitive / reflexive-transitive closure on names")
    print()
    print("Class names (intensions)")
    print("------------------------")
    print("Human, Mammal, Animal, Mortal, Greek, Philosopher")
    print()
    print("Facts")
    print("-----")
    print(f"• holds1(Human, {Socrates}).")
    print(f"• holds1(Greek, {Socrates}).")
    print(f"• holds1(Philosopher, {Socrates}).")
    print("• Human ⊆ Mammal ⊆ Animal ⊆ Mortal (via SubClassOf).")
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
    print("if x is a Human, and Human ⊆ Mammal ⊆ Animal ⊆ Mortal, then x is")
    print("also a Mammal, Animal, and Mortal.\n")

def print_question() -> None:
    print("Question")
    print("========")
    print(f"Q1) Which named classes C satisfy holds1(C,{Socrates})?               [auto engine]")
    print(f"Q2) ∃C: leq(C,Mortal) ∧ holds1(C,{Socrates}) ?  (witness class names)  [auto engine]")
    print(f"Q3) ∀C: (leq(C,Mortal) ∧ holds1(C,{Socrates})) → ∃x holds1(C,x) ?       [meta + engine]\n")

# ─────────────────────────────────────────────────────────────────────────────
# Queries
# ─────────────────────────────────────────────────────────────────────────────

def run_queries():
    # Q1: enumerate all classes C with holds1(C,Socrates)
    Cvar = Var("C")
    eng1, sols1, _ = ask([atom(Holds1, Cvar, Socrates)])
    classes = {deref(Cvar, s) for s in sols1 if isinstance(deref(Cvar, s), str)}

    # Q2: witnesses C with leq(C,Mortal) ∧ holds1(C,Socrates)
    Cvar = Var("C")
    eng2, sols2, _ = ask([atom(Leq, Cvar, Mortal), atom(Holds1, Cvar, Socrates)])
    witnesses = {deref(Cvar, s) for s in sols2 if isinstance(deref(Cvar, s), str)}

    # Q3: second-order-looking property:
    #     ∀C: (leq(C,Mortal) ∧ holds1(C,Socrates)) → ∃x holds1(C,x)
    second_order_ok = True
    for Cname in ALL_CLASSES:
        # antecedent: leq(C,Mortal) ∧ holds1(C,Socrates)
        _, cond_sols, _ = ask([atom(Leq, Cname, Mortal), atom(Holds1, Cname, Socrates)])
        if cond_sols:
            # consequent: ∃x holds1(C,x)
            Xvar = Var("X")
            _, mem_sols, _ = ask([atom(Holds1, Cname, Xvar)])
            if not mem_sols:
                second_order_ok = False
                break

    return (
        ("Q1", eng1, classes, "n/a"),
        ("Q2", eng2, witnesses, "n/a"),
        ("Q3", "mixed", second_order_ok, "n/a"),
    )

# ─────────────────────────────────────────────────────────────────────────────
# Answers + Reason
# ─────────────────────────────────────────────────────────────────────────────

def print_answer(res1, res2, res3) -> None:
    print("Answer")
    print("======")
    tag1, eng1, classes, _ = res1
    tag2, eng2, witnesses, _ = res2
    tag3, eng3, second_ok, _ = res3

    print(f"{tag1}) Engine: {eng1} → Classes C with holds1(C,{Socrates}): {fmt_class_set(classes)}")
    print(f"{tag2}) Engine: {eng2} → Witnesses C with leq(C,Mortal) and holds1(C,{Socrates}): {fmt_class_set(witnesses)}")
    print(f"{tag3}) Engine: {eng3} → "
          f"∀C (C⊆Mortal ∧ C({Socrates}) ⇒ ∃x C(x)) holds: {'Yes' if second_ok else 'No'}\n")

def print_reason(eng1: str, eng2: str) -> None:
    # eng1, eng2 kept for compatibility with eyezero main; not used directly.
    print("Reason why")
    print("==========")
    print("• We treat class symbols (Human, Mammal, Animal, Mortal, ...) as **names**.")
    print("  Application is the fixed predicate holds1(C,x), and subclassing lives at")
    print("  the name level via SubClassOf and its closure leq/leq_strict.")
    print("• The rules:")
    print("    leq_strict(C,D) :- SubClassOf(C,D).")
    print("    leq_strict(C,D) :- SubClassOf(C,E), leq_strict(E,D).")
    print("    leq(C,C).   leq(C,D) :- leq_strict(C,D).")
    print("    holds1(D,x) :- leq_strict(C,D), holds1(C,x).")
    print("  ensure that membership propagates along subclass chains.")
    print(f"• Since we have holds1(Human,{Socrates}) and Human ⊆ Mammal ⊆ Animal ⊆ Mortal,")
    print(f"  the engine derives holds1(Mortal,{Socrates}). This is the formal counterpart")
    print('  of the syllogism "All humans are mortal; Socrates is a human; therefore,')
    print('  Socrates is mortal."')
    print("• The third question looks “second-order” because it quantifies over class-names C:")
    print("    ∀C (C⊆Mortal ∧ C(Socrates) ⇒ ∃x C(x)).")
    print("  But semantically it is just first-order over two sorts (NAME, IND),")
    print("  with C ranging over names and application always via holds1.\n")

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

    # 1) SubClassOf chain as expected
    facts_bu, _ = solve_bottomup(PROGRAM, SIGNATURE)
    chain = {(c, d) for (c, d) in facts_bu.get(SubClassOf, set())}
    expected_chain = {
        (Human, Mammal),
        (Mammal, Animal),
        (Animal, Mortal),
    }
    check(chain == expected_chain, "SubClassOf chain mismatch.")
    notes.append("PASS 1: SubClassOf chain Human ⊆ Mammal ⊆ Animal ⊆ Mortal.")

    # 2) Reflexivity of leq for all class names
    for Cname in ALL_CLASSES:
        sols, _ = solve_topdown(PROGRAM, [atom(Leq, Cname, Cname)])
        check(bool(sols), f"Reflexivity of leq failed for {local(Cname)}.")
    notes.append("PASS 2: leq is reflexive on all class names.")

    # 3) Transitive closure: leq(Human,Mortal) holds
    sols, _ = solve_topdown(PROGRAM, [atom(Leq, Human, Mortal)])
    check(bool(sols), "Transitive closure (Human ⊆ Mortal) failed.")
    notes.append("PASS 3: leq(Human,Mortal) holds via closure.")

    # 4) Socrates is a Human and Mortal
    sols_h, _ = solve_topdown(PROGRAM, [atom(Holds1, Human, Socrates)])
    sols_m, _ = solve_topdown(PROGRAM, [atom(Holds1, Mortal, Socrates)])
    check(bool(sols_h) and bool(sols_m), "Socrates should be both Human and Mortal.")
    notes.append("PASS 4: Socrates is both Human and Mortal.")

    # 5) Socrates is an Animal and a Mammal (intermediate classes)
    for Cname in [Mammal, Animal]:
        sols_c, _ = solve_topdown(PROGRAM, [atom(Holds1, Cname, Socrates)])
        check(bool(sols_c), f"Socrates should be a {local(Cname)}.")
    notes.append("PASS 5: Socrates is Mammal and Animal via subclass chain.")

    # 6) Greek and Philosopher do NOT entail Mortal via subclass rules
    sols_g, _ = solve_topdown(PROGRAM, [atom(Leq, Greek, Mortal)])
    sols_p, _ = solve_topdown(PROGRAM, [atom(Leq, Philosopher, Mortal)])
    check(not sols_g and not sols_p, "Greek or Philosopher should not be ≤ Mortal in leq.")
    notes.append("PASS 6: No unintended leq relation from Greek/Philosopher to Mortal.")

    # 7) However, Socrates is Greek and Philosopher by base facts
    for Cname in [Greek, Philosopher]:
        sols_c, _ = solve_topdown(PROGRAM, [atom(Holds1, Cname, Socrates)])
        check(bool(sols_c), f"Socrates should be {local(Cname)} by base fact.")
    notes.append("PASS 7: Socrates is Greek and Philosopher as given.")

    # 8) Top-down vs bottom-up enumeration of all classes for Socrates
    Cvar = Var("C")
    sols_td, _ = solve_topdown(PROGRAM, [atom(Holds1, Cvar, Socrates)])
    enum_td = {deref(Cvar, s) for s in sols_td if isinstance(deref(Cvar, s), str)}
    facts_bu2, _ = solve_bottomup(PROGRAM, SIGNATURE)
    sols_bu = match_against_facts([atom(Holds1, Cvar, Socrates)], facts_bu2)
    enum_bu = {deref(Cvar, s) for s in sols_bu if isinstance(deref(Cvar, s), str)}
    check(enum_td == enum_bu, "Top-down and bottom-up disagree on Socrates' classes.")
    notes.append("PASS 8: Top-down and bottom-up agree on Socrates' class memberships.")

    # 9) Universal property: for every C with leq(Human,C), holds1(C,Socrates)
    for Cname in ALL_CLASSES:
        sols_leq, _ = solve_topdown(PROGRAM, [atom(Leq, Human, Cname)])
        if sols_leq:
            sols_mem, _ = solve_topdown(PROGRAM, [atom(Holds1, Cname, Socrates)])
            check(bool(sols_mem), f"Universal property failed at {local(Cname)}.")
    notes.append("PASS 9: For all C with Human ⊆ C, Socrates ∈ C.")

    # 10) Determinism: recomputing bottom-up yields the same Holds1 facts
    facts_bu3, _ = solve_bottomup(PROGRAM, SIGNATURE)
    check(facts_bu2.get(Holds1, set()) == facts_bu3.get(Holds1, set()),
          "Bottom-up closure not deterministic or stable.")
    notes.append("PASS 10: Bottom-up closure is stable and deterministic.")

    # 11) Second-order-looking property: every subclass of Mortal that contains Socrates is non-empty
    second_ok = True
    for Cname in ALL_CLASSES:
        sols_leq, _ = solve_topdown(PROGRAM, [atom(Leq, Cname, Mortal)])
        sols_mem_S, _ = solve_topdown(PROGRAM, [atom(Holds1, Cname, Socrates)])
        if sols_leq and sols_mem_S:
            Xvar = Var("X")
            sols_any, _ = solve_topdown(PROGRAM, [atom(Holds1, Cname, Xvar)])
            if not sols_any:
                second_ok = False
                break
    check(second_ok, "Second-order-looking property failed for some subclass of Mortal.")
    notes.append("PASS 11: Every subclass of Mortal that contains Socrates is non-empty.")

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

