#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Ershov's mixed computation — Herbrand logic edition (parity over Peano terms)

What this shows
---------------
In Herbrand semantics, terms are built from symbols. Here our signature is:

  • Constants: 0
  • Functions: s/1     (successor)
  • Predicates: even/1

Program (two Horn clauses):
  1) even(0).
  2) even(s(s(X))) :- even(X).

So, in the least Herbrand model:
  even(0) is True; even(s(0)) is False; even(s(s(0))) is True; etc.

Mixed computation idea:
  - Static: the signature and the Horn program above.
  - Dynamic: a ground term t = s^n(0), provided later (we encode it by n ≥ 0).
  - At "mix time" we observe that the only recursive clause consumes TWO
    layers of s/1. Hence the *residual program* only needs to peel n by 2
    until it reaches 0 or 1 — no symbolic unification or term construction.

This script prints:
  1) "Answer" — the generated residual function and a sample run,
  2) "Reason why" — how the program was specialized,
  3) "Check (harness)" — a verifier comparing residual vs. a generic
     Herbrand-term evaluator on many inputs.

Run with Python 3.x; no dependencies.
"""

from dataclasses import dataclass
from typing import Callable, List, Tuple
import random


# ───────────────────────── Herbrand terms for the generic path ─────────────────────────

class Term:  # base class
    pass

@dataclass(frozen=True)
class Zero(Term):  # constant 0
    def __str__(self): return "0"

@dataclass(frozen=True)
class S(Term):     # unary function s/1
    t: Term
    def __str__(self): return f"s({self.t})"

def int_to_term(n: int) -> Term:
    """Encode s^n(0) as a Herbrand term."""
    if n < 0: raise ValueError("n must be ≥ 0")
    t: Term = Zero()
    for _ in range(n):
        t = S(t)
    return t


# ───────────────────────── Generic Herbrand evaluator (unspecialized) ──────────────────

def prove_even_herbrand(t: Term) -> bool:
    """
    Iterative version of the same Horn-clause proof:
        (1) even(0).
        (2) even(s(s(X))) :- even(X).
    """
    while True:
        if isinstance(t, Zero):
            return True                  # clause (1)
        if isinstance(t, S) and isinstance(t.t, S):
            t = t.t.t                    # clause (2): strip two s-layers
            continue
        return False                     # no clause matches


# ───────────────────────── Specialization artifact ─────────────────────────

@dataclass
class SpecializationResult:
    func: Callable[[int], bool]  # residual: n ↦ even(s^n(0)) ?
    source: str                  # generated Python source code
    trace: List[str]             # human-readable mix-time reasoning


def specialize_even() -> SpecializationResult:
    """
    Treat the Horn program as *static* and the ground input as *dynamic*,
    but encode the input term s^n(0) by the integer n. Specialize now, emit
    a residual function even_res(n) with no symbolic operations.
    """
    lines: List[str] = []
    trace: List[str] = []

    # Explain the Herbrand setting
    trace.append("Signature Σ: constants {0}, function {s/1}, predicate {even/1}.")
    trace.append("Herbrand universe: {0, s(0), s(s(0)), ...}.")
    trace.append("Program clauses: (1) even(0).  (2) even(s(s(X))) :- even(X).")
    trace.append("Observation: the only recursive clause strips TWO s-layers.")
    trace.append("⇒ For a ground term t = s^n(0), even(t) depends only on n mod 2.")
    trace.append("We encode terms by their s-depth n (non-negative integers).")
    trace.append("At mix time we replace unification with n-=2 steps and the base test n==0.")

    # Emit residual code
    lines.append("def even_res(n: int) -> bool:")
    lines.append('    """Residual: decide even(s^n(0)) for n ≥ 0 (specialized to the fixed Horn program)."""')
    lines.append("    if n < 0:")
    lines.append("        raise ValueError('n must be ≥ 0 (represents s^n(0))')")
    lines.append("    # From clause (2): each s(s(_)) layer lets us subtract 2 from n")
    lines.append("    while n >= 2:")
    lines.append("        n -= 2")
    lines.append("    # From clause (1): even(0). All other one-layer remainder is odd.")
    lines.append("    return n == 0")

    source = "\n".join(lines)

    # Materialize the function
    namespace: dict = {}
    exec(source, namespace)  # safe: locally generated code
    residual_func = namespace["even_res"]

    return SpecializationResult(func=residual_func, source=source, trace=trace)


# ───────────────────────── Pretty-printers for the three sections ──────────────────────

def print_answer(spec: SpecializationResult, example_n: int) -> None:
    print("Answer")
    print("------")
    print("Residual program (generated at mix time):")
    print(spec.source)
    value = spec.func(example_n)
    print(f"Example evaluation: for n = {example_n} (i.e., t = s^{example_n}(0)), even_res(n) = {value}")
    print()

def print_reason(spec: SpecializationResult, example_n: int) -> None:
    print("Reason why")
    print("----------")
    for line in spec.trace:
        print("•", line)
    # Show an illustrative unfolding for the example n
    n = example_n
    path = [n]
    while n >= 2:
        n -= 2
        path.append(n)
    verdict = (n == 0)
    print(f"• Example unfolding for n = {example_n}: n path {path} → return (n==0) = {verdict}.")
    print()

def print_check(spec: SpecializationResult, trials: int = 400) -> None:
    print("Check (harness)")
    print("----------------")
    random.seed(1337)

    # Deterministic edge cases + randoms
    samples: List[int] = [0, 1, 2, 3, 10, 11, 100, 101]
    samples += [random.randint(0, 10_000) for _ in range(trials)]

    failures: List[Tuple[int, bool, bool]] = []
    for n in samples:
        t = int_to_term(n)
        want = prove_even_herbrand(t)
        got = spec.func(n)
        if want != got:
            failures.append((n, want, got))

    if not failures:
        print(f"PASS: residual even_res matches the generic Herbrand evaluator "
              f"on {len(samples)} inputs (including large n).")
    else:
        print(f"FAIL: {len(failures)} mismatches found. Showing up to 10:")
        for (n, want, got) in failures[:10]:
            print(f"  n = {n:>6}: expected {want}, got {got}")
    print()


# ───────────────────────── Main demo ─────────────────────────────────────

def main() -> None:
    # Build the residual program specialized to the fixed Horn clauses
    spec = specialize_even()

    # 1) Show residual code and a sample evaluation
    example_n = 13
    print_answer(spec, example_n)

    # 2) Explain the mix-time reasoning (with an example unfolding)
    print_reason(spec, example_n)

    # 3) Verify equivalence vs. a generic Herbrand-term evaluator
    print_check(spec, trials=600)


if __name__ == "__main__":
    main()

