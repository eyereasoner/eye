#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Ershov's mixed computation — Herbrand logic (the Barber Paradox)

What this shows
---------------
In classical first-order/Herbrand semantics, fix a signature with:
  - constant:  b        (the barber)
  - predicate: Shaves/2

Axiom (the paradox):
  ∀x.  Shaves(b, x)  ↔  ¬Shaves(x, x)

If the Herbrand universe contains b (it must, as a constant), the ground
instance x=b yields:
  Shaves(b, b) ↔ ¬Shaves(b, b)
which is impossible in classical logic. Therefore, the theory is *inconsistent*:
there is no interpretation (no assignment of truth values to ground atoms)
that can satisfy the axiom.

Mixed computation idea
----------------------
Treat the *theory* (axiom and domain) as **static**, and the *interpretation* of
Shaves/2 as **dynamic** (it might be supplied later as a dictionary mapping
pairs to booleans). We can unroll the quantifier over the (finite) domain *now*
and immediately discover that the instance x=b is a contradiction. Thus the
residual program can be a constant function that returns `False` for the axiom,
independent of any future Shaves/2 assignment — no search, unification, or
quantifier handling left at run time.

The script prints:
  1) "Answer" — the generated residual evaluator and a sample evaluation,
  2) "Reason why" — a step-by-step mix-time explanation,
  3) "Check (harness)" — a verifier showing that the generic evaluator always
     reports the axiom as False, matching the residual one.

Run with Python 3.x; no dependencies.
"""

from dataclasses import dataclass
from typing import Dict, Tuple, List
import random


# ---------- Generic (unspecialized) evaluator ----------

def barber_axiom_holds_generic(domain: List[str],
                               S: Dict[Tuple[str, str], bool]) -> bool:
    """
    Evaluate the sentence  ∀x. Shaves(b,x) == (not Shaves(x,x))  classically
    on a *given interpretation* S ⊆ domain×domain (encoded as a dict of booleans).
    Missing pairs default to False.
    """
    def val(a: str, b: str) -> bool:
        return S.get((a, b), False)

    for x in domain:
        if val("b", x) != (not val(x, x)):
            return False
    return True


# ---------- Specialization result container ----------

@dataclass
class SpecializationResult:
    func: callable                       # residual evaluator: S -> bool
    source: str                          # generated Python source
    trace: List[str]                     # mix-time reasoning (human-readable)


# ---------- The "mixer": unroll ∀ and fold constants ----------

def specialize_barber(domain: List[str]) -> SpecializationResult:
    """
    Treat the domain and axiom as *static*. Generate a residual evaluator for
    the sentence that takes only a dynamic interpretation S.
    """
    lines: List[str] = []
    trace: List[str] = []

    # Mix-time reasoning
    trace.append("We treat the domain and the axiom as static; the Shaves/2 interpretation S is dynamic.")
    trace.append(f"Domain D = {domain}")
    trace.append("Sentence: ∀x. Shaves(b,x) == (not Shaves(x,x))")
    trace.append("Unroll the universal quantifier over D and examine each ground instance:")

    # Build residual code header
    lines.append("def barber_sentence_res(S: dict) -> bool:")
    lines.append('    """Residual: truth of ∀x. Shaves(b,x) == (not Shaves(x,x)) over the fixed domain."""')
    lines.append(f"    D = {repr(domain)}")
    lines.append("    def val(a, b): return S.get((a, b), False)")

    # Examine each instance at mix time; x='b' already forces a contradiction.
    inconsistent = False
    for x in domain:
        if x == "b":
            trace.append(" - For x='b': instance is A == (not A) with A := Shaves(b,b) → contradiction.")
            inconsistent = True
            # Keep a comment in the residual code documenting the contradiction.
            lines.append("    # Ground instance x='b': val('b','b') == (not val('b','b'))  → impossible")
        else:
            trace.append(f" - For x='{x}': instance is Shaves(b,'{x}') == (not Shaves('{x}','{x}')). "
                         f"(Constraint on S if we were searching for a model.)")

    if inconsistent:
        trace.append("Conclusion at mix time: the sentence is unsatisfiable for any S; residual returns False.")
        lines.append("    return False")
    else:
        # (This branch cannot happen when 'b' ∈ domain, included here for completeness.)
        conj = " and ".join([f"(val('b','{x}') == (not val('{x}','{x}')))" for x in domain])
        lines.append(f"    return {conj}")

    source = "\n".join(lines)

    # Materialize the function
    ns: Dict[str, object] = {}
    exec(source, ns)  # safe: code generated locally from constants
    residual_func = ns["barber_sentence_res"]

    return SpecializationResult(func=residual_func, source=source, trace=trace)


# ---------- Pretty-printers for the three sections ----------

def print_answer(spec: SpecializationResult, example_S: Dict[Tuple[str, str], bool]) -> None:
    print("Answer")
    print("------")
    print("Residual program (generated at mix time):")
    print(spec.source)
    value = spec.func(example_S)
    print(f"Example evaluation on a random Shaves/2 interpretation S: result = {value}")
    print()

def print_reason(spec: SpecializationResult) -> None:
    print("Reason why")
    print("----------")
    for line in spec.trace:
        print("•", line)
    print()

def print_check(spec: SpecializationResult, domain: List[str], trials: int = 400) -> None:
    print("Check (harness)")
    print("----------------")
    random.seed(20250827)

    def rand_interp() -> Dict[Tuple[str, str], bool]:
        S: Dict[Tuple[str, str], bool] = {}
        for a in domain:
            for b in domain:
                S[(a, b)] = bool(random.getrandbits(1))
        return S

    failures = 0
    for _ in range(trials):
        S = rand_interp()
        want = barber_axiom_holds_generic(domain, S)
        got  = spec.func(S)
        if want != got:
            failures += 1
            print("Mismatch example:")
            print("  expected (generic) =", want, " got (residual) =", got)
            break

    if failures == 0:
        print(f"PASS: residual evaluator agrees with the generic evaluator on {trials} random interpretations.")
    print()


# ---------- Main demo ----------

def main() -> None:
    # Static Herbrand domain that *includes* the barber constant 'b'
    domain = ["b", "alice", "bob"]

    # Build the residual evaluator specialized to the Barber axiom over this domain
    spec = specialize_barber(domain)

    # 1) Show residual code and a sample evaluation (random S)
    example_S = {("b", "alice"): True, ("alice", "alice"): False}  # rest default to False
    print_answer(spec, example_S)

    # 2) Explain the mix-time reasoning (the paradox arises at x='b')
    print_reason(spec)

    # 3) Verify equivalence vs. the generic evaluator on many interpretations
    print_check(spec, domain, trials=800)


if __name__ == "__main__":
    main()

