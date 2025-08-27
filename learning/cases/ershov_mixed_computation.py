#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Ershov's mixed computation — a tiny, runnable demo in one file.

What this program shows
-----------------------
Ershov introduced *mixed computation* (a.k.a. partial evaluation): if part of a
program’s input is known *now* (at “mix time”) and the rest arrives *later*
(at “run time”), we can execute the *static* part today and generate a smaller,
faster *residual program* that expects only the *dynamic* part tomorrow.

This script illustrates that idea with exponentiation:

  - Original program: pow_plain(x, n) computes x**n by repeated multiplication.
  - Mixed computation: given a *static* exponent n, we specialize the program,
    producing a *residual function* pow_n(x) that multiplies exactly as often
    as needed, with no loop or branches left — the structure is decided now.

A minimal, human-readable specializer (the “mixer”) emits Python source for the
residual function and executes it. We print:

  1) "Answer" — the generated residual code and a sample evaluation,
  2) "Reason why" — a concise, step-by-step justification of what was decided
     at mix time (which bits of n caused which multiplications),
  3) "Check (harness)" — a small test harness that verifies the residual
     program matches the original on many random inputs.

Run it directly with Python 3.x; no dependencies.
"""

from dataclasses import dataclass
from typing import Callable, List, Tuple
import random


# ---------- Original (unspecialized) program ----------

def pow_plain(x: int, n: int) -> int:
    """
    Compute x**n using a simple loop (n >= 0).
    This is the *generic* program that takes both inputs at run time.
    """
    if n < 0:
        raise ValueError("This demo handles non-negative exponents only.")
    r = 1
    while n > 0:
        r *= x
        n -= 1
    return r


# ---------- Specialization result container ----------

@dataclass
class SpecializationResult:
    func: Callable[[int], int]   # the residual function of one argument x
    source: str                  # generated Python source code for that function
    trace: List[str]             # human-readable reasoning of mix-time decisions


# ---------- The "mixer": perform mixed computation w.r.t. static n ----------

def specialize_pow(n: int) -> SpecializationResult:
    """
    Given a *static* exponent n, generate a *residual* function pow_n(x)
    such that pow_n(x) == pow_plain(x, n) for all x.

    Strategy: exponentiation by squaring, unrolled at mix time.
      - We iterate through the bits of n (from LSB upward).
      - Each '1' bit means: multiply the accumulator by the current power.
      - Each step squares the current power for the next bit.
    At mix time we *emit statements*, not a run-time loop.
    """
    if n < 0:
        raise ValueError("This demo handles non-negative exponents only.")

    lines: List[str] = []
    trace: List[str] = []

    func_name = f"pow_{n}"
    lines.append(f"def {func_name}(x):")
    lines.append(f"    \"\"\"Residual program computing x**{n}, specialized at mix time.\"\"\"")
    lines.append("    r = 1")

    # Always initialize the first power to x (x^(2^0))
    lines.append("    p0 = x  # x^(2**0)")
    p_name = "p0"

    if n == 0:
        trace.append("n == 0 → at mix time we decide the answer is the constant 1.")
        lines.append("    # No multiplications needed; exponent is zero.")
        lines.append("    return r")
    else:
        k = n
        bit = 0
        while k > 0:
            next_exists = (k >> 1) != 0
            if k & 1:
                lines.append(f"    r = r * {p_name}  # include bit {bit} (2**{bit})")
                trace.append(f"Bit {bit} of n is 1 → multiply by x^(2**{bit}).")
            else:
                trace.append(f"Bit {bit} of n is 0 → skip multiplication.")
            if next_exists:
                # Prepare next power for the next bit: p_{i+1} = p_i * p_i
                next_p = f"p{bit+1}"
                lines.append(f"    {next_p} = {p_name} * {p_name}  # square for next bit")
                p_name = next_p

            k >>= 1
            bit += 1

        lines.append("    return r")

    source = "\n".join(lines)

    # Materialize the function from the generated source.
    namespace: dict = {}
    exec(source, namespace)  # safe here; code is locally generated, no user input
    residual_func = namespace[func_name]

    # Include a top-level reasoning preface.
    preface = [
        "We treat the exponent n as *static* (known now) and the base x as *dynamic* (known later).",
        "Therefore we can eliminate the loop/branches by inspecting the bits of n at mix time.",
        "Each '1' bit emits one multiply by the current power; every step emits one squaring.",
    ]
    trace = preface + trace

    return SpecializationResult(func=residual_func, source=source, trace=trace)


# ---------- Pretty-printers for the three requested sections ----------

def print_answer(n: int, example_x: int, spec: SpecializationResult) -> None:
    print("Answer")
    print("------")
    print("Residual program (generated at mix time):")
    print(spec.source)
    value = spec.func(example_x)
    print(f"Example evaluation: for x = {example_x}, pow_{n}(x) = {value}")
    print()


def print_reason(spec: SpecializationResult) -> None:
    print("Reason why")
    print("----------")
    for line in spec.trace:
        print("•", line)
    print()


def print_check(n: int, spec: SpecializationResult, trials: int = 200) -> None:
    print("Check (harness)")
    print("----------------")
    random.seed(1337)

    # We test a blend of small and larger magnitudes, including negatives.
    samples: List[int] = []
    samples += [random.randint(-12, 12) for _ in range(trials // 2)]
    samples += [random.randint(-1_000, 1_000) for _ in range(trials // 2)]

    # Always include a few hand-picked corner cases
    samples += [0, 1, -1, 2, -2, 10, -10]

    failures: List[Tuple[int, int, int]] = []
    for x in samples:
        try:
            want = pow_plain(x, n)
            got = spec.func(x)
        except Exception as e:
            print(f"Runtime error for x = {x}: {e}")
            return
        if want != got:
            failures.append((x, want, got))

    if not failures:
        print(f"PASS: residual program matches original pow_plain(x, {n}) "
              f"for {len(samples)} tested values of x.")
    else:
        print(f"FAIL: {len(failures)} mismatches found. Showing up to 10:")
        for (x, want, got) in failures[:10]:
            print(f"  x = {x:>6}: expected {want}, got {got}")

    print()


# ---------- Main demo ----------

def main() -> None:
    # Choose the static and dynamic parts:
    # - n is *static* (we know it now),
    # - x is *dynamic* (unknown until run time).
    static_n = 13        # Feel free to change this and re-run.
    example_dynamic_x = 7

    spec = specialize_pow(static_n)

    print_answer(static_n, example_dynamic_x, spec)
    print_reason(spec)
    print_check(static_n, spec, trials=300)


if __name__ == "__main__":
    main()

