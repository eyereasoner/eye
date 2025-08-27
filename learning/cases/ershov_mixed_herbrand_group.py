#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Ershov's mixed computation — Herbrand logic (group theory over S3)

What this program shows
-----------------------
We illustrate Ershov’s *mixed computation* (partial evaluation) in a Herbrand-style
setting for **group theory**. We use the classical presentation of S3:

  Generators:  r (a 3-cycle), s (a transposition)
  Relations:   r^3 = e,  s^2 = e,  s r s = r^{-1}   (equivalently: s r = r^{-1} s)

Herbrand viewpoint:
  - Terms are *words* over {r, s} (and optionally R = r^{-1}, S = s^{-1}).
  - The generic evaluator reduces words using the relations above (rewrite/unify).

Mixed computation:
  - We treat the **group** (S3) and its presentation as *static*.
  - We specialize the generic reducer into a tiny residual program that
    scans the input word once, updating a finite state (the current group element),
    using a baked-in Cayley transition table. No unification, no rewriting loops.

Outputs
-------
1) "Answer" — the generated residual code and a sample evaluation on a word.
2) "Reason why" — what was decided at mix time (states, transitions, normal forms).
3) "Check (harness)" — verification that the residual normal form matches the
   generic Herbrand reducer on many random words.

Run with Python 3.x; no dependencies.
"""

from dataclasses import dataclass
from typing import Callable, List, Tuple, Dict, Union
import random

# ─────────────────────── Utilities ───────────────────────

Letter = str  # 'r','R','s','S'
Word   = Union[str, List[Letter]]

def normalize_letters(w: Word) -> List[Letter]:
    """Accept a string or a list of letters and normalize to a list."""
    if isinstance(w, str):
        letters = list(w)
    else:
        letters = list(w)
    for ch in letters:
        if ch not in ("r", "R", "s", "S"):
            raise ValueError(f"illegal letter {ch!r}; allowed: 'r','R','s','S'")
    return letters

# ─────────────────────── Generic Herbrand reducer (unspecialized) ───────────────────────

def word_to_base_letters(letters: List[Letter]) -> List[Letter]:
    """
    Expand inverses to {r,s}-only:
      R -> r r     (since r^{-1} = r^2)
      S -> s       (since s^{-1} = s)
    """
    out: List[Letter] = []
    for ch in letters:
        if ch == "r":
            out.append("r")
        elif ch == "R":
            out.extend(["r", "r"])
        elif ch in ("s", "S"):
            out.append("s")
    return out

def reduce_S3_generic(w: Word) -> Tuple[int, int]:
    """
    Reduce a word to S3 normal form r^i s^j with i∈{0,1,2}, j∈{0,1} by rewriting.

    Rewriting rules (leftmost, repeat to fixpoint):
      1) s s     → ε
      2) r r r   → ε
      3) s r     → r r s        (since s r = r^{-1} s = r^2 s)
    """
    letters = word_to_base_letters(normalize_letters(w))

    changed = True
    while changed:
        changed = False
        i = 0
        out: List[Letter] = []
        n = len(letters)
        while i < n:
            # try rule 1: s s -> ε
            if i + 1 < n and letters[i] == "s" and letters[i + 1] == "s":
                i += 2
                changed = True
                continue
            # try rule 2: r r r -> ε
            if i + 2 < n and letters[i] == letters[i + 1] == letters[i + 2] == "r":
                i += 3
                changed = True
                continue
            # try rule 3: s r -> r r s
            if i + 1 < n and letters[i] == "s" and letters[i + 1] == "r":
                out.extend(["r", "r", "s"])
                i += 2
                changed = True
                continue
            # otherwise copy
            out.append(letters[i])
            i += 1
        letters = out

    # At fixpoint, all s's (if any) are at the end and there is at most one.
    j = 1 if (len(letters) > 0 and letters[-1] == "s") else 0
    if j == 1:
        letters = letters[:-1]

    # Now letters contain only r's, count modulo 3
    i = (len(letters) % 3)
    return (i, j)  # represents element r^i s^j


# ─────────────────────── Specialization artefact ───────────────────────

@dataclass
class SpecializationResult:
    nf: Callable[[Word], Tuple[int, int]]  # residual: word -> (i,j) with element r^i s^j
    source: str                            # generated Python source
    trace: List[str]                       # mix-time reasoning

def specialize_S3() -> SpecializationResult:
    """
    Treat the group S3 (its presentation, and thus its Cayley table) as *static*.
    Build a residual evaluator that:
      - pre-expands inverses (R -> r r, S -> s),
      - scans letters with a baked-in transition table on 6 states,
      - returns the normal-form pair (i,j) with i in {0,1,2}, j in {0,1}.
    """
    # Canonical indexing of S3 elements as pairs (i,j) for r^i s^j
    states: List[Tuple[int, int]] = [(0,0),(1,0),(2,0),(0,1),(1,1),(2,1)]  # e, r, r2, s, rs, r2s
    index_of: Dict[Tuple[int,int], int] = {p:i for i,p in enumerate(states)}

    # Define multiplication in pair form:
    # (i1,j1)*(i2,j2) = (i1+i2, j2) if j1=0;  else (i1 - i2, 1 XOR j2) if j1=1   (mod i in 3)
    def mul_pair(a: Tuple[int,int], b: Tuple[int,int]) -> Tuple[int,int]:
        (i1, j1), (i2, j2) = a, b
        if j1 == 0:
            return ((i1 + i2) % 3, j2)
        else:
            return ((i1 - i2) % 3, (1 ^ j2))

    # Generator elements as pairs
    r = (1,0)                 # r
    s = (0,1)                 # s
    R = (2,0)                 # r^{-1} = r^2
    S = (0,1)                 # s^{-1} = s

    # Build transition tables for scanning letters on the RIGHT: state := state * gen
    def build_trans(gen_pair: Tuple[int,int]) -> List[int]:
        out: List[int] = []
        for st in states:
            nxt = mul_pair(st, gen_pair)
            out.append(index_of[nxt])
        return out

    TR = build_trans(r)
    TS = build_trans(s)
    TRinv = build_trans(R)    # not strictly needed (we pre-expand), but handy for the trace
    TSinv = build_trans(S)

    # Emit residual code
    lines: List[str] = []
    lines.append("def nf_res(word):")
    lines.append('    """Residual S3 normal form: return (i,j) for r^i s^j given a word over r,R,s,S."""')
    lines.append("    # Normalize input to a list of letters")
    lines.append("    letters = list(word) if isinstance(word, str) else list(word)")
    lines.append("    for ch in letters:")
    lines.append("        if ch not in ('r','R','s','S'):")
    lines.append("            raise ValueError('illegal letter: ' + repr(ch))")
    lines.append("")
    lines.append("    # Expand inverses to {r,s} only (R -> r r, S -> s)")
    lines.append("    seq = []")
    lines.append("    for ch in letters:")
    lines.append("        if ch == 'r': seq.append('r')")
    lines.append("        elif ch == 'R': seq.extend(['r','r'])")
    lines.append("        elif ch in ('s','S'): seq.append('s')")
    lines.append("")
    lines.append("    # Scan with a precomputed Cayley transition table over 6 states")
    lines.append(f"    TR = {TR}")
    lines.append(f"    TS = {TS}")
    lines.append("    state = 0  # start at e = r^0 s^0")
    lines.append("    for ch in seq:")
    lines.append("        if ch == 'r':")
    lines.append("            state = TR[state]")
    lines.append("        else:  # ch == 's'")
    lines.append("            state = TS[state]")
    lines.append("")
    lines.append("    # Map state index back to (i,j)")
    lines.append("    states = [(0,0),(1,0),(2,0),(0,1),(1,1),(2,1)]")
    lines.append("    return states[state]")
    source = "\n".join(lines)

    # Materialize the residual function
    ns: Dict[str, object] = {}
    exec(source, ns)  # safe: locally generated code
    nf_func = ns["nf_res"]

    # Build a human-readable trace
    def fmt_state(k: int) -> str:
        i,j = states[k]
        name = {0:"e",1:"r",2:"r^2"}[i] + ("" if j==0 else "·s")
        return f"{k}:{name}"
    trace: List[str] = []
    trace.append("We fix S3 with presentation ⟨ r, s | r^3=e, s^2=e, s r s = r^{-1} ⟩ as *static*.")
    trace.append("Herbrand terms are words over {r,s} (R=r^{-1} expands to r r; S=s^{-1} expands to s).")
    trace.append("At mix time we replace rewriting with a DFA over 6 states representing elements:")
    trace.append("  0:e, 1:r, 2:r^2, 3:s, 4:r·s, 5:r^2·s (normal form r^i s^j).")
    trace.append("We precompute transitions state := state * gen on the right:")
    def row(label: str, T: List[int]) -> str:
        return f"  {label}: " + ", ".join(f"{fmt_state(i)}→{fmt_state(T[i])}" for i in range(6))
    trace.append(row("TR (× r)", TR))
    trace.append(row("TS (× s)", TS))
    trace.append("Residual evaluator: expand inverses, scan with TR/TS, return the pair (i,j).")

    return SpecializationResult(nf=nf_func, source=source, trace=trace)


# ─────────────────────── Pretty-printers for the three sections ───────────────────────

def print_answer(spec: SpecializationResult, example_word: Word) -> None:
    print("Answer")
    print("------")
    print("Residual program (generated at mix time):")
    print(spec.source)
    nf = spec.nf(example_word)
    print(f"Example evaluation: word = {example_word!r}  →  normal form (i,j) = {nf} (means r^i s^j)")
    print(f"Is identity? {'yes' if nf==(0,0) else 'no'}")
    print()

def print_reason(spec: SpecializationResult) -> None:
    print("Reason why")
    print("----------")
    for line in spec.trace:
        print("•", line)
    print()

def print_check(spec: SpecializationResult, trials: int = 800) -> None:
    print("Check (harness)")
    print("----------------")
    random.seed(20250827)

    # A bunch of random words over r,R,s,S with varying lengths
    def rand_word() -> str:
        alphabet = ["r","R","s","S"]
        L = random.randint(0, 100)
        return "".join(random.choice(alphabet) for _ in range(L))

    tests: List[str] = []
    # Deterministic probes
    tests += ["", "r", "rr", "rrr", "rrrr", "s", "ss", "rs", "sr", "srs", "R", "RSrS"]
    # Randoms
    for _ in range(trials):
        tests.append(rand_word())

    failures: List[Tuple[str, Tuple[int,int], Tuple[int,int]]] = []
    for w in tests:
        want = reduce_S3_generic(w)
        got  = spec.nf(w)
        if want != got:
            failures.append((w, want, got))
            break

    if not failures:
        print(f"PASS: residual normal form agrees with the generic Herbrand reducer "
              f"on {len(tests)} words.")
    else:
        w, want, got = failures[0]
        print("FAIL: mismatch found.")
        print(f"  word = {w!r}")
        print(f"  expected (generic) = {want}, got (residual) = {got}")
    print()


# ─────────────────────── Main demo ───────────────────────

def main() -> None:
    # Build residual evaluator specialized to S3
    spec = specialize_S3()

    # 1) Show the residual code and a sample evaluation
    example = "s r s r R S".replace(" ", "")  # a small, slightly tricky word
    print_answer(spec, example)

    # 2) Explain the mix-time decisions
    print_reason(spec)

    # 3) Verify equivalence vs. the generic Herbrand reducer
    print_check(spec, trials=1000)


if __name__ == "__main__":
    main()

