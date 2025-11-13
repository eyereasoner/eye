#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Turing Machine — Binary Incrementer (ARC: Answer / Reason / Check), self-contained

Machine:
  One-tape, deterministic TM. Program maps (state, symbol) → (write, move, next).
  Symbols: '0', '1', '_' (blank). Moves: 'L', 'R', 'N'.

Program (same behavior as the original demo):
  S0: scan right to the first blank; then go left to start adding
  S1: propagate +1 with carry: 1→0 and move left; 0→1 and halt; blank→1 and halt
"""

from __future__ import annotations
from typing import Dict, Tuple, List, Set


# ───────────────────────────── TM interpreter ─────────────────────────────
class TuringMachine:
    """A simple one-tape deterministic TM interpreter."""

    def __init__(
        self,
        program: Dict[Tuple[str, str], Tuple[str, str, str]],
        blank: str = "_",
        max_steps: int = 10_000,
    ):
        self.program = program
        self.blank = blank
        self.max_steps = max_steps

    def run(
        self,
        tape: List[str],
        start_state: str,
        halt_states: Set[str],
    ) -> List[str]:
        # Sparse tape: index -> symbol
        tape_dict = {i: sym for i, sym in enumerate(tape)}
        head = 0
        state = start_state
        steps = 0

        while state not in halt_states and steps < self.max_steps:
            sym = tape_dict.get(head, self.blank)
            key = (state, sym)
            if key not in self.program:
                break  # implicit halt if no rule
            new_sym, move, new_state = self.program[key]
            tape_dict[head] = new_sym
            if move == "L":
                head -= 1
            elif move == "R":
                head += 1
            # 'N' → no move
            state = new_state
            steps += 1

            # Optional safety: could assert steps < self.max_steps

        # Compact tape back to a list and trim blanks at both ends
        if tape_dict:
            lo = min(tape_dict)
            hi = max(tape_dict)
            out = [tape_dict.get(i, self.blank) for i in range(lo, hi + 1)]
            while out and out[0] == self.blank:
                out.pop(0)
            while out and out[-1] == self.blank:
                out.pop()
        else:
            out = []
        return out


# ───────────────────────────── Binary +1 program ────────────────────────────
S0, S1, HALT = "S0", "S1", "HALT"
BLANK = "_"

PROGRAM: Dict[Tuple[str, str], Tuple[str, str, str]] = {
    # S0: march right until the first blank, then step left and switch to S1
    (S0, "0"): ("0", "R", S0),
    (S0, "1"): ("1", "R", S0),
    (S0, BLANK): (BLANK, "L", S1),

    # S1: add 1 with carry (to the left)
    (S1, "0"): ("1", "N", HALT),   # found a 0 → write 1 and halt
    (S1, "1"): ("0", "L", S1),     # carry: flip 1→0 and continue left
    (S1, BLANK): ("1", "N", HALT), # overflow (all 1s) → write leading 1 and halt
}

TM = TuringMachine(PROGRAM, BLANK)


def increment(bits: List[int]) -> List[int]:
    """Increment an unsigned binary integer represented as a list of 0/1."""
    in_tape = [str(b) for b in bits]
    out = TM.run(in_tape, S0, {HALT})
    return [int(c) for c in out] if out else []


# ───────────────────────────────── ARC: Answer ──────────────────────────────
def print_answer() -> None:
    print("Answer")
    print("======")
    samples = {
        "A1": [1, 0, 1, 0, 0, 1],
        "A2": [1, 0, 1, 1, 1, 1],
        "A3": [1, 1, 1, 1, 1, 1],
        "A4": [],  # empty tape ≡ zero
    }
    for label, tape in samples.items():
        print(f"{label}: {tape} -> {increment(tape)}")


# ─────────────────────────────── ARC: Reason why ────────────────────────────
def print_reason() -> None:
    print("\nReason why")
    print("==========")
    print("We interpret a one-tape TM over symbols {0,1,_}. The program has two phases:")
    print("  • S0 scans right to the first blank to find the number’s end, then moves left.")
    print("  • S1 performs +1 with carry: 1→0 and move left; if it sees 0, write 1 and halt;")
    print("    if it runs off the left end (blank), write a leading 1 and halt.")
    print("Thus, e.g., 101001 → 101010; 111111 → 1000000; [] (zero) → [1].")


# ─────────────────────────── ARC: Check (harness) ────────────────────────────
def bits_to_int(bits: List[int]) -> int:
    x = 0
    for b in bits:
        x = (x << 1) | (b & 1)
    return x

def int_to_bits(x: int) -> List[int]:
    if x <= 0:
        return []
    out: List[int] = []
    while x:
        out.append(x & 1)
        x >>= 1
    return list(reversed(out))

def normalize(bits: List[int]) -> List[int]:
    """Strip leading zeros (just in case)."""
    i = 0
    while i < len(bits) and bits[i] == 0:
        i += 1
    return bits[i:]

def print_check() -> None:
    print("\nCheck (harness)")
    print("===============")
    ok_all = True

    # A) Correctness vs Python integers on a broad range
    ok = True
    for n in range(0, 1 << 12):  # 0..4095
        bits = int_to_bits(n)
        inc_bits = increment(bits)
        want = int_to_bits(n + 1)
        if normalize(inc_bits) != normalize(want):
            ok = False
            print(f"  mismatch at n={n}: got {inc_bits}, want {want}")
            break
    print(f"Matches Python n→n+1 for 0..4095? {ok}")
    ok_all &= ok

    # B) Structural properties: output length is either len or len+1; no internal blanks
    def well_formed(bs: List[int]) -> bool:
        return all(b in (0, 1) for b in bs)

    ok_len = True
    ok_wf = True
    for bits in ([], [0], [1], [1,0], [1,1], [1,1,1,1,1,1], [1,0,1,0,0,1], [1,0,1,1,1,1]):
        out = increment(bits)
        if not (len(out) == len(bits) or len(out) == len(bits) + 1):
            ok_len = False
        if not well_formed(out):
            ok_wf = False
    print(f"Output length ∈ {{len, len+1}} on samples? {ok_len}")
    print(f"Output symbols only 0/1?                    {ok_wf}")
    ok_all &= ok_len and ok_wf

    # C) Monotonicity: interpreting bit-vectors as integers, increment strictly increases
    ok_mono = True
    prev = -1
    for n in range(0, 200):
        nxt = bits_to_int(increment(int_to_bits(n)))
        if not (nxt == n + 1):
            ok_mono = False
            break
        prev = nxt
    print(f"Monotone w.r.t. integer value (first 200)? {ok_mono}")
    ok_all &= ok_mono

    # D) All-ones case grows by one digit: 1^k → 1·0^k
    ok_ones = True
    for k in range(1, 16):
        inp = [1] * k
        out = increment(inp)
        ok_ones &= (out == [1] + [0] * k)
    print(f"All-ones case yields 1 followed by k zeros? {ok_ones}")
    ok_all &= ok_ones

    print(f"\nAll checks passed? {ok_all}")


# ─────────────────────────────────── Main ────────────────────────────────────
if __name__ == "__main__":
    print_answer()
    print_reason()
    print_check()

