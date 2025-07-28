#!/usr/bin/env python3
"""
Universal Turing Machine demo: binary incrementer.

Run with:
    python turing.py
"""

from typing import Dict, Tuple, List, Set


class TuringMachine:
    """
    A simple one-tape deterministic Turing-machine interpreter.
    `program` maps (state, symbol) → (new_symbol, move, new_state)
    where `move` is 'L', 'R', or 'N' (no move).
    """
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
        self, tape: List[str], start_state: str, halt_states: Set[str]
    ) -> List[str]:
        # Represent tape as a sparse dictionary {index: symbol}
        tape_dict = {i: sym for i, sym in enumerate(tape)}
        head = 0
        state = start_state
        steps = 0

        while state not in halt_states and steps < self.max_steps:
            sym = tape_dict.get(head, self.blank)
            key = (state, sym)
            if key not in self.program:
                break  # No rule → implicit halt

            new_sym, move, new_state = self.program[key]

            # Write new symbol
            tape_dict[head] = new_sym

            # Move head
            if move == "L":
                head -= 1
            elif move == "R":
                head += 1
            # 'N' means no move

            state = new_state
            steps += 1

        # Convert sparse dict back to a compact list (trim blanks)
        if tape_dict:
            min_i = min(tape_dict)
            max_i = max(tape_dict)
            out = [tape_dict.get(i, self.blank) for i in range(min_i, max_i + 1)]
            while out and out[0] == self.blank:
                out.pop(0)
            while out and out[-1] == self.blank:
                out.pop()
        else:
            out = []

        return out


# ---------------------------------------------------------------------------
# Binary incrementer TM
# ---------------------------------------------------------------------------

S0, S1, HALT = "S0", "S1", "HALT"
BLANK = "_"

PROGRAM = {
    # State S0: seek right-most blank
    (S0, "0"): ("0", "R", S0),
    (S0, "1"): ("1", "R", S0),
    (S0, BLANK): (BLANK, "L", S1),
    # State S1: add 1 with carry propagation
    (S1, "0"): ("1", "N", HALT),
    (S1, "1"): ("0", "L", S1),
    (S1, BLANK): ("1", "N", HALT),
}


def increment(bits: List[int]) -> List[int]:
    """Increment an unsigned binary integer represented as a list of 0/1."""
    tm = TuringMachine(PROGRAM, BLANK)
    in_tape = [str(b) for b in bits]
    out = tm.run(in_tape, S0, {HALT})
    return [int(c) for c in out] if out else []


# ---------------------------------------------------------------------------
# Demo
# ---------------------------------------------------------------------------

def _demo() -> None:
    tests = {
        "A1": [1, 0, 1, 0, 0, 1],
        "A2": [1, 0, 1, 1, 1, 1],
        "A3": [1, 1, 1, 1, 1, 1],
        "A4": [],
    }
    for label, tape in tests.items():
        print(f"{label}: {tape} -> {increment(tape)}")


if __name__ == "__main__":
    _demo()

