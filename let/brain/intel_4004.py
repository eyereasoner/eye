#!/usr/bin/env python3
"""
Mini Intel 4004-style Emulator + Micro-Assembler + Harness
----------------------------------------------------------

This script emulates a *subset* of the Intel 4004 CPU sufficient to run a small
demonstration program. It is intended to be educational and tiny rather than
cycle-accurate. The choices below mirror the 4004 spirit:

  • 4-bit accumulator A
  • 16 general registers R0..R15 (each 4 bits)
  • 1-bit carry flag (CY)
  • 12-bit program counter (PC) with 256-byte pages
  • A 3-level call stack placeholder (not used by the demo)
  • A handful of 4004-like opcodes (see table below)

We also include a *micro-assembler* for a readable assembly language that maps
to these opcodes. The demo program runs on the emulator and computes the sum
1 + 2 + ... + 10 = 55 using only 4-bit math and carry propagation, then we
print an Answer / Reason / Check.

⚠️ Accuracy note
----------------
This is **not** a cycle-accurate reproduction of every 4004 nuance. It
implements a faithful-enough subset to show how 4-bit arithmetic, carry, and
looping work. The instruction encodings are chosen to match common references
where convenient (e.g., LDM/ADD/XCH families) but are simplified where needed
(e.g., ISZ semantics described below).

Implemented instructions (byte encodings):
------------------------------------------
All single-byte unless noted.

NOP          : 0x00                   — no operation
HLT          : 0xFF                   — halt (convenience for the emulator)
CLB          : 0xF0                   — clear A and CY
CLC          : 0xF1                   — clear carry

LDM imm4     : 0xD0 | imm             — A ← imm (imm is 0..15)
XCH Rr       : 0x20 | r               — swap(A, Rr)
ADD Rr       : 0x80 | r               — A ← (A + Rr + CY), CY set on nibble carry
INC Rr       : 0x60 | r               — Rr ← (Rr + 1) mod 16

ISZ Rr, addr : (0x70 | r) <addr8>     — Rr ← (Rr + 1) mod 16;
                                         if Rr != 0 then PC jumps within page to addr,
                                         else execution continues (no extra skip).
                                         (This “jump-if-not-zero” flavor is a
                                          small simplification of the original 4004’s
                                          “increment and skip if zero” behavior.)
                                         Address is page-local (low 8 bits).

This subset is enough to implement a triangular-number loop in pure 4-bit math:
we store the 8-bit result across two 4-bit registers (high nibble and low nibble)
and propagate carry from the low to high nibble by adding a zero register while
including the carry in the ADD.

Demo program idea
-----------------
Compute S = 1 + 2 + ... + 10. We maintain:
  • R0 = low nibble of the running sum
  • R1 = high nibble of the running sum
  • R2 = current addend i (starts at 1, increments to 10)
  • R3 = constant 0 (used to “add carry only” to the high nibble)
  • R5 = loop counter seed 6 for ISZ trick:
        ISZ increments R5; when it wraps to 0 (after 10 increments),
        the loop stops; otherwise it jumps back.

Why seed = 6? On a 4-bit counter, adding 10 steps to 6 wraps to 0:
  6→7→8→9→A→B→C→D→E→F→0  (10 increments)

So we get exactly 10 loop iterations.

Author: You + ChatGPT
"""

from dataclasses import dataclass, field
from typing import List, Dict, Tuple


# ---------------------------
# CPU and Emulator Structures
# ---------------------------

@dataclass
class CPU4004:
    """Minimal 4004-style CPU state."""
    A: int = 0                       # 4-bit accumulator
    CY: int = 0                      # carry flag (0/1)
    PC: int = 0                      # 12-bit program counter
    R: List[int] = field(default_factory=lambda: [0]*16)  # 16 × 4-bit registers
    stack: List[int] = field(default_factory=list)         # 3-level stack (unused in demo)
    halted: bool = False

    def page_base(self) -> int:
        """Return the 12-bit PC's current page base (high 4 bits)."""
        return self.PC & 0xF00

    def __repr__(self) -> str:
        regs = " ".join(f"R{i}:{self.R[i]:X}" for i in range(16))
        return f"<A:{self.A:X} CY:{self.CY} PC:{self.PC:03X} {regs}>"


class Mini4004Emulator:
    """
    A tiny emulator for a 4004-like CPU.

    Memory model:
      • 4 KB program space (4096 bytes) — enough for small demos.
    """

    def __init__(self, program: bytes, start_address: int = 0x000, trace: bool = False):
        self.mem = bytearray(4096)
        self.mem[:len(program)] = program
        self.cpu = CPU4004()
        self.cpu.PC = start_address & 0xFFF
        self.trace = trace

    # -------------
    # Fetch/Decode
    # -------------

    def fetch_u8(self) -> int:
        """Fetch one byte from memory and advance PC."""
        b = self.mem[self.cpu.PC]
        self.cpu.PC = (self.cpu.PC + 1) & 0xFFF
        return b

    def run(self, max_steps: int = 10000):
        """Run until HLT or step budget is exhausted."""
        steps = 0
        while not self.cpu.halted and steps < max_steps:
            pc_before = self.cpu.PC
            opcode = self.fetch_u8()
            steps += 1

            if self.trace:
                print(f"PC={pc_before:03X} OP={opcode:02X} {self.cpu}")

            # ---- Single-byte fixed instructions ----
            if opcode == 0x00:  # NOP
                continue
            elif opcode == 0xFF:  # HLT
                self.cpu.halted = True
                break
            elif opcode == 0xF0:  # CLB: clear A and carry
                self.cpu.A = 0
                self.cpu.CY = 0
                continue
            elif opcode == 0xF1:  # CLC: clear carry
                self.cpu.CY = 0
                continue

            # ---- LDM imm4 ----
            if (opcode & 0xF0) == 0xD0:
                imm = opcode & 0x0F
                self.cpu.A = imm & 0x0F
                continue

            # ---- XCH Rr ----
            if (opcode & 0xF0) == 0x20:
                r = opcode & 0x0F
                self.cpu.A, self.cpu.R[r] = self.cpu.R[r] & 0x0F, self.cpu.A & 0x0F
                continue

            # ---- ADD Rr (with carry) ----
            if (opcode & 0xF0) == 0x80:
                r = opcode & 0x0F
                total = (self.cpu.A & 0x0F) + (self.cpu.R[r] & 0x0F) + (self.cpu.CY & 0x01)
                self.cpu.CY = 1 if total >= 0x10 else 0
                self.cpu.A = total & 0x0F
                continue

            # ---- INC Rr ----
            if (opcode & 0xF0) == 0x60:
                r = opcode & 0x0F
                self.cpu.R[r] = (self.cpu.R[r] + 1) & 0x0F
                continue

            # ---- ISZ Rr, addr8 (2 bytes) ----
            if (opcode & 0xF0) == 0x70:
                r = opcode & 0x0F
                addr8 = self.fetch_u8()  # low 8 bits; page-local
                self.cpu.R[r] = (self.cpu.R[r] + 1) & 0x0F
                if self.cpu.R[r] != 0:
                    # Jump within the current 256-byte page
                    self.cpu.PC = (self.cpu.PC & 0xF00) | (addr8 & 0xFF)
                # else: fall through (no jump), continue with next instruction
                continue

            # If we reach here, opcode is unknown for our subset
            raise RuntimeError(f"Unknown opcode 0x{opcode:02X} at PC=0x{pc_before:03X}")

        if not self.cpu.halted:
            raise RuntimeError("Step budget exhausted without HLT")

        return self.cpu  # Final state after halt


# -------------------
# Micro-Assembler
# -------------------

class MiniAssembler:
    """
    Ultra-small assembler for our 4004-like subset.

    Grammar (tokens are whitespace-separated; ';' starts a comment):
      LABEL:               defines a label at current PC
      NOP | HLT | CLB | CLC
      LDM <imm4>           imm4 is 0..15 (dec or hex 0xN)
      XCH Rn               n in 0..15
      ADD Rn               "
      INC Rn               "
      ISZ Rn, LABEL        2-byte instruction; page-local jump to LABEL

    Labels must fit in the same 256-byte page as the ISZ instruction.
    """

    def __init__(self):
        self.registers = {f"R{i}": i for i in range(16)}

    def parse_imm4(self, tok: str) -> int:
        v = int(tok, 0)
        if not (0 <= v <= 15):
            raise ValueError("imm4 must be between 0 and 15")
        return v

    def assemble(self, lines: List[str], origin: int = 0x000) -> bytes:
        # Pass 1: collect labels and compute addresses
        pc = origin & 0xFFF
        labels: Dict[str, int] = {}
        cleaned: List[Tuple[int, List[str]]] = []  # list of (pc_at_line, tokens)

        for raw in lines:
            # Strip comments
            src = raw.split(';', 1)[0].strip()
            if not src:
                continue
            # Label?
            if src.endswith(':'):
                label = src[:-1].strip()
                if not label.isidentifier():
                    raise ValueError(f"Invalid label: {label!r}")
                labels[label] = pc
                continue

            # Tokenize
            tokens = [t.strip().upper() for t in src.replace(',', ' , ').split()]
            cleaned.append((pc, tokens))

            # Size accounting (predict instruction length)
            op = tokens[0]
            if op in ("NOP", "HLT", "CLB", "CLC"):
                pc = (pc + 1) & 0xFFF
            elif op == "LDM":
                pc = (pc + 1) & 0xFFF
            elif op in ("XCH", "ADD", "INC"):
                pc = (pc + 1) & 0xFFF
            elif op == "ISZ":
                pc = (pc + 2) & 0xFFF
            else:
                raise ValueError(f"Unknown mnemonic in pass1: {op}")

        # Pass 2: encode bytes
        out = bytearray()
        pc = origin & 0xFFF
        for pc_line, tokens in cleaned:
            op = tokens[0]
            if op == "NOP":
                out.append(0x00)
                pc += 1

            elif op == "HLT":
                out.append(0xFF)
                pc += 1

            elif op == "CLB":
                out.append(0xF0)
                pc += 1

            elif op == "CLC":
                out.append(0xF1)
                pc += 1

            elif op == "LDM":
                if len(tokens) != 2:
                    raise ValueError("LDM expects one operand")
                imm = self.parse_imm4(tokens[1])
                out.append(0xD0 | imm)
                pc += 1

            elif op == "XCH":
                if len(tokens) != 2:
                    raise ValueError("XCH expects one operand")
                r = self.registers.get(tokens[1])
                if r is None:
                    raise ValueError("XCH operand must be R0..R15")
                out.append(0x20 | r)
                pc += 1

            elif op == "ADD":
                if len(tokens) != 2:
                    raise ValueError("ADD expects one operand")
                r = self.registers.get(tokens[1])
                if r is None:
                    raise ValueError("ADD operand must be R0..R15")
                out.append(0x80 | r)
                pc += 1

            elif op == "INC":
                if len(tokens) != 2:
                    raise ValueError("INC expects one operand")
                r = self.registers.get(tokens[1])
                if r is None:
                    raise ValueError("INC operand must be R0..R15")
                out.append(0x60 | r)
                pc += 1

            elif op == "ISZ":
                # ISZ Rn, LABEL
                if len(tokens) != 4 or tokens[2] != ',':
                    raise ValueError("ISZ syntax: ISZ Rn, LABEL")
                r = self.registers.get(tokens[1])
                if r is None:
                    raise ValueError("ISZ operand must be R0..R15")
                label = tokens[3]
                if label not in labels:
                    raise ValueError(f"Unknown label: {label}")
                target = labels[label] & 0xFF  # page-local low byte
                out.append(0x70 | r)
                out.append(target)
                pc += 2

            else:
                raise ValueError(f"Unknown mnemonic in pass2: {op}")

        return bytes(out)


# -------------------
# Demo Program
# -------------------

def triangular_sum_1_to_10_program() -> List[str]:
    """
    Return assembly lines for:
        S = 1 + 2 + ... + 10

    Register usage:
      R0 = sum low nibble
      R1 = sum high nibble
      R2 = current addend i (1..10)
      R3 = constant 0  (so ADD R3 adds only carry)
      R5 = loop counter seed = 6 (ISZ trick for 10 iterations)
    """
    return [
        "        CLB           ; A=0, CY=0",
        "        XCH R0        ; R0 ← 0 (sum low)",
        "        XCH R1        ; R1 ← 0 (sum high)",
        "        LDM 0         ; A=0",
        "        XCH R3        ; R3 ← 0 (constant zero)",
        "        LDM 1         ; A=1",
        "        XCH R2        ; R2 ← 1 (first addend)",
        "        LDM 6         ; seed for 10-iteration ISZ loop",
        "        XCH R5        ; R5 ← 6",
        "LOOP:",
        "        XCH R0        ; A ↔ R0 (bring low nibble into A)",
        "        ADD R2        ; A = low + i + carry",
        "        XCH R0        ; store low nibble back",
        "        XCH R1        ; bring high nibble into A",
        "        ADD R3        ; A = high + 0 + carry (propagate carry)",
        "        XCH R1        ; store high nibble back",
        "        INC R2        ; i++",
        "        ISZ R5, LOOP  ; repeat until R5 wraps to 0 (after 10 incs)",
        "        NOP           ; fallthrough when done",
        "        HLT",
    ]


# ---------------
# Harness / Main
# ---------------

def run_demo(trace: bool = False):
    """
    Assemble and execute the demo on the emulator, then print:
      Answer, Reason, Check
    """
    asm = MiniAssembler()
    program_text = triangular_sum_1_to_10_program()
    program_bytes = asm.assemble(program_text, origin=0x000)

    emu = Mini4004Emulator(program_bytes, start_address=0x000, trace=trace)
    final_cpu = emu.run()

    # Reconstruct 8-bit result from two nibbles R1:R0
    hi = final_cpu.R[1] & 0x0F
    lo = final_cpu.R[0] & 0x0F
    answer = (hi << 4) | lo

    # Reason text (what the 4004 program did)
    reason = (
        "The emulated 4-bit program loops 10 times. On each iteration it adds the "
        "current addend i (starting at 1) into the low nibble R0. Any nibble carry "
        "is propagated to the high nibble R1 by performing ADD with a zero register "
        "while including the carry. The ISZ loop counter is seeded to 6 so that after "
        "10 increments it wraps to 0, ending the loop. The final R1:R0 = 0x3:0x7 = 55."
    )

    # Independent check in Python
    expected = sum(range(1, 11))
    passed = (answer == expected)
    check = f"PASS (emulator={answer}, python={expected})" if passed \
            else f"FAIL (emulator={answer}, python={expected})"

    print(f"Answer: {answer}")
    print(f"Reason: {reason}")
    print(f"Check:  {check}")

    # Optionally, expose the final CPU state for curiosity/debugging.
    # (Not required; uncomment if you want to see it.)
    # print(final_cpu)


if __name__ == "__main__":
    # Set trace=True to see instruction-by-instruction execution.
    run_demo(trace=False)

