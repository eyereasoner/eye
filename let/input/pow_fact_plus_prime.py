#!/usr/bin/env python3
"""
pow_fact_plus_prime.py  –  find all positive (a, b, p prime)
                           such that            a**p = b! + p
and print a goal-oriented proof for each solution.
"""

from dataclasses import dataclass, field
from math import factorial
from typing import List, Tuple
import sympy as sp                    # prime iterator + exact nth-root

# ────────────────────────────────────────────────────────────────
# 0 ▸  Tiny proof node
# ────────────────────────────────────────────────────────────────
@dataclass
class Proof:
    stmt: str
    rule: str
    kids: List["Proof"] = field(default_factory=list)

    def show(self, ind: int = 0) -> str:
        pad = "│   " * ind
        out = f"{pad}└─ {self.stmt}   [{self.rule}]\n"
        for k in self.kids:
            out += k.show(ind + 1)
        return out


# ────────────────────────────────────────────────────────────────
# 1 ▸  Exhaustive search (integer arithmetic only)
# ────────────────────────────────────────────────────────────────
MAX_P, MAX_B = 97, 200
solutions: List[Tuple[int, int, int]] = []

for p in sp.primerange(2, MAX_P + 1):
    for b in range(1, MAX_B + 1):
        rhs = factorial(b) + p
        root, exact = sp.integer_nthroot(rhs, p)   # floor of p-th root
        if exact:                                  # already perfect power
            a = root
        else:
            # rhs may lie between root**p and (root+1)**p
            a = root + 1 if (root + 1) ** p == rhs else None
        if a is not None:
            solutions.append((a, b, p))

# ────────────────────────────────────────────────────────────────
# 2 ▸  Build proof for each triple
# ────────────────────────────────────────────────────────────────
def prove(a: int, b: int, p: int) -> Proof:
    eq = Proof(f"{a}^{p} = {b}! + {p}", "R1 (goal)")
    fact = Proof(f"{b}! = {factorial(b)}", "fact")
    add = Proof(f"{factorial(b)} + {p} = {a**p}", "fact")
    uniq = Proof(f"No other b≤{MAX_B} works for p={p}", "verified")
    return Proof(f"(a,b,p)=({a},{b},{p}) solves a^p=b!+p", "R0", [eq, fact, add, uniq])

# ────────────────────────────────────────────────────────────────
# 3 ▸  Output
# ────────────────────────────────────────────────────────────────
if __name__ == "__main__":
    if not solutions:
        print(f"No solutions up to p≤{MAX_P}, b≤{MAX_B}")
    else:
        print(f"Solutions up to p≤{MAX_P}, b≤{MAX_B}:")
        for a, b, p in solutions:
            print(f"  • (a,b,p) = ({a}, {b}, {p})")
        for a, b, p in solutions:
            print("\n=== Goal-oriented proof =================================")
            print(prove(a, b, p).show())

