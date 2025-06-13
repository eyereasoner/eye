"""
oddnumber.py – Odd natural numbers 1..10 in ProbLog
===================================================

Translates the N3 numeric example into ProbLog:

* Base fact: `natural(10)`.
* Recursive rule to generate all positive naturals down to 1.
* Parity rules via arithmetic `mod`.
* Query every `prop(odd,N)` for N = 1..10.

All answers have probability **1.0** for their correct parity.
"""
from problog.program import PrologString
from problog import get_evaluatable

# Build program ------------------------------------------------------------
parts = [
    "natural(10).",
    "natural(Y) :- natural(X), Y is X - 1, Y > 0.",
    "prop(odd,X)  :- natural(X), 1 is X mod 2.",
]
for n in range(1, 11):
    parts.append(f"query(prop(odd,{n})).")

code = "\n".join(parts)
model = PrologString(code)
results = get_evaluatable().create_from(model).evaluate()

for q, p in sorted(results.items(), key=lambda item: str(item[0])):
    print(f"{q}: {p}")
