#!/usr/bin/env python3
# -------------------------------------------------------------------------
# % Peano arithmetic
# % See https://en.wikipedia.org/wiki/Peano_axioms
#
# Numbers are represented as 0 and nested s/1 functors:
#     0         → 0
#     s(0)      → 1
#     s(s(0))   → 2
# etc.
#
# In Python we **compute with ordinary ints** for efficiency and
# supply helper routines that render an int back into a Peano term
# so printed results look like the original Prolog query.
# -------------------------------------------------------------------------

# ---------- helper: convert int → Peano string --------------------------
def to_peano(n: int) -> str:
    return "0" if n == 0 else f"s({to_peano(n - 1)})"


# ---------- add ---------------------------------------------------------
# % add
# add(A, 0, A).
# add(A, s(B), s(C)) :-
#     add(A, B, C).
def add(a: int, b: int) -> int:
    return a if b == 0 else 1 + add(a, b - 1)


# ---------- multiply ----------------------------------------------------
# % multiply
# multiply(_, 0, 0).
# multiply(A, s(B), C) :-
#     multiply(A, B, D),
#     add(A, D, C).
def multiply(a: int, b: int) -> int:
    return 0 if b == 0 else add(a, multiply(a, b - 1))


# ---------- factorial (wrapper + worker) -------------------------------
# % factorial
# factorial(A, B) :-
#     fac(A, s(0), B).
#
# fac(0, A, A).
# fac(s(A), B, C) :-
#     multiply(B, s(A), D),
#     fac(A, D, C).
def factorial(n: int) -> int:
    def fac(a: int, acc: int) -> int:
        return acc if a == 0 else fac(a - 1, multiply(acc, a))
    return fac(n, 1)          # s(0) = 1


# -------------------------------------------------------------------------
# % query (Prolog-style, mirrored in Python)
# ?-
#     multiply(s(0), s(s(0)), A),
#     add(A, s(s(s(0))), B),
#     factorial(B, _).
#
# Python: perform the same steps and print Peano + decimal values.
# -------------------------------------------------------------------------

# ---------- ARC sections ------------------------------------------------
def arc_answer(A: int, B: int, fact_B: int) -> None:
    print("Answer")
    print("------")
    print(f"A  = {to_peano(A)}   # {A}")
    print(f"B  = {to_peano(B)}   # {B}")
    print(f"B! = {to_peano(fact_B)}   # {fact_B}")
    print()

def arc_reason(A: int, B: int, fact_B: int) -> None:
    print("Reason why")
    print("----------")
    print("We interpret the Prolog rules over the natural numbers:")
    print("  add(A,0,A).")
    print("  add(A,s(B),s(C)) :- add(A,B,C).")
    print("  multiply(_,0,0).")
    print("  multiply(A,s(B),C) :- multiply(A,B,D), add(A,D,C).")
    print("  factorial(N,B) :- fac(N,s(0),B) with fac(0,A,A) and fac(s(A),B,C) :- multiply(B,s(A),D), fac(A,D,C).")
    print()
    print("Applied to the query:")
    print("  1) A = multiply(s(0), s(s(0))) = 1 * 2 = 2")
    print("  2) B = add(A, s(s(s(0))))     = 2 + 3 = 5")
    print("  3) factorial(B, _)            = 5!     = 120")
    print("Each step follows the recursive clauses above; in Python we compute with ints,")
    print("then pretty-print results back as Peano terms for readability.")
    print()

def arc_check(A: int, B: int, fact_B: int) -> None:
    print("Check (harness)")
    print("---------------")
    # 0) Query results are as expected
    assert A == 2, f"A should be 2, got {A}"
    assert B == 5, f"B should be 5, got {B}"
    assert fact_B == 120, f"B! should be 120, got {fact_B}"

    # 1) Addition equals Python '+' on small domain; commutativity & associativity
    for x in range(0, 7):
        for y in range(0, 7):
            assert add(x, y) == x + y
            assert add(y, x) == y + x
        for y in range(0, 7):
            for z in range(0, 7):
                assert add(add(x, y), z) == add(x, add(y, z))

    # 2) Multiplication equals Python '*' and distributes over addition
    for x in range(0, 7):
        for y in range(0, 7):
            assert multiply(x, y) == x * y
        for y in range(0, 5):
            for z in range(0, 5):
                assert multiply(x, add(y, z)) == add(multiply(x, y), multiply(x, z))

    # 3) Factorial matches the standard iterative definition for small n
    def fact_iter(n: int) -> int:
        acc = 1
        for k in range(2, n + 1):
            acc = acc * k
        return acc
    for n in range(0, 7):
        assert factorial(n) == fact_iter(n)

    print("OK: query values correct; add/multiply align with +/*; laws (comm, assoc, distrib) hold on tests; factorial OK.")


# ---------- main --------------------------------------------------------
if __name__ == "__main__":
    # multiply(s(0), s(s(0)), A)   → 1 * 2 = 2
    A = multiply(1, 2)

    # add(A, s(s(s(0))), B)        → 2 + 3 = 5
    B = add(A, 3)

    # factorial(B, _)              → 5! = 120
    fact_B = factorial(B)

    # ARC output
    arc_answer(A, B, fact_B)
    arc_reason(A, B, fact_B)
    arc_check(A, B, fact_B)

