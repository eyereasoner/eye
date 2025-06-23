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
# % query
# ?-
#     multiply(s(0), s(s(0)), A),
#     add(A, s(s(s(0))), B),
#     factorial(B, _).
#
# Python: perform the same steps and print Peano + decimal values.
# -------------------------------------------------------------------------
if __name__ == "__main__":
    # multiply(s(0), s(s(0)), A)   → 1 * 2 = 2
    A = multiply(1, 2)

    # add(A, s(s(s(0))), B)        → 2 + 3 = 5
    B = add(A, 3)

    # factorial(B, _)              → 5! = 120
    fact_B = factorial(B)

    print(f"A  = {to_peano(A)}   # {A}")
    print(f"B  = {to_peano(B)}   # {B}")
    print(f"B! = {to_peano(fact_B)}   # {fact_B}")

