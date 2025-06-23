# ------------------
# Collatz conjecture
# ------------------
#
# See https://en.wikipedia.org/wiki/Collatz_conjecture

from functools import lru_cache

# ---------- helpers that mirror the N3 math built-ins ----------
def remainder(a: int, b: int) -> int:        #  (a b) math:remainder ?r
    return a % b

def integer_quotient(a: int, b: int) -> int: #  (a b) math:integerQuotient ?q
    return a // b

def collatz_step(n: int) -> int:
    """
    Implements the if-then-else block in the N3 rule:
        even  -> n/2
        odd   -> 3*n + 1
    """
    return integer_quotient(n, 2) if remainder(n, 2) == 0 else 3 * n + 1

# ---------- :collatz predicate translated to a Python function ----------
@lru_cache(maxsize=None)          # caches every call exactly like log:callWithCut
def collatz(n: int) -> tuple[int, ...]:
    """
    The two N3 rules become:

    1.  Base rule
        { ?N :collatz (?N (?N)) }            <= { true log:callWithCut true. }
        →  in Python: when n == 1 return (1,)

    2.  Recursive rule (build list via list:firstRest)
        { ?N0 :collatz (?N ?M) }             <= { … }
        →  in Python: prepend n to the list returned by the recursive call
    """
    if n == 1:                               # base rule
        return (1,)                          #   ( ?M == (1) )

    next_n = collatz_step(n)                 #   ?N1 in the N3 code
    return (n,) + collatz(next_n)            #   list:firstRest (?N0 ?J)


# ---------- the N3 “query” block translated ----------
def run_query(limit: int = 10000) -> dict[int, tuple[int, ...]]:
    """
    Mirrors the block:

        {
            10000 log:repeat ?N0.
            (?N0 1) math:sum ?N.
            ?N :collatz (1 ?M).
        } log:query { … }

    In practice:
      • log:repeat enumerates 0..limit-1
      • math:sum (?N0 1) → N0 + 1   → numbers 1..limit
      • :collatz (1 ?M)  → call our Python collatz()
    """
    return {n0 + 1: collatz(n0 + 1) for n0 in range(limit)}


if __name__ == "__main__":
    sequences = run_query(10000)   # exactly what the N3 query asks for
    for n, seq in sequences.items():
        print(f"{n:4} ➜ {seq}")
