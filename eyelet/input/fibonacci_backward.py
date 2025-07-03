#!/usr/bin/env python3
# fib_proof.py  –  prints fibonacci values *and* backward-style proofs

from functools import lru_cache

# -------------------------------------------------------------------
# 1 ▸  Efficient numeric evaluator (fast-doubling, memoised)
# -------------------------------------------------------------------
@lru_cache(maxsize=None)
def fib(n: int) -> int:
    """Return Fibonacci(n) using fast doubling."""
    if n == 0:
        return 0
    if n == 1:
        return 1
    if n % 2 == 0:           # n = 2k
        k = n // 2
        fk = fib(k)
        fk1 = fib(k - 1)
        return (2 * fk1 + fk) * fk         # F(2k)   = F(k)·[2·F(k−1)+F(k)]
    else:                    # n = 2k+1
        k = n // 2
        fk = fib(k)
        fk1 = fib(k + 1)
        return fk**2 + fk1**2              # F(2k+1) = F(k)^2 + F(k+1)^2

# -------------------------------------------------------------------
# 2 ▸  Pretty backward-chaining proof printer
# -------------------------------------------------------------------
def prove_fib(n: int, indent=0, step=[0], summarise_above=15):
    """Print a backward proof that fib(n) = … down to fib(0)/fib(1)."""
    ind = "  " * indent
    step[0] += 1
    print(f"{ind}Step {step[0]:02d}: prove fib({n})")

    if n == 0:
        print(f"{ind}  ✓ fact   (fib(0) = 0)")
        return
    if n == 1:
        print(f"{ind}  ✓ fact   (fib(1) = 1)")
        return

    if n > summarise_above:
        # Compact the middle of huge traces
        print(f"{ind}  → via recurrence  fib({n}) = fib({n-1}) + fib({n-2})")
        print(f"{ind}    (omitting expanded sub-proofs for n > {summarise_above})")
        # Still show the two immediate recursive calls succinctly
        step[0] += 1
        print(f"{ind}  Step {step[0]:02d}: assume fib({n-1}) proven")
        step[0] += 1
        print(f"{ind}  Step {step[0]:02d}: assume fib({n-2}) proven")
        return

    # Normal detailed expansion
    print(f"{ind}  → via recurrence  fib({n}) = fib({n-1}) + fib({n-2})")
    prove_fib(n - 1, indent + 1, step, summarise_above)
    prove_fib(n - 2, indent + 1, step, summarise_above)

# -------------------------------------------------------------------
# 3 ▸  Demo – matches your original “cases” list
# -------------------------------------------------------------------
if __name__ == "__main__":
    cases = [0, 1, 6, 91, 283, 3674]

    for n in cases:
        val = fib(n)
        print(f"\nfibonacci({n}) = {val}")
        print("=== Proof ===============================================")
        prove_fib(n)

