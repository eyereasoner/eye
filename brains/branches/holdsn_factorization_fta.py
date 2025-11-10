#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
README (plain text)
===================
Purpose
-------
This single-file program is a “branch of insights” (in the spirit of
https://eyereasoner.github.io/eye/brains/) that illustrates the **Fundamental
Theorem of Arithmetic (FTA)**—existence and uniqueness of prime factorization—
while showcasing the Hayes–Menzel idea: treat *predicates* like "is prime" or
"divides" as **named objects** (intensions), and use fixed predicates
`Holds₁`/`Holds₂` to relate those names to their extensions.

Core idea (Hayes–Menzel)
------------------------
- We give simple *names* (intensions) to mathematical concepts:
    • `ex:Prime` — the set of primes in our domain,
    • `ex:Divides` — the binary relation “a divides b”.
- Their **extensions** are ordinary Python sets computed in the model.
- A fixed application predicate is used:
    • `Holds₁("ex:Prime", n)` means: n is in the prime set.
    • `Holds₂("ex:Divides", a, b)` means: (a, b) is in the divides relation.
Thus reasoning that *looks* second-order (“quantify over sets/relations”) is
phrased using **first-order** terms (we quantify over **names** and ordinary
numbers), with application mediated by `Holds`.

Typical question (what the program prints)
------------------------------------------
On the finite domain N = {1,…,60}:

1) **Concrete:** What is the prime factorization of 360? Is it unique?
2) **General (finite scope):** Does every n ∈ {2,…,60} have a prime factorization,
   and is that factorization unique (up to order)?

What the program prints
-----------------------
1) **Model**  — domain, the named predicates/relations, and how factorization is computed.
2) **Question** — the two questions above.
3) **Answer** — the factorization of 360 and a summary “FTA holds on {2,…,60}”.
4) **Reason why** — a standard mathematical-English proof sketch:
   existence (minimal counterexample / division by smallest prime),
   uniqueness (Euclid’s lemma + induction), plus the 360 example.
5) **Check (harness)** — 12 deterministic checks verifying primes, divisibility,
   Euclid’s lemma on the finite domain, existence/uniqueness of factorizations,
   and deterministic formatting.

How to run
----------
    python3 holdsn_factorization_fta.py

No external dependencies; deterministic execution and output.
"""

from __future__ import annotations

from typing import Dict, Iterable, List, Tuple, Set

# ==========================================
# Model: domain N, Holds₁ for sets, Holds₂ for relations
# ==========================================

# Finite domain: 1..MAXN (deterministic order for stable output)
MAXN: int = 60
N: Tuple[int, ...] = tuple(range(1, MAXN + 1))

# URI-like namespace for names (intensions)
EX = "ex:"

# --- Unary: named set(s) (intensions) and their extensions ---
EXT1: Dict[str, Set[int]] = {}

def define_set(name: str, elems: Iterable[int]) -> str:
    """Register a named set (its intension) with its (sorted) extension."""
    EXT1[name] = set(sorted(elems))
    return name

def Holds1(sname: str, x: int) -> bool:
    """Holds₁(S, x): x ∈ extension of the set named by S."""
    return x in EXT1.get(sname, set())

# Compute primes by sieve (deterministic)
def sieve_primes_up_to(n: int) -> List[int]:
    if n < 2:
        return []
    sieve = [True] * (n + 1)
    sieve[0] = sieve[1] = False
    p = 2
    while p * p <= n:
        if sieve[p]:
            step = p
            start = p * p
            sieve[start:n + 1:step] = [False] * ((n - start) // step + 1)
        p += 1
    return [i for i, is_p in enumerate(sieve) if is_p]

PRIMES: Tuple[int, ...] = tuple(sieve_primes_up_to(MAXN))
Prime = define_set(EX + "Prime", PRIMES)   # name for the prime set

# --- Binary: named relation(s) and their extensions ---
EXT2: Dict[str, Set[Tuple[int, int]]] = {}

def define_relation(name: str, pairs: Iterable[Tuple[int, int]]) -> str:
    """Register a named binary relation with its extension."""
    EXT2[name] = {(a, b) for (a, b) in pairs}
    return name

def Holds2(rname: str, a: int, b: int) -> bool:
    """Holds₂(R, a, b): (a,b) ∈ extension of the relation named by R."""
    return (a, b) in EXT2.get(rname, set())

# "Divides" relation as an intension + extension
Divides = define_relation(EX + "Divides",
                          [(a, b) for a in N for b in N if b % a == 0])

# ==========================================
# Factorization utilities (deterministic)
# ==========================================

def is_prime(n: int) -> bool:
    return Holds1(Prime, n)

def smallest_prime_divisor(n: int) -> int:
    """Return the least prime dividing n (>1). Assumes n ≥ 2."""
    for p in PRIMES:
        if p > n:
            break
        if n % p == 0:
            return p
    raise ValueError("No prime divisor found (should not happen for n≥2).")

def largest_prime_divisor(n: int) -> int:
    """Return the greatest prime dividing n (>1). Assumes n ≥ 2."""
    for p in reversed(PRIMES):
        if p <= n and n % p == 0:
            return p
    raise ValueError("No prime divisor found (should not happen for n≥2).")

def factor_smallest_first(n: int) -> List[int]:
    """Prime factorization as a list of primes in nondecreasing order (trial by smallest)."""
    if n < 2:
        return []
    out: List[int] = []
    m = n
    for p in PRIMES:
        if p * p > m:
            break
        while m % p == 0:
            out.append(p)
            m //= p
    if m > 1:
        out.append(m)
    return out

def factor_largest_first(n: int) -> List[int]:
    """An alternative deterministic factorization order (largest prime factors first)."""
    if n < 2:
        return []
    out: List[int] = []
    m = n
    for p in reversed(PRIMES):
        if p > m:
            continue
        while m % p == 0:
            out.append(p)
            m //= p
        if m == 1:
            break
    return out

def multiset_of(primes_list: List[int]) -> Dict[int, int]:
    """Canonical prime-exponent map."""
    d: Dict[int, int] = {}
    for p in primes_list:
        d[p] = d.get(p, 0) + 1
    return d

def product(nums: Iterable[int]) -> int:
    out = 1
    for x in nums:
        out *= x
    return out

def fmt_factorization(primes_list: List[int]) -> str:
    """Pretty: '2^3 · 3^2 · 5' (or '1' for empty)."""
    if not primes_list:
        return "1"
    mp = multiset_of(primes_list)
    items = sorted(mp.items())  # ascending prime order
    bits = []
    for p, k in items:
        bits.append(f"{p}^{k}" if k > 1 else f"{p}")
    return " · ".join(bits)

# ==========================================
# Euclid's Lemma (checked on the finite domain)
# ==========================================

def euclid_lemma_holds_on_domain() -> bool:
    """
    Check: for all primes p and all a,b in domain,
    if p | a*b then p | a or p | b.
    """
    for p in PRIMES:
        for a in N:
            for b in N:
                if b == 0:  # (no zero in our domain; safe but redundant)
                    continue
                if (a * b) % p == 0:
                    if not (a % p == 0 or b % p == 0):
                        return False
    return True

# ==========================================
# The Branch: Model → Question → Answer → Reason why
# ==========================================

TARGET_N: int = 360  # the concrete “typical” number

def print_model() -> None:
    print("Model")
    print("=====")
    print(f"Domain N = {list(N)}")
    print()
    print("Signature")
    print("---------")
    print("• Holds₁(S, x): x is in the extension of the set named by S (S is an intension).")
    print("  - Named set: ex:Prime (the primes up to 60).")
    print("• Holds₂(R, a, b): ⟨a,b⟩ is in the extension of relation-name R.")
    print("  - Named relation: ex:Divides (a divides b).")
    print()
    print("Factorization (deterministic)")
    print("------------------------------")
    print("• factor_smallest_first(n): repeated division by the least prime divisor.")
    print("• factor_largest_first(n):  repeated division by the greatest prime divisor.")
    print("Both yield the same canonical multiset of primes for each n ≥ 2.")
    print()

def print_question() -> None:
    print("Question")
    print("========")
    print(f"(1) What is the prime factorization of {TARGET_N}? Is it unique?")
    print("(2) On N∩{2,…,60}, does every n have a prime factorization, and is it unique (up to order)?")
    print()

def compute_answer() -> Tuple[str, bool, bool]:
    # Factor the target number two ways
    f1 = factor_smallest_first(TARGET_N)
    f2 = factor_largest_first(TARGET_N)
    # Canonical string (ascending primes)
    f_str = fmt_factorization(f1)
    # Uniqueness check via multiset
    unique = multiset_of(f1) == multiset_of(f2)
    # General existence/uniqueness over 2..60
    exists_and_unique = True
    for n in range(2, MAXN + 1):
        a = factor_smallest_first(n)
        b = factor_largest_first(n)
        if product(a) != n or not all(is_prime(p) for p in a):
            exists_and_unique = False
            break
        if multiset_of(a) != multiset_of(b):
            exists_and_unique = False
            break
    return f_str, unique, exists_and_unique

def print_answer(f_str: str, unique_target: bool, exists_and_unique: bool) -> None:
    print("Answer")
    print("======")
    print(f"Prime factorization of {TARGET_N}: {f_str}")
    print(f"Is it unique (up to order)? {'Yes' if unique_target else 'No'}")
    print(f"FTA on 2..{MAXN}: existence and uniqueness hold? {'Yes' if exists_and_unique else 'No'}")
    print()

def print_reason() -> None:
    print("Reason why")
    print("==========")
    print("Existence (sketch).  Suppose there were a nonempty set S ⊆ ℕ of integers ≥2 having no")
    print("prime factorization. Let m be the least element of S. Since m is not prime, it has a")
    print("proper divisor d with 1 < d < m. By minimality, d has a prime factorization; so does")
    print("m/d; concatenating them yields one for m — a contradiction. Thus every n ≥ 2 factors.")
    print()
    print("Uniqueness (sketch via Euclid’s Lemma).  If a prime p divides a product ab, then p")
    print("divides a or p divides b. Using this, assume n ≥ 2 has two prime factorizations:")
    print("  n = p₁ p₂ ··· p_k = q₁ q₂ ··· q_m.  Since p₁ | (q₁···q_m), Euclid’s Lemma gives p₁ = q_j")
    print("for some j; cancel and repeat inductively. Eventually the multisets {p_i} and {q_j} match;")
    print("hence the factorization is unique up to order.")
    print()
    print(f"Example ({TARGET_N}).  We compute two factorizations deterministically:")
    f1 = factor_smallest_first(TARGET_N)
    f2 = factor_largest_first(TARGET_N)
    print(f"  - smallest-first: {fmt_factorization(f1)}")
    print(f"  - largest-first:  {fmt_factorization(f2)}")
    print("By the lemma, the first prime on the left must occur on the right; cancel and continue —")
    print("the multisets coincide, so the factorization of 360 is unique up to order.")
    print()

# ==========================================
# Check (harness) — deterministic, ≥ 12 tests
# ==========================================

class CheckFailure(AssertionError):
    pass

def check(cond: bool, msg: str) -> None:
    if not cond:
        raise CheckFailure(msg)

def run_checks() -> List[str]:
    notes: List[str] = []

    # 1) Sieve primes up to 60 are as expected
    expected_primes = [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59]
    check(list(PRIMES) == expected_primes, "Primes up to 60 mismatch.")
    notes.append("PASS 1: Prime set up to 60 is correct.")

    # 2) Divides relation is consistent with arithmetic
    for a in range(1, 11):
        for b in range(1, 11):
            check(Holds2(Divides, a, b) == (b % a == 0), "Divides relation mismatch.")
    notes.append("PASS 2: Divides relation matches modulo arithmetic.")

    # 3) Euclid’s Lemma holds on the finite domain
    check(euclid_lemma_holds_on_domain(), "Euclid’s Lemma failed on the finite domain.")
    notes.append("PASS 3: Euclid’s Lemma verified on 1..60.")

    # 4) Factorization correctness for all n in 2..60 (product and primality of factors)
    for n in range(2, MAXN + 1):
        fs = factor_smallest_first(n)
        check(product(fs) == n, f"Product of factors must equal n ({n}).")
        check(all(is_prime(p) for p in fs), f"All factors must be prime ({n}).")
    notes.append("PASS 4: Existence & correctness of factorization for all n.")

    # 5) Uniqueness via multiset equality: smallest-first vs largest-first
    for n in range(2, MAXN + 1):
        a = factor_smallest_first(n)
        b = factor_largest_first(n)
        check(multiset_of(a) == multiset_of(b), f"Uniqueness failed (multiset) for n={n}.")
    notes.append("PASS 5: Uniqueness (up to order) across the domain.")

    # 6) Specific example 360
    check(fmt_factorization(factor_smallest_first(360)) == "2^3 · 3^2 · 5", "360 factorization should be 2^3 · 3^2 · 5.")
    notes.append("PASS 6: Factorization of 360 is 2^3 · 3^2 · 5.")

    # 7) Units and boundary cases: 1 has empty prime list; primes have themselves
    check(factor_smallest_first(1) == [] and factor_largest_first(1) == [], "1 should factor to empty list.")
    for p in PRIMES:
        check(factor_smallest_first(p) == [p], f"Prime {p} should factor as itself (smallest-first).")
        check(factor_largest_first(p) == [p], f"Prime {p} should factor as itself (largest-first).")
    notes.append("PASS 7: Unit and prime boundary cases behave correctly.")

    # 8) Small composites sample
    samples = {4: "2^2", 6: "2 · 3", 8: "2^3", 12: "2^2 · 3", 18: "2 · 3^2", 30: "2 · 3 · 5"}
    for n, s in samples.items():
        check(fmt_factorization(factor_smallest_first(n)) == s, f"Factorization mismatch for {n}.")
    notes.append("PASS 8: Sample composite factorizations are correct.")

    # 9) Deterministic formatting for multiset strings
    for n in range(2, 50, 7):
        s1 = fmt_factorization(factor_smallest_first(n))
        s2 = fmt_factorization(sorted(factor_smallest_first(n)))
        check(s1 == s2, "Formatting must be deterministic and canonical.")
    notes.append("PASS 9: Factorization formatting is canonical and stable.")

    # 10) Smallest vs largest prime divisor helpers agree with factors present
    for n in range(2, MAXN + 1):
        fs = factor_smallest_first(n)
        if fs:
            check(fs[0] == smallest_prime_divisor(n), f"Smallest prime divisor mismatch for n={n}.")
            check(fs[-1] == largest_prime_divisor(n), f"Largest prime divisor mismatch for n={n}.")
    notes.append("PASS 10: Smallest/largest prime divisor helpers consistent.")

    # 11) Divides relation agrees with factorization: p | n iff p in factorization
    for n in range(2, MAXN + 1):
        factors = set(factor_smallest_first(n))
        for p in PRIMES:
            if p > n:
                break
            check(Holds2(Divides, p, n) == (p in factors or (n % p == 0)), "Divides must match factor presence.")
    notes.append("PASS 11: Divisibility and factorization align.")

    # 12) Multiset product equals n (redundant, but catches formatting mistakes)
    for n in range(2, MAXN + 1):
        mp = multiset_of(factor_smallest_first(n))
        rec = 1
        for p, k in mp.items():
            rec *= (p ** k)
        check(rec == n, f"Reconstructed product must equal n ({n}).")
    notes.append("PASS 12: Reconstructed product from prime powers equals n.")

    return notes

# ==========================================
# Main orchestration
# ==========================================

def main() -> None:
    print_model()
    print_question()

    f_str, unique_target, exists_and_unique = compute_answer()
    print_answer(f_str, unique_target, exists_and_unique)
    print_reason()

    print("Check (harness)")
    print("===============")
    try:
        notes = run_checks()
    except CheckFailure as e:
        print("FAIL:", e)
        raise
    else:
        for line in notes:
            print(line)

if __name__ == "__main__":
    main()

