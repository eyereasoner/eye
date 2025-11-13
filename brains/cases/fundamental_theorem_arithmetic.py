#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
README (plain text)
===================
Purpose
-------
This CASE module for `eyezero.py` is a “branch of insights - brains” case
(in the sense of https://eyereasoner.github.io/eye/brains/) that illustrates
the **Fundamental Theorem of Arithmetic (FTA)** on a finite domain, while
showcasing the **Hayes–Menzel** idea:

  • We give *names* (intensions) to mathematical predicates/relations, e.g.
      - `ex:Prime`   — the set of primes in our domain;
      - `ex:Divides` — the binary relation “a divides b”.
  • Their **extensions** are ordinary Python sets of numbers/pairs.
  • A fixed *application* vocabulary is used:

        ex:holds1(S, x)      (NAME, IND)       — “x is in the set named S”
        ex:holds2(R, a, b)   (NAME, IND, IND)  — “⟨a,b⟩ in the relation named R”

    So “prime(n)” and “Divides(a,b)” are modeled as *first-order* predicates
    over **names and numbers-as-individuals**. Internally we encode numbers as
    strings ("1","2",...) in the logical world; arithmetic uses Python ints.

Domain & scope
--------------
We work over N = {1,…,60} with:

  • ex:Prime   — primes up to 60;
  • ex:Divides — divides relation restricted to N×N.

We compute factorizations only for numbers in [1,60], and we verify:

  • Concrete example: factorization of 360.
  • Finite FTA: for every n ∈ {2,…,60},
      - n has a prime factorization (existence),
      - that factorization is unique up to order (uniqueness).

What this CASE prints
---------------------
1) **Model** — N, the named predicates/relations, and their interpretation.
2) **Question** — two core questions:

       Q1  What is the prime factorization of 360? Is it unique?
       Q2  On N∩{2,…,60}, does FTA (existence + uniqueness) hold?

3) **Answer** — concrete factorization string for 360 and Yes/No answers.
4) **Reason why** — a standard mathematical-English proof sketch:
     - existence via a minimal counterexample argument;
     - uniqueness via Euclid’s Lemma + induction;
     - and the 360 example.
5) **Check (harness)** — 12+ deterministic tests:
     - primes and divides are correct,
     - Euclid’s Lemma holds on the finite domain,
     - every n has a correct prime factorization,
     - uniqueness via multiset equality,
     - EyeZero’s bottom-up view of holds₁/holds₂ matches the extensional store.

How to run
----------
Standalone:

    python3 fundamental_theorem_arithmetic.py

Via EyeZero runner:

    python3 eyezero.py fundamental_theorem_arithmetic.py

Output
------
Model → Question → Answer → Reason why → Check (harness)
"""

from __future__ import annotations

from typing import Dict, Iterable, List, Tuple, Set

from eyezero import (
    Var, Atom, Clause, atom, fact,
    solve_topdown, solve_bottomup, match_against_facts,
    NAME, IND, Signature, deref, local, fmt_pairs, fmt_set,
)

# ─────────────────────────────────────────────────────────────────────────────
# Model: domain N, named sets/relations, and EyeZero facts
# ─────────────────────────────────────────────────────────────────────────────

# Finite domain: 1..MAXN (deterministic order for stable output)
MAXN: int = 60
N: Tuple[int, ...] = tuple(range(1, MAXN + 1))   # arithmetic domain (ints)

# URI-like namespace for names (intensions)
EX = "ex:"

# Application predicates (first-order, in the EyeZero sense)
Holds1Pred = EX + "holds1"   # (NAME, IND)
Holds2Pred = EX + "holds2"   # (NAME, IND, IND)

# Signature for EyeZero
SIGNATURE: Signature = {
    Holds1Pred: (NAME, IND),
    Holds2Pred: (NAME, IND, IND),
}

# EyeZero program of facts (no rules needed here)
PROGRAM: List[Clause] = []

# --- Unary: named set(s) (intensions) and their extensions ---
# Internally, individuals in the logic world are strings "1", "2", ...

EXT1: Dict[str, Set[str]] = {}  # map set-name -> extension as a set of IND-strings


def define_set(name: str, elems: Iterable[int]) -> str:
    """
    Register a named set (its intension) with its extension, and
    add EyeZero facts holds1(name,x) for all x in the extension.

    elems are Python ints; we store their string codes as IND terms.
    """
    s = {str(x) for x in elems}
    EXT1[name] = s
    for x in s:
        PROGRAM.append(fact(Holds1Pred, name, x))
    return name


def Holds1_py(sname: str, x: int) -> bool:
    """Holds₁(S,x): x ∈ extension of the set named by S (using extensional store)."""
    return str(x) in EXT1.get(sname, set())


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
PrimeSet = define_set(EX + "Prime", PRIMES)  # name for the prime set

# --- Binary: named relation(s) and their extensions ---
# Again, store IND arguments as strings.

EXT2: Dict[str, Set[Tuple[str, str]]] = {}  # map relation-name -> set of (a,b) (strings)


def define_relation(name: str, pairs: Iterable[Tuple[int, int]]) -> str:
    """
    Register a named binary relation with its extension, and
    add EyeZero facts holds2(name,a,b) for all (a,b) in the extension.

    pairs are Python ints; we store their string codes as IND terms.
    """
    s: Set[Tuple[str, str]] = {(str(a), str(b)) for (a, b) in pairs}
    EXT2[name] = s
    for (a, b) in s:
        PROGRAM.append(fact(Holds2Pred, name, a, b))
    return name


def Holds2_py(rname: str, a: int, b: int) -> bool:
    """Holds₂(R,a,b): (a,b) ∈ extension of the relation named by R (using extensional store)."""
    return (str(a), str(b)) in EXT2.get(rname, set())


# "Divides" relation as an intension + extension
Divides = define_relation(
    EX + "Divides",
    [(a, b) for a in N for b in N if b % a == 0],
)

# ─────────────────────────────────────────────────────────────────────────────
# Factorization utilities (deterministic)
# ─────────────────────────────────────────────────────────────────────────────

def is_prime(n: int) -> bool:
    return Holds1_py(PrimeSet, n)


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
    """Prime factorization as a list of primes in nondecreasing order."""
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
    """Alternative deterministic factorization order (largest prime factors first)."""
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

# ─────────────────────────────────────────────────────────────────────────────
# Euclid's Lemma (checked on the finite domain)
# ─────────────────────────────────────────────────────────────────────────────

def euclid_lemma_holds_on_domain() -> bool:
    """
    Check: for all primes p and all a,b in domain, if p | (a*b) then
    p | a or p | b.

    We use plain integer arithmetic (×, %) and the PRIMES list.
    """
    for p in PRIMES:
        for a in N:
            for b in N:
                if (a * b) % p == 0:
                    if not (a % p == 0 or b % p == 0):
                        return False
    return True

# ─────────────────────────────────────────────────────────────────────────────
# The Branch: Model → Question → Answer → Reason why
# ─────────────────────────────────────────────────────────────────────────────

TARGET_N: int = 360  # the concrete “typical” number

def print_model() -> None:
    print("Model")
    print("=====")
    print(f"Domain N = {list(N)}")
    print()
    print("Named predicates & relations (intensional level)")
    print("-----------------------------------------------")
    print(f"• {PrimeSet} : the set of primes up to {MAXN}.")
    print(f"• {Divides}  : the divides relation restricted to N×N.")
    print()
    print("Application predicates (first-order, EyeZero-style)")
    print("---------------------------------------------------")
    print(f"• {Holds1Pred}(S,x) — x is in the extension of the set named S.")
    print(f"• {Holds2Pred}(R,a,b) — ⟨a,b⟩ is in the extension of relation-name R.")
    print("  In particular:")
    print(f"    - {Holds1Pred}({PrimeSet}, n) ⇔ n is prime (for n in N).")
    print(f"    - {Holds2Pred}({Divides}, a, b) ⇔ a divides b (for a,b in N).")
    print()
    print("Factorization (deterministic algorithms)")
    print("----------------------------------------")
    print("• factor_smallest_first(n): repeated division by the least prime divisor.")
    print("• factor_largest_first(n): repeated division by the greatest prime divisor.")
    print("Both yield the same canonical multiset of primes for each n ≥ 2.\n")


def print_question() -> None:
    print("Question")
    print("========")
    print(f"(Q1) What is the prime factorization of {TARGET_N}? Is it unique (up to order)?")
    print(f"(Q2) On N∩{{2,…,{MAXN}}}, does every n have a prime factorization,")
    print("     and is that factorization unique up to order (finite-scope FTA)?\n")


def compute_answer() -> Tuple[str, bool, bool]:
    # Factor the target number two ways
    f1 = factor_smallest_first(TARGET_N)
    f2 = factor_largest_first(TARGET_N)

    # Canonical string (ascending primes)
    f_str = fmt_factorization(f1)

    # Uniqueness check via multiset equality
    unique_360 = multiset_of(f1) == multiset_of(f2)

    # General existence/uniqueness over 2..MAXN
    exists_and_unique = True
    for n in range(2, MAXN + 1):
        a = factor_smallest_first(n)
        b = factor_largest_first(n)
        # existence & correctness: product is n and all factors are prime
        if product(a) != n or not all(is_prime(p) for p in a):
            exists_and_unique = False
            break
        # uniqueness up to order: same multiset
        if multiset_of(a) != multiset_of(b):
            exists_and_unique = False
            break

    return f_str, unique_360, exists_and_unique

# ─────────────────────────────────────────────────────────────────────────────
# EyeZero-facing queries
# ─────────────────────────────────────────────────────────────────────────────

def run_queries():
    """
    For compatibility with eyezero.py, we return a tuple of “results”.
    We do not actually use EyeZero for factorization; the reasoning is
    arithmetic here, while EyeZero is used in the harness as a model checker.
    """
    f_str, unique_360, fta_ok = compute_answer()
    # Engine tag "meta" indicates that this is meta-level reasoning here.
    res1 = ("Q1", "meta", f_str, unique_360)
    res2 = ("Q2", "meta", fta_ok, "n/a")
    return (res1, res2)

# ─────────────────────────────────────────────────────────────────────────────
# Answers + Reason why
# ─────────────────────────────────────────────────────────────────────────────

def print_answer(res1, res2) -> None:
    print("Answer")
    print("======")
    tag1, eng1, f_str, unique_360 = res1
    tag2, eng2, fta_ok, _ = res2

    print(f"{tag1}) Engine: {eng1}")
    print(f"    Prime factorization of {TARGET_N}: {f_str}")
    print(f"    Is it unique (up to order)? {'Yes' if unique_360 else 'No'}")
    print()
    print(f"{tag2}) Engine: {eng2}")
    print(f"    FTA on 2..{MAXN}: existence and uniqueness hold?")
    print(f"    {'Yes' if fta_ok else 'No'}\n")


def print_reason(eng1: str, eng2: str) -> None:
    print("Reason why")
    print("==========")
    print("Existence (sketch).")
    print("-------------------")
    print("Suppose there were a nonempty set S ⊆ ℕ of integers ≥ 2 that have no prime")
    print("factorization. Let m be the least element of S. Since m is not prime, it has a")
    print("proper divisor d with 1 < d < m. By minimality, d has a prime factorization;")
    print("so does m/d. Concatenating these factorizations yields one for m — a contradiction.")
    print("Thus every n ≥ 2 has a prime factorization.")
    print()
    print("Uniqueness (sketch via Euclid’s Lemma).")
    print("--------------------------------------")
    print("Euclid’s Lemma: If a prime p divides a product ab, then p divides a or p divides b.")
    print("Assume n ≥ 2 has two prime factorizations:")
    print("  n = p₁ p₂ ··· p_k = q₁ q₂ ··· q_m.")
    print("Since p₁ | (q₁···q_m), Euclid’s Lemma gives p₁ = q_j for some j; cancel p₁ on both")
    print("sides and repeat inductively. Eventually the multisets {p_i} and {q_j} match;")
    print("hence the factorization is unique up to order.")
    print()
    print(f"Example: n = {TARGET_N}.")
    print("------------------------")
    f1 = factor_smallest_first(TARGET_N)
    f2 = factor_largest_first(TARGET_N)
    print(f"smallest-first: {fmt_factorization(f1)}")
    print(f"largest-first : {fmt_factorization(f2)}")
    print("By the lemma, the first prime on the left must occur on the right; cancel and")
    print("continue. The multisets coincide, so the factorization of 360 is unique up to")
    print("ordering of the prime factors.\n")

# ─────────────────────────────────────────────────────────────────────────────
# Check (harness) — deterministic, ≥ 12 tests + EyeZero cross-check
# ─────────────────────────────────────────────────────────────────────────────

class CheckFailure(AssertionError):
    pass


def check(cond: bool, msg: str) -> None:
    if not cond:
        raise CheckFailure(msg)


def run_checks() -> List[str]:
    notes: List[str] = []

    # 0) EyeZero bottom-up view of holds1/holds2 matches the extensional store
    facts, _ = solve_bottomup(PROGRAM, SIGNATURE)
    h1_facts = facts.get(Holds1Pred, set())
    h2_facts = facts.get(Holds2Pred, set())

    ext1_union = {(PrimeSet, x) for x in EXT1[PrimeSet]}
    ext2_union = {(Divides, a, b) for (a, b) in EXT2[Divides]}

    check(h1_facts == ext1_union, "EyeZero holds1 facts do not match extensional Prime set.")
    check(h2_facts == ext2_union, "EyeZero holds2 facts do not match extensional Divides relation.")
    notes.append("PASS 0: EyeZero bottom-up holds₁/holds₂ facts match the extensional store.")

    # 1) Sieve primes up to 60 are as expected
    expected_primes = [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59]
    check(list(PRIMES) == expected_primes, "Primes up to 60 mismatch.")
    notes.append("PASS 1: Prime set up to 60 is correct.")

    # 2) Divides relation is consistent with arithmetic (sample 1..10)
    for a in range(1, 11):
        for b in range(1, 11):
            check(Holds2_py(Divides, a, b) == (b % a == 0), "Divides relation mismatch.")
    notes.append("PASS 2: Divides relation matches modulo arithmetic on 1..10.")

    # 3) Euclid’s Lemma holds on the finite domain
    check(euclid_lemma_holds_on_domain(), "Euclid’s Lemma failed on the finite domain.")
    notes.append("PASS 3: Euclid’s Lemma verified on 1..60.")

    # 4) Factorization correctness for all n in 2..60 (product and primality of factors)
    for n in range(2, MAXN + 1):
        fs = factor_smallest_first(n)
        check(product(fs) == n, f"Product of factors must equal n ({n}).")
        check(all(is_prime(p) for p in fs), f"All factors must be prime ({n}).")
    notes.append("PASS 4: Existence & correctness of factorization for all n in 2..60.")

    # 5) Uniqueness via multiset equality: smallest-first vs largest-first
    for n in range(2, MAXN + 1):
        a = factor_smallest_first(n)
        b = factor_largest_first(n)
        check(multiset_of(a) == multiset_of(b), f"Uniqueness failed (multiset) for n={n}.")
    notes.append("PASS 5: Uniqueness (up to order) across the domain.")

    # 6) Specific example 360
    check(fmt_factorization(factor_smallest_first(360)) == "2^3 · 3^2 · 5",
          "360 factorization should be 2^3 · 3^2 · 5.")
    notes.append("PASS 6: Factorization of 360 is 2^3 · 3^2 · 5.")

    # 7) Units and boundary cases: 1 has empty prime list; primes have themselves
    check(factor_smallest_first(1) == [] and factor_largest_first(1) == [],
          "1 should factor to empty list.")
    for p in PRIMES:
        check(factor_smallest_first(p) == [p], f"Prime {p} should factor as itself (smallest-first).")
        check(factor_largest_first(p) == [p], f"Prime {p} should factor as itself (largest-first).")
    notes.append("PASS 7: Unit and prime boundary cases behave correctly.")

    # 8) Small composites sample
    samples = {
        4: "2^2",
        6: "2 · 3",
        8: "2^3",
        12: "2^2 · 3",
        18: "2 · 3^2",
        30: "2 · 3 · 5",
    }
    for n, s in samples.items():
        check(fmt_factorization(factor_smallest_first(n)) == s,
              f"Factorization mismatch for {n}.")
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
            check(fs[0] == smallest_prime_divisor(n),
                  f"Smallest prime divisor mismatch for n={n}.")
            check(fs[-1] == largest_prime_divisor(n),
                  f"Largest prime divisor mismatch for n={n}.")
    notes.append("PASS 10: Smallest/largest prime divisor helpers consistent.")

    # 11) Divides relation agrees with factorization: p | n iff p in factorization
    for n in range(2, MAXN + 1):
        factors = set(factor_smallest_first(n))
        for p in PRIMES:
            if p > n:
                break
            expected = (n % p == 0)
            check(Holds2_py(Divides, p, n) == expected,
                  "Divides must match arithmetic divisibility.")
            if expected:
                check(p in factors, f"Divides({p},{n}) implies {p} is a prime factor of {n}.")
    notes.append("PASS 11: Divisibility and factorization align on the domain.")

    # 12) Multiset product equals n (reconstruction)
    for n in range(2, MAXN + 1):
        mp = multiset_of(factor_smallest_first(n))
        rec = 1
        for p, k in mp.items():
            rec *= (p ** k)
        check(rec == n, f"Reconstructed product must equal n ({n}).")
    notes.append("PASS 12: Reconstructed product from prime powers equals n.")

    return notes

# ─────────────────────────────────────────────────────────────────────────────
# Main
# ─────────────────────────────────────────────────────────────────────────────

def main() -> None:
    print_model()
    print_question()
    res1, res2 = run_queries()
    print_answer(res1, res2)
    print_reason(res1[1], res2[1])
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

