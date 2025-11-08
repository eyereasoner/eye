#!/usr/bin/env python3
"""
Self-contained Python program that demonstrates and verifies:

    For every integer n, the last digit of n^5 is the same as the last digit of n.

Output sections (with slightly improved spacing/layout):
  - Answer
  - Reason why (a short, rigorous modular-arithmetic proof)
  - Independent Check (harness) with several complementary tests

This script is intentionally heavily commented for clarity and instructional use.
"""

from typing import List, Tuple, Optional


# -----------------------------
# Core helpers (with comments)
# -----------------------------

def last_digit(n: int) -> int:
    """
    Return the *last decimal digit* of n, i.e., n mod 10 in {0,1,2,3,4,5,6,7,8,9}.

    Notes:
    - Python's % operator returns the least nonnegative remainder, so n % 10 is
      exactly the last decimal digit for any integer n (including negatives).
      Examples:
        137 % 10 == 7
        -3  % 10 == 7  (since -3 ≡ 7 (mod 10))
    """
    return n % 10


def holds(n: int) -> bool:
    """
    Property under test: "last digit of n^5 equals last digit of n".
    We compare (n^5) mod 10 to n mod 10.
    """
    return last_digit(n**5) == last_digit(n)


def residues_table() -> List[Tuple[int, int]]:
    """
    Compute a compact 'exhaustive' check at the level of last digits:
    For each possible last digit d in {0,...,9}, compute d^5 mod 10.

    Why this works:
    - If two integers have the same last digit, they are congruent modulo 10.
    - The last digit of n depends only on n modulo 10.
    - Therefore, checking the statement for representatives d=0..9 covers all n.
    """
    # Python's built-in pow supports a fast modular form: pow(base, exp, mod).
    # pow(d, 5, 10) computes (d^5) mod 10 without forming d^5 in full.
    return [(d, pow(d, 5, 10)) for d in range(10)]


def brute_force_range(a: int, b: int, step: int = 1) -> Tuple[int, int, Optional[int]]:
    """
    Deterministic 'wide net' check over a contiguous integer interval [a, b].

    Returns:
      (count_passed, total_checked, counterexample_or_None)

    If a counterexample is found, iteration stops early and that n is returned.
    Otherwise, counterexample is None and count_passed == total_checked.
    """
    count = 0
    total = 0
    counterexample = None

    for n in range(a, b + 1, step):
        total += 1
        if holds(n):
            count += 1
        else:
            counterexample = n
            break

    return count, total, counterexample


def random_trials(trials: int = 100_000, seed: int = 20250819) -> Tuple[int, int, Optional[int]]:
    """
    Pseudo-random, large-domain spot-check that includes negative and large-magnitude integers.

    We deliberately include negatives and large magnitudes because:
    - The proof is universal (all integers), not just small positives.
    - It's reassuring to test a diverse set of residues mod 10 across a wide range.

    Returns:
      (count_passed, trials_requested, counterexample_or_None)
    """
    import random
    random.seed(seed)

    count = 0
    counterexample = None

    # Choose a broad symmetric range around 0; the exact range is arbitrary.
    # We're exercising different residues mod 10, not attempting completeness here.
    LO, HI = -10**12, 10**12

    for _ in range(trials):
        n = random.randint(LO, HI)
        if holds(n):
            count += 1
        else:
            counterexample = n
            break

    return count, trials, counterexample


# -----------------------------
# Explanatory text (proof)
# -----------------------------

def reason_paragraphs() -> List[str]:
    """
    Return the proof as distinct paragraphs to allow nicer spacing in the output.
    """
    return [
        # Paragraph 1: modulo 5
        ("(1) Mod 5:\n"
         "    By Fermat's Little Theorem, for any integer n not divisible by 5 we have n^4 ≡ 1 (mod 5),\n"
         "    hence n^5 ≡ n (mod 5). If 5 | n then both sides are 0 modulo 5. So in all cases n^5 ≡ n (mod 5)."),
        # Paragraph 2: modulo 2
        ("(2) Mod 2:\n"
         "    Parity is preserved by an odd power: n odd ⇒ n^5 odd, n even ⇒ n^5 even, so n^5 ≡ n (mod 2)."),
        # Paragraph 3: combine
        ("(3) Combine (CRT):\n"
         "    Since 10 = 2·5 and gcd(2,5) = 1, the Chinese Remainder Theorem yields n^5 ≡ n (mod 10).\n"
         "    Therefore, n and n^5 have the same last decimal digit."),
        # Paragraph 4: alternative
        ("Alternative quick check:\n"
         "    For d = 0,…,9 compute d^5 mod 10; each equals d, which exhausts all last digits.")
    ]


# -----------------------------
# Pretty-print helpers
# -----------------------------

def print_section_title(title: str) -> None:
    """
    Print a section title with consistent spacing before/after.
    (Slightly improved spacing over the original version.)
    """
    print(title)
    print()  # blank line after the title


def print_block(lines: List[str]) -> None:
    """
    Print a block of text with a small left indent and blank lines between paragraphs.
    """
    for i, para in enumerate(lines):
        for line in para.splitlines():
            print("  " + line)
        if i != len(lines) - 1:
            print()  # blank line between paragraphs
    print()  # trailing blank line after the block


# -----------------------------
# Program entry point
# -----------------------------

def main() -> None:
    # --- Section: Answer ---
    print_section_title("Answer:")
    print_block([
        "For every integer n, the last digit of n^5 equals the last digit of n."
    ])

    # --- Section: Reason why ---
    print_section_title("Reason why:")
    print_block(reason_paragraphs())

    # --- Section: Independent Check (harness) ---
    print_section_title("Independent Check (harness):")

    # 1) Exhaustive residue table for the 10 possible last digits
    table = residues_table()
    mapping = ", ".join(f"{d}→{r}" for d, r in table)
    ok = all(d == r for d, r in table)
    print("  • Residue check mod 10 for digits 0–9")
    print(f"    Mapping: {mapping}")
    print(f"    Result : {'OK — each d^5≡d (mod 10)' if ok else 'FAIL'}")
    print()  # spacer between subchecks

    # 2) Brute-force contiguous range (deterministic)
    a, b = -200_000, 200_000
    passed, total, bad = brute_force_range(a, b)
    print("  • Brute-force check on a contiguous range")
    if bad is None:
        print(f"    Range  : [{a}, {b}]")
        print(f"    Result : {passed}/{total} passed — OK")
    else:
        print(f"    Counterexample found in range [{a}, {b}]: n={bad}")
    print()  # spacer between subchecks

    # 3) Random spot-checks over a huge domain (includes negatives/positives)
    passed, trials, bad = random_trials(trials=100_000, seed=20250819)
    print("  • Random spot-checks over a wide domain")
    if bad is None:
        print(f"    Domain : integers in [−10^12, 10^12]")
        print(f"    Trials : {trials}")
        print(f"    Result : {passed}/{trials} passed — OK")
    else:
        print(f"    Counterexample found in random trials: n={bad}")
    print()  # spacer between subchecks

    # Optional: quick demonstration for a few hand-picked values (including negatives).
    demo_vals = [0, 1, 2, 7, 9, 10, 11, -1, -2, -13, 10**6 + 3]
    demos = "\n".join(
        f"    n={n:<12} last(n)={last_digit(n)}   last(n^5)={last_digit(n**5)}"
        for n in demo_vals
    )
    print("  • Sample demonstrations")
    print(demos)
    print()  # final trailing newline for a clean end


if __name__ == "__main__":
    main()

