#!/usr/bin/env python3
"""
Search for the (1–9)-pandigital number whose first n digits are divisible by n
for all n = 1..9. Then print:
- Answer
- Reason why (step-by-step divisibility chain)
- Check (harness) with explicit pass/fail diagnostics
"""

from typing import List, Tuple

DIGITS = "123456789"

def is_pandigital_1_to_9(n: int) -> bool:
    s = str(n)
    return len(s) == 9 and set(s) == set(DIGITS)

def prefixes(n: int):
    s = str(n)
    for i in range(1, len(s) + 1):
        yield i, int(s[:i])

def has_prefix_divisibility_chain(n: int) -> bool:
    return all(p % k == 0 for k, p in prefixes(n))

def find_solutions() -> List[int]:
    """Backtracking search with simple pruning rules."""
    solutions: List[int] = []

    def backtrack(pos: int, prefix: int, used: set):
        if pos == 10:  # built 9 digits
            solutions.append(prefix)
            return

        for ch in DIGITS:
            if ch in used:
                continue
            d = int(ch)

            # Prune by known necessary conditions
            if pos == 2 and d % 2 != 0:   # second digit must be even
                continue
            if pos == 5 and d != 5:       # fifth digit must be 5 (divisible by 5)
                continue

            new_prefix = prefix * 10 + d
            if new_prefix % pos != 0:
                continue

            used.add(ch)
            backtrack(pos + 1, new_prefix, used)
            used.remove(ch)

    backtrack(1, 0, set())
    return solutions

def explain_solution(n: int) -> List[str]:
    lines = []
    lines.append("- It uses each digit 1–9 exactly once (1–9 pandigital).")
    lines.append("- For each n = 1..9, the number formed by the first n digits is divisible by n:")
    for k, p in prefixes(n):
        q, r = divmod(p, k)
        lines.append(f"  n={k}: {p} ÷ {k} = {q} (remainder {r})")
    return lines

def run_checks(solutions: List[int]) -> Tuple[bool, List[str]]:
    msgs = []
    all_ok = True

    if not solutions:
        msgs.append("- Found at least one solution: FAIL")
        return False, msgs

    if len(solutions) == 1:
        msgs.append("- Uniqueness (exactly one solution): OK")
    else:
        msgs.append(f"- Uniqueness (exactly one solution): FAIL (found {len(solutions)})")
        all_ok = False

    for sol in solutions:
        pan = is_pandigital_1_to_9(sol)
        chain = has_prefix_divisibility_chain(sol)
        msgs.append(f"- pandigital(1–9) for {sol}: {'OK' if pan else 'FAIL'}")
        msgs.append(f"- prefix divisibility n=1..9 for {sol}: {'OK' if chain else 'FAIL'}")
        if not pan or not chain:
            all_ok = False

    return all_ok, msgs

def main():
    # ----- Search -----
    solutions = find_solutions()

    # ----- Answer -----
    print("Answer")
    if solutions:
        print(solutions[0] if len(solutions) == 1 else solutions)
    else:
        print("No solution found.")
    print()

    # ----- Reason why -----
    print("Reason why")
    if solutions:
        for line in explain_solution(solutions[0]):
            print(line)
    else:
        print("- (no solution to explain)")
    print()

    # ----- Check (harness) -----
    print("Check (harness)")
    ok, messages = run_checks(solutions)
    for m in messages:
        print(m)
    if not ok:
        print("\nDetails:")
        if not solutions:
            print("• Search returned no solutions.")
        elif len(solutions) != 1:
            print(f"• Expected exactly 1 solution, got {len(solutions)}.")
        for sol in solutions:
            if not is_pandigital_1_to_9(sol):
                print(f"• {sol} is not 1–9 pandigital.")
            for k, p in prefixes(sol):
                if p % k != 0:
                    print(f"• Prefix of length {k} -> {p} is not divisible by {k}.")

if __name__ == "__main__":
    main()

