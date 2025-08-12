#!/usr/bin/env python3
# -----------------------------------------------------------------------------
# Kaprekar 4-digit sweep
#
# What this script does
# ---------------------
# Runs Kaprekar’s routine on every 4-digit state (0000–9999, leading zeros kept),
# counts how many reach 6174 (Kaprekar’s constant), how many do not (rep-digits),
# and measures how many iterations each converging number needs.
#
# Output format
# -------------
# 1/ Answer        : concise results (how many converge, max iterations, etc.)
# 2/ Reason why    : brief explanation of what’s going on and why some don’t converge
# 3/ Check (harness): exhaustive-check details (distribution, examples, non-convergers)
#
# Usage
# -----
# Run directly with Python 3:  python kaprekar.py
# -----------------------------------------------------------------------------

from collections import Counter

def kaprekar_step(n: int) -> int:
    """One step of Kaprekar’s routine for 4-digit numbers (leading-zero aware)."""
    s = f"{n:04d}"
    asc  = int("".join(sorted(s)))                 # e.g. '1345' -> 1345
    desc = int("".join(sorted(s, reverse=True)))   # e.g. '5431' -> 5431
    return desc - asc

def kaprekar_iterations(n: int, cap: int = 20):
    """
    Return how many steps it takes to hit 6174,
    or None for the special rep-digits that never do (e.g., 0000, 1111, …, 9999).
    A small 'cap' safety limit prevents accidental infinite loops if logic changes.
    """
    current = n
    for k in range(cap + 1):            # plenty of head-room
        if current == 6174: return k    # done
        if current == 0:    return None # stuck (rep-digit ➜ 0000 ➜ 0000 …)
        current = kaprekar_step(current)

if __name__ == "__main__":
    # ---- Exhaustive check over 0000-9999 ------------------------------------
    TOTAL     = 10_000
    non_conv  = []          # the 10 rep-digits
    dist      = Counter()   # steps ➜ count
    max_iter  = 0
    max_cases = []

    for n in range(TOTAL):
        steps = kaprekar_iterations(n)
        if steps is None:
            non_conv.append(n)
        else:
            dist[steps] += 1
            if steps > max_iter:
                max_iter, max_cases = steps, [n]
            elif steps == max_iter:
                max_cases.append(n)

    # -------------------- 1/ Answer ------------------------------------------
    print("1/ Answer")
    print(f"- Kaprekar’s constant (6174) is reached by {TOTAL - len(non_conv)}/{TOTAL} numbers.")
    print(f"- Maximum iterations required: {max_iter}")
    print(f"- Non-converging cases (rep-digits): {len(non_conv)}\n")

    # -------------------- 2/ Reason why --------------------------------------
    print("2/ Reason why")
    print("- Kaprekar’s routine: sort digits descending and ascending (keeping leading zeros), then subtract.")
    print("- All 4-digit numbers except the rep-digits eventually land on 6174 and then stay there.")
    print("- Rep-digits (e.g., 1111) produce 0000 in one step and get stuck at 0000; they never reach 6174.\n")

    # -------------------- 3/ Check (harness) ---------------------------------
    print("3/ Check (harness)")
    print("Distribution (#numbers needing k steps):")
    for k in sorted(dist):
        print(f"{k:2d} → {dist[k]:4d}")

    # show a few example numbers that require the maximum number of steps
    # (slice just to keep output tidy)
    print(f"\nExample numbers that need {max_iter} steps:", [f"{x:04d}" for x in max_cases[:10]])

    # list all non-converging rep-digits in 4-digit padded form
    print("Non-converging rep-digits:", [f"{x:04d}" for x in sorted(non_conv)])

