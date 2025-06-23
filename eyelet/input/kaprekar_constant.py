from collections import Counter

def kaprekar_step(n: int) -> int:
    """One step of Kaprekar’s routine (4-digit, leading-zero aware)."""
    s = f"{n:04d}"
    asc  = int("".join(sorted(s)))
    desc = int("".join(sorted(s, reverse=True)))
    return desc - asc

def kaprekar_iterations(n: int, cap: int = 20):
    """
    Return how many steps it takes to hit 6174,
    or None for the special rep-digits that never do.
    """
    current = n
    for k in range(cap + 1):            # plenty of head-room
        if current == 6174: return k    # done
        if current == 0:    return None # stuck (rep-digit ➜ 0000 ➜ 0000 …)
        current = kaprekar_step(current)

# ---- Exhaustive check over 0000-9999 ----------------------------------------
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

# ---- Results ---------------------------------------------------------------
print(f"Kaprekar’s constant reached by {TOTAL-len(non_conv)}/{TOTAL} numbers.")
print(f"Maximum iterations required: {max_iter}\n")
print("Distribution (#numbers needing k steps):")
for k in sorted(dist):
    print(f"{k:2d} → {dist[k]:4d}")
print("\nExample numbers that need 7 steps:", max_cases[:10])
print("Non-converging rep-digits:", [f'{x:04d}' for x in non_conv])

