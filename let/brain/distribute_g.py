# ------------------------------------------------------------
# "Distribute G" — Find G such that the number of ways to write G
# as the sum of four positive integers (order doesn't matter)
# equals G itself.
#
# This script provides:
#   1) A clear enumerator (for "proof" style output).
#   2) A fast dynamic-programming counter to scan many n quickly.
#   3) Printed outputs listing ALL 11 partitions when n=11, and
#      verifying that n=11 is the only solution for 1 ≤ n ≤ 10000.
#
# NOTE: As requested, there is NO demonstration of the n=8 example.
# ------------------------------------------------------------

from functools import lru_cache

# ---------- Part 1: Enumerate partitions into k parts (orderless) ----------
def enumerate_partitions_into_k(n: int, k: int):
    """
    Yield all k-tuples (a1,...,ak) of positive integers with
    a1 <= a2 <= ... <= ak and a1 + ... + ak = n.
    This directly mirrors the problem statement and is suitable
    for "proof" output because we can list all solutions.
    """
    def backtrack(remaining, parts_left, min_part, current):
        if parts_left == 0:
            if remaining == 0:
                yield tuple(current)
            return
        # Choose the next part 'a'. To keep the tuple nondecreasing,
        # we require a >= min_part. To ensure enough remains for the
        # remaining (parts_left-1) positive parts, we cap 'a' at
        # remaining - (parts_left - 1)*1.
        max_a = remaining - (parts_left - 1)  # leave at least 1 for each later part
        for a in range(min_part, max_a + 1):
            current.append(a)
            yield from backtrack(remaining - a, parts_left - 1, a, current)
            current.pop()

    yield from backtrack(n, k, 1, [])


# ---------- Part 2: Fast counting via dynamic programming ----------
# We use the classic recurrence for the number of partitions p_k(n)
# into exactly k positive parts:
#     p_k(n) = p_k(n - k) + p_{k-1}(n - 1)
# Explanation:
#   - p_k(n - k): subtract 1 from each of the k parts (valid when n>=k).
#   - p_{k-1}(n - 1): remove a part equal to 1 (valid when n>=1).
# Base cases:
#   - p_0(0) = 1   (one way to partition zero using zero parts)
#   - p_k(n) = 0 when n < 0 or k < 0 or (n > 0 and k == 0)
@lru_cache(maxsize=None)
def count_partitions_into_k(n: int, k: int) -> int:
    if n == 0 and k == 0:
        return 1
    if n <= 0 or k <= 0:
        return 0
    return count_partitions_into_k(n - k, k) + count_partitions_into_k(n - 1, k - 1)


# ---------- Part 3: Goal-oriented search + "proof" output ----------
def run():
    # Search for all n ≤ LIMIT with p_4(n) = n using the fast DP counter.
    LIMIT = 10000
    matches = [n for n in range(1, LIMIT + 1) if count_partitions_into_k(n, 4) == n]

    print(f"Scanning 1..{LIMIT} for solutions to p_4(n) = n ...")
    print("Matches found:", matches)
    print()

    # "Proof" style output for the found G (should be 11):
    if matches:
        G = matches[0]
        proof_list = list(enumerate_partitions_into_k(G, 4))
        print(f"Therefore, G = {G}.")
        print(f"To justify this, here are ALL {len(proof_list)} partitions of {G} "
              f"into four positive integers (order doesn't matter):")
        for tup in proof_list:
            print(" + ".join(map(str, tup)))
    else:
        print("No solution found in the scanned range. Increase LIMIT and retry.")

# Execute
run()

