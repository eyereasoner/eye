#!/usr/bin/env python3
"""
Distribute-G (ARC-ified)
========================
Find all integers G such that the number of ways to write G as the sum of four
positive integers (order does not matter) equals G itself.

We provide:
  • A clean enumerator (lists all partitions for proof-style output).
  • A fast DP counter p_k(n) with memoization.
  • ARC sections:
      Answer — the solution(s) and their partitions (shows all 11 for G=11),
      Reason why — brief derivation logic,
      Check (harness) — determinism + cross-checks (enumerator vs DP, uniqueness up to 10 000).
"""

from __future__ import annotations
from functools import lru_cache
from typing import Generator, Iterable, List, Tuple

# ---------- Part 1: Enumerate partitions into k parts (orderless) ----------
def enumerate_partitions_into_k(n: int, k: int) -> Generator[Tuple[int, ...], None, None]:
    """
    Yield all k-tuples (a1,...,ak) of positive integers with
    a1 <= a2 <= ... <= ak and a1 + ... + ak = n.
    Deterministic, lexicographic by construction.
    """
    def backtrack(remaining: int, parts_left: int, min_part: int, current: List[int]):
        if parts_left == 0:
            if remaining == 0:
                yield tuple(current)
            return
        # keep enough for remaining positive parts
        max_a = remaining - (parts_left - 1)
        for a in range(min_part, max_a + 1):
            current.append(a)
            yield from backtrack(remaining - a, parts_left - 1, a, current)
            current.pop()

    yield from backtrack(n, k, 1, [])


# ---------- Part 2: Fast counting via dynamic programming ----------
# p_k(n) (exactly k positive parts) by the classic recurrence:
#   p_k(n) = p_k(n - k) + p_{k-1}(n - 1)  with  p_0(0)=1, otherwise 0 on invalid args.
@lru_cache(maxsize=None)
def count_partitions_into_k(n: int, k: int) -> int:
    if n == 0 and k == 0:
        return 1
    if n <= 0 or k <= 0:
        return 0
    return count_partitions_into_k(n - k, k) + count_partitions_into_k(n - 1, k - 1)


# ---------- Helpers ----------
def list_partitions(n: int, k: int) -> List[Tuple[int, ...]]:
    return list(enumerate_partitions_into_k(n, k))

def check_is_partition_list(n: int, k: int, parts: Iterable[Tuple[int, ...]]) -> None:
    seen = set()
    for t in parts:
        assert len(t) == k, "Wrong length"
        assert all(x > 0 for x in t), "Non-positive part"
        assert sum(t) == n, "Sum mismatch"
        assert all(t[i] <= t[i+1] for i in range(len(t)-1)), "Not nondecreasing"
        assert t not in seen, "Duplicate partition"
        seen.add(t)


# ---------- ARC Main ----------
def main() -> None:
    LIMIT = 10_000
    K = 4

    # Search quickly using DP
    matches = [n for n in range(1, LIMIT + 1) if count_partitions_into_k(n, K) == n]

    # ── Answer ──────────────────────────────────────────────────────────────
    print("Answer")
    print("------")
    if matches:
        print(f"Solutions to p_{K}(n) = n with 1 ≤ n ≤ {LIMIT}: {matches}")
    else:
        print(f"No solutions in 1..{LIMIT}")
    print()

    # For the canonical case G=11, print all partitions (there are 11)
    if 11 in matches:
        parts_11 = list_partitions(11, K)
        print("All partitions of 11 into four positive integers (orderless):")
        for tup in parts_11:
            print(" + ".join(map(str, tup)))
    print()

    # ── Reason why ─────────────────────────────────────────────────────────
    print("Reason why")
    print("----------")
    print("We count p_k(n), the number of partitions of n into exactly k positive parts,")
    print("via the standard recurrence p_k(n) = p_k(n-k) + p_{k-1}(n-1) with p_0(0)=1.")
    print("We then test p_4(n) = n. For n=11 we enumerate all nondecreasing 4-tuples")
    print("summing to 11 and observe there are exactly 11 of them, matching p_4(11)=11.")
    print()

    # ── Check (harness) ────────────────────────────────────────────────────
    print("Check (harness)")
    print("---------------")

    # 1) Determinism of enumeration order and content for n=11
    parts_a = list_partitions(11, K)
    parts_b = list_partitions(11, K)
    assert parts_a == parts_b, "Non-deterministic enumeration for n=11"
    check_is_partition_list(11, K, parts_a)

    # 2) Enumerator vs DP count agreement for a sweep
    for n in range(1, 41):  # small sweep is enough to sanity-check
        if n <= 30:  # keep enumeration cheap
            cnt_enum = len(list_partitions(n, K))
            cnt_dp = count_partitions_into_k(n, K)
            assert cnt_enum == cnt_dp, f"Enumerator vs DP mismatch at n={n}: {cnt_enum} vs {cnt_dp}"

    # 3) Uniqueness claim in 1..LIMIT (expected single solution 11)
    if matches:
        assert matches == [11], f"Unexpected solutions in 1..{LIMIT}: {matches}"

    # 4) Exact count check at G=11
    assert count_partitions_into_k(11, K) == 11, "p_4(11) must be 11"

    print("All checks passed.")
    if matches:
        print(f"  unique solution : {matches[0]}")

if __name__ == "__main__":
    main()

