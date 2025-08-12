#!/usr/bin/env python3
"""
New Scientist puzzle Enigma 1225 — faithful Python translation of:
  https://raw.githubusercontent.com/eyereasoner/arvol/refs/heads/main/input/enigma1225.pl

Key fidelity points vs. Prolog:
- Partition successor sequence is generated deterministically from [(N,1)].
- splitter() does NOT reverse its accumulator (my earlier version did; fixed).
- rep_perm builds permutations by rotating each chunk left then flattening.
- Matrix constraint: M == transpose( list_permute(P, M) ).
- Rows must be pairwise distinct (as lists of logic variables / classes).
- Symbols are printed as first-occurrence class IDs (not frequency ranks).
- Total = sum_i (i * freq_i) with frequencies sorted ascending (ties allowed).
- We maximize Total over the deterministic partition chain; we return the FIRST
  solution attaining that max (matching Prolog’s enumeration behavior).

ARC-style output:
- Answer: size, permutation, board (class IDs), max total
- Reason why: frequency counts and the total formula
- Check: verifies constraints and optimality
"""

from dataclasses import dataclass
from typing import List, Tuple, Dict, Iterable, Optional
import sys

# ----------------------------
# Utilities
# ----------------------------

def transpose(mat: List[List[int]]) -> List[List[int]]:
    if not mat:
        return []
    return [list(col) for col in zip(*mat)]

def flatten(xs):
    out = []
    for row in xs:
        out.extend(row)
    return out

def chunk_take(seq: List[int], k: int):
    return seq[:k], seq[k:]

def from_to(m: int, n: int) -> List[int]:
    return list(range(m, n + 1))

def list_permute(permutation_1_based: List[int], L: List):
    return [L[p - 1] for p in permutation_1_based]

def list_rotate_left(lst: List[int]) -> List[int]:
    if not lst:
        return []
    return lst[1:] + lst[:1]

# --------------------------
# Partition successor
# --------------------------

Partition = List[Tuple[int, int]]  # list of (K, AlphaK)

def next_partition(part: Partition) -> Optional[Partition]:
    if not part:
        return None
    k1, a1 = part[0]

    # [(2,1)|T] -> [(1,2)|T]
    if k1 == 2 and a1 == 1:
        return [(1, 2)] + part[1:]

    # [(2, A>1)|T] -> [(1,2),(2,A-1)|T]
    if k1 == 2 and a1 > 1:
        return [(1, 2), (2, a1 - 1)] + part[1:]

    # [(K>2,1)|T] -> [(1,1),(K-1,1)|T]
    if k1 > 2 and a1 == 1:
        return [(1, 1), (k1 - 1, 1)] + part[1:]

    # [(K>2, A>1)|T] -> [(1,1),(K-1,1),(K,A-1)|T]
    if k1 > 2 and a1 > 1:
        return [(1, 1), (k1 - 1, 1), (k1, a1 - 1)] + part[1:]

    # Heads starting with (1, A1)
    if k1 == 1 and len(part) >= 2:
        a1_ = a1
        k2, a2 = part[1]
        tail = part[2:]

        # [(1,A1),(2,1)|T] -> [(1, A1+2)|T]
        if k2 == 2 and a2 == 1:
            return [(1, a1_ + 2)] + tail

        # [(1,A1),(2,A2>1)|T] -> [(1, A1+2),(2, A2-1)|T]
        if k2 == 2 and a2 > 1:
            return [(1, a1_ + 2), (2, a2 - 1)] + tail

        # [(1,A1),(L>2,1)|T] -> depends on Rest
        if k2 > 2 and a2 == 1:
            newL = k2 - 1
            total = a1_ + k2
            rest_val = total % newL
            ratio = total // newL
            if rest_val > 0:
                return [(rest_val, 1), (newL, ratio)] + tail
            else:
                return [(newL, ratio)] + tail

        # [(1,A1),(L>2,AL>1)|T] -> depends on Rest, keep (L,AL-1)
        if k2 > 2 and a2 > 1:
            newL = k2 - 1
            total = a1_ + k2
            rest_val = total % newL
            ratio = total // newL
            if rest_val > 0:
                return [(rest_val, 1), (newL, ratio), (k2, a2 - 1)] + tail
            else:
                return [(newL, ratio), (k2, a2 - 1)] + tail

    return None

def ad_partitions(n: int) -> Iterable[Partition]:
    """
    Generate the deterministic chain of admissible partitions starting at [(N,1)].
    Guard (like Prolog ad_partition) requires the head length K>1.
    """
    current = [(n, 1)]
    while True:
        if current and current[0][0] > 1:
            yield current
        nxt = next_partition(current)
        if nxt is None:
            break
        current = nxt

# -----------------------
# splitter + rep_perm
# -----------------------

def splitter(n: int, part: Partition) -> List[List[int]]:
    """
    Deterministically partition [1..n] into consecutive chunks:
      For each (K,AlphaK) take AlphaK chunks of size K in order.
    NOTE: No reverse here (critical for matching the Prolog enumeration).
    """
    L = from_to(1, n)
    acc: List[List[int]] = []
    for K, AlphaK in part:
        for _ in range(AlphaK):
            seg, L = chunk_take(L, K)
            acc.append(seg)
    return acc

def rep_perm(n: int, part: Partition) -> List[int]:
    """
    Prolog: rotate each chunk left once, then flatten -> a 1-based permutation.
    """
    chunks = splitter(n, part)
    rotated = [list_rotate_left(seg) for seg in chunks]
    return flatten(rotated)

# --------------------------
# Union-Find for equalities
# --------------------------

class DSU:
    def __init__(self, size: int):
        self.parent = list(range(size))
        self.rank = [0] * size

    def find(self, x: int) -> int:
        while self.parent[x] != x:
            self.parent[x] = self.parent[self.parent[x]]
            x = self.parent[x]
        return x

    def union(self, a: int, b: int):
        ra, rb = self.find(a), self.find(b)
        if ra == rb: return
        if self.rank[ra] < self.rank[rb]:
            self.parent[ra] = rb
        elif self.rank[ra] > self.rank[rb]:
            self.parent[rb] = ra
        else:
            self.parent[rb] = ra
            self.rank[ra] += 1

# -------------------------------------------
# Core: impose constraints, score, and choose
# -------------------------------------------

@dataclass
class SquareSolution:
    permutation: List[int]        # 1-based
    board: List[List[int]]        # class labels (first-occurrence order)
    freqs_sorted: List[int]
    total: int

def build_solution(n: int, perm: List[int]) -> Optional[SquareSolution]:
    """
    Enforce M == transpose(list_permute(perm, M)) on an N×N grid of logic vars.
    Represent each cell as a UF element; merge equalities; ensure distinct rows.
    Then print the matrix with class IDs assigned by first occurrence (row-major).
    Total is computed from frequencies sorted ascending (labels don't affect it).
    """
    size = n * n
    dsu = DSU(size)

    def lid(i: int, j: int) -> int:
        return i * n + j

    # Equalities from M = transpose(list_permute(perm, M)):
    for i in range(n):
        for j in range(n):
            a = lid(i, j)
            b = lid(perm[j] - 1, i)  # perm is 1-based
            dsu.union(a, b)

    reps = [dsu.find(x) for x in range(size)]

    # Row signatures (by class rep id). Distinct rows required.
    row_sigs = [tuple(reps[i*n:(i+1)*n]) for i in range(n)]
    if len(set(row_sigs)) != n:
        return None

    # Frequencies of classes present:
    freq_map: Dict[int, int] = {}
    for r in reps:
        rr = dsu.find(r)
        freq_map[rr] = freq_map.get(rr, 0) + 1
    freqs_sorted = sorted(freq_map.values())
    total = sum((i + 1) * f for i, f in enumerate(freqs_sorted))

    # Label classes by FIRST OCCURRENCE in row-major order (1..K)
    rep_to_label: Dict[int, int] = {}
    next_label = 1
    labeled_grid: List[List[int]] = []
    idx = 0
    for i in range(n):
        row_labels = []
        for j in range(n):
            rr = dsu.find(reps[idx])
            if rr not in rep_to_label:
                rep_to_label[rr] = next_label
                next_label += 1
            row_labels.append(rep_to_label[rr])
            idx += 1
        labeled_grid.append(row_labels)

    return SquareSolution(
        permutation=perm,
        board=labeled_grid,
        freqs_sorted=freqs_sorted,
        total=total
    )

def enigma1225(n: int) -> SquareSolution:
    """
    Mirror the Prolog flow:
      setof(Total, ... square(..., Total, ...), Totals),
      Max = last(Totals),
      then find the FIRST square(..., Max, ...) in the same enumeration order.
    """
    # First pass: gather all totals to find Max
    totals_seen = []
    candidates: List[Tuple[Partition, List[int], int]] = []
    for part in ad_partitions(n):
        perm = rep_perm(n, part)
        sol = build_solution(n, perm)
        if sol is None:
            continue
        totals_seen.append(sol.total)
        candidates.append((part, perm, sol.total))

    if not totals_seen:
        raise RuntimeError("No solutions found.")
    max_total = max(totals_seen)

    # Second pass: return the FIRST solution achieving max_total
    for part, perm, total in candidates:
        if total == max_total:
            sol = build_solution(n, perm)
            if sol is not None:
                return sol

    raise RuntimeError("Max solution unexpectedly missing.")

# -------------------------------
# Pretty printing & ARC-like I/O
# -------------------------------

def print_board(board: List[List[int]]):
    width = max(1, max(len(str(x)) for row in board for x in row))
    for row in board:
        print("  " + " ".join(f"{x:>{width}d}" for x in row))

def reason_text(sol: SquareSolution) -> str:
    lines = []
    lines.append("We constrain M = transpose(list_permute(P, M)) and require distinct rows.")
    lines.append("Cells merge into equivalence classes (same symbol).")
    lines.append("Total computed as: sort frequencies ascending, then sum (rank * freq).")
    lines.append(f"Frequencies (ascending): {sol.freqs_sorted}")
    parts = [f"{i+1}*{f}" for i, f in enumerate(sol.freqs_sorted)]
    lines.append("Total = " + " + ".join(parts) + f" = {sol.total}")
    return "\n".join(lines)

def check_harness(n: int, sol: SquareSolution):
    # 1) Size
    assert len(sol.board) == n and all(len(r) == n for r in sol.board), "Board size mismatch."

    # 2) Constraint check directly on concrete board:
    permuted = list_permute(sol.permutation, sol.board)
    lhs = transpose(permuted)
    assert lhs == sol.board, "M != transpose(list_permute(P, M)) on the concrete board."

    # 3) Distinct rows
    rows = [tuple(r) for r in sol.board]
    assert len(set(rows)) == n, "Rows are not pairwise distinct."

    # 4) Frequencies and total must match
    counts: Dict[int, int] = {}
    for v in flatten(sol.board):
        counts[v] = counts.get(v, 0) + 1
    freqs_sorted = sorted(counts.values())
    assert freqs_sorted == sol.freqs_sorted, "Frequency multiset mismatch."
    total = sum((i + 1) * f for i, f in enumerate(freqs_sorted))
    assert total == sol.total, "Total mismatch."

    # 5) Optimality: recompute max the same way and compare
    max_total = -1
    for part in ad_partitions(n):
        perm = rep_perm(n, part)
        s2 = build_solution(n, perm)
        if s2 is None:
            continue
        if s2.total > max_total:
            max_total = s2.total
    assert sol.total == max_total, "Solution is not maximal."

def main():
    n = 8
    sol = enigma1225(n)

    # ----- ARC output -----
    print("Answer")
    print("------")
    print(f"Size: {n}")
    print(f"Permutation (1-based): {sol.permutation}")
    print(f"Max total: {sol.total}")
    print("Board (class IDs, first-occurrence order):")
    print_board(sol.board)
    print()

    print("Reason why")
    print("----------")
    print(reason_text(sol))
    print()

    print("Check (harness)")
    print("---------------")
    try:
        check_harness(n, sol)
        print("OK: constraints hold and the total is maximal for N =", n)
    except AssertionError as e:
        print("FAILED:", e)
        sys.exit(1)

if __name__ == "__main__":
    main()

