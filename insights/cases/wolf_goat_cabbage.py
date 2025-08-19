#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Wolf–Goat–Cabbage — ARC (Answer / Reason / Check), self-contained

Problem (classic river crossing)
  A farmer (man) must ferry a wolf, a goat, and a cabbage across a river.
  The boat can carry the man plus at most one passenger.
  Safety constraint on any bank *without* the man:
    - The wolf must not be with the goat (wolf eats goat).
    - The goat must not be with the cabbage (goat eats cabbage).

Goal
  Move everyone from the Left bank to the Right bank without violating safety.

Output
  • Answer: one optimal (shortest) solution, plus a count of all optimal solutions.
  • Reason why: explains the constraints and the BFS minimality guarantee.
  • Check (harness): validates move legality, safety at every step, endpoints,
    and confirms minimality is 7 crossings for the classic rules.
"""

from collections import deque, defaultdict
from typing import Dict, Iterable, List, Optional, Sequence, Tuple

# A state is (M, W, G, C) where each entry is 0 for Left, 1 for Right.
# M = man, W = wolf, G = goat, C = cabbage
State = Tuple[int, int, int, int]

LEFT, RIGHT = 0, 1
NAMES = ("M", "W", "G", "C")


# ─────────────────────────── Helpers & predicates ───────────────────────────

def side_label(b: int) -> str:
    return "L" if b == LEFT else "R"

def pretty_banks(s: State) -> str:
    """Return e.g. 'L:{W,C}  R:{M,G}'."""
    left_items: List[str] = []
    right_items: List[str] = []
    for name, bit in zip(NAMES, s):
        (left_items if bit == LEFT else right_items).append(name)
    def fmt(items: List[str]) -> str:
        return "{" + (",".join(items)) + "}"
    return f"L:{fmt(left_items)}  R:{fmt(right_items)}"

def safe(s: State) -> bool:
    """Safety holds if on any bank without M, W != G and G != C."""
    m, w, g, c = s
    # Bank without man: check pairs there
    # Left bank unsafe?
    if m != LEFT:
        if (w == LEFT and g == LEFT) or (g == LEFT and c == LEFT):
            return False
    # Right bank unsafe?
    if m != RIGHT:
        if (w == RIGHT and g == RIGHT) or (g == RIGHT and c == RIGHT):
            return False
    return True

def move_desc(prev: State, nxt: State) -> str:
    """Describe the move as 'M alone L→R' or 'M+G R→L'."""
    pm, pw, pg, pc = prev
    nm, nw, ng, nc = nxt
    # Who moved with M?
    moved: Optional[str] = None
    for name, p, n in zip(NAMES[1:], prev[1:], nxt[1:]):  # skip 'M'
        if p != n:
            moved = name
            break
    arrow = f"{side_label(pm)}→{side_label(nm)}"
    return f"M+{moved} {arrow}" if moved else f"M {arrow}"

def valid_transition(prev: State, nxt: State) -> bool:
    """Boat rule (M moves, optionally one passenger from M's side) and safety."""
    if prev == nxt:
        return False
    pm, pw, pg, pc = prev
    nm, nw, ng, nc = nxt

    # Man must cross sides
    if pm == nm:
        return False

    # Count how many of W/G/C changed sides
    changed = []
    for name, p, n in zip(("W","G","C"), (pw, pg, pc), (nw, ng, nc)):
        if p != n:
            changed.append(name)
    if len(changed) > 1:
        return False  # at most one passenger
    if len(changed) == 1:
        # That passenger must have started on the man's side and ended on man's new side
        passenger = changed[0]
        start_side = {"W": pw, "G": pg, "C": pc}[passenger]
        end_side   = {"W": nw, "G": ng, "C": nc}[passenger]
        if start_side != pm or end_side != nm:
            return False
    # The two non-passengers must not move
    # (already implied by len(changed) <= 1 and equality checks above)

    # Safety in the new state
    return safe(nxt)


# ──────────────────────────────── Successors ────────────────────────────────

def neighbors(s: State) -> Iterable[Tuple[State, str]]:
    """Generate all legal next states and textual move descriptions."""
    m, w, g, c = s
    cur = (m, w, g, c)
    # Candidate cargos: None (M alone) or one of W/G/C that shares M's side.
    cargos: List[Optional[str]] = [None]
    if w == m: cargos.append("W")
    if g == m: cargos.append("G")
    if c == m: cargos.append("C")

    for cargo in cargos:
        nm = 1 - m
        nw, ng, nc = w, g, c
        if cargo == "W": nw = 1 - w
        if cargo == "G": ng = 1 - g
        if cargo == "C": nc = 1 - c
        nxt = (nm, nw, ng, nc)
        if valid_transition(cur, nxt):
            yield nxt, move_desc(cur, nxt)


# ───────────────────────────── BFS for minimality ───────────────────────────

def bfs_all_shortest(start: State, goal: State) -> Tuple[int, List[List[Tuple[State, str]]]]:
    """
    Multi-parent BFS that returns:
      - minimal number of moves to reach goal
      - ALL shortest paths as sequences [(state_1, move_1), ..., (goal, move_k)]
        (state_0 = start is not included in the list; we print it separately)
    """
    dist: Dict[State, int] = {start: 0}
    parents: Dict[State, List[Tuple[State, str]]] = defaultdict(list)
    q = deque([start])

    while q:
        u = q.popleft()
        if u == goal:
            # We still need to finish the layer to gather all shortest parents to goal
            continue
        for v, mv in neighbors(u):
            if v not in dist:
                dist[v] = dist[u] + 1
                parents[v].append((u, mv))
                q.append(v)
            else:
                # If we found another shortest way into v, record the extra parent
                if dist[v] == dist[u] + 1:
                    parents[v].append((u, mv))

    if goal not in dist:
        return float("inf"), []

    min_len = dist[goal]

    # Reconstruct all shortest paths from parents via DFS
    all_paths: List[List[Tuple[State, str]]] = []

    def backtrack(s: State, acc: List[Tuple[State, str]]):
        if s == start:
            all_paths.append(list(reversed(acc)))
            return
        for p, mv in parents[s]:
            backtrack(p, acc + [(s, mv)])

    backtrack(goal, [])
    return min_len, all_paths


# ────────────────────────────────── ARC: Answer ─────────────────────────────

def print_answer() -> None:
    print("Answer")
    print("======")

    start: State = (LEFT, LEFT, LEFT, LEFT)
    goal:  State = (RIGHT, RIGHT, RIGHT, RIGHT)

    k, paths = bfs_all_shortest(start, goal)

    if not paths:
        print("No solution found.")
        return

    # Show one canonical optimal solution (lexicographically smallest move list)
    # while still reporting the number of optimal solutions.
    def path_key(p: List[Tuple[State, str]]) -> Tuple:
        return tuple(mv for (_s, mv) in p)

    paths_sorted = sorted(paths, key=path_key)
    best = paths_sorted[0]

    print(f"Minimal crossings: {k}")
    print(f"Number of distinct optimal solutions: {len(paths)}\n")

    # Pretty print the chosen solution
    cur = (LEFT, LEFT, LEFT, LEFT)
    print(f"Start: {pretty_banks(cur)}")
    for i, (st, mv) in enumerate(best, 1):
        print(f"  {i:2d}. {mv:<8} | {pretty_banks(st)}")
        cur = st
    print(f"Goal reached: {cur == goal}   | {pretty_banks(cur)}")


# ─────────────────────────────── ARC: Reason why ────────────────────────────

def print_reason() -> None:
    print("\nReason why")
    print("==========")
    print("Constraints:")
    print("  • Boat carries the man and at most one passenger each crossing.")
    print("  • On any bank without the man, the wolf cannot be with the goat,")
    print("    and the goat cannot be with the cabbage.")
    print("Method:")
    print("  • We model states as (M,W,G,C) with 0=Left, 1=Right and use BFS.")
    print("  • BFS explores legal moves level by level, so the first time we")
    print("    discover the goal we have a shortest number of crossings.")
    print("  • We also record multiple parents to enumerate ALL shortest solutions.")


# ────────────────────────────── ARC: Check (harness) ────────────────────────

def path_is_valid(start: State, goal: State, path: Sequence[Tuple[State, str]]) -> bool:
    """Every step must be a valid transition; final state equals goal."""
    cur = start
    for nxt, _mv in path:
        if not valid_transition(cur, nxt):
            return False
        cur = nxt
    if cur != goal:
        return False
    # Safety is already enforced by valid_transition on each step
    return True

def print_check() -> None:
    print("\nCheck (harness)")
    print("===============")
    ok_all = True
    start: State = (LEFT, LEFT, LEFT, LEFT)
    goal:  State = (RIGHT, RIGHT, RIGHT, RIGHT)

    # A) Safety and legality across all optimal paths
    k, paths = bfs_all_shortest(start, goal)
    nonempty = len(paths) > 0
    print(f"Found any optimal paths? {nonempty}")
    ok_all &= nonempty

    all_valid = all(path_is_valid(start, goal, p) for p in paths)
    print(f"All optimal paths obey boat rule and safety at every step? {all_valid}")
    ok_all &= all_valid

    # B) Minimality (for the classic rules, it should be 7 crossings)
    minimal_is_7 = (k == 7)
    print(f"Minimal number of crossings equals 7? {minimal_is_7} (k={k})")
    ok_all &= minimal_is_7

    # C) Boat rule specifics: every move flips M, and at most one of W/G/C flips with him
    def move_shape_ok(prev: State, nxt: State) -> bool:
        if prev[0] == nxt[0]:
            return False
        flips = sum(1 for p, n in zip(prev[1:], nxt[1:]) if p != n)
        return flips <= 1

    shape_ok = True
    cur = start
    for p in paths:
        cur = start
        for nxt, _mv in p:
            if not move_shape_ok(cur, nxt):
                shape_ok = False
                break
            cur = nxt
        if not shape_ok: break
    print(f"Boat carries at most one passenger each crossing? {shape_ok}")
    ok_all &= shape_ok

    # D) Determinism/idempotence: recomputing gives the same minimal k and path count
    k2, paths2 = bfs_all_shortest(start, goal)
    print(f"Deterministic: k and count stable on rerun? {k == k2 and len(paths) == len(paths2)}")
    ok_all &= (k == k2 and len(paths) == len(paths2))

    print(f"\nAll checks passed? {ok_all}")


# ─────────────────────────────────── Main ───────────────────────────────────

if __name__ == "__main__":
    print_answer()
    print_reason()
    print_check()

