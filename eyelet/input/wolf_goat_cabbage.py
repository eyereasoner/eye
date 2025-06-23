#!/usr/bin/env python3
"""
Enumerate every valid solution of the Wolf–Goat–Cabbage puzzle
whose total length is ≤ max_depth (default = 9).  Results are
printed as Prolog facts of the form

    answer(solution([w,w,w,w], [goat, nothing, wolf, …])).

The program stops at depth-9, which gives the same 20 facts
you showed: 2 optimal (7-step) and 18 sub-optimal (9-step)
solutions.
"""
from collections import deque

ITEMS      = ("farmer", "wolf", "goat", "cabbage")
MOVE_NAME  = ("nothing", "wolf", "goat", "cabbage")   # index = passenger
START      = (0, 0, 0, 0)   # all on West bank (0 = w, 1 = e)
GOAL       = (1, 1, 1, 1)   # all on East bank
MAX_DEPTH  = 9              # raise if you want still longer solutions


def safe(state):
    """True if goat is never left alone with wolf or cabbage."""
    f, w, g, c = state
    return not ((w == g and f != w) or (g == c and f != g))


def neighbours(state):
    """Generate (next_state, passenger_index) pairs."""
    f, w, g, c = state
    sides = [f, w, g, c]

    # farmer alone
    nxt = (1-f, w, g, c)
    if safe(nxt):
        yield nxt, 0          # 0 ⇒ 'nothing'

    # farmer with somebody on same bank
    for i in (1, 2, 3):       # wolf, goat, cabbage
        if sides[i] == f:
            ns = list(state)
            ns[0] = 1-f       # farmer crosses
            ns[i] = 1-ns[i]   # passenger crosses
            nxt = tuple(ns)
            if safe(nxt):
                yield nxt, i


def prolog_fact(moves):
    """Return a string: answer(solution([w,w,w,w], [goat, …]))."""
    mv_str = ", ".join(moves)
    return f"answer(solution([w, w, w, w], [{mv_str}]))."


def enumerate_solutions(max_depth=MAX_DEPTH):
    """
    Breadth-first enumeration of *all* solutions whose length ≤ max_depth.
    """
    frontier = deque([(START, [])])
    while frontier:
        state, path = frontier.popleft()
        if len(path) > max_depth:
            continue
        if state == GOAL:
            yield path
        for nxt, passenger in neighbours(state):
            frontier.append((nxt, path + [MOVE_NAME[passenger]]))


if __name__ == "__main__":
    for sol in enumerate_solutions():
        print(prolog_fact(sol))
