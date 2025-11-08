#!/usr/bin/env python3
r"""
Monkey and Bananas — faithful Python translation of:
  https://raw.githubusercontent.com/eyereasoner/arvol/refs/heads/main/input/monkey-bananas.pl

This mirrors the Prolog program:

  plan(L) :-
      initial_state(I),
      goal_state(G),
      reachable(I, L, G).

  reachable(S, [], S).
  reachable(S1, [M|L], S3) :- legal_move(S1, M, S2), reachable(S2, L, S3).

  initial_state([loc1, loc2, loc3, n, n]).
  goal_state([_, _, _, _, y]).

  legal_move([...], climb_on,  ...).
  legal_move([...], climb_off, ...).
  legal_move([...], grab,      ...).
  legal_move([...], push(X),   ...) :- member(X,[loc1,loc2,loc3]), X \= M.
  legal_move([...], go(X),     ...) :- member(X,[loc1,loc2,loc3]), X \= M.

and the query:
  (true :+ plan(L)) :- between(1,5,I), length(L,I).

Key fidelity points
-------------------
• State layout is exactly [BananasLoc, MonkeyLoc, BoxLoc, OnBox(y/n), HasBananas(y/n).
• Move enumeration order matches clause order: climb_on, climb_off, grab, then push(X) (X in [loc1,loc2,loc3]), then go(X) (same X order), with X \= M.
• Search tries increasing plan lengths 1..5 and returns the first success (DFS per length).
• Answer prints the first plan, the state trace, and the final state.  Reason/Check explain & verify.
"""

from dataclasses import dataclass
from typing import List, Tuple, Optional, Iterable

# Locations in the explicit order used by Prolog's member/2
LOCS = ("loc1", "loc2", "loc3")

# State is [BananasLoc, MonkeyLoc, BoxLoc, OnBox('y'/'n'), HasBananas('y'/'n')]
State = Tuple[str, str, str, str, str]


# ----------------------------
# Problem declaration (1:1)
# ----------------------------

def initial_state() -> State:
    """
    Prolog: initial_state([loc1, loc2, loc3, n, n]).
    [BananasLoc, MonkeyLoc, BoxLoc, OnBox, HasBananas]
    """
    return ("loc1", "loc2", "loc3", "n", "n")


def goal_state_pattern(s: State) -> bool:
    """
    Prolog: goal_state([_, _, _, _, y]).
    Only requires HasBananas == 'y'.
    """
    return s[4] == "y"


# ----------------------------
# Legal moves (Prolog order)
# ----------------------------

@dataclass(frozen=True)
class Move:
    """Atoms rendered to match the Prolog syntax in prints."""
    kind: str        # "climb_on" | "climb_off" | "grab" | "push" | "go"
    arg: Optional[str] = None  # location for push/go

    def __str__(self) -> str:
        return f"{self.kind}({self.arg})" if self.arg is not None else self.kind

    # <-- Fix: allow width/alignment formatting (e.g., f"{mv:<12}")
    def __format__(self, spec: str) -> str:
        return format(str(self), spec)


def legal_moves(state: State) -> Iterable[Tuple[Move, State, str]]:
    r"""
    Yield (move, next_state, reason) in the SAME order as the Prolog clauses appear.

      legal_move([B,M,M,n,H], climb_on, [B,M,M,y,H]).
      legal_move([B,M,M,y,H], climb_off,[B,M,M,n,H]).
      legal_move([B,B,B,y,n], grab,     [B,B,B,y,y]).
      legal_move([B,M,M,n,H], push(X),  [B,X,X,n,H]) :- X in LOCS, X \= M.
      legal_move([B,M,L,n,H], go(X),    [B,X,L,n,H]) :- X in LOCS, X \= M.
    """
    B, M, L, OB, H = state

    # climb_on
    if L == M and OB == "n":
        next_state = (B, M, L, "y", H)
        yield Move("climb_on"), next_state, "Box at monkey's location and not on it ⇒ climb_on allowed."

    # climb_off
    if L == M and OB == "y":
        next_state = (B, M, L, "n", H)
        yield Move("climb_off"), next_state, "Monkey is on the box ⇒ climb_off allowed."

    # grab
    if B == M == L and OB == "y" and H == "n":
        next_state = (B, B, B, "y", "y")
        yield Move("grab"), next_state, "Bananas, monkey, and box co-located and on_box ⇒ grab allowed."

    # push(X): requires L==M, OB=='n', X ≠ M; enumerate X in LOCS order
    if L == M and OB == "n":
        for X in LOCS:
            if X != M:
                next_state = (B, X, X, "n", H)
                yield Move("push", X), next_state, f"Box with monkey (L==M) and not on it; push to {X}."

    # go(X): requires OB=='n', X ≠ M; enumerate X in LOCS order
    if OB == "n":
        for X in LOCS:
            if X != M:
                next_state = (B, X, L, "n", H)
                yield Move("go", X), next_state, f"Not on box; walk to {X}."

# ----------------------------
# Depth-bounded DFS (exact length)
# ----------------------------

def dfs_exact_length(start: State, depth: int) -> Optional[Tuple[List[Move], List[State], List[str]]]:
    """
    Deterministic DFS that enumerates move sequences of EXACTLY 'depth'
    using the same clause order and member/2 location order as the Prolog.
    Returns the FIRST (plan, trace, reasons) that reaches the goal at the end.
    """
    # trace includes start state; states[i] is state BEFORE executing move[i]
    states: List[State] = [start]
    reasons: List[str] = []
    moves: List[Move] = []

    def rec(s: State, k: int) -> bool:
        if k == 0:
            return goal_state_pattern(s)
        for mv, s2, why in legal_moves(s):
            moves.append(mv)
            states.append(s2)
            reasons.append(why)
            if rec(s2, k - 1):
                return True
            # backtrack
            moves.pop()
            states.pop()
            reasons.pop()
        return False

    if rec(start, depth):
        return moves[:], states[:], reasons[:]
    return None

def find_first_plan(max_len: int = 5) -> Tuple[List[Move], List[State], List[str]]:
    """
    Try lengths 1..max_len (inclusive) and return the first successful plan,
    matching: (true :+ plan(L)) :- between(1,5,I), length(L,I).
    """
    s0 = initial_state()
    for d in range(1, max_len + 1):
        res = dfs_exact_length(s0, d)
        if res is not None:
            return res
    raise RuntimeError(f"No plan found with length ≤ {max_len}.")

# ----------------------------
# Pretty printing
# ----------------------------

def fmt_state(s: State) -> str:
    return f"[bananas={s[0]}, monkey={s[1]}, box={s[2]}, on_box={s[3]}, has_bananas={s[4]}]"

def print_trace(moves: List[Move], states: List[State], reasons: List[str]) -> None:
    print(f"Start: {fmt_state(states[0])}")
    for i, mv in enumerate(moves, 1):
        # <-- Fix: now __format__ is supported; width works
        print(f"  {i:>2}. {mv:<12} → {fmt_state(states[i])}")

# ----------------------------
# Check harness
# ----------------------------

def check_harness(moves: List[Move], states: List[State]) -> None:
    """
    Re-verify all preconditions *in the same order* and confirm:
      - every step is a legal_move
      - final state satisfies the goal pattern
      - minimality within lengths 1..len(moves)-1 (no shorter plan works)
    Raises AssertionError on any failure.
    """
    assert len(states) == len(moves) + 1, "Trace length mismatch."

    # 1) Each transition must appear in legal_moves of its source (and match target)
    for i, mv in enumerate(moves):
        s = states[i]
        s_next = states[i + 1]
        found = False
        for legal_mv, legal_next, _ in legal_moves(s):
            if legal_mv == mv and legal_next == s_next:
                found = True
                break
        assert found, f"Illegal move at step {i+1}: {mv} from {fmt_state(s)}."

    # 2) Final state must satisfy goal
    assert goal_state_pattern(states[-1]), "Final state does not satisfy goal_state([_,_,_,_,y])."

    # 3) Minimality: ensure there is no solution with fewer steps (bounded by 1..len(moves)-1)
    s0 = states[0]
    for d in range(1, len(moves)):
        assert dfs_exact_length(s0, d) is None, f"Found a shorter plan of length {d}, so result is not minimal."

# ----------------------------
# Main (ARC-style output)
# ----------------------------

def main():
    plan, trace, reasons = find_first_plan(max_len=5)

    print("Answer")
    print("------")
    print("Plan (first found with length ≤ 5, Prolog order):")
    print("  " + " → ".join(str(m) for m in plan))
    print("Final state:")
    print(f"  {fmt_state(trace[-1])}")
    print()
    print("Trace")
    print("-----")
    print_trace(plan, trace, reasons)
    print()

    print("Reason why")
    print("----------")
    for i, (mv, why) in enumerate(zip(plan, reasons), 1):
        print(f"{i}. {mv}: {why}")
    print()

    print("Check (harness)")
    print("---------------")
    try:
        check_harness(plan, trace)
        print("OK: all preconditions hold, goal reached, and no shorter plan exists (within 1..len(plan)-1).")
    except AssertionError as e:
        print("FAILED:", e)
        raise

if __name__ == "__main__":
    main()

