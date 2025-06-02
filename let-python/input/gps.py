#!/usr/bin/env python3
"""
Goal-driven Parallel Sequences – full Python implementation
Based on the original Prolog program by Jos De Roo
Paper: https://www.sciencedirect.com/science/article/pii/S1532046421000794
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Any, Dict, Generator, List, Optional, Set, Tuple


# ──────────────────────────────────────────────────────────────
#  Prolog-style helpers
# ──────────────────────────────────────────────────────────────
def _is_var(token: Any) -> bool:
    """Heuristic: a *variable* starts with a capital letter or '_'."""
    return isinstance(token, str) and (token[0].isupper() or token[0] == "_")


def unify(
    pattern: Tuple[Any, ...],
    fact: Tuple[Any, ...],
    env: Optional[Dict[str, Any]] = None
) -> Optional[Dict[str, Any]]:
    """
    Unify *pattern* with *fact* (1-to-1 correspondence).
    Return an extended binding environment or None on failure.
    """
    if len(pattern) != len(fact):
        return None

    env = dict(env or {})          # local copy
    for p, f in zip(pattern, fact):
        if _is_var(p):             # variable
            if p in env and env[p] != f:
                return None        # conflict
            env[p] = f
        elif p != f:               # constant mismatch
            return None
    return env


def instantiate(pattern: Tuple[Any, ...], env: Dict[str, Any]) -> Tuple[Any, ...]:
    """Replace variables in *pattern* using *env* bindings."""
    return tuple(env.get(tok, tok) for tok in pattern)


# ──────────────────────────────────────────────────────────────
#  Domain entities
# ──────────────────────────────────────────────────────────────
@dataclass(frozen=True)
class Transition:
    map_id: str
    from_pattern: Tuple[Any, ...]
    to_pattern: Tuple[Any, ...]
    action: str
    duration: float
    cost: float
    belief: float
    comfort: float


# ──────────────────────────────────────────────────────────────
#  Utility predicates
# ──────────────────────────────────────────────────────────────
def stage_count(maps_history: List[str]) -> int:
    """
    Count *stages* (= maximal contiguous sequences executed on the same map).
    Mirrors the Prolog stagecount/2 predicate.
    """
    if not maps_history:
        return 1
    stages = 1
    last = maps_history[0]
    for m in maps_history[1:]:
        if m != last:
            stages += 1
            last = m
    return stages


def goal_satisfied(state: Set[Tuple[Any, ...]], goal_pattern: Tuple[Any, ...]) -> bool:
    """True when *goal_pattern* unifies with at least one fact in *state*."""
    return any(unify(goal_pattern, fact) is not None for fact in state)


def apply_transition(
    state: Set[Tuple[Any, ...]],
    trans: Transition,
    env: Dict[str, Any]
) -> Set[Tuple[Any, ...]]:
    """
    Successor state after executing *trans* with binding *env*.
    Implements Prolog’s becomes/2 (replacement of one fact by another).
    """
    new_state = set(state)
    new_state.discard(instantiate(trans.from_pattern, env))
    new_state.add   (instantiate(trans.to_pattern,   env))
    return new_state


# ──────────────────────────────────────────────────────────────
#  Depth-first search (generator version)
# ──────────────────────────────────────────────────────────────
def _dfs(
    state: Set[Tuple[Any, ...]],
    transitions: List[Transition],
    goal_pattern: Tuple[Any, ...],
    limits: Tuple[float, float, float, float, int],
    maps_hist: List[str],
    path_hist: List[str],
    dur: float,
    cost: float,
    belief: float,
    comfort: float,
    visited: Set[frozenset],
) -> Generator[Tuple[List[str], float, float, float, float], None, None]:
    """
    Recursive DFS with pruning.  Yields every admissible (path, metrics) tuple.
    """
    state_key = frozenset(state)
    if state_key in visited:
        return
    visited.add(state_key)

    if goal_satisfied(state, goal_pattern):
        yield path_hist, dur, cost, belief, comfort

    max_dur, max_cost, min_belief, min_comfort, max_stages = limits

    for tr in transitions:
        # stage-limit
        if stage_count(maps_hist + [tr.map_id]) > max_stages:
            continue

        # try to fire the transition from any matching fact
        for fact in state:
            env = unify(tr.from_pattern, fact)
            if env is None:
                continue

            # metric updates & pruning
            dur2 = dur + tr.duration
            if dur2 > max_dur:
                continue
            cost2 = cost + tr.cost
            if cost2 > max_cost:
                continue
            belief2 = belief * tr.belief
            if belief2 < min_belief:
                continue
            comfort2 = comfort * tr.comfort
            if comfort2 < min_comfort:
                continue

            next_state = apply_transition(state, tr, env)

            yield from _dfs(
                next_state,
                transitions,
                goal_pattern,
                limits,
                maps_hist + [tr.map_id],
                path_hist + [tr.action],
                dur2,
                cost2,
                belief2,
                comfort2,
                visited.copy()        # local back-tracking
            )


def all_paths(
    state: Set[Tuple[Any, ...]],
    transitions: List[Transition],
    goal_pattern: Tuple[Any, ...],
    limits: Tuple[float, float, float, float, int],
) -> Generator[Tuple[List[str], float, float, float, float], None, None]:
    """
    Public entry: yield *every* admissible plan (path, duration, cost, belief, comfort).
    """
    yield from _dfs(
        state,
        transitions,
        goal_pattern,
        limits,
        maps_hist=[],
        path_hist=[],
        dur=0.0,
        cost=0.0,
        belief=1.0,
        comfort=1.0,
        visited=set(),
    )


# ──────────────────────────────────────────────────────────────
#  Example knowledge base (partial Belgium map)
# ──────────────────────────────────────────────────────────────
transitions: List[Transition] = [
    Transition(
        "map_be",
        ("location", "S", "gent"),
        ("location", "S", "brugge"),
        "drive_gent_brugge",
        1500.0,
        0.006,
        0.96,
        0.99,
    ),
    Transition(
        "map_be",
        ("location", "S", "gent"),
        ("location", "S", "kortrijk"),
        "drive_gent_kortrijk",
        1600.0,
        0.007,
        0.96,
        0.99,
    ),
    Transition(
        "map_be",
        ("location", "S", "kortrijk"),
        ("location", "S", "brugge"),
        "drive_kortrijk_brugge",
        1600.0,
        0.007,
        0.96,
        0.99,
    ),
    Transition(
        "map_be",
        ("location", "S", "brugge"),
        ("location", "S", "oostende"),
        "drive_brugge_oostende",
        900.0,
        0.004,
        0.98,
        1.00,
    ),
]

initial_state: Set[Tuple[Any, ...]] = {("location", "i1", "gent")}
goal_pattern: Tuple[Any, ...] = ("location", "_", "oostende")  # Prolog-style variable "_"
limits: Tuple[float, float, float, float, int] = (5000.0, 5.0, 0.2, 0.4, 1)


# ──────────────────────────────────────────────────────────────
#  Demo driver (mirrors the original Prolog query)
# ──────────────────────────────────────────────────────────────
if __name__ == "__main__":
    for i, (path, dur, cost, bel, comf) in enumerate(
        all_paths(initial_state, transitions, goal_pattern, limits), start=1
    ):
        print(f"Solution {i}")
        print("  Path     :", " → ".join(path))
        print("  Duration :", dur)
        print("  Cost     :", cost)
        print("  Belief   :", bel)
        print("  Comfort  :", comf)
        print()

