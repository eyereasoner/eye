#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
dining_philosophers.py — Deterministic Dining Philosophers (ARC-ified)
───────────────────────────────────────────────────────────────────────────

What this does
--------------
Runs the Dining Philosophers problem with *deterministic* turn-taking:
only the philosopher whose id equals the shared `turn` may attempt to eat.
This guarantees:
  • no deadlock,
  • no starvation,
  • a fixed, repeatable eating order: 0→1→…→N−1, repeated for each round.

ARC output
----------
• Answer
    – Prints the eating schedule per round, total meals, and timing.

• Reason why
    – Explains how the condition variable schedules turns and why this removes
      deadlocks (even though forks are still real locks).

• Check (harness)
    – Verifies:
        1) order is exactly 0..N−1 for each of M rounds,
        2) each philosopher ate exactly M times,
        3) “eat” sections do not overlap (strict start/end interleaving),
        4) run finishes (no deadlock/starvation).

Notes
-----
We keep thinking/eating sleeps short so the demo runs fast. All prints are made
AFTER threads join to keep output tidy and deterministic.
"""

from __future__ import annotations
import threading
import time
from dataclasses import dataclass
from typing import List, Tuple, Dict

# ─────────────────────────────────────────────────────────────
# Configuration
# ─────────────────────────────────────────────────────────────
N_PHILOSOPHERS = 5
MEALS_PER_PHIL = 3
THINK_TIME     = 0.01   # seconds
EAT_TIME       = 0.01

# ─────────────────────────────────────────────────────────────
# Logging structures (thread-safe append; print later)
# ─────────────────────────────────────────────────────────────
@dataclass
class Log:
    kind: str           # "think" | "eat_start" | "eat_end"
    phil: int
    meal: int
    t: float            # timestamp (monotonic)

class Logger:
    def __init__(self) -> None:
        self._logs: List[Log] = []
        self._lock = threading.Lock()
    def add(self, kind: str, phil: int, meal: int) -> None:
        with self._lock:
            self._logs.append(Log(kind, phil, meal, time.monotonic()))
    @property
    def logs(self) -> List[Log]:
        return self._logs[:]

# ─────────────────────────────────────────────────────────────
# Simulation
# ─────────────────────────────────────────────────────────────
def run_sim(n: int, meals: int, think_s: float, eat_s: float) -> Tuple[List[Log], float]:
    """Run the deterministic dining simulation; return (logs, elapsed_seconds)."""
    forks = [threading.Lock() for _ in range(n)]
    turn_cond = threading.Condition()
    turn = {"i": 0}  # box turn in a dict so closures can mutate

    logger = Logger()
    t0 = time.perf_counter()

    def philosopher(i: int) -> None:
        left  = forks[i]
        right = forks[(i + 1) % n]
        for meal in range(1, meals + 1):
            # deterministic "thinking"
            logger.add("think", i, meal)
            time.sleep(think_s)

            # wait for our turn to eat
            with turn_cond:
                while turn["i"] != i:
                    turn_cond.wait()
                # safe to try both forks (no contention by design)
                with left, right:
                    logger.add("eat_start", i, meal)
                    time.sleep(eat_s)
                    logger.add("eat_end", i, meal)
                # pass the token
                turn["i"] = (turn["i"] + 1) % n
                turn_cond.notify_all()

    threads = [threading.Thread(target=philosopher, args=(i,), name=f"P{i}") for i in range(n)]
    for t in threads: t.start()
    for t in threads: t.join()
    elapsed = time.perf_counter() - t0
    return logger.logs, elapsed

# ─────────────────────────────────────────────────────────────
# ARC — Answer
# ─────────────────────────────────────────────────────────────
def arc_answer(logs: List[Log], n: int, m: int, elapsed: float) -> None:
    print("Answer")
    print("------")
    # Build eating sequence (by start time) and group into rounds
    starts = [L for L in logs if L.kind == "eat_start"]
    starts.sort(key=lambda L: L.t)
    print(f"Philosophers: {n}, meals each: {m}, total meals: {n*m}")
    print(f"Timing: {elapsed:.3f}s  (think={THINK_TIME:.3f}s, eat={EAT_TIME:.3f}s)\n")

    # Show schedule per round
    print("Eating schedule (deterministic order):")
    for r in range(m):
        seg = starts[r*n:(r+1)*n]
        order = " → ".join(str(L.phil) for L in seg)
        print(f"  Round {r+1}: {order}")
    print()

# ─────────────────────────────────────────────────────────────
# ARC — Reason why
# ─────────────────────────────────────────────────────────────
def arc_reason() -> None:
    print("Reason why")
    print("----------")
    print("We serialize eating with a condition-variable *token* (`turn`).")
    print("Only the philosopher whose id equals `turn` proceeds to grab both forks.")
    print("Everyone else waits on the condition, so no two philosophers attempt to")
    print("lock conflicting forks simultaneously. After eating, the philosopher")
    print("increments `turn` (mod N) and signals the condition.")
    print("\nConsequences:")
    print("  • Deadlock impossible (no circular wait; only one contender).")
    print("  • Starvation impossible (token visits philosophers round-robin).")
    print("  • Deterministic output (order 0..N−1 repeats for each round).")
    print()

# ─────────────────────────────────────────────────────────────
# ARC — Check (harness)
# ─────────────────────────────────────────────────────────────
def arc_check(logs: List[Log], n: int, m: int) -> None:
    print("Check (harness)")
    print("---------------")
    # Extract starts/ends in temporal order
    starts = sorted((L for L in logs if L.kind == "eat_start"), key=lambda L: L.t)
    ends   = sorted((L for L in logs if L.kind == "eat_end"),   key=lambda L: L.t)

    # 1) correct counts
    assert len(starts) == len(ends) == n*m, "Unexpected number of start/end events."

    # 2) strict alternation: S0 < E0 < S1 < E1 < ...
    for k in range(n*m):
        assert starts[k].t < ends[k].t, f"Start after end at meal #{k}."
        if k+1 < n*m:
            assert ends[k].t <= starts[k+1].t, "Overlap between consecutive eat sections."

    # 3) deterministic order per round: 0..n-1
    for r in range(m):
        seg = starts[r*n:(r+1)*n]
        expected = list(range(n))
        got = [L.phil for L in seg]
        assert got == expected, f"Round {r+1} order mismatch: got {got}, expected {expected}"

    # 4) each philosopher ate exactly m times
    counts: Dict[int,int] = {i:0 for i in range(n)}
    for L in starts: counts[L.phil] += 1
    for i in range(n):
        assert counts[i] == m, f"Phil {i} ate {counts[i]} times, expected {m}."

    print("OK: no overlap, correct order each round, correct counts, run completed.\n")

# ─────────────────────────────────────────────────────────────
# Main
# ─────────────────────────────────────────────────────────────
if __name__ == "__main__":
    logs, elapsed = run_sim(N_PHILOSOPHERS, MEALS_PER_PHIL, THINK_TIME, EAT_TIME)
    arc_answer(logs, N_PHILOSOPHERS, MEALS_PER_PHIL, elapsed)
    arc_reason()
    arc_check(logs, N_PHILOSOPHERS, MEALS_PER_PHIL)

