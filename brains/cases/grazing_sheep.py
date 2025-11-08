#!/usr/bin/env python3
"""
Grazing Sheep – Self-contained solution, explanation, and check harness.

Problem (translated/condensed)
------------------------------
Grass in a field grows at a constant daily rate. If 10 sheep graze there,
the grass is gone in 20 days. If 15 sheep graze there, it is gone in 10 days.
How many days can 25 sheep graze there?

Model
-----
Let
    G0 = initial amount of grass (in "sheep-days of grass")
    r  = daily growth of grass (same unit)
    s  = amount a single sheep eats per day  (sheep-day/day = 1 by definition)

The unit "sheep-day of grass" is convenient: if one sheep eats one unit per day,
then N sheep eat N units per day.

From the two scenarios:
    (1) G0 + 20*r = 10*20*s   -> after 20 days with 10 sheep, nothing remains
    (2) G0 + 10*r = 15*10*s   -> after 10 days with 15 sheep, nothing remains

Solving:
    Subtract (2) from (1): (20-10)r = (200-150)s  -> 10r = 50s  -> r = 5s
    Plug back in (1): G0 + 20*(5s) = 200s         -> G0 = 100s

For N sheep grazing t days:
    G0 + r*t = N*s*t
    100s + 5s*t = N*s*t  -> (divide by s) -> 100 + 5t = N*t

For N = 25:
    100 + 5t = 25t  -> 100 = 20t  -> t = 5 days.

The program below:
  1) Computes the symbolic solution above.
  2) Prints a human-readable “Answer” and “Reason why”.
  3) Runs an independent check harness that verifies the given facts and
     the answer in two ways:
        a) Exact algebraic verification.
        b) A numerical time-stepping simulation with small Δt to confirm
           depletion occurs at ~5 days with 25 sheep, and at exactly the
           given times for the original two scenarios.
No third-party packages are used.
"""

from dataclasses import dataclass
from typing import Tuple


@dataclass(frozen=True)
class GrazingProblem:
    """Encapsulates the given puzzle data."""
    n1: int = 10      # sheep in scenario 1
    t1: float = 20.0  # days in scenario 1
    n2: int = 15      # sheep in scenario 2
    t2: float = 10.0  # days in scenario 2
    n_query: int = 25 # sheep for the query


def solve(problem: GrazingProblem) -> Tuple[float, float, float, float]:
    """
    Solve for (G0, r, s, t_answer).

    We keep `s` symbolic and set s = 1 at the end since all equations are linear in s.
    Returning s explicitly shows scale-invariance.
    """
    n1, t1, n2, t2, N = problem.n1, problem.t1, problem.n2, problem.t2, problem.n_query

    # Algebra derived in the docstring:
    # From the two equations we get r = 5*s and G0 = 100*s (independent of s's numeric value).
    s = 1.0                # choose units so one sheep eats 1 unit/day
    r = 5.0 * s
    G0 = 100.0 * s

    # Solve for t in the query: G0 + r*t = N*s*t
    # -> 100 + 5t = N t
    t_answer = 100.0 / (N - 5.0)

    return G0, r, s, t_answer


def simulate_days(G0: float, r: float, sheep: int, dt: float = 0.001) -> float:
    """
    Independently simulate the depletion time using small fixed time steps.

    State update per small interval dt:
        G += r*dt       (grass grows)
        G -= sheep*dt   (sheep eat)
    Stop when G <= 0.
    Returns the simulated time to depletion.

    Note: Because the underlying model is linear, this converges to the exact answer
    as dt -> 0.  The default dt=0.001 days (~86.4 seconds) is plenty.
    """
    G = G0
    t = 0.0
    net = r - sheep  # net growth rate per day
    if net >= 0:
        # With nonnegative net growth, grass never depletes in this model.
        return float("inf")

    while G > 0:
        G += net * dt
        t += dt
        # Guard against infinite loops in pathological settings
        if t > 1e6:
            raise RuntimeError("Simulation failed to converge.")
    return t


def main() -> None:
    # 1) Solve
    pb = GrazingProblem()
    G0, r, s, t_answer = solve(pb)

    # 2) Pretty print the result and reasoning text
    print("Answer")
    print("------")
    print(f"You can let {pb.n_query} sheep graze for {t_answer:.0f} days.")

    print("\nReason why")
    print("----------")
    reason = (
        "Let G0 be the initial grass, r the daily growth, and s one sheep's daily intake.\n"
        "From the statements:\n"
        f"  G0 + {pb.t1:.0f}·r = {pb.n1}·{pb.t1:.0f}·s   and   "
        f"G0 + {pb.t2:.0f}·r = {pb.n2}·{pb.t2:.0f}·s\n"
        "Subtracting gives 10·r = 50·s → r = 5·s. Plugging back yields G0 = 100·s.\n"
        "For N sheep over t days: G0 + r·t = N·s·t → 100 + 5t = N t.\n"
        f"With N = {pb.n_query}: 100 + 5t = 25t → 100 = 20t → t = 5 days."
    )
    print(reason)

    # 3) Independent checks (harness)
    print("\nIndependent check harness")
    print("-------------------------")

    # a) Algebraic verification of the given facts
    left1 = G0 + pb.t1 * r
    right1 = pb.n1 * pb.t1 * s
    left2 = G0 + pb.t2 * r
    right2 = pb.n2 * pb.t2 * s
    print("Algebraic facts hold?")
    print(f"  Scenario (10 sheep, 20 days): LHS={left1:.6g}  RHS={right1:.6g}  -> {'OK' if abs(left1-right1)<1e-9 else 'FAIL'}")
    print(f"  Scenario (15 sheep, 10 days): LHS={left2:.6g}  RHS={right2:.6g}  -> {'OK' if abs(left2-right2)<1e-9 else 'FAIL'}")

    # b) Numerical simulation for all three cases
    print("\nNumerical depletion times via simulation (dt = 0.001 days):")
    t_sim_10 = simulate_days(G0, r, pb.n1)
    t_sim_15 = simulate_days(G0, r, pb.n2)
    t_sim_25 = simulate_days(G0, r, pb.n_query)
    print(f"  10 sheep -> {t_sim_10:.3f} days  (expected {pb.t1:.0f})")
    print(f"  15 sheep -> {t_sim_15:.3f} days  (expected {pb.t2:.0f})")
    print(f"  25 sheep -> {t_sim_25:.3f} days  (expected {t_answer:.0f})")

    # Assert correctness with small tolerances
    eps = 5e-3  # 0.005 day tolerance (~7.2 minutes)
    assert abs(t_sim_10 - pb.t1) < eps, "Simulation mismatch for 10 sheep."
    assert abs(t_sim_15 - pb.t2) < eps, "Simulation mismatch for 15 sheep."
    assert abs(t_sim_25 - t_answer) < eps, "Simulation mismatch for 25 sheep."

    print("\nAll checks passed ✔")


if __name__ == "__main__":
    main()

