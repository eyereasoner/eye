"""Goal‑driven path search with *explanations*.

This module is an expanded Python translation of Jos De Roo’s
EYE/N3 *GPS – Goal‑driven Parallel Sequencing* demo.

New in this version
===================
• **Lazy DFS generator** (`search`) – memory‑friendly on large graphs.
• **Self‑contained constraint pruning** – limits changed in one place.
• **Rich explanations** – every decision is logged so the user can
  trace *exactly* why a path was accepted.

Try it from the command line:

```bash
$ python gps_explain.py
```

and you’ll see a complete, step‑by‑step justification for every
solution path from *Gent* to *Oostende*.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Callable, Iterator, List, Sequence

# ─── Data structures ────────────────────────────────────────────────────────


@dataclass(frozen=True, slots=True)
class Edge:
    """One `<map>` gps:description triple (immutable)."""

    map_id: str
    from_loc: str
    to_loc: str
    action: str
    duration: float
    cost: float
    belief: float
    comfort: float


@dataclass(slots=True)
class Path:
    """A (partial) path plus its running totals and an explanation log."""

    actions: List[str]
    maps: List[str]
    duration: float
    cost: float
    belief: float
    comfort: float
    current_loc: str
    explanations: List[str] = field(default_factory=list)

    # ── Constraint‑aware extension ──────────────────────────────────
    def extended(self, edge: Edge, constraints: dict) -> "Path":
        """Return a *new* Path with *edge* appended **and** a fresh log row."""
        ndur = self.duration + edge.duration
        ncst = self.cost + edge.cost
        nbel = self.belief * edge.belief
        ncom = self.comfort * edge.comfort
        nmaps = self.maps + [edge.map_id]
        nstage = stagecount(nmaps)

        expl = (
            f"From {edge.from_loc} take '{edge.action}' → {edge.to_loc}. "
            f"Updated totals: duration={ndur}, cost={ncst}, belief={nbel:.3f}, "
            f"comfort={ncom:.3f}, stages={nstage}. "
            "✓ Constraints ok." if self._within_raw(
                ndur, ncst, nbel, ncom, nstage, constraints
            )
            else "✗ Would break constraints."
        )

        return Path(
            actions=self.actions + [edge.action],
            maps=nmaps,
            duration=ndur,
            cost=ncst,
            belief=nbel,
            comfort=ncom,
            current_loc=edge.to_loc,
            explanations=self.explanations + [expl],
        )

    # ── Constraint checking helpers ─────────────────────────────────
    @staticmethod
    def _within_raw(
        dur: float,
        cst: float,
        bel: float,
        com: float,
        stg: int,
        c: dict,
    ) -> bool:
        return (
            dur <= c["max_duration"]
            and cst <= c["max_cost"]
            and bel >= c["min_belief"]
            and com >= c["min_comfort"]
            and stg <= c["max_stagecount"]
        )

    def within(self, c: dict) -> bool:  # noqa: D401
        """True if *all* query limits are still satisfied."""
        return self._within_raw(
            self.duration,
            self.cost,
            self.belief,
            self.comfort,
            stagecount(self.maps),
            c,
        )

    # ── Pretty print ────────────────────────────────────────────────
    def __str__(self) -> str:  # noqa: Dunder
        head = (
            "\n".join(
                (
                    f"Path  : {' → '.join(self.actions) or '(start in place)'}",
                    f"Dur   : {self.duration}",
                    f"Cost  : {self.cost}",
                    f"Belief: {self.belief:.3f}",
                    f"Comfort: {self.comfort:.3f}",
                )
            )
            + "\n"
        )
        body = "\n".join(f"  {msg}" for msg in self.explanations)
        return head + body


# ─── Stagecount helper (exactly the 3 gps:stagecount rules) ─────────────────

def stagecount(maps: Sequence[str]) -> int:
    if not maps:
        return 0
    return 1 + sum(prev != nxt for prev, nxt in zip(maps, maps[1:]))


# ─── Depth‑first proof search ───────────────────────────────────────────────

def search(
    *,
    start: str,
    is_goal: Callable[[str], bool],
    graph: Sequence[Edge],
    constraints: dict,
) -> Iterator[Path]:
    """Generate every admissible path *with explanations* (DFS, no cut)."""

    stack: list[tuple[Path, int]] = [
        (Path([], [], 0.0, 0.0, 1.0, 1.0, start, [f"Start at {start}."]), 0)
    ]

    while stack:
        path, edge_idx = stack.pop()

        if is_goal(path.current_loc):
            path.explanations.append(f"Goal reached at {path.current_loc} → solution emitted.")
            yield path
            continue  # keep hunting for alternative proofs

        # Resume where we left off on this node (classic iterative DFS)
        for i in range(edge_idx, len(graph)):
            edge = graph[i]
            if edge.from_loc != path.current_loc:
                continue

            new_path = path.extended(edge, constraints)
            if not new_path.within(constraints):
                # Extension is logged but violates constraints → skip.
                continue

            # Push parent back (to continue with next edge later) then child.
            stack.append((path, i + 1))
            stack.append((new_path, 0))
            break  # depth‑first: explore child immediately


# ─── Demo data (Belgian road map) ───────────────────────────────────────────

EDGES: list[Edge] = [
    Edge("map-BE", "Gent", "Brugge", "drive_gent_brugge", 1500, 0.006, 0.96, 0.99),
    Edge("map-BE", "Gent", "Kortrijk", "drive_gent_kortrijk", 1600, 0.007, 0.96, 0.99),
    Edge("map-BE", "Kortrijk", "Brugge", "drive_kortrijk_brugge", 1600, 0.007, 0.96, 0.99),
    Edge("map-BE", "Brugge", "Oostende", "drive_brugge_oostende", 900, 0.004, 0.98, 1.00),
]

CONSTRAINTS = dict(
    max_duration=5_000.0,
    max_cost=5.0,
    min_belief=0.2,
    min_comfort=0.4,
    max_stagecount=1,
)


# ─── Script entrypoint ─────────────────────────────────────────────────────

if __name__ == "__main__":
    goal = lambda loc: loc == "Oostende"  # noqa: E731

    for i, sol in enumerate(
        search(start="Gent", is_goal=goal, graph=EDGES, constraints=CONSTRAINTS),
        1,
    ):
        print(f"\nSolution {i}\n{'─' * 20}\n{sol}\n")

