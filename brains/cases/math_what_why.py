#!/usr/bin/env python3
"""
Mathematics: the WHAT and the WHY

This program builds the shortest plan (a sequence of axiom adoptions)
that makes a chosen set of mathematical concepts available.

It prints a JSON report with three keys:
- "answer": the selected plan and the final state (axioms, concepts)
- "reason": list of plain-English lines
- "check": self-checks that justify the plan

Run:
    python3 math_what_why.py

Requires Python 3.9+ and no external packages.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Dict, List, Optional, Set, Tuple
from collections import deque
import json
import random

# -----------------------------------------------------------------------------
# Configuration: axioms, concepts (with prerequisites), and target concepts
# -----------------------------------------------------------------------------
CANDIDATE_AXIOMS: List[str] = [
    "classical-logic",
    "set-existence",
    "function-formation",
    "natural-induction",
    "algebra-operations",
    "order-completeness",
    "topology-axioms",
    "symmetry-axioms",
]

# A concept appears when *all* of its prerequisites are present.
# Prerequisites may be axioms or other concepts.
PREREQUISITES: Dict[str, Set[str]] = {
    # Foundations
    "proofs": {"classical-logic"},
    "sets": {"set-existence"},
    "functions": {"sets", "function-formation"},

    # Arithmetic & algebra
    "naturals": {"sets", "natural-induction"},
    "arithmetic": {"naturals"},
    "rings": {"sets", "algebra-operations"},
    "fields": {"rings"},

    # Analysis & geometry
    "reals": {"fields", "order-completeness"},
    "topology": {"sets", "topology-axioms"},
    "analysis": {"reals", "proofs"},
    "geometry": {"reals", "symmetry-axioms"},
    "calculus": {"analysis"},

    # Higher viewpoints
    "category-theory": {"functions", "proofs"},

    # Algebraic structures driven by symmetry axioms
    "groups": {"sets", "symmetry-axioms"},

    # Explanatory goals
    "invariants": {"proofs", "groups", "topology"},
    "transfer-principle": {"category-theory", "proofs"},
    "explanatory-power": {"invariants", "transfer-principle"},
}

# Concepts we want to hold in the final state (the “what” and the “why”)
OBSERVATIONS: Set[str] = {
    "arithmetic",
    "topology",
    "geometry",
    "calculus",
    "category-theory",
    "invariants",
    "transfer-principle",
    "explanatory-power",
}

# Search controls
MAX_DEPTH: int = 8           # number of candidate axioms
CAP_PATHS: int = 200_000     # safety cap for exploration
FAST_THOROUGH: bool = True   # stop exploring deeper once a shortest plan is found
PERMUTATION_TRIALS: int = 0  # >0: re-run under shuffled axiom orders (robustness)


# -----------------------------------------------------------------------------
# Data structures
# -----------------------------------------------------------------------------
@dataclass(frozen=True)
class Step:
    """
    One action in the plan.

    - action: human text like "adopt classical-logic"
    - added_axiom: the axiom name added at this step
    - enabled_concepts: concepts that became available *at this step*
    """
    action: str
    added_axiom: Optional[str] = None
    enabled_concepts: Tuple[str, ...] = ()


@dataclass
class PathResult:
    path: List[Step]
    final_axioms: Set[str]
    final_concepts: Set[str]


# -----------------------------------------------------------------------------
# Engine
# -----------------------------------------------------------------------------
class MathWhatWhy:
    """
    Find a shortest axiom-adoption plan that achieves the goal.

    Main ideas (in plain language):
    - Represent sets of axioms/concepts as small checklists (bitsets).
    - Repeatedly add any concept whose prerequisites are met (fixed point).
    - Explore axiom orders level by level (BFS) to get a minimal number of steps.
    """

    def __init__(self, candidate_axioms: List[str], prereq: Dict[str, Set[str]], observations: Set[str]) -> None:
        # Names and indexes
        self.candidate_axioms = list(candidate_axioms)
        self.observations = set(observations)
        self.concept_names = list(prereq.keys())
        self.ax_index: Dict[str, int] = {a: i for i, a in enumerate(self.candidate_axioms)}
        self.cx_index: Dict[str, int] = {c: i for i, c in enumerate(self.concept_names)}  # <-- fixed line

        # Precompute, per concept, which axioms and which concepts it requires
        self.req_ax_mask: List[int] = [0] * len(self.concept_names)
        self.req_cx_mask: List[int] = [0] * len(self.concept_names)
        for c, reqs in prereq.items():
            ci = self.cx_index[c]
            ax_mask = 0
            cx_mask = 0
            for r in reqs:
                if r in self.ax_index:
                    ax_mask |= 1 << self.ax_index[r]
                elif r in self.cx_index:
                    cx_mask |= 1 << self.cx_index[r]
                else:
                    raise KeyError(f"Unknown prerequisite: {r}")
            self.req_ax_mask[ci] = ax_mask
            self.req_cx_mask[ci] = cx_mask

        # Mask for the target observations (all are concepts here)
        self.obs_mask_cx = 0
        for o in self.observations:
            self.obs_mask_cx |= 1 << self.cx_index[o]

        # Cache: concepts implied by a given axiom set
        self._infer_cache: Dict[int, int] = {}

    # -- Convenience: checklists <-> names
    def ax_mask_to_names(self, ax_mask: int) -> List[str]:
        return [name for i, name in enumerate(self.candidate_axioms) if (ax_mask >> i) & 1]

    def cx_mask_to_names(self, cx_mask: int) -> List[str]:
        return [name for i, name in enumerate(self.concept_names) if (cx_mask >> i) & 1]

    # -- Logic: which concepts appear under a given axiom set?
    def infer_concepts(self, ax_mask: int) -> int:
        if ax_mask in self._infer_cache:
            return self._infer_cache[ax_mask]

        cx_mask = 0
        changed = True
        while changed:
            changed = False
            for ci in range(len(self.concept_names)):
                if (cx_mask >> ci) & 1:   # already present
                    continue
                if (self.req_ax_mask[ci] & ~ax_mask) != 0:
                    continue
                if (self.req_cx_mask[ci] & ~cx_mask) != 0:
                    continue
                cx_mask |= 1 << ci
                changed = True

        self._infer_cache[ax_mask] = cx_mask
        return cx_mask

    # -- Next choices: adopt one new axiom not yet present
    def _expand(self, ax_mask: int) -> List[Tuple[int, int, int]]:
        out: List[Tuple[int, int, int]] = []
        for ai in range(len(self.candidate_axioms)):
            if not ((ax_mask >> ai) & 1):
                new_ax = ax_mask | (1 << ai)
                new_cx = self.infer_concepts(new_ax)
                out.append((ai, new_ax, new_cx))
        return out

    # -- Search: BFS; in FAST_THOROUGH mode, stop exploring once minimal depth is known
    def search(self, max_depth: int, cap_paths: int, fast: bool) -> Tuple[Optional[PathResult], int, int]:
        Node = Tuple[int, Tuple[int, ...]]  # (axiom_mask, tuple of axiom indices)
        q: deque[Node] = deque([(0, tuple())])

        total_nodes = 0
        consistent_count = 0
        best_depth: Optional[int] = None
        best_actions: Optional[Tuple[str, ...]] = None
        best_final: Optional[Tuple[int, int]] = None
        best_path_indices: Optional[Tuple[int, ...]] = None

        while q:
            ax_mask, path = q.popleft()
            cx_mask = self.infer_concepts(ax_mask)
            total_nodes += 1

            # Goal met?
            if (cx_mask & self.obs_mask_cx) == self.obs_mask_cx:
                consistent_count += 1
                actions = tuple(f"adopt {self.candidate_axioms[i]}" for i in path)
                # Choose shortest; if tied, choose lexicographically smallest by action text
                if best_depth is None or len(path) < best_depth or (len(path) == best_depth and (best_actions is None or actions < best_actions)):
                    best_depth = len(path)
                    best_actions = actions
                    best_final = (ax_mask, cx_mask)
                    best_path_indices = path

            if len(path) >= max_depth:
                continue

            if fast and (best_depth is not None) and len(path) >= best_depth:
                continue

            for ai, new_ax, _ in self._expand(ax_mask):
                q.append((new_ax, (*path, ai)))

            if total_nodes >= cap_paths:
                break

        if best_final is None or best_path_indices is None:
            return None, total_nodes, consistent_count

        # Build the chosen path with enabled-concept deltas
        steps: List[Step] = []
        cur_ax = 0
        cur_cx = self.infer_concepts(cur_ax)
        for ai in best_path_indices:
            next_ax = cur_ax | (1 << ai)
            next_cx = self.infer_concepts(next_ax)
            delta_cx = next_cx & ~cur_cx
            enabled = tuple(sorted(self.cx_mask_to_names(delta_cx)))
            ax_name = self.candidate_axioms[ai]
            steps.append(Step(action=f"adopt {ax_name}", added_axiom=ax_name, enabled_concepts=enabled))
            cur_ax, cur_cx = next_ax, next_cx

        chosen = PathResult(
            path=steps,
            final_axioms=set(self.ax_mask_to_names(best_final[0])),
            final_concepts=set(self.cx_mask_to_names(best_final[1])),
        )
        return chosen, total_nodes, consistent_count

    # -- Helper: reveal a shorter plan if one exists below a given depth
    def find_shorter_example(self, limit_depth: int) -> List[str]:
        if limit_depth <= 0:
            return []
        Node = Tuple[int, Tuple[int, ...]]
        q: deque[Node] = deque([(0, tuple())])
        while q:
            ax_mask, path = q.popleft()
            cx_mask = self.infer_concepts(ax_mask)
            if (cx_mask & self.obs_mask_cx) == self.obs_mask_cx:
                return [f"adopt {self.candidate_axioms[i]}" for i in path]
            if len(path) >= limit_depth:
                continue
            for ai, new_ax, _ in self._expand(ax_mask):
                q.append((new_ax, (*path, ai)))
        return []

    # -- Human-readable explanation (returned as a list of lines)
    def explain(self, chosen: PathResult, total_nodes: int, consistent_count: int) -> List[str]:
        lines: List[str] = []
        lines.append("Goal: achieve the WHAT (core concepts) and the WHY (explanations and transfer).")
        lines.append("Target observations: " + ", ".join(sorted(self.observations)) + ".")
        lines.append(f"Explored {total_nodes} order-distinct axiom paths (depth ≤ {MAX_DEPTH}).")
        lines.append(f"From these, {consistent_count} endpoints satisfied the goal.")
        lines.append("Selected steps:")
        for i, step in enumerate(chosen.path, start=1):
            enabled = ", ".join(step.enabled_concepts) if step.enabled_concepts else "—"
            lines.append(f"  {i}. {step.action} → newly enabled concepts: {enabled}")
        lines.append("Final axioms: " + ", ".join(sorted(chosen.final_axioms)) + ".")
        lines.append("Final concepts: " + ", ".join(sorted(chosen.final_concepts)) + ".")
        return lines

    # -- Self-checks for trustworthiness of the result
    def independent_check(self, chosen: PathResult) -> Dict[str, object]:
        # Recompute concepts from the final axiom set
        ax_mask = 0
        for a in chosen.final_axioms:
            ax_mask |= 1 << self.ax_index[a]
        recomputed_cx = self.infer_concepts(ax_mask)
        recomputed_set = set(self.cx_mask_to_names(recomputed_cx))

        c1 = (recomputed_set == chosen.final_concepts)
        c2 = self.observations.issubset(recomputed_set)

        # (A) Monotonicity along the path + enabled deltas match
        ax_prog = 0
        prev_cx = self.infer_concepts(ax_prog)
        monotonic = True
        enabled_records_consistent = True
        for step in chosen.path:
            ai = self.ax_index[step.added_axiom] if step.added_axiom else None
            if ai is None or ((ax_prog >> ai) & 1):
                monotonic = False
                break
            ax_prog |= 1 << ai
            now_cx = self.infer_concepts(ax_prog)
            if (prev_cx & ~now_cx) != 0:
                monotonic = False
                break
            delta = now_cx & ~prev_cx
            delta_names = tuple(sorted(self.cx_mask_to_names(delta)))
            if tuple(step.enabled_concepts) != delta_names:
                enabled_records_consistent = False
            prev_cx = now_cx

        # (B) Axiom-set minimality for reaching the observations
        dispensable: List[str] = []
        final_ax_list = sorted(chosen.final_axioms)
        for a in final_ax_list:
            ai = self.ax_index[a]
            alt_mask = ax_mask & ~(1 << ai)
            if (self.infer_concepts(alt_mask) & self.obs_mask_cx) == self.obs_mask_cx:
                dispensable.append(a)
        minimal_axioms = (len(dispensable) == 0)

        # (C) No shorter path exists
        shorter_example: List[str] = []
        if len(chosen.path) > 0:
            shorter_example = self.find_shorter_example(limit_depth=len(chosen.path) - 1)
        minimal_steps = (len(shorter_example) == 0)

        # (D) Dependency graph among concepts is acyclic
        def has_cycle() -> bool:
            graph: Dict[str, Set[str]] = {c: set() for c in self.concept_names}
            for c in self.concept_names:
                ci = self.cx_index[c]
                for rci in range(len(self.concept_names)):
                    if (self.req_cx_mask[ci] >> rci) & 1:
                        graph[c].add(self.concept_names[rci])
            visited: Set[str] = set()
            stack: Set[str] = set()

            def dfs(u: str) -> bool:
                visited.add(u)
                stack.add(u)
                for v in graph[u]:
                    if v not in visited and dfs(v):
                        return True
                    if v in stack:
                        return True
                stack.remove(u)
                return False

            for node in graph:
                if node not in visited and dfs(node):
                    return True
            return False

        acyclic = not has_cycle()

        # (E) Optional: robustness to axiom ordering
        base_axioms = set(chosen.final_axioms)
        base_len = len(chosen.path)
        mismatches: List[Dict[str, object]] = []
        for i in range(PERMUTATION_TRIALS):
            perm = list(self.candidate_axioms)
            random.shuffle(perm)
            if perm == self.candidate_axioms:
                random.shuffle(perm)
            engine2 = MathWhatWhy(perm, PREREQUISITES, self.observations)
            chosen2, _, _ = engine2.search(MAX_DEPTH, CAP_PATHS, fast=FAST_THOROUGH)
            if not chosen2:
                mismatches.append({"trial": i + 1, "issue": "no_path_under_permutation"})
            else:
                eq_axioms = set(chosen2.final_axioms) == base_axioms
                eq_len = len(chosen2.path) == base_len
                if not (eq_axioms and eq_len):
                    mismatches.append({"trial": i + 1, "final_axioms_equal": eq_axioms, "path_length_equal": eq_len})
        permutation_invariant = (len(mismatches) == 0)

        passed = bool(
            c1 and c2 and monotonic and minimal_axioms and minimal_steps and acyclic and permutation_invariant and enabled_records_consistent
        )
        return {
            "recomputed_concepts_match": c1,
            "observations_satisfied": c2,
            "monotonic_along_path": monotonic,
            "enabled_records_consistent": enabled_records_consistent,
            "axiom_minimality": minimal_axioms,
            "dispensable_axioms": sorted(dispensable),
            "minimal_steps": minimal_steps,
            "shorter_path_example": shorter_example,
            "acyclic_concept_dependencies": acyclic,
            "permutation_invariant": permutation_invariant,
            "permutation_mismatches": mismatches,
            "passed": passed,
        }

    # -- Orchestrate a full run
    def run(self, max_depth: int, cap_paths: int) -> Dict[str, object]:
        chosen, total, consistent = self.search(max_depth=max_depth, cap_paths=cap_paths, fast=FAST_THOROUGH)
        if not chosen:
            answer = {"selected_path": None, "final_state": None}
            reason_lines: List[str] = ["No consistent plan found under current settings."]
            check = {"passed": False}
        else:
            answer = {
                "selected_path": [step.action for step in chosen.path],
                "final_state": {
                    "axioms": sorted(chosen.final_axioms),
                    "concepts": sorted(chosen.final_concepts),
                },
            }
            reason_lines = self.explain(chosen, total_nodes=total, consistent_count=consistent)
            check = self.independent_check(chosen)
        return {"answer": answer, "reason": reason_lines, "check": check}


def main() -> None:
    engine = MathWhatWhy(CANDIDATE_AXIOMS, PREREQUISITES, OBSERVATIONS)
    result = engine.run(MAX_DEPTH, CAP_PATHS)
    print(json.dumps(result, indent=2, ensure_ascii=False))


if __name__ == "__main__":
    main()

