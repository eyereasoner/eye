#!/usr/bin/env python3
"""
P3-STYLE PROGRAM — "Mathematics: the WHAT and the WHY" (THOROUGH VERSION)

FOR A WIDE AUDIENCE
--------------------
What is mathematics? A language of **structures** (like numbers, groups, spaces)
plus **proof** — the rulebook for trustworthy reasoning.
Why does mathematics matter? Because a small set of axioms and definitions can
**explain** and **compress** a huge variety of patterns. Proofs let us carry
truth across contexts (the **transfer principle**) and find **invariants** —
properties that remain stable as things change.

This toy program mirrors that story using the P3 pattern:
  • **Answer** — a path that selects axioms (from a "library") and shows which
    mathematical concepts emerge along the way.
  • **Reason** — a plain-English explanation of why that path was chosen.
  • **Check** — independent verifications (monotonicity, minimality, acyclicity,
    permutation-invariance) that the result is internally consistent.

THOROUGH variant: counts order‑distinct paths, keeps expanding after success, and
uses all candidate axioms. The point is transparency, not speed.

Run:
    python3 math_what_why.py

Customize:
- Edit the CONFIG section to adjust candidate axioms, the dependency ladder, and
  the target observations (competencies) you want the final state to satisfy.
- Output is JSON with fields {"answer", "reason", "check"}.
"""
from __future__ import annotations
from dataclasses import dataclass
from typing import List, Set, Dict, Tuple, Optional
import json
import random

# =====================
# CONFIG (Axioms & Goals)
# =====================

# Candidate AXIOMS ("library")
CANDIDATE_AXIOMS: List[str] = [
    "classical-logic",        # proof rules (modus ponens, excluded middle)
    "set-existence",          # there are sets/collections
    "function-formation",     # functions between sets
    "natural-induction",      # induction principle for N
    "algebra-operations",     # +, *, distributivity scaffolding
    "order-completeness",     # completeness needed for R
    "topology-axioms",        # open sets, unions/intersections
    "symmetry-axioms",        # group-like symmetry principles
]

# Emergent CONCEPTS (features) with AND-dependencies on axioms and/or other concepts
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
    "invariants": {"proofs", "groups", "topology"},  # defined after groups below

    # Algebraic structures driven by symmetry axioms
    "groups": {"sets", "symmetry-axioms"},

    # Explanatory goals (the "why")
    "transfer-principle": {"category-theory", "proofs"},
    "explanatory-power": {"invariants", "transfer-principle"},
}

# What do we want the final state to demonstrate? (the "what" & "why")
OBSERVATIONS: Set[str] = {
    # the WHAT — core competencies
    "arithmetic", "topology", "geometry", "calculus", "category-theory",
    # the WHY — why math matters societally and scientifically
    "invariants", "transfer-principle", "explanatory-power",
}

MAX_DEPTH: int = 8       # allow full adoption of all 8 axioms (needed for the observations)
CAP_PATHS: int = 70000   # high enough to traverse to depth 8 in thorough mode

# =====================
# Engine (Program)
# =====================

@dataclass(frozen=True)
class Step:
    action: str
    added_axiom: Optional[str] = None
    enabled_concepts: Tuple[str, ...] = ()

@dataclass
class PathResult:
    path: List[Step]
    final_axioms: Set[str]
    final_concepts: Set[str]

class MathWhatWhy:
    def __init__(self,
                 candidate_axioms: List[str],
                 prereq: Dict[str, Set[str]],
                 observations: Set[str]):
        self.candidate_axioms = list(candidate_axioms)
        self.prereq = {k: set(v) for k, v in prereq.items()}
        self.observations = set(observations)
        self._infer_cache: Dict[frozenset, frozenset] = {}

    # ---------- Logic (Reason)
    def infer_concepts(self, axioms: Set[str]) -> Set[str]:
        """Least fixed point of concept emergence given axioms and dependencies."""
        key = frozenset(axioms)
        if key in self._infer_cache:
            return set(self._infer_cache[key])
        concepts: Set[str] = set()
        changed = True
        while changed:
            changed = False
            for c, reqs in self.prereq.items():
                if c not in concepts and reqs.issubset(axioms | concepts):
                    concepts.add(c)
                    changed = True
        fs = frozenset(concepts)
        self._infer_cache[key] = fs
        return set(fs)

    def _expand(self, axioms: Set[str]) -> List[Tuple[str, Set[str]]]:
        nxt = []
        for ax in self.candidate_axioms:  # fixed order for determinism
            if ax not in axioms:
                nxt.append((ax, set([*axioms, ax])))
        return nxt

    # ---------- Search (thorough, order‑distinct, expand after success)
    def enumerate_paths(self, max_depth: int, cap_paths: int) -> Tuple[List[PathResult], int]:
        initial = (frozenset(), tuple())
        queue: List[Tuple[frozenset, Tuple[Step, ...]]] = [initial]
        all_paths: List[PathResult] = []
        visited_count = 0

        while queue:
            axioms_fs, steps = queue.pop(0)
            axioms = set(axioms_fs)
            concepts = self.infer_concepts(axioms)
            visited_count += 1

            all_paths.append(PathResult(list(steps), set(axioms), set(concepts)))

            if len(steps) >= max_depth:
                continue

            for next_ax, new_axioms in self._expand(axioms):
                new_concepts = self.infer_concepts(new_axioms)
                enabled = tuple(sorted(new_concepts - concepts))
                new_step = Step(action=f"adopt {next_ax}", added_axiom=next_ax, enabled_concepts=enabled)
                new_state = frozenset(new_axioms)
                queue.append((new_state, tuple([*steps, new_step])))

            if len(all_paths) >= cap_paths:
                break

        return all_paths, visited_count

    # ---------- Top‑down selection (filter by Goal)
    def filter_consistent(self, paths: List[PathResult]) -> List[PathResult]:
        return [p for p in paths if self.observations.issubset(p.final_concepts)]

    # ---------- Choice policy (deterministic)
    def choose_path(self, candidates: List[PathResult]) -> Optional[PathResult]:
        if not candidates:
            return None
        return sorted(candidates, key=lambda p: (len(p.path), tuple(s.action for s in p.path)))[0]

    # ---------- Explanation (Reason)
    def explain(self, chosen: PathResult, total_nodes: int, consistent_count: int) -> str:
        lines: List[str] = []
        lines.append("Goal: demonstrate the WHAT (core concepts) and the WHY (explanatory virtues) of mathematics.")
        lines.append("Target observations: " + ", ".join(sorted(self.observations)) + ".")
        lines.append(f"Enumerated {total_nodes} order‑distinct axiom paths (depth ≤ {MAX_DEPTH}).")
        lines.append(f"Top‑down selection filtered these to {consistent_count} consistent endpoints.")
        lines.append("We chose the shortest path achieving the goal; ties broken lexicographically.")
        lines.append("Selected steps:")
        for i, step in enumerate(chosen.path, start=1):
            enabled = ", ".join(step.enabled_concepts) if step.enabled_concepts else "—"
            lines.append(f"  {i}. {step.action} → newly enabled concepts: {enabled}")
        lines.append("Final axioms: " + ", ".join(sorted(chosen.final_axioms)) + ".")
        lines.append("Final concepts: " + ", ".join(sorted(chosen.final_concepts)) + ".")
        lines.append("")
        lines.append("Interpretation:")
        lines.append("• The WHAT: numbers, structures (groups/fields/spaces), and proof as the engine.")
        lines.append("• The WHY: with completeness and symmetry we get reals, analysis, invariants; with\n  functions and proof discipline we can transfer results across domains (category view).")
        return "\n".join(lines)

    # ---------- Independent verification (Check)
    def independent_check(self, chosen: PathResult) -> Dict[str, object]:
        recomputed = self.infer_concepts(set(chosen.final_axioms))
        c1 = recomputed == chosen.final_concepts
        c2 = self.observations.issubset(recomputed)

        # (A) Monotonicity along the axiom path
        ax_prog: Set[str] = set()
        prev_concepts: Set[str] = set()
        monotonic = True
        enabled_records_consistent = True
        for step in chosen.path:
            if not step.added_axiom or step.added_axiom in ax_prog:
                monotonic = False
                break
            ax_prog.add(step.added_axiom)
            now = self.infer_concepts(ax_prog)
            if not prev_concepts.issubset(now):
                monotonic = False
                break
            delta = tuple(sorted(now - prev_concepts))
            if tuple(step.enabled_concepts) != delta:
                enabled_records_consistent = False
            prev_concepts = now

        # (B) Axiom-set minimality for reaching the observations
        dispensable: List[str] = []
        for ax in chosen.final_axioms:
            alt = set(chosen.final_axioms)
            alt.remove(ax)
            if self.observations.issubset(self.infer_concepts(alt)):
                dispensable.append(ax)
        minimal_axioms = (len(dispensable) == 0)

        # (C) No shorter path exists
        shorter_example: List[str] = []
        if len(chosen.path) > 0:
            all_shorter, _ = self.enumerate_paths(max_depth=len(chosen.path)-1, cap_paths=CAP_PATHS)
            consistent_shorter = self.filter_consistent(all_shorter)
            if consistent_shorter:
                shorter_example = [s.action for s in self.choose_path(consistent_shorter).path]
        minimal_steps = (len(shorter_example) == 0)

        # (D) Dependency graph among concepts is acyclic
        def has_cycle() -> bool:
            graph: Dict[str, Set[str]] = {c: set() for c in self.prereq}
            for c, reqs in self.prereq.items():
                for r in reqs:
                    if r in self.prereq:
                        graph[c].add(r)
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

        # (E) Invariance under permutations of candidate axioms
        base_axioms = set(chosen.final_axioms)
        base_len = len(chosen.path)
        mismatches: List[Dict[str, object]] = []
        trials = 1
        for i in range(trials):
            perm = list(self.candidate_axioms)
            random.shuffle(perm)
            if perm == self.candidate_axioms:
                random.shuffle(perm)
            engine2 = MathWhatWhy(perm, self.prereq, self.observations)
            paths2, _ = engine2.enumerate_paths(MAX_DEPTH, CAP_PATHS)
            consistent2 = engine2.filter_consistent(paths2)
            chosen2 = engine2.choose_path(consistent2)
            if not chosen2:
                mismatches.append({"trial": i+1, "issue": "no_path_under_permutation"})
            else:
                eq_axioms = set(chosen2.final_axioms) == base_axioms
                eq_len = len(chosen2.path) == base_len
                if not (eq_axioms and eq_len):
                    mismatches.append({"trial": i+1,
                                       "final_axioms_equal": eq_axioms,
                                       "path_length_equal": eq_len})
        permutation_invariant = (len(mismatches) == 0)

        passed = bool(c1 and c2 and monotonic and minimal_axioms and minimal_steps and acyclic and permutation_invariant and enabled_records_consistent)
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

    # ---------- Orchestration
    def run(self, max_depth: int, cap_paths: int) -> Dict[str, object]:
        all_paths, total = self.enumerate_paths(max_depth=max_depth, cap_paths=cap_paths)
        consistent = self.filter_consistent(all_paths)
        chosen = self.choose_path(consistent)

        if not chosen:
            answer = {"selected_path": None, "final_state": None}
            reason = "No consistent path found under current depth and logic."
            check = {"passed": False}
        else:
            answer = {
                "selected_path": [step.action for step in chosen.path],
                "final_state": {
                    "axioms": sorted(chosen.final_axioms),
                    "concepts": sorted(chosen.final_concepts),
                },
            }
            reason = self.explain(chosen, total_nodes=total, consistent_count=len(consistent))
            check = self.independent_check(chosen)

        return {"answer": answer, "reason": reason, "check": check}


def main():
    engine = MathWhatWhy(CANDIDATE_AXIOMS, PREREQUISITES, OBSERVATIONS)
    result = engine.run(MAX_DEPTH, CAP_PATHS)
    print(json.dumps(result, indent=2))


if __name__ == "__main__":
    main()

