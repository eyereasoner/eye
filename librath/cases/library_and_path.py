#!/usr/bin/env python3
"""
P3-STYLE PROGRAM — "The Library and the Path" (THOROUGH VERSION)
(with life, mind, language, many‑minds, and culture)

FOR A WIDE AUDIENCE
--------------------
Imagine a gigantic "library" that contains every simple rulebook the universe could
have followed. A "path" is what you get by picking rules from that library one by
one, watching how each new rule lets more complex things appear. This tiny program
plays with that idea. It tries many short paths and keeps only those that match the
world we see **today**. That “filter by what we observe now” is called **top‑down
selection** in the story this program references.

The code follows a P3 contract:
  • **Answer** — the path it picked and the final state it leads to.
  • **Reason** — a plain explanation of why that path was chosen.
  • **Check** — an independent verification that the explanation really works.

This file is the **thorough** variant:
- Treats different **orders** of adding laws as distinct paths.
- **Does not** deduplicate by law set and **keeps expanding even after success**.
- Uses **all candidate laws** when exploring.
(Expect large counts like 28,961 states for 8 laws and depth ≤ 6.)

Run:
    python3 library_and_path.py

Customize:
- Edit the CONFIG section to change candidate laws, feature prerequisites, and the
  target observations you want the program to satisfy.
- The output is a JSON object with fields: {"answer", "reason", "check"}.

FINAL COMMENTS
--------------
• The big numbers in "Enumerated …" reflect that we count **order‑distinct** paths.
• The chosen path is the **shortest** one that satisfies your observations; if many
  tie, we pick the lexicographically smallest (by step actions) to be deterministic.
• To speed up exploration without changing the chosen minimal path, you can lower
  MAX_DEPTH or CAP_PATHS. A separate "fast" variant could deduplicate by law set and
  stop expanding once observations hold — but this file intentionally keeps the
  thorough behavior for transparency and comparability.
"""
from __future__ import annotations
from dataclasses import dataclass
from typing import List, Set, Dict, Tuple, Optional
import json
import random

# =====================
# CONFIG (Data & Goal)
# =====================

CANDIDATE_LAWS: List[str] = [
    "gravity-stable",
    "atoms-stable",
    "chemistry-stable",
    "electroweak-stable",
    "inflationary-dynamics",
    "magnetism-stable",
    # new laws to support language & many-minds abstractions
    "communication-stable",
    "population-dynamics",
]

# Declarative Logic: feature prerequisites (can depend on laws and/or other features)
PREREQUISITES: Dict[str, Set[str]] = {
    # Cosmic structure from gravity
    "stars": {"gravity-stable"},
    "galaxies": {"gravity-stable"},

    # Chemistry stack
    "carbon-chemistry": {"atoms-stable", "chemistry-stable"},

    # Emergence ladder
    "life": {"stars", "carbon-chemistry"},
    "mind": {"life"},
    # language needs minds + some communication-enabling law
    "language": {"mind", "communication-stable"},
    # many-minds is a stand‑in for social multiplicity/coordination capacity
    "many-minds": {"mind", "population-dynamics"},
    # culture requires language and many minds
    "culture": {"language", "many-minds"},

    # Legacy feature kept for continuity; defined via mind
    "observers": {"mind"},
}

# Goal: present-day observations that must hold at the final state
OBSERVATIONS: Set[str] = {
    "galaxies",
    "carbon-chemistry",
    "life",
    "mind",
    "language",
    "many-minds",
    "culture",
}

MAX_DEPTH: int = 6       # cap on path length (steps of law stabilization)
CAP_PATHS: int = 20000   # safety cap to avoid state explosion during enumeration

# =====================
# Engine (Program)
# =====================

@dataclass(frozen=True)
class Step:
    action: str
    added_law: Optional[str] = None
    enabled_features: Tuple[str, ...] = ()

@dataclass
class PathResult:
    path: List[Step]
    final_laws: Set[str]
    final_features: Set[str]

class LibraryAndPath:
    def __init__(self,
                 candidate_laws: List[str],
                 prereq: Dict[str, Set[str]],
                 observations: Set[str]):
        self.candidate_laws = list(candidate_laws)
        self.prereq = {k: set(v) for k, v in prereq.items()}
        self.observations = set(observations)
        # Memoization cache for feature inference (makes checks faster; logic unchanged)
        self._infer_cache: Dict[frozenset, frozenset] = {}

    # ---------- Logic (Reason)
    def infer_features(self, laws: Set[str]) -> Set[str]:
        """Compute the least fixed point of feature emergence given laws and feature deps."""
        key = frozenset(laws)
        cached = self._infer_cache.get(key)
        if cached is not None:
            return set(cached)
        features: Set[str] = set()
        changed = True
        while changed:
            changed = False
            for feat, reqs in self.prereq.items():
                if feat not in features and reqs.issubset(laws | features):
                    features.add(feat)
                    changed = True
        fs = frozenset(features)
        self._infer_cache[key] = fs
        return set(fs)

    def _expand(self, laws: Set[str]) -> List[Tuple[str, Set[str]]]:
        """All one-step stabilizations available from current law set (order matters)."""
        nxt = []
        for law in self.candidate_laws:  # fixed order for determinism
            if law not in laws:
                nxt.append((law, set([*laws, law])))
        return nxt

    # ---------- Search (finding candidate Paths)
    def enumerate_paths(self, max_depth: int, cap_paths: int) -> Tuple[List[PathResult], int]:
        """BFS over stabilization sequences up to max_depth; returns all visited nodes.
        THOROUGH: we do NOT deduplicate by state and we keep expanding after success.
        """
        initial = (frozenset(), tuple())  # (laws_set, steps)
        queue: List[Tuple[frozenset, Tuple[Step, ...]]] = [initial]
        all_paths: List[PathResult] = []
        expanded_count = 0

        while queue:
            laws_fs, steps = queue.pop(0)
            laws = set(laws_fs)
            features = self.infer_features(laws)
            expanded_count += 1

            # record this path endpoint
            all_paths.append(PathResult(path=list(steps), final_laws=set(laws), final_features=features))

            if len(steps) >= max_depth:
                continue

            for next_law, new_laws in self._expand(laws):
                new_features = self.infer_features(new_laws)
                enabled = tuple(sorted(new_features - features))
                new_step = Step(action=f"stabilize {next_law}", added_law=next_law, enabled_features=enabled)
                new_state = frozenset(new_laws)
                queue.append((new_state, tuple([*steps, new_step])))

            if len(all_paths) >= cap_paths:
                break

        return all_paths, expanded_count

    # ---------- Top‑down selection (filter by Goal)
    def filter_consistent(self, paths: List[PathResult]) -> List[PathResult]:
        return [p for p in paths if self.observations.issubset(p.final_features)]

    # ---------- Choice policy (deterministic)
    def choose_path(self, candidates: List[PathResult]) -> Optional[PathResult]:
        if not candidates:
            return None
        # Prefer shortest path; tie-breaker: lexicographic by step actions for determinism
        return sorted(candidates, key=lambda p: (len(p.path), tuple(s.action for s in p.path)))[0]

    # ---------- Explanation (Reason)
    def explain(self, chosen: PathResult, total_states: int, consistent_count: int) -> str:
        lines: List[str] = []
        lines.append("Goal: find a path that yields the observations: " + ", ".join(sorted(self.observations)) + ".")
        lines.append(f"Enumerated {total_states} candidate states/paths (BFS up to depth {MAX_DEPTH}).")
        lines.append(f"Top-down selection filtered these to {consistent_count} consistent endpoints.")
        lines.append("We then chose the shortest path that achieves the goal; ties broken lexicographically.")
        lines.append("Selected steps:")
        for i, step in enumerate(chosen.path, start=1):
            enabled = ", ".join(step.enabled_features) if step.enabled_features else "—"
            lines.append(f"  {i}. {step.action} → newly enabled features: {enabled}")
        lines.append("Final laws: " + ", ".join(sorted(chosen.final_laws)) + ".")
        lines.append("Final features: " + ", ".join(sorted(chosen.final_features)) + ".")
        return "".join(lines)

    # ---------- Independent verification (Check)
    def independent_check(self, chosen: PathResult) -> Dict[str, object]:
        # Recompute features from the final set of laws (order-independent) and compare
        recomputed = self.infer_features(set(chosen.final_laws))
        c1 = recomputed == chosen.final_features
        # Observations hold
        c2 = self.observations.issubset(recomputed)

        # (A) Monotonicity of features and laws along the path
        laws_progress: Set[str] = set()
        prev_features: Set[str] = set()
        monotonic = True
        enabled_records_consistent = True
        for step in chosen.path:
            # Each step must add a new law and never remove features
            if not step.added_law or step.added_law in laws_progress:
                monotonic = False
                break
            laws_progress.add(step.added_law)
            now_features = self.infer_features(laws_progress)
            if not prev_features.issubset(now_features):
                monotonic = False
                break
            # Check that the recorded enabled_features matches recomputed delta
            delta = tuple(sorted(now_features - prev_features))
            if tuple(step.enabled_features) != delta:
                enabled_records_consistent = False
            prev_features = now_features

        # (B) Law-set minimality for achieving observations
        dispensable: List[str] = []
        for law in chosen.final_laws:
            alt_laws = set(chosen.final_laws)
            alt_laws.remove(law)
            if self.observations.issubset(self.infer_features(alt_laws)):
                dispensable.append(law)
        minimal_laws = (len(dispensable) == 0)

        # (C) No shorter path exists (independent search bounded to shorter depths)
        shorter_example: List[str] = []
        if len(chosen.path) > 0:
            all_shorter, _ = self.enumerate_paths(max_depth=len(chosen.path)-1, cap_paths=CAP_PATHS)
            consistent_shorter = self.filter_consistent(all_shorter)
            if consistent_shorter:
                shorter_example = [s.action for s in self.choose_path(consistent_shorter).path]
        minimal_steps = (len(shorter_example) == 0)

        # (D) Acyclic feature dependency graph (no circular prerequisites among features)
        def has_cycle() -> bool:
            graph: Dict[str, Set[str]] = {f: set() for f in self.prereq.keys()}
            for f, reqs in self.prereq.items():
                for r in reqs:
                    if r in self.prereq:  # r is a feature
                        graph[f].add(r)
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

        # (E) Invariance of final state under permutations of candidate law order
        base_final_laws = set(chosen.final_laws)
        base_len = len(chosen.path)
        permutations_checked = 0
        mismatches: List[Dict[str, object]] = []
        trials = 5
        for i in range(trials):
            perm = list(self.candidate_laws)
            random.shuffle(perm)
            if perm == self.candidate_laws:
                random.shuffle(perm)
            engine2 = LibraryAndPath(perm, self.prereq, self.observations)
            paths2, _ = engine2.enumerate_paths(MAX_DEPTH, CAP_PATHS)
            consistent2 = engine2.filter_consistent(paths2)
            chosen2 = engine2.choose_path(consistent2)
            if not chosen2:
                mismatches.append({"trial": i+1, "issue": "no_path_under_permutation"})
            else:
                eq_laws = set(chosen2.final_laws) == base_final_laws
                eq_len = len(chosen2.path) == base_len
                if not (eq_laws and eq_len):
                    mismatches.append({"trial": i+1, "final_laws_equal": eq_laws, "path_length_equal": eq_len})
            permutations_checked += 1
        permutation_invariant = (len(mismatches) == 0)

        passed = bool(
            c1 and c2 and monotonic and acyclic and minimal_laws and minimal_steps and permutation_invariant and enabled_records_consistent
        )

        return {
            "recomputed_features_match": c1,
            "observations_satisfied": c2,
            "monotonic_along_path": monotonic,
            "enabled_records_consistent": enabled_records_consistent,
            "law_minimality": minimal_laws,
            "dispensable_laws": sorted(dispensable),
            "minimal_steps": minimal_steps,
            "shorter_path_example": shorter_example,
            "acyclic_feature_dependencies": acyclic,
            "permutation_invariant": permutation_invariant,
            "permutations_checked": permutations_checked,
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
                    "laws": sorted(chosen.final_laws),
                    "features": sorted(chosen.final_features),
                },
            }
            reason = self.explain(chosen, total_states=total, consistent_count=len(consistent))
            check = self.independent_check(chosen)

        return {"answer": answer, "reason": reason, "check": check}


def main():
    engine = LibraryAndPath(CANDIDATE_LAWS, PREREQUISITES, OBSERVATIONS)
    result = engine.run(MAX_DEPTH, CAP_PATHS)
    print(json.dumps(result, indent=2))


if __name__ == "__main__":
    main()

