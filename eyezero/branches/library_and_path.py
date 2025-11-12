#!/usr/bin/env python3
"""
The Library and the Path

This program imagines a "library" of candidate laws. A "path" is a
sequence of adoptions (one by one). As laws are adopted, features
appear once their prerequisites are satisfied.

Output JSON:
- "answer": chosen path (actions) and final state (laws, features)
- "reason": list of plain-English lines
- "check": self-checks for trust

Run:
    python3 library_and_path.py

Python 3.9+; no external packages.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import List, Set, Dict, Tuple, Optional
from collections import deque
import json
import random

# -----------------------------------------------------------------------------
# Configuration: candidate laws, feature prerequisites, target observations
# -----------------------------------------------------------------------------
CANDIDATE_LAWS: List[str] = [
    "gravity-stable",
    "atoms-stable",
    "chemistry-stable",
    "electroweak-stable",
    "inflationary-dynamics",
    "magnetism-stable",
    "communication-stable",
    "population-dynamics",
]

# A feature appears when *all* of its prerequisites are present.
# Prerequisites may reference laws and/or other features.
PREREQUISITES: Dict[str, Set[str]] = {
    # Cosmic structure from gravity
    "stars": {"gravity-stable"},
    "galaxies": {"gravity-stable"},

    # Chemistry stack
    "carbon-chemistry": {"atoms-stable", "chemistry-stable"},

    # Emergence ladder
    "life": {"stars", "carbon-chemistry"},
    "mind": {"life"},

    # Language needs minds + some communication-enabling law
    "language": {"mind", "communication-stable"},

    # Many-minds as a stand-in for social multiplicity/coordination capacity
    "many-minds": {"mind", "population-dynamics"},

    # Culture requires language and many minds
    "culture": {"language", "many-minds"},

    # Legacy feature for continuity; defined via mind
    "observers": {"mind"},
}

# What we want to be true in the final state (today’s observations)
OBSERVATIONS: Set[str] = {
    "galaxies",
    "carbon-chemistry",
    "life",
    "mind",
    "language",
    "many-minds",
    "culture",
}

# Search controls
MAX_DEPTH: int = len(CANDIDATE_LAWS)  # allow adopting up to all laws
CAP_PATHS: int = 200_000              # safety cap on visited nodes
FAST_THOROUGH: bool = True            # stop once minimal-depth solutions are covered
PERMUTATION_TRIALS: int = 0           # >0: robustness checks under shuffled law order


# -----------------------------------------------------------------------------
# Data structures
# -----------------------------------------------------------------------------
@dataclass(frozen=True)
class Step:
    """
    One action in the plan.

    - action: human text like "stabilize gravity-stable"
    - added_law: the law adopted at this step
    - enabled_features: features that became available *at this step*
    """
    action: str
    added_law: Optional[str] = None
    enabled_features: Tuple[str, ...] = ()


@dataclass
class PathResult:
    path: List[Step]
    final_laws: Set[str]
    final_features: Set[str]


# -----------------------------------------------------------------------------
# Engine
# -----------------------------------------------------------------------------
class LibraryAndPath:
    """
    Build and evaluate candidate paths of law adoptions.

    Ideas in simple terms:
    - Treat sets of laws/features as growing checklists (bitsets).
    - A feature appears as soon as all its prerequisites are present.
    - Explore adoption orders breadth-first (shortest paths first).
    """

    def __init__(self, candidate_laws: List[str], prereq: Dict[str, Set[str]], observations: Set[str]):
        self.candidate_laws = list(candidate_laws)
        self.observations = set(observations)
        self.feature_names = list(prereq.keys())

        # Index lookups
        self.law_index: Dict[str, int] = {a: i for i, a in enumerate(self.candidate_laws)}
        self.feat_index: Dict[str, int] = {f: i for i, f in enumerate(self.feature_names)}

        # For each feature, precompute masks of required laws and required features
        self.req_law_mask: List[int] = [0] * len(self.feature_names)
        self.req_feat_mask: List[int] = [0] * len(self.feature_names)
        for f, reqs in prereq.items():
            fi = self.feat_index[f]
            lm = 0
            fm = 0
            for r in reqs:
                if r in self.law_index:
                    lm |= 1 << self.law_index[r]
                elif r in self.feat_index:
                    fm |= 1 << self.feat_index[r]
                else:
                    raise KeyError(f"Unknown prerequisite: {r}")
            self.req_law_mask[fi] = lm
            self.req_feat_mask[fi] = fm

        # Observation mask (targets are features)
        self.obs_mask_feat = 0
        for o in self.observations:
            self.obs_mask_feat |= 1 << self.feat_index[o]

        # Cache for feature inference under a given law set
        self._infer_cache: Dict[int, int] = {}  # law_mask -> feat_mask

    # ----- Convenience: masks <-> names
    def law_mask_to_names(self, mask: int) -> List[str]:
        return [name for i, name in enumerate(self.candidate_laws) if (mask >> i) & 1]

    def feat_mask_to_names(self, mask: int) -> List[str]:
        return [name for i, name in enumerate(self.feature_names) if (mask >> i) & 1]

    # ----- Logic: which features appear under a given set of laws?
    def infer_features(self, law_mask: int) -> int:
        cached = self._infer_cache.get(law_mask)
        if cached is not None:
            return cached

        feat_mask = 0
        changed = True
        while changed:
            changed = False
            for fi in range(len(self.feature_names)):
                if (feat_mask >> fi) & 1:
                    continue
                if (self.req_law_mask[fi] & ~law_mask) != 0:
                    continue
                if (self.req_feat_mask[fi] & ~feat_mask) != 0:
                    continue
                feat_mask |= 1 << fi
                changed = True

        self._infer_cache[law_mask] = feat_mask
        return feat_mask

    # ----- Next choices: adopt one more law not yet present
    def _expand(self, law_mask: int) -> List[Tuple[int, int, int]]:
        out: List[Tuple[int, int, int]] = []
        for li in range(len(self.candidate_laws)):
            if not ((law_mask >> li) & 1):
                new_law_mask = law_mask | (1 << li)
                new_feat_mask = self.infer_features(new_law_mask)
                out.append((li, new_law_mask, new_feat_mask))
        return out

    # ----- Streaming BFS with optional early stop at minimal depth
    def search(self, max_depth: int, cap_paths: int, fast: bool) -> Tuple[Optional[PathResult], int, int]:
        Node = Tuple[int, Tuple[int, ...]]  # (law_mask, tuple of law indices)
        q: deque[Node] = deque([(0, tuple())])

        total_nodes = 0
        consistent_count = 0
        best_depth: Optional[int] = None
        best_actions: Optional[Tuple[str, ...]] = None
        best_final: Optional[Tuple[int, int]] = None
        best_path: Optional[Tuple[int, ...]] = None

        while q:
            law_mask, path = q.popleft()
            feat_mask = self.infer_features(law_mask)
            total_nodes += 1

            # Goal met?
            if (feat_mask & self.obs_mask_feat) == self.obs_mask_feat:
                consistent_count += 1
                actions = tuple(f"stabilize {self.candidate_laws[i]}" for i in path)
                # Choose shortest; if tied, lexicographically by action text
                if best_depth is None or len(path) < best_depth or (len(path) == best_depth and (best_actions is None or actions < best_actions)):
                    best_depth = len(path)
                    best_actions = actions
                    best_final = (law_mask, feat_mask)
                    best_path = path

            if len(path) >= max_depth:
                continue

            # In FAST mode, skip exploring deeper once minimal depth is known
            if fast and (best_depth is not None) and len(path) >= best_depth:
                continue

            for li, new_law_mask, _ in self._expand(law_mask):
                q.append((new_law_mask, (*path, li)))

            if total_nodes >= cap_paths:
                break

        if best_final is None or best_path is None:
            return None, total_nodes, consistent_count

        # Rebuild the chosen path with enabled-feature deltas
        steps: List[Step] = []
        cur_law = 0
        cur_feat = self.infer_features(cur_law)
        for li in best_path:
            next_law = cur_law | (1 << li)
            next_feat = self.infer_features(next_law)
            delta = next_feat & ~cur_feat
            enabled = tuple(sorted(self.feat_mask_to_names(delta)))
            lname = self.candidate_laws[li]
            steps.append(Step(action=f"stabilize {lname}", added_law=lname, enabled_features=enabled))
            cur_law, cur_feat = next_law, next_feat

        chosen = PathResult(
            path=steps,
            final_laws=set(self.law_mask_to_names(best_final[0])),
            final_features=set(self.feat_mask_to_names(best_final[1])),
        )
        return chosen, total_nodes, consistent_count

    # ----- Helper: reveal a shorter plan if one exists below a given depth
    def find_shorter_example(self, limit_depth: int) -> List[str]:
        if limit_depth <= 0:
            return []
        Node = Tuple[int, Tuple[int, ...]]
        q: deque[Node] = deque([(0, tuple())])
        while q:
            law_mask, path = q.popleft()
            feat_mask = self.infer_features(law_mask)
            if (feat_mask & self.obs_mask_feat) == self.obs_mask_feat:
                return [f"stabilize {self.candidate_laws[i]}" for i in path]
            if len(path) >= limit_depth:
                continue
            for li, new_law_mask, _ in self._expand(law_mask):
                q.append((new_law_mask, (*path, li)))
        return []

    # ----- Explanation (returned as a list of human-readable lines)
    def explain(self, chosen: PathResult, total_nodes: int, consistent_count: int) -> List[str]:
        lines: List[str] = []
        lines.append("Goal: find a path that yields the observations.")
        lines.append("Target observations: " + ", ".join(sorted(self.observations)) + ".")
        lines.append(f"Explored {total_nodes} order-distinct law paths (depth ≤ {MAX_DEPTH}).")
        lines.append(f"From these, {consistent_count} endpoints satisfied the goal.")
        lines.append("Selected steps:")
        for i, step in enumerate(chosen.path, start=1):
            enabled = ", ".join(step.enabled_features) if step.enabled_features else "—"
            lines.append(f"  {i}. {step.action} → newly enabled features: {enabled}")
        lines.append("Final laws: " + ", ".join(sorted(chosen.final_laws)) + ".")
        lines.append("Final features: " + ", ".join(sorted(chosen.final_features)) + ".")
        return lines

    # ----- Independent verification
    def independent_check(self, chosen: PathResult) -> Dict[str, object]:
        # Recompute features from the final *set* of laws (order-independent)
        law_mask = 0
        for a in chosen.final_laws:
            law_mask |= 1 << self.law_index[a]
        recomputed_feat = self.infer_features(law_mask)
        recomputed_set = set(self.feat_mask_to_names(recomputed_feat))
        c1 = (recomputed_set == chosen.final_features)

        # Observations hold
        c2 = self.observations.issubset(recomputed_set)

        # (A) Monotonicity of laws/features along the chosen path + enabled deltas match
        laws_prog = 0
        prev_feat = self.infer_features(laws_prog)
        monotonic = True
        enabled_records_consistent = True
        for step in chosen.path:
            li = self.law_index[step.added_law] if step.added_law else None
            if li is None or ((laws_prog >> li) & 1):
                monotonic = False
                break
            laws_prog |= 1 << li
            now_feat = self.infer_features(laws_prog)
            if (prev_feat & ~now_feat) != 0:
                monotonic = False
                break
            delta = now_feat & ~prev_feat
            delta_names = tuple(sorted(self.feat_mask_to_names(delta)))
            if tuple(step.enabled_features) != delta_names:
                enabled_records_consistent = False
            prev_feat = now_feat

        # (B) Minimality of final law set for the observations
        dispensable: List[str] = []
        final_laws_list = sorted(chosen.final_laws)
        for a in final_laws_list:
            li = self.law_index[a]
            alt_mask = law_mask & ~(1 << li)
            if (self.infer_features(alt_mask) & self.obs_mask_feat) == self.obs_mask_feat:
                dispensable.append(a)
        minimal_laws = (len(dispensable) == 0)

        # (C) No strictly shorter path exists
        shorter_example: List[str] = []
        if len(chosen.path) > 0:
            shorter_example = self.find_shorter_example(limit_depth=len(chosen.path) - 1)
        minimal_steps = (len(shorter_example) == 0)

        # (D) Acyclic feature dependency graph (no circular prerequisites among features)
        def has_cycle() -> bool:
            graph: Dict[str, Set[str]] = {f: set() for f in self.feature_names}
            for f in self.feature_names:
                fi = self.feat_index[f]
                for rfi in range(len(self.feature_names)):
                    if (self.req_feat_mask[fi] >> rfi) & 1:
                        graph[f].add(self.feature_names[rfi])
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

        # (E) Robustness under permutations of candidate law order (optional)
        base_laws = set(chosen.final_laws)
        base_len = len(chosen.path)
        mismatches: List[Dict[str, object]] = []
        for i in range(PERMUTATION_TRIALS):
            perm = list(self.candidate_laws)
            random.shuffle(perm)
            if perm == self.candidate_laws:
                random.shuffle(perm)
            engine2 = LibraryAndPath(perm, PREREQUISITES, self.observations)
            chosen2, _, _ = engine2.search(MAX_DEPTH, CAP_PATHS, fast=FAST_THOROUGH)
            if not chosen2:
                mismatches.append({"trial": i + 1, "issue": "no_path_under_permutation"})
            else:
                eq_laws = set(chosen2.final_laws) == base_laws
                eq_len = len(chosen2.path) == base_len
                if not (eq_laws and eq_len):
                    mismatches.append({
                        "trial": i + 1,
                        "final_laws_equal": eq_laws,
                        "path_length_equal": eq_len
                    })
        permutation_invariant = (len(mismatches) == 0)

        passed = bool(
            c1
            and c2
            and monotonic
            and acyclic
            and minimal_laws
            and minimal_steps
            and permutation_invariant
            and enabled_records_consistent
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
            "permutation_mismatches": mismatches,
            "passed": passed,
        }

    # ----- Orchestrate a full run
    def run(self, max_depth: int, cap_paths: int) -> Dict[str, object]:
        chosen, total, consistent = self.search(max_depth=max_depth, cap_paths=cap_paths, fast=FAST_THOROUGH)
        if not chosen:
            answer = {"selected_path": None, "final_state": None}
            reason_lines = ["No consistent path found under current settings."]
            check = {"passed": False}
        else:
            answer = {
                "selected_path": [step.action for step in chosen.path],
                "final_state": {
                    "laws": sorted(chosen.final_laws),
                    "features": sorted(chosen.final_features),
                },
            }
            reason_lines = self.explain(chosen, total_nodes=total, consistent_count=consistent)
            check = self.independent_check(chosen)

        return {"answer": answer, "reason": reason_lines, "check": check}


def main():
    engine = LibraryAndPath(CANDIDATE_LAWS, PREREQUISITES, OBSERVATIONS)
    result = engine.run(MAX_DEPTH, CAP_PATHS)
    print(json.dumps(result, indent=2, ensure_ascii=False))


if __name__ == "__main__":
    main()

