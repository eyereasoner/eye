"""
Tabled backward-chaining (tiny N3-ish demo) with a COMPACT failure printer.
This treats strings starting with "?" as variables (e.g., "?X").
All other strings are constants (e.g., "A", "B", "C", "D").

Key improvement
---------------
We add a **local fixpoint loop** per goal in `prove_tabled`: after collecting
facts, we apply rules repeatedly until no new instances are added to the table
entry for that goal's predicate. This lets recursive rules (like transitivity)
see newly derived answers in the same call, producing the full closure.

Representation
--------------
- Triples are (S, P, O)
- Variables are strings starting with "?": e.g., "?X", "?Who"
- Rules are (rule_id, HEAD, [BODY_TRIPLES...])
- Facts are ground triples (no variables)
- Built-in supported: math:greaterThan

Features
--------
- Standardize-apart on each rule application (avoids variable capture)
- Variant-based tabling (memoization) with in-progress detection
- **Local fixpoint per goal** (saturates recursive rules like transitivity)
- Proof trees for successful derivations
- Compact failure explanations (depth/width/line caps + grouping)

With the KB below, the open query:
  ask(("?X", ":moreInterestingThan", "?Y"), show_all=True)
returns (order may vary):
  B>C, D>B, A>B, D>C, A>C
"""

from itertools import count
from dataclasses import dataclass, field
from typing import Any, Dict, Iterable, List, Optional, Tuple, Set
from collections import defaultdict

Triple = Tuple[Any, str, Any]
Subst = Dict[str, Any]

# -----------------------------------------------------------------------------
# Knowledge base: rules and facts (extend freely)
# -----------------------------------------------------------------------------

RULES: List[Tuple[str, Triple, List[Triple]]] = [
    # R1: by score
    # (?X :moreInterestingThan ?Y) <= (?X :score ?SX), (?Y :score ?SY), (?SX math:greaterThan ?SY)
    (
        "R1_score",
        ("?X", ":moreInterestingThan", "?Y"),
        [
            ("?X", ":score", "?SX"),
            ("?Y", ":score", "?SY"),
            ("?SX", "math:greaterThan", "?SY"),
        ],
    ),
    # R2: award/boring context
    # (?X :moreInterestingThan ?Y) <= (?X :hasAward true), (?Y :boring true)
    (
        "R2_award_boring",
        ("?X", ":moreInterestingThan", "?Y"),
        [
            ("?X", ":hasAward", True),
            ("?Y", ":boring", True),
        ],
    ),
    # R3: transitivity (recursive)
    # (?X :moreInterestingThan ?Z) <= (?X :moreInterestingThan ?Y), (?Y :moreInterestingThan ?Z)
    (
        "R3_transitive",
        ("?X", ":moreInterestingThan", "?Z"),
        [
            ("?X", ":moreInterestingThan", "?Y"),
            ("?Y", ":moreInterestingThan", "?Z"),
        ],
    ),
]

FACTS: List[Triple] = [
    # Scores (ground facts)
    ("A", ":score", 8),
    ("B", ":score", 5),

    # Base edge to enable A > C by transitivity (no score for C):
    ("B", ":moreInterestingThan", "C"),

    # Contextual facts for the award/boring rule: D > B
    ("D", ":hasAward", True),
    ("B", ":boring", True),
]

# -----------------------------------------------------------------------------
# Variables, substitution, unification
# -----------------------------------------------------------------------------

def is_var(t: Any) -> bool:
    """A term is a variable iff it's a string starting with '?'."""
    return isinstance(t, str) and t.startswith("?")

def walk(t: Any, theta: Subst) -> Any:
    """Follow substitutions to a representative (cycle‑guarded)."""
    seen = set()
    while is_var(t) and t in theta:
        if t in seen:
            break
        seen.add(t)
        t = theta[t]
    return t

def subst_term(t: Any, theta: Subst) -> Any:
    return walk(t, theta)

def subst_triple(tr: Triple, theta: Subst) -> Triple:
    s, p, o = tr
    return (subst_term(s, theta), p, subst_term(o, theta))

def unify(a: Any, b: Any, theta: Subst) -> Optional[Subst]:
    a, b = walk(a, theta), walk(b, theta)
    if a == b:
        return theta
    if is_var(a):
        t2 = dict(theta); t2[a] = b; return t2
    if is_var(b):
        t2 = dict(theta); t2[b] = a; return t2
    return None

def unify_triples(pat: Triple, fact: Triple, theta: Subst) -> Optional[Subst]:
    s1, p1, o1 = pat
    s2, p2, o2 = fact
    if p1 != p2:
        return None
    theta = unify(s1, s2, theta)
    if theta is None:
        return None
    theta = unify(o1, o2, theta)
    return theta

# -----------------------------------------------------------------------------
# Standardize-apart for rules (prevents variable capture across applications)
# -----------------------------------------------------------------------------

_fresh = count(1)

def std_apart_rule(rule: Tuple[str, Triple, List[Triple]]) -> Tuple[str, Triple, List[Triple]]:
    """Rename ?Vars in a rule with a fresh numeric suffix on each application."""
    rid, head, body = rule
    n = next(_fresh)
    mapping: Dict[str, str] = {}

    def rn(t: Any) -> Any:
        if is_var(t):
            if t not in mapping:
                mapping[t] = f"{t}_{n}"
            return mapping[t]
        return t

    def rn_triple(tr: Triple) -> Triple:
        s, p, o = tr
        return (rn(s), p, rn(o))

    return rid, rn_triple(head), [rn_triple(b) for b in body]

# -----------------------------------------------------------------------------
# Built-ins
# -----------------------------------------------------------------------------

def is_builtin(pred: str) -> bool:
    return pred == "math:greaterThan"

def eval_builtin(goal: Triple) -> Tuple[bool, str]:
    """Built-ins succeed only when their arguments are ground."""
    s, p, o = goal
    if p == "math:greaterThan":
        if is_var(s) or is_var(o):
            return False, "not ground"
        try:
            ok = s > o
        except Exception as e:
            return False, f"error: {e}"
        return ok, f"{s} > {o}"
    return False, f"unknown builtin {p}"

# -----------------------------------------------------------------------------
# Variant canonicalization (for tabling keys)
# -----------------------------------------------------------------------------

def canonicalize_goal(g: Triple) -> Triple:
    """
    Alpha‑variant canonicalization: replace variables by ?v0, ?v1 ... in
    order of first appearance; keep constants as‑is; predicate unchanged.
    """
    s, p, o = g
    mapping: Dict[str, str] = {}
    counter = 0

    def canon_term(t: Any) -> Any:
        nonlocal counter
        if is_var(t):
            if t not in mapping:
                mapping[t] = f"?v{counter}"
                counter += 1
            return mapping[t]
        return t

    return (canon_term(s), p, canon_term(o))

# -----------------------------------------------------------------------------
# Proof / Failure objects and pretty printers
# -----------------------------------------------------------------------------

@dataclass
class ProofNode:
    goal: Triple
    kind: str                        # "fact", "builtin", "rule", or "memo"
    detail: Optional[str] = None
    children: List["ProofNode"] = field(default_factory=list)

def pp_proof(node: ProofNode, indent: int = 0) -> None:
    pad = "  " * indent
    if node.kind == "rule":
        print(f"{pad}Goal: {node.goal}  via {node.detail}")
        for ch in node.children:
            pp_proof(ch, indent + 1)
        print(f"{pad}✓ Proven: {node.goal}")
    elif node.kind == "fact":
        print(f"{pad}✓ fact {node.detail}")
    elif node.kind == "builtin":
        print(f"{pad}✓ builtin {node.goal} ({node.detail})")
    elif node.kind == "memo":
        print(f"{pad}✓ memoized {node.goal}")

@dataclass
class Failure:
    goal: Triple
    reason: str
    children: List["Failure"] = field(default_factory=list)

def pp_failure(node: Failure,
               indent: int = 0,
               max_depth: int = 6,
               max_children: int = 3,
               max_lines: int = 60) -> None:
    """Compact failure tree printer (depth/width/line caps + grouping)."""
    state = {"lines": 0}
    def _pp(n: Failure, d: int) -> None:
        if state["lines"] >= max_lines:
            return
        pad = "  " * d
        print(f"{pad}✗ {n.goal} — {n.reason}")
        state["lines"] += 1
        if not n.children:
            return
        if d >= max_depth:
            if state["lines"] < max_lines:
                print(pad + "  … (more detail truncated)")
                state["lines"] += 1
            return
        groups = defaultdict(list)
        for ch in n.children:
            groups[(ch.goal, ch.reason)].append(ch)
        grouped = sorted(groups.items(), key=lambda kv: len(kv[1]), reverse=True)
        shown = 0
        for (g_goal, g_reason), group in grouped:
            if shown >= max_children:
                remaining = sum(len(v) for _, v in grouped[shown:])
                if state["lines"] < max_lines:
                    print(pad + f"  … ({remaining} similar branches hidden)")
                    state["lines"] += 1
                break
            rep = group[0]; times = len(group)
            suffix = f" ×{times}" if times > 1 else ""
            if state["lines"] >= max_lines:
                break
            print(f"{pad}  ↳ {g_goal} — {g_reason}{suffix}")
            state["lines"] += 1
            for grand in rep.children[:max_children]:
                if state["lines"] >= max_lines: break
                _pp(grand, d + 2)
            shown += 1
    _pp(node, indent)

# -----------------------------------------------------------------------------
# Tabling: memo table of variant -> instances + state
# -----------------------------------------------------------------------------

@dataclass
class TableEntry:
    key: Triple                           # canonicalized goal
    instances: Set[Triple] = field(default_factory=set)  # proven instances
    in_progress: bool = False
    completed: bool = False

TABLE: Dict[Triple, TableEntry] = {}
DEFAULT_DEPTH_LIMIT = 200  # secondary safety net

# -----------------------------------------------------------------------------
# Tabled prover (with local fixpoint per goal)
# -----------------------------------------------------------------------------

def table_answers_for(goal_inst: Triple, theta: Subst) -> Iterable[Tuple[Subst, ProofNode]]:
    """Yield answers by unifying already-memoized instances with the current goal."""
    canon = canonicalize_goal(goal_inst)
    entry = TABLE.get(canon)
    if not entry or not entry.instances:
        return
    for inst in list(entry.instances):
        s2 = unify_triples(goal_inst, inst, dict(theta))
        if s2 is not None:
            yield s2, ProofNode(goal=goal_inst, kind="memo", detail="memoized")

def prove_tabled(
    goal: Triple,
    theta: Subst,
    depth: int = 0,
    depth_limit: int = DEFAULT_DEPTH_LIMIT,
) -> Iterable[Tuple[Subst, ProofNode]]:
    """
    Prove `goal` under substitution `theta` using tabling + local fixpoint.

    Important behavior for recursion:
    - If a subgoal for the *same* predicate is encountered while its table
      entry is in_progress, we **only consume current memoized answers** for
      that subgoal (no re-expansion). The **outer local fixpoint loop** then
      re-applies rules until no new instances are added, letting recursive rules
      (like transitivity) see newly derived answers in the same call.
    """
    if depth > depth_limit:
        return

    g = subst_triple(goal, theta)
    canon = canonicalize_goal(g)
    entry = TABLE.get(canon)

    # 0) Try memoized answers first
    if entry:
        for s2, memo_node in table_answers_for(g, theta):
            yield s2, memo_node
        if entry.in_progress:
            # Avoid left-recursive expansion; outer caller will iterate to fixpoint.
            return
    else:
        entry = TableEntry(key=canon)
        TABLE[canon] = entry

    # Mark as in progress while computing new answers
    entry.in_progress = True

    s, p, o = g

    # 1) Built-ins
    if is_builtin(p):
        ok, reason = eval_builtin(g)
        if ok:
            inst = g  # ground by construction when builtin succeeds
            if inst not in entry.instances:
                entry.instances.add(inst)
                yield theta, ProofNode(goal=g, kind="builtin", detail=reason)
        entry.in_progress = False
        entry.completed = True
        return

    # 2) Facts (one pass is enough; facts don't change)
    for fact in FACTS:
        if fact[1] != p:
            continue
        theta2 = unify_triples(g, fact, dict(theta))
        if theta2 is not None:
            inst = subst_triple(g, theta2)
            if inst not in entry.instances:
                entry.instances.add(inst)
                yield theta2, ProofNode(goal=g, kind="fact", detail=str(fact))

    # 3) Rules (apply to **local fixpoint**: loop until no new instances)
    changed = True
    while changed:
        changed = False

        for rule in RULES:
            rid, head, body = std_apart_rule(rule)
            if head[1] != p:
                continue
            theta_head = unify_triples(g, head, dict(theta))
            if theta_head is None:
                continue

            # Prove the body in sequence with backtracking
            def prove_body(goals: List[Triple], th: Subst) -> Iterable[Tuple[Subst, List[ProofNode]]]:
                if not goals:
                    yield th, []
                    return
                first, rest = goals[0], goals[1:]
                # IMPORTANT: If `first` is the same predicate as `g`, and our table
                # is in_progress, `prove_tabled`(first, ...) will yield only the
                # **current memoized** answers. Because we're in a while-loop that
                # repeats until no new instances are added, newly derived answers
                # will be picked up on the next iteration.
                for th1, proof_first in prove_tabled(first, th, depth + 1, depth_limit):
                    for th2, proof_rest in prove_body(rest, th1):
                        yield th2, [proof_first] + proof_rest

            for th_final, children in prove_body(body, theta_head):
                inst = subst_triple(g, th_final)
                if inst not in entry.instances:
                    entry.instances.add(inst)
                    changed = True
                    yield th_final, ProofNode(goal=g, kind="rule", detail=rid, children=children)

    # Done expanding this goal to local fixpoint
    entry.in_progress = False
    entry.completed = True

# -----------------------------------------------------------------------------
# Diagnosis (unchanged from prior version; compact-aware)
# -----------------------------------------------------------------------------

@dataclass
class Failure:
    goal: Triple
    reason: str
    children: List["Failure"] = field(default_factory=list)

def diagnose(
    goal: Triple,
    theta: Subst,
    depth: int = 0,
    depth_limit: int = DEFAULT_DEPTH_LIMIT,
) -> Failure:
    """Explain why a goal cannot be proven (compact tree of reasons)."""
    if depth > depth_limit:
        return Failure(goal=subst_triple(goal, theta), reason=f"depth limit {depth_limit} exceeded")

    g = subst_triple(goal, theta)
    s, p, o = g
    canon = canonicalize_goal(g)
    entry = TABLE.get(canon)

    # If table already has matching instances, this shouldn't be called
    if entry:
        for inst in entry.instances:
            if unify_triples(g, inst, dict(theta)) is not None:
                return Failure(goal=g, reason="unexpected: memoized instance already proves goal")

    # Built-in predicate
    if is_builtin(p):
        ok, reason = eval_builtin(g)
        if not ok:
            return Failure(goal=g, reason=f"builtin {p} failed: {reason}")
        return Failure(goal=g, reason=f"unexpected: builtin {p} succeeded")

    # Facts quick check
    has_same_pred_fact = False
    for fact in FACTS:
        if fact[1] != p:
            continue
        has_same_pred_fact = True
        if unify_triples(g, fact, dict(theta)) is not None:
            return Failure(goal=g, reason="unexpected: matched a fact")

    # Gather applicable rules by predicate
    rules_p = [r for r in RULES if r[1][1] == p]
    if not rules_p and not has_same_pred_fact:
        return Failure(goal=g, reason=f"no facts and no rules with head predicate {p}")

    rule_failures: List[Failure] = []
    any_head_unified = False

    for rule in rules_p:
        rid, head, body = std_apart_rule(rule)
        theta_head = unify_triples(g, head, dict(theta))
        if theta_head is None:
            rule_failures.append(Failure(goal=g, reason=f"rule {rid} not applicable (head doesn't unify)"))
            continue
        any_head_unified = True

        # Try to satisfy body sequentially; report the first failing subgoal
        th_curr = theta_head
        body_ok = True
        body_fail_children: List[Failure] = []
        for sub in body:
            # Try memo first
            sub_inst_g = subst_triple(sub, th_curr)
            sub_canon = canonicalize_goal(sub_inst_g)
            sub_entry = TABLE.get(sub_canon)
            satisfied_by_memo = False
            if sub_entry:
                for inst in sub_entry.instances:
                    if unify_triples(sub_inst_g, inst, dict(th_curr)) is not None:
                        satisfied_by_memo = True
                        break
            if satisfied_by_memo:
                continue

            # Otherwise try to prove it freshly (bounded)
            found = False
            for _th_next, _proof in prove_tabled(sub, th_curr, depth + 1, depth_limit):
                found = True
                break
            if not found:
                body_ok = False
                body_fail_children.append(diagnose(sub, th_curr, depth + 1, depth_limit))
                break

        if body_ok:
            return Failure(goal=g, reason=f"unexpected: rule {rid} would actually prove the goal")
        else:
            rule_failures.append(Failure(goal=g, reason=f"rule {rid} body not provable", children=body_fail_children))

    if not any_head_unified:
        return Failure(goal=g, reason="no applicable rules unify with the goal's arguments")

    return Failure(goal=g, reason="all applicable rules' bodies failed", children=rule_failures)

# -----------------------------------------------------------------------------
# ask() helper
# -----------------------------------------------------------------------------

def ask(goal: Triple,
        show_all: bool = False,
        depth_limit: int = DEFAULT_DEPTH_LIMIT,
        explain: str = "compact") -> None:
    """
    Run a query, printing either:
      - YES (with the first or all proofs), plus variable bindings from the query, or
      - NO with a compact explanation of why not.
    """
    print(f"\n=== Query {goal} ===")
    # Clear the memo table between independent queries for clarity
    TABLE.clear()

    answers = list(prove_tabled(goal, {}, depth_limit=depth_limit))
    if answers:
        for i, (theta, proof) in enumerate(answers, start=1):
            inst = subst_triple(goal, theta)
            # Show bindings only for variables that appear in the query itself
            bindings = {t: subst_term(t, theta) for t in (goal[0], goal[2]) if is_var(t)}
            extra = f"  bindings={bindings}" if bindings else ""
            print(f"Answer {i}: YES  (instance = {inst}){extra}")
            pp_proof(proof)
            if not show_all:
                break
    else:
        print("Answer: NO")
        failure = diagnose(goal, {}, depth_limit=depth_limit)
        print("Why not?")
        if explain == "compact":
            pp_failure(failure, max_depth=6, max_children=3, max_lines=60)
        else:
            pp_failure(failure, max_depth=999, max_children=999, max_lines=10_000)

# -----------------------------------------------------------------------------
# Demo
# -----------------------------------------------------------------------------

if __name__ == "__main__":
    # 1) Score rule: A > B (8 > 5)
    ask(("A", ":moreInterestingThan", "B"))

    # 2) Transitivity: A > B and (fact) B > C  ⇒  A > C (no score for C)
    ask(("A", ":moreInterestingThan", "C"))

    # 3) Context rule: D has an award and B is boring  ⇒  D > B
    ask(("D", ":moreInterestingThan", "B"))

    # 4) Failure example: C > A should fail (compact explanation)
    ask(("C", ":moreInterestingThan", "A"))

    # 5) Open variable: Who is moreInterestingThan C?
    #    Expect B (fact), A (via A>B & B>C), and D (via D>B & B>C).
    ask(("?Who", ":moreInterestingThan", "C"), show_all=True)

    # 6) Both ends open: expect B>C, D>B, A>B, D>C, A>C (order may vary)
    ask(("?X", ":moreInterestingThan", "?Y"), show_all=True)

