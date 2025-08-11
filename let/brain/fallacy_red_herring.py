#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Red Herring — ARC (Answer / Reason / Check), self-contained

Idea (toy detector)
  An argument A commits a red herring if it leans on a salient, *off-topic*
  consideration while providing no evidence that actually bears on the stated issue.

Encoding
  issue(A, I)               – the focal question/topic the argument *should* address
  evidence(A, T)            – a topic used as evidence in A
  conclusion_topic(A, C)    – (for display) the topic of the conclusion
  salient(T)                – emotionally charged / attention-grabbing topic
  support(X, Y)             – topic-level relevance: X can support/relate to Y

Detection (minimal)
  Let Relevant(A)  = { e | evidence(A,e) and e ↝* issue(A) via support edges }.
  Let IrrelSal(A)  = { e | evidence(A,e) and NOT (e ↝* issue(A)) and salient(e) }.
  We flag red_herring(A) iff IrrelSal(A) is non-empty AND Relevant(A) is empty.

This captures the “distract and steer away from the issue” flavor without
penalizing arguments that mention a colorful aside *in addition to* relevant evidence.
"""

from collections import deque
from typing import Dict, List, Set, Tuple

# ───────────────────────────── Example arguments ─────────────────────────────
sentences: Dict[str, str] = {
    "Arg1": "We shouldn’t strengthen privacy regulation because the CEO is very charitable.",
    "Arg2": "We shouldn’t strengthen privacy regulation because compliance costs will hurt small firms.",
    "Arg3": "Approve the transport plan; besides, the opposition had a scandal years ago.",
    "Arg4": "Adopt the health policy; clinical trials show reduced mortality.",
    "Arg5": "Support the emissions cap; it reduces smog — and by the way, the sponsor is popular.",
}

issue: Dict[str, str] = {
    "Arg1": "privacy_regulation",
    "Arg2": "privacy_regulation",
    "Arg3": "transport_plan",
    "Arg4": "health_policy",
    "Arg5": "emissions_cap",
}

# Evidence topics mentioned/relied upon in each argument
evidence: Set[Tuple[str, str]] = {
    ("Arg1", "charity"),
    ("Arg2", "compliance_costs"),
    ("Arg3", "scandal"),
    ("Arg4", "clinical_trials"),
    ("Arg4", "reduced_mortality"),
    ("Arg5", "smog_reduction"),
    ("Arg5", "popularity"),
}

# Conclusion topic (for display only)
conclusion_topic: Dict[str, str] = {
    "Arg1": "oppose_privacy_regulation",
    "Arg2": "oppose_privacy_regulation",
    "Arg3": "approve_transport_plan",
    "Arg4": "adopt_health_policy",
    "Arg5": "support_emissions_cap",
}

# Which topics are (toy) “salient” / distraction-prone
salient_topics: Set[str] = {"charity", "scandal", "celebrity_gossip", "popularity"}

# Topic-level relevance links (tiny background)
# Read: support(X, Y) means topic X bears on/helps assess issue Y.
support_edges: Set[Tuple[str, str]] = {
    ("compliance_costs", "privacy_regulation"),
    ("clinical_trials", "health_policy"),
    ("reduced_mortality", "health_policy"),
    ("smog_reduction", "emissions_cap"),
    # (intentionally no links from 'charity', 'scandal', or 'popularity' to issues)
}

# ───────────────────────────── Inference helpers ─────────────────────────────
def reachable(src: str, dst: str) -> bool:
    """BFS reachability in the support graph: is dst reachable from src?"""
    if src == dst:
        return True
    Q: deque[str] = deque([src])
    seen: Set[str] = set()
    while Q:
        u = Q.popleft()
        if u in seen:
            continue
        seen.add(u)
        for (a, b) in support_edges:
            if a == u:
                if b == dst:
                    return True
                if b not in seen:
                    Q.append(b)
    return False

def classify_argument(aid: str) -> Tuple[bool, List[str], List[str]]:
    """
    Return (is_red_herring, relevant_evidence, irrelevant_salient_evidence).
    """
    I = issue[aid]
    evs = sorted([t for (A, t) in evidence if A == aid])
    rel: List[str] = []
    irr_sal: List[str] = []

    for t in evs:
        if reachable(t, I):
            rel.append(t)
        else:
            if t in salient_topics:
                irr_sal.append(t)

    is_rh = (len(irr_sal) > 0 and len(rel) == 0)
    return is_rh, rel, irr_sal

# ────────────────────────────────── ARC: Answer ─────────────────────────────
def print_answer() -> None:
    print("Answer")
    print("======")
    results: Dict[str, bool] = {}

    for aid, text in sentences.items():
        print(f"\n=== {aid}: {text}")
        I = issue[aid]
        C = conclusion_topic[aid]
        evs = sorted([t for (A, t) in evidence if A == aid])
        print(f"Issue:        {I}")
        print(f"Conclusion:   {C}")
        print(f"Evidence:     {evs or '∅'}")

        is_rh, rel, irr_sal = classify_argument(aid)
        print(f"Relevant→Issue: {rel or '∅'}")
        print(f"Salient off-topic: {irr_sal or '∅'}")

        if is_rh:
            print("Result: RED HERRING (salient distraction with no relevant evidence)")
        else:
            print("Result: no red herring detected")
        results[aid] = is_rh

    print("\nSummary")
    for aid in sorted(sentences):
        print(f"  {aid}: {'red herring' if results[aid] else 'ok'}")

# ───────────────────────────────── ARC: Reason why ──────────────────────────
def print_reason() -> None:
    print("\nReason why")
    print("==========")
    print("We model topic-level relevance explicitly via support(X, issue).")
    print("An argument is flagged only if it leans on at least one *salient, off-topic*")
    print("consideration and lacks *any* evidence that connects to the issue.")
    print("Thus a colorful aside does not trigger a flag when the argument also presents")
    print("relevant evidence (see Arg5).")

# ─────────────────────────────── ARC: Check (harness) ───────────────────────
def print_check() -> None:
    print("\nCheck (harness)")
    print("===============")
    ok_all = True

    # 1) Expected classifications
    expected = {
        "Arg1": True,   # charity → salient & off-topic; no relevant evidence
        "Arg2": False,  # compliance_costs → relevant
        "Arg3": True,   # scandal → salient & off-topic; no relevant evidence
        "Arg4": False,  # trials & mortality → relevant
        "Arg5": False,  # has both relevant (smog_reduction) and salient off-topic (popularity) → NOT flagged
    }
    ok_cls = True
    for aid, want in expected.items():
        got, _, _ = classify_argument(aid)
        if got != want:
            ok_cls = False
            print(f"  MISMATCH {aid}: got {got}, want {want}")
    print(f"Expected classifications hold? {ok_cls}")
    ok_all &= ok_cls

    # 2) Soundness: if we add a relevance link for a previously off-topic salient item, the flag should clear
    support_edges.add(("charity", "privacy_regulation"))
    cleared, _, _ = classify_argument("Arg1")
    print(f"Arg1 clears after adding support(charity→privacy_regulation)? {not cleared}")
    # revert
    support_edges.remove(("charity", "privacy_regulation"))
    ok_all &= (not cleared)

    # 3) Soundness: adding any relevant evidence should clear a red herring
    evidence.add(("Arg3", "traffic_flow"))
    support_edges.add(("traffic_flow", "transport_plan"))
    cleared2, _, _ = classify_argument("Arg3")
    print(f"Arg3 clears after adding relevant evidence? {not cleared2}")
    # revert
    evidence.discard(("Arg3", "traffic_flow"))
    support_edges.remove(("traffic_flow", "transport_plan"))
    ok_all &= (not cleared2)

    # 4) Determinism / idempotence
    a = classify_argument("Arg4")
    b = classify_argument("Arg4")
    print(f"Deterministic (same inputs ⇒ same result)? {a == b}")
    ok_all &= (a == b)

    print(f"\nAll checks passed? {ok_all}")

# ─────────────────────────────────── Main ───────────────────────────────────
if __name__ == "__main__":
    print_answer()
    print_reason()
    print_check()

