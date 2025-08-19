#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Ad Hominem Fallacy — ARC (Answer / Reason / Check), self-contained

Definition (toy detector)
  fallacy(ad_hominem, A) holds when:
    • A contains a personal attack, and
    • A offers no evidence for/against the claim.

We encode each argument A with two extracted signals:
  ("A", "attack",  True|False)
  ("A", "evidence", True|False)

Rule:
  R-ad-hominem:
    { ("?A","attack",True), ("?A","evidence",False) }  ⇒  fallacy(ad_hominem, ?A)

We keep a tiny backward-chaining prover with a readable trace.
"""

from itertools import count
from typing import Dict, Tuple, List, Iterable

# ── facts (extracted signals) ─────────────────────────────────────────
facts = {
    ("Arg1", "attack", True),
    ("Arg1", "evidence", False),

    ("Arg2", "attack", False),
    ("Arg2", "evidence", True),

    ("Arg3", "attack", True),
    ("Arg3", "evidence", False),

    ("Arg4", "attack", False),
    ("Arg4", "evidence", False),
}

# Natural language versions for printing
sentences = {
    "Arg1": "Don’t listen to Smith; he’s a criminal.",
    "Arg2": "Smith’s argument is flawed because the data show X.",
    "Arg3": "You’re just too young to understand economics.",
    "Arg4": "Climate is changing primarily because of CO₂.",
}

# ── rule set ─────────────────────────────────────────────────────────
rules = [
    dict(
        id="R-ad-hominem",
        head=("fallacy", "ad_hominem", "?A"),
        body=[
            ("?A", "attack", True),
            ("?A", "evidence", False),
        ],
    ),
]

# ── unification helpers ──────────────────────────────────────────────
def is_var(t): return isinstance(t, str) and t.startswith("?")

def subst(term, env):
    if isinstance(term, tuple):
        return tuple(subst(x, env) for x in term)
    if is_var(term):
        while term in env and env[term] != term:
            term = env[term]
    return env.get(term, term)

def unify(pat, fact, env=None):
    """Unify nested tuples/atoms; returns extended env or None."""
    env = dict(env or {})
    pat = subst(pat, env)
    fact = subst(fact, env)

    if pat == fact:
        return env
    if is_var(pat):
        # occurs-check not needed for our flat facts
        if pat in env and env[pat] != fact:
            return None
        env[pat] = fact
        return env
    if is_var(fact):
        return unify(fact, pat, env)
    if isinstance(pat, tuple) and isinstance(fact, tuple) and len(pat) == len(fact):
        for a, b in zip(pat, fact):
            env = unify(a, b, env)
            if env is None:
                return None
        return env
    return None

# ── backward prover with full trace ──────────────────────────────────
def bc(goal: Tuple, env: Dict, depth: int, step=count(1)) -> Iterable[Dict]:
    g = subst(goal, env)
    indent = " " * depth
    print(f"{indent}Step {next(step):02}: prove {g}")

    # (a) facts
    found = False
    for f in facts:
        env2 = unify(g, f, env)
        if env2:
            print(f"{indent}  ✓ fact {f}")
            found = True
            yield env2
    if found:
        return  # goal satisfied by facts

    # (b) rules
    for r in rules:
        env_head = unify(r["head"], g, env)
        if env_head is None:
            continue
        print(f"{indent}  → via {r['id']}")
        def prove_seq(idx: int, ecur: Dict):
            if idx == len(r["body"]):
                yield ecur
                return
            atom = subst(r["body"][idx], ecur)
            any_sub = False
            for e_next in bc(atom, ecur, depth + 1, step):
                any_sub = True
                yield from prove_seq(idx + 1, e_next)
            if not any_sub:
                print(f"{indent}  ✗ sub-goal fails: {atom}")
        yield from prove_seq(0, env_head)

# ────────────────────────────── ARC: Answer ────────────────────────────────
def print_answer():
    print("Answer")
    print("======")
    results: Dict[str, bool] = {}

    for aid, text in sentences.items():
        print(f"\n=== {aid}: {text}")
        goal = ("fallacy", "ad_hominem", aid)
        proved = any(bc(goal, {}, 0))
        results[aid] = proved
        print("Result:", "ad hominem\n" if proved else "no ad hominem\n")

    print("Summary:")
    for a in sentences:
        print(f"  {a}: {'ad hominem' if results[a] else 'ok'}")

# ───────────────────────────── ARC: Reason why ─────────────────────────────
def print_reason():
    print("\nReason why")
    print("==========")
    print("This toy detector flags ad hominem when an argument attacks a person")
    print("AND offers no evidence. That matches Arg1 (“criminal”) and Arg3 (“too young”),")
    print("but not Arg2 (which cites data) or Arg4 (which presents a claim sans attack).")
    print("We prove fallacy(ad_hominem, A) by backward-chaining against the single rule above.")

# ─────────────────────────── ARC: Check (harness) ───────────────────────────
def print_check():
    print("\nCheck (harness)")
    print("===============")
    ok_all = True

    # 1) Ground truth matches our four examples
    def classify(aid: str) -> bool:
        return any(bc(("fallacy", "ad_hominem", aid), {}, 0))

    expected = {"Arg1": True, "Arg2": False, "Arg3": True, "Arg4": False}
    ok_cls = True
    for k, want in expected.items():
        got = classify(k)
        if got != want:
            ok_cls = False
            print(f"  MISMATCH {k}: got {got}, want {want}")
    print(f"Expected classifications hold? {ok_cls}")
    ok_all &= ok_cls

    # 2) Soundness w.r.t. the rule: add evidence to an attacking argument ⇒ not ad hominem
    facts.add(("ArgX", "attack", True))
    facts.add(("ArgX", "evidence", True))
    ok_sound = not classify("ArgX")
    print(f"Attacking + evidence is NOT flagged? {ok_sound}")
    ok_all &= ok_sound
    # cleanup
    facts.discard(("ArgX", "attack", True))
    facts.discard(("ArgX", "evidence", True))

    # 3) Completeness w.r.t. the rule: attack + no evidence ⇒ flagged
    facts.add(("ArgY", "attack", True))
    facts.add(("ArgY", "evidence", False))
    ok_complete = classify("ArgY")
    print(f"Attack without evidence IS flagged? {ok_complete}")
    ok_all &= ok_complete
    facts.discard(("ArgY", "attack", True))
    facts.discard(("ArgY", "evidence", False))

    # 4) Determinism/idempotence: running twice doesn’t change anything
    once = classify("Arg1")
    twice = classify("Arg1")
    print(f"Deterministic/back-to-back same result? {once == twice}")
    ok_all &= (once == twice)

    print(f"\nAll checks passed? {ok_all}")

# ─────────────────────────────────── Main ───────────────────────────────────
if __name__ == "__main__":
    print_answer()
    print_reason()
    print_check()

