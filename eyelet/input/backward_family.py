"""
backward_family.py  –  backward-chaining with proof trace
The data and rules are identical to the forward-chaining demo, but the
engine now *starts from the goal* and works backwards.
"""

from itertools import count
from copy import deepcopy

# ──────────────────────────────────────────────────────────────
# Facts
# ──────────────────────────────────────────────────────────────
facts = {
    ("alice", "a", "FEMALE"),
    ("bob",   "a", "MALE"),
    ("carol", "a", "FEMALE"),
    ("dave",  "a", "MALE"),

    ("alice", "parent", "bob"),
    ("bob",   "parent", "carol"),

    ("friend_of", "a", "SYMMETRIC_RELATION"),
    ("alice", "friend_of", "dave"),
}

# ──────────────────────────────────────────────────────────────
# Rules (rule-id, body, head)
# ──────────────────────────────────────────────────────────────
rules = [
    ("R-gp",  [("?x","parent","?y"), ("?y","parent","?z")],
               ("?x","grandparent","?z")),
    ("R-mom", [("?x","parent","?y"), ("?x","a","FEMALE")],
               ("?x","mother","?y")),
    ("R-dad", [("?x","parent","?y"), ("?x","a","MALE")],
               ("?x","father","?y")),
    ("R-gma", [("?x","grandparent","?y"), ("?x","a","FEMALE")],
               ("?x","grandmother","?y")),
    ("R-gpa", [("?x","grandparent","?y"), ("?x","a","MALE")],
               ("?x","grandfather","?y")),
    ("R-sym", [("?p","a","SYMMETRIC_RELATION"), ("?x","?p","?y")],
               ("?y","?p","?x")),
]

# ──────────────────────────────────────────────────────────────
# Unification helpers
# ──────────────────────────────────────────────────────────────
is_var = lambda t: isinstance(t,str) and t.startswith("?")

def unify(pattern, datum, θ):
    """Return a substitution that makes pattern = datum under θ, or None."""
    θ = dict(θ)
    for p, d in zip(pattern, datum):
        if is_var(p):
            if p in θ and θ[p] != d:
                return None
            θ[p] = d
        elif p != d:
            return None
    return θ

def subst(triple, θ):          # apply substitution
    return tuple(θ.get(t,t) for t in triple)

# ──────────────────────────────────────────────────────────────
# Pretty printing helpers
# ──────────────────────────────────────────────────────────────
step_no = count(1)
indent   = lambda d: "  "*d

# ──────────────────────────────────────────────────────────────
# Depth-first backward-chaining prover (with tracing)
# ──────────────────────────────────────────────────────────────
def bc_prove(goal, θ, depth, path):
    g = subst(goal, θ)
    tag = next(step_no)
    print(f"{indent(depth)}Step {tag:02}: prove {g}")

    # 1. Try ground facts
    for fact in facts:
        θ2 = unify(g, fact, {})
        if θ2 is not None:
            print(f"{indent(depth)}  ✓ fact {fact}")
            yield {**θ, **θ2}
            return            # first fact suffices for this demo

    # 2. Try rules whose head unifies with the goal
    for rid, body, head in rules:
        θ_head = unify(head, g, {})
        if θ_head is None:            # head doesn’t match
            continue
        head_inst = subst(head, θ_head)
        key = (rid, head_inst)
        if key in path:               # loop guard
            continue

        print(f"{indent(depth)}  → via {rid}")
        # prove body sequentially
        def prove_seq(i, θ_curr):
            if i == len(body):
                yield θ_curr
            else:
                for θ_next in bc_prove(body[i], θ_curr, depth+1, path|{key}):
                    yield from prove_seq(i+1, θ_next)

        for θ_final in prove_seq(0, {**θ, **θ_head}):
            yield θ_final
            return                    # stop after first proof to keep trace tidy

# ──────────────────────────────────────────────────────────────
# Convenience wrapper
# ──────────────────────────────────────────────────────────────
def ask(goal):
    print(f"\n=== Proving {goal} ===")
    for θ in bc_prove(goal, {}, 0, frozenset()):
        print(f"✔ PROVED {subst(goal, θ)}\n")
        return True
    print("✗ NOT PROVED\n")
    return False

# ──────────────────────────────────────────────────────────────
# Demo queries
# ──────────────────────────────────────────────────────────────
for g in [
    ("alice","grandparent","carol"),
    ("alice","grandmother","carol"),
    ("bob",  "father",     "carol"),
    ("dave", "friend_of",  "alice"),
]:
    ask(g)

