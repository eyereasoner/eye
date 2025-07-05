"""
family.py  –  goal-oriented (backward-chaining) proof with trace.

The data and rules are identical to the forward-chaining demo, but the
engine *starts from the goal* and works backwards. This is also called a
goal-oriented proof or SLD-resolution tree (in Prolog terminology).
"""

from itertools import count

# ──────────────────────────────────────────────────────────────
# 1. Facts: base knowledge (as (subject, relation, object) triples)
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
# 2. Rules: (rule_id, [body_goals], head)
#    Each rule is Horn-style: body (list of goals), head (conclusion)
# ──────────────────────────────────────────────────────────────
rules = [
    ("R-gp",  [("?x", "parent", "?y"), ("?y", "parent", "?z")],
               ("?x", "grandparent", "?z")),
    ("R-mom", [("?x", "parent", "?y"), ("?x", "a", "FEMALE")],
               ("?x", "mother", "?y")),
    ("R-dad", [("?x", "parent", "?y"), ("?x", "a", "MALE")],
               ("?x", "father", "?y")),
    ("R-gma", [("?x", "grandparent", "?y"), ("?x", "a", "FEMALE")],
               ("?x", "grandmother", "?y")),
    ("R-gpa", [("?x", "grandparent", "?y"), ("?x", "a", "MALE")],
               ("?x", "grandfather", "?y")),
    ("R-sym", [("?p", "a", "SYMMETRIC_RELATION"), ("?x", "?p", "?y")],
               ("?y", "?p", "?x")),
]

# ──────────────────────────────────────────────────────────────
# 3. Unification and substitution utilities
# ──────────────────────────────────────────────────────────────

is_var = lambda t: isinstance(t, str) and t.startswith("?")

def unify(pattern, datum, θ):
    """Unify a pattern triple with a datum triple under substitution θ.
       Returns an extended substitution dict if unification succeeds, else None."""
    θ = dict(θ)  # shallow copy to avoid side effects
    for p, d in zip(pattern, datum):
        if is_var(p):
            if p in θ:
                if θ[p] != d:
                    return None
            else:
                θ[p] = d
        elif p != d:
            return None
    return θ

def subst(triple, θ):
    """Apply substitution θ to a triple, replacing variables with their values."""
    return tuple(θ.get(t, t) for t in triple)

# ──────────────────────────────────────────────────────────────
# 4. Proof tracing and indentation helpers
# ──────────────────────────────────────────────────────────────
step_no = count(1)
def indent(d):
    return "  " * d

# ──────────────────────────────────────────────────────────────
# 5. Backward-chaining prover (depth-first, prints trace)
# ──────────────────────────────────────────────────────────────
def bc_prove(goal, θ, depth, path):
    g = subst(goal, θ)
    tag = next(step_no)
    print(f"{indent(depth)}Step {tag:02}: prove {g}")

    # 1. Try all ground facts
    for fact in facts:
        θ2 = unify(g, fact, {})
        if θ2 is not None:
            print(f"{indent(depth)}  ✓ fact {fact}")
            yield {**θ, **θ2}
            return  # stop at first fact for tidy trace

    # 2. Try all rules whose head unifies with the goal
    for rid, body, head in rules:
        body_std, head_std = standardise_apart(body, head)   # ← NEW
        θ_head = unify(head_std, g, {})
        if θ_head is None:
            continue        # head doesn't match goal

        head_inst = subst(head, θ_head)
        key = (rid, head_inst)
        if key in path:     # loop detection
            continue

        print(f"{indent(depth)}  → via {rid}")
        # Prove each goal in the rule body, in order
        def prove_seq(i, θ_curr):
            if i == len(body):
                yield θ_curr
            else:
                for θ_next in bc_prove(body[i], θ_curr, depth + 1, path | {key}):
                    yield from prove_seq(i + 1, θ_next)

        for θ_final in prove_seq(0, {**θ, **θ_head}):
            yield θ_final
            return  # stop after first proof to keep trace tidy

fresh = count(1).__next__          # 1, 2, 3, …
def standardise_apart(body, head):
    """Return a *renamed-apart* copy of this rule."""
    rename = {}
    def fresh_term(t):
        if isinstance(t, str) and t.startswith("?"):
            rename.setdefault(t, f"{t}_{fresh()}")
            return rename[t]
        return t
    return [tuple(map(fresh_term, triple)) for triple in body], \
           tuple(map(fresh_term, head))

# ──────────────────────────────────────────────────────────────
# 6. Convenience wrapper to ask a question and print trace
# ──────────────────────────────────────────────────────────────
def ask(goal):
    print(f"\n=== Proving {goal} ===")
    for θ in bc_prove(goal, {}, 0, frozenset()):
        print(f"✔ PROVED {subst(goal, θ)}\n")
        return True
    print("✗ NOT PROVED\n")
    return False

# ──────────────────────────────────────────────────────────────
# 7. Example queries
# ──────────────────────────────────────────────────────────────
if __name__ == "__main__":
    queries = [
        ("alice", "grandparent", "carol"),
        ("alice", "grandmother", "carol"),
        ("bob",   "father",      "carol"),
        ("dave",  "friend_of",   "alice"),
    ]
    for g in queries:
        ask(g)

