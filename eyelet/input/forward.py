# Reasoning example that uses **variables in all three positions of a triple,
# including the predicate itself**.  The script derives family relations and
# leverages a rule that treats any property declared `SYMMETRIC_RELATION`
# as symmetric.

from itertools import product
from copy import deepcopy

# ---------------------------------------------------------------------------
# Initial knowledge
# ---------------------------------------------------------------------------
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

# ---------------------------------------------------------------------------
# Rules  (rule_id, antecedents, consequent)
# Variables start with "?"
#
#  R‑sym:  ?p a SYMMETRIC_RELATION,  ?x ?p ?y   =>   ?y ?p ?x
#          (predicate variable ?p)
#
rules = [
    # Grandparent
    ("R‑gp",  [("?x", "parent", "?y"), ("?y", "parent", "?z")],
               ("?x", "grandparent", "?z")),

    # Mother and father
    ("R‑mom", [("?x", "parent", "?y"), ("?x", "a", "FEMALE")],
               ("?x", "mother", "?y")),
    ("R‑dad", [("?x", "parent", "?y"), ("?x", "a", "MALE")],
               ("?x", "father", "?y")),

    # Grandmother / grandfather
    ("R‑gma", [("?x", "grandparent", "?y"), ("?x", "a", "FEMALE")],
               ("?x", "grandmother", "?y")),
    ("R‑gpa", [("?x", "grandparent", "?y"), ("?x", "a", "MALE")],
               ("?x", "grandfather", "?y")),

    # Symmetric-property rule (predicate variable)
    ("R‑sym", [("?p", "a", "SYMMETRIC_RELATION"), ("?x", "?p", "?y")],
               ("?y", "?p", "?x")),
]

# ---------------------------------------------------------------------------
# Small unification helpers
# ---------------------------------------------------------------------------
is_var = lambda t: isinstance(t, str) and t.startswith("?")

def unify(pattern, fact, theta):
    theta = dict(theta)
    for p, f in zip(pattern, fact):
        if is_var(p):
            if p in theta and theta[p] != f:
                return None
            theta[p] = f
        elif p != f:
            return None
    return theta

def subst(triple, theta):
    return tuple(theta.get(t, t) for t in triple)

# ---------------------------------------------------------------------------
# Forward‑chaining with proof trace
# ---------------------------------------------------------------------------
proof = {f: ("GIVEN", []) for f in facts}
step = 0
changed = True
while changed:
    changed = False
    for rid, ants, cons in sorted(rules, key=lambda r: r[0]):
        # Find candidate facts for each antecedent
        matches = [[] for _ in ants]
        for i, ant in enumerate(ants):
            for fact in sorted(facts):
                sub = unify(ant, fact, {})
                if sub is not None:
                    matches[i].append((fact, sub))

        # Iterate over combinations of antecedent matches
        for combo in product(*matches):
            theta, premises = {}, []
            consistent = True
            for fact, sub in combo:
                premises.append(fact)
                for var, val in sub.items():
                    if var in theta and theta[var] != val:
                        consistent = False
                        break
                    theta[var] = val
                if not consistent:
                    break
            if not consistent:
                continue

            new_fact = subst(cons, theta)
            if new_fact not in facts:
                step += 1
                facts.add(new_fact)
                proof[new_fact] = (rid, deepcopy(premises))
                print(f"Step {step:02}: {new_fact}   (by {rid} using {premises})")
                changed = True

print("\n=== Fix-point reached ===")
for triple in sorted(facts):
    print(triple)

