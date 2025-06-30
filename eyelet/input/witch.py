"""
witch.py – derive that GIRL is a WITCH

Direct translation of https://www.w3.org/2000/10/swap/test/reason/witch.n3:

    @prefix : <witch#>.
    @keywords is, of, a.

    { ?x a BURNS. ?x a WOMAN }   => { ?x a WITCH }.           # R1
    GIRL a WOMAN.                                               

    { ?x a ISMADEOFWOOD. }      => { ?x a BURNS. }            # R2
    { ?x a FLOATS }             => { ?x a ISMADEOFWOOD }.     # R3
    DUCK a FLOATS.                                               

    { ?x a FLOATS. ?x SAMEWEIGHT ?y } => { ?y a FLOATS }.     # R4
    DUCK SAMEWEIGHT GIRL.
"""
from itertools import product
from copy import deepcopy

# ---------------------------------------------------------------------------
# Knowledge base (initial facts)
# ---------------------------------------------------------------------------
facts = {
    ("GIRL", "a", "WOMAN"),
    ("DUCK", "a", "FLOATS"),
    ("DUCK", "SAMEWEIGHT", "GIRL"),
}

# Rules  (id, antecedents, consequent)
rules = [
    ("R1", [("?x", "a", "BURNS"), ("?x", "a", "WOMAN")], ("?x", "a", "WITCH")),
    ("R2", [("?x", "a", "ISMADEOFWOOD")],              ("?x", "a", "BURNS")),
    ("R3", [("?x", "a", "FLOATS")],                    ("?x", "a", "ISMADEOFWOOD")),
    ("R4", [("?x", "a", "FLOATS"), ("?x", "SAMEWEIGHT", "?y")],
           ("?y", "a", "FLOATS")),
]

# ---------------------------------------------------------------------------
# Utilities
# ---------------------------------------------------------------------------
is_var = lambda t: isinstance(t, str) and t.startswith("?")

def unify(pattern, fact, theta):
    """Try to extend substitution theta so pattern matches fact."""
    theta = dict(theta)
    for p, f in zip(pattern, fact):
        if is_var(p):
            if p in theta and theta[p] != f:
                return None
            theta[p] = f
        elif p != f:
            return None
    return theta

def apply_subst(triple, theta):
    return tuple(theta.get(t, t) for t in triple)

# ---------------------------------------------------------------------------
# Forward-chaining reasoner with proof logging
# ---------------------------------------------------------------------------
proof = {f: ("GIVEN", []) for f in facts}
step = 0
changed = True
while changed:
    changed = False
    for rid, ants, cons in rules:
        # collect candidate matches for each antecedent
        matches = [[] for _ in ants]
        for i, ant in enumerate(ants):
            for fact in facts:
                sub = unify(ant, fact, {})
                if sub is not None:
                    matches[i].append((fact, sub))

        # try all combinations of antecedent matches
        for combo in product(*matches):
            subst, used = {}, []
            consistent = True
            for fact, sub in combo:
                used.append(fact)
                for v, val in sub.items():
                    if v in subst and subst[v] != val:
                        consistent = False
                        break
                    subst[v] = val
                if not consistent:
                    break
            if not consistent:
                continue
            new_fact = apply_subst(cons, subst)
            if new_fact not in facts:
                step += 1
                facts.add(new_fact)
                proof[new_fact] = (rid, deepcopy(used))
                print(f"Step {step:02}: {new_fact[0]} {new_fact[1]} {new_fact[2]}   "
                      f"(by {rid} using {used})")
                changed = True

print("\n=== Fix-point reached ===")
goal = ("GIRL", "a", "WITCH")
print(f"{'PROVED' if goal in facts else 'NOT PROVED'}: {goal}")

