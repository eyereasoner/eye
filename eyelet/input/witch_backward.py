# Backward‑style proof for the “witch” example
# -------------------------------------------
#
# Query:  ?S a :WITCH.
#
# This script:
#   1.  Loads the Monty‑Python rules & facts.
#   2.  Forward‑chains once, *recording a justification* for every
#       derived fact.
#   3.  Walks those justifications backward to print a readable
#       proof trace for each individual that ends up being a witch.

# 1 ▸ KB ----------------------------------------------------------------------
facts = [
    ('isa', 'GIRL', 'WOMAN'),
    ('isa', 'DUCK', 'FLOATS'),
    ('sameweight', 'DUCK', 'GIRL'),
]

rules = [
    {
        'name': 'burns_woman_witch',                # BURNS(x) ∧ WOMAN(x) → WITCH(x)
        'premises': [('isa', '?x', 'BURNS'),
                     ('isa', '?x', 'WOMAN')],
        'consequent': ('isa', '?x', 'WITCH')
    },
    {
        'name': 'wood_burns',                       # ISMADEOFWOOD(x) → BURNS(x)
        'premises': [('isa', '?x', 'ISMADEOFWOOD')],
        'consequent': ('isa', '?x', 'BURNS')
    },
    {
        'name': 'floats_wood',                      # FLOATS(x) → ISMADEOFWOOD(x)
        'premises': [('isa', '?x', 'FLOATS')],
        'consequent': ('isa', '?x', 'ISMADEOFWOOD')
    },
    {
        'name': 'floats_sameweight',                # FLOATS(x) ∧ SAMEWEIGHT(x,y) → FLOATS(y)
        'premises': [('isa', '?x', 'FLOATS'),
                     ('sameweight', '?x', '?y')],
        'consequent': ('isa', '?y', 'FLOATS')
    },
]

# 2 ▸  Mini pattern‑matcher utilities ----------------------------------------
def is_var(t): return isinstance(t, str) and t.startswith('?')

def substitute(term, env, seen=None):
    """Replace variables in *term* using *env*, guarding against cycles."""
    if seen is None:
        seen = set()
    if is_var(term):
        if term in seen:
            return term
        seen.add(term)
        return substitute(env.get(term, term), env, seen)
    if isinstance(term, tuple):
        return tuple(substitute(t, env, set()) for t in term)
    return term

def unify(pat, fact, env):
    """Return extended env if pat ∪ env unifies with fact, else None."""
    pat = substitute(pat, env)
    fact = substitute(fact, env)
    if pat == fact:
        return env
    if is_var(pat):
        new = env.copy(); new[pat] = fact; return new
    if is_var(fact):
        return unify(fact, pat, env)
    if isinstance(pat, tuple) and isinstance(fact, tuple) and len(pat) == len(fact):
        for pi, fi in zip(pat, fact):
            env = unify(pi, fi, env)
            if env is None: return None
        return env
    return None

# 3 ▸  Forward‑chaining with justification tracking --------------------------
derived = set(facts)
why = {f: ('fact', None) for f in facts}      # fact → (rule, [supporting facts])

changed = True
while changed:
    changed = False
    for rule in rules:
        envs = [({}, [])]                     # list of (env, supports)
        for prem in rule['premises']:
            next_envs = []
            for env, supports in envs:
                pattern = substitute(prem, env)
                for fact in derived:
                    env2 = unify(pattern, fact, env.copy())
                    if env2 is not None:
                        next_envs.append((env2, supports + [fact]))
            envs = next_envs
        for env, supports in envs:
            cons = substitute(rule['consequent'], env)
            if cons not in derived:
                derived.add(cons)
                why[cons] = (rule['name'], supports)
                changed = True

# 4 ▸  Pretty proof printer ---------------------------------------------------
def term_str(t):
    if t[0] == 'isa':
        return f"{t[2]}({t[1]})"
    if t[0] == 'sameweight':
        return f"SAMEWEIGHT({t[1]}, {t[2]})"
    return str(t)

def print_proof(goal):
    step = {'n': 0}
    def rec(fact, depth):
        indent = '  ' * depth
        step['n'] += 1
        print(f"{indent}Step {step['n']:02d}: prove {term_str(fact)}")
        rule, supports = why[fact]
        if rule == 'fact':
            print(f"{indent}  ✓ fact")
        else:
            print(f"{indent}  → via {rule}")
            for sup in supports:
                rec(sup, depth + 1)
    rec(goal, 0)
    print("✔ PROVED\n")

# 5 ▸  Show all witches -------------------------------------------------------
witches = [s for p, s, c in derived if p == 'isa' and c == 'WITCH']

print("=== All proofs for  ?- ?S a :WITCH ===\n")
for s in sorted(witches):
    print(f"--- Proof for S = {s} ---")
    print_proof(('isa', s, 'WITCH'))

