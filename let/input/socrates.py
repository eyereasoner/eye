# ------------------
# Socrates inference
# ------------------
facts = [
    ('isa', 'Socrates', 'Human'),
    ('subclass', 'Human', 'Mortal'),
]

rules = [
    {
        'name': 'subclass_rule',
        'premises': [('subclass', '?A', '?B'),
                     ('isa',       '?S', '?A')],
        'consequent': ('isa', '?S', '?B')
    }
]

# --- very small unification & helpers -----------
def is_var(t): return isinstance(t, str) and t.startswith('?')

def subst(t, env):
    if is_var(t):
        return subst(env[t], env) if t in env else t
    if isinstance(t, tuple):
        return tuple(subst(x, env) for x in t)
    return t

def unify(x, y, env):
    x, y = subst(x, env), subst(y, env)
    if x == y:                       return env
    if is_var(x):                    e=env.copy(); e[x]=y; return e
    if is_var(y):                    return unify(y, x, env)
    if isinstance(x, tuple) and len(x)==len(y):
        for xi, yi in zip(x, y):
            env = unify(xi, yi, env)
            if env is None: return None
        return env
    return None

def fmt(t):  # pretty printer for terms
    return f"{t[0]}({', '.join(map(str, t[1:]))})" if isinstance(t, tuple) else str(t)

# --- silent solver to enumerate answers ----------
def solve(goal):
    sol = []
    def and_prove(goals, env):
        if not goals:
            sol.append(env); return
        head, *tail = goals
        for env2 in or_prove(head, env):
            and_prove(tail, env2)
    def or_prove(g, env):
        g = subst(g, env)
        for f in facts:
            e = unify(f, g, env.copy())
            if e is not None: yield e
        for r in rules:
            e = unify(r['consequent'], g, env.copy())
            if e is not None:
                def prove_premises(prem, e2):
                    if not prem: yield e2; return
                    h,*t = prem
                    for e3 in or_prove(h, e2):
                        yield from prove_premises(t, e3)
                yield from prove_premises(r['premises'], e)
    and_prove([goal], {})
    return sol

# --- traced prover (pretty output) ----------------
def show_proof(S, C):
    step = {'n':0}
    def prove(g, env, d):
        indent='  '*d
        step['n']+=1
        print(f"{indent}Step {step['n']:02d}: prove {fmt(subst(g,env))}")
        for f in facts:
            e=unify(f, subst(g,env), env.copy())
            if e is not None:
                print(f"{indent}  ✓ fact")
                return True
        for r in rules:
            e=unify(r['consequent'], subst(g,env), env.copy())
            if e is not None:
                print(f"{indent}  → via {r['name']}")
                for p in r['premises']:
                    if not prove(p, e, d+1): break
                else: return True
        return False
    if prove(('isa', S, C), {}, 0):
        chain = f"{S} → {C}" if C=='Human' else f"{S} → Human → {C}"
        print("✔ PROVED\nShortest chain:", chain)
    else: print("✗ FAILED")
    
# ---------- run everything ------------------------
goal = ('isa', '?S', '?C')
answers = { (e['?S'], e['?C']) for e in solve(goal) }

print("=== All proofs for  ?- isa(S, C) ===\n")
for s,c in sorted(answers):
    print(f"--- Proof for S = {s}, C = {c} ---")
    show_proof(s, c); print()

