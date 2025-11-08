#!/usr/bin/env python3
"""
Socrates Syllogism — ARC style (Answer / Reason / Check), self-contained

Answer:
    What conclusions can we derive about Socrates?

Reason why:
    Classic syllogism:
        1) All humans are mortal.          (subclass(Human, Mortal))
        2) Socrates is a human.            (isa(Socrates, Human))
        Therefore Socrates is mortal.      (via rule: subclass + isa ⇒ isa)

Check (harness):
    - Independent forward-chaining computes the same closure as our tiny prover.
    - We verify that 'isa(Socrates, Mortal)' is among the answers and that every
      derived answer has a one-step justification from the facts and rule.
No imports; tiny unifier + backtracking proof search.
"""

# ---------------- facts & a single rule ----------------
facts = [
    ('isa', 'Socrates', 'Human'),
    ('subclass', 'Human', 'Mortal'),
]

rules = [
    {
        'name': 'subclass_rule',
        'premises': [('subclass', '?A', '?B'), ('isa', '?S', '?A')],
        'consequent': ('isa', '?S', '?B'),
    }
]

# -------------- utilities: variables, substitute, unify --------------
def is_var(x):
    return isinstance(x, str) and x.startswith('?')

def subst(term, env):
    """Apply env to term (supporting tuples)."""
    if is_var(term):
        return subst(env[term], env) if term in env else term
    if isinstance(term, tuple):
        return tuple(subst(t, env) for t in term)
    return term

def unify(a, b, env):
    """Unify a and b under env; return extended env or None."""
    a, b = subst(a, env), subst(b, env)
    if a == b:
        return env
    if is_var(a):
        new = env.copy()
        new[a] = b
        return new
    if is_var(b):
        return unify(b, a, env)
    if isinstance(a, tuple) and isinstance(b, tuple) and len(a) == len(b):
        for ai, bi in zip(a, b):
            env = unify(ai, bi, env)
            if env is None:
                return None
        return env
    return None

# -------------- tiny prover (depth-first with backtracking) --------------
def prove(goal):
    """Enumerate environments that satisfy the (single) goal."""
    solutions = []

    def and_prove(goals, env):
        if not goals:
            solutions.append(env)
            return
        head, *tail = goals
        for env2 in or_prove(head, env):
            and_prove(tail, env2)

    def or_prove(g, env):
        g = subst(g, env)
        # facts
        for f in facts:
            e = unify(f, g, env.copy())
            if e is not None:
                yield e
        # rules
        for r in rules:
            e = unify(r['consequent'], g, env.copy())
            if e is not None:
                def prove_premises(premises, e2):
                    if not premises:
                        yield e2
                        return
                    h, *t = premises
                    for e3 in or_prove(h, e2):
                        yield from prove_premises(t, e3)
                yield from prove_premises(r['premises'], e)

    and_prove([goal], {})
    return solutions

# -------------- pretty proof for a specific S, C --------------
def show_short_proof(s, c):
    """Print a shortest chain S → ... → C using subclass, if it exists."""
    # collect subclass edges and the starting type for s
    edges = {}
    start_types = []
    for k, a, b in facts:
        if k == 'subclass':
            edges.setdefault(a, set()).add(b)
        elif k == 'isa' and a == s:
            start_types.append(b)

    # direct fact?
    if ('isa', s, c) in facts:
        print(f"    ✓ fact: isa({s}, {c})")
        return True

    # simple BFS without imports
    visited = set()
    queue = []
    for t in start_types:
        queue.append((t, [t]))
        visited.add(t)

    while queue:
        t, path = queue.pop(0)  # pop front
        if t in edges:
            for u in edges[t]:
                if u in visited:
                    continue
                new_path = path + [u]
                if u == c:
                    chain = [s] + new_path
                    print("    ✓ rule: " + " → ".join(chain))
                    return True
                visited.add(u)
                queue.append((u, new_path))

    print("    (no chain found)")
    return False

# -------------- forward-chaining closure for harness --------------
def forward_closure():
    """Compute all isa(S, C) entailed by facts via the single rule."""
    isa = {(s, c) for (k, s, c) in facts if k == 'isa'}
    subclass = {(a, b) for (k, a, b) in facts if k == 'subclass'}
    changed = True
    while changed:
        changed = False
        for (s, a) in list(isa):
            for (a2, b) in subclass:
                if a2 == a and (s, b) not in isa:
                    isa.add((s, b))
                    changed = True
    return isa

# -------------- ARC: Answer / Reason / Check --------------
def print_answer():
    print("Answer")
    print("======")
    goal = ('isa', '?S', '?C')
    envs = prove(goal)
    answers = sorted({(e.get('?S', None), e.get('?C', None)) for e in envs})
    # only show bindings where both are concrete
    answers = [(s, c) for (s, c) in answers if isinstance(s, str) and isinstance(c, str) and not s.startswith('?') and not c.startswith('?')]
    print("All solutions to ?- isa(S, C):")
    for s, c in answers:
        print(f"  S = {s:8s}  C = {c}")
    # Highlight the classic one
    print("\nSyllogism instance:")
    ok = show_short_proof('Socrates', 'Mortal')
    if not ok:
        print("    (expected chain missing)")

def print_reason():
    print("\nReason why")
    print("==========")
    print("We assume a single inference rule:")
    print("  subclass(A, B) & isa(S, A)  ⇒  isa(S, B)")
    print("With facts:")
    print("  isa(Socrates, Human)")
    print("  subclass(Human, Mortal)")
    print("Instantiating A=Human, B=Mortal, S=Socrates yields isa(Socrates, Mortal).")

def print_check():
    print("\nCheck (harness)")
    print("===============")
    # 1) Closure by forward chaining equals solver answers
    closure = forward_closure()
    envs = prove(('isa', '?S', '?C'))
    solver = {(e['?S'], e['?C']) for e in envs if '?S' in e and '?C' in e}
    same = solver == closure
    print(f"closure == solver ? {same}")
    if not same:
        print(f"  closure: {sorted(closure)}")
        print(f"  solver : {sorted(solver)}")

    # 2) Expected answers present
    expected = {('Socrates', 'Human'), ('Socrates', 'Mortal')}
    ok_expected = expected.issubset(closure)
    print(f"contains expected {{Socrates→Human, Socrates→Mortal}} ? {ok_expected}")

    # 3) Each non-fact answer is justified by one rule instance
    subclass = {(a, b) for (k, a, b) in facts if k == 'subclass'}
    isa_facts = {(s, c) for (k, s, c) in facts if k == 'isa'}
    justification_ok = True
    for (s, c) in closure:
        if (s, c) in isa_facts:
            continue
        # must be some a with (s,a) in isa_facts and (a,c) in subclass
        found = any((s, a) in isa_facts and (a, c) in subclass for a in {a for (s2, a) in isa_facts if s2 == s})
        if not found:
            justification_ok = False
            print(f"  missing justification for isa({s}, {c})")
    print(f"one-step justification for derived answers ? {justification_ok}")

if __name__ == "__main__":
    print_answer()
    print_reason()
    print_check()

