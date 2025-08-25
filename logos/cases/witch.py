#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Monty Python — Witch proof (ARC: Answer / Reason / Check), self-contained

Query:
  ?- isa(S, WITCH).

We use the classic toy KB:
  Facts:
    isa(GIRL, WOMAN)
    isa(DUCK, FLOATS)
    sameweight(DUCK, GIRL)

  Rules:
    R1: isa(X, BURNS) ∧ isa(X, WOMAN)           → isa(X, WITCH)
    R2: isa(X, ISMADEOFWOOD)                    → isa(X, BURNS)
    R3: isa(X, FLOATS)                          → isa(X, ISMADEOFWOOD)
    R4: isa(X, FLOATS) ∧ sameweight(X, Y)       → isa(Y, FLOATS)

The script forward-chains while recording *why* each derived fact holds, then
prints the witches and a readable backward proof.
"""

# ───────────────────────── 0) KB ─────────────────────────
facts = [
    ('isa', 'GIRL', 'WOMAN'),
    ('isa', 'DUCK', 'FLOATS'),
    ('sameweight', 'DUCK', 'GIRL'),
]

rules = [
    { 'name': 'R1_burns_woman_witch',
      'premises': [('isa', '?x', 'BURNS'), ('isa', '?x', 'WOMAN')],
      'consequent': ('isa', '?x', 'WITCH') },

    { 'name': 'R2_wood_burns',
      'premises': [('isa', '?x', 'ISMADEOFWOOD')],
      'consequent': ('isa', '?x', 'BURNS') },

    { 'name': 'R3_floats_wood',
      'premises': [('isa', '?x', 'FLOATS')],
      'consequent': ('isa', '?x', 'ISMADEOFWOOD') },

    { 'name': 'R4_floats_sameweight',
      'premises': [('isa', '?x', 'FLOATS'), ('sameweight', '?x', '?y')],
      'consequent': ('isa', '?y', 'FLOATS') },
]

# ─────────────────── 1) Tiny matcher / unifier ───────────────────
def is_var(t): return isinstance(t, str) and t.startswith('?')

def substitute(term, env, seen=None):
    if seen is None: seen = set()
    if is_var(term):
        if term in seen: return term
        seen.add(term)
        return substitute(env.get(term, term), env, seen)
    if isinstance(term, tuple):
        return tuple(substitute(t, env, set()) for t in term)
    return term

def unify(pat, fact, env):
    pat = substitute(pat, env); fact = substitute(fact, env)
    if pat == fact: return env
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

# ─────────────── 2) Forward chaining with justifications ───────────────
def derive_closure(facts, rules):
    derived = set(facts)
    why = {f: ('fact', []) for f in derived}
    changed = True
    while changed:
        changed = False
        for rule in rules:
            envs = [({}, [])]  # list of (env, supports)
            for prem in rule['premises']:
                next_envs = []
                for env, supports in envs:
                    patt = substitute(prem, env)
                    for f in derived:
                        e2 = unify(patt, f, env.copy())
                        if e2 is not None:
                            next_envs.append((e2, supports + [f]))
                envs = next_envs
            for env, supports in envs:
                cons = substitute(rule['consequent'], env)
                if cons not in derived:
                    derived.add(cons)
                    why[cons] = (rule['name'], supports)
                    changed = True
    return derived, why

DERIVED, WHY = derive_closure(facts, rules)

# ───────────────────────── 3) Pretty proof ─────────────────────────
def tstr(t):
    k = t[0]
    if k == 'isa': return f"{t[2]}({t[1]})"
    if k == 'sameweight': return f"SAMEWEIGHT({t[1]}, {t[2]})"
    return str(t)

def print_proof(goal):
    step = {'n': 0}
    def rec(fact, depth):
        step['n'] += 1
        indent = ' ' * depth
        print(f"{indent}Step {step['n']:02d}: prove {tstr(fact)}")
        rule, supports = WHY.get(fact, ('fact', []))
        if rule == 'fact':
            print(f"{indent}  ✓ fact")
        else:
            print(f"{indent}  → via {rule}")
            for s in supports:
                rec(s, depth + 1)
    rec(goal, 0)
    print("✔ PROVED\n")

# ─────────────────────────── ARC: Answer ───────────────────────────
def print_answer():
    print("Answer")
    print("======")
    witches = sorted(s for (p,s,c) in DERIVED if p == 'isa' and c == 'WITCH')
    if witches:
        print("All S such that isa(S, WITCH):")
        for s in witches:
            print(f"  S = {s}")
    else:
        print("No witches found.")

    # Show a readable proof for GIRL (the interesting case)
    if 'GIRL' in witches:
        print("\nProof for S = GIRL:\n")
        print_proof(('isa', 'GIRL', 'WITCH'))

# ──────────────────────── ARC: Reason why ──────────────────────────
def print_reason():
    print("\nReason why")
    print("==========")
    print("Chain of implications (one possible route):")
    print("  isa(DUCK, FLOATS)  &  sameweight(DUCK, GIRL)")
    print("    ⇒ (R4) isa(GIRL, FLOATS)")
    print("    ⇒ (R3) isa(GIRL, ISMADEOFWOOD)")
    print("    ⇒ (R2) isa(GIRL, BURNS)")
    print("  plus the fact isa(GIRL, WOMAN)")
    print("    ⇒ (R1) isa(GIRL, WITCH).")

# ────────────────────── ARC: Check (harness) ───────────────────────
def print_check():
    print("\nCheck (harness)")
    print("===============")
    ok_all = True

    # 1) Expected witch set is exactly {GIRL}
    witches = {s for (p,s,c) in DERIVED if p == 'isa' and c == 'WITCH'}
    ok = witches == {'GIRL'}
    print(f"Witch set == {{GIRL}} ? {ok}"); ok_all &= ok

    # 2) Every non-fact has a justification that bottoms out in facts
    def bottoms_out(f):
        seen = set()
        def dfs(x):
            if x in seen: return True  # guard cycles (shouldn't happen here)
            seen.add(x)
            rule, sups = WHY.get(x, ('fact', []))
            if rule == 'fact': return True
            return all(dfs(s) for s in sups)
        return dfs(f)
    all_just = all(bottoms_out(f) for f in DERIVED)
    print(f"All derived facts justified by a finite proof tree? {all_just}")
    ok_all &= all_just

    # 3) Idempotence of closure
    again, _ = derive_closure(DERIVED, rules)
    fixed = again == DERIVED
    print(f"Forward closure is a fixed point? {fixed}")
    ok_all &= fixed

    # 4) Sensitivity test: remove sameweight ⇒ GIRL is no longer forced to FLOAT
    facts2 = [f for f in facts if f[0] != 'sameweight']
    d2, _ = derive_closure(facts2, rules)
    witches2 = {s for (p,s,c) in d2 if p == 'isa' and c == 'WITCH'}
    ok = witches2 == set()
    print(f"Without SAMEWEIGHT, no witch derived? {ok}")
    ok_all &= ok

    print(f"\nAll checks passed? {ok_all}")

# ────────────────────────────── Main ───────────────────────────────
if __name__ == "__main__":
    print_answer()
    print_reason()
    print_check()

