# Robust backward-chainer with rule generation and variable bindings.

from itertools import product
from copy import deepcopy

# facts
facts = {
    (":Minka","a",":Cat"),
    (":Charly","a",":Dog"),
}

# meta rule definition
meta_rules = [
    {
        "id":"R1-cat-makes-rule",
        "premises":[("?x","a",":Cat")],
        "gen_rule": {
            "id":"Rgen-from-cat",
            "premises":[("?y","a",":Dog")],
            "conclusions":[(":test",":is","true")]
        }
    }
]

# ordinary rules list (start empty, will generate)
rules = []

def is_var(t): return isinstance(t,str) and t.startswith("?")

def unify(pat, fact, θ):
    θ = dict(θ)
    for p,f in zip(pat,fact):
        if is_var(p):
            if p in θ and θ[p]!=f: return None
            θ[p]=f
        elif p!=f:
            return None
    return θ

def subst(term, θ):
    if isinstance(term, tuple):
        return tuple(subst(x,θ) for x in term)
    return θ.get(term,term)

# derive rules from meta rules once
for mr in meta_rules:
    # find substitutions for meta premises
    combos=[[]]
    for prem in mr["premises"]:
        new=[]
        for f in facts:
            θ=unify(prem,f,{})
            if θ is not None:
                for c in combos:
                    merged=dict(c,**θ)
                    new.append(merged)
        combos=new
    for θ in combos:
        new_rule={
            "id":mr["gen_rule"]["id"],
            "premises":[subst(p,θ) for p in mr["gen_rule"]["premises"]],
            "conclusions":[subst(c,θ) for c in mr["gen_rule"]["conclusions"]],
            "origin":mr["id"]
        }
        if new_rule not in rules:
            rules.append(new_rule)

# backward prover with substitution
step=1
def indent(d): return "  "*d

def bc(goal, θ, depth, seen):
    global step
    goal_inst=subst(goal,θ)
    print(f"{indent(depth)}Step {step:02}: prove {goal_inst}")
    step+=1

    # try facts
    for f in facts:
        θ2=unify(goal_inst,f,{})
        if θ2 is not None:
            θ_total={**θ,**θ2}
            print(f"{indent(depth)}  ✓ fact {f}")
            yield θ_total
            return

    # try ordinary rules
    for r in rules:
        for head in r["conclusions"]:
            θ_head=unify(head,goal_inst,{})
            if θ_head is None: continue
            key=(r["id"],tuple(subst(head,θ_head)))
            if key in seen: continue
            print(f"{indent(depth)}  → via {r['id']}")
            θ_new={**θ,**θ_head}
            def prove_seq(i, θ_curr):
                if i==len(r["premises"]):
                    yield θ_curr
                else:
                    for θ_next in bc(r["premises"][i], θ_curr, depth+1, seen|{key}):
                        yield from prove_seq(i+1, θ_next)
            for θ_final in prove_seq(0, θ_new):
                yield θ_final
                return

# query
query=(":test",":is","true")
print(f"\n=== Proving {query} ===")
for θ in bc(query,{},0,frozenset()):
    print("✔ PROVED",subst(query,θ))
    break

