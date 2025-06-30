# Fix provenance for rule ids: ensure every rule id maps even seed rules.

# Re‑run with provenance adjustment.

from itertools import product, count
from copy import deepcopy

facts = {
    (":Minka", "a", ":Cat"),
    (":Charly", "a", ":Dog"),
}

rules = [
    {
        "id": "R1-cat‑makes‑rule",
        "premises": [("?x", "a", ":Cat")],
        "conclusions": [
            {
                "NEW_RULE": {
                    "premises": [("?y", "a", ":Dog")],
                    "conclusions": [(":test", ":is", "true")],
                    "template_name": "Rgen-from-cat"
                }
            }
        ],
    }
]

rule_counter = count(1)
def fresh_rule_id(template): return f"{template}-{next(rule_counter)}"

def is_var(t): return isinstance(t,str) and t.startswith("?")
def unify(p,fact,θ):
    θ = dict(θ)
    for pterm,fterm in zip(p,fact):
        if is_var(pterm):
            if pterm in θ and θ[pterm]!=fterm: return None
            θ[pterm]=fterm
        elif pterm!=fterm: return None
    return θ
def substitute(term,θ):
    if isinstance(term,tuple):
        return tuple(substitute(t,θ) for t in term)
    return θ.get(term,term)

proof = {f:("GIVEN",[]) for f in facts}
# record seed rules as GIVEN so that provenance lookup works
for r in rules:
    proof[r["id"]] = ("GIVEN", [])

step=0
changed=True
while changed:
    changed=False
    for rule in list(rules):
        # build match list
        combos=[]
        for prem in rule["premises"]:
            matches=[]
            for fact in facts:
                θ=unify(prem,fact,{})
                if θ is not None:
                    matches.append((fact,θ))
            combos.append(matches)
        for combo in product(*combos):
            θ={}
            premises=[]
            ok=True
            for fact,θi in combo:
                premises.append(fact)
                for v,val in θi.items():
                    if v in θ and θ[v]!=val: ok=False;break
                    θ[v]=val
                if not ok: break
            if not ok: continue
            for concl in rule["conclusions"]:
                if isinstance(concl,dict) and "NEW_RULE" in concl:
                    tpl=concl["NEW_RULE"]
                    new_rule={
                        "id": fresh_rule_id(tpl["template_name"]),
                        "premises":[substitute(p,θ) for p in tpl["premises"]],
                        "conclusions":[substitute(c,θ) for c in tpl["conclusions"]],
                    }
                    if not any(r["premises"]==new_rule["premises"] and r["conclusions"]==new_rule["conclusions"] for r in rules):
                        rules.append(new_rule)
                        proof[new_rule["id"]] = (rule["id"], deepcopy(premises))
                        step+=1
                        print(f"Step {step:02}: new rule {new_rule['id']}   (by {rule['id']} using {premises})")
                        changed=True
                else:
                    fact=substitute(concl,θ)
                    if fact not in facts:
                        facts.add(fact)
                        proof[fact]=(rule["id"], deepcopy(premises))
                        step+=1
                        print(f"Step {step:02}: {fact}   (by {rule['id']} using {premises})")
                        changed=True

query=(":test",":is","true")
print("\n=== Query:",query,"===")
if query in facts:
    print("✔ PROVED\n")
    chain=[]
    node=query
    visited=set()
    while True:
        src,prems=proof[node]
        chain.append((node,src,prems))
        if src=="GIVEN" or src in visited:
            break
        visited.add(src)
        node=src
    chain.reverse()
    for item,src,prems in chain:
        if isinstance(item,tuple):
            print(f"fact {item}  ← {src} via {prems}")
        else:
            print(f"rule {item}  ← {src} via {prems}")
else:
    print("✗ NOT PROVED")

