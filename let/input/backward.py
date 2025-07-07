"""
Python rewrite of the N3 backward-rule example
from https://www.w3.org/2000/10/swap/doc/tutorial-1.pdf (p. 17).

Rule in N3:
    { ?X :moreInterestingThan ?Y. } <= { ?X math:greaterThan ?Y. }.

Informally:
    X is :moreInterestingThan Y  ⇐  X math:greaterThan Y
    (i.e. X is more interesting if X > Y)
"""

from itertools import count

# ----------------------------------------------------------------------------
# One backward rule, written declaratively
#     ?X :moreInterestingThan ?Y  ⇐  ?X math:greaterThan ?Y
# ----------------------------------------------------------------------------
RULE_ID = "R-moreInteresting"
RULE_BODY = ("?X", "math:greaterThan", "?Y")
RULE_HEAD = ("?X", "moreInterestingThan", "?Y")

# ----------------------------------------------------------------------------
# A tiny backward‑chaining engine with proof printing
# ----------------------------------------------------------------------------
step = count(1)
def indent(d): return "  "*d

def unify(pat, fact, theta):
    """Positional unify with constants or vars starting with '?'. Return subst."""
    theta = dict(theta)
    for p,f in zip(pat,fact):
        if isinstance(p,str) and p.startswith("?"):
            if p in theta and theta[p] != f:
                return None
            theta[p] = f
        elif p != f:
            return None
    return theta

def subst(triple, theta):
    return tuple(theta.get(t,t) for t in triple)

def prove(goal, theta, depth):
    g = subst(goal, theta)
    tag = next(step)
    print(f"{indent(depth)}Step {tag:02}: prove {g}")

    subj,pred,obj = g
    # Built‑in evaluation for math:greaterThan
    if pred == "math:greaterThan":
        if isinstance(subj,(int,float)) and isinstance(obj,(int,float)) and subj>obj:
            print(f"{indent(depth)}  ✓ built‑in check {subj} > {obj}")
            yield theta
        else:
            print(f"{indent(depth)}  ✗ built‑in check fails")
        return

    # Apply the only rule if head matches
    θ_head = unify(RULE_HEAD,g,{})
    if θ_head is not None:
        head_inst = subst(RULE_HEAD,θ_head)
        print(f"{indent(depth)}  → via {RULE_ID}")
        for θ_final in prove(RULE_BODY,{**theta,**θ_head},depth+1):
            yield θ_final

def ask(goal):
    print(f"\n=== Query {goal} ===")
    found=False
    for θ in prove(goal,{},0):
        print(f"✔ PROVED {subst(goal,θ)}\n")
        found=True
        break
    if not found:
        print(f"✗ NOT PROVED {goal}\n")

# ----------------------------------------------------------------------------
# Run the sample query from the tutorial (and an extra failing one)
# ----------------------------------------------------------------------------
ask( (5,"moreInterestingThan",3) )
ask( (3,"moreInterestingThan",5) )

