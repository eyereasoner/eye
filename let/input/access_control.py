# Backward‑chaining access‑control reasoner with proof trace
#
# This mirrors the forward‑chaining "access_control_forward.py"
# but drives the search from a query and prints every inference step.

from itertools import count
from copy import deepcopy

# ───────────────────────── Seed facts ─────────────────────────
facts = {
    # property meta
    ("inherits_from","a","TRANSITIVE_RELATION"),
    ("before","a","TRANSITIVE_RELATION"),
    ("communicates_with","a","SYMMETRIC_RELATION"),

    # dates & ordering
    ("DATE_TODAY","a","DATE"),
    ("DATE_2025-12-31","a","DATE"),
    ("DATE_2030-01-01","a","DATE"),
    ("DATE_TODAY","before","DATE_2025-12-31"),
    ("DATE_2025-12-31","before","DATE_2030-01-01"),

    # role hierarchy
    ("ADMIN","inherits_from","MANAGER"),
    ("MANAGER","inherits_from","EMPLOYEE"),

    # role → simple permissions
    ("ADMIN","has_permission","READ_CONFIDENTIAL"),
    ("MANAGER","has_permission","READ_PRIVATE"),
    ("EMPLOYEE","has_permission","READ_PUBLIC"),

    # permission objects
    ("PERM_CONF_ANY","permits_action","READ_CONFIDENTIAL"),
    ("PERM_CONF_ANY","permits_purpose","ANY"),
    ("PERM_CONF_ANY","valid_until","DATE_2030-01-01"),

    ("PERM_PRIV_ACC","permits_action","READ_PRIVATE"),
    ("PERM_PRIV_ACC","permits_purpose","ACCOUNTING"),
    ("PERM_PRIV_ACC","valid_until","DATE_2025-12-31"),

    ("PERM_PUB_ANY","permits_action","READ_PUBLIC"),
    ("PERM_PUB_ANY","permits_purpose","ANY"),
    ("PERM_PUB_ANY","valid_until","DATE_2030-01-01"),

    # role ↦ permission object
    ("ADMIN","has_permission_obj","PERM_CONF_ANY"),
    ("MANAGER","has_permission_obj","PERM_PRIV_ACC"),
    ("EMPLOYEE","has_permission_obj","PERM_PUB_ANY"),

    # users
    ("Alice","has_role","MANAGER"),
    ("Bob","has_role","EMPLOYEE"),
    ("Charlie","has_role","ADMIN"),

    # resources
    ("resource1","requires_permission","PERM_CONF_ANY"),
    ("resource1","usage_purpose","ANY"),

    ("resource2","requires_permission","PERM_PRIV_ACC"),
    ("resource2","usage_purpose","ACCOUNTING"),

    ("resource3","requires_permission","PERM_PUB_ANY"),
    ("resource3","usage_purpose","ANY"),

    # social
    ("Alice","communicates_with","Charlie"),
}

# ───────────────────────── Rules ─────────────────────────
raw_rules = [
    # symmetry
    ("?y ?p ?x",
     ["?p a SYMMETRIC_RELATION",
      "?x ?p ?y"],
     "R-sym"),

    # transitive
    ("?x ?p ?z",
     ["?p a TRANSITIVE_RELATION",
      "?x ?p ?y",
      "?y ?p ?z"],
     "R-trans"),

    # role inheritance: user gets ancestor role
    ("?u has_role ?r2",
     ["?u has_role ?r1",
      "?r1 inherits_from ?r2"],
     "R-role-inherit"),

    # role→permission object to user
    ("?u has_permission_obj ?perm",
     ["?u has_role ?r",
      "?r has_permission_obj ?perm"],
     "R-perm-assign"),

    # permission currency
    ("?perm is_current true",
     ["?perm valid_until ?exp",
      "DATE_TODAY before ?exp"],
     "R-perm-current"),

    # user current permission
    ("?u has_current_permission ?perm",
     ["?u has_permission_obj ?perm",
      "?perm is_current true"],
     "R-user-current-perm"),

    # purpose‑matching access
    ("?u can_access ?res",
     ["?u has_current_permission ?perm",
      "?perm permits_purpose ?purpose",
      "?res requires_permission ?perm",
      "?res usage_purpose ?purpose"],
     "R-access-purpose"),

    # ANY purpose
    ("?u can_access ?res",
     ["?u has_current_permission ?perm",
      "?perm permits_purpose ANY",
      "?res requires_permission ?perm"],
     "R-access-any"),
]

# Parse rule strings -> tuple triples
def parse_triple(t):
    return tuple(t.split())

rules = []
for head, body_list, rid in raw_rules:
    rules.append({
        "id": rid,
        "head": parse_triple(head),
        "body": [parse_triple(b) for b in body_list],
    })

# ───────────────────────── Unification ----------

def is_var(x): return isinstance(x,str) and x.startswith("?")

def unify(pat, fact, θ=None):
    θ=dict(θ or {})
    for p,f in zip(pat,fact):
        if is_var(p):
            if p in θ and θ[p]!=f: return None
            θ[p]=f
        elif p!=f: return None
    return θ

def subst(triple, θ):
    return tuple(θ.get(t,t) for t in triple)

# ───────────────────────── Backward engine -------
step=count(1)
def indent(d): return "  "*d

def bc(goal, θ, depth, seen):
    g=subst(goal,θ)
    tag=next(step)
    print(f"{indent(depth)}Step {tag:02}: prove {g}")

    # try fact
    for f in facts:
        θ2=unify(g,f,{})
        if θ2 is not None:
            print(f"{indent(depth)}  ✓ fact")
            yield {**θ, **θ2}
            return

    # rules
    for rule in rules:
        θh=unify(rule["head"], g, {})
        if θh is None: continue
        instance=subst(rule["head"], θh)
        key=(rule["id"], instance)
        if key in seen: continue
        print(f"{indent(depth)}  → via {rule['id']}")
        θ_new={**θ, **θh}

        def prove_seq(i, θ_curr):
            if i==len(rule["body"]):
                yield θ_curr
            else:
                sub_goal=rule["body"][i]
                for θ_next in bc(subst(sub_goal, θ_curr), θ_curr, depth+1, seen|{key}):
                    yield from prove_seq(i+1, θ_next)

        for θ_final in prove_seq(0, θ_new):
            yield θ_final
            return

# ───────────────────────── Run sample queries ----
queries=[
    ("Alice","can_access","resource2"),      # expect ✓
    ("Bob","can_access","resource1"),        # expect ✗
    ("Bob","can_access","resource3"),        # expect ✓
    ("Charlie","can_access","resource1"),    # expect ✓
    ("Charlie","communicates_with","Alice"), # symmetry
]

for q in queries:
    print(f"\n=== Query {q} ===")
    success=False
    for θ in bc(q, {}, 0, frozenset()):
        success=True
        break
    print("✔ PROVED\n" if success else "✗ NOT PROVED\n")

