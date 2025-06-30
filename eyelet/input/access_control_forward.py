from itertools import product
from copy import deepcopy

# ────────────────────────────────────────────────────────────────────────────
# Initial facts
# ────────────────────────────────────────────────────────────────────────────
facts = {
    # Property classifications
    ("inherits_from", "a", "TRANSITIVE_RELATION"),
    ("communicates_with", "a", "SYMMETRIC_RELATION"),

    # Role hierarchy
    ("ADMIN",   "inherits_from", "MANAGER"),
    ("MANAGER", "inherits_from", "EMPLOYEE"),

    # Role-to-permission mapping
    ("ADMIN",   "has_permission", "READ_CONFIDENTIAL"),
    ("MANAGER", "has_permission", "READ_PRIVATE"),
    ("EMPLOYEE","has_permission", "READ_PUBLIC"),

    # Resource protection
    ("resource1", "requires_permission", "READ_CONFIDENTIAL"),
    ("resource2", "requires_permission", "READ_PRIVATE"),
    ("resource3", "requires_permission", "READ_PUBLIC"),

    # User role assignments
    ("Alice",   "has_role", "MANAGER"),
    ("Bob",     "has_role", "EMPLOYEE"),
    ("Charlie", "has_role", "ADMIN"),

    # Social edge
    ("Alice", "communicates_with", "Charlie"),
}

# ────────────────────────────────────────────────────────────────────────────
# Rules  (rule_id, antecedents, consequent)
# ────────────────────────────────────────────────────────────────────────────
rules = [
    # Generic symmetry
    ("R-sym",
     [("?p", "a", "SYMMETRIC_RELATION"),
      ("?x", "?p", "?y")],
     ("?y", "?p", "?x")),

    # Generic transitivity
    ("R-trans",
     [("?p", "a", "TRANSITIVE_RELATION"),
      ("?x", "?p", "?y"),
      ("?y", "?p", "?z")],
     ("?x", "?p", "?z")),

    # User inherits ancestor roles
    ("R-role-inherit",
     [("?u", "has_role", "?r1"),
      ("?r1", "inherits_from", "?r2")],
     ("?u", "has_role", "?r2")),

    # Role grants permission to user
    ("R-perms",
     [("?u", "has_role", "?r"),
      ("?r", "has_permission", "?perm")],
     ("?u", "has_permission", "?perm")),

    # Permission allows access
    ("R-access",
     [("?u", "has_permission", "?perm"),
      ("?res", "requires_permission", "?perm")],
     ("?u", "can_access", "?res")),
]

# ────────────────────────────────────────────────────────────────────────────
# Tiny deterministic forward-chaining engine
# ────────────────────────────────────────────────────────────────────────────
def is_var(t): return isinstance(t, str) and t.startswith("?")

def unify(pat, fact, theta):
    theta = dict(theta)
    for p, f in zip(pat, fact):
        if is_var(p):
            if p in theta and theta[p] != f:
                return None
            theta[p] = f
        elif p != f:
            return None
    return theta

def subst(triple, theta): return tuple(theta.get(t, t) for t in triple)

proof = {f: ("GIVEN", []) for f in facts}
step = 0
changed = True
while changed:
    changed = False
    for rid, ants, cons in sorted(rules, key=lambda r: r[0]):        # deterministic
        matches = [[] for _ in ants]
        for i, ant in enumerate(ants):
            for fact in sorted(facts):                               # deterministic
                sub = unify(ant, fact, {})
                if sub: matches[i].append((fact, sub))

        for combo in product(*matches):
            theta, prem, ok = {}, [], True
            for fact, sub in combo:
                prem.append(fact)
                for v, val in sub.items():
                    if v in theta and theta[v] != val:
                        ok = False; break
                    theta[v] = val
                if not ok: break
            if not ok: continue

            derived = subst(cons, theta)
            if derived not in facts:
                facts.add(derived)
                step += 1
                proof[derived] = (rid, deepcopy(prem))
                print(f"Step {step:02}: {derived}   (by {rid} using {prem})")
                changed = True

# ────────────────────────────────────────────────────────────────────────────
# Full fix-point
# ────────────────────────────────────────────────────────────────────────────
print("\n=== Fix-point reached – full closure ===")
for t in sorted(facts):
    print(t)

# Selected queries
queries = [
    ("Alice",   "can_access", "resource2"),   # True
    ("Alice",   "has_role", "EMPLOYEE"),      # True via inheritance
    ("Charlie", "can_access", "resource1"),   # True
    ("Bob",     "can_access", "resource1"),   # False
    ("Charlie", "communicates_with", "Alice") # True via symmetry
]
print("\n=== Selected queries ===")
for q in queries:
    print(f"{'✓' if q in facts else '✗'} {q}")

