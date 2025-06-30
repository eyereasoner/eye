# usage_control_reasoner.py  (Python 3.x)

from itertools import product
from copy import deepcopy

# ░░░ 1. Seed facts  ░░░
facts = {
    # --- property meta ---
    ("inherits_from", "a", "TRANSITIVE_RELATION"),
    ("before",        "a", "TRANSITIVE_RELATION"),
    ("communicates_with", "a", "SYMMETRIC_RELATION"),

    # --- dates & ordering ---
    ("DATE_TODAY",        "a", "DATE"),
    ("DATE_2025-12-31",   "a", "DATE"),
    ("DATE_2030-01-01",   "a", "DATE"),
    ("DATE_TODAY", "before", "DATE_2025-12-31"),
    ("DATE_2025-12-31", "before", "DATE_2030-01-01"),

    # --- role hierarchy ---
    ("ADMIN",   "inherits_from", "MANAGER"),
    ("MANAGER", "inherits_from", "EMPLOYEE"),

    # --- permission objects ---
    ("PERM_READ_PUBLIC_ANY",      "permits_action",  "READ_PUBLIC"),
    ("PERM_READ_PUBLIC_ANY",      "permits_purpose", "ANY"),
    ("PERM_READ_PUBLIC_ANY",      "valid_until",     "DATE_2030-01-01"),

    ("PERM_READ_PRIVATE_ACCOUNTING", "permits_action",  "READ_PRIVATE"),
    ("PERM_READ_PRIVATE_ACCOUNTING", "permits_purpose", "ACCOUNTING"),
    ("PERM_READ_PRIVATE_ACCOUNTING", "valid_until",     "DATE_2025-12-31"),

    # --- role → permission object assignments ---
    ("EMPLOYEE", "has_permission_obj", "PERM_READ_PUBLIC_ANY"),
    ("MANAGER",  "has_permission_obj", "PERM_READ_PRIVATE_ACCOUNTING"),

    # --- users & initial roles ---
    ("Alice",   "has_role", "MANAGER"),
    ("Bob",     "has_role", "EMPLOYEE"),
    ("Charlie", "has_role", "ADMIN"),

    # --- resources with required permission & purpose ---
    ("public_doc",     "requires_permission", "PERM_READ_PUBLIC_ANY"),
    ("public_doc",     "usage_purpose",       "ANY"),
    ("accounting_doc", "requires_permission", "PERM_READ_PRIVATE_ACCOUNTING"),
    ("accounting_doc", "usage_purpose",       "ACCOUNTING"),

    # --- social edge (for symmetry demo) ---
    ("Alice", "communicates_with", "Charlie"),
}

# ░░░ 2. Rules ░░░
rules = [
    # symmetry & transitivity (generic, use predicate variable)
    ("R-sym",
     [("?p", "a", "SYMMETRIC_RELATION"), ("?x", "?p", "?y")],
     ("?y", "?p", "?x")),
    ("R-trans",
     [("?p", "a", "TRANSITIVE_RELATION"), ("?x", "?p", "?y"), ("?y", "?p", "?z")],
     ("?x", "?p", "?z")),
    # every DATE is reflexively before itself (inclusive ≤)
    ("R-date-reflex",
     [("?d", "a", "DATE")],
     ("?d", "before", "?d")),

    # role derivations
    ("R-role-inherit",
     [("?u", "has_role", "?r1"), ("?r1", "inherits_from", "?r2")],
     ("?u", "has_role", "?r2")),
    ("R-perm-assign",
     [("?u", "has_role", "?r"), ("?r", "has_permission_obj", "?perm")],
     ("?u", "has_permission_obj", "?perm")),

    # validity & user-current permissions
    ("R-perm-current",
     [("?perm", "valid_until", "?exp"), ("DATE_TODAY", "before", "?exp")],
     ("?perm", "is_current", "true")),
    ("R-user-current-perm",
     [("?u", "has_permission_obj", "?perm"), ("?perm", "is_current", "true")],
     ("?u", "has_current_permission", "?perm")),

    # access rules
    ("R-access-purpose",
     [("?u", "has_current_permission", "?perm"),
      ("?perm", "permits_purpose", "?purpose"),
      ("?res", "requires_permission", "?perm"),
      ("?res", "usage_purpose", "?purpose")],
     ("?u", "can_access", "?res")),
    ("R-access-any",
     [("?u", "has_current_permission", "?perm"),
      ("?perm", "permits_purpose", "ANY"),
      ("?res", "requires_permission", "?perm")],
     ("?u", "can_access", "?res")),
]

# ░░░ 3. Deterministic forward-chaining engine ░░░
def is_var(t): return isinstance(t, str) and t.startswith("?")
def unify(pat, fact, θ):
    θ = dict(θ)
    for p, f in zip(pat, fact):
        if is_var(p):
            if p in θ and θ[p] != f: return None
            θ[p] = f
        elif p != f: return None
    return θ
def subst(triple, θ): return tuple(θ.get(t, t) for t in triple)

proof, step, changed = {f: ("GIVEN", []) for f in facts}, 0, True
while changed:
    changed = False
    for rid, ants, cons in sorted(rules, key=lambda r: r[0]):        # stable order
        prem_sets = [[] for _ in ants]
        for i, ant in enumerate(ants):
            for fact in sorted(facts):
                σ = unify(ant, fact, {})
                if σ: prem_sets[i].append((fact, σ))
        for combo in product(*prem_sets):
            σ, used, ok = {}, [], True
            for fact, θ in combo:
                used.append(fact)
                for v, val in θ.items():
                    if v in σ and σ[v] != val: ok=False; break
                    σ[v] = val
                if not ok: break
            if not ok: continue
            new = subst(cons, σ)
            if new not in facts:
                facts.add(new)
                proof[new] = (rid, deepcopy(used))
                step += 1
                print(f"Step {step:02}: {new}   (by {rid})")
                changed = True

# ░░░ 4. Full fix-point & sample queries ░░░
print(f"\n=== Fix-point reached – {len(facts)} facts ===")
for t in sorted(facts):
    print(t)

queries = [
    ("Alice", "can_access", "accounting_doc"),   # ✓ purpose Accounting
    ("Bob",   "can_access", "accounting_doc"),   # ✗ no such permission
    ("Bob",   "can_access", "public_doc"),       # ✓ ANY purpose
    ("Charlie","can_access","accounting_doc"),   # ✓ via inherited MANAGER role
    ("Charlie","communicates_with","Alice"),     # ✓ symmetry rule
]
print("\n=== Selected queries ===")
for q in queries:
    print(f"{'✓' if q in facts else '✗'} {q}")

