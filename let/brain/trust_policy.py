"""
trust_policy.py
───────────────

Key features reproduced
───────────────────────
• Transitive delegation (Admin → DeptHead → MgrAlice → User)
• Revocation certificates
• Role and clearance checks
• Work-hours context predicate
• Optional break-glass emergency override
"""

from collections import defaultdict
from typing import Dict, Set, List, Tuple

# ───────────────────────────────────────────────────────────────
# 1 ▸  Policy facts
# ───────────────────────────────────────────────────────────────
DELEGATIONS: List[Tuple[str, str]] = [
    ("admin",      "dept_head"),
    ("dept_head",  "mgr_alice"),
    ("mgr_alice",  "alice"),
    ("dept_head",  "bob"),          # Bob bypasses mgr_alice
    ("admin",      "vendor_mgr"),
    ("vendor_mgr", "eve"),
]

REVOKED: Set[str] = {"bob"}

ROLE: Dict[str, str] = {
    "alice":      "employee",
    "bob":        "employee",
    "eve":        "contractor",
    "mgr_alice":  "employee",
    "dept_head":  "employee",
}

CLEARANCE: Dict[str, str] = {
    "alice":      "secret",
    "bob":        "secret",
    "mgr_alice":  "secret",
    "dept_head":  "secret",
    "eve":        "confidential",
}

WORK_HOURS: bool = True          # stub - assume it is office time
BREAK_GLASS: bool = False        # set to True for the emergency override


# ───────────────────────────────────────────────────────────────
# 2 ▸  Helper:  compute the set of *trusted* principals
# ───────────────────────────────────────────────────────────────
def compute_trusted() -> Set[str]:
    """Return the least fixed point of the two trusted/2 rules."""
    # adjacency list  Admin ─delegate→  Node
    edges: Dict[str, Set[str]] = defaultdict(set)
    for src, dst in DELEGATIONS:
        edges[src].add(dst)

    trusted: Set[str] = set()
    worklist: List[str] = []             # newly trusted nodes to expand

    # rule 1: direct delegation by admin
    for dst in edges["admin"]:
        if dst not in REVOKED:
            trusted.add(dst)
            worklist.append(dst)

    # rule 2: transitive closure through already-trusted nodes
    while worklist:
        y = worklist.pop()
        for x in edges.get(y, ()):
            if x not in REVOKED and x not in trusted:
                trusted.add(x)
                worklist.append(x)

    return trusted


# ───────────────────────────────────────────────────────────────
# 3 ▸  Authorisation decision for one user U
# ───────────────────────────────────────────────────────────────
def allow_read_secret_data(u: str, trusted: Set[str]) -> bool:
    """Implement the compound allow/2 rule."""
    main_rule = all((
        u in trusted,
        ROLE.get(u) == "employee",
        CLEARANCE.get(u) == "secret",
        WORK_HOURS,
    ))
    return main_rule or BREAK_GLASS


# ───────────────────────────────────────────────────────────────
# 4 ▸  Run the evaluation and print a report
# ───────────────────────────────────────────────────────────────
def main() -> None:
    trusted = compute_trusted()

    queries = [
        "alice",
        "bob",
        "eve",
        "mgr_alice",
        "dept_head",
    ]

    print("Authorisation results (1 = allowed, 0 = denied)")
    for u in queries:
        res = allow_read_secret_data(u, trusted)
        print(f"allow(read_secret_data,{u}): {int(res)}")


if __name__ == "__main__":
    main()

