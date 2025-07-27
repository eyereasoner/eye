"""
family_tree.py
--------------

A complete family-tree reasoner implemented purely with
Peirce’s existential graphs.

• Each atomic fact is a β-graph triple:  (subject, predicate, object)
• The *only* inference step is Peirce’s single-composition rule:
      fuse TWO graphs sharing a line of identity → ONE new graph.
• All rules below are expressed as *two-premise* templates.

The script:
    1. loads base facts and rules,
    2. saturates the sheet of assertion,
    3. prints the same relationship queries as the OO demo,
    4. prints a full Peirce proof tree for every ground query,
    5. prints a variable query:  (“Grandpa”, “grandfather”, ?x).

Determinism notes:
- Base facts are stored in a list and seeded in sorted order.
- Each saturation round iterates facts in sorted order.
- prove_query iterates proofs in sorted key order.
- q(...) returns subjects in sorted order.
"""

from itertools import count

# ──────────────────────────────────────────────────────────────
# helpers: unification, substitution, variable freshening
# ──────────────────────────────────────────────────────────────
is_var = lambda t: isinstance(t, str) and t.startswith("?")

def unify(pat, fact, θ):
    θ = dict(θ)
    for p, d in zip(pat, fact):
        if is_var(p):
            if p in θ and θ[p] != d:
                return None
            θ[p] = d
        elif p != d:
            return None
    return θ

def subst(triple, θ):
    return tuple(θ.get(t, t) for t in triple)

gensym = count(1).__next__
def apart(body, head):
    """Standardise-apart a rule copy (unused in this script)."""
    ren = {}
    f = lambda t: ren.setdefault(t, f"{t}_{gensym()}") if is_var(t) else t
    return [tuple(map(f, t)) for t in body], tuple(map(f, head))

# ──────────────────────────────────────────────────────────────
# 1.  base β-graphs (assertions) – gender, parent, spouse
#     Use a LIST (not a set), and seed things in sorted order.
# ──────────────────────────────────────────────────────────────
base_facts = [
    # gender
    ("Grandpa", "a", "MALE"),   ("Grandma", "a", "FEMALE"),
    ("Dad", "a", "MALE"),       ("Mom", "a", "FEMALE"),
    ("Child1", "a", "MALE"),    ("Child2", "a", "FEMALE"),
    ("Uncle", "a", "MALE"),     ("Aunt", "a", "FEMALE"),
    ("Cousin", "a", "MALE"),    ("MomSister", "a", "FEMALE"),
    ("MaternalGrandpa", "a", "MALE"), ("MaternalGrandma", "a", "FEMALE"),

    # parent
    ("Grandpa", "parent", "Dad"), ("Grandma", "parent", "Dad"),
    ("Grandpa", "parent", "Uncle"), ("Grandma", "parent", "Uncle"),
    ("Dad", "parent", "Child1"), ("Mom", "parent", "Child1"),
    ("Dad", "parent", "Child2"), ("Mom", "parent", "Child2"),
    ("Uncle", "parent", "Cousin"), ("Aunt", "parent", "Cousin"),
    ("MaternalGrandpa", "parent", "Mom"), ("MaternalGrandma", "parent", "Mom"),
    ("MaternalGrandpa", "parent", "MomSister"),
    ("MaternalGrandma", "parent", "MomSister"),

    # spouse  (one direction; symmetry comes from a rule)
    ("Grandpa", "spouse", "Grandma"),
    ("Dad",     "spouse", "Mom"),
    ("Uncle",   "spouse", "Aunt"),
]

facts = set(base_facts)        # will grow during saturation

# ──────────────────────────────────────────────────────────────
# 2.  two-premise composition rules (pure Peirce)
#     (Keep rule order fixed; we iterate rules in this order.)
# ──────────────────────────────────────────────────────────────
rules = [
    ("sym-spouse", ("?x","spouse","?y"), ("?x","a","?G"),
                   ("?y","spouse","?x")),

    ("sibling", ("?p","parent","?x"), ("?p","parent","?y"),
                ("?x","sibling","?y")),
    ("sym-sib", ("?x","sibling","?y"), ("?x","a","?G"),
                ("?y","sibling","?x")),

    ("brother", ("?x","sibling","?y"), ("?x","a","MALE"),
                ("?x","brother","?y")),
    ("sister", ("?x","sibling","?y"), ("?x","a","FEMALE"),
               ("?x","sister","?y")),

    ("grandparent", ("?x","parent","?y"), ("?y","parent","?z"),
                    ("?x","grandparent","?z")),
    ("grandfather", ("?x","grandparent","?z"), ("?x","a","MALE"),
                    ("?x","grandfather","?z")),
    ("grandmother", ("?x","grandparent","?z"), ("?x","a","FEMALE"),
                    ("?x","grandmother","?z")),

    ("father", ("?x","parent","?y"), ("?x","a","MALE"),
               ("?x","father","?y")),
    ("mother", ("?x","parent","?y"), ("?x","a","FEMALE"),
               ("?x","mother","?y")),

    ("uncle-blood", ("?x","brother","?p"), ("?p","parent","?y"),
                    ("?x","uncle","?y")),
    ("aunt-blood",  ("?x","sister","?p"), ("?p","parent","?y"),
                    ("?x","aunt","?y")),
    ("uncle-mar", ("?x","spouse","?s"), ("?s","aunt","?y"),
                  ("?x","uncle","?y")),
    ("aunt-mar", ("?x","spouse","?s"), ("?s","uncle","?y"),
                 ("?x","aunt","?y")),
]

# ──────────────────────────────────────────────────────────────
# 3.  forward saturation  (★ Peirce cut-erasure loop ★)
#     proofs[(S,P,O)] = ("fact", [])  or  (rule_id, [premise1,premise2])
#     Seed proofs in a deterministic (sorted) order.
# ──────────────────────────────────────────────────────────────
proofs = {}
for f in sorted(base_facts):
    proofs[f] = ("fact", [])

changed = True
while changed:
    changed = False
    # Iterate over a frozen snapshot of facts in sorted order.
    cur = sorted(facts)
    for rid, p1, p2, head in rules:           # rule order is fixed
        for f1 in cur:                         # facts in sorted order
            θ1 = unify(p1, f1, {})
            if θ1 is None:
                continue
            for f2 in cur:                     # facts in sorted order
                θ2 = unify(p2, f2, θ1)
                if θ2 is None:
                    continue
                new = subst(head, θ2)
                if new not in facts:
                    facts.add(new)
                    proofs[new] = (rid, [f1, f2])  # store premises in fixed order
                    changed = True

# ──────────────────────────────────────────────────────────────
# 4.  proof printers
# ──────────────────────────────────────────────────────────────
def show_ground(triple, depth=0):
    """Print a full justification tree for a *ground* triple."""
    ind = "  " * depth
    kind, prem = proofs[triple]
    if kind == "fact":
        print(f"{ind}{triple}   [fact]")
    else:
        rid = kind
        print(f"{ind}{triple}   [via {rid}]")
        for pr in prem:                 # premises are already in a deterministic order
            show_ground(pr, depth + 1)

def prove_query(pattern):
    """Match the (possibly variable) pattern against all facts
       and print one proof per answer, in a deterministic order."""
    found = False
    # Scan proofs in sorted key order instead of insertion order.
    for fact in sorted(proofs.keys()):
        θ = unify(pattern, fact, {})
        if θ is None:
            continue
        found = True
        inst = subst(pattern, θ)
        print(f"\n=== Proof for {inst}   (θ = {θ}) ===")
        show_ground(fact)
    if not found:
        print(f"\n✗ NOT PROVED   {pattern}")

# ──────────────────────────────────────────────────────────────
# 5.  convenience accessors  (same API as OO version)
#     Ensure q returns results in sorted order.
# ──────────────────────────────────────────────────────────────
def q(pred, s=None, o=None):
    return sorted(
        [S for (S, P, O) in facts if P == pred and (s is None or S == s) and (o is None or O == o)]
    )

father   = lambda c: next(iter(q("father", None, c)), None)
mother   = lambda c: next(iter(q("mother", None, c)), None)
children = lambda p: sorted(q("parent", p, None))
siblings = lambda x: sorted([o for o in q("sibling", x, None) if o != x])
brothers = lambda x: sorted(q("brother", None, x))
grandps  = lambda x: sorted(q("grandparent", None, x))
uncles   = lambda x: sorted(q("uncle", None, x))
aunts    = lambda x: sorted(q("aunt", None, x))

# ──────────────────────────────────────────────────────────────
# 6.  demo
# ──────────────────────────────────────────────────────────────
if __name__ == "__main__":

    # convenience queries (original OO output)
    print("Father of Child1:", father("Child1"))
    print("Mother of Child2:", mother("Child2"))
    print("Children of Dad:", children("Dad"))
    print("Siblings of Child1:", siblings("Child1"))
    print("Brothers of Child2:", brothers("Child2"))
    print("Grandparents of Child1:", grandps("Child1"))
    print("Uncles of Child1:", uncles("Child1"))
    print("Aunts of Child2:", aunts("Child2"))

    # full proofs for every ground triple used above
    ground_queries = [
        ("Dad","father","Child1"),
        ("Mom","mother","Child2"),
        ("Dad","parent","Child1"),
        ("Dad","parent","Child2"),
        ("Child2","sibling","Child1"),
        ("Child1","brother","Child2"),
        ("Grandpa","grandparent","Child1"),
        ("Grandma","grandparent","Child1"),
        ("MaternalGrandpa","grandparent","Child1"),
        ("MaternalGrandma","grandparent","Child1"),
        ("Uncle","uncle","Child1"),
        ("Dad","uncle","Child1"),
        ("Aunt","aunt","Child2"),
        ("Mom","aunt","Child2"),
        ("MomSister","aunt","Child2")
    ]
    for qtr in ground_queries:
        prove_query(qtr)

    # variable-pattern proof
    prove_query(("Grandpa", "grandfather", "?x"))

