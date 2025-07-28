"""
family.py
---------

A family-tree reasoner with *Horn logic* **forward chaining**.

• Facts  : triples  (subject, predicate, object)
• Rules  : Horn clauses  head :- body₁, …, bodyₙ
• Engine : deterministic forward–chaining saturation that
           records one proof tree per new fact.
• Output : the same convenience API and demo as the earlier scripts,
           plus a full proof tree for every demo query.

We keep the classic (slightly permissive) definition of *sibling*—two
individuals are siblings if they share a parent, **even if they are
the same person**.  That is what ultimately lets the program derive,
e.g.,  `Dad uncle Child1` and `Mom aunt Child2`.
"""

from collections import namedtuple
from itertools import count

# ──────────────────────────────────────────────────────────────
# helpers: variables, unification, substitution, renaming
# ──────────────────────────────────────────────────────────────
is_var = lambda t: isinstance(t, str) and t.startswith("?")

def unify(pat, fact, θ):
    """Unify *one* pattern triple with one fact under current θ."""
    θ = dict(θ)
    for p, f in zip(pat, fact):
        # pattern variable
        if is_var(p):
            if p in θ and θ[p] != f:
                return None           # clash
            θ[p] = f
        # (rare) variable in fact
        elif is_var(f):
            if f in θ and θ[f] != p:
                return None
            θ[f] = p
        # two ground symbols
        elif p != f:
            return None
    return θ

def subst(triple, θ):
    """Substitute θ into a triple (may stay partially ground)."""
    return tuple(θ.get(t, t) for t in triple)

# fresh-variable renaming
gensym  = count(1).__next__
Clause  = namedtuple("Clause", "name head body")
def rename(clause):
    ren = {}
    f   = lambda t: ren.setdefault(t, f"{t}_{gensym()}") if is_var(t) else t
    head = tuple(map(f, clause.head))
    body = [tuple(map(f, lit)) for lit in clause.body]
    return Clause(clause.name, head, body)

# ──────────────────────────────────────────────────────────────
# knowledge base
# ──────────────────────────────────────────────────────────────
base_facts = [
    # gender
    ("Frans", "a", "MALE"),
    ("Jo", "a", "MALE"),
    ("Paul", "a", "MALE"),
    ("Pieter-Jan", "a", "MALE"),
    ("Tim", "a", "MALE"),
    ("Bert", "a", "MALE"),
    ("Bart", "a", "MALE"),
    ("Maria", "a", "FEMALE"),
    ("Maaike", "a", "FEMALE"),
    ("Rita", "a", "FEMALE"),
    ("Goedele", "a", "FEMALE"),
    ("Veerle", "a", "FEMALE"),
    ("Ann", "a", "FEMALE"),
    ("Veer", "a", "FEMALE"),

    # parent
    ("Frans", "parent", "Jo"),
    ("Maria", "parent", "Jo"),
    ("Frans", "parent", "Rita"),
    ("Maria", "parent", "Rita"),
    ("Jo", "parent", "Goedele"),
    ("Maaike", "parent", "Goedele"),
    ("Jo", "parent", "Veerle"),
    ("Maaike", "parent", "Veerle"),
    ("Paul", "parent", "Ann"),
    ("Rita", "parent", "Ann"),
    ("Paul", "parent", "Bart"),
    ("Rita", "parent", "Bart"),

    # spouse  (one direction; symmetry via rule)
    ("Frans", "spouse", "Maria"),
    ("Jo", "spouse", "Maaike"),
    ("Paul", "spouse", "Rita"),
    ("Pieter-Jan", "spouse", "Goedele"),
    ("Tim", "spouse", "Veerle"),
    ("Bert", "spouse", "Ann"),
    ("Bart", "spouse", "Veer"),
]

rules = [
    # symmetry of spouse
    Clause("sym-spouse",
           ("?y","spouse","?x"),
           [("?x","spouse","?y")]),

    # siblings
    Clause("sibling",
           ("?x","sibling","?y"),
           [("?p","parent","?x"),
            ("?p","parent","?y"),
            ("?x", "≠", "?y")]),

    Clause("sym-sib",
           ("?y","sibling","?x"),
           [("?x","sibling","?y")]),

    # gender-specialised siblings
    Clause("brother",
           ("?x","brother","?y"),
           [("?x","sibling","?y"),
            ("?x","a","MALE")]),

    Clause("sister",
           ("?x","sister","?y"),
           [("?x","sibling","?y"),
            ("?x","a","FEMALE")]),

    # grand relations
    Clause("grandparent",
           ("?x","grandparent","?z"),
           [("?x","parent","?y"),
            ("?y","parent","?z")]),

    Clause("grandfather",
           ("?x","grandfather","?z"),
           [("?x","grandparent","?z"),
            ("?x","a","MALE")]),

    Clause("grandmother",
           ("?x","grandmother","?z"),
           [("?x","grandparent","?z"),
            ("?x","a","FEMALE")]),

    # immediate parent
    Clause("father",
           ("?x","father","?y"),
           [("?x","parent","?y"),
            ("?x","a","MALE")]),

    Clause("mother",
           ("?x","mother","?y"),
           [("?x","parent","?y"),
            ("?x","a","FEMALE")]),

    # uncles & aunts (blood + by marriage)
    Clause("uncle-blood",
           ("?x","uncle","?y"),
           [("?x","brother","?p"),
            ("?p","parent","?y")]),

    Clause("aunt-blood",
           ("?x","aunt","?y"),
           [("?x","sister","?p"),
            ("?p","parent","?y")]),

    Clause("uncle-mar",
           ("?x","uncle","?y"),
           [("?x","spouse","?s"),
            ("?s","aunt","?y")]),

    Clause("aunt-mar",
           ("?x","aunt","?y"),
           [("?x","spouse","?s"),
            ("?s","uncle","?y")]),
]

# ──────────────────────────────────────────────────────────────
# forward saturation with proof recording
# ──────────────────────────────────────────────────────────────
facts_list = list(base_facts)        # preserves order
facts_set  = set(base_facts)         # O(1) membership tests

proofs = {f: ("fact", []) for f in base_facts}

def add_fact(t, rule_name, premises):
    """Insert new fact exactly once, keeping list order stable."""
    if t not in facts_set:
        facts_set.add(t)
        facts_list.append(t)         # <- order comes from when we derive it
        proofs[t] = (rule_name, premises)

def saturate():
    changed = True
    while changed:
        changed = False
        for rule in rules:               # deterministic rule order
            rc = rename(rule)            # fresh variables per pass

            # incremental join over the clause body
            substitutions = [{}]         # start with empty θ list
            premises       = [[]]

            for lit in rc.body:
                new_subs, new_prem = [], []
                for θ, pr_so_far in zip(substitutions, premises):
                    lit_inst = subst(lit, θ)

                    # built-ins: inequality
                    if lit_inst[1] == "≠":
                        a, b = lit_inst[0], lit_inst[2]
                        if a != b:
                            new_subs.append(θ)
                            new_prem.append(pr_so_far)
                        continue

                    for fact in facts_list:   # iterate in deterministic order
                        θ2 = unify(lit_inst, fact, θ)
                        if θ2 is not None:
                            new_subs.append(θ2)
                            new_prem.append(pr_so_far + [fact])

                substitutions, premises = new_subs, new_prem
                if not substitutions:    # early fail
                    break

            # add each resulting head instance
            for θ, body_facts in zip(substitutions, premises):
                head_inst = subst(rc.head, θ)
                if head_inst not in facts_set:
                    add_fact(head_inst, rule.name, body_facts)
                    changed = True

saturate()        # run once on import

# ──────────────────────────────────────────────────────────────
# proof display helpers
# ──────────────────────────────────────────────────────────────
def _show(triple, depth=0):
    ind = "  " * depth
    kind, prem = proofs[triple]
    if kind == "fact":
        print(f"{ind}{triple}   [fact]")
    else:
        print(f"{ind}{triple}   [via {kind}]")
        for p in prem:
            _show(p, depth + 1)

def prove_query(pattern):
    """Print a proof for *every* matching fact."""
    found = False
    for f in facts_list:
        θ = unify(pattern, f, {})
        if θ is None:
            continue
        found = True
        inst = subst(pattern, θ)
        print(f"\n=== Proof for {inst}   (θ = {θ}) ===")
        _show(f)
    if not found:
        print(f"\n✗ NOT PROVED   {pattern}")

# ──────────────────────────────────────────────────────────────
# convenience API  (same as the original scripts)
# ──────────────────────────────────────────────────────────────
q        = lambda pred, s=None, o=None: [S for (S,P,O) in facts_list
                                         if P == pred and (s is None or S == s) and
                                            (o is None or O == o)]
father   = lambda c: next(iter(q("father", None, c)), None)
mother   = lambda c: next(iter(q("mother", None, c)), None)
children = lambda p: sorted(q("parent", p, None))
siblings = lambda x: sorted([o for o in q("sibling", x, None) if o != x])
brothers = lambda x: sorted(q("brother", None, x))
grandps  = lambda x: sorted(q("grandparent", None, x))
uncles   = lambda x: sorted(q("uncle", None, x))
aunts    = lambda x: sorted(q("aunt", None, x))

# ──────────────────────────────────────────────────────────────
# demo
# ──────────────────────────────────────────────────────────────
if __name__ == "__main__":
    prove_query(("?s", "?p", "?o"))

