#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
tabled_horn_engine.py
=====================

A tiny, generic **tabled Horn-clause engine** (Datalog style) with
**goal-oriented proof reconstruction**.

Features
--------
• Declarative KB: 
    - base_facts:   ground atoms like ("oneway","paris","orleans")
    - rules:        Clause(label, head, body) with variables ?X, ?Y, …
    - queries:      Clause("Q", ("query",), [ ... ])   # zero-arity head
• Variables & unification with occurs-check; standardizing apart per use
• Tabling (least fixpoint) for **all intensional predicates** (IDB)
• Provenance: for each derived fact, which rule and which ground body atoms
• Clean proofs:
    - print each goal,
    - on **rule applications** show bindings (restricted to query vars),
    - on **facts** show ✓ (no bindings),
    - **no** failure lines
• Works with **left recursion** (e.g., transitive closure with path→path)

Limitations (by design, to keep it small)
-----------------------------------------
• Positive Horn clauses only (no negation, no arithmetic)
• Finite constants domain (no function symbols)
• Built-ins like inequality are easy to add but omitted for brevity

Usage
-----
- Pick EXAMPLE = "graph" or "family" at the bottom.
- Run:  python3 tabled_horn_engine.py
"""

from collections import defaultdict, deque
from itertools import count

# ────────────────────────────────────────────────────────────────────────
# Clause type
# ────────────────────────────────────────────────────────────────────────
class Clause:
    def __init__(self, label, head, body):
        """
        head: tuple like ("father","?X","?Y")  or zero-arity ("query",)
        body: list of tuples (atoms)
        """
        self.label, self.head, self.body = label, head, body
    def __repr__(self): return f"Clause({self.label}, {self.head}, {self.body})"

# ────────────────────────────────────────────────────────────────────────
# Tiny term / unification helpers
# ────────────────────────────────────────────────────────────────────────
VAR_COUNTER = count(0)
def is_var(x): return isinstance(x,str) and x.startswith("?")

def standardize_apart(clause: Clause) -> Clause:
    """Fresh variables per rule use, for display & joins."""
    m={}
    def ren(t):
        if is_var(t):
            if t not in m: m[t]=f"{t}_{next(VAR_COUNTER)}"
            return m[t]
        return t
    return Clause(
        clause.label,
        tuple(ren(x) for x in clause.head),
        [tuple(ren(x) for x in a) for a in clause.body]
    )

def subst_term(t, θ):
    """Resolve chained bindings with cycle guard."""
    seen=set()
    while is_var(t) and t in θ and t not in seen:
        seen.add(t); t = θ[t]
    return t

def subst_atom(a, θ): return tuple(subst_term(t, θ) for t in a)

def occurs_in(v, t, θ) -> bool:
    t = subst_term(t, θ)
    if v == t: return True
    if isinstance(t, tuple): return any(occurs_in(v, x, θ) for x in t)
    return False

def unify(a, b, θ):
    """Robinson unification for tuples; returns extended θ or None."""
    if θ is None: return None
    if a == b:    return θ
    if is_var(a): return unify_var(a, b, θ)
    if is_var(b): return unify_var(b, a, θ)
    if isinstance(a, tuple) and isinstance(b, tuple) and len(a) == len(b):
        for x, y in zip(a, b):
            θ = unify(x, y, θ)
            if θ is None: return None
        return θ
    return None

def unify_var(v, t, θ):
    if v in θ: return unify(θ[v], t, θ)
    t = subst_term(t, θ)
    if is_var(t) and t in θ: return unify(v, θ[t], θ)
    if occurs_in(v, t, θ): return None
    θ[v] = t
    return θ

# ────────────────────────────────────────────────────────────────────────
# Tabled bottom-up evaluator with provenance
# ────────────────────────────────────────────────────────────────────────
class TabledKB:
    """
    EDB: extensional tables (facts)      pred -> set[tuple(args)]
    IDB: intensional tables (derived)    pred -> set[tuple(args)]
    PROV: provenance                      pred -> { args_tuple: (label, [ground_body_atoms]) }
    """
    def __init__(self, base_facts, rules):
        self.EDB = defaultdict(set)
        for atom in base_facts:
            pred, *args = atom
            self.EDB[pred].add(tuple(args))
        self.rules = rules
        self.IDB  = defaultdict(set)
        self.PROV = defaultdict(dict)

    def all_facts(self, pred):
        """Union of EDB and IDB for pred."""
        return self.EDB[pred] | self.IDB[pred]

    def eval_body(self, body):
        """
        Generate groundings (θ, ground_atoms) for the body using current tables.
        ground_atoms is the instantiated list of body atoms (pred,*consts).
        """
        def backtrack(i, θ, chosen):
            if i == len(body):
                yield θ, chosen
                return
            atom = body[i]
            pred, *args = atom
            # Collect candidate tuples for this predicate from tables
            for tpl in self.all_facts(pred):
                # Try unify (pred,*args) with (pred,*tpl)
                θ2 = unify(atom, (pred, *tpl), dict(θ))
                if θ2 is None: 
                    continue
                ground = subst_atom(atom, θ2)
                yield from backtrack(i+1, θ2, chosen + [ground])
        yield from backtrack(0, {}, [])

    def saturate(self):
        """
        Compute least fixpoint for all rules (positive Horn/Datalog).
        For each *new* derived head, record the first provenance.
        """
        changed = True
        while changed:
            changed = False
            for raw in self.rules:
                r = standardize_apart(raw)
                pred_h, *args_h = r.head
                for θ, ground_body in self.eval_body(r.body):
                    head_inst = subst_atom(r.head, θ)
                    _, *hargs = head_inst
                    # Heads must be ground (no vars) to add to table
                    if any(is_var(x) for x in hargs):
                        continue
                    hargs = tuple(hargs)
                    if hargs not in self.IDB[pred_h] and hargs not in self.EDB[pred_h]:
                        self.IDB[pred_h].add(hargs)
                        # keep first provenance only (deterministic enough)
                        if hargs not in self.PROV[pred_h]:
                            self.PROV[pred_h][hargs] = (raw.label, ground_body)
                        changed = True

# ────────────────────────────────────────────────────────────────────────
# Proof reconstruction (top-down, but from provenance tables)
# ────────────────────────────────────────────────────────────────────────
def bindings_for_query_vars(solution_binding, stems):
    """Map only the requested query-variable stems (?X, ?Y, …) to their ground values."""
    out={}
    for s in stems:
        # allow both ?X and standardized ?X_*
        for k,v in solution_binding.items():
            if k == s or k.startswith(s + "_"):
                out[k]=v
    return out

def print_rule_application(indent, label, head_tpl, body_list, bindings):
    print(f"{indent}  → apply {label}: {head_tpl} :- {body_list}  with {bindings}")

def prove_from_prov(atom, kb: TabledKB, step_counter, stems, solution_binding, depth=0):
    """
    Print a compact proof for a ground `atom` using provenance:
      - facts (EDB) → ✓
      - IDB fact    → show rule + recursively prove each body atom
    """
    indent = "  " * depth
    pred, *args = atom
    print(f"{indent}Step {next(step_counter):02}: prove {atom}")

    # Fact?
    if tuple(args) in kb.EDB[pred]:
        print(f"{indent}  ✓ fact {atom}")
        return

    # Derived?
    prov = kb.PROV[pred].get(tuple(args))
    if not prov:
        # No provenance (shouldn't happen if we saturated correctly)
        return
    label, ground_body = prov

    # Pretty standardized head/body (for display only)
    head_vars = []
    for _ in args: head_vars.append(f"?V_{next(VAR_COUNTER)}")
    head_tpl = (pred, *head_vars)
    body_tpl = []
    for (p, *bargs) in ground_body:
        # show a schematic variable for each arg, just for shape
        vars_for_b = [f"?V_{next(VAR_COUNTER)}" for _ in bargs]
        body_tpl.append((p, *vars_for_b))

    # Only show query-variable bindings (e.g., ?X / ?X_*)
    bmap = bindings_for_query_vars(solution_binding, stems)
    print_rule_application(indent, label, head_tpl, body_tpl, bmap)

    # Subgoals (use the actual *ground* body atoms for recursion)
    for i, g in enumerate(ground_body, 1):
        print(f"{indent}    subgoal {i}/{len(ground_body)}: {g}")
        prove_from_prov(g, kb, step_counter, stems, solution_binding, depth+1)

# ────────────────────────────────────────────────────────────────────────
# Query evaluation (using tables): returns solution bindings for query vars
# ────────────────────────────────────────────────────────────────────────
def solve_query(query: Clause, kb: TabledKB):
    """
    Evaluate a query rule with head ("query",) and some body atoms.
    Returns a list of solution substitutions (variable->constant).
    """
    sols = []
    for θ, _ground in kb.eval_body(query.body):
        # Keep only variable->ground-constant bindings
        θg = {k: subst_term(v, θ) for k,v in θ.items() if not is_var(subst_term(v, θ))}
        sols.append(θg)
    return sols

# ────────────────────────────────────────────────────────────────────────
# Demo KBs
# ────────────────────────────────────────────────────────────────────────

def kb_graph():
    base_facts = [
        ("oneway","paris","orleans"),
        ("oneway","paris","chartres"),
        ("oneway","paris","amiens"),
        ("oneway","orleans","blois"),
        ("oneway","orleans","bourges"),
        ("oneway","blois","tours"),
        ("oneway","chartres","lemans"),
        ("oneway","lemans","angers"),
        ("oneway","lemans","tours"),
        ("oneway","angers","nantes"),
    ]
    rules = [
        Clause("C1", ("path","?U","?V"), [("oneway","?U","?V")]),
        # works fine even with left recursion thanks to tabling:
        Clause("C2", ("path","?U","?V"), [("path","?U","?Z"), ("path","?Z","?V")]),
        # non-left-recursive alternative (also fine):
        # Clause("C2", ("path","?U","?V"), [("oneway","?U","?Z"), ("path","?Z","?V")]),
    ]
    queries = [Clause("Q", ("query",), [("path","?X","nantes")])]
    stems   = ("?X",)
    return base_facts, rules, queries, stems

def kb_family():
    base_facts = [
        # gender
        ("a","Frans","MALE"), ("a","Jo","MALE"), ("a","Paul","MALE"),
        ("a","Pieter-Jan","MALE"), ("a","Tim","MALE"), ("a","Bert","MALE"),
        ("a","Bart","MALE"), ("a","Maria","FEMALE"), ("a","Maaike","FEMALE"),
        ("a","Rita","FEMALE"), ("a","Goedele","FEMALE"), ("a","Veerle","FEMALE"),
        ("a","Ann","FEMALE"), ("a","Veer","FEMALE"),
        # parent
        ("parent","Frans","Jo"), ("parent","Maria","Jo"),
        ("parent","Frans","Rita"), ("parent","Maria","Rita"),
        ("parent","Jo","Goedele"), ("parent","Maaike","Goedele"),
        ("parent","Jo","Veerle"), ("parent","Maaike","Veerle"),
        ("parent","Paul","Ann"), ("parent","Rita","Ann"),
        ("parent","Paul","Bart"), ("parent","Rita","Bart"),
        # spouse (one direction; symmetry via rule)
        ("spouse","Frans","Maria"), ("spouse","Jo","Maaike"),
        ("spouse","Paul","Rita"), ("spouse","Pieter-Jan","Goedele"),
        ("spouse","Tim","Veerle"), ("spouse","Bert","Ann"),
        ("spouse","Bart","Veer"),
    ]
    rules = [
        # symmetry of spouse
        Clause("sym-spouse", ("spouse","?Y","?X"), [("spouse","?X","?Y")]),
        # siblings
        Clause("sibling", ("sibling","?X","?Y"),
               [("parent","?P","?X"), ("parent","?P","?Y")]),
        Clause("sym-sib", ("sibling","?Y","?X"), [("sibling","?X","?Y")]),
        # gender-specialized siblings
        Clause("brother", ("brother","?X","?Y"), [("sibling","?X","?Y"), ("a","?X","MALE")]),
        Clause("sister",  ("sister", "?X","?Y"), [("sibling","?X","?Y"), ("a","?X","FEMALE")]),
        # grand relations
        Clause("grandparent", ("grandparent","?X","?Z"), [("parent","?X","?Y"), ("parent","?Y","?Z")]),
        Clause("grandfather", ("grandfather","?X","?Z"), [("grandparent","?X","?Z"), ("a","?X","MALE")]),
        Clause("grandmother", ("grandmother","?X","?Z"), [("grandparent","?X","?Z"), ("a","?X","FEMALE")]),
        # immediate parents
        Clause("father", ("father","?X","?Y"), [("parent","?X","?Y"), ("a","?X","MALE")]),
        Clause("mother", ("mother","?X","?Y"), [("parent","?X","?Y"), ("a","?X","FEMALE")]),
        # uncles & aunts (blood + by marriage)
        Clause("uncle-blood", ("uncle","?X","?Y"), [("brother","?X","?P"), ("parent","?P","?Y")]),
        Clause("aunt-blood",  ("aunt","?X","?Y"),   [("sister", "?X","?P"), ("parent","?P","?Y")]),
        Clause("uncle-mar",   ("uncle","?X","?Y"), [("spouse","?X","?S"), ("aunt","?S","?Y")]),
        Clause("aunt-mar",    ("aunt","?X","?Y"),  [("spouse","?X","?S"), ("uncle","?S","?Y")]),
    ]
    # Query examples (pick one by uncommenting)
    # queries = [Clause("Q", ("query",), [("uncle","?X","Bart")])]
    # stems   = ("?X",)
    queries = [Clause("Q", ("query",), [("grandmother","?X","Ann")])]
    stems   = ("?X",)
    return base_facts, rules, queries, stems

# ────────────────────────────────────────────────────────────────────────
# Driver
# ────────────────────────────────────────────────────────────────────────
if __name__ == "__main__":
    # Pick your demo KB
    EXAMPLE = "graph"   # "graph" or "family"
    if EXAMPLE == "graph":
        base_facts, rules, queries, stems = kb_graph()
    else:
        base_facts, rules, queries, stems = kb_family()

    # Build KB and table all derived facts (with provenance)
    kb = TabledKB(base_facts, rules)
    kb.saturate()

    # Solve the (single) query rule and collect solutions
    assert len(queries) == 1, "This demo expects exactly one query rule."
    solutions = solve_query(queries[0], kb)

    # Extract distinct answer tuples for the requested stems (e.g., (?X,))
    # Keep ordering deterministic by sorting
    def pick(binding, stems):
        return tuple(binding.get(s) or next((binding[k] for k in binding if k.startswith(s+"_")), None)
                     for s in stems)

    answers = sorted({ pick(sol, stems) for sol in solutions })

    # Print a compact proof for each answer (first target atom in query body)
    target_atom = queries[0].body[0]   # prove this one per answer
    print(f"\n=== Proofs for  {queries[0].body}  (tabling) ===")
    for ans in answers:
        print(f"\n--- Proof for {dict(zip(stems, ans))} ---")
        # Ground the target atom using the solution binding (just the stems we expose)
        # We reconstruct a complete binding by merging stems onto the atom via unification
        θ0 = {}
        for s, val in zip(stems, ans):
            if val is not None:
                θ0[s] = val
        θg = unify(target_atom, (target_atom[0], *target_atom[1:]), dict(θ0))  # same atom, θ0 just seeds vars
        ground_goal = subst_atom(target_atom, θg)
        steps = count(1)
        prove_from_prov(ground_goal, kb, steps, stems, θ0)
        print("✔ PROVED")

    # Optional: a tiny witness section (only meaningful for the graph demo)
    if EXAMPLE == "graph":
        # Recreate reverse BFS to GOAL for a compact path witness
        preds = defaultdict(set)
        for (u,v) in kb.EDB["oneway"]:
            preds[v].add(u)
        def parents(goal):
            parent={}; dq=deque([goal]); seen={goal}
            while dq:
                x=dq.popleft()
                for p in preds[x]:
                    if p not in seen:
                        seen.add(p); parent[p]=x; dq.append(p)
            return parent
        GOAL = "nantes"
        PARENT = parents(GOAL)
        def chain(x):
            if x==GOAL: return [x]
            path=[x]
            while path[-1]!=GOAL: path.append(PARENT[path[-1]])
            return path
        print("\n=== Solutions & Shortest Witnesses (graph) ===")
        for (x,) in answers:
            print(f"X = {x:<10} path: {' → '.join(chain(x))}")

