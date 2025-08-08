#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
tabled_horn_engine.py
=====================

A tiny tabled Horn-clause (Datalog-style) engine with:
  • Declarative KB (facts, named rules, built-ins), and zero-arity query rules.
  • Variables (?X, ?Y, …), standardizing apart, unification (with occurs-check).
  • Bottom-up tabling with provenance (works with left recursion).
  • Clean proofs: show each goal; show bindings ONLY on rule apps (restricted to query vars);
    facts and built-ins as ✓; no failure lines.

This driver runs TWO separate KBs & queries:
  1) Graph KB:     Q: query :- path(?X, "nantes")
  2) Family KB:    Q: query :- grandfather(?X, ?Y)

Usage:
  $ python3 tabled_horn_engine.py
"""

from collections import defaultdict, deque
from itertools import count

# ────────────────────────────────────────────────────────────────────────
# Basic clause & unification utils
# ────────────────────────────────────────────────────────────────────────
class Clause:
    def __init__(self, label, head, body):
        """
        head: e.g. ("path","?U","?V") or zero-arity ("query",)
        body: list of atoms, each a tuple like ("parent","?X","?Y")
        """
        self.label, self.head, self.body = label, head, body
    def __repr__(self): return f"Clause({self.label}, {self.head}, {self.body})"

VAR_COUNTER = count(0)
def is_var(x): return isinstance(x,str) and x.startswith("?")

def standardize_apart(clause: Clause) -> Clause:
    """Fresh variables per rule use (for joins & pretty printing)."""
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
    """Resolve chained substitutions with a small cycle guard."""
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
# Tabled evaluator with provenance + built-ins
# ────────────────────────────────────────────────────────────────────────
BUILTINS = {"neq"}

class TabledKB:
    """
    EDB: pred -> set[tuple(args)]               (extensional facts)
    IDB: pred -> set[tuple(args)]               (derived facts)
    PROV: pred -> { args_tuple: (label, [ground_body_atoms]) }   (first provenance)
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
        if pred in BUILTINS: return set()
        return self.EDB[pred] | self.IDB[pred]

    # Built-ins evaluated on the fly (no facts stored)
    def eval_builtin(self, atom, θ):
        pred, *args = atom
        if pred == "neq":
            a1, a2 = (subst_term(a, θ) for a in args)
            if not is_var(a1) and not is_var(a2) and a1 != a2:
                yield dict(θ), (pred, a1, a2)
            return
        # Unknown built-in → fail safely
        return

    def eval_body(self, body):
        """
        Backtracking join over current tables + built-ins.
        Yields (θ, ground_body_atoms) for each satisfying grounding.
        """
        def backtrack(i, θ, chosen):
            if i == len(body):
                yield θ, chosen
                return
            atom = body[i]
            pred, *args = atom

            # Built-in?
            if pred in BUILTINS:
                for θ2, ground in self.eval_builtin(atom, θ):
                    yield from backtrack(i+1, θ2, chosen + [ground])
                return

            # Non-built-in: join with known facts
            for tpl in self.all_facts(pred):
                θ2 = unify(atom, (pred, *tpl), dict(θ))
                if θ2 is None: 
                    continue
                ground = subst_atom(atom, θ2)
                yield from backtrack(i+1, θ2, chosen + [ground])

        yield from backtrack(0, {}, [])

    def saturate(self):
        """
        Least fixpoint for all rules (positive Horn). Records first provenance.
        """
        changed = True
        while changed:
            changed = False
            for raw in self.rules:
                if raw.head[0] in BUILTINS:   # nonsense to derive a builtin
                    continue
                r = standardize_apart(raw)
                pred_h, *args_h = r.head
                for θ, ground_body in self.eval_body(r.body):
                    head_inst = subst_atom(r.head, θ)
                    _, *hargs = head_inst
                    if any(is_var(x) for x in hargs):  # must be ground
                        continue
                    hargs = tuple(hargs)
                    if hargs not in self.EDB[pred_h] and hargs not in self.IDB[pred_h]:
                        self.IDB[pred_h].add(hargs)
                        if hargs not in self.PROV[pred_h]:
                            self.PROV[pred_h][hargs] = (raw.label, ground_body)
                        changed = True

# ────────────────────────────────────────────────────────────────────────
# Proof printing (from provenance)
# ────────────────────────────────────────────────────────────────────────
def bindings_for_query_vars(solution_binding, stems):
    """Keep only requested query var stems (?X, ?Y, …) (also matches ?X_*, ?Y_*)."""
    out={}
    for s in stems:
        for k,v in solution_binding.items():
            if k == s or k.startswith(s + "_"):
                out[k]=v
    return out

def print_rule_application(indent, label, head_tpl, body_list, bindings):
    print(f"{indent}  → apply {label}: {head_tpl} :- {body_list}  with {bindings}")

def prove_from_prov(atom, kb: TabledKB, step_counter, stems, solution_binding, depth=0):
    """
    Compact proof for a *ground* atom using provenance tables:
      • facts → ✓ fact(...)
      • built-ins → ✓ builtin(...)
      • derived → show rule + recursively prove each ground body atom
    """
    indent = "  " * depth
    pred, *args = atom
    print(f"{indent}Step {next(step_counter):02}: prove {atom}")

    if pred in BUILTINS:
        print(f"{indent}  ✓ builtin {atom}")
        return

    if tuple(args) in kb.EDB[pred]:
        print(f"{indent}  ✓ fact {atom}")
        return

    prov = kb.PROV[pred].get(tuple(args))
    if not prov:
        return
    label, ground_body = prov

    # Pretty schematic head/body just for display
    head_vars = [f"?V_{next(VAR_COUNTER)}" for _ in args]
    head_tpl = (pred, *head_vars)
    body_tpl = []
    for (p, *bargs) in ground_body:
        body_tpl.append((p, *[f"?V_{next(VAR_COUNTER)}" for _ in bargs]))

    bmap = bindings_for_query_vars(solution_binding, stems)
    print_rule_application(indent, label, head_tpl, body_tpl, bmap)

    for i, g in enumerate(ground_body, 1):
        print(f"{indent}    subgoal {i}/{len(ground_body)}: {g}")
        prove_from_prov(g, kb, step_counter, stems, solution_binding, depth+1)

# ────────────────────────────────────────────────────────────────────────
# Query solving (using tables)
# ────────────────────────────────────────────────────────────────────────
def solve_query(query: Clause, kb: TabledKB):
    """Evaluate a zero-arity head query rule. Returns list of θ (ground var bindings)."""
    sols=[]
    for θ, _ground in kb.eval_body(query.body):
        θg = {k: subst_term(v, θ) for k,v in θ.items() if not is_var(subst_term(v, θ))}
        sols.append(θg)
    return sols

# ────────────────────────────────────────────────────────────────────────
# KBs & queries
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
        # Left-recursive TC (fine with tabling)
        Clause("C2", ("path","?U","?V"), [("path","?U","?Z"), ("path","?Z","?V")]),
        # Non-left-recursive alternative (also fine):
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
        # spouse (one-way; symmetry via rule)
        ("spouse","Frans","Maria"), ("spouse","Jo","Maaike"),
        ("spouse","Paul","Rita"), ("spouse","Pieter-Jan","Goedele"),
        ("spouse","Tim","Veerle"), ("spouse","Bert","Ann"),
        ("spouse","Bart","Veer"),
    ]
    rules = [
        # spouse symmetry
        Clause("sym-spouse", ("spouse","?Y","?X"), [("spouse","?X","?Y")]),

        # siblings: share parent and X ≠ Y (built-in)
        Clause("sibling", ("sibling","?X","?Y"),
               [("parent","?P","?X"),
                ("parent","?P","?Y"),
                ("neq","?X","?Y")]),

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
    # Your query: all grandfathers ?X of ?Y
    queries = [Clause("Q", ("query",), [("grandfather","?X","?Y")])]
    stems   = ("?X","?Y")
    return base_facts, rules, queries, stems

# ────────────────────────────────────────────────────────────────────────
# Helpers for running a KB+queries block
# ────────────────────────────────────────────────────────────────────────
def run_block(title, base_facts, rules, queries, stems):
    print(f"\n# ==== {title} ====")
    kb = TabledKB(base_facts, rules)
    kb.saturate()

    assert len(queries) == 1, "This demo expects one query rule per block."
    q = queries[0]
    solutions = solve_query(q, kb)

    # Extract distinct answer tuples for the requested stems order
    def pick(binding, stems):
        return tuple(binding.get(s) or next((binding[k] for k in binding if k.startswith(s+"_")), None)
                     for s in stems)
    answers = sorted({ pick(sol, stems) for sol in solutions })

    # Prove first atom in the query body per answer
    target_atom = q.body[0]
    print(f"\n=== Proofs for {q.body} (tabling) ===")
    for ans in answers:
        print(f"\n--- Proof for {dict(zip(stems, ans))} ---")
        θ0 = {s:v for s,v in zip(stems, ans) if v is not None}
        θg = unify(target_atom, (target_atom[0], *target_atom[1:]), dict(θ0))
        ground_goal = subst_atom(target_atom, θg)
        steps = count(1)
        prove_from_prov(ground_goal, kb, steps, stems, θ0)
        print("✔ PROVED")

    # For the graph block, add a tiny witness table
    if target_atom[0] == "path" and target_atom[-1] == "nantes":
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

# ────────────────────────────────────────────────────────────────────────
# Main: run BOTH queries you asked for
# ────────────────────────────────────────────────────────────────────────
if __name__ == "__main__":
    # 1) Graph: query :- path(?X, "nantes")
    g_base, g_rules, g_queries, g_stems = kb_graph()
    run_block("Graph KB  —  Q: path(?X, \"nantes\")", g_base, g_rules, g_queries, g_stems)

    # 2) Family: query :- grandfather(?X, ?Y)
    f_base, f_rules, f_queries, f_stems = kb_family()
    run_block("Family KB —  Q: grandfather(?X, ?Y)", f_base, f_rules, f_queries, f_stems)

