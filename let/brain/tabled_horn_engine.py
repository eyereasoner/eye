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
# Clause & unification
# ────────────────────────────────────────────────────────────────────────
class Clause:
    def __init__(self, label, head, body):
        self.label, self.head, self.body = label, head, body
    def __repr__(self): return f"Clause({self.label}, {self.head}, {self.body})"

# Global counter used when standardizing apart DURING TABLE EVAL
VAR_COUNTER = count(0)

def is_var(x): return isinstance(x,str) and x.startswith("?")

def reset_var_counter():
    global VAR_COUNTER
    VAR_COUNTER = count(0)

def standardize_apart(clause: Clause) -> Clause:
    """Fresh variables per rule use (stable given deterministic iteration)."""
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
# Tabled evaluator with provenance + built-ins (deterministic)
# ────────────────────────────────────────────────────────────────────────
BUILTINS = {"neq"}

def _sorted_tuples(it):
    # sort tuples lexicographically (strings only in this toy engine)
    return sorted(it)

class TabledKB:
    """
    EDB: pred -> sorted list of tuples(args)
    IDB: pred -> sorted list of tuples(args) (kept sorted as we add)
    PROV: pred -> { args_tuple: (label, [ground_body_atoms]) }  (first-hit)
    """
    def __init__(self, base_facts, rules):
        # Store EDB as sorted lists for deterministic iteration
        by_pred = defaultdict(set)
        for atom in base_facts:
            pred, *args = atom
            by_pred[pred].add(tuple(args))
        self.EDB = {p: _sorted_tuples(s) for p,s in by_pred.items()}
        self.rules = list(rules)  # preserve order
        self.IDB  = defaultdict(list)   # we’ll keep these sorted
        self.PROV = defaultdict(dict)

    def all_facts(self, pred):
        """Deterministic union of EDB and IDB as a sorted list."""
        if pred in BUILTINS:
            return []
        edb = self.EDB.get(pred, [])
        idb = self.IDB.get(pred, [])
        # merge two sorted lists deterministically
        i=j=0; merged=[]
        while i < len(edb) and j < len(idb):
            if edb[i] <= idb[j]:
                if not merged or merged[-1]!=edb[i]: merged.append(edb[i])
                i+=1
            else:
                if not merged or merged[-1]!=idb[j]: merged.append(idb[j])
                j+=1
        while i < len(edb):
            if not merged or merged[-1]!=edb[i]: merged.append(edb[i])
            i+=1
        while j < len(idb):
            if not merged or merged[-1]!=idb[j]: merged.append(idb[j])
            j+=1
        return merged

    def _idb_add(self, pred, args_tuple, prov_label, ground_body):
        """Insert into IDB keeping the list sorted; record first provenance."""
        lst = self.IDB[pred]
        # binary-insert (simple linear insert is fine for toy sizes)
        import bisect
        i = bisect.bisect_left(lst, args_tuple)
        if i < len(lst) and lst[i] == args_tuple:
            return False
        lst.insert(i, args_tuple)
        if args_tuple not in self.PROV[pred]:
            self.PROV[pred][args_tuple] = (prov_label, ground_body)
        return True

    def eval_builtin(self, atom, θ):
        pred, *args = atom
        if pred == "neq":
            a1, a2 = (subst_term(a, θ) for a in args)
            if not is_var(a1) and not is_var(a2) and a1 != a2:
                yield dict(θ), (pred, a1, a2)
            return
        return

    def eval_body(self, body):
        """
        Deterministic backtracking join:
        • iterate atoms in given order,
        • for non-builtins, iterate candidate tuples in sorted order.
        """
        def backtrack(i, θ, chosen):
            if i == len(body):
                yield θ, chosen
                return
            atom = body[i]
            pred, *args = atom

            if pred in BUILTINS:
                for θ2, ground in self.eval_builtin(atom, θ):
                    yield from backtrack(i+1, θ2, chosen + [ground])
                return

            for tpl in self.all_facts(pred):  # sorted
                θ2 = unify(atom, (pred, *tpl), dict(θ))
                if θ2 is None: 
                    continue
                ground = subst_atom(atom, θ2)
                yield from backtrack(i+1, θ2, chosen + [ground])

        yield from backtrack(0, {}, [])

    def saturate(self):
        """
        Least fixpoint; rule order preserved; body joins deterministic;
        IDB lists are kept sorted as we add.
        """
        reset_var_counter()
        changed = True
        while changed:
            changed = False
            for raw in self.rules:                 # rule order stable
                if raw.head[0] in BUILTINS:        # ignore nonsense
                    continue
                r = standardize_apart(raw)         # deterministic given rule/body order
                pred_h, *args_h = r.head
                for θ, ground_body in self.eval_body(r.body):
                    head_inst = subst_atom(r.head, θ)
                    _, *hargs = head_inst
                    if any(is_var(x) for x in hargs):
                        continue
                    hargs = tuple(hargs)
                    # skip if already in EDB
                    if hargs in self.EDB.get(pred_h, []):
                        continue
                    # insert sorted; record first provenance
                    if self._idb_add(pred_h, hargs, raw.label, ground_body):
                        changed = True

# ────────────────────────────────────────────────────────────────────────
# Proof printing (deterministic schematic vars)
# ────────────────────────────────────────────────────────────────────────
# Separate counter JUST for pretty schematic vars in printing
SCHEMA_COUNTER = count(0)
def reset_schema_counter():
    global SCHEMA_COUNTER
    SCHEMA_COUNTER = count(0)

def schema_vars(n):
    return [f"?V_{next(SCHEMA_COUNTER)}" for _ in range(n)]

def bindings_for_query_vars(solution_binding, stems):
    out={}
    # print stems in the order provided
    for s in stems:
        # match both ?X and ?X_*
        # sort keys for determinism
        for k in sorted(solution_binding.keys()):
            if k == s or k.startswith(s + "_"):
                out[k]=solution_binding[k]
    return out

def print_rule_application(indent, label, head_tpl, body_list, bindings):
    print(f"{indent}  → apply {label}: {head_tpl} :- {body_list}  with {bindings}")

def prove_from_prov(atom, kb: TabledKB, step_counter, stems, solution_binding, depth=0):
    indent = "  " * depth
    pred, *args = atom
    print(f"{indent}Step {next(step_counter):02}: prove {atom}")

    if pred in BUILTINS:
        print(f"{indent}  ✓ builtin {atom}")
        return

    if tuple(args) in kb.EDB.get(pred, []):
        print(f"{indent}  ✓ fact {atom}")
        return

    prov = kb.PROV[pred].get(tuple(args))
    if not prov:
        return
    label, ground_body = prov

    # Deterministic schematic head/body
    reset_schema_counter()
    head_tpl = (pred, *schema_vars(len(args)))
    body_tpl = []
    for (p, *bargs) in ground_body:
        body_tpl.append((p, *schema_vars(len(bargs))))

    bmap = bindings_for_query_vars(solution_binding, stems)
    print_rule_application(indent, label, head_tpl, body_tpl, bmap)

    for idx, g in enumerate(ground_body, 1):
        print(f"{indent}    subgoal {idx}/{len(ground_body)}: {g}")
        prove_from_prov(g, kb, step_counter, stems, solution_binding, depth+1)

# ────────────────────────────────────────────────────────────────────────
# Query solving
# ────────────────────────────────────────────────────────────────────────
def solve_query(query: Clause, kb: TabledKB):
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
        ("oneway","angers","nantes"),
        ("oneway","blois","tours"),
        ("oneway","chartres","lemans"),
        ("oneway","lemans","angers"),
        ("oneway","lemans","tours"),
        ("oneway","orleans","blois"),
        ("oneway","orleans","bourges"),
        ("oneway","paris","amiens"),
        ("oneway","paris","chartres"),
        ("oneway","paris","orleans"),
    ]
    # Keep rule order as declared
    rules = [
        Clause("C1", ("path","?U","?V"), [("oneway","?U","?V")]),
        Clause("C2", ("path","?U","?V"), [("path","?U","?Z"), ("path","?Z","?V")]),
    ]
    queries = [Clause("Q", ("query",), [("path","?X","nantes")])]
    stems   = ("?X",)
    return base_facts, rules, queries, stems

def kb_family():
    base_facts = [
        ("a","Ann","FEMALE"), ("a","Bart","MALE"), ("a","Bert","MALE"),
        ("a","Frans","MALE"), ("a","Goedele","FEMALE"), ("a","Jo","MALE"),
        ("a","Maaike","FEMALE"), ("a","Maria","FEMALE"), ("a","Paul","MALE"),
        ("a","Pieter-Jan","MALE"), ("a","Rita","FEMALE"), ("a","Tim","MALE"),
        ("a","Veer","FEMALE"), ("a","Veerle","FEMALE"),
        ("parent","Frans","Jo"), ("parent","Frans","Rita"),
        ("parent","Jo","Goedele"), ("parent","Jo","Veerle"),
        ("parent","Maria","Jo"), ("parent","Maria","Rita"),
        ("parent","Maaike","Goedele"), ("parent","Maaike","Veerle"),
        ("parent","Paul","Ann"), ("parent","Paul","Bart"),
        ("parent","Rita","Ann"), ("parent","Rita","Bart"),
        ("spouse","Bart","Veer"), ("spouse","Bert","Ann"),
        ("spouse","Frans","Maria"), ("spouse","Jo","Maaike"),
        ("spouse","Paul","Rita"), ("spouse","Pieter-Jan","Goedele"),
        ("spouse","Tim","Veerle"),
    ]
    rules = [
        Clause("sym-spouse", ("spouse","?Y","?X"), [("spouse","?X","?Y")]),
        Clause("sibling", ("sibling","?X","?Y"),
               [("parent","?P","?X"), ("parent","?P","?Y"), ("neq","?X","?Y")]),
        Clause("sym-sib", ("sibling","?Y","?X"), [("sibling","?X","?Y")]),
        Clause("brother", ("brother","?X","?Y"), [("sibling","?X","?Y"), ("a","?X","MALE")]),
        Clause("sister",  ("sister", "?X","?Y"), [("sibling","?X","?Y"), ("a","?X","FEMALE")]),
        Clause("grandparent", ("grandparent","?X","?Z"), [("parent","?X","?Y"), ("parent","?Y","?Z")]),
        Clause("grandfather", ("grandfather","?X","?Z"), [("grandparent","?X","?Z"), ("a","?X","MALE")]),
        Clause("grandmother", ("grandmother","?X","?Z"), [("grandparent","?X","?Z"), ("a","?X","FEMALE")]),
        Clause("father", ("father","?X","?Y"), [("parent","?X","?Y"), ("a","?X","MALE")]),
        Clause("mother", ("mother","?X","?Y"), [("parent","?X","?Y"), ("a","?X","FEMALE")]),
        Clause("uncle-blood", ("uncle","?X","?Y"), [("brother","?X","?P"), ("parent","?P","?Y")]),
        Clause("aunt-blood",  ("aunt","?X","?Y"),   [("sister", "?X","?P"), ("parent","?P","?Y")]),
        Clause("uncle-mar",   ("uncle","?X","?Y"), [("spouse","?X","?S"), ("aunt","?S","?Y")]),
        Clause("aunt-mar",    ("aunt","?X","?Y"),  [("spouse","?X","?S"), ("uncle","?S","?Y")]),
    ]
    queries = [Clause("Q", ("query",), [("grandfather","?X","?Y")])]
    stems   = ("?X","?Y")
    return base_facts, rules, queries, stems

# ────────────────────────────────────────────────────────────────────────
# Runner helpers (deterministic outputs)
# ────────────────────────────────────────────────────────────────────────
def bindings_for_query_vars(solution_binding, stems):
    out={}
    for s in stems:
        for k in sorted(solution_binding.keys()):
            if k == s or k.startswith(s + "_"):
                out[k]=solution_binding[k]
    return out

def print_rule_application(indent, label, head_tpl, body_list, bindings):
    print(f"{indent}  → apply {label}: {head_tpl} :- {body_list}  with {bindings}")

def prove_from_prov(atom, kb: TabledKB, step_counter, stems, solution_binding, depth=0):
    indent = "  " * depth
    pred, *args = atom
    print(f"{indent}Step {next(step_counter):02}: prove {atom}")

    if pred in BUILTINS:
        print(f"{indent}  ✓ builtin {atom}")
        return

    if tuple(args) in kb.EDB.get(pred, []):
        print(f"{indent}  ✓ fact {atom}")
        return

    prov = kb.PROV[pred].get(tuple(args))
    if not prov:
        return
    label, ground_body = prov

    reset_schema_counter()
    head_tpl = (pred, *schema_vars(len(args)))
    body_tpl = [(p, *schema_vars(len(bargs))) for (p, *bargs) in ground_body]

    bmap = bindings_for_query_vars(solution_binding, stems)
    print_rule_application(indent, label, head_tpl, body_tpl, bmap)

    for i, g in enumerate(ground_body, 1):
        print(f"{indent}    subgoal {i}/{len(ground_body)}: {g}")
        prove_from_prov(g, kb, step_counter, stems, solution_binding, depth+1)

def solve_query(query: Clause, kb: TabledKB):
    sols=[]
    for θ, _ground in kb.eval_body(query.body):
        θg = {k: subst_term(v, θ) for k,v in θ.items() if not is_var(subst_term(v, θ))}
        sols.append(θg)
    return sols

def pick(binding, stems):
    return tuple(binding.get(s) or next((binding[k] for k in sorted(binding) if k.startswith(s+"_")), None)
                 for s in stems)

def run_block(title, base_facts, rules, queries, stems):
    print(f"\n# ==== {title} ====")
    kb = TabledKB(base_facts, rules)
    kb.saturate()

    assert len(queries) == 1
    q = queries[0]
    solutions = solve_query(q, kb)
    answers = sorted({ pick(sol, stems) for sol in solutions })

    target_atom = q.body[0]
    print(f"\n=== Proofs for {q.body} (tabling; deterministic) ===")
    for ans in answers:
        print(f"\n--- Proof for {dict(zip(stems, ans))} ---")
        θ0 = {s:v for s,v in zip(stems, ans) if v is not None}
        θg = unify(target_atom, (target_atom[0], *target_atom[1:]), dict(θ0))
        ground_goal = subst_atom(target_atom, θg)
        steps = count(1)
        prove_from_prov(ground_goal, kb, steps, stems, θ0)
        print("✔ PROVED")

    # Optional deterministic witness for graph
    if target_atom[0] == "path" and target_atom[-1] == "nantes":
        preds = defaultdict(set)
        for (u,v) in kb.EDB["oneway"]:
            preds[v].add(u)
        def parents(goal):
            parent={}; dq=deque([goal]); seen={goal}
            while dq:
                x=dq.popleft()
                for p in sorted(preds[x]):   # sorted for determinism
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
# Main
# ────────────────────────────────────────────────────────────────────────
if __name__ == "__main__":
    # 1) Graph: query :- path(?X, "nantes")
    g_base, g_rules, g_queries, g_stems = kb_graph()
    run_block("Graph KB  —  Q: path(?X, \"nantes\")", g_base, g_rules, g_queries, g_stems)

    # 2) Family: query :- grandfather(?X, ?Y)
    f_base, f_rules, f_queries, f_stems = kb_family()
    run_block("Family KB —  Q: grandfather(?X, ?Y)", f_base, f_rules, f_queries, f_stems)

