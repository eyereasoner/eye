# Deep taxonomy
# See http://ruleml.org/WellnessRules/files/WellnessRulesN3-2009-11-10.pdf
"""
Propagate instance–class facts up a subclass-of hierarchy.

Given:
  element_of  – set of (instance, class) pairs
  subclass_of – iterable of (subclass, superclass) pairs

Return:
  inferred    – set of all (instance, superclass-or-class) pairs
                reachable through zero or more subclass-of hops.

Complexity
  Each taxonomy edge and each newly-derived fact is touched exactly once,
  so the algorithm runs in O(E + F) time and uses O(V + F) memory, where
    E = len(subclass_of), F = |result|, and V is the number of unique classes.
  Cycles are handled gracefully.
"""

# ───────────────────────────── Core algorithm (unchanged) ─────────────────────────────

def deep_taxonomy(element_of, subclass_of):
    # Build adjacency list: child → list of its direct parents
    graph = {}
    for sub, sup in subclass_of:
        graph.setdefault(sub, []).append(sup)

    inferred = set(element_of)       # All known (instance, class) facts
    queue = list(element_of)         # Work-list seeded with the originals

    # Propagate up the hierarchy: each fact is processed once
    while queue:
        inst, cls = queue.pop()      # Pop a pending fact
        for sup in graph.get(cls, ()):   # For every direct superclass …
            pair = (inst, sup)
            if pair not in inferred:     # Only if we haven’t seen it before
                inferred.add(pair)
                queue.append(pair)       # Queue for further propagation
    return inferred


# ---------- demo / sanity-check (Deep-Taxonomy 100 000) -----------------
N = 100_000
element_of = {('i', 'n0')}
subclass_of = ([(f'n{k}', f'n{k+1}') for k in range(N)] +
               [(f'n{k}', f'i{k+1}') for k in range(N)] +
               [(f'n{k}', f'j{k+1}') for k in range(N)] +
               [(f'n{k}', f'k{k+1}') for k in range(N)])

pairs = deep_taxonomy(element_of, subclass_of)
dt_result = max((p for p in pairs if p[0] == 'i' and p[1].startswith('n')),
                key=lambda p: int(p[1][1:]))  # ('i', 'n100000')
print('dt_result =', dt_result)


# ────────────────────────────────── ARC: Answer ──────────────────────────────────

def _count_family(prefix: str) -> int:
    return sum(1 for (inst, cls) in pairs if inst == 'i' and cls.startswith(prefix))

def print_answer():
    print("\nAnswer")
    print("======")

    # Basic counts
    num_facts = len(pairs)
    num_seed  = len(element_of)
    num_edges = len(subclass_of)
    num_classes = len({c for _i, c in pairs})

    # Family reachability (given this demo’s construction)
    n_chain = sum(1 for (_i, c) in pairs if _i == 'i' and c.startswith('n'))  # includes n0..nN
    i_branch = _count_family('i')
    j_branch = _count_family('j')
    k_branch = _count_family('k')

    print(f"Seed facts |element_of| = {num_seed}")
    print(f"Taxonomy edges |subclass_of| = {num_edges}")
    print(f"Inferred facts |pairs| = {num_facts}  (unique classes reached = {num_classes})")

    # The headline result from the original script
    inst, last_class = dt_result
    last_n = int(last_class[1:]) if last_class.startswith('n') else None
    print("\nDeepest chain on the ‘n’ lineage:")
    print(f"  dt_result = {dt_result}  → deepest n = n{last_n}")

    print("\nBy lineage (instance ‘i’):")
    print(f"  n* classes reached: {n_chain}")
    print(f"  i* classes reached: {i_branch}")
    print(f"  j* classes reached: {j_branch}")
    print(f"  k* classes reached: {k_branch}")


# ──────────────────────────────── ARC: Reason why ────────────────────────────────

def print_reason():
    print("\nReason why")
    print("==========")
    print("We perform a forward reachability over the subclass-of DAG/graph:")
    print("  • Build parent adjacency: for each class C, parents(C) = {superclasses of C}.")
    print("  • Seed a work-list with the given (instance, class) facts.")
    print("  • Pop (i, C); for each S ∈ parents(C) that hasn’t been seen, add (i, S) and push it.")
    print("Because each derived pair is enqueued at most once, and each taxonomy edge is")
    print("examined only when its child class is popped, the algorithm runs in O(E+F) time")
    print("and O(V+F) memory (dedup sets ensure termination even with cycles).")


# ─────────────────────────────── ARC: Check (harness) ──────────────────────────────

def print_check():
    print("\nCheck (harness)")
    print("===============")
    ok_all = True

    # A) The original demo should reach exactly nN on the n-chain
    try:
        got_n = dt_result[1]
        expect_n = f"n{N}"
        ok_chain = (got_n == expect_n)
        print(f"Deepest n in demo equals n{N}? {ok_chain}  (got {got_n})")
        ok_all &= ok_chain
    except Exception as e:
        print("Error checking deepest n:", e)
        ok_all = False

    # B) Monotonicity / superset: result includes all seeds
    seeds_included = all(s in pairs for s in element_of)
    print(f"Seeds included in closure? {seeds_included}")
    ok_all &= seeds_included

    # C) Idempotence on a small toy taxonomy (also checks cycle handling)
    toy_elem = {('x', 'A')}
    toy_tax  = [('A', 'B'), ('B', 'C'), ('C', 'B')]   # cycle B↔C
    toy_res  = deep_taxonomy(toy_elem, toy_tax)
    toy_fix  = deep_taxonomy(toy_res, toy_tax)
    idem_ok  = (toy_res == toy_fix) and toy_res.issuperset(toy_elem)
    print(f"Idempotence and cycle tolerance on toy taxonomy? {idem_ok}")
    ok_all &= idem_ok

    # D) Adding a new superclass yields a superset (monotone)
    mono_tax  = toy_tax + [('C', 'D')]
    mono_res  = deep_taxonomy(toy_elem, mono_tax)
    monotone  = toy_res.issubset(mono_res) and ('x', 'D') in mono_res
    print(f"Monotonicity (add edge C→D adds (x,D))? {monotone}")
    ok_all &= monotone

    # E) Determinism: re-running on the same inputs yields the same size
    pairs2 = deep_taxonomy(element_of, subclass_of)
    deterministic = (len(pairs2) == len(pairs)) and (dt_result in pairs2)
    print(f"Deterministic on demo inputs (size & key fact stable)? {deterministic}")
    ok_all &= deterministic

    print(f"\nAll checks passed? {ok_all}")


# ───────────────────────────────────── Main ─────────────────────────────────────

if __name__ == "__main__":
    print_answer()
    print_reason()
    print_check()

