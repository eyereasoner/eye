# Deep taxonomy
# See http://ruleml.org/WellnessRules/files/WellnessRulesN3-2009-11-10.pdf

"""
Propagate instance–class facts up a subclass-of hierarchy.

Given:
    element_of   – set of (instance, class) pairs
    subclass_of  – iterable of (subclass, superclass) pairs

Return:
    inferred     – set of all (instance, superclass-or-class) pairs
                   reachable through zero or more subclass-of hops.

Complexity:

Each taxonomy edge and each newly-derived fact is touched **exactly once**,
so the algorithm runs in **O(E + F)** time and uses **O(V + F)** memory where  
`E = len(subclass_of)`, `F = |result|`, and `V` is the number of unique
classes.  Cycles are handled gracefully.
"""

def deep_taxonomy(element_of, subclass_of):
    # Build adjacency list: child → list of its direct parents
    graph = {}
    for sub, sup in subclass_of:
        graph.setdefault(sub, []).append(sup)

    inferred = set(element_of)   # All known (instance, class) facts
    queue    = list(element_of)  # Work-list seeded with the originals

    # Propagate up the hierarchy: each fact is processed once
    while queue:
        inst, cls = queue.pop()          # Pop a pending fact
        for sup in graph.get(cls, ()):   # For every direct superclass …
            pair = (inst, sup)
            if pair not in inferred:     # Only if we haven’t seen it before
                inferred.add(pair)
                queue.append(pair)       # Queue for further propagation

    return inferred

# ---------- demo / sanity-check (Deep-Taxonomy 100 000) -----------------
N = 100_000
element_of = {('i', 'n0')}
subclass_of = [(f'n{k}', f'n{k+1}') for k in range(N)] + \
              [(f'n{k}', f'i{k+1}') for k in range(N)] + \
              [(f'n{k}', f'j{k+1}') for k in range(N)] + \
              [(f'n{k}', f'k{k+1}') for k in range(N)]

pairs = deep_taxonomy(element_of, subclass_of)
dt_result = max((p for p in pairs if p[0] == 'i' and p[1].startswith('n')),
                key=lambda p: int(p[1][1:]))       # ('i', 'n100000')
print('dt_result =', dt_result)

