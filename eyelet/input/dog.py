#!/usr/bin/env python3
"""
Deterministic version of the EYE “dog licence” example.

Only std-lib + rdflib required:
    pip install rdflib
"""
from rdflib import Graph, Namespace

# ---------- Namespaces ----------
EX = Namespace("http://example.org/")

# ---------- Input data ----------
g = Graph()
g.bind("", EX)
g.bind("ex", EX)

alice, bob = EX.alice, EX.bob

for i in range(1, 6):
    g.add((alice, EX.hasDog, EX[f"dog{i}"]))

g.add((bob, EX.hasDog, EX.dog6))
g.add((bob, EX.hasDog, EX.dog7))

# ---------- Rule: > 4 dogs -> mustHave dogLicense ----------
def add_licence_triples(graph: Graph, ns: Namespace = EX) -> None:
    for person in {s for s, p, _ in graph.triples((None, ns.hasDog, None))}:
        if sum(1 for _ in graph.objects(person, ns.hasDog)) > 4:
            graph.add((person, ns.mustHave, ns.dogLicense))

add_licence_triples(g)

# ---------- Deterministic output ----------
# Sort by subject, then predicate, then object – all rendered with the
# same namespace manager so the strings compare consistently.
nm = g.namespace_manager
for s, p, o in sorted(g, key=lambda t: (t[0].n3(nm), t[1].n3(nm), t[2].n3(nm))):
    print(f"{s.n3(nm)}  {p.n3(nm)}  {o.n3(nm)} .")

