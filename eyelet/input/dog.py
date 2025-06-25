#!/usr/bin/env python3
"""
Re-implementation of the EYE “dog” case in pure Python.

Data (facts) and the rule are embedded in this file so it has no
external dependencies except the rdflib library, which you can install
with:
    pip install rdflib
"""
from rdflib import Graph, Namespace, URIRef, Literal
from rdflib.namespace import RDF

# ----- Namespaces ----------------------------------------------------
EX   = Namespace("http://example.org/")
LOG  = Namespace("http://www.w3.org/2000/10/swap/log#")   # Unused here
MATH = Namespace("http://www.w3.org/2000/10/swap/math#")  # Unused here

# --------------------------------------------------------------------
# 1. Build the source graph with the same five facts as dog.n3
# --------------------------------------------------------------------
g = Graph()
g.bind("", EX)             # default namespace
g.bind("ex", EX)

# Persons
alice = EX.alice
bob   = EX.bob

# Alice’s five dogs
for i in range(1, 6):
    g.add((alice, EX.hasDog, EX[f"dog{i}"]))

# Bob’s two dogs
g.add((bob, EX.hasDog, EX.dog6))
g.add((bob, EX.hasDog, EX.dog7))

# --------------------------------------------------------------------
# 2. Very small “reasoner”: if someone has >4 dogs,
#    assert   ?person ex:mustHave ex:dogLicense .
# --------------------------------------------------------------------
def add_licence_triples(graph):
    """
    Implements the rule from dog.n3:

        { ?S ex:hasDog ?D .          # there is at least one dog
          (1 { ?S ex:hasDog ?Dog }   # collect all ?Dog into ?List
             ?List) log:collectAllIn ?Scope .
          ?List math:sum ?Count .
          ?Count math:greaterThan 4 .
        }
        =>
        { ?S ex:mustHave ex:dogLicense } .

    In straight Python we just count the dogs.
    """
    for person in set(graph.subjects(predicate=EX.hasDog)):
        num_dogs = len(list(graph.objects(person, EX.hasDog)))
        if num_dogs > 4:
            graph.add((person, EX.mustHave, EX.dogLicense))

add_licence_triples(g)

# --------------------------------------------------------------------
# 3. Show results -----------------------------------------------------
print("\n=== Triples after reasoning ===")
for s, p, o in g:
    print(f"{s.n3(g.namespace_manager)}  {p.n3(g.namespace_manager)}  {o.n3(g.namespace_manager)} .")

