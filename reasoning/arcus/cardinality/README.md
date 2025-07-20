# 🔢 OWL Cardinality Check in EYE

This example implements an **OWL-style cardinality constraint** using Notation3 logic and the EYE reasoner. It demonstrates how to ensure that individuals have an exact number of values for a given property, following the semantics of `owl:cardinality`.

We implement this by defining a counting mechanism over graph triples using `log:GraphCounter` and negative surface logic.

---

## 🧪 Sample Data

```turtle
:HasCardinalityCheck owl:onProperty :name.
:HasCardinalityCheck owl:cardinality 3.

:P1 :name "Patrick".
:P1 :name "Christian".
:P1 :name "Herman".

:P2 :name "Stefanie".
:P2 :name "Elizabeth".
```

---

## 🧩 Cardinality Logic

### Rule: Check Property Count Against Constraint

```turtle
(_:a _:b _:c _:x _:y _:Counter _:o) log:onNegativeSurface [ log:graph (
    [ log:triple (_:a owl:onProperty _:b) ]
    [ log:triple (_:a owl:cardinality _:c) ]
    [ log:triple (_:x _:b _:y) ]
    [ log:triple (_:Counter rdf:type log:GraphCounter) ]
    [ log:triple ([ log:graph ([ _:x _:b _:o ]) ] _:Counter _:c) ]
    [ log:triple (() log:onNegativeSurface [ log:graph (
        [ log:triple (_:x rdf:type _:a) ]
    )]) ]
)].
```

### Component: Graph Counter Definition

```turtle
:cardinalCount a log:GraphCounter.
(_:Graph _:Count _:List _:Scope) log:onNegativeSurface [ log:graph (
    [ log:triple (() log:onNegativeSurface [ log:graph (
        [ log:triple (_:Graph :cardinalCount _:Count) ]
    )]) ]
    [ log:triple ((() _:Graph _:List) log:collectAllIn _:Scope) ]
    [ log:triple (_:List list:length _:Count) ]
)].
```

This local `log:GraphCounter` mimics the behavior of a counter across graph patterns.

---

## ❓ Query: Who Violates Cardinality?

```turtle
(_:P) log:onNegativeSurface [ log:graph (
    [ log:triple (_:P rdf:type :HasCardinalityCheck) ]
    [ log:triple (() log:onNegativeAnswerSurface [ log:graph (
        [ log:triple (_:P rdf:type :HasCardinalityCheck) ]
    )]) ]
)].
```

This query tests which individuals satisfy the declared `owl:cardinality` condition.

---

## ▶️ Running the Program

Use EYE with:

```bash
eye --quiet --nope cardinality.ttl
```

Remove `--nope` to see the full justification proof:

```bash
eye --quiet cardinality.ttl
```

---

## 🧠 Summary

This logic demonstrates how to simulate **OWL cardinality constraints** using EYE.
It shows how to:

* Count triples via custom `log:GraphCounter`
* Embed logic constraints with `owl:onProperty` and `owl:cardinality`
* Use negative surface reasoning to enforce property value limits

