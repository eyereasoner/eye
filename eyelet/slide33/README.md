# 🦉 OWL Restriction Reasoning in eyelet

This eyelet logic model demonstrates how to **simulate OWL restrictions** using RDF Turtle and N3 Logic — specifically, the behavior of the `owl:allValuesFrom` constraint.

It shows how eyelet can derive class memberships and enforce OWL-style reasoning over RDF graphs using scoped negation and logical surfaces.

See slide 33 example from https://www.slideshare.net/PatHayes/blogic-iswc-2009-invited-talk

---

## 📚 Prefixes

```turtle
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix log: <http://www.w3.org/2000/10/swap/log#> .
@prefix :    <http://example.org/ns#> .
```

---

## 📜 OWL Restriction Axiom

```turtle
:aaa owl:onProperty :bbb .
:aaa owl:allValuesFrom :ccc .
```

This OWL restriction states:

> Any resource of type `:aaa` can only relate via `:bbb` to resources of type `:ccc`.

---

## ✅ Entailment Case 1: `:yyy` Should Be a `:ccc`

```turtle
:xxx rdf:type :aaa .
:xxx :bbb :yyy .
```

Given the OWL restriction above, this should **entail**:

```turtle
:yyy rdf:type :ccc .
```

eyelet can be used to enforce and check this inference.

---

## ✅ Entailment Case 2: Inferring Type of `:xxx` from Negative Evidence

```turtle
(_:y) log:onNegativeSurface [ log:graph (
    [ log:triple (:xxx :bbb _:y) ]
    [ log:triple (() log:onNegativeSurface [ log:graph (
        [ log:triple (_:y rdf:type :ccc) ]
    ) ]) ]
)].
```

This rule captures the idea:

> If `:xxx` links to some resource via `:bbb` that **is not** of type `:ccc`,
> then `:xxx` **cannot** be of type `:aaa`.

This represents **OWL-style validation failure** using **negative surfaces**.

---

## 🧠 Full Semantics of `owl:allValuesFrom`

```turtle
(_:a _:b _:c) log:onNegativeSurface [ log:graph (
    [ log:triple (_:a owl:onProperty _:b) ]
    [ log:triple (_:a owl:allValuesFrom _:c) ]

    [ log:triple (() log:onNegativeSurface [ log:graph (
        [ log:triple ((_:x _:y) log:onNegativeSurface [ log:graph (
            [ log:triple (_:x rdf:type _:a) ]
            [ log:triple (_:x _:b _:y) ]
            [ log:triple (() log:onNegativeSurface [ log:graph (
                [ log:triple (_:y rdf:type _:c) ]
            ) ]) ]
        ) ]) ]

        [ log:triple ((_:x) log:onNegativeSurface [ log:graph (
            [ log:triple ((_:y) log:onNegativeSurface [ log:graph (
                [ log:triple (_:x _:b _:y) ]
                [ log:triple (() log:onNegativeSurface [ log:graph (
                    [ log:triple (_:y rdf:type _:c) ]
                ) ]) ]
            ) ]) ]
            [ log:triple (() log:onNegativeSurface [ log:graph (
                [ log:triple (_:x rdf:type _:a) ]
            ) ]) ]
        ) ]) ]
    ) ]) ]
)].
```

This rule encodes the **complete semantics** of `owl:allValuesFrom`, capturing all structural violations and using **nested negative surfaces** to rule out membership.

---

## ❓ Query: Test for Consistency on Class Membership

```turtle
(_:S _:C) log:onNegativeSurface [ log:graph (
    [ log:triple (_:S rdf:type _:C) ]
    [ log:triple (() log:onNegativeAnswerSurface [ log:graph (
        [ log:triple (_:S rdf:type _:C) ]
    ) ]) ]
)].
```

This query asks:

> Is it **inconsistent** (i.e., leads to contradiction) to assert that `_:S` is a `_:C`?

This enables scoped **non-monotonic validation** via the use of `log:onNegativeAnswerSurface`.

---

> **TIP:** `owl:allValuesFrom` can be simulated in eyelet using a combination of `log:onNegativeSurface`, logical implication, and RDF list constructs.

> **NOTE:** This model enables **OWL-style reasoning** without relying on an OWL reasoner — using only eyelet and N3 Logic.

> **Reference:** Based on the semantics of [`owl:allValuesFrom`](https://www.w3.org/TR/owl2-syntax/#Object_Property_All-Values_From) and inspired by OWL validation techniques in RDF logic systems.

