# 🧠 Derived Rule Inference with arcus

This example demonstrates **deriving a rule** from facts and using it to prove a new statement. A rule is generated based on the presence of certain types, then used in a nested implication to trigger a conclusion.

---

## 📚 Prefixes

```turtle
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix log: <http://www.w3.org/2000/10/swap/log#> .
@prefix var: <http://www.w3.org/2000/10/swap/var#> .
@prefix : <http://example.org/#> .
```

---

## 🐾 Facts

```turtle
:Alice a :Cat .
:Minka a :Cat .
:Charly a :Dog .
```

These are the individuals and their types.

---

## 🔁 Derived Rule

```turtle
[ log:graph (
    [ log:triple (var:x rdf:type :Cat) ]
)] log:implies [ log:graph (
    [ log:triple (
        [ log:graph (
            [ log:triple (var:y rdf:type :Dog) ]
        )] log:implies [ log:graph (
            [ log:triple (:test :is true) ]
        )]
    ) ]
)] .
```

* If there exists a cat, a new **derived rule** is created:

  > “If there’s a dog, then `:test :is true`.”

This structure nests one rule inside another via logical implication.

---

## 🔍 Query

```turtle
[ log:graph (
    [ log:triple (:test :is true) ]
)] log:impliesAnswer [ log:graph (
    [ log:triple (:test :is true) ]
)] .
```

This query checks whether the `:test` result has been logically inferred.

---

## ✅ Expected Output

```turtle
:test :is true .
```

arcus will derive this fact through the chained reasoning process.

---

> **NOTE:** This pattern is useful in **meta-reasoning** and **conditional inference**, where a rule emerges only in the presence of a certain context (like the existence of a `:Cat`).

> **TIP:** Use this approach to model **adaptive rules** that only activate under data-dependent conditions.

