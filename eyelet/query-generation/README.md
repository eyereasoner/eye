# 🔁 Query Generation in eyelet

This eyelet reasoning model demonstrates **meta-query generation**, where a query not only requests information — it **generates another query** that retrieves the answer.

The technique showcases **query abstraction and templating**, which is useful in question-answering, query rewriting, and logic-based automation.

---

## 📚 Prefixes

```turtle
@prefix rdf:  <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix log:  <http://www.w3.org/2000/10/swap/log#> .
@prefix var:  <http://www.w3.org/2000/10/swap/var#> .
@prefix :     <http://example.org/#> .
```

---

## 🧾 Fact

```turtle
:Socrates a :Human .
```

This fact states that `:Socrates` is a `:Human`.

---

## ❓ Meta-Query: Generate a Query Template

```turtle
[ log:graph (
    [ log:triple (var:I rdf:type var:C) ]
)] log:impliesAnswer [ log:graph (
    [ log:triple ([ log:graph (
        [ log:triple (var:WHO rdf:type var:WHAT) ]
    ) ] log:impliesAnswer [ log:graph (
        [ log:triple (var:WHO rdf:type var:WHAT) ]
    ) ]) ]
)].
```

This meta-query performs two steps:

1. It matches existing facts of the form `?I rdf:type ?C`.
2. It **generates** a new query that asks:
   *“Who is of what type?”* — and wraps it in an inner `log:impliesAnswer`.

The result is a **template query** derived from observed patterns in the data.

---

### 🔍 Example Result

Given the input fact:

```turtle
:Socrates rdf:type :Human .
```

The meta-query will generate the equivalent of:

```turtle
[ log:graph (
    [ log:triple (:Socrates rdf:type :Human) ]
)].
```

Which retrieves:

```turtle
:Socrates rdf:type :Human .
```

This is an **auto-generated query** that mirrors existing data structure.

---

> **TIP:** This pattern enables reasoning **about reasoning**, ideal for AI agents, query composition, and question reformulation systems.

> **NOTE:** The outer query triggers the **construction of an inner query**, which is then evaluated for results — a common strategy in logic programming and meta-reasoning.

> **Reference:** Based on principles of higher-order logic and inspired by question-answering frameworks using RDF and N3 Logic.

