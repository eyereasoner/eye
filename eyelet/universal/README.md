# 🌍 Universal Statements in Eyelet

This Eyelet reasoning model demonstrates **universal quantification**, **Skolemization**, and **class membership** using RDF Turtle and N3 Logic.

It encodes universal truths like *“Everything is a resource”* and captures existential claims like *“Everybody loves someone who is lonely.”*

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

## 🧾 Rule: Everything is a Resource

```turtle
[ log:graph (
    [ log:triple (var:X rdf:type rdfs:Resource) ]
)] log:isImpliedBy true.
```

This is a **universal statement**:

> Every entity (`X`) is of type `rdfs:Resource`.

---

## 💙 Rule: Everyone Loves Someone (Who is Lonely)

### Step 1 — **Love Someone**

```turtle
[ log:graph (
    [ log:triple (var:A :loves var:B) ]
)] log:isImpliedBy [ log:graph (
    [ log:triple ((var:A) log:skolem var:B) ]
)].
```

> Every person (`A`) loves **some** specific individual (`B`), introduced via Skolemization.

### Step 2 — **That Someone is Lonely**

```turtle
[ log:graph (
    [ log:triple (var:B :is :lonely) ]
)] log:isImpliedBy [ log:graph (
    [ log:triple ((var:A) log:skolem var:B) ]
)].
```

> That same individual (`B`) is also **lonely**.

Together, these rules encode:

> **Everyone loves someone who is lonely.**

---

## ❓ Queries

### Query 1 — Is `:pat` a `rdfs:Resource`?

```turtle
[ log:graph (
    [ log:triple (:pat rdf:type rdfs:Resource) ]
)] log:impliesAnswer [ log:graph (
    [ log:triple (:pat rdf:type rdfs:Resource) ]
)].
```

This confirms that `:pat` satisfies the universal type rule.

---

### Query 2 — Who Does Bob Love (That Is Lonely)?

```turtle
[ log:graph (
    [ log:triple (:bob :loves var:X) ]
    [ log:triple (var:X :is :lonely) ]
)] log:impliesAnswer [ log:graph (
    [ log:triple (:bob :loves var:X) ]
    [ log:triple (var:X :is :lonely) ]
)].
```

This retrieves the **existentially implied** individual `:bob` loves — someone who is lonely.

---

> **TIP:** Use `log:skolem` in Eyelet to represent existentially implied individuals — uniquely generated but logically scoped.

> **NOTE:** `log:isImpliedBy true` acts like a universal axiom or a global truth in this context.

> **Reference:** This model captures ideas from first-order logic, showing how to represent universally and existentially quantified statements using N3 Logic.

