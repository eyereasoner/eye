# ❌ Scoped Negation as Failure in Eyelet

This Eyelet reasoning model demonstrates **Scoped Negation as Failure (SNAF)** — a reasoning pattern in N3 Logic that allows inferring negation based on the **absence** of information in a given graph.

The example uses a simple social domain to infer that *Alice hates nobody*, because there's no evidence to the contrary.

---

## 📚 Prefixes

```turtle
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix log: <http://www.w3.org/2000/10/swap/log#> .
@prefix var: <http://www.w3.org/2000/10/swap/var#> .
@prefix :    <http://example.org/#> .
```

---

## 📂 Facts

```turtle
:Alice :loves :Bob .
:Bob a :Person .
```

These base facts tell us that Alice loves Bob, and Bob is a person.

---

## 🔁 Rule: Negation by Absence (SNAF)

```turtle
[ log:graph (
    [ log:triple (var:SCOPE log:notIncludes [ log:graph (
        [ log:triple (:Alice :hates var:X) ]
    ) ]) ]
    [ log:triple (var:X rdf:type :Person) ]
)] log:implies [ log:graph (
    [ log:triple (:Alice :hates :Nobody) ]
)].
```

This rule says:

> If, **in the current scope**, there is no evidence that Alice hates anyone who is a `:Person`,
> then we can infer that *Alice hates `:Nobody`*.

This is a classic example of **negation as failure** — inferring negation when a pattern does not match.

---

## ❓ Query: Who Does Alice Hate?

```turtle
[ log:graph (
    [ log:triple (var:s :hates var:o) ]
)] log:impliesAnswer [ log:graph (
    [ log:triple (var:s :hates var:o) ]
)].
```

This query asks:

> *Who does Alice hate?*

Since the graph does not contain any `:Alice :hates ?X` facts, and all conditions for the rule are met, Eyelet will infer:

```turtle
:Alice :hates :Nobody .
```

---

> **TIP:** `log:notIncludes` allows negation to be **scoped**, meaning it only checks for absence in a specific graph — not globally.

> **NOTE:** This approach avoids paradoxes of global closed-world reasoning and supports declarative default reasoning.

> **Reference:** Based on Scoped Negation as Failure as described in [N3 Logic documentation](https://www.w3.org/2000/10/swap/doc/).

