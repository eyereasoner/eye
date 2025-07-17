# 🐶 Dog License Rule in eyelet

This eyelet logic program enforces a **policy rule**:

> *If a person has more than 4 dogs, they must have a dog license.*

It uses RDF data, logic inference, and math aggregation to determine when the condition is met.

---

## 📚 Prefixes

```turtle
@prefix math: <http://www.w3.org/2000/10/swap/math#> .
@prefix log:  <http://www.w3.org/2000/10/swap/log#> .
@prefix var:  <http://www.w3.org/2000/10/swap/var#> .
@prefix :     <http://example.org/#> .
```

---

## 🧾 Facts

```turtle
:alice :hasDog :dog1, :dog2, :dog3, :dog4, :dog5 .
:bob   :hasDog :dog6, :dog7 .
```

* Alice has 5 dogs
* Bob has 2 dogs

---

## 🧠 Rule: Require License If More Than 4 Dogs

```turtle
[ log:graph (
    [ log:triple (var:Subject :hasDog var:Any) ]

    # Collect all dogs owned by Subject
    [ log:triple ((1 [ log:graph (
        [ log:triple (var:Subject :hasDog var:Dog) ]
    ) ] var:List) log:collectAllIn var:Scope) ]

    # Count how many dogs
    [ log:triple (var:List math:sum var:Count) ]

    # Check threshold
    [ log:triple (var:Count math:greaterThan 4) ]
)] log:implies [ log:graph (
    [ log:triple (var:Subject :mustHave :dogLicense) ]
)].
```

This rule:

* Gathers all dogs per subject
* Sums how many they own
* Infers they must have a license if count > 4

---

## ❓ Query: Who Must Have a Dog License?

```turtle
[ log:graph (
    [ log:triple (var:Subject :mustHave :dogLicense) ]
)] log:impliesAnswer [ log:graph (
    [ log:triple (var:Subject :mustHave :dogLicense) ]
)].
```

This query asks:

> *Which individuals must have a dog license according to the rule?*

Expected output:

```turtle
:alice :mustHave :dogLicense .
```

---

> **TIP:** Use `log:collectAllIn` with embedded graphs to dynamically gather facts scoped to a particular subject.

> **NOTE:** `math:sum` works on the count of collected bindings, enabling eyelet to model rules based on **cardinality constraints**.

> **Reference:** This pattern generalizes to other threshold-based policy rules (e.g., maximum pets, seats reserved, etc.).

