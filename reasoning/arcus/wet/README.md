# 🔄 Proof by Contrapositive in arcus

This arcus reasoning model demonstrates **logical inference** using **contrapositive reasoning** in RDF Turtle and N3 Logic.

It shows how to infer that a premise is false based on a conclusion that is false — using formal logic and `log:implies`.

---

## 📚 Prefixes

```turtle
@prefix log: <http://www.w3.org/2000/10/swap/log#> .
@prefix var: <http://www.w3.org/2000/10/swap/var#> .
@prefix :    <https://eyereasoner.github.io/ns#> .
```

---

## 🚫 Premise: The Ground is Not Wet

```turtle
# The ground is not wet
[ log:graph (
    [ log:triple (:ground :is :wet) ]
)] log:implies false.
```

This asserts a **negative fact**:

> The ground is **not** wet.

---

## 🌧️ Rule: Rain Implies Wet Ground

```turtle
# If it is raining, then the ground is wet
[ log:graph (
    [ log:triple (:it :is :raining) ]
)] log:implies [ log:graph (
    [ log:triple (:ground :is :wet) ]
)].
```

This defines a **causal rule**:

> If it is raining, the ground will be wet.

---

## 🔁 Contrapositive Rule

```turtle
# Proof by contrapositive
[ log:graph (
    [ log:triple (var:P log:implies var:C) ]
    [ log:triple (var:C log:implies false) ]
)] log:implies [ log:graph (
    [ log:triple (var:P log:implies false) ]
)].
```

This general rule captures the **contrapositive inference**:

> If `P ⇒ C` and `C` is false, then `P` must also be false.

---

## ❓ Query: What Implies False?

```turtle
[ log:graph (
    [ log:triple ([ log:graph (
        [ log:triple (var:X :is var:Y) ]
    ) ] log:implies false) ]
)] log:impliesAnswer [ log:graph (
    [ log:triple ([ log:graph (
        [ log:triple (var:X :is var:Y) ]
    ) ] log:implies false) ]
)].
```

This query asks:

> *Which statements about an entity being something are inconsistent (i.e., imply falsehood)?*

---

> **TIP:** The `log:implies false` pattern is used to model **negation** or inconsistency.

> **NOTE:** The contrapositive rule in logic is:
> If `P ⇒ C` and `¬C`, then `¬P`.

> **Reference:** This model demonstrates reasoning with contrapositive inference, commonly used in formal logic and rule-based systems.

