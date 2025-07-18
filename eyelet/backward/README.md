# 🔁 Backward Rule Example

This example demonstrates a simple **backward rule** in Turtle/N3 using the EYE reasoner. Backward rules let you work *goal‑first*: you state what you want to prove and let the reasoner search backwards for supporting facts.

The pattern is adapted from the [W3C N3 tutorial, p. 17](https://www.w3.org/2000/10/swap/doc/tutorial-1.pdf).

## 🧮 Rule Logic

> **Rule** – `X` is *more interesting* than `Y` **if** `X` is numerically greater than `Y`.

```turtle
@prefix math: <http://www.w3.org/2000/10/swap/math#> .
@prefix log:  <http://www.w3.org/2000/10/swap/log#> .
@prefix var:  <http://www.w3.org/2000/10/swap/var#> .
@prefix :     <http://example.org/#> .

[ log:graph  ( [ log:triple (var:X :moreInterestingThan var:Y) ] ) ]
    log:isImpliedBy
[ log:graph  ( [ log:triple (var:X math:greaterThan    var:Y) ] ) ] .
```

`log:isImpliedBy` encodes the dependency *backwards*: the reasoner starts with a goal like `X :moreInterestingThan Y` and looks for a way to satisfy it.

## ❓ Query

Ask whether **5 is more interesting than 3**:

```turtle
[ log:graph  ( [ log:triple (5 :moreInterestingThan 3) ] ) ]
    log:impliesAnswer
[ log:graph  ( [ log:triple (:test :is true) ] ) ] .
```

## ▶️ Running the program

Execute the file (e.g. `backward.ttl`) with EYE:

```bash
eye --quiet --nope backward.ttl
```

*Remove the `--nope` flag* to see EYE’s proof tree step‑by‑step.

Typical answer:

```turtle
:test :is true .
```

## 🧠 Summary

This minimal example shows how `log:isImpliedBy` supports goal‑driven inference in EYE. Backward rules are especially useful when your dataset is large or when you want proof explanations on demand.

