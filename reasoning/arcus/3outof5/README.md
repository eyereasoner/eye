# ✅ 3-out-of-5 Optional Matching in arcus

This arcus logic program implements a **scoring pattern** using `log:callWithOptional` and numeric constraints. The goal is to infer that a subject meets a classification (e.g., `:3outof5`) if **at least 3 out of 5 properties are present**.

---

## 📚 Prefixes

```turtle
@prefix math: <http://www.w3.org/2000/10/swap/math#> .
@prefix log:  <http://www.w3.org/2000/10/swap/log#> .
@prefix var:  <http://www.w3.org/2000/10/swap/var#> .
@prefix :     <https://eyereasoner.github.io/eye/reasoning#> .
```

---

## 🧾 Facts

```turtle
:s :p1 true .
#:s :p2 true .  # (commented out / optional)
:s :p3 true .
:s :p4 true .
#:s :p5 true .  # (commented out / optional)
:s :p6 true .
:s :p7 true .
```

Only some properties (`:p1`, `:p3`, `:p4`) are asserted as `true`. Others like `:p2` and `:p5` are **optional** and may be missing.

---

## 🧠 Query Logic: Count How Many Properties Match

This rule uses **optional pattern matching** to count the number of satisfied properties:

```turtle
[ log:graph (
    # Optional match for :p1
    [ log:triple (true log:callWithOptional [ log:graph (
        [ log:triple (:s :p1 true) ]
        [ log:triple (var:C1 log:equalTo 1) ]
    ) ]) ]
    [ log:triple (true log:callWithOptional [ log:graph (
        [ log:triple (var:C1 log:equalTo 0) ]
    ) ]) ]

    # Optional match for :p2
    [ log:triple (true log:callWithOptional [ log:graph (
        [ log:triple (:s :p2 true) ]
        [ log:triple (var:C2 log:equalTo 1) ]
    ) ]) ]
    [ log:triple (true log:callWithOptional [ log:graph (
        [ log:triple (var:C2 log:equalTo 0) ]
    ) ]) ]

    # Repeat for :p3 to :p5
    [ log:triple (true log:callWithOptional [ log:graph (
        [ log:triple (:s :p3 true) ]
        [ log:triple (var:C3 log:equalTo 1) ]
    ) ]) ]
    [ log:triple (true log:callWithOptional [ log:graph (
        [ log:triple (var:C3 log:equalTo 0) ]
    ) ]) ]

    [ log:triple (true log:callWithOptional [ log:graph (
        [ log:triple (:s :p4 true) ]
        [ log:triple (var:C4 log:equalTo 1) ]
    ) ]) ]
    [ log:triple (true log:callWithOptional [ log:graph (
        [ log:triple (var:C4 log:equalTo 0) ]
    ) ]) ]

    [ log:triple (true log:callWithOptional [ log:graph (
        [ log:triple (:s :p5 true) ]
        [ log:triple (var:C5 log:equalTo 1) ]
    ) ]) ]
    [ log:triple (true log:callWithOptional [ log:graph (
        [ log:triple (var:C5 log:equalTo 0) ]
    ) ]) ]

    # Add the results
    [ log:triple ((var:C1 var:C2 var:C3 var:C4 var:C5) math:sum var:C) ]
    [ log:triple (var:C math:notLessThan 3) ]
)] log:impliesAnswer [ log:graph (
    [ log:triple (:s :is :3outof5) ]
)].
```

---

### 🔍 How It Works

* Each property `:p1` to `:p5` is checked optionally.
* If present, it contributes `1` to the score; otherwise `0`.
* The scores are summed.
* If the total is **≥ 3**, we infer `:s :is :3outof5`.

---

> **TIP:** `log:callWithOptional` lets you match missing triples without failing the whole rule — ideal for partial satisfaction checks.

> **NOTE:** This approach generalizes well for **threshold-based reasoning**, like "at least N of M conditions met."

> **Reference:** Inspired by fuzzy logic and SPARQL-style optional patterns, implemented using N3 Logic and arcus.

