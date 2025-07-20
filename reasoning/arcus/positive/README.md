# ♻️ Proof by Contradiction (Eyelet Mini‑Demo)

This example shows how to encode a **proof by contradiction** (a.k.a. *reductio ad absurdum*) in eyelet / N3 using `log:satisfiable false` together with *nested graph contexts*.

Goal: infer that every integer `X` in the range **0 … 9** is `:Positive`.

---

## 🧠 Idea

For each generated number `X` (via `10 log:repeat X`), we build an *assumption
context* containing two implications:

1. **(A)** If `X ≥ 0` then `X :is :Positive`.
2. **(B)** If `X :is :Positive` then **false**.

If that context were satisfiable, we could have both a reason to make `X`
positive (from ≥ 0) and a reason that positive implies false, leading to
inconsistency. Declaring the whole assumption context `log:satisfiable false`
tells EYE the conjunction *cannot* be satisfied; thus we may conclude the
negation of the “bad” chain, namely that `X :is :Positive` holds.

So we *encode* the contradictory pair and mark it unsatisfiable, extracting the positive predicate as the result: a logical pattern analogous to:

> From ¬( (X≥0 → Positive) ∧ (Positive → ⊥) ) infer Positive.

---

## 🧩 Key fragment

```turtle
# For X = 0 .. 9:
[ log:graph (
    [ log:triple (10 log:repeat var:X) ]
    [ log:triple ([ log:graph (
        # (A)   X ≥ 0  ⇒  X :is :Positive
        [ log:triple ([ log:graph (
            [ log:triple (var:X math:notLessThan 0) ]
        ) ] log:implies [ log:graph (
            [ log:triple (var:X :is :Positive) ]
        ) ]) ]
        # (B)   X :is :Positive  ⇒  false
        [ log:triple ([ log:graph (
            [ log:triple (var:X :is :Positive) ]
        ) ] log:implies false) ]
    ) ] log:satisfiable false) ]
) ] log:implies [ log:graph (
    [ log:triple (var:X :is :Positive) ]
)].
````

---

## ❓ Query

```turtle
[ log:graph ( [ log:triple (var:X :is :Positive) ] ) ]
  log:impliesAnswer
[ log:graph ( [ log:triple (var:X :is :Positive) ] ) ].
```

---

## ▶️ Run

```bash
eye --quiet --nope positive.ttl
```

Drop `--nope` to see the proof steps for each `X`.

---

## ✅ Expected answer

```turtle
{ 0 :is :Positive.
  1 :is :Positive.
  2 :is :Positive.
  3 :is :Positive.
  4 :is :Positive.
  5 :is :Positive.
  6 :is :Positive.
  7 :is :Positive.
  8 :is :Positive.
  9 :is :Positive. }
```

---

## 🔍 What’s happening internally

| Component               | Purpose                                           |
| ----------------------- | ------------------------------------------------- |
| `10 log:repeat var:X`   | Generates integers `0 … 9`.                       |
| Inner two `log:implies` | Encode a hypothetical chain leading to `false`.   |
| `log:satisfiable false` | Declares that entire hypothetical set impossible. |
| Outer `log:implies`     | From impossibility, assert `var:X :is :Positive`. |
| Answer rule             | Collects all derived `:is :Positive` facts.       |

---

## 🧪 Variations

* **Different range**: replace `10 log:repeat` with `N log:repeat` for `0 … N‑1`.
* **Different predicate**: swap `:Positive` for `:NonNegative` (or any label).
* **Extra filter**: Add a guard e.g. `(var:X math:lessThan 5)` inside the outer `log:graph` to restrict results.

