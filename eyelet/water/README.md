# 🔀 Disjunction Elimination in eyelet

This eyelet logic program models **disjunction elimination** — also known as **proof by cases** — in RDF Turtle and N3 Logic. It demonstrates how an entity known to belong to a disjunction (e.g. being solid, liquid, or gas) can be shown to satisfy a shared consequence (e.g. being observable).

Inspired by classical logic and the pattern described in the [Wikipedia article on Disjunction Elimination](https://en.wikipedia.org/wiki/Disjunction_elimination).

---

## 📚 Prefixes

```turtle
@prefix list: <http://www.w3.org/2000/10/swap/list#> .
@prefix log:  <http://www.w3.org/2000/10/swap/log#> .
@prefix var:  <http://www.w3.org/2000/10/swap/var#> .
@prefix :     <https://eyereasoner.github.io/ns#> .
```

---

## 🌊 Base Fact: Water is an Inorganic Compound

```turtle
:water :is :InorganicCompound.
```

This asserts the fact that water is an inorganic compound — a known category.

---

## 🔀 Disjunction: Water is Solid, Liquid, or Gas

```turtle
[ log:graph (
    [ log:triple (var:A :is :InorganicCompound) ]
)] log:implies [ log:graph (
    [ log:triple ((var:A) log:allPossibleCases (
        [ log:graph ( [ log:triple (var:A :is :solid) ] ) ]
        [ log:graph ( [ log:triple (var:A :is :liquid) ] ) ]
        [ log:graph ( [ log:triple (var:A :is :gas) ] ) ]
    )) ]
)].
```

This rule introduces a **disjunction** for inorganic compounds:

> They could be solid, liquid, or gas.

---

## 👁️‍🗨️ Observable Forms: Each Case Implies Observability

```turtle
# solid ⇒ observable
[ log:graph ( [ log:triple (var:A :is :solid) ] )]
  log:implies [ log:graph ( [ log:triple (var:A :is :observable) ] )].

# liquid ⇒ observable
[ log:graph ( [ log:triple (var:A :is :liquid) ] )]
  log:implies [ log:graph ( [ log:triple (var:A :is :observable) ] )].

# gas ⇒ observable
[ log:graph ( [ log:triple (var:A :is :gas) ] )]
  log:implies [ log:graph ( [ log:triple (var:A :is :observable) ] )].
```

Each **disjunct** independently implies the same outcome:

> Anything that is solid, liquid, or gas is observable.

---

## 📚 Disjunction Elimination (Proof by Cases)

```turtle
[ log:graph (
    [ log:triple ((var:A) log:allPossibleCases var:B) ]
    [ log:triple ((
        [ log:graph (
            [ log:triple (var:B list:member [ log:graph (var:A :is var:C) ]) ]
        ) ]
        [ log:graph (
            [ log:triple ([ log:graph (
                [ log:triple (var:A :is var:C) ]
            ) ] log:implies [ log:graph (
                [ log:triple (var:A :is :observable) ]
            ) ]) ]
        ) ]
    ) log:forAllIn var:SCOPE) ]
)] log:implies [ log:graph (
    [ log:triple (var:A :is :observable) ]
)].
```

This rule states:
If you know **all possible cases** for `A`, and you can prove that each case implies the same result, then you can **eliminate the disjunction** and assert the result directly.

---

## ❓ Query: What Is `A`?

```turtle
[ log:graph (
    [ log:triple (var:A :is var:B) ]
)] log:impliesAnswer [ log:graph (
    [ log:triple (var:A :is var:B) ]
)].
```

This query asks:

> What can we infer `A` is?

Given the rules, eyelet will derive `:water :is :observable` by disjunction elimination.

---

> **TIP:** Use `log:allPossibleCases` to represent disjunctions in eyelet. Pair this with `log:forAllIn` to eliminate them via shared consequence.

> **NOTE:** Disjunction elimination is a valid inference rule in classical logic — and eyelet models it using graph-based implications over RDF Turtle.

> **Reference:** Based on [Disjunction Elimination](https://en.wikipedia.org/wiki/Disjunction_elimination) and formalized using N3 Logic.

