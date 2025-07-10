# 🧠 Zebra Puzzle in Eyelet (Einstein’s Riddle)

This Eyelet program models the classic **Zebra Puzzle**, also known as **Einstein’s Riddle**, using RDF Turtle and N3 Logic. The puzzle describes a set of constraints over five neighboring houses with different attributes — and asks: *“Who owns the fish?”*

---

## 📚 Prefixes

```turtle
@prefix list: <http://www.w3.org/2000/10/swap/list#> .
@prefix log:  <http://www.w3.org/2000/10/swap/log#> .
@prefix var:  <http://www.w3.org/2000/10/swap/var#> .
@prefix :     <http://eulersharp.sourceforge.net/2005/11swap/zebra#> .
```

---

## 🔍 Main Rule: Encode Puzzle Constraints

```turtle
[ log:graph (
    [ log:triple (var:L log:equalTo (
        ;; House order (left to right)
        (var:A1 :norwegian var:A2 var:A3 var:A4)
        (:blue var:A5 var:A6 var:A7 var:A8)
        (var:A9 var:A10 var:A11 :milk var:A12)
        var:A13 var:A14
    )) ]

    ;; Neighbors and relationships
    [ log:triple (var:L :pair ((var:A15 var:A16 var:A17 var:A18 :blends)
                               (var:A19 var:A20 :cats var:A21 var:A22))) ]
    [ log:triple (var:L :pair ((var:A23 var:A24 :horse var:A25 var:A26)
                               (var:A27 var:A28 var:A29 var:A30 :dunhill))) ]
    [ log:triple (var:L :sublist ((:green var:A31 var:A32 :coffee var:A33)
                                  (:white var:A34 var:A35 var:A36 var:A37))) ]
    [ log:triple (var:L :pair ((var:A38 var:A39 var:A40 var:A41 :blends)
                               (var:A42 var:A43 var:A44 :water var:A45))) ]

    ;; Membership constraints
    [ log:triple (var:L list:member (:red :brit var:A46 var:A47 var:A48)) ]
    [ log:triple (var:L list:member (var:A49 :swede :dogs var:A50 var:A51)) ]
    [ log:triple (var:L list:member (var:A52 :dane var:A53 :tea var:A54)) ]
    [ log:triple (var:L list:member (var:A55 var:A56 :birds var:A57 :pallmall)) ]
    [ log:triple (var:L list:member (:yellow var:A58 var:A59 var:A60 :dunhill)) ]
    [ log:triple (var:L list:member (var:A61 var:A62 var:A63 :beer :bluemasters)) ]
    [ log:triple (var:L list:member (var:A64 :german var:A65 var:A66 :prince)) ]
    [ log:triple (var:L list:member (var:A67 var:B :fish var:A69 var:A70)) ]
)] log:implies [ log:graph (
    [ log:triple (var:B :eats :fish) ]
)].
```

This rule:

* Asserts the structure and ordering of the five houses
* Defines relationships between house features (colors, pets, nationalities, drinks, cigars)
* Uses `log:equalTo`, `list:member`, and `:pair`/`:sublist` to impose constraints
* Infers the person (via `var:B`) who owns the fish

---

## 🔁 Supporting Rules

```turtle
# Convert sublist to pair (directional)
[ log:graph (
    [ log:triple (var:A :pair (var:B var:C)) ]
)] log:isImpliedBy [ log:graph (
    [ log:triple (var:A :sublist (var:B var:C)) ]
)].

# Convert sublist to pair (reverse direction)
[ log:graph (
    [ log:triple (var:A :pair (var:B var:C)) ]
)] log:isImpliedBy [ log:graph (
    [ log:triple (var:A :sublist (var:C var:B)) ]
)].

# Infer sublist from list append
[ log:graph (
    [ log:triple (var:A :sublist var:B) ]
)] log:isImpliedBy [ log:graph (
    [ log:triple ((var:C var:D) list:append var:A) ]
    [ log:triple ((var:E var:B) list:append var:C) ]
)].
```

These rules help interpret neighbor relationships (`:pair`) by leveraging `:sublist` and `list:append`.

---

## ❓ Query: Who Eats Fish?

```turtle
[ log:graph (
    [ log:triple (var:WHO :eats :fish) ]
)] log:impliesAnswer [ log:graph (
    [ log:triple (var:WHO :eats :fish) ]
)].
```

This query will return the entity bound to `var:WHO` — the person who owns the fish.

---

> **TIP:** The Zebra Puzzle is a classic constraint satisfaction problem. Eyelet models it using RDF lists, logical implication, and list-based reasoning patterns.

> **NOTE:** Variables like `var:A1`, `var:B`, etc., are universally quantified. The solution emerges from unifying constraints through logic inference.

> **Reference:** Based on the [*Zebra Puzzle*](https://en.wikipedia.org/wiki/Zebra_Puzzle) and modeled using Eyelet and N3 Logic.

