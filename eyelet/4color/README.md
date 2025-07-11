# 🗺️ Four Color Map Coloring in Eyelet

This Eyelet reasoning model tests the **Four Color Theorem** — the idea that **any planar map** can be colored using just four colors such that **no two neighboring regions share the same color**.

This example encodes a map of European Union countries and infers a valid 4-coloring using **graph traversal** and **logical constraints**.

---

## 📚 Prefixes

```turtle
@prefix list: <http://www.w3.org/2000/10/swap/list#> .
@prefix log:  <http://www.w3.org/2000/10/swap/log#> .
@prefix var:  <http://www.w3.org/2000/10/swap/var#> .
@prefix :     <http://example.org/#> .
```

---

## 🗺️ EU Map Representation

The map is encoded as a list of countries with their neighboring countries using the `:neighbours` predicate. For example:

```turtle
:Belgium :neighbours (:France :Netherlands :Luxemburg :Germany) .
:France :neighbours (:Spain :Belgium :Luxemburg :Germany :Italy) .
:Germany :neighbours (:Netherlands :Belgium :Luxemburg :Denmark :France :Austria :Poland :Czech_Republic) .
...
```

Each country is linked to its geographic neighbors, allowing logic to constrain valid colorings.

---

## 🧠 Coloring Logic

### Rule: Start Coloring the Map

```turtle
[ log:graph (
    [ log:triple (var:MAP :color var:PLACES) ]
)] log:isImpliedBy [ log:graph (
    [ log:triple (((var:PLACE var:X) [ log:graph (
        [ log:triple (var:PLACE :neighbours var:Y) ]
    ) ] var:PLACES) log:collectAllIn var:SCOPE) ]
    [ log:triple (var:PLACES :places true) ]
    [ log:triple (true log:callWithCut true) ]
)].
```

This rule triggers the process of generating a map coloring based on neighbor constraints.

---

### Rule: Valid Placement of Colors

```turtle
[ log:graph (
    [ log:triple (var:PLACES :places true) ]
)] log:isImpliedBy [ log:graph (
    [ log:triple (var:PLACES list:firstRest ((var:PLACE var:COLOR) var:TAIL)) ]
    [ log:triple (var:TAIL :places true) ]
    [ log:triple (var:PLACE :neighbours var:NEIGHBOURS) ]
    [ log:triple ((:red :green :blue :yellow) list:member var:COLOR) ]
    [ log:triple ((1 [ log:graph (
        [ log:triple (var:TAIL list:member (var:NEIGHBOUR var:COLOR)) ]
        [ log:triple (var:NEIGHBOURS list:member var:NEIGHBOUR) ]
    ) ] ()) log:collectAllIn var:SCOPE) ]
)].
```

This rule ensures:

* Each place is assigned one of four colors
* No neighbor shares the same color
* Remaining places are colored recursively

---

### Base Case: End of Coloring

```turtle
[ log:graph (
    [ log:triple (() :places true) ]
)] log:isImpliedBy true.
```

When no more places are left to color, the coloring is considered complete.

---

## ❓ Query: Color the EU Map

```turtle
[ log:graph (
    [ log:triple (:mapEU :color var:PLACES) ]
)] log:impliesAnswer [ log:graph (
    [ log:triple (:mapEU :color var:PLACES) ]
)].
```

This query asks Eyelet to return a valid assignment of four colors to countries in the `:mapEU`.

---

> **TIP:** The logic uses `log:collectAllIn` to group colors assigned across recursive patterns and `list:member` to enforce valid color choices.

> **NOTE:** The program simulates a **backtracking search** in logic form — exploring color combinations until constraints are satisfied.

> **Reference:** Inspired by the [Four Color Theorem](https://en.wikipedia.org/wiki/Four_color_theorem), this model demonstrates constraint satisfaction in Eyelet and N3 Logic.

