# 🗼 Towers of Hanoi in arcus

This example models the classic **Towers of Hanoi** problem using RDF Turtle and N3 Logic, inspired by *The Art of Prolog* by Leon Sterling and Ehud Shapiro.

It recursively computes the sequence of moves required to transfer `N` disks from one peg to another, using a third peg as intermediary.

---

### 📚 Prefix Declarations

```turtle
@prefix math: <http://www.w3.org/2000/10/swap/math#> .
@prefix list: <http://www.w3.org/2000/10/swap/list#> .
@prefix log:  <http://www.w3.org/2000/10/swap/log#> .
@prefix var:  <http://www.w3.org/2000/10/swap/var#> .
@prefix :     <http://example.org/#> .
```

---

### 🔁 Recursive Rule: Move `N` Disks

```turtle
[ log:graph (
    [ log:triple ((var:N var:X var:Y var:Z) :moves var:M) ]
)] log:isImpliedBy [ log:graph (

    # N must be greater than 1
    [ log:triple (var:N math:greaterThan 1) ]

    # N1 = N - 1
    [ log:triple ((var:N 1) math:difference var:N1) ]

    # Move N1 disks from X to Z using Y
    [ log:triple ((var:N1 var:X var:Z var:Y) :moves var:M1) ]

    # Move N1 disks from Z to Y using X
    [ log:triple ((var:N1 var:Z var:Y var:X) :moves var:M2) ]

    # Append moves: M = M1 ++ [(X, Y)] ++ M2
    [ log:triple ((var:M1 ((var:X var:Y)) var:M2) list:append var:M) ]

)].
```

* This rule breaks the problem into:

  1. Move `N-1` disks to intermediate peg
  2. Move the largest disk
  3. Move `N-1` disks to the destination peg
* The moves are collected using `list:append`.

---

### 🧱 Base Case: Move One Disk

```turtle
[ log:graph (
    [ log:triple ((1 var:X var:Y var:Z) :moves ((var:X var:Y))) ]
)] log:isImpliedBy true.
```

* Moving a single disk from `X` to `Y` is a direct move.

---

### ❓ Query: Compute Moves for 6 Disks

```turtle
[ log:graph (
    [ log:triple ((6 :left :right :center) :moves var:M) ]
)] log:impliesAnswer [ log:graph (
    [ log:triple ((6 :left :right :center) :moves var:M) ]
)].
```

* This query asks for the **full move sequence** to transfer 6 disks from `:left` to `:right` using `:center` as intermediary.

---

> \[!TIP]
> The recursive logic in N3 mirrors the classical recursive definition of the Towers of Hanoi solution.

> \[!NOTE]
> Moves are represented as lists of pairs `(X Y)`, and combined using `list:append`.

**Reference:** Based on [_The Art of Prolog_](https://en.wikipedia.org/wiki/The_Art_of_Prolog) and N3 logic modeling using arcus.

