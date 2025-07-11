# 🧠 Turing Machine in Eyelet

This Eyelet reasoning program simulates a **Turing machine** using RDF Turtle and N3 Logic. It is a demonstration of **Turing completeness**, adapted from the [Prolog-based proof of Turing completeness](https://en.wikipedia.org/wiki/Prolog#Turing_completeness).

The machine encoded here performs **binary increment** — adding 1 to a binary number.

---

## 📚 Prefixes

```turtle
@prefix list: <http://www.w3.org/2000/10/swap/list#> .
@prefix log:  <http://www.w3.org/2000/10/swap/log#> .
@prefix var:  <http://www.w3.org/2000/10/swap/var#> .
@prefix :     <http://example.org/#> .
```

---

## 🖥️ Turing Machine Interpreter

These rules form the interpreter that defines how a Turing machine computes.

### 🟢 Starting Point

```turtle
# Start with empty input
[ log:graph (
    [ log:triple (() :compute var:OutTape) ]
)] log:isImpliedBy [ log:graph (
    [ log:triple (var:Machine :start var:I) ]
    [ log:triple ((var:I () "#" ()) :find var:OutTape) ]
)].
```

### 📜 Computation with Input Tape

```turtle
# Apply machine to input list
[ log:graph (
    [ log:triple (var:List :compute var:OutTape) ]
)] log:isImpliedBy [ log:graph (
    [ log:triple (var:List list:firstRest (var:Head var:Tail)) ]
    [ log:triple (var:Machine :start var:I) ]
    [ log:triple ((var:I () var:Head var:Tall) :find var:OutTape) ]
)].
```

---

## ⚙️ Transition Rules and Movement

### Step Execution

```turtle
[ log:graph (
    [ log:triple ((var:State var:Left var:Cell var:Right) :find var:OutTape) ]
)] log:isImpliedBy [ log:graph (
    [ log:triple ((var:State var:Cell var:Write var:Move) :tape var:Next) ]
    [ log:triple ((var:Move var:Left var:Write var:Right var:A var:B var:C) :move true) ]
    [ log:triple ((var:Next var:A var:B var:C) :continue var:OutTape) ]
)].
```

### Halting Condition

```turtle
# When state is :halt
[ log:graph (
    [ log:triple ((:halt var:Left var:Cell var:Right) :continue var:OutTape) ]
)] log:isImpliedBy [ log:graph (
    [ log:triple (var:Left :reverse var:R) ]
    [ log:triple (var:List list:firstRest (var:Cell var:Right)) ]
    [ log:triple ((var:R var:List) list:append var:OutTape) ]
)].
```

---

## 🔄 Tape Movement Logic

The tape is modeled as a triple of `(left, cell, right)` lists. Here are examples of left and right movement:

```turtle
# Move left
[ log:graph (
    [ log:triple ((:left var:List var:Cell var:Right var:Tail var:Head var:L) :move true) ]
)] log:isImpliedBy [ log:graph (
    [ log:triple (var:List list:firstRest (var:Head var:Tail)) ]
    [ log:triple (var:L list:firstRest (var:Cell var:Right)) ]
)].

# Move right
[ log:graph (
    [ log:triple ((:right var:Left var:Cell var:List var:L var:Head var:Tail) :move true) ]
)] log:isImpliedBy [ log:graph (
    [ log:triple (var:List list:firstRest (var:Head var:Tail)) ]
    [ log:triple (var:L list:firstRest (var:Cell var:Left)) ]
)].
```

---

## 🔁 Reverse List (Used for Halting Output)

```turtle
# Base case
[ log:graph (
    [ log:triple (() :reverse ()) ]
)] log:isImpliedBy true.

# Recursive reversal
[ log:graph (
    [ log:triple (var:List :reverse var:Reverse) ]
)] log:isImpliedBy [ log:graph (
    [ log:triple (var:List list:firstRest (var:Head var:Tail)) ]
    [ log:triple (var:Tail :reverse var:R) ]
    [ log:triple ((var:R (var:Head)) list:append var:Reverse) ]
)].
```

---

## ➕ Example Machine: Add 1 to a Binary Number

This simple Turing machine increments a binary number, where:

* `0` becomes `1`
* `1` becomes `0` with carry
* `#` is the tape end symbol
* `:halt` is the stop state

```turtle
:add1 :start 0.

(0 0 0 :right) :tape 0.
(0 1 1 :right) :tape 0.
(0 "#" "#" :left) :tape 1.
(1 0 1 :stop) :tape :halt.
(1 1 0 :left) :tape 1.
(1 "#" 1 :stop) :tape :halt.
```

---

## ❓ Query: Run Turing Machine on Binary Input

```turtle
[ log:graph (
    [ log:triple ((1 0 1 0 0 1) :compute var:A1) ]
    [ log:triple ((1 0 1 1 1 1) :compute var:A2) ]
    [ log:triple ((1 1 1 1 1 1) :compute var:A3) ]
    [ log:triple (() :compute var:A4) ]
)] log:impliesAnswer [ log:graph (
    [ log:triple ((1 0 1 0 0 1) :compute var:A1) ]
    [ log:triple ((1 0 1 1 1 1) :compute var:A2) ]
    [ log:triple ((1 1 1 1 1 1) :compute var:A3) ]
    [ log:triple (() :compute var:A4) ]
)].
```

Each query applies the machine to a binary input list and returns the incremented output list.

---

> **TIP:** This model encodes a complete Turing machine interpreter, showing how computation can be encoded declaratively in RDF.

> **NOTE:** The machine uses `list:firstRest` and `list:append` to simulate tape operations, and logical continuation for state transitions.

> **Reference:** Based on [Turing completeness](https://en.wikipedia.org/wiki/Prolog#Turing_completeness), reimagined with N3 Logic and Eyelet.

