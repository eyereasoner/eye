# Collatz Reasoning in eyelet

This example demonstrates how **eyelet** models the **Collatz sequence** using RDF Turtle and N3 Logic. It shows a base case, recursive step, and a query using functional constructs and rule-based reasoning.

---

### 📚 Prefix Declarations

```turtle
@prefix rdf:  <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix math: <http://www.w3.org/2000/10/swap/math#> .
@prefix list: <http://www.w3.org/2000/10/swap/list#> .
@prefix log:  <http://www.w3.org/2000/10/swap/log#> .
@prefix var:  <http://www.w3.org/2000/10/swap/var#> .
@prefix :     <https://eyereasoner.github.io/ns#> .
```

---

### 🔁 Base Case: Collatz of a Number is the Number Itself (with a Tuple)

```turtle
[ log:graph (
    [ log:triple (var:N :collatz (var:N (var:N))) ]
)] log:isImpliedBy [ log:graph (
    [ log:triple (true log:callWithCut true) ]
)].
```

* This rule encodes the **base case** of the Collatz sequence.
* If a value exists, it trivially maps to itself (as a tuple with repetition).

---

### 🔁 Recursive Case: Even & Odd Handling with `log:ifThenElseIn`

```turtle
[ log:graph (
    [ log:triple (var:N0 :collatz (var:N var:M)) ]
)] log:isImpliedBy [ log:graph (

    # if (N0 mod 2 == 0) then N1 = N0 / 2
    [ log:triple ((
        [ log:graph (
            [ log:triple ((var:N0 2) math:remainder 0) ]
        ) ]
        [ log:graph (
            [ log:triple ((var:N0 2) math:integerQuotient var:N1) ]
        ) ]
        [ log:graph (
            [ log:triple ((3 var:N0) math:product var:N2) ]
            [ log:triple ((var:N2 1) math:sum var:N1) ]
        ) ]
    ) log:ifThenElseIn var:SCOPE) ]

    # Recursive call
    [ log:triple (var:N1 :collatz (var:N var:J)) ]

    # Build result list: M = (N0 . J)
    [ log:triple (var:M list:firstRest (var:N0 var:J)) ]

)].
```

* This rule captures the **recursive step**:

  * Uses `log:ifThenElseIn` to differentiate even and odd numbers.
  * Builds the result list of the sequence with `list:firstRest`.

---

### ❓ Query: Find Collatz Sequence Ending in 1 After Incrementing N0

```turtle
[ log:graph (
    [ log:triple (1000 log:repeat var:N0) ]
    [ log:triple ((var:N0 1) math:sum var:N) ]
    [ log:triple (var:N :collatz (1 var:M)) ]
)] log:impliesAnswer [ log:graph (
    [ log:triple (var:N :collatz (1 var:M)) ]
)].
```

* This **query** asks:

  * For a number `N0`, repeat a computation 1000 times.
  * Add 1 to `N0` to get `N`.
  * Does `N`'s Collatz sequence eventually reach `1` with result list `M`?

---

> \[!TIP]
> This model uses `log:ifThenElseIn` for conditional logic, `math:` functions for arithmetic, and `list:` terms to build recursive sequences—all expressed in RDF Turtle.

> \[!NOTE]
> The `var:` prefix (universally quantified) ensures variables can be reused across patterns and rules consistently.

