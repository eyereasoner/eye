# ♛ N-Queens Problem in EYE

This example encodes the classic **N-Queens problem** in Turtle/N3 using the EYE reasoner.
The task is to place N queens on an N×N chessboard such that no two queens threaten each other.

Inspired by a [Prolog solution](https://hanslen.github.io/2016/05/02/AI-problem-N-queens-problem-%E2%80%93-solved-in-prolog/), the program uses logic rules, list recursion, and arithmetic.

---

## 🔢 Range Constructor

A helper to generate a range list from `I` to `J`:

```turtle
[ log:graph (
    [ log:triple ((var:J var:J) :range (var:J)) ]
)] log:isImpliedBy true.

[ log:graph (
    [ log:triple ((var:I var:J) :range var:INs) ]
)] log:isImpliedBy [ log:graph (
    [ log:triple (var:INs list:firstRest (var:I var:Ns)) ]
    [ log:triple (var:I math:lessThan var:J) ]
    [ log:triple ((var:I 1) math:sum var:I1) ]
    [ log:triple ((var:I1 var:J) :range var:Ns) ]
)].
```

---

## ♟️ Queen Placement Logic

Queens are placed recursively using backtracking. We build a list of positions `Qs`:

```turtle
# Top-level entry point
[ log:graph (
    [ log:triple (var:N :queens var:Qs) ]
)] log:isImpliedBy [ log:graph (
    [ log:triple ((1 var:N) :range var:Us) ]
    [ log:triple ((var:Us ()) :queens3 var:Qs) ]
)].

# Base case
[ log:graph (
    [ log:triple ((() var:Qs) :queens3 var:Qs) ]
)] log:isImpliedBy true.

# Recursive placement
[ log:graph (
    [ log:triple ((var:Us var:Ps) :queens3 var:Qs) ]
)] log:isImpliedBy [ log:graph (
    [ log:triple (var:Us list:select (var:Q var:Us1)) ]
    [ log:triple ((
        [ log:graph ([ log:triple (var:Q :attack var:Ps) ]) ]
        false true
    ) log:ifThenElseIn var:SCOPE) ]
    [ log:triple (var:QPs list:firstRest (var:Q var:Ps)) ]
    [ log:triple ((var:Us1 var:QPs) :queens3 var:Qs) ]
)].
```

---

## 🚫 Attack Constraints

Defines when a queen `Q` attacks any other in list `Qs`, based on diagonal rules:

```turtle
# Entry rule
[ log:graph (
    [ log:triple (var:Q :attack var:Qs) ]
)] log:isImpliedBy [ log:graph (
    [ log:triple ((var:Q 1) :attack3 var:Qs) ]
)].

# Diagonal attack by sum
[ log:graph (
    [ log:triple ((var:X var:N) :attack3 var:YYs) ]
)] log:isImpliedBy [ log:graph (
    [ log:triple (var:YYs list:firstRest (var:Y var:Ys)) ]
    [ log:triple ((var:Y var:N) math:sum var:X) ]
)].

# Diagonal attack by difference
[ log:graph (
    [ log:triple ((var:X var:N) :attack3 var:YYs) ]
)] log:isImpliedBy [ log:graph (
    [ log:triple (var:YYs list:firstRest (var:Y var:Ys)) ]
    [ log:triple ((var:Y var:N) math:difference var:X) ]
)].

# Recurse through rest of list
[ log:graph (
    [ log:triple ((var:X var:N) :attack3 var:YYs) ]
)] log:isImpliedBy [ log:graph (
    [ log:triple (var:YYs list:firstRest (var:Y var:Ys)) ]
    [ log:triple ((var:N 1) math:sum var:N1) ]
    [ log:triple ((var:X var:N1) :attack3 var:Ys) ]
)].
```

---

## ❓ Query

Ask for a solution to the 8-queens puzzle:

```turtle
[ log:graph (
    [ log:triple (8 :queens var:Qs) ]
)] log:impliesAnswer [ log:graph (
    [ log:triple (8 :queens var:Qs) ]
)].
```

---

## ▶️ Running the Program

Execute using EYE:

```bash
eye --quiet --nope n-queens.ttl
```

For proof/debugging:

```bash
eye --quiet n-queens.ttl
```

---

## 🧠 Summary

This example demonstrates nondeterministic search and constraint reasoning in EYE. The use of recursive list handling, numerical checks, and `log:ifThenElseIn` enables compact modeling of combinatorial problems like N-Queens.

