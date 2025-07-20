# 🧮 Fibonacci Numbers in EYE

This example computes **Fibonacci numbers** using recursive backward rules in Notation3/Turtle syntax.
The logic follows the mathematical definition, encoded using `log:isImpliedBy` for goal-directed reasoning.

See the full definition here: [Wikipedia – Fibonacci Number](https://en.wikipedia.org/wiki/Fibonacci_number)

## 📐 Rule Logic

We define a `:fibonacci` predicate to compute the Fibonacci number for a given non-negative integer.

### 🪄 Rule: Forward to Fib-State

```turtle
[ log:graph (
    [ log:triple (var:X :fibonacci var:Y)]
)] log:isImpliedBy [ log:graph (
    [ log:triple ((var:X 0 1) :fib var:Y)]
)].
```

### 🧱 Base Cases

```turtle
[ log:graph (
    [ log:triple ((0 var:A var:B) :fib var:A)]
)] log:isImpliedBy true.

[ log:graph (
    [ log:triple ((1 var:A var:B) :fib var:B)]
)] log:isImpliedBy true.
```

### 🔁 Recursive Case

```turtle
[ log:graph (
    [ log:triple ((var:X var:A var:B) :fib var:Y)]
)] log:isImpliedBy [ log:graph (
    [ log:triple (var:X math:greaterThan 1)]
    [ log:triple ((var:X 1) math:difference var:J)]
    [ log:triple ((var:A var:B) math:sum var:K)]
    [ log:triple ((var:J var:B var:K) :fib var:Y)]
)].
```

## ❓ Queries

We can query the Fibonacci number for several values:

```turtle
[ log:graph ( [ log:triple (0 :fibonacci var:Y)] ) ] log:impliesAnswer [ log:graph ( [ log:triple (0 :fibonacci var:Y)] ) ].
[ log:graph ( [ log:triple (1 :fibonacci var:Y)] ) ] log:impliesAnswer [ log:graph ( [ log:triple (1 :fibonacci var:Y)] ) ].
[ log:graph ( [ log:triple (6 :fibonacci var:Y)] ) ] log:impliesAnswer [ log:graph ( [ log:triple (6 :fibonacci var:Y)] ) ].
[ log:graph ( [ log:triple (91 :fibonacci var:Y)] ) ] log:impliesAnswer [ log:graph ( [ log:triple (91 :fibonacci var:Y)] ) ].
[ log:graph ( [ log:triple (283 :fibonacci var:Y)] ) ] log:impliesAnswer [ log:graph ( [ log:triple (283 :fibonacci var:Y)] ) ].
[ log:graph ( [ log:triple (3674 :fibonacci var:Y)] ) ] log:impliesAnswer [ log:graph ( [ log:triple (3674 :fibonacci var:Y)] ) ].
```

## ▶️ Running the Program

Run the program using EYE:

```bash
eye --quiet --nope fibonacci.ttl
```

To see the full proof trace, omit `--nope`:

```bash
eye --quiet fibonacci.ttl
```

⚠️ Higher numbers like 283 or 3674 may be slow due to deep recursion. Consider testing smaller values first.

## 🧠 Summary

This example showcases recursive reasoning with arithmetic using `math:` built-ins in EYE. The backward rule approach allows goal-directed logic programming, ideal for problems like Fibonacci where we can recurse to base cases.

