# 🔄 Basic Monadic Benchmark (BMB) in EYE

The **Basic Monadic Benchmark (BMB)** is a classic stress‑test for RDF rule engines. 
It measures how well a reasoner handles **long chains** of monadic triples and deep
rule recursion. The task: **detect a 10‑step cycle** over any predicate `R` in a large
random graph.

Input graphs of size `10^n` triples can be generated with
[`graphgen.n3`](http://josd.github.io/bmb/graphgen.n3).

---

## 🗃️ Dataset

`graphgen.n3` emits N triples like:

```turtle
:i30 :i12 :i33 .
:i61 :i29 :i42 .
:i57 :i46 :i87 .
# … up to 10ⁿ triples
```

Every URI of the form `:iX` is *both* potential subject, predicate, and object –
a true monadic graph.

---

## 🧠 Rule Logic

The single backward rule looks for a closed chain of *exactly ten* hops
(`D0 → D1 → … → D9 → D0`) along **one and the same** relation `R`:

```turtle
[ log:graph (
    [ log:triple (var:D0 var:R var:D1)]
    [ log:triple (var:D1 var:R var:D2)]
    [ log:triple (var:D2 var:R var:D3)]
    [ log:triple (var:D3 var:R var:D4)]
    [ log:triple (var:D4 var:R var:D5)]
    [ log:triple (var:D5 var:R var:D6)]
    [ log:triple (var:D6 var:R var:D7)]
    [ log:triple (var:D7 var:R var:D8)]
    [ log:triple (var:D8 var:R var:D9)]
    [ log:triple (var:D9 var:R var:D0)]
)] log:impliesAnswer [ log:graph (
    [ log:triple (var:R :cycle (var:D0 var:D1 var:D2 var:D3 var:D4 var:D5 var:D6 var:D7 var:D8 var:D9 var:D0))]
)].
```

The answer lists the predicate `R` and the discovered 10‑node cycle as an RDF
`list`.

---

## ❓ Query Strategy

No explicit query block is needed—EYE treats the above
`log:impliesAnswer` rule as both **pattern** and **answer template**.
It will output every cycle it can prove.

---

## ▶️ Running the Benchmark

```bash
eye --quiet --nope basic-monadic.ttl        # fastest; suppresses proof
```

To **see the proof**, simply drop `--nope`:

```bash
eye --quiet basic-monadic.ttl
```

For a 10⁶‑triple graph, expect several seconds on a modern laptop; larger
graphs stress memory and indexing.

---

## 🧪 Tips for Accurate Benchmarks

1. **Isolate I/O** – pipe input via `cat` to avoid disk parsing overhead.
2. **Scale linearly** – test at 10³, 10⁴, 10⁵ … to observe complexity.

---

## 📝 Summary

BMB is a minimal yet brutal workload: one rule, huge data, deep joins.
It highlights indexing strategy, join ordering, and tail‑recursion handling in
rule engines. With EYE, you can experiment by:

* Changing the cycle length (add more `var:Dn` lines)
* Requesting shorter/longer paths
* Parallelising input via multiple `@prefix` segments

