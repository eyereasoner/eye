# 🛣️ Graph Path Traversal in EYE

This example demonstrates simple **graph-based reasoning** in Turtle/N3 using the EYE reasoner.
We define a network of cities and roads in France and compute *reachable paths* via a transitive rule.

## 🗺️ Road Network

We define a set of one-way connections between cities:

```turtle
:paris :oneway :orleans.
:paris :oneway :chartres.
:paris :oneway :amiens.
:orleans :oneway :blois.
:orleans :oneway :bourges.
:blois :oneway :tours.
:chartres :oneway :lemans.
:lemans :oneway :angers.
:lemans :oneway :tours.
:angers :oneway :nantes.
```

## 🧠 Reasoning Rules

### 🧩 Subproperty Rule: `:oneway ⊆ :path`

```turtle
[ log:graph (
    [ log:triple (var:A :oneway var:B)]
)] log:implies [ log:graph (
    [ log:triple (var:A :path var:B)]
)].
```

### 🔁 Transitive Rule: Path Composition

```turtle
[ log:graph (
    [ log:triple (var:A :path var:B)]
    [ log:triple (var:B :path var:C)]
)] log:implies [ log:graph (
    [ log:triple (var:A :path var:C)]
)].
```

## ❓ Query

Find all cities from which one can reach Nantes by following `:path`:

```turtle
[ log:graph (
    [ log:triple (var:A :path :nantes)]
)] log:impliesAnswer [ log:graph (
    [ log:triple (var:A :path :nantes)]
)].
```

## ▶️ Running the Program

Run the path-traversal logic with:

```bash
eye --quiet --nope graph.ttl
```

To see the full reasoning proof, drop the `--nope` flag:

```bash
eye --quiet graph.ttl
```

## 🧭 Summary

This example shows how to:

* Encode graph edges using N3 triples
* Derive transitive reachability
* Use EYE for forward-chaining and answer generation

You can extend this to explore undirected graphs, cycles, or labeled edges with additional rules.

