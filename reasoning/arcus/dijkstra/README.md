# 🛣️ Dijkstra (Forward Search) – Eyelet Rule Demo

A logic (eyelet / N3) rendition of **Dijkstra’s shortest‑path algorithm** over a
small weighted, *undirected* graph.  
The rules manipulate *lists-as-queues* using the built‑ins in the `list:` and
`math:` vocabularies to iteratively expand the *cheapest* frontier first.

We query the shortest path and cost from **`:a` to `:f`**.

---

## 🗺️ Graph (edges)

```turtle
(:a :b) :edge 4 .
(:a :c) :edge 2 .
(:b :c) :edge 1 .
(:b :d) :edge 5 .
(:c :d) :edge 8 .
(:c :e) :edge 10 .
(:d :e) :edge 2 .
(:d :f) :edge 6 .
(:e :f) :edge 3 .
````

Edges are made **undirected** by a symmetric rule:

```turtle
[ log:graph ( [ log:triple ((var:A var:B) :edge var:W) ] ) ]
  log:implies
[ log:graph ( [ log:triple ((var:B var:A) :edge var:W) ] ) ].
```

---

## 🧠 Idea of the encoding

A *priority queue* of partial paths is represented as a **sorted list of
triples**:

```
((Cost Node Path)  (Cost2 Node2 Path2) ...)
```

The inductive predicate `:dijkstra2` holds over a tuple:

```
(Queue  Goal  Visited)
```

and produces `(ReversePath  FinalCost)` when the cheapest head has reached the
goal.  Otherwise it:

1. Pops the cheapest `(Cost Node Path)`.
2. Generates **new frontier entries** for each unvisited neighbor:

   * NewCost = Cost + Weight
   * New entry = `(NewCost Neighbor (Node Path))`
3. Appends these entries to the tail, re‐sorts the queue, and
4. Recurse with updated visited set `(Node Visited)`.

When the goal surfaces at the head, auxiliary list unrolling rules extract
`Cost`, `Goal`, and accumulated `Path`, then reverse it to produce the output
path (start → goal) for the top‑level `:dijkstra` predicate.

---

## 🔧 Key rule fragments

Top‑level wrapper (user‑friendly interface):

```turtle
# (Start Goal) :dijkstra (Path Cost)
[ log:graph (
    [ log:triple ((var:Start var:Goal) :dijkstra (var:Path var:Cost)) ]
) ] log:isImpliedBy [ log:graph (
    [ log:triple ((((0 var:Start)) var:Goal ()) :dijkstra2 (var:RevPath var:Cost)) ]
    [ log:triple (var:RevPath list:reverse var:Path) ]
) ].
```

Recursive expansion (core step – simplified view):

```turtle
[ log:graph (
    [ log:triple ((var:Queue var:Goal var:Visited) :dijkstra2 (var:ResultPath var:ResultCost)) ]
) ] log:isImpliedBy [ log:graph (
    # dequeue cheapest
    [ log:triple (var:Queue list:firstRest (var:Head var:Tail)) ]
    [ log:triple (var:Head  list:firstRest (var:Cost var:Rest1)) ]
    [ log:triple (var:Rest1 list:firstRest (var:Node var:Path)) ]

    # generate neighbors (comprehension)
    [ log:triple ((var:Gen [ log:graph (
        [ log:triple (var:Gen list:firstRest (var:NewCost var:Rest2)) ]
        [ log:triple (var:Rest2 list:firstRest (var:Neighbor var:Rest3)) ]
        [ log:triple (var:Rest3 list:firstRest (var:Node var:Path)) ]
        [ log:triple ((var:Node var:Neighbor) :edge var:W) ]
        [ log:triple (var:Visited list:notMember var:Neighbor) ]
        [ log:triple ((var:Cost var:W) math:sum var:NewCost) ]
    )] var:Neighbors) log:collectAllIn var:Scope) ]

    # merge + sort new queue
    [ log:triple ((var:Tail var:Neighbors) list:append var:Queue2) ]
    [ log:triple (var:Queue2 list:sort var:Sorted) ]

    # extend visited
    [ log:triple (var:Visited list:firstRest (var:Node var:Visited2)) ]

    # recurse
    [ log:triple ((var:Sorted var:Goal var:Visited2) :dijkstra2 (var:ResultPath var:ResultCost)) ]
) ].
```

Goal detection + extraction:

```turtle
[ log:graph (
    [ log:triple ((var:Q var:Goal var:V) :dijkstra2 (var:Path var:Cost)) ]
) ] log:isImpliedBy [ log:graph (
    [ log:triple (var:Q list:firstRest (var:Head var:_)) ]
    [ log:triple (var:Head list:firstRest (var:Cost var:R1)) ]
    [ log:triple (var:R1 list:firstRest (var:Goal var:R2)) ]
    [ log:triple (var:R2 list:firstRest (var:Goal var:Path)) ]
) ].
```

---

## ❓ Query

```turtle
[ log:graph ( [ log:triple ((:a :f) :dijkstra (var:Path var:Cost)) ] ) ]
  log:impliesAnswer
[ log:graph ( [ log:triple ((:a :f) :dijkstra (var:Path var:Cost)) ] ) ].
```

---

## ▶️ Run

```bash
eye --quiet --nope dijkstra.ttl
```

**Expected answer (one possible formatting):**

```turtle
{ (:a :f) :dijkstra ((:a :c :b :d :e :f) 14). }
```

(Shortest path `a → c → b → d → e → f` with total cost **14**.)

---

## 🧪 Try it yourself

* Add a cheaper shortcut edge, e.g. `(:c :f) :edge 5 .` – rerun and observe the
  path/cost update.
* Remove an edge to see the queue exploration change (add `--why` for proof
  trace).
* Introduce multiple queries: just add more `(Start Goal) :dijkstra` answer
  rules; the generic machinery handles each independently.

---

## 💡 Notes

* `list:sort` keeps the queue ordered by (Cost …) tuples so expansion is always
  optimal-first (Dijkstra property).
* The comprehension with `log:collectAllIn` builds all admissible neighbor
  extensions *in one logical step*.
* Termination follows from a finite graph & monotonically growing visited set.

