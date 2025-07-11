# 🚀 Goal-Driven Parallel Sequences in arcus

This example demonstrates **goal-driven reasoning** with **state transitions** using **Notation3 (N3)**. Inspired by **linear logic** and planning systems, it models how propositions (facts) **change state** as transitions are applied — enabling **adaptive, cost-aware pathfinding** toward a goal.

> 🧠 Inspired by:  
> - [Linear Logic and Inference (CMU Lecture)](https://www.cs.cmu.edu/~fp/courses/15816-s12/lectures/01-inference.pdf)  
> - [wstLogic – Weighted State Transition Logic](https://github.com/hongsun502/wstLogic)

---

## 📘 Prefixes

```ttl
@prefix math: <http://www.w3.org/2000/10/swap/math#>.
@prefix list: <http://www.w3.org/2000/10/swap/list#>.
@prefix log: <http://www.w3.org/2000/10/swap/log#>.
@prefix gps: <http://eyereasoner.github.io/eye/reasoning/gps/gps-schema#>.
@prefix var: <http://www.w3.org/2000/10/swap/var#>.
@prefix : <http://example.org/#>.
````

---

## 📍 Goal: Find a Feasible Path to a Target State

This system allows querying:

* From an **initial state**
* To a **goal state**
* Subject to constraints on:

  * **Duration**
  * **Cost**
  * **Belief** (confidence)
  * **Comfort** (quality)
  * **Maximum stage count**

---

## 🔁 Core Logic: Path-Finding Rules

The logic encodes:

### 1. **Recursive search** through transitions:

* `gps:findpath` calls `gps:findpaths`
* Each step extends the current path, updates metrics, and moves to the next state

### 2. **Stage counting**:

* Tracks how many transitions differ in context (e.g., from different maps)
* Uses `gps:stagecount` to control plan depth

### 3. **Cost and constraint tracking**:

* Each step adds:

  * `duration += d`
  * `cost += c`
  * `belief *= b`
  * `comfort *= c`
* Valid paths are filtered based on min/max thresholds

---

## 🗺️ Example: Map of Belgium

Transitions define how the system evolves:

| From     | To       | Action                   | Duration | Cost  | Belief | Comfort |
| -------- | -------- | ------------------------ | -------- | ----- | ------ | ------- |
| Gent     | Brugge   | `:drive_gent_brugge`     | 1500.0   | 0.006 | 0.96   | 0.99    |
| Gent     | Kortrijk | `:drive_gent_kortrijk`   | 1600.0   | 0.007 | 0.96   | 0.99    |
| Kortrijk | Brugge   | `:drive_kortrijk_brugge` | 1600.0   | 0.007 | 0.96   | 0.99    |
| Brugge   | Oostende | `:drive_brugge_oostende` | 900.0    | 0.004 | 0.98   | 1.00    |

Initial state:

```ttl
:i1 :location :Gent.
```

---

## ❓ Query: Reach Oostende

```ttl
[ log:graph (
  [ log:triple (() gps:findpath (
      [ log:graph ([ log:triple (var:S :location :Oostende) ])]
      var:PATH var:DURATION var:COST var:BELIEF var:COMFORT
      (5000.0 5.0 0.2 0.4 1)
  ))]
)] log:impliesAnswer [ log:graph (
  [ log:triple (var:S gps:path (var:PATH var:DURATION var:COST var:BELIEF var:COMFORT))]
)].
```

This requests a path from **current state** to a **goal state** where the agent is in **Oostende**, within defined constraints.

The system will backtrack valid transitions, compute metrics at each step, and return a viable path if one exists.

---

## ✅ Conclusion

This GPS system demonstrates:

* **Linear logic**: resources (states) are consumed and produced during transitions
* **Recursive path planning** under constraints
* **Weighted evaluation**: paths evaluated based on confidence, comfort, cost, etc.
* **Goal-directed search** with maximum control over stages and metrics

> 🧠 This is a high-level example of symbolic **adaptive planning**, suitable for modeling intelligent agents, automated workflows, or predictive routing systems.

