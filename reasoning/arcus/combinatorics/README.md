# 🧩 Combinations & Permutations in EYE

This example expresses two classic list operations—**combinations** and **permutations**—as recursive rules in Turtle/N3 for the EYE reasoner.

* **Combination** `(I As) :combination Bs`  – choose `I` items from list `As`, returning an *ordered* result list `Bs` (sorted ascending).
* **Permutation** `As :permutation Bs` – reorder list `As` into list `Bs` (all orderings).

---

## 📐 Combination Rules

```turtle
# 0‑combination of any list is the empty list
[ log:graph (
    [ log:triple ((0 var:A) :combination ()) ]
)] log:isImpliedBy true.

# I>0: pick an element B, recurse on I‑1 and the remaining Cs
[ log:graph (
    [ log:triple ((var:I var:As) :combination var:Bs) ]
)] log:isImpliedBy [ log:graph (
    [ log:triple (var:I math:greaterThan 0) ]
    [ log:triple (var:As list:select (var:B var:Cs)) ]  # split into chosen B and rest Cs
    [ log:triple ((var:I 1) math:difference var:J) ]   # I‑1 → J
    [ log:triple ((var:J var:Cs) :combination var:Ds) ]
    [ log:triple (var:Es list:firstRest (var:B var:Ds)) ]  # prepend B to Ds
    [ log:triple (var:Es list:sort var:Bs) ]               # ensure sorted order
)].
```

`list:select` nondeterministically chooses one element (`B`) and returns the remainder (`Cs`).

---

## 🔄 Permutation Rules

```turtle
() :permutation ().   # empty list permutes to empty list

[ log:graph (
    [ log:triple (var:As :permutation var:Bs) ]
)] log:isImpliedBy [ log:graph (
    [ log:triple (var:Bs list:firstRest (var:B var:Cs)) ]     # pick B as first of Bs
    [ log:triple (var:As list:select (var:B var:Ds)) ]        # remove B from As
    [ log:triple (var:Ds :permutation var:Cs) ]               # permute the rest
)].
```

The rule builds each permutation by choosing the first element (`B`) and permutating the remaining sub‑list recursively.

---

## ❓ Queries

```turtle
# All I‑element combinations from list (1 2 3 4 5), where I ranges 0..5 via log:repeat
[ log:graph (
    [ log:triple (6 log:repeat var:I) ]           # generate I = 0..5
    [ log:triple ((var:I (1 2 3 4 5)) :combination var:C) ]
)] log:impliesAnswer [ log:graph (
    [ log:triple ((var:I (1 2 3 4 5)) :combination var:C) ]
)].

# All permutations of list (1 2 3 4 5)
[ log:graph (
    [ log:triple ((1 2 3 4 5) :permutation var:P) ]
)] log:impliesAnswer [ log:graph (
    [ log:triple ((1 2 3 4 5) :permutation var:P) ]
)].
```

---

## ▶️ Running the Program

```bash
eye --quiet --nope combinatorics.ttl
```

Remove `--nope` to inspect the proof steps.

---

## 🧠 Summary

This example demonstrates how EYE can perform **nondeterministic list manipulation** via backtracking (`list:select`) and recursion—yielding classic combinatorial results entirely through declarative logic.

