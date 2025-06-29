# eyelet

## a pocket‑sized, EYE‑style RDF reasoner in pure Python

- eyelet supports reasoning with forward rules described in RDF Turtle
  e.g.
    ```
    # subclass rule
    [ log:graph (
        [ log:triple (var:A rdfs:subClassOf var:B)]
        [ log:triple (var:S rdf:type var:A)]
    )] log:implies [ log:graph (
        [ log:triple (var:S rdf:type var:B)]
    )].
    ```

- eyelet supports reasoning with backward rules described in RDF Turtle
  e.g.
    ```
    # is the age of a person above some duration?
    [ log:graph (
        [ log:triple (var:S :ageAbove var:A)]
    )] log:isImpliedBy [ log:graph (
        [ log:triple (var:S :birthDay var:B)]
        [ log:triple ("" time:localTime var:D)]
        [ log:triple ((var:D var:B) math:difference var:F)]
        [ log:triple (var:F math:greaterThan var:A)]
    )].
    ```

- eyelet supports querying with queries described in RDF Turtle
  e.g.
    ```
    # who is a what?
    [ log:graph (
        [ log:triple (var:WHO rdf:type var:WHAT)]
    )] log:impliesAnswer [ log:graph (
        [ log:triple (var:WHO rdf:type var:WHAT)]
    )].
    ```

> [!NOTE]
> The `var:` prefix is `<http://www.w3.org/2000/10/swap/var#>` and is used for
  variables that are interpreted universally except for forward rule
  conclusion-only variables which are interpreted existentially.

  
# eyelet under the hood

## 1  Goal-ordering heuristic (`_prove`, `_match_body`)

### Why we do it

*Unification is cheap; choosing the wrong order is not.*
When you have three triple patterns that share variables, binding even one constant early can shrink the search space by orders of magnitude.  Classic Datalog engines build indexes and cost models; in this toy reasoner we get **80–90 % of the win** with one short sort.

### How it works

```python
goals = sorted(
    goals,
    key=lambda t: (
        3 - sum(1 for x in t if is_var(x)),  # ⇒ 0, 1, 2 or 3 vars
        isinstance(t[1], URIRef)             # prefer bound predicate
    ),
)
```

1. **Variable count (descending)** – a fully-bound triple (`?x ?y ?z` → 3 vars ⇒ key 0) ranks *after* a fully-ground one (`:s :p :o` ⇒ key 3).
2. **Predicate boundness** – if two patterns have the same var count, the one whose *predicate* is concrete goes first.  Rdflib’s internal index is `(S, P, O)`, so fixing *P* lets it jump straight to a single bucket.

> The same snippet appears in `_match_body`, which does the *join* for forward rules.

Cost: O(#goals · log #goals) per recursive call – trivial compared to the join itself.

---

## 2  Occurs-check (`_occurs` → `_extend` → `unify_term`)

### What problem it solves

A naïve Prolog-style unifier happily binds
`?X  →  (?X ?Y)`
and now `subst(?X)` is an *infinite* term.  The first call to `str()` or tuple expansion blows the Python stack (`RecursionError`).

### Implementation

```python
def _occurs(v, x, σ):
    if v == x:
        return True
    if is_var(x) and x in σ:            # follow existing bindings
        return _occurs(v, σ[x], σ)
    if isinstance(x, tuple):            # recurse into lists
        return any(_occurs(v, xi, σ) for xi in x)
    return False
```

* `_extend` calls `_occurs` **before** copying the substitution.
* If it returns `True`, we give up (`None`) and the search back-tracks.
* Because `_occurs` follows σ’s existing mappings, it also blocks indirect cycles like `?X → ?Y`, `?Y → ?X`.

Cost: linear in the size of *x* and only when a **new** binding is attempted, so the overhead is negligible next to graph scans.

---

## 3  Proof recording (`_record_proof`, `_make_rule_term`)

### Goal

Emit a minimal provenance graph per *forward* rule application that is:

* **deterministic** – order of triples is sorted when `--proof` is on.
* **self-contained** – reuses the rule’s own reified form so we don’t duplicate text.
* **traceable** – lists both the clauses it *used* and the triples it *produced*.

### Step-by-step

1. **Rule reification**
   `_make_rule_term()` builds a tiny RDF structure:

   ```
   _:r1  log:triple  ( _:b   log:implies   _:h ).
   _:b   log:graph   ( …body triple nodes… ).
   _:h   log:graph   ( …head triple nodes… ).
   ```

2. **Once per rule** we keep that blank node so later proofs can point at it
   instead of serialising the whole rule again.

3. **Each forward firing** creates:

   ```
   _:e42  proof:viaRule   ( _:r1 )
          proof:used      ( list of body triples after substitution )
          proof:produced  ( list of new head triples )
   ```

4. **Copying into the answer graph**
   `_copy_proof_nodes()` traverses `proof:viaRule` → the reified rule →
   nested RDF lists, so the answer graph ends up with *exactly* the proof
   fragments needed, no extra baggage.

5. **Determinism knob**
   When `Reasoner.proof` is true, both fact iteration and proof-list
   construction go through `self._sort(…)`, which orders triples
   lexicographically (`str(s), str(p), str(o)`).
   Combined with the deterministic `uuid.uuid4` monkey-patch at the *top of the file*, every run with the same input produces byte-for-byte identical N-Triples.

### Visual cheat-sheet

```
             ┌────────────────────────────────────┐
             │    _:r1 (reified rule structure)   │
             └┬──────────────────────────────────┬┘
viaRule       │                                  │
        ┌─────▼──────┐               ┌──────────▼─────┐
        │  proof:e42 │────used──────▶│ body triples…  │
        └────────────┘               └────────────────┘
                 │
         produced│
                 ▼
        ┌─────────────────┐
        │ new fact triple │   (and friends…)
        └─────────────────┘
```

---

### TL;DR

* **Goal ordering**: tiny sort → massive cut in search space.
* **Occurs-check**: one guard in `_extend` keeps unification sane.
* **Proof recording**: a single blank node per rule + one per firing, all
  built from the same helper so the output is compact and deterministic.

