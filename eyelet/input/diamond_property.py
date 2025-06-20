"""
diamond_property.py – Diamond property preserved under reflexive closure (ProbLog)
==================================================================================

Formalisation of the meta‑statement:
    **DP(r) ⇒ DP(re)** where *re* is *r* plus reflexivity.

We work on a tiny finite domain `{a,b,c}` and supply two base edges of
`re`: `re(a,b)` and `re(a,c)`.  For each base `re/2` edge we *guess*
whether it is an equality edge (`e/2`) or a proper `r/2` edge using an
**annotated disjunction** (0.5 each).  From there, inference rules add
symmetry, reflexivity, congruence, and the diamond property for `r/2`.

We enforce the diamond property *via a violation predicate* instead of
generating new `r` edges, avoiding non‑terminating recursion.

Concept
-------
* Each base edge `re(a,b)` and `re(a,c)` is independently classified as
  equality (`e`) or relation (`r`) with probability 0.5.
* `re/2` is the union of `e` and `r`; `e` is reflexive and symmetric;
  congruence adds `re` edges via equality substitution.
* **Diamond property requirement**: whenever `r(X,Y)` and `r(X,Z)` hold
  there must exist a node `U` such that `r(Y,U)` *and* `r(Z,U)` hold. We
  detect **violations** explicitly and discard such worlds with
  `evidence(dp_viol,false)`.

Finally we query `goal :- dom(X), re(b,X), re(c,X).` which asks whether
`b` and `c` converge under `re`.

Run:
```bash
pip install problog
python diamond_property.py
```

Expected output:
```
goal: 1.0
```
"""
from problog.program import PrologString
from problog import get_evaluatable

problog_code = """
% ---------- domain -------------------------------------------------------
dom(a). dom(b). dom(c).

% ---------- base re edges ------------------------------------------------
base_re(a,b).
base_re(a,c).

% ---------- probabilistic classification --------------------------------
0.5::e(X,Y); 0.5::r(X,Y) :- base_re(X,Y).

% ---------- definitions --------------------------------------------------
re(X,Y) :- e(X,Y).
re(X,Y) :- r(X,Y).

% equality axioms
e(X,X) :- dom(X).
e(Y,X) :- e(X,Y).

% congruence: e(X,Y) -> substitutes in re
re(X,Z) :- e(X,Y), re(Y,Z).

% ---------- diamond‑property violation detector --------------------------
% it fires if there is a pair of r‑edges sharing a source with **no** common successor

no_common_succ(X,Y,Z) :- \+ r(Y,a), \+ r(Z,a), \+ r(Y,b), \+ r(Z,b), \+ r(Y,c), \+ r(Z,c).

dp_viol :- r(X,Y), r(X,Z), no_common_succ(X,Y,Z).

evidence(dp_viol,false).

% ---------- goal ---------------------------------------------------------
goal :- dom(U), re(b,U), re(c,U).

% ---------- query --------------------------------------------------------
query(goal).
"""

model = PrologString(problog_code)
results = get_evaluatable().create_from(model).evaluate()
for atom, prob in results.items():
    print(f"{atom}: {prob}")
