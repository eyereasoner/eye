"""
beetle6.py – Beetle‑6 ("beetle is nice") in ProbLog
===========================================================

This encoding mirrors the *skeptical* reading of the earlier tableau:

* `beetle` is a `car`.
* A car is **green or blue** (exclusive choice).
* If it’s green, it’s **nice or pretty** (exclusive choice).
* `blue` or `pretty` ⇒ `beautiful`.
* **Constraint**: a car cannot be beautiful.  We implement this with an
  auxiliary predicate `bad/0` and add the ProbLog evidence
  `evidence(bad,false).` which throws away every world in which the
  constraint is violated.  The remaining probability mass is
  renormalised, so the query probability corresponds to the skeptical
  intersection.

Expected result:
```
prop(nice,beetle): 1.0
prop(beautiful,beetle): 0.0
prop(blue,beetle): 0.0
```

Install & run:
```bash
pip install problog
python beetle6.py
```
"""
from problog.program import PrologString
from problog import get_evaluatable

problog_code = """
% ----------- facts -------------------------------------------------------
car(beetle).

% ----------- annotated disjunctions -------------------------------------
1/2::prop(green,A); 1/2::prop(blue,A) :- car(A).

1/2::prop(nice,A); 1/2::prop(pretty,A) :- prop(green,A).

% ----------- deterministic rules ----------------------------------------
prop(beautiful,A) :- prop(blue,A).
prop(beautiful,A) :- prop(pretty,A).

% ----------- integrity constraint ---------------------------------------
bad :- car(A), prop(beautiful,A).

evidence(bad,false).

% ----------- queries -----------------------------------------------------
query(prop(nice,beetle)).
query(prop(beautiful,beetle)).
query(prop(blue,beetle)).
"""

model = PrologString(problog_code)
result = get_evaluatable().create_from(model).evaluate()

for q, p in result.items():
    print(f"{q}: {p}")
