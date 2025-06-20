"""
water.py – “water is observable” example in ProbLog
==================================================

*Facts*
    water a InorganicCompound.
*Choice*
    Every inorganic compound is **solid _or_ liquid _or_ gas**.
*Rules*
    Each phase implies `observable`.

We encode the three-way exclusive choice with an *annotated disjunction*
where each branch has probability 1/3 (any set of weights summing to ≤1
works; they are only used by ProbLog to enumerate worlds, we care about
whether the query is certain).

Expected output:
```
prop(observable,water): 1.0
prop(solid,water): 0.333333...
prop(liquid,water): 0.333333...
prop(gas,water): 0.333333...
```

Run:
    pip install problog
    python water.py
"""
from problog.program import PrologString
from problog import get_evaluatable

problog_code = """
% ----- fact --------------------------------------------------------------
inorganic_compound(water).

% ----- 3‑way exclusive disjunction --------------------------------------
1/3::prop(solid,A); 1/3::prop(liquid,A); 1/3::prop(gas,A) :- inorganic_compound(A).

% ----- deterministic rules ----------------------------------------------
prop(observable,A) :- prop(solid,A).
prop(observable,A) :- prop(liquid,A).
prop(observable,A) :- prop(gas,A).

% ----- queries -----------------------------------------------------------
query(prop(observable,water)).
query(prop(solid,water)).
query(prop(liquid,water)).
query(prop(gas,water)).
"""

model = PrologString(problog_code)
results = get_evaluatable().create_from(model).evaluate()
for q, p in results.items():
    print(f"{q}: {p}")
