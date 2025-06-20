"""
beetle12.py – Encoding the Beetle‑12 example in **ProbLog**
=========================================================

ProbLog supports *annotated disjunctions*, which map neatly onto the
N3 “($ {...} {...} $)” choice constructs.  By giving each branch a
probability of 0.5 we capture the idea that *exactly one* of the options
holds in any particular possible world, while being able to compute the
**skeptical probability** (the probability that a query is true across
all possible choices).  In this deterministic example that probability
will turn out to be 1.

Install ProbLog with:

```bash
pip install problog
```

Run the script:

```bash
python beetle12.py
```

You should see that `beetle` is **beautiful** with probability **1.0**
and none of the intermediate colours/qualities are certain.
"""
from problog.program import PrologString
from problog import get_evaluatable

problog_code = """
% -------- facts -----------------------------------------------------------
car(beetle).

% -------- annotated disjunctions -----------------------------------------
1/2::prop(green,A); 1/2::prop(blue,A)  :- car(A).

1/2::prop(nice,A);  1/2::prop(pretty,A)  :- prop(green,A).

1/2::prop(pretty1,A); 1/2::prop(pretty2,A) :- prop(pretty,A).
1/2::prop(nice1,A);   1/2::prop(nice2,A)   :- prop(nice,A).

1/2::prop(pretty11,A); 1/2::prop(pretty12,A) :- prop(pretty1,A).
1/2::prop(pretty21,A); 1/2::prop(pretty22,A) :- prop(pretty2,A).

1/2::prop(nice11,A); 1/2::prop(nice12,A) :- prop(nice1,A).
1/2::prop(nice21,A); 1/2::prop(nice22,A) :- prop(nice2,A).

% -------- deterministic beauty rules -------------------------------------
prop(beautiful,A) :- prop(blue,A).
prop(beautiful,A) :- prop(pretty11,A).
prop(beautiful,A) :- prop(pretty12,A).
prop(beautiful,A) :- prop(pretty21,A).
prop(beautiful,A) :- prop(pretty22,A).
prop(beautiful,A) :- prop(nice11,A).
prop(beautiful,A) :- prop(nice12,A).
prop(beautiful,A) :- prop(nice21,A).
prop(beautiful,A) :- prop(nice22,A).

% -------- queries ---------------------------------------------------------
query(prop(beautiful,beetle)).
query(prop(green,beetle)).
query(prop(blue,beetle)).
"""

model = PrologString(problog_code)
result = get_evaluatable().create_from(model).evaluate()

for query, prob in result.items():
    print(f"{query}: {prob}")
