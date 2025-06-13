"""
ancestor.py – tabling in ProbLog
===============================

Run:
    pip install problog
    python ancestor.py

Expected output:
```
ancestor(ann,bob): 1.0
ancestor(ann,derek): 1.0
ancestor(ann,eliza): 1.0
ancestor(ann,chris): 1.0
ancestor(bob,derek): 1.0
ancestor(bob,eliza): 1.0
ancestor(derek,eliza): 1.0
```
"""
from problog.program import PrologString
from problog import get_evaluatable

problog_code = """
% ----- fact ---------------------------------------------------------------
parent(ann, bob).
parent(ann, chris).
parent(bob, derek).
parent(derek, eliza).

% ----- deterministic rules ------------------------------------------------
ancestor(X, Y) :- ancestor(X, Z), parent(Z, Y).
ancestor(X, Y) :- parent(X, Y).

% ----- queries ------------------------------------------------------------
query(ancestor(X, Y)).
"""

model = PrologString(problog_code)
for q, p in get_evaluatable().create_from(model).evaluate().items():
    print(f"{q}: {p}")
