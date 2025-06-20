"""
beetle.py – The minimal Beetle disjunction in ProbLog
====================================================

Logic:
  car(beetle).
  car(X) -> green(X) v blue(X)      (exclusive disjunction)
  green(X) -> beautiful(X)
  blue(X)  -> beautiful(X)

With no further constraints, *every* possible world makes the Beetle
beautiful, so the skeptical probability for `prop(beautiful,beetle)` is
1.  Colour splits 50–50.

Run:
    pip install problog
    python beetle.py
"""
from problog.program import PrologString
from problog import get_evaluatable

problog_code = """
% ----- fact ---------------------------------------------------------------
car(beetle).

% ----- disjunction --------------------------------------------------------
1/2::prop(green,A); 1/2::prop(blue,A) :- car(A).

% ----- deterministic rules ------------------------------------------------
prop(beautiful,A) :- prop(green,A).
prop(beautiful,A) :- prop(blue,A).

% ----- queries ------------------------------------------------------------
query(prop(beautiful,beetle)).
query(prop(green,beetle)).
query(prop(blue,beetle)).
"""

model = PrologString(problog_code)
for q, p in get_evaluatable().create_from(model).evaluate().items():
    print(f"{q}: {p}")
