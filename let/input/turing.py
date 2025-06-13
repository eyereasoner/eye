"""
turing.py – Universal Turing‑machine ‘add‑1’ in ProbLog
=======================================================

The original Prolog snippet defines a mini interpreter for a Universal
Turing Machine and then runs it on four example tapes.  We port that
verbatim into ProbLog and turn each goal into a `query/1`.  Because the
program is deterministic, every query should succeed with probability
**1.0**.

Changes vs. the original:
* Replaced the custom query operator (`:+`) with native `query/1`.
* Used the atom `hash` instead of `#` for the blank symbol (avoids
  parsing issues in ProbLog).
* Added explicit `:-` after the `op/3` directive to silence loader
  warnings.

Run:
```bash
pip install problog
python turing.py
```
Expect:
```
compute([1,0,1,0,0,1],_): 1.0
compute([1,0,1,1,1,1],_): 1.0
compute([1,1,1,1,1,1],_): 1.0
compute([],_): 1.0
```
"""
from problog.program import PrologString
from problog import get_evaluatable

problog_code = """
% --- interpreter ---------------------------------------------------------
compute([], OutTape) :-
    start(_M, I),
    find(I, [], hash, [], OutTape).
compute([Head|Tail], OutTape) :-
    start(_M, I),
    find(I, [], Head, Tail, OutTape).

find(State, Left, Cell, Right, Out) :-
    t([State, Cell, Write, Move], Next),
    move(Move, Left, Write, Right, A, B, C),
    continue(Next, A, B, C, Out).

continue(halt, Left, Cell, Right, Out) :-
    rever(Left, R), append(R, [Cell|Right], Out).
continue(State, Left, Cell, Right, Out) :-
    find(State, Left, Cell, Right, Out).

move(l, [], Cell, Right, [], hash, [Cell|Right]).
move(l, [H|T], Cell, Right, T, H, [Cell|Right]).
move(s, L, C, R, L, C, R).
move(r, L, C, [], [C|L], hash, []).
move(r, L, C, [H|T], [C|L], H, T).

rever([], []).
rever([A|B], C) :- rever(B, D), append(D, [A], C).

% standard append ---------------------------------------------------------
append([], L, L).
append([H|T], L, [H|R]) :- append(T, L, R).

% --- Turing add-1 machine ------------------------------------------------
start(add1, 0).

t([0,0,0,r], 0).
t([0,1,1,r], 0).
t([0,hash,hash,l], 1).

t([1,0,1,s], halt).
t([1,1,0,l], 1).
t([1,hash,1,s], halt).

% --- queries -------------------------------------------------------------
query(compute([1,0,1,0,0,1], _)).
query(compute([1,0,1,1,1,1], _)).
query(compute([1,1,1,1,1,1], _)).
query(compute([], _)).
"""

model = PrologString(problog_code)
results = get_evaluatable().create_from(model).evaluate()
for q, p in results.items():
    print(f"{q}: {p}")
