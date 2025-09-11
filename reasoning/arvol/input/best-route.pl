:- use_module(library(janus)).
:- use_module(library(strings)).

% a tiny Python module
:- py_module(pyhelpers, {|string||
def score(route):
    # sum of squared step differences
    return sum((b - a)**2 for a, b in zip(route, route[1:]))
|}).

% best route
best(Candidates, Best) :-
    findall(Cost-Route,
        (   member(Route, Candidates),
            py_call(pyhelpers:score(Route), Cost)
        ),
        Pairs
    ),
    keysort(Pairs, [Best|_]).

% query
true :+ best([[1,3,4],[1,2,4],[1,4,2]], _Best).
