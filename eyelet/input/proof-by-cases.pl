% Proof by cases
% See https://en.wikipedia.org/wiki/Disjunction_elimination

:- op(1200, xfx, :+).

:- dynamic(allPossibleCases/2).

% water is an inorganic compound
inorganic_compound(water).

% proof by cases
ascribed(A, observable) :-
    allPossibleCases([A], B),
    \+ (member(ascribed(A, C), B),
        \+ (ascribed(A, observable) :+ ascribed(A, C))
    ).

% water is solid or liquid or gas
allPossibleCases([A],
        [
            ascribed(A, solid),
            ascribed(A, liquid),
            ascribed(A, gas)
        ]
    ) :+ inorganic_compound(A).

% solid, liquid and gas things are observable
ascribed(A, observable) :+
    ascribed(A, solid).

ascribed(A, observable) :+
    ascribed(A, liquid).

ascribed(A, observable) :+
    ascribed(A, gas).

% query
true :+ ascribed(_, _).
