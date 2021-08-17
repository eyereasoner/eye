% Traversing graph paths

web_nsp(etc_,'http://josd.github.io/eye/two/cases#').

etc_oneway(etc_paris,etc_orleans).
etc_oneway(etc_paris,etc_chartres).
etc_oneway(etc_paris,etc_amiens).
etc_oneway(etc_orleans,etc_blois).
etc_oneway(etc_orleans,etc_bourges).
etc_oneway(etc_blois,etc_tours).
etc_oneway(etc_chartres,etc_lemans).
etc_oneway(etc_lemans,etc_angers).
etc_oneway(etc_lemans,etc_tours).
etc_oneway(etc_angers,etc_nantes).

etc_path(A,B) :-
    etc_oneway(A,B).
etc_path(A,B) :-
    etc_oneway(A,C),
    etc_path(C,B).

% test cases
case(etc_path(_CITY,etc_nantes)).

test :-
    case(A),
    A,
    write(A),
    write('.\n'),
    fail.
test :-
    halt.
