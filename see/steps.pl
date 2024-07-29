% Policy model finding example

:- use_module(library(lists)).
:- use_module(library(iso_ext)).

:- dynamic('urn:example:canDo'/2).

% Pat is an individual
'urn:example:Individual'('urn:example:Pat').

% if X is an individual then X can do A or B
'https://eyereasoner.github.io/see#sequent'(
    (
        'urn:example:Individual'(A)
    ),
    [
        'urn:example:canDo'(A,'urn:example:A'),
        'urn:example:canDo'(A,'urn:example:B')
    ]
).
% if X is an individual who can do A then X can do E or D
'https://eyereasoner.github.io/see#sequent'(
    (
        'urn:example:Individual'(A),
        'urn:example:canDo'(A,'urn:example:A')
    ),
    [
        'urn:example:canDo'(A,'urn:example:E'),
        'urn:example:canDo'(A,'urn:example:D')
    ]
).
% if X is an individual who can do D then X can do E or F
'https://eyereasoner.github.io/see#sequent'(
    (
        'urn:example:Individual'(A),
        'urn:example:canDo'(A,'urn:example:D')
    ),
    [
        'urn:example:canDo'(A,'urn:example:E'),
        'urn:example:canDo'(A,'urn:example:F')
    ]
).

% find model reaching goal
'https://eyereasoner.github.io/see#findModel'(A,[_,B,B]) :-
    A,
    !.
'https://eyereasoner.github.io/see#findModel'(A,[B,C,D]) :-
    'https://eyereasoner.github.io/see#sequent'(E,F),
    member(G,F),
    E,
    \+member('https://eyereasoner.github.io/see#sequent'(E,F),B),
    append(B,['https://eyereasoner.github.io/see#sequent'(E,F)],H),
    append(C,[G],I),
    becomes(true,G),
    call_cleanup(
        'https://eyereasoner.github.io/see#findModel'(A,[H,I,D]),
        becomes(G,true)
    ).

% linear implication
becomes(A,B) :-
    catch(A,_,fail),
    conj_list(A,C),
    forall(
        member(D,C),
        retract(D)
    ),
    conj_list(B,E),
    forall(
        member(F,E),
        assertz(F)
    ).

conj_list(true,[]) :-
    !.
conj_list(A,[A]) :-
    A \= (_,_),
    A \= false,
    !.
conj_list((A,B),[A|C]) :-
    conj_list(B,C).

% query: find model where X is an individual who can do E
query('https://eyereasoner.github.io/see#findModel'(
        (
            'urn:example:Individual'(A),
            'urn:example:canDo'(A,'urn:example:E')
        ),
        [[],[],_B]
    )
).

test :-
    query(Q),
    Q,
    write_term(Q,[numbervars(true),quoted(true),double_quotes(true)]),
    write('.\n'),
    fail;
    halt.
