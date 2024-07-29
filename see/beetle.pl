% beetle example

:- use_module(library(iso_ext)).
:- use_module(library(lists)).
:- use_module(library(terms)).

:- dynamic(brake/0).
:- dynamic('https://eyereasoner.github.io/see#sequent'/2).
:- dynamic('urn:example:is'/2).

% beetle is a car
'urn:example:Car'('urn:example:beetle').

% all cars are green or blue
'https://eyereasoner.github.io/see#sequent'(
    'urn:example:Car'(A),
    [
        'urn:example:is'(A,'urn:example:green'),
        'urn:example:is'(A,'urn:example:blue')
    ]
).

% green things are beautiful
'https://eyereasoner.github.io/see#sequent'(
    'urn:example:is'(A,'urn:example:green'),
    [
        'urn:example:is'(A,'urn:example:beautiful')
    ]
).

% blue things are beautiful
'https://eyereasoner.github.io/see#sequent'(
    'urn:example:is'(A,'urn:example:blue'),
    [
        'urn:example:is'(A,'urn:example:beautiful')
    ]
).

% resolution
'https://eyereasoner.github.io/see#sequent'(
    (
        'https://eyereasoner.github.io/see#sequent'(A,B),
        select(C,B,D),
        'https://eyereasoner.github.io/see#sequent'(C,E),
        length(E,F),
        F =< 1,
        append(E,D,G)
    ),
    [
        'https://eyereasoner.github.io/see#sequent'(A,G)
    ]
).

% factoring
'https://eyereasoner.github.io/see#sequent'(
    (
        'https://eyereasoner.github.io/see#sequent'(A,B),
        list_to_set(B,C)
    ),
    [
        'https://eyereasoner.github.io/see#sequent'(A,C)
    ]
).

% sequent
sequent :-
    (   'https://eyereasoner.github.io/see#sequent'(A,Bs),
        A,
        (   Bs = []
        ->  write(fuse('https://eyereasoner.github.io/see#sequent'(A,Bs),A)),
            nl,
            halt(2)
        ;   true
        ),
        Bs = [B],
        \+B,
        step(B),
        retract(brake),
        false
    ;   brake,
        !
    ;   assertz(brake),
        sequent
    ).

step((A,B)) :-
    !,
    step(A),
    step(B).
step(A) :-
    (   \+A
    ->  assertz(A)
    ;   true
    ).

% query
query('urn:example:is'(_A,_B)).

test :-
    sequent,
    query(Q),
    Q,
    write_term(Q,[numbervars(true),quoted(true),double_quotes(true)]),
    write('.\n'),
    fail;
    halt.
