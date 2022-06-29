% ---------------------
% Roundel -- Jos De Roo
% ---------------------
%
% See https://github.com/josd/eye/tree/master/looking/roundel#readme
%

:- use_module(library(lists)).
:- use_module(library(terms)).

:- op(1150, xfx, =>).

:- dynamic(brake/0).
:- dynamic(label/1).
:- dynamic(pred/1).

% forward chaining
forward :-
    (Prem => Conc),
    Prem,
    \+Conc,
    labelvars(Conc),
    astep(Conc),
    retract(brake),
    fail.
forward :-
    (   brake
    ->  !
    ;   assertz(brake),
        forward
    ).

% create witnesses
labelvars(Term) :-
    (   retract(label(Current))
    ->  true
    ;   Current = 0
    ),
    numbervars(Term,Current,Next),
    assertz(label(Next)).

% assert new step
astep((A, B)) :-
    !,
    astep(A),
    astep(B).
astep(A) :-
    (   \+A
    ->  assertz(A),
        (   functor(A, B, 2),
            \+pred(B)
        ->  assertz(pred(B))
        ;   true
        )
    ;   true
    ).

%
% built-ins
%
'https://josd.github.io/eye/ns#pso_triple'([P, S, O], Triple) :-
    (   var(P)
    ->  pred(P)
    ;   true
    ),
    Triple =.. [P, S, O].
