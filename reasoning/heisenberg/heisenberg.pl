% ----------
% Heisenberg
% ----------

% Heisenberg performs backward chaining for Prolog rules like HEAD :- BODY and
% forward chaining for rules like BODY -: HEAD with HEAD being a conjunction.
% There is no principle to tell whether to use backward or forward chaining.

:- op(1150,xfx,-:).

:- dynamic(neg/1).
:- dynamic(goal/0).
:- dynamic(label/1).
:- dynamic(answer/1).
:- dynamic(limited_answer/1).

:- initialization(load_libraries).

load_libraries :-
    catch(use_module(library(between)),_,true),
    catch(use_module(library(iso_ext)),_,true),
    use_module(library(lists)),
    use_module(library(terms)).

run :-
    heisenberg,
    forall(
        answer(A),
        (   write('[] :heisenberg-conducted """'),
            writeq(A),
            write('""".'),
            nl
        )
    ).

% the heisenberg chainer
heisenberg :-
    (Prem -: Conc),
    Prem,
    \+Conc,
    (   Conc = goal
    ->  labelvars(Prem),
        (   \+answer(Prem)
        ->  assertz(answer(Prem))
        ;   true
        ),
        (   limited_answer(1)
        ;   true
        ->  retract(limited_answer(N)),
            M is N-1,
            assertz(limited_answer(M)),
            fail
        )
    ;   labelvars(Conc),
        astep(Conc),
        retract(goal),
        fail
    ).
heisenberg :-
    (   goal
    ->  true
    ;   assertz(goal),
        heisenberg
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
astep((A,B)) :-
    astep(A),
    astep(B).
astep(false) :-
    throw('inference_fuse').
astep(A) :-
    (   \+A
    ->  assertz(A)
    ;   true
    ).
