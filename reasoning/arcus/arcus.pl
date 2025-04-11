% --------------------
% eyeneo -- Jos De Roo
% --------------------
%
% See https://github.com/eyereasoner/eyeneo
%

:- use_module(library(between)).
:- use_module(library(format)).
:- use_module(library(iso_ext)).
:- use_module(library(lists)).
:- use_module(library(terms)).

:- op(1200, xfx, :+).

:- dynamic((:+)/2).
:- dynamic(answer/1).
:- dynamic(brake/0).
:- dynamic(closure/1).
:- dynamic(count/2).
:- dynamic(fuse/1).
:- dynamic(limit/1).
:- dynamic(step/3).

version('eyeneo v1.3.1 (2025-04-10)').

% main goal
main :-
    assertz(closure(0)),
    assertz(limit(-1)),
    assertz(count(fm, 0)),
    assertz(count(mf, 0)),
    (   (_ :+ _)
    ->  true
    ;   version(Version),
        format(user_error, "~w~n", [Version]),
        halt(0)
    ),
    forall(
        (Conc :+ Prem),
        dynify((Conc :+ Prem))
    ),
    catch(eyeneo, E,
        (   E = halt(Exit)
        ->  true
        ;   format(user_error, "*** ~w~n", [E]),
            Exit = 1
        )
    ),
    count(fm, Fm),
    (   Fm = 0
    ->  true
    ;   format(user_error, "*** fm=~w~n", [Fm])
    ),
    count(mf, Mf),
    (   Mf = 0
    ->  true
    ;   format(user_error, "*** mf=~w~n", [Mf])
    ),
    (   Exit = 0
    ->  true
    ;   true
    ),
    halt(Exit).

%
% eyeneo
%
% 1/ select rule Conc :+ Prem
% 2/ prove Prem and if it fails backtrack to 1/
% 3/ if Conc = true assert answer + step
%    else if Conc = false output fuse + steps and stop
%    else if ~Conc assert Conc + step and retract brake
% 4/ backtrack to 2/ and if it fails go to 5/
% 5/ if brake
%       if not stable start again at 1/
%       else output answers + steps and stop
%    else assert brake and start again at 1/
%

eyeneo :-
    (   (Conc :+ Prem),                         % 1/
        copy_term((Conc :+ Prem), Rule),
        Prem,                                   % 2/
        (   Conc = true                         % 3/
        ->  assert_conj(answer(Prem)),
            assert_conj(step(Rule, Prem, Conc))
        ;   (   Conc = false
            ->  format(":- op(1200, xfx, :+).~n~n", []),
                portray_clause(fuse(Prem)),
                (   step(_, _, _),
                    nl
                ->  forall(
                        step(R, P, C),
                        portray_clause(step(R, P, C))
                    )
                ;   true
                ),
                throw(halt(2))
            ;   (   Conc \= (_ :+ _)
                ->  skolemize(Conc, 0, _)
                ;   true
                ),
                \+ Conc,
                assert_conj(Conc),
                assert_conj(step(Rule, Prem, Conc)),
                retract(brake)
            )
        ),
        fail                                    % 4/
    ;   (   brake                               % 5/
        ->  (   closure(Closure),
                limit(Limit),
                Closure < Limit,
                NewClosure is Closure+1,
                becomes(closure(Closure), closure(NewClosure)),
                eyeneo
            ;   format(":- op(1200, xfx, :+).~n~n", []),
                forall(
                    answer(P),
                    portray_clause(answer(P))
                ),
                (   step(_, _, _),
                    nl
                ->  forall(
                        step(R, P, C),
                        portray_clause(step(R, P, C))
                    )
                ;   true
                )
            )
        ;   assertz(brake),
            eyeneo
        )
    ).

% assert conjunction
assert_conj((B, C)) :-
    assert_conj(B),
    assert_conj(C).
assert_conj(A) :-
    (   \+ A
    ->  assertz(A)
    ;   true
    ).

% skolemize
skolemize(Term, N0, N) :-
    term_variables(Term, Vars),
    skolemize_(Vars, N0, N).

skolemize_([], N, N) :-
    !.
skolemize_([Sk|Vars], N0, N) :-
    number_chars(N0, C0),
    atom_chars(A0, C0),
    atom_concat('sk_', A0, Sk),
    N1 is N0+1,
    skolemize_(Vars, N1, N).

% stable(+Level)
%   fail if the deductive closure at Level is not yet stable
stable(Level) :-
    limit(Limit),
    (   Limit < Level
    ->  becomes(limit(Limit), limit(Level))
    ;   true
    ),
    closure(Closure),
    Level =< Closure.

% linear implication
becomes(A, B) :-
    catch(A, _, fail),
    conj_list(A, C),
    forall(
        member(D, C),
        retract(D)
    ),
    conj_list(B, E),
    forall(
        member(F, E),
        assertz(F)
    ).

% conjunction tofro list
conj_list(true, []).
conj_list(A, [A]) :-
    A \= (_, _),
    A \= false,
    !.
conj_list((A, B), [A|C]) :-
    conj_list(B, C).

% make dynamic predicates
dynify(A) :-
    var(A),
    !.
dynify(A) :-
    atomic(A),
    !.
dynify([]) :-
    !.
dynify([A|B]) :-
    !,
    dynify(A),
    dynify(B).
dynify(A) :-
    A =.. [B|C],
    length(C, N),
    (   current_predicate(B/N)
    ->  true
    ;   functor(T, B, N),
        catch((assertz(T), retract(T)), _, true)
    ),
    dynify(C).

% debugging tools
fm(A) :-
    format(user_error, "*** ~q~n", [A]),
    count(fm, B),
    C is B+1,
    becomes(count(fm, B), count(fm, C)).

mf(A) :-
    forall(
        catch(A, _, fail),
        (   format(user_error, "*** ", []),
            portray_clause(user_error, A),
            count(mf, B),
            C is B+1,
            becomes(count(mf, B), count(mf, C))
        )
    ).
