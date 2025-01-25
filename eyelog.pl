% --------------------
% eyelog -- Jos De Roo
% --------------------

:- op(1200, xfx, :+).

:- dynamic(answer/1).
:- dynamic(brake/0).
:- dynamic(closure/1).
:- dynamic(count/2).
:- dynamic(limit/1).
:- dynamic(step/3).

% main goal
main :-
    assertz(closure(0)),
    assertz(limit(-1)),
    assertz(count(fm, 0)),
    assertz(count(mf, 0)),
    (   (_ :+ _)
    ->  format(':- op(1200, xfx, :+).~n~n')
    ;   true
    ),
    forall(
        (Conc :+ Prem),
        dynify((Conc :+ Prem))
    ),
    catch(run, E,
        (   (   E = halt(Exit)
            ->  true
            ;   format(user_error, '~w~n', [E]),
                Exit = 1
            )
        )
    ),
    count(fm, Fm),
    (   Fm = 0
    ->  true
    ;   format(user_error, '*** fm=~w~n', [Fm])
    ),
    count(mf, Mf),
    (   Mf = 0
    ->  true
    ;   format(user_error, '*** mf=~w~n', [Mf])
    ),
    ignore(Exit = 0),
    halt(Exit).

% run eyelog abstract machine
%
% 1/ select rule Conc :+ Prem
% 2/ prove Prem and if it fails backtrack to 1/
% 3/ if Conc = true assert answer(Prem)
%    else if Conc = false stop with return code 2
%    else if ~Conc assert Conc and retract brake
% 4/ backtrack to 2/ and if it fails go to 5/
% 5/ if brake
%       if not stable start again at 1/
%       else output answers + proof steps and stop
%    else assert brake and start again at 1/
%
run :-
    (   (Conc :+ Prem),     % 1/
        copy_term((Conc :+ Prem), Rule, _),
        Prem,               % 2/
        (   Conc = true     % 3/
        ->  (   \+answer(Prem)
            ->  assertz(answer(Prem)),
                assertz(step(Rule, Prem, true))
            ;   true
            )
        ;   (   Conc = false
            ->  format('% inference fuse, return code 2~n'),
                portray_clause(fuse(Prem)),
                throw(halt(2))
            ;   (   Conc \= (_ :+ _)
                ->  skolemize(Conc, 0, _)
                ;   true
                ),
                \+ Conc,
                astep(Conc),
                assertz(step(Rule, Prem, Conc)),
                retract(brake)
            )
        ),
        fail                % 4/
    ;   (   brake           % 5/
        ->  (   closure(Closure),
                limit(Limit),
                Closure < Limit,
                NewClosure is Closure+1,
                becomes(closure(Closure), closure(NewClosure)),
                run
            ;   answer(Prem),
                portray_clause(answer(Prem)),
                fail
            ;   (   step(_, _, _)
                ->  format('~n% proof steps~n'),
                    step(Rule, Prem, Conc),
                    portray_clause(step(Rule, Prem, Conc)),
                    fail
                ;   true
                )
            ;   true
            )
        ;   assertz(brake),
            run
        )
    ).

% assert new step
astep((B, C)) :-
    astep(B),
    astep(C).
astep(A) :-
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
    (   \+current_predicate(B/N)
    ->  dynamic(B/N)
    ;   true
    ),
    dynify(C).

% debugging tools
fm(A) :-
    format(user_error, '*** ~q~n', [A]),
    count(fm, B),
    C is B+1,
    becomes(count(fm, B), count(fm, C)).

mf(A) :-
    forall(
        catch(A, _, fail),
        (   format(user_error, '*** ~q~n', [A]),
            count(mf, B),
            C is B+1,
            becomes(count(mf, B), count(mf, C))
        )
    ).
