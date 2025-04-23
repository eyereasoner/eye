% Evolutionary algorithm
% See https://en.wikipedia.org/wiki/Evolutionary_algorithm
% Original code from https://rosettacode.org/wiki/Evolutionary_algorithm#Prolog

:- op(1200, xfx, :+).

:- use_module(library(random)).

:- dynamic(evolution/3).

'urn:example:solve'(TargetAtom) :-
    atom_codes(TargetAtom, Target),
    length(Target, Len),
    random_text(Len, Start),
    evolve(0, 5, Target, Start).    % evolution 0 and 5% probability for a mutation

random_text(0, []).                 % generate some random text (fixed length)
random_text(Len, [H|T]) :-
    succ(L, Len),
    random_alpha(H),
    random_text(L, T).

random_alpha(Ch) :-                 % generate a single random character
    random(R),
    P is round(R*26),
    (   P = 0
    ->  Ch is 32
    ;   Ch is P+64
    ).

evolve(Evolution, Probability, Target, mutation(Score, Value)) :-
    atom_codes(Val, Value),
    (   \+evolution(Evolution, Score, Val)
    ->  assertz(evolution(Evolution, Score, Val))
    ;   true
    ),
    (   Score = 0
    ->  true
    ;   evolve(Evolution, Probability, Target, Value)
    ).
evolve(Evolution, Probability, Target, Start) :-
    findall(mutation(Score, M),     % generate 80 mutations, select the best
        (   between(1, 80, _),
            mutate(Probability, Start, M),
            score(M, Score, Target)
        ),
        Mutations
    ),
    sort(Mutations, [Best|_]),
    succ(Evolution, Evo),
    evolve(Evo, Probability, Target, Best).

mutate(_, [], []).                  % mutate(Probability, Input, Output)
mutate(Probability, [H|Txt], [H|Mut]) :-
    random(R),
    P is round(R*100),
    P > Probability,
    !,
    mutate(Probability, Txt, Mut).
mutate(Probability, [_|Txt], [M|Mut]) :-
    random_alpha(M),
    mutate(Probability, Txt, Mut).

score(Txt, Score, Target) :-
    score(Target, Txt, 0, Score).

score([], [], Score, Score).        % score a generated mutation (count diffs)
score([Ht|Tt], [Ht|Tp], C, Score) :-
    !,
    score(Tt, Tp, C, Score).
score([_|Tt], [_|Tp], C, Score) :-
    succ(C, N),
    score(Tt, Tp, N, Score).

% query
true :+ set_random(seed(100)).
true :+ evolution(_, _, _).
true :+ 'urn:example:solve'('METHINKS IT IS LIKE A WEASEL').
