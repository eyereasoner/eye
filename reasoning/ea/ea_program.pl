% Original code from https://rosettacode.org/wiki/Evolutionary_algorithm#Prolog

:- use_module(library(random)).

'<http://josd.github.io/eye/reasoning/ea#solve>'(TargetAtom, true) :-
    atom_codes(TargetAtom, Target),
    length(Target, Len),
    random_text(Len, Start),        % Start is the initial text
    Chance is 1-(1/(Len+1)),        % Chance is the probability for a mutation
    evolve(Chance, Target, Start).

random_text(0, []).                 % generate some random text (fixed length)
random_text(Len, [H|T]) :-
    succ(L, Len),
    random_alpha(H),
    random_text(L, T).

random_alpha(Ch) :-                 % generate a single random character
    random(N),
    P is truncate(64+(N*27)),
    random_alpha(P, Ch).

random_alpha(64, 32).
random_alpha(P, P).

evolve(_, _, mutation(0, Result)).
evolve(Chance, Target, mutation(S, Value)) :-
    !,
    evolve(Chance, Target, Value).
evolve(Chance, Target, Start) :-
    findall(mutation(S, M),         % generate 30 mutations, select the best
        (   between(1, 30, _),
            mutate(Chance, Start, M),
            score(M, S, Target)
        ),
        Mutations
    ),
    sort(Mutations, [Best|_]),
    evolve(Chance, Target, Best).

mutate(_, [], []).                  % mutate(Probability, Input, Output)
mutate(P, [H|Txt], [H|Mut]) :-
    random(R),
    R < P,
    !,
    mutate(P, Txt, Mut).
mutate(P, [_|Txt], [M|Mut]) :-
    random_alpha(M),
    mutate(P, Txt, Mut).

score(Txt, Score, Target) :-
    score(Target, Txt, 0, Score).

score([], [], Score, Score).        % score a generated mutation (count diffs)
score([Ht|Tt], [Ht|Tp], C, Score) :-
    !,
    score(Tt, Tp, C, Score).
score([_|Tt], [_|Tp], C, Score) :-
    succ(C, N),
    score(Tt, Tp, N, Score).
