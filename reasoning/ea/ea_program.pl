% Original code from https://rosettacode.org/wiki/Evolutionary_algorithm#Prolog

:- use_module(library(random)).

'<http://josd.github.io/eye/reasoning/ea#solve>'(Target, true) :-
    atom_codes(Target, C),
    length(C, Len),
    rndTxt(Len, Start),         % Start is the initial text
    Chance is 1-(1/(Len+1)),    % Chance is the probability for a mutation
    ea(0, Chance, C, Start).

rndAlpha(64, 32).               % Generate a single random character
rndAlpha(P, P).                 % 32 is a space, and 65->90 are upper case

rndAlpha(Ch) :-
    random(N),
    P is truncate(64+(N*27)),
    !,
    rndAlpha(P, Ch).

rndTxt(0, []).                  % Generate some random text (fixed length)
rndTxt(Len, [H|T]) :-
    succ(L, Len),
    rndAlpha(H),
    !,
    rndTxt(L, T).

score([], [], Score, Score).   % Score a generated mutation (count diffs)
score([Ht|Tt], [Ht|Tp], C, Score) :-
    !,
    score(Tt, Tp, C, Score).
score([_|Tt], [_|Tp], C, Score) :-
    succ(C, N),
    !,
    score(Tt, Tp, N, Score).

score(Txt, Score, Target) :-
    !,
    score(Target, Txt, 0, Score).

mutate(_, [], []).              % mutate(Probability, Input, Output)
mutate(P, [H|Txt], [H|Mut]) :-
    random(R),
    R < P,
    !,
    mutate(P, Txt, Mut).
mutate(P, [_|Txt], [M|Mut]) :-
    rndAlpha(M),
    !,
    mutate(P, Txt, Mut).

ea(Tries, _, _, mutation(0, Result)).
ea(Tries, Chance, Target, mutation(S, Value)) :-
    !,
    ea(Tries, Chance, Target, Value).
ea(Tries, Chance, Target, Start) :-
    findall(mutation(S, M),     % Generate 30 mutations, select the best.
        (   between(1, 30, _),
            mutate(Chance, Start, M),
            score(M, S, Target)
        ),
        Mutations
    ),
    sort(Mutations, [Best|_]),
    succ(Tries, N),
    !,
    ea(N, Chance, Target, Best).
