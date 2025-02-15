% Sieve of Eratosthenes

:- op(1200, xfx, :+).

'<https://eyereasoner.github.io/ns#primes>'(Limit, Ps) :-
    range(2, Limit, Is),
    sift(Is, Ps).

range(Start, End, []) :-
    Start > End.
range(Start, End, [Start|Rest]) :-
    Start =< End,
    Next is Start+1,
    range(Next, End, Rest).

sift([], []).
sift([I|Is], [I|Ps]) :-
    remove(I, Is, New),
    sift(New, Ps).

remove(_, [], []).
remove(P, [I|Is], Nis) :-
    0 is I mod P,
    remove(P, Is, Nis).
remove(P, [I|Is], [I|Nis]) :-
    X is I mod P,
    X \= 0,
    remove(P, Is, Nis).

% query
true :+ '<https://eyereasoner.github.io/ns#primes>'(10000, _).
