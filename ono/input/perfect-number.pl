% Create perfect numbers
% See https://en.wikipedia.org/wiki/Perfect_number

:- op(1200, xfx, :+).

% context
'urn:example:perfectNumber'(N, P) :- perfect(N, P).

%divisible(10, 2).
divisible(X, Y) :-
    N is Y*Y,
    N =< X,
    X mod Y =:= 0.
divisible(X, Y) :-
    Y < X,
    Y1 is Y + 1,
    divisible(X, Y1).


%isprime([3], Z).
isprime([X|_], X) :-
    Y is 2,
    X > 1,
    \+ divisible(X, Y).
isprime([_|T], Z) :-
    isprime(T, Z).


%Calculate the power of one number
%ie power(2, 10, R)
power(_, 0, 1) :- !.
power(N, K, R) :-
    K1 is K-1,
    power(N, K1, R1),
    R is R1*N.


%formula of perfect numbers 2^(p-1)*(2^p-1)
%ie calc(2, 10, R)
calc(2, K, R) :-
    power(2, K, X),
    R1 is X-1,
    power(2, K-1, R2),
    R is R1 * R2.

%using lists
%ie calc([2, 3, 4], R).
listperf([K|_], R) :-
    calc(2, K, R).
listperf([_|T], Z) :-
    listperf(T, Z).


%generate one list of N numbers.
%genList(10, L).
generateList(0, []).
generateList(N, [X|Xs]) :-
    N > 0,
    X is N+1,
    N1 is N-1, generateList(N1, Xs).

%list of N perfect numbers
%perfect(100, C)
perfect(N, C) :-
    generateList(N, R),
    findall(L, isprime(R, L), P),
    listperf(P, C).

% query
(true :+ 'urn:example:perfectNumber'(2025, _)) :- fail; true.
