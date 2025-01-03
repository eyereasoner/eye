% Closed Knight's Tour.
% Original from https://www.metalevel.at/knight/

:- use_module(library(clpfd)).
:- use_module(library(apply)).

'<urn:example:n_tour>'(N, Ts) :-
    length(Ts, N),
    maplist(same_length(Ts), Ts),
    append(Ts, Vs),
    successors(Vs, N, 1),
    circuit(Vs),
    maplist(label, Ts),
    !.

successors([], _, _).
successors([V|Vs], N, K0) :-
    findall(Num, n_k_next(N, K0, Num), [Next|Nexts]),
    foldl(num_to_dom, Nexts, Next, Dom),
    V in Dom,
    K1 #= K0 + 1,
    successors(Vs, N, K1).

num_to_dom(N, D0, D0\/N).

n_x_y_k(N, X, Y, K) :-
    [X,Y] ins 1..N,
    K #= N*(Y-1) + X.

n_k_next(N, K, Next) :-
    n_x_y_k(N, X0, Y0, K),
    [DX,DY] ins -2 \/ -1 \/ 1 \/ 2,
    abs(DX) + abs(DY) #= 3,
    [X,Y] ins 1..N,
    X #= X0 + DX,
    Y #= Y0 + DY,
    n_x_y_k(N, X, Y, Next),
    label([DX,DY]).

% query
true :+ '<urn:example:n_tour>'(8, _).
