% New Scientist puzzle Enigma 1225

:- op(1200, xfx, :+).

'urn:example:enigma1225'(Size, [Permutation, Board, Max]) :-
    setof(Total, M^Freq^Perm^square(Size, M, Total, Freq, Perm), Totals),
    lastlist(Totals, Max),
    square(Size, Board, Max, _, Permutation).

var_matrix(Size, M) :-
    repeat(Size, Size, RowLengths),
    maplist(var_list, RowLengths, M).

repeat(X, 1, [X]) :-
    !.
repeat(X, N, [X|R]) :-
    NewN is N - 1,
    repeat(X, NewN, R).

var_list(N, L) :-
    length(L, N).

list_permute([], _, []).
list_permute([P1|Rest], L, [H|T]) :-
    nth1(P1, L, H),
    list_permute(Rest, L, T).

snd((_, X), X).

retain_var(_, [], []).
retain_var(V, [H|T], [H|L]) :-
    H == V,
    retain_var(V, T, L).
retain_var(V, [H|T], L) :-
    H \== V,
    retain_var(V, T, L).

count_var(VarList, Var, Num) :-
    retain_var(Var, VarList, List),
    length(List, Num).

total(IntPairs, Total) :-
    total(IntPairs, 0, Total).

total([], S, S).
total([(X, Y)|T], Acc, S) :-
    NewAcc is Acc + X*Y,
    total(T, NewAcc, S).

zip([], _, []) :-
    !.
zip(_, [], []) :-
    !.
zip([H1|T1], [H2|T2], [(H1, H2)|T]) :-
    zip(T1, T2, T).

from_to(M, N, L) :-
    (   var(L)
    ;   L = []
    ;   L = [_|_]
    ),
    integer(M),
    integer(N),
    M =< N,
    from_to_acc(M, [N], L),
    !.
from_to(H, N, [H|T]) :-
    lastlist([H|T], N),
    !,
    H =< N.

from_to_acc(H, [H|T], [H|T]).
from_to_acc(M, [H|T], L) :-
    NewHead is H - 1,
    !,
    from_to_acc(M, [NewHead, H|T], L).

eval_matrix(Matrix, FreqSorted) :-
    flatlist(Matrix, Entries),
    setof(E, member(E, Entries), Set),
    maplist(count_var(Entries), Set, Multiplicities),
    zip(Multiplicities, Set, Frequencies),
    sort(Frequencies, FreqSorted),
    maplist(snd, FreqSorted, VarsSorted),
    length(VarsSorted, NVars),
    from_to(1, NVars, VarsSorted).

distinct([_]).
distinct([H|T]) :-
    notin(H, T),
    distinct(T).

notin(_, []).
notin(E, [H|T]) :-
    E \== H,
    notin(E, T).

next_partition([(2, 1)|T], [(1, 2)|T]).
next_partition([(2, AlphaK)|T], [(1, 2), (2, NewAlphaK)|T]) :-
    AlphaK > 1,
    NewAlphaK is AlphaK - 1.
next_partition([(K, 1)|T], [(1, 1), (NewK, 1)|T]) :-
    K > 2,
    NewK is K - 1.
next_partition([(K, AlphaK)|T], [(1, 1), (NewK, 1), (K, NewAlphaK)|T]) :-
    K > 2,
    AlphaK > 1,
    NewK is K - 1,
    NewAlphaK is AlphaK - 1.
next_partition([(1, Alpha1), (2, 1)|T], [(1, NewAlpha)|T]) :-
    NewAlpha is Alpha1 + 2.
next_partition([(1, Alpha1), (2, Alpha2)|T], [(1, NewAlpha1), (2, NewAlpha2)|T]) :-
    Alpha2 > 1,
    NewAlpha1 is Alpha1 + 2,
    NewAlpha2 is Alpha2 - 1.
next_partition([(1, Alpha1), (L, 1)|T], [(Rest, 1), (NewL, Ratio)|T]) :-
    L > 2,
    NewL is L - 1,
    Rest is (Alpha1 + L) mod NewL,
    Rest > 0,
    Ratio is (Alpha1 + L) // NewL.
next_partition([(1, Alpha1), (L, 1)|T], [(NewL, Ratio)|T]) :-
    L > 2,
    NewL is L - 1,
    Rest is (Alpha1 + L) mod NewL,
    Rest =:= 0,
    Ratio is (Alpha1 + L) // NewL.
next_partition([(1, Alpha1), (L, AlphaL)|T], [(Rest, 1), (NewL, Ratio), (L, NewAlphaL)|T]) :-
    L > 2,
    AlphaL > 1,
    NewL is L - 1,
    Rest is (Alpha1 + L) mod NewL,
    Rest > 0,
    Ratio is (Alpha1 + L) // NewL,
    NewAlphaL is AlphaL - 1.
next_partition([(1, Alpha1), (L, AlphaL)|T], [(NewL, Ratio), (L, NewAlphaL)|T]) :-
    L > 2,
    AlphaL > 1,
    NewL is L - 1,
    Rest is (Alpha1 + L) mod NewL,
    Rest =:= 0,
    Ratio is (Alpha1 + L) // NewL,
    NewAlphaL is AlphaL - 1.

ad_partition(N, [(K, AlphaK)|T]) :-
    generator([(N, 1)], [(K, AlphaK)|T]),
    K > 1.

generator(From, From).
generator(Last, P) :-
    next_partition(Last, New),
    generator(New, P).

splitter(N, Type, S) :-
    from_to(1, N, L),
    splitter(L, Type, [], S).

splitter([], [(_, 0)], Acc, S) :-
    reverse(Acc, S),
    !.
splitter(L, [(_, 0)|T], Acc, S) :-
    splitter(L, T, Acc, S).
splitter(L, [(K, AlphaK)|T], Acc, S) :-
    AlphaK > 0,
    append(L1, L2, L),
    length(L1, K),
    NewAlphaK is AlphaK - 1,
    splitter(L2, [(K, NewAlphaK)|T], [L1|Acc], S).

list_rotate([H|T], L) :-
    append(T, [H], L).

rep_perm(N, Type, Perm) :-
    splitter(N, Type, S),
    maplist(list_rotate, S, R),
    flatlist(R, Perm).

square(Size, M, Total, Frequencies, Permutation) :-
    var_matrix(Size, M),
    ad_partition(Size, Partition),
    rep_perm(Size, Partition, Permutation),
    list_permute(Permutation, M, P),
    transpose_matrix(P, M),
    distinct(M),
    eval_matrix(M, Frequencies),
    total(Frequencies, Total).

flatlist(List, FlatList) :-
    flatlist_(List, [], FlatList0),
    !,
    FlatList = FlatList0.

flatlist_(Var, Tl, [Var|Tl]) :-
    var(Var),
    !.
flatlist_([], Tl, Tl) :- !.
flatlist_([Hd|Tl], Tail, List) :-
    !,
    flatlist_(Hd, FlatHeadTail, List),
    flatlist_(Tl, Tail, FlatHeadTail).
flatlist_(NonList, Tl, [NonList|Tl]).

lastlist([X|Xs], Last) :-
    lastlist_(Xs, X, Last).

lastlist_([], Last, Last).
lastlist_([X|Xs], _, Last) :-
    lastlist_(Xs, X, Last).

transpose_matrix([], []).
transpose_matrix([A|B], C) :-
    transpose_matrix(A, [A|B], C).

transpose_matrix([], _, []).
transpose_matrix([_|A], B, [C|D]) :-
    lists_reform(B, C, E),
    transpose_matrix(A, E, D).

lists_reform([], [], []).
lists_reform([[A|B]|C], [A|D], [B|E]) :-
    lists_reform(C, D, E).

% query
true :+ 'urn:example:enigma1225'(8, _).
