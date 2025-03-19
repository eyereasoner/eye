% Matrix
% See https://en.wikipedia.org/wiki/Matrix_(mathematics)
% Original code from https://github.com/friguzzi/matrix
%
% This module performs matrix operations.
% Impemented operations:
%  - sum
%  - difference
%  - multiplication
%  - Cholesky decomposition https://en.wikipedia.org/wiki/Cholesky_decomposition
%  - determinant for positive semi-definite matrices (using Cholesky decomposition)
%  - inversion for positive semi-definite matrices (using Cholesky decomposition)
%  - inversion for lower triangular matrices
%
% The library was developed for dealing with multivariate Gaussian distributions,
% thats the reson for the focus on positive semi-definite matrices
%
% @author Fabrizio Riguzzi
% @license Artistic License 2.0
% @copyright Fabrizio Riguzzi

:- op(1200, xfx, :+).

%% matrix_div_scal(+A, +V, -B) is det.
% divide matrix A by scalar V
%
matrix_div_scal(A, V, B) :-
    maplist(maplist(div(V)), A, B).

div(A, B, C) :-
    C is B/A.
%% matrix_mult_scal(+A, +V, -B) is det.
% multiply matrix A by scalar V
%
matrix_mult_scal(A, V, B) :-
    maplist(maplist(mult(V)), A, B).

mult(A, B, C) :-
    C is A*B.

%% 'urn:example:determinant'(+A, -D) is det.
% computes the determinant for a positive semi-definite matrix.
% Uses the Cholenski decomposition
% ==
% :+ 'urn:example:determinant'([[2, -1, 0], [-1, 2, -1], [0, -1, 2]], D).
% D = 3.999999999999999.
% ==
'urn:example:determinant'(A, Det) :-
    'urn:example:cholesky_decomposition'(A, L),
    get_diagonal(L, D),
    foldl(prod, D, 1, DetL),
    Det is DetL*DetL.

prod(A, P0, P) :-
    P is P0*A.

%% 'urn:example:matrix_inversion'(+M, -IM) is det.
% inversion of a positive semi-definite matrix. Uses the Cholenski
% decomposition
% ==
% :+ 'urn:example:matrix_inversion'([[2, -1, 0], [-1, 2, -1], [0, -1, 2]], L).
% L = [[0.7499999999999999, 0.5000000000000001, 0.2500000000000001], [0.5000000000000001, 1.0000000000000004, 0.5000000000000002], [0.2500000000000001, 0.5000000000000002, 0.7500000000000001]].
% ==
'urn:example:matrix_inversion'(A, B) :-
    'urn:example:cholesky_decomposition'(A, L),
    'urn:example:matrix_inv_triang'(L, LI),
    transpose_matrix(LI, LIT),
    'urn:example:matrix_multiply'([LIT, LI], B).

%% 'urn:example:matrix_inv_triang'(+M, -IM) is det.
% inversion of a lower triangular matrix
% code from
% http://www.mymathlib.com/c_source/matrices/linearsystems/unit_lower_triangular.c
% http://www.mcs.csueastbay.edu/~malek/TeX/Triangle.pdf
% code from
% ==
% :+ 'urn:example:matrix_inv_triang'([[2, 0, 0], [-1, 2, 0], [0, -1, 2]], L).
% L = [[0.5, 0.0, 0.0], [0.25, 0.5, 0.0], [0.125, 0.25, 0.5]].
% ==
'urn:example:matrix_inv_triang'(L1, L2) :-
    get_diagonal(L1, D),
    maplist(inv, D, ID),
    length(ID, N),
    NN is N*N,
    listd(NN, N, ID, L0),
    identify_rows(L0, N, IDM),
    'urn:example:matrix_multiply'([IDM, L1], LL1),
    append(LL1, LT),
    length(LL1, N),
    matrix_inv_i(1, N, LT, LTT),
    identify_rows(LTT, N, LL2),
    'urn:example:matrix_multiply'([LL2, IDM], L2).

matrix_inv_i(N, N, LT, LT) :-
    !.
matrix_inv_i(I, N, LT, LTTT) :-
    matrix_inv_j(0, I, N, LT, LTT),
    I1 is I+1,
    matrix_inv_i(I1, N, LTT, LTTT).

matrix_inv_j(I, I, _N, LT, LT) :-
    !.
matrix_inv_j(I, I, _N, LT, LT) :-
    !.
matrix_inv_j(J, I, N, LT, LTTTT) :-
    get_v(I, J, N, LT, Vij),
    V_ij is -Vij,
    set_v(I, J, N, LT, LTT, V_ij),
    J1 is J+1,
    matrix_inv_k(J1, J, I, N, LTT, LTTT),
    matrix_inv_j(J1, I, N, LTTT, LTTTT).

matrix_inv_k(I, _J, I, _N, LT, LT) :-
    !.
matrix_inv_k(K, J, I, N, LT, LTTT) :-
    get_v(I, K, N, LT, Vik),
    get_v(K, J, N, LT, Vkj),
    get_v(I, J, N, LT, Vij),
    NVij is Vij-Vik*Vkj,
    set_v(I, J, N, LT, LTT, NVij),
    K1 is K+1,
    matrix_inv_k(K1, J, I, N, LTT, LTTT).

inv(A, B) :-
    B is 1.0/A.

get_diagonal(L, D) :-
    length(L, N),
    append(L, LT),
    get_diag(0, N, LT, D).

get_diag(N, N, _L, []) :-
    !.
get_diag(N0, N, L, [H|R]) :-
    get_v(N0, N0, N, L, H),
    N1 is N0+1,
    get_diag(N1, N, L, R).

%% 'urn:example:matrix_multiply'([+X, +Y], -M) is det.
%
%   X(N*P), Y(P*M), M(N*M)
% ==
% :+ 'urn:example:matrix_multiply'([[[1, 2], [3, 4], [5, 6]], [[1, 1, 1], [1, 1, 1]]], R).
% R = [[3, 3, 3], [7, 7, 7], [11, 11, 11]].
% ==
% code from http://stackoverflow.com/questions/34206275/matrix-multiplication-with-prolog
'urn:example:matrix_multiply'([X, Y], M) :-
    matrix_mul(X, Y, M0),
    maplist(maplist(is), M, M0).

matrix_mul(X, Y, M) :-
    transpose_matrix(Y, T),
    maplist(row_multiply(T), X, M).

row_multiply(T, X, M) :-
    maplist(dot_product(X), T, M).

%% dot_product(+X, +Y, -D) is det.
% computes the dot produce of two vectors
%
dot_product([X|Xs], [T|Ts], M) :-
    foldl(mul, Xs, Ts, X*T, M).

mul(X, T, M, M+X*T).

%% matrix_diff(+A, +B, -C) is det
matrix_diff(X, Y, S) :-
    maplist(maplist(diff), X, Y, S).

diff(A, B, C) :-
    C is A-B.

%% 'urn:example:matrix_sum'([+A, +B], -C) is det
% ==
% :+ 'urn:example:matrix_sum'([[[1, 2], [3, 4], [5, 6]], [[1, 2], [3, 4], [5, 6]]], M).
% ==
'urn:example:matrix_sum'([X, Y], S) :-
    maplist(maplist(sum), X, Y, S).

sum(A, B, C) :-
    C is A+B.

%% 'urn:example:cholesky_decomposition'(+A, -L) is det.
% computes the Cholesky decomposition of a positive semi-definite matrix
% code from https://rosettacode.org/wiki/Cholesky_decomposition#C
% ==
% :+ 'urn:example:cholesky_decomposition'([[25, 15, -5], [15, 18, 0], [-5, 0, 11]], L).
% L = [[5.0, 0, 0], [3.0, 3.0, 0], [-1.0, 1.0, 3.0]].
% :+ 'urn:example:cholesky_decomposition'([[18, 22, 54, 42], [22, 70, 86, 62], [54, 86, 174, 134], [42, 62, 134, 106]], L).
% L = [[4.242640687119285, 0, 0, 0], [5.185449728701349, 6.565905201197403, 0, 0], [12.727922061357857, 3.0460384954008553, 1.6497422479090704, 0], [9.899494936611667, 1.624553864213788, 1.8497110052313648, 1.3926212476456026]].
% ==
'urn:example:cholesky_decomposition'(A, L) :-
    append(A, AL),
    length(AL, NL),
    list0(NL, LL),
    length(A, N),
    cholesky_i(0, N, AL, LL, LLL),
    identify_rows(LLL, N, L).

cholesky_i(N, N, _A, L, L) :-
    !.
cholesky_i(I, N, A, L, LLL) :-
    cholesky_j(0, I, N, A, L, LL),
    I1 is I+1,
    cholesky_i(I1, N, A, LL, LLL).

cholesky_j(I, I, N, A, L, LLL) :-
    !,
    cholesky_k(0, I, I, N, 0, S, L, LL),
    get_v(I, I, N, A, Aii),
    V is sqrt(Aii-S),
    set_v(I, I, N, LL, LLL, V).
cholesky_j(J, I, N, A, L, LLLL) :-
    cholesky_k(0, J, I, N, 0, S, L, LL),
    get_v(I, J, N, A, Aij),
    get_v(J, J, N, LL, Ljj),
    V is 1.0/Ljj*(Aij-S),
    set_v(I, J, N, LL, LLL, V),
    J1 is J+1,
    cholesky_j(J1, I, N, A, LLL, LLLL).

cholesky_k(J, J, _I, _N, S, S, L, L) :-
    !.
cholesky_k(K, J, I, N, S0, S, L, LL) :-
    get_v(I, K, N, L, Lik),
    get_v(J, K, N, L, Ljk),
    S1 is S0+Lik*Ljk,
    K1 is K+1,
    cholesky_k(K1, J, I, N, S1, S, L, LL).

get_v(I, J, N, M, V) :-
    E is I*N+J,
    append(C, [V|_D], M),
    length(C, E),
    !.

set_v(I, J, N, M, MM, V) :-
    E is I*N+J,
    append(C, [_|D], M),
    length(C, E),
    !,
    append(C, [V|D], MM).

identify_rows([], _N, []) :-
    !.
identify_rows(E, N, [R|L]) :-
    length(R, N),
    append(R, Rest, E),
    identify_rows(Rest, N, L).

%% list0(+N, -L) is det
% returns a list of N zeros
list0(0, []) :-
    !.
list0(N, [0|T]) :-
    N1 is N-1,
    list0(N1, T).

listd(0, _D, _L, []) :-
    !.
listd(N, D, [E|R], [E|T]) :-
    N rem (D+1) =:= 1,
    !,
    N1 is N-1,
    listd(N1, D, R, T).
listd(N, D, L, [0|T]) :-
    N1 is N-1,
    listd(N1, D, L, T).

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
true :+ 'urn:example:determinant'([[2, -1, 0], [-1, 2, -1], [0, -1, 2]], _).
true :+ 'urn:example:matrix_inversion'([[2, -1, 0], [-1, 2, -1], [0, -1, 2]], _).
true :+ 'urn:example:matrix_inversion'([[18, 22, 54, 42], [22, 70, 86, 62], [54, 86, 174, 134], [42, 62, 134, 106]], _).
true :+ 'urn:example:matrix_inv_triang'([[2, 0, 0], [-1, 2, 0], [0, -1, 2]], _).
true :+ 'urn:example:matrix_multiply'([[[1, 2], [3, 4], [5, 6]], [[1, 1, 1], [1, 1, 1]]], _).
true :+ 'urn:example:matrix_multiply'([[[18, 22, 54, 42], [22, 70, 86, 62], [54, 86, 174, 134], [42, 62, 134, 106]], [[2.515624999999984, 0.4843749999999933, -1.296874999999973, 0.3593749999999767], [0.4843749999999933, 0.1406249999999978, -0.3281249999999918, 0.1406249999999936], [-1.296874999999973, -0.3281249999999918, 1.015624999999971, -0.5781249999999781], [0.3593749999999767, 0.1406249999999936, -0.5781249999999781, 0.5156249999999853]]], _).
true :+ 'urn:example:matrix_sum'([[[1, 2], [3, 4], [5, 6]], [[1, 2], [3, 4], [5, 6]]], _).
true :+ 'urn:example:cholesky_decomposition'([[25, 15, -5], [15, 18, 0], [-5, 0, 11]], _).
true :+ 'urn:example:cholesky_decomposition'([[18, 22, 54, 42], [22, 70, 86, 62], [54, 86, 174, 134], [42, 62, 134, 106]], _).
