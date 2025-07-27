% quine_mccluskey.pl

% Convert integer to binary list of length N
int_to_bin(0, 0, []) :- !.
int_to_bin(_, 0, []) :- !.
int_to_bin(N, Width, [B|Bs]) :-
    Width > 0,
    B is (N >> (Width - 1)) mod 2,
    Width1 is Width - 1,
    int_to_bin(N, Width1, Bs).

% Replace differing bit with '-'
combine([], [], [], 0).
combine([A|T1], [A|T2], [A|T3], D) :-
    combine(T1, T2, T3, D).
combine([A|T1], [B|T2], ['-'|T3], D) :-
    A \= B,
    combine(T1, T2, T3, D1),
    D is D1 + 1.

% Attempt all combinations differing in one bit
combine_all([], [], []).
combine_all([term(A1, L1)|T], Combined, Unused) :-
    combine_with_rest(term(A1, L1), T, NewTerms, Used),
    combine_all(T, CT, U2),
    append(NewTerms, CT, Combined),
    (Used -> Unused = U2 ; Unused = [term(A1, L1)|U2]).

combine_with_rest(_, [], [], false).
combine_with_rest(term(A1, L1), [term(A2, L2)|Rest], [term(C, L3)|New], true) :-
    combine(A1, A2, C, 1),
    append(L1, L2, L),
    sort(L, L3),
    !,
    combine_with_rest(term(A1, L1), Rest, New, _).
combine_with_rest(Term, [_|Rest], New, Used) :-
    combine_with_rest(Term, Rest, New, Used).

% Repeat combining until no new terms found
iterate_combine(Terms, Final) :-
    combine_all(Terms, Combined, Unused),
    Combined \= [],
    !,
    append(Unused, Combined, Next),
    sort(Next, NextU),
    iterate_combine(NextU, Final).
iterate_combine(Terms, Terms).

% Check if term covers a minterm
covers([], []).
covers(['-'|T1], [_|T2]) :- covers(T1, T2).
covers([B|T1], [B|T2]) :- B \= '-', covers(T1, T2).

% Check if a minterm is uniquely covered
is_essential(M, PrimeImps, Imp) :-
    findall(term(Bin, Ms),
        (member(term(Bin, Ms), PrimeImps), member(M, Ms)),
        Covers),
    Covers = [Imp].

% Extract essential prime implicants
essential([], _, []).
essential([M|Ms], PrimeImps, [Imp|Ess]) :-
    is_essential(M, PrimeImps, Imp), !,
    essential(Ms, PrimeImps, Ess).
essential([_|Ms], PrimeImps, Ess) :-
    essential(Ms, PrimeImps, Ess).

% Main predicate: returns binary implicants
quine_mccluskey(NumVars, Minterms, Simplified) :-
    findall(term(Bin, [M]),
        (member(M, Minterms), int_to_bin(M, NumVars, Bin)),
        Terms0),
    iterate_combine(Terms0, PrimeImps),
    essential(Minterms, PrimeImps, Essentials0),
    sort(Essentials0, Essentials),
    findall(Bin, member(term(Bin, _), Essentials), Simplified), !.

% ===== SYMBOLIC OUTPUT =====

% Wrapper for symbolic output
% Example: quine_mccluskey_expr(3, [1,3,7], "A'B + BC")
quine_mccluskey_expr(NumVars, Minterms, Expr) :-
    quine_mccluskey(NumVars, Minterms, Implicants),
    numlist(1, NumVars, Nums),
    maplist(var_name, Nums, Vars),
    symbolic_result(Implicants, Vars, Expr).

% Map index to variable: 1 -> 'A', 2 -> 'B', ...
var_name(N, Var) :-
    Code is 64 + N,
    char_code(Var, Code).

% Convert list of binary implicants to symbolic string (e.g., [0,1,'-'] => "A'B")
symbolic_result([], _, "").
symbolic_result(Implicants, Vars, Expr) :-
    maplist(bin_to_expr(Vars), Implicants, Terms),
    atomic_list_concat(Terms, ' + ', Expr).

% Convert a binary list to symbolic string using Var names
bin_to_expr(Vars, Bin, Expr) :-
    bin_to_expr_parts(Bin, Vars, Parts),
    atomic_list_concat(Parts, '', Expr).

bin_to_expr_parts([], [], []).
bin_to_expr_parts(['-'|Bs], [_|Vs], Parts) :-
    bin_to_expr_parts(Bs, Vs, Parts).
bin_to_expr_parts([1|Bs], [V|Vs], [V|Parts]) :-
    bin_to_expr_parts(Bs, Vs, Parts).
bin_to_expr_parts([0|Bs], [V|Vs], [Neg|Parts]) :-
    atom_concat(V, '\'', Neg),
    bin_to_expr_parts(Bs, Vs, Parts).

% query
true :+ quine_mccluskey_expr(3, [1,3,7], _).
