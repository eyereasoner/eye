% -----------------------------------------------------------------
% Euler Yet another proof Engine mathematical Library -- Jos De Roo
% -----------------------------------------------------------------

'<http://josd.github.io/eyel#complex_exponentiation>'([A, B], [C, D], [E, F]) :-
    polaire([A, B], [R, T]),
    E is R^C*exp(-D*T)*cos(D*log(R)+C*T),
    F is R^C*exp(-D*T)*sin(D*log(R)+C*T).

'<http://josd.github.io/eyel#polynomial_roots>'(A, B) :-
    lz(A, C),
    racines(C, B).

'<http://josd.github.io/eyel#equation_solver>'([literal(A, type('<http://www.w3.org/2001/XMLSchema#string>')), literal(B, type('<http://www.w3.org/2001/XMLSchema#string>'))], C) :-
    term_to_atom(D, A),
    findall(E,
        (   solve_equation(D, B, B=F),
            E is F
        ),
        C
    ).

'<http://josd.github.io/eyel#euler_totient>'(A, B) :-
    when(
        (   nonvar(A)
        ),
        (   euler_totient(A, B)
        )
    ).

%
% Solving polynomial equations of degree 4
% See http://alain.colmerauer.free.fr/alcol/ArchivesPublications/Equation4/Equation4.pdf
%

% strike leading zero coefficients of polynomial
lz([[A, B]|C], D) :-
    A =:= 0,
    B =:= 0,
    !,
    lz(C, D).
lz(A, A).

% Liste des racines d'un polynome
racines(P, L) :-
    findall(Z, racine(P, Z), L).

% Racine d'un polynome
racine([A, B], Z) :-
    est(Z, moins(div(B, A))).
racine([A, B, C], Z) :-
    est(P, div(B, fois([2, 0], A))),
    est(Q, div(C, A)),
    est(Z, add(moins(P), fois(racarreeun, racine(2, moins(carre(P), Q))))).
racine([A, B, C, D], Zp) :-
    est(T, div(B, fois([-3, 0], A))),
    est(P, div(add(fois([3, 0], fois(A, carre(T))), add(fois([2, 0], fois(B, T)), C)), A)),
    est(Q, div(add(fois(A, cube(T)), add(fois(B, carre(T)), add(fois(C, T), D))), A)),
    solutionCardan(P, Q, Z),
    est(Zp, add(Z, T)).
racine([A, B, C, D, E], Zp) :-
    est(T, div(B, fois([-4, 0], A))),
    est(P, div(add(fois([6, 0], fois(A, carre(T))), add(fois([3, 0], fois(B, T)), C)), A)),
    est(Q, div(add(fois(fois([4, 0], A), cube(T)), add(fois(fois([3, 0], B), carre(T)), add(fois(fois([2, 0], C), T), D))), A)),
    est(R, div(add(fois(A, pquatre(T)), add(fois(B, cube(T)), add(fois(C, carre(T)), add(fois(D, T), E)))), A)),
    solutionLagrange(P, Q, R, Z),
    est(Zp, add(Z, T)).

% Polynome a partir de ses racines
polynome(L, P) :-
    polynome(L, [[1, 0]], P).

polynome([], P0, P0).
polynome([X|L], P0, P4) :-
    conc(P0, [[0, 0]], P1),
    est(Xp, moins(X)),
    foisl(Xp, P0, P2),
    addll(P1, [[0, 0]|P2], P3),
    polynome(L, P3, P4).

conc([], L, L).
conc([E|L], Lp, [E|Lpp]) :-
    conc(L, Lp, Lpp).

foisl(_, [], []).
foisl(X, [Y|L], [Z|Lp]) :-
    est(Z, fois(X, Y)),
    foisl(X, L, Lp).

addl(_, [], []).
addl(X, [Y|L], [Z|Lp]) :-
    est(Z, addl(X, Y)),
    addl(X, L, Lp).

addll([], [], []).
addll([X|L], [Y|Lp], [Z|Lpp]) :-
    est(Z, add(X, Y)),
    addll(L, Lp, Lpp).

% Solution de l'equation du troisieme degre selon Cardan
solutionCardan(P, Q, Z) :-
    nul(P),
    est(Z, fois(racubiqueun, racine(3, moins(Q)))).
solutionCardan(Pp, Qp, Z) :-
    nonnul(Pp),
    est(P, div(Pp, [3, 0])),
    est(Q, div(Qp, [2, 0])),
    est(Raccubique, fois(racubiqueun, racine(3, moins(racine(2, add(carre(Q), cube(P))), Q)))),
    est(Z, moins(Raccubique, div(P, Raccubique))).

% Solutions de l'equation du quatrieme degre selon Lagrange
solutionLagrange(P, Q, R, Z) :-
    est(A, [1, 0]),
    est(B, fois([2, 0], P)),
    est(C, moins(carre(P), fois([4, 0], R))),
    est(D, moins(carre(Q))),
    racines([A, B, C, D], [Y1, Y2, Y3]),
    est(Y1p, racine(2, Y1)),
    est(Y2p, racine(2, Y2)),
    est(Y3p, racine(2, Y3)),
    est(U1, div(add(Y1p, add(Y2p, Y3p)), [2, 0])),
    est(U2, div(moins(Y1p, add(Y2p, Y3p)), [2, 0])),
    est(U3, div(moins(Y3p, add(Y1p, Y2p)), [2, 0])),
    est(U4, div(moins(Y2p, add(Y1p, Y3p)), [2, 0])),
    est(V1, fois(U1, fois(U2, U3))),
    est(V2, fois(U1, fois(U2, U4))),
    est(V3, fois(U1, fois(U3, U4))),
    est(V4, fois(U2, fois(U3, U4))),
    epsilon(E, moins(add(V1, add(V2, add(V3, V4)))), Q),
    dans(U, [U1, U2, U3, U4]),
    est(Z, fois(E, U)).

epsilon([1, 0], _, Q) :-
    nul(Q).
epsilon(E, S, Q) :-
    nonnul(Q),
    est(E, div(S, Q)).

dans(U, [U|_]).
dans(U, [_|L]) :-
    dans(U, L).

% Valeurs de l'enchainement des operations sur les complexes
est(Z, Z) :-
    Z = [_, _].
est(Z, T) :-
    T =.. [F],
    catch(Tp =.. [F, Z], _, fail),
    call(Tp).
est(Z, T) :-
    T =.. [F, X],
    est(Xp, X),
    Tp =.. [F, Xp, Z],
    call(Tp).
est(Z, T) :-
    T =.. [F, X, Y],
    dif(F, racine),
    dif(F, '.'),
    est(Xp, X),
    est(Yp, Y),
    Tp =.. [F, Xp, Yp, Z],
    call(Tp).
est(Z, racine(N, X)) :-
    est(Xp, X),
    racine(N, Xp, Z).

% Operations sur les complexes
moins([X1, X2], [Y1, Y2]) :-
    Y1 is -X1,
    Y2 is -X2.

moins([X1, X2], [Y1, Y2], [Z1, Z2]) :-
    Z1 is X1-Y1,
    Z2 is X2-Y2.

add([X1, X2], [Y1, Y2], [Z1, Z2]) :-
    Z1 is X1+Y1,
    Z2 is X2+Y2.

fois([X1, X2], [Y1, Y2], [Z1, Z2]) :-
    Z1 is X1*Y1-X2*Y2,
    Z2 is X1*Y2+X2*Y1.

invers([X1, X2], [Y1, Y2]) :-
    Y1 is X1/(X1**2+X2**2),
    Y2 is -X2/(X1**2+X2**2).

div(X, Y, Z) :-
    invers(Y, Yp),
    fois(X, Yp, Z).

carre(X, Y) :-
    fois(X, X, Y).

cube(X, Y) :-
    carre(X, Xp),
    fois(X, Xp, Y).

pquatre(X, Y) :-
    carre(X, Xp),
    carre(Xp, Y).

racarreeun([1, 0]).
racarreeun([-1, 0]).

racubiqueun([1, 0]).
racubiqueun([X, Y]) :-
    X is -1/2,
    Y is sqrt(3)/2.
racubiqueun([X, Y]) :-
    X is -1/2,
    Y is -sqrt(3)/2.

racine(_, X, [0, 0]) :- nul(X).
racine(N, X, Y) :-
    nonnul(X),
    polaire(X, [R, T]),
    root(N, R, Rp),
    Tp is T/N,
    cartesien([Rp, Tp], Y).

root(N, X, Y) :-
    Y is exp(log(X)/N).

polaire([X, Y], [R, Tp]) :-
    R is sqrt(X**2+Y**2),
    T is acos(abs(X)/R),
    cadran(X, Y, T, Tp).

cadran(X, Y, T, Tp) :-
    X >= 0,
    Y >= 0,
    Tp = T.
cadran(X, Y, T, Tp) :-
    X < 0,
    Y >= 0,
    Tp is pi-T.
cadran(X, Y, T, Tp) :-
    X < 0,
    Y < 0,
    Tp is T+pi.
cadran(X, Y, T, Tp) :-
    X >= 0,
    Y < 0,
    Tp is 2*pi-T.

cartesien([R, T], [X1, X2]) :-
    X1 is R*cos(T),
    X2 is R*sin(T).

% Problemes de zero
nul([X, Y]) :-
    nulreel(X),
    nulreel(Y),
    !.

nonnul(Z) :-
    nul(Z),
    !,
    fail.
nonnul(_).

nulreel(0) :-
    !.
nulreel(0.0) :-
    !.
nulreel(-0.0).

%
% Equation solver
% Code from the book "The Art of Prolog" Chapter 23
%

% solve_equation(Equation, Unknown, Solution) :-
% Solution is a solution to the equation Equation
% in the unknown Unknown.
solve_equation(A*B=0, X, Solution) :-
    !,
    factorize(A*B, X, Factors-[]),
    remove_duplicates(Factors, Factors1),
    solve_factors(Factors1, X, Solution).
solve_equation(Equation, X, Solution) :-
    single_occurrence(X, Equation),
    !,
    position(X, Equation, [Side|Position]),
    maneuver_sides(Side, Equation, Equation1),
    isolate(Position, Equation1, Solution).
solve_equation(Lhs=Rhs, X, Solution) :-
    polynomial(Lhs, X),
    polynomial(Rhs, X),
    !,
    polynomial_normal_form(Lhs-Rhs, X, PolyForm),
    solve_polynomial_equation(PolyForm, X, Solution).
solve_equation(Equation, X, Solution) :-
    offenders(Equation, X, Offenders),
    multiple(Offenders),
    homogenize(Equation, X, Equation1, X1),
    solve_equation(Equation1, X1, Solution1),
    solve_equation(Solution1, X, Solution).

% The factorization method
%
% factorize(Expression, Subterm, Factors) :-
% Factors is a difference-list consisting of the factors of
% the multiplicative term Expression that contains the Subterm.
factorize(A*B, X, Factors-Rest) :-
    !,
    factorize(A, X, Factors-Factors1),
    factorize(B, X, Factors1-Rest).
factorize(C, X, [C|Factors]-Factors) :-
    subterm(X, C),
    !.
factorize(_, _, Factors-Factors).

% solve_factors(Factors, Unknown, Solution) :-
% Solution is a solution of the equation Factor=0 in
% the Unknown for some Factor in the list of Factors.
solve_factors([Factor|_], X, Solution) :-
    solve_equation(Factor=0, X, Solution).
solve_factors([_|Factors], X, Solution) :-
    solve_factors(Factors, X, Solution).

% The isolation method
maneuver_sides(1, Lhs = Rhs, Lhs = Rhs) :-
    !.
maneuver_sides(2, Lhs = Rhs, Rhs = Lhs) :-
    !.

isolate([N|Position], Equation, IsolatedEquation) :-
    isolax(N, Equation, Equation1),
    isolate(Position, Equation1, IsolatedEquation).
isolate([], Equation, Equation).

% Axioms for Isolation
isolax(1, -Lhs = Rhs, Lhs = -Rhs).
isolax(1, Term1+Term2 = Rhs, Term1 = Rhs-Term2).
isolax(2, Term1+Term2 = Rhs, Term2 = Rhs-Term1).
isolax(1, Term1-Term2 = Rhs, Term1 = Rhs+Term2).
isolax(2, Term1-Term2 = Rhs, Term2 = Term1-Rhs).
isolax(1, Term1*Term2 = Rhs, Term1 = Rhs/Term2) :-
    Term2 \== 0.
isolax(2, Term1*Term2 = Rhs, Term2 = Rhs/Term1) :-
    Term1 \== 0.
isolax(1, Term1/Term2 = Rhs, Term1 = Rhs*Term2) :-
    Term2 \== 0.
isolax(2, Term1/Term2 = Rhs, Term2 = Term1/Rhs) :-
    Rhs \== 0.
isolax(1, Term1^Term2 = Rhs, Term1 = Rhs^(-Term2)).
isolax(2, Term1^Term2 = Rhs, Term2 = log(Rhs)/log(Term1)).
isolax(1, sin(U) = V, U = asin(V)).
isolax(1, sin(U) = V, U = 180-asin(V)).
isolax(1, cos(U) = V, U = acos(V)).
isolax(1, cos(U) = V, U = -acos(V)).

% The polynomial method
polynomial(X, X) :-
    !.
polynomial(Term, _) :-
    atomic(Term),
    !.
polynomial(Term1+Term2, X) :-
    !,
    polynomial(Term1, X),
    polynomial(Term2, X).
polynomial(Term1-Term2, X) :-
    !,
    polynomial(Term1, X),
    polynomial(Term2, X).
polynomial(Term1*Term2, X) :-
    !,
    polynomial(Term1, X),
    polynomial(Term2, X).
polynomial(Term1/Term2, X) :-
    !,
    polynomial(Term1, X),
    atomic(Term2).
polynomial(Term ^ N, X) :-
    !,
    integer(N),
    N >= 0,
    polynomial(Term, X).

% polynomial_normal_form(Expression, Term, PolyNormalForm) :-
% PolyNormalForm  is the polynomial normal form of the
% Expression, which is a polynomial in Term.
polynomial_normal_form(Polynomial, X, NormalForm) :-
    polynomial_form(Polynomial, X, PolyForm),
    remove_zero_terms(PolyForm, NormalForm),
    !.

polynomial_form(X, X, [(1, 1)]).
polynomial_form(X^N, X, [(1, N)]).
polynomial_form(Term1+Term2, X, PolyForm) :-
    polynomial_form(Term1, X, PolyForm1),
    polynomial_form(Term2, X, PolyForm2),
    add_polynomials(PolyForm1, PolyForm2, PolyForm).
polynomial_form(Term1-Term2, X, PolyForm) :-
    polynomial_form(Term1, X, PolyForm1),
    polynomial_form(Term2, X, PolyForm2),
    subtract_polynomials(PolyForm1, PolyForm2, PolyForm).
polynomial_form(Term1*Term2, X, PolyForm) :-
    polynomial_form(Term1, X, PolyForm1),
    polynomial_form(Term2, X, PolyForm2),
    multiply_polynomials(PolyForm1, PolyForm2, PolyForm).
polynomial_form(Term^N, X, PolyForm) :-
    !,
    polynomial_form(Term, X, PolyForm1),
    binomial(PolyForm1, N, PolyForm).
polynomial_form(Term, X, [(Term, 0)]) :-
    free_of(X, Term),
    !.

remove_zero_terms([(0, _)|Poly], Poly1) :-
    !,
    remove_zero_terms(Poly, Poly1).
remove_zero_terms([(C, N)|Poly], [(C, N)|Poly1]) :-
    C \== 0,
    !,
    remove_zero_terms(Poly, Poly1).
remove_zero_terms([], []).

% Polynomial manipulation routines
%
% add_polynomials(Poly1, Poly2, Poly) :-
% Poly is the sum of Poly1 and Poly2, where
% Poly1, Poly2 and Poly are all in polynomial form.
add_polynomials([], Poly, Poly) :-
    !.
add_polynomials(Poly, [], Poly) :-
    !.
add_polynomials([(Ai, Ni)|Poly1], [(Aj, Nj)|Poly2], [(Ai, Ni)|Poly]) :-
    Ni > Nj,
    !,
    add_polynomials(Poly1, [(Aj, Nj)|Poly2], Poly).
add_polynomials([(Ai, Ni)|Poly1], [(Aj, Nj)|Poly2], [(A, Ni)|Poly]) :-
    Ni =:= Nj,
    !,
    A is Ai+Aj,
    add_polynomials(Poly1, Poly2, Poly).
add_polynomials([(Ai, Ni)|Poly1], [(Aj, Nj)|Poly2], [(Aj, Nj)|Poly]) :-
    Ni < Nj,
    !,
    add_polynomials([(Ai, Ni)|Poly1], Poly2, Poly).

% subtract_polynomials(Poly1, Poly2, Poly) :-
% Poly is the difference of Poly1 and Poly2, where
% Poly1, Poly2 and Poly are all in polynomial form.
subtract_polynomials(Poly1, Poly2, Poly) :-
    multiply_single(Poly2, (-1, 0), Poly3),
    add_polynomials(Poly1, Poly3, Poly),
    !.

% multiply_single(Poly1, Monomial, Poly) :-
% Poly is the product of Poly1 and Monomial, where
% Poly1, and Poly are in polynomial form, and Monomial
% has the form (C, N) denoting the monomial C*X^N.
multiply_single([(C1, N1)|Poly1], (C, N), [(C2, N2)|Poly]) :-
    C2 is C1*C,
    N2 is N1+N,
    multiply_single(Poly1, (C, N), Poly).
multiply_single([], _, []).

% multiply_polynomials(Poly1, Poly2, Poly) :-
% Poly  is the product of Poly1 and Poly2, where
% Poly1, Poly2 and Poly are all in polynomial form.
multiply_polynomials([(C, N)|Poly1], Poly2, Poly) :-
    multiply_single(Poly2, (C, N), Poly3),
    multiply_polynomials(Poly1, Poly2, Poly4),
    add_polynomials(Poly3, Poly4, Poly).
multiply_polynomials([], _, []).

binomial(Poly, 1, Poly).

% solve_polynomial_equation(Equation, Unknown, Solution) :-
% Solution  is a solution to the polynomial Equation
% in the unknown Unknown.
solve_polynomial_equation(PolyEquation, X, X = -B/A) :-
    linear(PolyEquation),
    !,
    pad(PolyEquation, [(A, 1), (B, 0)]).
solve_polynomial_equation(PolyEquation, X, Solution) :-
    quadratic(PolyEquation),
    !,
    pad(PolyEquation, [(A, 2), (B, 1), (C, 0)]),
    discriminant(A, B, C, Discriminant),
    root(X, A, B, C, Discriminant, Solution).

discriminant(A, B, C, D) :-
    D is B*B-4*A*C.

root(X, A, B, _, 0, X= -B/(2*A)).
root(X, A, B, _, D, X= (-B+sqrt(D))/(2*A)) :-
    D > 0.
root(X, A, B, _, D, X= (-B-sqrt(D))/(2*A)) :-
    D > 0.

pad([(C, N)|Poly], [(C, N)|Poly1]) :-
    !,
    pad(Poly, Poly1).
pad(Poly, [(0, _)|Poly1]) :-
    pad(Poly, Poly1).
pad([], []).

linear([(_, 1)|_]).

quadratic([(_, 2)|_]).

% The homogenization method
%
% homogenize(Equation, X, Equation1, X1) :-
% The Equation in X is transformed to the polynomial
% Equation1 in X1 where X1 contains X.
homogenize(Equation, X, Equation1, X1) :-
    offenders(Equation, X, Offenders),
    reduced_term(X, Offenders, Type, X1),
    rewrite(Offenders, Type, X1, Substitutions),
    substitute(Equation, Substitutions, Equation1).

% offenders(Equation, Unknown, Offenders)
% Offenders is the set of offenders of the equation in the Unknown
offenders(Equation, X, Offenders) :-
    parse(Equation, X, Offenders1-[]),
    remove_duplicates(Offenders1, Offenders),
    multiple(Offenders).

reduced_term(X, Offenders, Type, X1) :-
    classify(Offenders, X, Type),
    candidate(Type, Offenders, X, X1).

% Heuristics for exponential equations
classify(Offenders, X, exponential) :-
    exponential_offenders(Offenders, X).

exponential_offenders([A^B|Offs], X) :-
    free_of(X, A),
    subterm(X, B),
    exponential_offenders(Offs, X).
exponential_offenders([], _).

candidate(exponential, Offenders, X, A^X) :-
    base(Offenders, A),
    polynomial_exponents(Offenders, X).

base([A^_|Offs], A) :-
    base(Offs, A).
base([], _).

polynomial_exponents([_^B|Offs], X) :-
    polynomial(B, X),
    polynomial_exponents(Offs, X).
polynomial_exponents([], _).

% Parsing the equation and making substitutions
%
% parse(Expression, Term, Offenders)
% Expression is traversed to produce the set of Offenders in Term,
% that is the non-algebraic subterms of Expression containing Term
parse(A+B, X, L1-L2) :-
    !,
    parse(A, X, L1-L3),
    parse(B, X, L3-L2).
parse(A*B, X, L1-L2) :-
    !,
    parse(A, X, L1-L3),
    parse(B, X, L3-L2).
parse(A-B, X, L1-L2) :-
    !,
    parse(A, X, L1-L3),
    parse(B, X, L3-L2).
parse(A=B, X, L1-L2) :-
    !,
    parse(A, X, L1-L3),
    parse(B, X, L3-L2).
parse(A^B, X, L) :-
    integer(B),
    !,
    parse(A, X, L).
parse(A, X, L-L) :-
    free_of(X, A),
    !.
parse(A, X, [A|L]-L) :-
    subterm(X, A),
    !.

% substitute(Equation, Substitutions, Equation1) :-
% Equation1 is the result of applying the list of
% Substitutions to Equation.
substitute(A+B, Subs, NewA+NewB) :-
    !,
    substitute(A, Subs, NewA),
    substitute(B, Subs, NewB).
substitute(A*B, Subs, NewA*NewB) :-
    !,
    substitute(A, Subs, NewA),
    substitute(B, Subs, NewB).
substitute(A-B, Subs, NewA-NewB) :-
    !,
    substitute(A, Subs, NewA),
    substitute(B, Subs, NewB).
substitute(A=B, Subs, NewA=NewB) :-
    !,
    substitute(A, Subs, NewA),
    substitute(B, Subs, NewB).
substitute(A^B, Subs, NewA^B) :-
    integer(B),
    !,
    substitute(A, Subs, NewA).
substitute(A, Subs, B) :-
    member(A=B, Subs),
    !.
substitute(A, _, A).

% Finding homogenization rewrite rules
rewrite([Off|Offs], Type, X1, [Off=Term|Rewrites]) :-
    homogenize_axiom(Type, Off, X1, Term),
    rewrite(Offs, Type, X1, Rewrites).
rewrite([], _, _, []).

% Homogenization axioms
homogenize_axiom(exponential, A^(N*X), A^X, (A^X)^N).
homogenize_axiom(exponential, A^(-X), A^X, 1/(A^X)).
homogenize_axiom(exponential, A^(X+B), A^X, A^B*A^X).

% Utilities
subterm(Term, Term).
subterm(Sub, Term) :-
    compound1(Term),
    functor(Term, _, N),
    subterm(N, Sub, Term).

subterm(N, Sub, Term) :-
    arg(N, Term, Arg),
    subterm(Sub, Arg).
subterm(N, Sub, Term) :-
    N > 0,
    N1 is N - 1,
    subterm(N1, Sub, Term).

position(Term, Term, []) :-
    !.
position(Sub, Term, Path) :-
    compound1(Term),
    functor(Term, _, N),
    position(N, Sub, Term, Path),
    !.

position(N, Sub, Term, [N|Path]) :-
    arg(N, Term, Arg),
    position(Sub, Arg, Path).
position(N, Sub, Term, Path) :-
    N > 1,
    N1 is N-1,
    position(N1, Sub, Term, Path).

free_of(Subterm, Term) :-
    occurrence(Subterm, Term, N),
    !,
    N=0.

single_occurrence(Subterm, Term) :-
    occurrence(Subterm, Term, N),
    !,
    N=1.

occurrence(Term, Term, 1) :-
    !.
occurrence(Sub, Term, N) :-
    compound1(Term),
    !,
    functor(Term, _, M),
    occurrence(M, Sub, Term, 0, N).
occurrence(Sub, Term, 0) :-
    Term \== Sub.

occurrence(M, Sub, Term, N1, N2) :-
    M > 0,
    !,
    arg(M, Term, Arg),
    occurrence(Sub, Arg, N),
    N3 is N+N1,
    M1 is M-1,
    occurrence(M1, Sub, Term, N3, N2).
occurrence(0, _, _, N, N).

multiple([_, _|_]).

remove_duplicates(Xs, Ys) :-
    no_doubles(Xs, Ys).

no_doubles([X|Xs], Ys) :-
    member(X, Xs),
    no_doubles(Xs, Ys).
no_doubles([X|Xs], [X|Ys]) :-
    nonmember(X, Xs),
    no_doubles(Xs, Ys).
no_doubles([], []).

nonmember(X, [Y|Ys]) :-
    X \== Y,
    nonmember(X, Ys).
nonmember(_, []).

compound1(Term) :-
    functor(Term, _, N),
    N > 0,
    !.

%
% Euler's totient function
% Original code from https://www.ic.unicamp.br/~meidanis/courses/mc336/2009s2/prolog/problemas/
%

% Let [[p1, m1], [p2, m2], [p3, m3], ...] be the list of prime factors (and their
% multiplicities) of a given number m. Then phi(m) can be calculated
% with the following formula:
%   phi(m) = (p1-1)*p1**(m1-1)*(p2-1)*p2**(m2-1)*(p3-1)*p3**(m3-1)*...

% euler_totient(N, Phi) :- Phi is the value of Euler's totient function
% for the argument N.
%   (integer, integer) (+, ?)

euler_totient(N, Phi) :-
    prime_factors_mult(N, L),
    to_phi(L, Phi).

to_phi([], 1).
to_phi([[F, 1]|L], Phi) :-
    !,
    to_phi(L, Phi1),
    Phi is Phi1*(F-1).
to_phi([[F, M]|L], Phi) :-
    M > 1,
    M1 is M-1,
    to_phi([[F, M1]|L], Phi1),
    Phi is Phi1*F.

% prime_factors_mult(N, L) :- L is the list of prime factors of N. It is
% composed of terms [F, M] where F is a prime factor and M its multiplicity.
%   (integer, list) (+, ?)

prime_factors_mult(N, L) :-
    N > 0,
    prime_factors_mult(N, L, 2).

% prime_factors_mult(N, L, K) :- L is the list of prime factors of N. It is
% known that N does not have any prime factors less than K.

prime_factors_mult(1, [], _) :-
    !.
prime_factors_mult(N, [[F, M]|L], F) :-
    divide(N, F, M, R),
    !,
    next_factor(R, F, NF),
    prime_factors_mult(R, L, NF).
prime_factors_mult(N, L, F) :-
    !,
    next_factor(N, F, NF),
    prime_factors_mult(N, L, NF).

% prime_factors(N, L) :- N is the list of prime factors of N.
%   (integer, list) (+, ?)

prime_factors(N, L) :-
    N > 0,
    prime_factors(N, L, 2).

% prime_factors(N, L, K) :- L is the list of prime factors of N. It is
% known that N does not have any prime factors less than K.

prime_factors(1, [], _) :-
    !.
prime_factors(N, [F|L], F) :-
   R is N//F,
   N =:= R*F,
   !,
   prime_factors(R, L, F).
prime_factors(N, L, F) :-
   next_factor(N, F, NF),
   prime_factors(N, L, NF).

% next_factor(N, F, NF) :- when calculating the prime factors of N
% and if F does not divide N then NF is the next larger candidate to
% be a factor of N.

next_factor(_, 2, 3) :-
    !.
next_factor(N, F, NF) :-
    F*F < N,
    !,
    NF is F+2.
next_factor(N, _, N).

% divide(N, F, M, R) :- N = R * F**M, M >= 1, and F is not a factor of R.
%   (integer, integer, integer, integer) (+, +, -, -)

divide(N, F, M, R) :-
    divi(N, F, M, R, 0),
    M > 0.

divi(N, F, M, R, K) :-
    S is N//F,
    N =:= S*F,
    !,
    K1 is K+1,
    divi(S, F, M, R, K1).
divi(N, _, M, N, M).
