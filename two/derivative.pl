% See https://en.wikipedia.org/wiki/Derivative

nsp(etc_,'http://josd.github.io/eye/two/cases#').

etc_derivative(U,X,Y) :-
    etc_d(U,X,V),
    etc_s(V,W),
    etc_s(W,Y).

etc_integral(U,X,Y) :-
    etc_d(V,X,U),
    etc_s(V,W),
    etc_s(W,Y).

% derivative
etc_d(U+V,X,DU+DV) :-
    etc_d(U,X,DU),
    etc_d(V,X,DV),
    !.
etc_d(U-V,X,DU-DV) :-
    etc_d(U,X,DU),
    etc_d(V,X,DV),
    !.
etc_d(U*V,X,DU*V+U*DV) :-
    etc_d(U,X,DU),
    etc_d(V,X,DV),
    !.
etc_d(U/V,X,(DU*V-U*DV)/V^2) :-
    !,
    etc_d(U,X,DU),
    etc_d(V,X,DV),
    !.
etc_d(U^N,X,DU*N*U^N1) :-
    N1 is N - 1,
    etc_d(U,X,DU),
    !.
etc_d(-U,X,-DU) :-
    etc_d(U,X,DU),
    !.
etc_d(exp(U),X,DU*exp(U)) :-
    etc_d(U,X,DU),
    !.
etc_d(log(U),X,DU/U) :-
    etc_d(U,X,DU),
    !.
etc_d(erf(U),X,DU*sqrt(pi)/2*exp(-U^2)) :-
    etc_d(U,X,DU),
    !.
etc_d(X,X,1) :-
    !.
etc_d(_,_,0).

% simplification
etc_s(A,A) :-
    atom(A),
    !.
etc_s(A,A) :-
    number(A),
    !.
etc_s(0+A,A) :-
    !.
etc_s(A+0,A) :-
    !.
etc_s(0-A,-A) :-
    !.
etc_s(A-0,A) :-
    !.
etc_s(0*_,0) :-
    !.
etc_s(_*0,0) :-
    !.
etc_s(1*A,A) :-
    !.
etc_s(A*1,A) :-
    !.
etc_s(0/A,0) :-
    A =\= 0,
    !.
etc_s(_/0,0) :-
    throw(etc_divide_by_zero).
etc_s(1/A,1/A) :-
    !.
etc_s(A/1,A) :-
    !.
etc_s(0^_,0) :-
    !.
etc_s(_^0,1) :-
    !.
etc_s(1^_,1) :-
    !.
etc_s(A^1,A) :-
    !.
etc_s(A,B) :-
    A =.. [_,C,D],
    number(C),
    number(D),
    !,
    B is A.
etc_s(A,B) :-
    A =.. [C,D,E],
    !,
    etc_s(D,F),
    etc_s(E,G),
    B =.. [C,F,G].
etc_s(A,A).

% test cases
case(etc_derivative((x+1)*((x^2+2)*(x^3+3)),x,_ANSWER)).
case(etc_derivative(x/x/x/x/x/x/x/x/x/x,x,_ANSWER)).
case(etc_derivative(log(log(log(log(log(log(log(log(log(log(x)))))))))),x,_ANSWER)).
case(etc_derivative(x*x*x*x*x*x*x*x*x*x,x,_ANSWER)).
case(etc_integral(1*6*x^5,x,_ANSWER)).
case(etc_integral(1*sqrt(pi)/2*exp(-x^2),x,_ANSWER)).

test :-
    case(A),
    A,
    write(A),
    write('.\n'),
    fail.
test :-
    halt.
