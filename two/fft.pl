% Fast Fourier Transform
% Code from the book "Clause and Effect" Chapter 10

:- use_module(library(lists)).

web_nsp(etc_,'http://josd.github.io/eye/two/cases#').

etc_fft(A,L) :-
    etc_eval(etc_p(A,etc_w^0),X0,8),
    etc_eval(etc_p(A,etc_w^1),X1,8),
    etc_eval(etc_p(A,etc_w^2),X2,8),
    etc_eval(etc_p(A,etc_w^3),X3,8),
    etc_eval(etc_p(A,etc_w^4),X4,8),
    etc_eval(etc_p(A,etc_w^5),X5,8),
    etc_eval(etc_p(A,etc_w^6),X6,8),
    etc_eval(etc_p(A,etc_w^7),X7,8),
    etc_gen((X0;X1;X2;X3;X4;X5;X6;X7),[]-L,_).

etc_eval(etc_p([I],_),etc_a(I),_).
etc_eval(etc_p(L,V^P),A1+V^P*A2,N) :-
    etc_alternate(L,L1,L2),
    P1 is (P*2) mod N,
    etc_eval(etc_p(L1,V^P1),A1,N),
    etc_eval(etc_p(L2,V^P1),A2,N).

etc_alternate([],[],[]).
etc_alternate([A,B|T],[A|T1],[B|T2]) :-
    etc_alternate(T,T1,T2).

% etc_gen(InTree,ListOutFront-ListOutBack,NodeIndex)
etc_gen(X+Y,L0-L3,A) :-
    !,
    etc_gen(X,L0-L1,A1),
    etc_gen(Y,L1-L2,A2),
    etc_node(etc_n(A,etc_op(+,A1,A2)),L2-L3).
etc_gen(X*Y,L0-L3,A) :-
    !,
    etc_gen(X,L0-L1,A1),
    etc_gen(Y,L1-L2,A2),
    etc_node(etc_n(A,etc_op(*,A1,A2)),L2-L3).
etc_gen((X;Y),L0-L2,_) :-
    !,
    etc_gen(X,L0-L1,_),
    etc_gen(Y,L1-L2,_).
etc_gen(X,L0-L1,A) :-
    etc_node(etc_n(A,X),L0-L1).

% etc_node(TryNode,OutDiffList)
etc_node(etc_n(1,N),[]-[etc_n(1,N)]) :-
    !.
etc_node(N,L-L) :-
    memberchk(N,L),
    !.
etc_node(etc_n(A1,N1),[etc_n(A,N)|T]-[etc_n(A1,N1),etc_n(A,N)|T]) :-
    A1 is A+1.

% test cases
case(etc_fft([0,1,2,3,4,5,6,7],_ANSWER)).

test :-
    case(A),
    A,
    write(A),
    write('.\n'),
    fail.
test :-
    halt.
