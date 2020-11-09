% Fast Fourier Transform
% Code from the book "Clause and Effect" Chapter 10

%:- use_module(library(lists)).
:- initialization(test).

test :-
    fft([0,1,2,3,4,5,6,7],FFT),
    FFT = [n(64,op(+,49,63)),n(63,op(*,62,52)),n(62,w^7),n(61,op(+,42,60)),n(60,op(*,47,44)),n(59,op(+,31,58)),n(58,op(*,57,38)),n(57,w^5),n(56,op(+,11,55)),n(55,op(*,24,21)),n(54,op(+,49,53)),n(53,op(*,50,52)),n(52,op(+,34,51)),n(51,op(*,47,36)),n(50,w^3),n(49,op(+,26,48)),n(48,op(*,47,29)),n(47,w^6),n(46,op(+,42,45)),n(45,op(*,27,44)),n(44,op(+,15,43)),n(43,op(*,24,19)),n(42,op(+,5,41)),n(41,op(*,24,9)),n(40,op(+,31,39)),n(39,op(*,32,38)),n(38,op(+,34,37)),n(37,op(*,27,36)),n(36,op(+,16,35)),n(35,op(*,24,17)),n(34,op(+,12,33)),n(33,op(*,24,13)),n(32,w^1),n(31,op(+,26,30)),n(30,op(*,27,29)),n(29,op(+,6,28)),n(28,op(*,24,7)),n(27,w^2),n(26,op(+,1,25)),n(25,op(*,24,3)),n(24,w^4),n(23,op(+,11,22)),n(22,op(*,2,21)),n(21,op(+,15,20)),n(20,op(*,2,19)),n(19,op(+,16,18)),n(18,op(*,2,17)),n(17,a(7)),n(16,a(3)),n(15,op(+,12,14)),n(14,op(*,2,13)),n(13,a(5)),n(12,a(1)),n(11,op(+,5,10)),n(10,op(*,2,9)),n(9,op(+,6,8)),n(8,op(*,2,7)),n(7,a(6)),n(6,a(2)),n(5,op(+,1,4)),n(4,op(*,2,3)),n(3,a(4)),n(2,w^0),n(1,a(0))],
    write(true),
    halt.

fft(A,L) :-
    eval(p(A,w^0),X0,8),
    eval(p(A,w^1),X1,8),
    eval(p(A,w^2),X2,8),
    eval(p(A,w^3),X3,8),
    eval(p(A,w^4),X4,8),
    eval(p(A,w^5),X5,8),
    eval(p(A,w^6),X6,8),
    eval(p(A,w^7),X7,8),
    gen((X0;X1;X2;X3;X4;X5;X6;X7),[]-L,_).

eval(p([I],_),a(I),_).
eval(p(L,V^P),A1+V^P*A2,N) :-
    alternate(L,L1,L2),
    P1 is (P*2) mod N,
    eval(p(L1,V^P1),A1,N),
    eval(p(L2,V^P1),A2,N).

alternate([],[],[]).
alternate([A,B|T],[A|T1],[B|T2]) :-
    alternate(T,T1,T2).

% gen(InTree,ListOutFront-ListOutBack,NodeIndex)
gen(X+Y,L0-L3,A) :-
    !,
    gen(X,L0-L1,A1),
    gen(Y,L1-L2,A2),
    node(n(A,op(+,A1,A2)),L2-L3).
gen(X*Y,L0-L3,A) :-
    !,
    gen(X,L0-L1,A1),
    gen(Y,L1-L2,A2),
    node(n(A,op(*,A1,A2)),L2-L3).
gen((X;Y),L0-L2,_) :-
    !,
    gen(X,L0-L1,_),
    gen(Y,L1-L2,_).
gen(X,L0-L1,A) :-
    node(n(A,X),L0-L1).

% node(TryNode,OutDiffList)
node(n(1,N),[]-[n(1,N)]) :-
    !.
node(N,L-L) :-
    memberchk(N,L),
    !.
node(n(A1,N1),[n(A,N)|T]-[n(A1,N1),n(A,N)|T]) :-
    A1 is A+1.
