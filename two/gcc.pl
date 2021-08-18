% Gray Code Counter
% Code from the book "Clause and Effect" Chapter 8

nsp(etc_,'http://josd.github.io/eye/two/cases#').

etc_and(0,0,0).
etc_and(0,1,0).
etc_and(1,0,0).
etc_and(1,1,1).

etc_or(0,0,0).
etc_or(0,1,1).
etc_or(1,0,1).
etc_or(1,1,1).

etc_inv(0,1).
etc_inv(1,0).

etc_dff(_,0,Q,Q).
etc_dff(D,1,_,D).

etc_neta(A,B,Q) :-
    etc_and(A,B,T1),
    etc_inv(A,NA),
    etc_inv(B,NB),
    etc_and(NA,NB,T2),
    etc_or(T1,T2,Q).

etc_netb(A,B,C,Q1,Q2) :-
    etc_and(A,C,T1),
    etc_inv(C,NC),
    etc_and(B,NC,T2),
    etc_inv(A,NA),
    etc_and(NA,C,T3),
    etc_or(T1,T2,Q1),
    etc_or(T2,T3,Q2).

etc_gcc(C,etc_s(Qa,Qb,Qc),etc_s(Za,Zb,Zc)) :-
    etc_netb(Qa,Qb,Qc,D1,D2),
    etc_neta(Qa,Qb,D3),
    etc_dff(D1,C,Qa,Za),
    etc_dff(D2,C,Qb,Zb),
    etc_dff(D3,C,Qc,Zc).

etc_testgcc([],_,[]).
etc_testgcc([C|Cs],S,[N|Ns]) :-
    etc_gcc(C,S,N),
    etc_testgcc(Cs,N,Ns).

% test cases
case(etc_testgcc([1,1,1,1,1,1,1,1,1],etc_s(0,0,0),_ANSWER)).

test :-
    case(A),
    A,
    write(A),
    write('.\n'),
    fail.
test :-
    halt.
