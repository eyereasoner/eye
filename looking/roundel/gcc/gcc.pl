% Gray Code Counter
% Code from the book "Clause and Effect" Chapter 8

'https://josd.github.io/eye/ns#gcc'([[],_],[]).
'https://josd.github.io/eye/ns#gcc'([[C|Cs],S],[N|Ns]) :-
    gcc(C,S,N),
    'https://josd.github.io/eye/ns#gcc'([Cs,N],Ns).

gcc(C,[Qa,Qb,Qc],[Za,Zb,Zc]) :-
    neta(Qa,Qb,D1),
    netb(Qa,Qb,Qc,D2,D3),
    dff(D1,C,Qc,Zc),
    dff(D2,C,Qa,Za),
    dff(D3,C,Qb,Zb).

neta(A,B,Q) :-
    and(A,B,T1),
    inv(A,NA),
    inv(B,NB),
    and(NA,NB,T2),
    or(T1,T2,Q).

netb(A,B,C,Q1,Q2) :-
    and(A,C,T1),
    inv(C,NC),
    and(B,NC,T2),
    inv(A,NA),
    and(NA,C,T3),
    or(T1,T2,Q1),
    or(T2,T3,Q2).

dff(_,0,Q,Q).
dff(D,1,_,D).

and(0,0,0).
and(0,1,0).
and(1,0,0).
and(1,1,1).

or(0,0,0).
or(0,1,1).
or(1,0,1).
or(1,1,1).

inv(0,1).
inv(1,0).

% query
query('https://josd.github.io/eye/ns#gcc'([[1,1,1,1,1,1,1,1,1],[0,0,0]],_ANSWER)).

run :-
    query(Q),
    Q,
    writeq(Q),
    write('.\n'),
    fail;
    true.
