% See Chaper 11 of "Thinking as Computation" from Hector J. Levesque

:- initialization(main).

main :-
    estunsat(2,[[on(a,b)],[on(b,c)],[green(a)],[not(green(c))]],[on(X,Y),green(X),not(green(Y))]),
    writeln([on(X,Y),green(X),not(green(Y))]),
    halt.

% Dclauses in DL are unsat, after making N copies of them.
unsat(N,DL) :- copies(N,DL,DL2), nopick(DL2,[]).

% copies(N,X,Y): Y is the result of making N copies of list X.
copies(0,_,[]).
copies(N,X,Y) :- 
   N>0, N1 is N-1, copies(N1,X,L), copy_term(X,X2), append(X2,L,Y).

% nopick(DL,P): there is no way to pick literals from the
% dclauses in DL, given that the ones in P have been picked.
nopick([[]|_],_).
nopick([[L|D]|T],P) :- unpickable(L,T,P), nopick([D|T],P).

% The literal L cannot be picked.
unpickable(L,_,P) :- member_neg(L,P).  % Negation of L is picked.
unpickable(L,T,P) :- nopick(T,[L|P]).  % Cannot add L to picked.

% As before
member_neg(A,P) :- member(not(A),P).
member_neg(not(A),P) :- member(A,P).

%------------------------------------------------------------------
% Dclauses DL entails Q using unsat.
estunsat(N,DL,Q) :- negs(Q,NQ), unsat(N,[NQ|DL]).

% As before
negs([],[]).
negs([not(A)|T],[A|NT]) :- negs(T,NT).
negs([A|T],[not(A)|NT]) :- \+ A=not(_), negs(T,NT).
