% Graph Literals to Augment Simple Statements

p([[a,b,c]],o).
p([[r,s,t]],o).
p([[r,s,t],[d,e,f],[r,s,t]],o).

q(A,B) :-
    p(C,B),
    forall(member(D,A),member(D,C)),
    forall(member(E,C),member(E,A)).

% query implies goal
q([[_A,b,c]],o) -: goal.
q([[a,B,C],[a,B,C]],o) -: goal.
q([[a,B,C],[_A,B,C]],o) -: goal.
q([[r,_S,t]],o) -: goal.
q([[d,e,f],[r,_S,t]],o) -: goal.
q([[d,_E,f],[r,s,t]],o) -: goal.
q([[d,e,f],[r,s,t],[d,_E,f]],o) -: goal.
q([[d,_E,f],[r,_S,t],[d,e,f]],o) -: goal.
