% Graph Literals to Augment Simple Statements

:- use_module(library(iso_ext)).
:- use_module(library(lists)).

p([[a,b,c]],o).
p([[r,s,t]],o).
p([[r,s,t],[d,e,f],[r,s,t]],o).

q(A,B) :-
    p(C,B),
    forall(member(D,A),member(D,C)),
    forall(member(E,C),member(E,A)).

% query
query(q([[_A,b,c]],o)).
query(q([[a,B,C],[a,B,C]],o)).
query(q([[a,B,C],[_A,B,C]],o)).
query(q([[r,_S,t]],o)).
query(q([[d,e,f],[r,_S,t]],o)).
query(q([[d,_E,f],[r,s,t]],o)).
query(q([[d,e,f],[r,s,t],[d,_E,f]],o)).
query(q([[d,_E,f],[r,_S,t],[d,e,f]],o)).
