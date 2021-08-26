% Graph Literals to Augment Simple Statements

:- use_module(library(lists)).
:- use_module(library(iso_ext)).

p([[a,b,c]],o).
p([[r,s,t]],o).
p([[r,s,t],[d,e,f],[r,s,t]],o).

glass(p(A,B)) :-
    p(C,B),
    forall(member(D,A),member(D,C)),
    forall(member(E,C),member(E,A)).

% test cases
case(glass(p([[_A,b,c]],o))).
case(glass(p([[a,B,C],[a,B,C]],o))).
case(glass(p([[a,B,C],[_A,B,C]],o))).
case(glass(p([[r,_S,t]],o))).
case(glass(p([[d,e,f],[r,_S,t]],o))).
case(glass(p([[d,_E,f],[r,s,t]],o))).
case(glass(p([[d,e,f],[r,s,t],[d,_E,f]],o))).
case(glass(p([[d,_E,f],[r,_S,t],[d,e,f]],o))).

test :-
    case(A),
    A,
    writeq(A),
    write('.\n'),
    fail.
test :-
    halt.
