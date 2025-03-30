% Prolog theorem prover
% Richard A. O'Keefe
% from "Prolog Compared with Lisp?," SIGPLAN Notices, v. 18 #5, May 1983

:- op(1200, xfx, :+).
:- op(950, xfy, #).     % disjunction
:- op(850, xfy, &).     % conjunction
:- op(500, fx, +).      % assertion
:- op(500, fx, -).      % denial

% context
'urn:example:propositionProver'(Premise, Conclusion) :- implies(Premise, Conclusion).

implies(Premise, Conclusion) :-
    opposite(Conclusion, Denial),
    add_conjunction(Premise, Denial, fs([],[],[],[])).

opposite(F0 & G0, F1 # G1) :-
    !,
    opposite(F0, F1),
    opposite(G0, G1).
opposite(F1 # G1, F0 & G0) :-
    !,
    opposite(F1, F0),
    opposite(G1, G0).
opposite(+Atom, -Atom) :-
    !.
opposite(-Atom, +Atom).

add_conjunction(F, G, Set) :-
    expand(F, Set, Mid),
    expand(G, Mid, New),
    refute(New).

expand(_, refuted, refuted).
expand(F & G, fs(D,_,_,_), refuted) :-
    includes_item(D, F & G),
    !.
expand(F & G, fs(D,C,P,N), fs(D,C,P,N)) :-
    includes_item(C, F & G),
    !.
expand(F & G, fs(D,C,P,N), New) :-
    !,
    expand(F, fs(D,[F & G|C],P,N), Mid),
    expand(G, Mid, New).
expand(F # G, fs(D,C,P,N), Set) :-
    !,
    opposite(F # G, Conj),
    extend(Conj, D, C, D1, fs(D1,C,P,N), Set).
expand(+Atom, fs(D,C,P,N), Set) :-
    !,
    extend(Atom, P, N, P1, fs(D,C,P1,N), Set).
expand(-Atom, fs(D,C,P,N), Set) :-
    extend(Atom, N, P, N1, fs(D,C,P,N1), Set).

includes_item([Head|_], Head) :-
    !.
includes_item([_|Tail], This) :-
    includes_item(Tail, This).

extend(Exp, _, Neg, _, _, refuted) :-
    includes_item(Neg, Exp),
    !.
extend(Exp, Pos, _, Pos, Set, Set) :-
    includes_item(Pos, Exp),
    !.
extend(Exp, Pos, _, [Exp|Pos], Set, Set).

refute(refuted).
refute(fs([F1 & G1|D], C, P, N)) :-
    opposite(F1, F0),
    opposite(G1, G0),
    Set = fs(D, C, P, N),
    add_conjunction(F0, G1, Set),
    add_conjunction(F0, G0, Set),
    add_conjunction(F1, G0, Set).

% query
true :+ 'urn:example:propositionProver'(-a, +to_be # -to_be).
true :+ 'urn:example:propositionProver'(-a & -a, -a).
true :+ 'urn:example:propositionProver'(-a, +b # -a).
true :+ 'urn:example:propositionProver'(-a & -b, -b & -a).
true :+ 'urn:example:propositionProver'(-a, -b # (+b & -a)).
true :+ 'urn:example:propositionProver'(-a # (-b # +c), -b # (-a # +c)).
true :+ 'urn:example:propositionProver'(-a # +b, (+b & -c) # (-a # +c)).
true :+ 'urn:example:propositionProver'((-a # +c) & (-b # +c), (-a & -b) # +c).
