% deontic logic

:- style_check(-discontiguous).
:- table(not/1).

% deontic logic rules
'<https://eyereasoner.github.io/ns#obligatory>'(A, true) :-
    not('<https://eyereasoner.github.io/ns#permitted>'(B, true)),
    negation(B, A).

'<https://eyereasoner.github.io/ns#permitted>'(A, true) :-
    not('<https://eyereasoner.github.io/ns#obligatory>'(B, true)),
    negation(B, A).

'<https://eyereasoner.github.io/ns#forbidden>'(A, true) :-
    '<https://eyereasoner.github.io/ns#obligatory>'(B, true),
    negation(B, A).

not('<https://eyereasoner.github.io/ns#obligatory>'(A, true)) :-
    '<https://eyereasoner.github.io/ns#obligatory>'(B, true),
    negation(B, A).
not('<https://eyereasoner.github.io/ns#permitted>'(A, true)) :-
    '<https://eyereasoner.github.io/ns#forbidden>'(A, true).
not('<https://eyereasoner.github.io/ns#forbidden>'(A, true)) :-
    '<https://eyereasoner.github.io/ns#permitted>'(A, true).

negation(not(A), A) :-
    !.
negation(A, not(A)).

% deontic logic examples
'<https://eyereasoner.github.io/ns#obligatory>'(pay_taxes, true).
'<https://eyereasoner.github.io/ns#obligatory>'(stop_at_red_light, true).
'<https://eyereasoner.github.io/ns#forbidden>'(steal, true).
'<https://eyereasoner.github.io/ns#permitted>'((drink, not(drive)), true).
'<https://eyereasoner.github.io/ns#permitted>'((not(drink), drive), true).
not('<https://eyereasoner.github.io/ns#permitted>'((drink, drive)), true).
