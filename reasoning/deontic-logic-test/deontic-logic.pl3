% deontic logic example

:- op(1200, xfx, :+).

:- discontiguous((:+)/2).
:- discontiguous('<urn:knows:not>'/1).
:- discontiguous('<urn:knows:obligatory>'/1).
:- discontiguous('<urn:knows:permitted>'/1).
:- discontiguous('<urn:knows:forbidden>'/1).

:- dynamic('<urn:knows:not>'/1).
:- dynamic('<urn:knows:obligatory>'/1).
:- dynamic('<urn:knows:permitted>'/1).
:- dynamic('<urn:knows:forbidden>'/1).

% deontic logic rules
'<urn:knows:obligatory>'(A) :+ '<urn:knows:not>'('<urn:knows:permitted>'(B)), negation(B, A).
'<urn:knows:permitted>'(A) :+ '<urn:knows:not>'('<urn:knows:obligatory>'(B)), negation(B, A).
'<urn:knows:forbidden>'(A) :+ '<urn:knows:obligatory>'(B), negation(B, A).

'<urn:knows:not>'('<urn:knows:obligatory>'(A)) :+ '<urn:knows:obligatory>'(B), negation(B, A).
'<urn:knows:not>'('<urn:knows:permitted>'(A)) :+ '<urn:knows:forbidden>'(A).
'<urn:knows:not>'('<urn:knows:forbidden>'(A)) :+ '<urn:knows:permitted>'(A).

negation('<urn:knows:not>'(A), A) :- !.
negation(A, '<urn:knows:not>'(A)).

% examples
'<urn:knows:obligatory>'(pay_taxes).
'<urn:knows:obligatory>'(stop_at_red_light).
'<urn:knows:forbidden>'(steal).
'<urn:knows:permitted>'((drink, '<urn:knows:not>'(drive))).
'<urn:knows:permitted>'(('<urn:knows:not>'(drink), drive)).
'<urn:knows:not>'('<urn:knows:permitted>'((drink, drive))).

% query
true :+ '<urn:knows:not>'(_).
true :+ '<urn:knows:obligatory>'(_).
true :+ '<urn:knows:permitted>'(_).
true :+ '<urn:knows:forbidden>'(_).
