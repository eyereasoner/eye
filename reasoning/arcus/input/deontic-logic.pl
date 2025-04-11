% deontic logic example
% See https://en.wikipedia.org/wiki/Deontic_logic

:- op(1200, xfx, :+).

:- discontiguous((:+)/2).
:- discontiguous('urn:example:not'/1).
:- discontiguous('urn:example:obligatory'/1).
:- discontiguous('urn:example:permitted'/1).
:- discontiguous('urn:example:forbidden'/1).

:- dynamic('urn:example:not'/1).
:- dynamic('urn:example:obligatory'/1).
:- dynamic('urn:example:permitted'/1).
:- dynamic('urn:example:forbidden'/1).

% deontic logic rules
'urn:example:obligatory'(A) :+ 'urn:example:not'('urn:example:permitted'(B)), negation(B, A).
'urn:example:permitted'(A) :+ 'urn:example:not'('urn:example:obligatory'(B)), negation(B, A).
'urn:example:forbidden'(A) :+ 'urn:example:obligatory'(B), negation(B, A).

'urn:example:not'('urn:example:obligatory'(A)) :+ 'urn:example:obligatory'(B), negation(B, A).
'urn:example:not'('urn:example:permitted'(A)) :+ 'urn:example:forbidden'(A).
'urn:example:not'('urn:example:forbidden'(A)) :+ 'urn:example:permitted'(A).

negation('urn:example:not'(A), A) :- !.
negation(A, 'urn:example:not'(A)).

% examples
'urn:example:obligatory'(pay_taxes).
'urn:example:obligatory'(stop_at_red_light).
'urn:example:forbidden'(steal).
'urn:example:permitted'((drink, 'urn:example:not'(drive))).
'urn:example:permitted'(('urn:example:not'(drink), drive)).
'urn:example:not'('urn:example:permitted'((drink, drive))).

% query
true :+ 'urn:example:not'(_).
true :+ 'urn:example:obligatory'(_).
true :+ 'urn:example:permitted'(_).
true :+ 'urn:example:forbidden'(_).
