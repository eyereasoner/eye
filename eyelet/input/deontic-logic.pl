% deontic logic example
% See https://en.wikipedia.org/wiki/Deontic_logic

:- op(1200, xfx, :+).

:- discontiguous((:+)/2).
:- discontiguous(negative/1).
:- discontiguous(obligatory/1).
:- discontiguous(permitted/1).
:- discontiguous(forbidden/1).

:- dynamic(negative/1).
:- dynamic(obligatory/1).
:- dynamic(permitted/1).
:- dynamic(forbidden/1).

% deontic logic rules
obligatory(A) :+ negative(permitted(B)), negation(B, A).
permitted(A) :+ negative(obligatory(B)), negation(B, A).
forbidden(A) :+ obligatory(B), negation(B, A).

negative(obligatory(A)) :+ obligatory(B), negation(B, A).
negative(permitted(A)) :+ forbidden(A).
negative(forbidden(A)) :+ permitted(A).

negation(negative(A), A) :- !.
negation(A, negative(A)).

% examples
obligatory(pay_taxes).
obligatory(stop_at_red_light).
forbidden(steal).
permitted((drink, negative(drive))).
permitted((negative(drink), drive)).
negative(permitted((drink, drive))).

% query
true :+ negative(_).
true :+ obligatory(_).
true :+ permitted(_).
true :+ forbidden(_).
