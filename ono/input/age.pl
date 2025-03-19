% age checker

:- op(1200, xfx, :+).

% person data
'urn:example:birthDay'('urn:example:patH', [1944, 8, 21]).

% simulation date
'urn:example:date'(simulation1, [2025, 1, 18]).

% is the age of a person above some years?
'urn:example:ageAbove'(S, A) :-
    'urn:example:birthDay'(S, [Yb, Mb, Db]),
    'urn:example:date'(simulation1, [Yd, Md, Dd]),
    Yd-Yb+(Md-Mb)/12+(Dd-Db)/365 > A.

% query
true :+ 'urn:example:ageAbove'(_, 80).
