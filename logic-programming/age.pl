% age checker

:- op(1200, xfx, :+).

% person data
'<urn:example:birthDay>'('<urn:example:patH>', [1944, 8, 21]).

% is the age of a person above some years?
'<urn:example:ageAbove>'(S, A) :-
    '<urn:example:birthDay>'(S, [Yb, Mb, Db]),
    Ya is Yb+A,
    date_time_stamp(date(Ya, Mb, Db, 22, 0, 0, _, _, _), Tc),
    get_time(T),
    Tc =< T.

% query
true :+ '<urn:example:ageAbove>'(_, 80).
