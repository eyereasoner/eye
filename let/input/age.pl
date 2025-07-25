% age checker

% person data
birthDay(patH, [1944, 8, 21]).

% simulation date
simulation_date(simulation1, [2025, 1, 18]).

% is the age of a person above some years?
ageAbove(S, A) :-
    birthDay(S, [Yb, Mb, Db]),
    simulation_date(simulation1, [Yd, Md, Dd]),
    Yd-Yb+(Md-Mb)/12+(Dd-Db)/365 > A.

% query
true :+ ageAbove(_, 80).
