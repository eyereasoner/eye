% Meta-interpretation
% Original code from https://www.youtube.com/watch?v=nmBkU-l1zyc&t=1870s

mi(Goal,prolog) :-
    mi([mi([mi([mi([mi([Goal])])])])]).

mi([]).
mi([G|Gs]) :-
    head_body_(G,Goals,Gs),
    mi(Goals).

head_body_(mi([]),Rs,Rs).
head_body_(mi([G|Gs]),[head_body_(G,Goals,Gs),mi(Goals)|Rs],Rs).

head_body_(head_body_(Head,Goals0,Goals),Rs,Rs) :-
    head_body_(Head,Goals0,Goals).

head_body_(natnum(0),Rs,Rs).
head_body_(natnum(s(X)),[natnum(X)|Rs],Rs).

% query implies goal
mi(natnum(_X),prolog) -: goal.

% 10 answers are fine
limited_answer(10).
