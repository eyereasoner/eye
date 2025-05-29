% If you have more than 4 dogs you need a license.

:- op(1200, xfx, :+).

hasDog(alice, dog1).
hasDog(alice, dog2).
hasDog(alice, dog3).
hasDog(alice, dog4).
hasDog(alice, dog5).
hasDog(bob, dog6).
hasDog(bob, dog7).

mustHave(Subject, dogLicense) :-
    hasDog(Subject, _),
    findall(Dog, hasDog(Subject, Dog), List),
    length(List, Count),
    Count > 4.

% query
true :+ mustHave(_, _).
