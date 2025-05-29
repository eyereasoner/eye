:- op(1200, xfx, :+).

answer(mustHave(alice,dogLicense)).

step((true:+mustHave(A,B)),mustHave(alice,dogLicense),true).
