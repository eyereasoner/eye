:- op(1200, xfx, :+).

answer(mustHave(alice, dogLicense)).

step((true:+mustHave(_, _)), mustHave(alice, dogLicense), true).
