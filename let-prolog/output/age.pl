:- op(1200, xfx, :+).

answer(ageAbove(patH, 80)).

step((true:+ageAbove(_, 80)), ageAbove(patH, 80), true).
