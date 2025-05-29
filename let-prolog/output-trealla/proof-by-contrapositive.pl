:- op(1200, xfx, :+).

answer((false:+ascribed(ground,wet))).
answer((false:+ascribed(it,raining))).

step(((false:+A):+(B:+A),(false:+B)),((ascribed(ground,wet):+ascribed(it,raining)),(false:+ascribed(ground,wet))),(false:+ascribed(it,raining))).
step((true:+(false:+A)),(false:+ascribed(ground,wet)),true).
step((true:+(false:+A)),(false:+ascribed(it,raining)),true).
