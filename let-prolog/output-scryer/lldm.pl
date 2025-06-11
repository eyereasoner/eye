:- op(1200, xfx, :+).

answer(lld_alarm(meas47)).

step((lld_alarm(A):+d(A,B),(B< -1.25;B>1.25)),(d(meas47,-1.9082339805374957),(-1.9082339805374957< -1.25;-1.9082339805374957>1.25)),lld_alarm(meas47)).
step((true:+lld_alarm(meas47)),lld_alarm(meas47),true).
