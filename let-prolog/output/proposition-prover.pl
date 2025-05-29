:- op(1200, xfx, :+).

answer(propositionProver(-a, +to_be# -to_be)).
answer(propositionProver(-a& -a, -a)).
answer(propositionProver(-a, +b# -a)).
answer(propositionProver(-a& -b, -b& -a)).
answer(propositionProver(-a, -b# +b& -a)).
answer(propositionProver(-a# -b# +c, -b# -a# +c)).
answer(propositionProver(-a# +b, +b& -c# -a# +c)).
answer(propositionProver((-a# +c)&(-b# +c), -a& -b# +c)).

step((true:+propositionProver(-a, +to_be# -to_be)), propositionProver(-a, +to_be# -to_be), true).
step((true:+propositionProver(-a& -a, -a)), propositionProver(-a& -a, -a), true).
step((true:+propositionProver(-a, +b# -a)), propositionProver(-a, +b# -a), true).
step((true:+propositionProver(-a& -b, -b& -a)), propositionProver(-a& -b, -b& -a), true).
step((true:+propositionProver(-a, -b# +b& -a)), propositionProver(-a, -b# +b& -a), true).
step((true:+propositionProver(-a# -b# +c, -b# -a# +c)), propositionProver(-a# -b# +c, -b# -a# +c), true).
step((true:+propositionProver(-a# +b, +b& -c# -a# +c)), propositionProver(-a# +b, +b& -c# -a# +c), true).
step((true:+propositionProver((-a# +c)&(-b# +c), -a& -b# +c)), propositionProver((-a# +c)&(-b# +c), -a& -b# +c), true).
