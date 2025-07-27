propositionProver(-a,+to_be# -to_be).
propositionProver(-a& -a,-a).
propositionProver(-a,+b# -a).
propositionProver(-a& -b,-b& -a).
propositionProver(-a,-b# +b& -a).
propositionProver(-a# -b# +c,-b# -a# +c).
propositionProver(-a# +b,+b& -c# -a# +c).
propositionProver((-a# +c)&(-b# +c),-a& -b# +c).

step(rule(propositionProver(-a, +to_be# -to_be), answer(propositionProver(-a, +to_be# -to_be))), propositionProver(-a, +to_be# -to_be), answer(propositionProver(-a, +to_be# -to_be))).
step(rule(propositionProver(-a& -a, -a), answer(propositionProver(-a& -a, -a))), propositionProver(-a& -a, -a), answer(propositionProver(-a& -a, -a))).
step(rule(propositionProver(-a, +b# -a), answer(propositionProver(-a, +b# -a))), propositionProver(-a, +b# -a), answer(propositionProver(-a, +b# -a))).
step(rule(propositionProver(-a& -b, -b& -a), answer(propositionProver(-a& -b, -b& -a))), propositionProver(-a& -b, -b& -a), answer(propositionProver(-a& -b, -b& -a))).
step(rule(propositionProver(-a, -b# +b& -a), answer(propositionProver(-a, -b# +b& -a))), propositionProver(-a, -b# +b& -a), answer(propositionProver(-a, -b# +b& -a))).
step(rule(propositionProver(-a# -b# +c, -b# -a# +c), answer(propositionProver(-a# -b# +c, -b# -a# +c))), propositionProver(-a# -b# +c, -b# -a# +c), answer(propositionProver(-a# -b# +c, -b# -a# +c))).
step(rule(propositionProver(-a# +b, +b& -c# -a# +c), answer(propositionProver(-a# +b, +b& -c# -a# +c))), propositionProver(-a# +b, +b& -c# -a# +c), answer(propositionProver(-a# +b, +b& -c# -a# +c))).
step(rule(propositionProver((-a# +c)&(-b# +c), -a& -b# +c), answer(propositionProver((-a# +c)&(-b# +c), -a& -b# +c))), propositionProver((-a# +c)&(-b# +c), -a& -b# +c), answer(propositionProver((-a# +c)&(-b# +c), -a& -b# +c))).
