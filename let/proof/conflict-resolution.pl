accessControl(john,report1,allow).
accessControl(jane,report2,allow).
accessControl(bob,report1,allow).

step(rule(accessControl(john, report1, A), answer(accessControl(john, report1, A))), accessControl(john, report1, allow), answer(accessControl(john, report1, allow))).
step(rule(accessControl(jane, report2, A), answer(accessControl(jane, report2, A))), accessControl(jane, report2, allow), answer(accessControl(jane, report2, allow))).
step(rule(accessControl(bob, report1, A), answer(accessControl(bob, report1, A))), accessControl(bob, report1, allow), answer(accessControl(bob, report1, allow))).
step(rule(user_role(A, admin), policy(allow, A, _)), user_role(john, admin), policy(allow, john, some0)).
step(rule(resource_confidentiality(A, confidential), policy(deny, _, A)), resource_confidentiality(report1, confidential), policy(deny, some1, report1)).
step(rule(user_department(A, it), policy(allow, A, _)), user_department(jane, it), policy(allow, jane, some2)).
