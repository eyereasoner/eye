:- op(1200, xfx, :+).

answer('<https://eyereasoner.github.io/ns#accessControl>'(john, report1, allow)).
answer('<https://eyereasoner.github.io/ns#accessControl>'(jane, report2, allow)).
answer('<https://eyereasoner.github.io/ns#accessControl>'(bob, report1, allow)).

% proof steps
step((policy(allow, A, _):+user_role(A, admin)), user_role(john, admin), policy(allow, john, sk_0)).
step((policy(deny, _, A):+resource_confidentiality(A, confidential)), resource_confidentiality(report1, confidential), policy(deny, sk_0, report1)).
step((policy(allow, A, _):+user_department(A, it)), user_department(jane, it), policy(allow, jane, sk_0)).
step((true:+'<https://eyereasoner.github.io/ns#accessControl>'(john, report1, _)), '<https://eyereasoner.github.io/ns#accessControl>'(john, report1, allow), true).
step((true:+'<https://eyereasoner.github.io/ns#accessControl>'(jane, report2, _)), '<https://eyereasoner.github.io/ns#accessControl>'(jane, report2, allow), true).
step((true:+'<https://eyereasoner.github.io/ns#accessControl>'(bob, report1, _)), '<https://eyereasoner.github.io/ns#accessControl>'(bob, report1, allow), true).
