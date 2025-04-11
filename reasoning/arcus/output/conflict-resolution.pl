:- op(1200, xfx, :+).

answer('urn:example:accessControl'(john, report1, allow)).
answer('urn:example:accessControl'(jane, report2, allow)).
answer('urn:example:accessControl'(bob, report1, allow)).

step((policy(allow, A, _):+user_role(A, admin)), user_role(john, admin), policy(allow, john, sk_0)).
step((policy(deny, _, A):+resource_confidentiality(A, confidential)), resource_confidentiality(report1, confidential), policy(deny, sk_0, report1)).
step((policy(allow, A, _):+user_department(A, it)), user_department(jane, it), policy(allow, jane, sk_0)).
step((true:+'urn:example:accessControl'(john, report1, _)), 'urn:example:accessControl'(john, report1, allow), true).
step((true:+'urn:example:accessControl'(jane, report2, _)), 'urn:example:accessControl'(jane, report2, allow), true).
step((true:+'urn:example:accessControl'(bob, report1, _)), 'urn:example:accessControl'(bob, report1, allow), true).
