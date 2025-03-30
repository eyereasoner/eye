:- op(1200, xfx, :+).

answer('urn:example:accessControl'(john,report1,allow)).
answer('urn:example:accessControl'(jane,report2,allow)).
answer('urn:example:accessControl'(bob,report1,allow)).

step((policy(allow,A,B):+user_role(A,admin)),user_role(john,admin),policy(allow,john,sk_0)).
step((policy(deny,A,B):+resource_confidentiality(B,confidential)),resource_confidentiality(report1,confidential),policy(deny,sk_0,report1)).
step((policy(allow,A,B):+user_department(A,it)),user_department(jane,it),policy(allow,jane,sk_0)).
step((true:+'urn:example:accessControl'(john,report1,A)),'urn:example:accessControl'(john,report1,allow),true).
step((true:+'urn:example:accessControl'(jane,report2,A)),'urn:example:accessControl'(jane,report2,allow),true).
step((true:+'urn:example:accessControl'(bob,report1,A)),'urn:example:accessControl'(bob,report1,allow),true).
