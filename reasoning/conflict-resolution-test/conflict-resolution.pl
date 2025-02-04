% policy conflict resolution example

:- op(1200, xfx, :+).

:- discontiguous((:+)/2).

% context
'<https://eyereasoner.github.io/ns#accessControl>'(User, Resource, Decision) :-
    access_control(User, Resource, Decision).

% users and their roles
user_role(john, admin).
user_role(jane, it_staff).
user_role(bob, employee).

% resources and their confidentiality
resource_confidentiality(report1, confidential).
resource_confidentiality(report2, non_confidential).

% departments
user_department(john, hr).
user_department(jane, it).
user_department(bob, finance).

% policy 1: allow access if the user is an admin
policy(allow, User, _) :+
    user_role(User, admin).

% policy 2: deny access if the resource is confidential
policy(deny, _, Resource) :+
    resource_confidentiality(Resource, confidential).

% policy 3: allow access if the user is in the IT department
policy(allow, User, _) :+
    user_department(User, it).

% conflict resolution: deny takes precedence over allow
resolve_conflict(Policies, Decision) :-
    (   member(deny, Policies)
    ->  Decision = deny
    ;   Decision = allow
    ).

% main access control rule
access_control(User, Resource, Decision) :-
    findall(Policy, policy(Policy, User, Resource), Policies),
    resolve_conflict(Policies, Decision).

% query
true :+ '<https://eyereasoner.github.io/ns#accessControl>'(john, report1, _).
true :+ '<https://eyereasoner.github.io/ns#accessControl>'(jane, report2, _).
true :+ '<https://eyereasoner.github.io/ns#accessControl>'(bob, report1, _).
