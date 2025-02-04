% policy conflict resolution example

:- op(1200, xfx, :+).
:- discontiguous (:+)/2.

% context
'<https://eyereasoner.github.io/ns#accessControl>'(User, Resource, Decision) :-
    access_control(User, Resource, Decision).

% Define the users and their roles
user_role(john, admin).
user_role(jane, it_staff).
user_role(bob, employee).

% Define the resources and their confidentiality
resource_confidentiality(report1, confidential).
resource_confidentiality(report2, non_confidential).

% Define the departments
user_department(john, hr).
user_department(jane, it).
user_department(bob, finance).

% Policy 1: Allow access if the user is an admin
policy(allow, User, _) :+
    user_role(User, admin).

% Policy 2: Deny access if the resource is confidential
policy(deny, _, Resource) :+
    resource_confidentiality(Resource, confidential).

% Policy 3: Allow access if the user is in the IT department
policy(allow, User, _) :+
    user_department(User, it).

% Conflict resolution: Deny takes precedence over allow
resolve_conflict(Policies, Decision) :-
    member(deny, Policies) -> Decision = deny ; Decision = allow.

% Main access control rule
access_control(User, Resource, Decision) :-
    findall(Policy, policy(Policy, User, Resource), Policies),
    resolve_conflict(Policies, Decision).

% query
true :+ '<https://eyereasoner.github.io/ns#accessControl>'(john, report1, _).
true :+ '<https://eyereasoner.github.io/ns#accessControl>'(jane, report2, _).
true :+ '<https://eyereasoner.github.io/ns#accessControl>'(bob, report1, _).
