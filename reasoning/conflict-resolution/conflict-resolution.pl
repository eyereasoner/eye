% users and their roles
'<urn:example:userRole>'('<urn:example:john>', '<urn:example:admin>').
'<urn:example:userRole>'('<urn:example:jane>', '<urn:example:itStaff>').
'<urn:example:userRole>'('<urn:example:bob>', '<urn:example:employee>').

% resources and their confidentiality
'<urn:example:userConfidentiality>'('<urn:example:report1>', '<urn:example:confidential').
'<urn:example:userConfidentiality>'('<urn:example:report2>', '<urn:example:nonConfidential>').

% departments
'<urn:example:userDepartment>'('<urn:example:john>', '<urn:example:hr>').
'<urn:example:userDepartment>'('<urn:example:jane>', '<urn:example:it>').
'<urn:example:userDepartment>'('<urn:example:bob>', '<urn:example:finance>').

% policy 1: allow access if the user is an admin
'<urn:example:policy>'('<urn:example:allow>', [User, _]) :-
    '<urn:example:userRole>'(User, '<urn:example:admin>').

% policy 2: deny access if the resource is confidential
'<urn:example:policy>'('<urn:example:deny>', [_, Resource]) :-
    '<urn:example:userConfidentiality>'(Resource, '<urn:example:confidential>').

% policy 3: allow access if the user is in the IT department
'<urn:example:policy>'('<urn:example:allow>', [User, _]) :-
    '<urn:example:userDepartment>'(User, it).

% conflict resolution: deny takes precedence over allow
'<urn:example:resolveConflict>'(Policies, Decision) :-
    (   member('<urn:example:deny>', Policies)
    ->  Decision = '<urn:example:deny>'
    ;   Decision = '<urn:example:allow>'
    ).

% main access control rule
'<urn:example:accessControl>'(User, [Resource, Decision]) :-
    findall(Policy, '<urn:example:policy>'(Policy, [User, Resource]), Policies),
    '<urn:example:resolveConflict>'(Policies, Decision).
