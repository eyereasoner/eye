% Healthcare access control
% Healthcare systems require strict access control to protect patient data
% while ensuring authorized personnel can access the information they need.

:- op(1200, xfx, :+).

:- discontiguous((:+)/2).
:- discontiguous(user_permission/3).

:- dynamic(user_permission/3).

% context
'urn:example:userPermission'(User, Permission, Patient) :-
    user_permission(User, Permission, Patient).

% users
user(alice).
user(bob).
user(charlie).
user(dave).

% roles
role(doctor).
role(nurse).
role(admin).
role(student).

% permissions
permission(view_patient_records).
permission(edit_patient_records).
permission(access_medications).
permission(access_billing).

% user-role assignments
user_role(alice, doctor).
user_role(bob, nurse).
user_role(charlie, admin).
user_role(dave, student).

% role-permission assignments
role_permission(doctor, view_patient_records).
role_permission(doctor, edit_patient_records).
role_permission(doctor, access_medications).
role_permission(nurse, view_patient_records).
role_permission(nurse, access_medications).
role_permission(admin, access_billing).

% patient-specific access
% doctors should only access their assigned patients
assigned_patient(alice, patient1).
assigned_patient(alice, patient2).
assigned_patient(bob, patient2).

% doctors and nurses can only view their assigned patients
user_permission(U, view_patient_records, P) :+
    user_role(U, R),
    role_permission(R, view_patient_records),
    assigned_patient(U, P).

% emergency overrides
% a doctor or nurse can override access rules in an emergency
emergency(patient3).

user_permission(U, view_patient_records, P) :+
    user_role(U, R),
    role_permission(R, view_patient_records),
    emergency(P).

% time-based constraints
% some sensitive actions should only happen during work hours
work_hours(8, 18).

user_permission([U, Time], edit_patient_records, P) :-
    user_role(U, R),
    role_permission(R, edit_patient_records),
    assigned_patient(U, P),
    work_hours(Start, End),
    Time >= Start,
    Time =< End.

% explicit denials
% some users might be explicitly denied certain permissions
denied(dave, view_patient_records).

user_permission(U, P, X) :+
    user_role(U, R),
    role_permission(R, P),
    \+denied(U, P),
    assigned_patient(U, X).

% query
true :+ 'urn:example:userPermission'(alice, _, patient1).
true :+ 'urn:example:userPermission'(bob, view_patient_records, patient2).
true :+ 'urn:example:userPermission'([alice, 7], edit_patient_records, patient1).
true :+ \+'urn:example:userPermission'([alice, 7], edit_patient_records, patient1).
true :+ 'urn:example:userPermission'(_, view_patient_records, patient3).
true :+ 'urn:example:userPermission'(dave, view_patient_records, patient1).
true :+ \+'urn:example:userPermission'(dave, view_patient_records, patient1).
