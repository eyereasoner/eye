% Healthcare access control

:- op(1200, xfx, :+).

:- discontiguous (:+)/2.
:- discontiguous user_perm/3.

:- dynamic(user_perm/3).

% Users
user(alice).
user(bob).
user(charlie).
user(dave).

% Roles
role(doctor).
role(nurse).
role(admin).
role(student).

% Permissions
perm(view_patient_records).
perm(edit_patient_records).
perm(access_medications).
perm(access_billing).

% User-Role Assignments
user_role(alice, doctor).
user_role(bob, nurse).
user_role(charlie, admin).
user_role(dave, student).

% Role-Permission Assignments
role_perm(doctor, view_patient_records).
role_perm(doctor, edit_patient_records).
role_perm(doctor, access_medications).
role_perm(nurse, view_patient_records).
role_perm(nurse, access_medications).
role_perm(admin, access_billing).

% Patient-Specific Access
% Doctors should only access their assigned patients.
assigned_patient(alice, patient1).
assigned_patient(alice, patient2).
assigned_patient(bob, patient2).

% Doctors/Nurses can only view their assigned patients.
user_perm(U, view_patient_records, P) :+
    user_role(U, R),
    role_perm(R, view_patient_records),
    assigned_patient(U, P).

% Emergency Overrides
% A doctor or nurse can override access rules in an emergency.
emergency(patient3).

user_perm(U, view_patient_records, P) :+
    user_role(U, R),
    role_perm(R, view_patient_records),
    emergency(P).

% Time-Based Constraints
% Some sensitive actions should only happen during work hours.
work_hours(8, 18).

user_perm(U, edit_patient_records, P, Time) :-
    user_role(U, R),
    role_perm(R, edit_patient_records),
    assigned_patient(U, P),
    work_hours(Start, End),
    Time >= Start,
    Time =< End.

% Explicit Denials
% Some users might be explicitly denied certain permissions.
denied(dave, view_patient_records).

user_perm(U, P, X) :+
    user_role(U, R),
    role_perm(R, P),
    \+denied(U, P),
    assigned_patient(U, X).

% query
true :+ user_perm(alice, _, patient1).
true :+ user_perm(bob, view_patient_records, patient2).
true :+ user_perm(alice, edit_patient_records, patient1, 7).
true :+ \+user_perm(alice, edit_patient_records, patient1, 7).
true :+ user_perm(_, view_patient_records, patient3).
true :+ user_perm(dave, view_patient_records, patient1).
true :+ \+user_perm(dave, view_patient_records, patient1).
