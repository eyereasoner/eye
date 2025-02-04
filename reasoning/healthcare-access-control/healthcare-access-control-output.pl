:- op(1200, xfx, :+).

answer(user_perm(alice, view_patient_records, patient1)).
answer(user_perm(alice, edit_patient_records, patient1)).
answer(user_perm(alice, access_medications, patient1)).
answer(user_perm(bob, view_patient_records, patient2)).
answer(\+user_perm(alice, edit_patient_records, patient1, 7)).
answer(user_perm(alice, view_patient_records, patient3)).
answer(user_perm(bob, view_patient_records, patient3)).
answer(\+user_perm(dave, view_patient_records, patient1)).

% proof steps
step((user_perm(A, view_patient_records, B):+user_role(A, C), role_perm(C, view_patient_records), assigned_patient(A, B)), (user_role(alice, doctor), role_perm(doctor, view_patient_records), assigned_patient(alice, patient1)), user_perm(alice, view_patient_records, patient1)).
step((user_perm(A, view_patient_records, B):+user_role(A, C), role_perm(C, view_patient_records), assigned_patient(A, B)), (user_role(alice, doctor), role_perm(doctor, view_patient_records), assigned_patient(alice, patient2)), user_perm(alice, view_patient_records, patient2)).
step((user_perm(A, view_patient_records, B):+user_role(A, C), role_perm(C, view_patient_records), assigned_patient(A, B)), (user_role(bob, nurse), role_perm(nurse, view_patient_records), assigned_patient(bob, patient2)), user_perm(bob, view_patient_records, patient2)).
step((user_perm(A, view_patient_records, B):+user_role(A, C), role_perm(C, view_patient_records), emergency(B)), (user_role(alice, doctor), role_perm(doctor, view_patient_records), emergency(patient3)), user_perm(alice, view_patient_records, patient3)).
step((user_perm(A, view_patient_records, B):+user_role(A, C), role_perm(C, view_patient_records), emergency(B)), (user_role(bob, nurse), role_perm(nurse, view_patient_records), emergency(patient3)), user_perm(bob, view_patient_records, patient3)).
step((user_perm(A, B, C):+user_role(A, D), role_perm(D, B), \+denied(A, B), assigned_patient(A, C)), (user_role(alice, doctor), role_perm(doctor, edit_patient_records), \+denied(alice, edit_patient_records), assigned_patient(alice, patient1)), user_perm(alice, edit_patient_records, patient1)).
step((user_perm(A, B, C):+user_role(A, D), role_perm(D, B), \+denied(A, B), assigned_patient(A, C)), (user_role(alice, doctor), role_perm(doctor, edit_patient_records), \+denied(alice, edit_patient_records), assigned_patient(alice, patient2)), user_perm(alice, edit_patient_records, patient2)).
step((user_perm(A, B, C):+user_role(A, D), role_perm(D, B), \+denied(A, B), assigned_patient(A, C)), (user_role(alice, doctor), role_perm(doctor, access_medications), \+denied(alice, access_medications), assigned_patient(alice, patient1)), user_perm(alice, access_medications, patient1)).
step((user_perm(A, B, C):+user_role(A, D), role_perm(D, B), \+denied(A, B), assigned_patient(A, C)), (user_role(alice, doctor), role_perm(doctor, access_medications), \+denied(alice, access_medications), assigned_patient(alice, patient2)), user_perm(alice, access_medications, patient2)).
step((user_perm(A, B, C):+user_role(A, D), role_perm(D, B), \+denied(A, B), assigned_patient(A, C)), (user_role(bob, nurse), role_perm(nurse, access_medications), \+denied(bob, access_medications), assigned_patient(bob, patient2)), user_perm(bob, access_medications, patient2)).
step((true:+user_perm(alice, _, patient1)), user_perm(alice, view_patient_records, patient1), true).
step((true:+user_perm(alice, _, patient1)), user_perm(alice, edit_patient_records, patient1), true).
step((true:+user_perm(alice, _, patient1)), user_perm(alice, access_medications, patient1), true).
step((true:+user_perm(bob, view_patient_records, patient2)), user_perm(bob, view_patient_records, patient2), true).
step((true:+ \+user_perm(alice, edit_patient_records, patient1, 7)), \+user_perm(alice, edit_patient_records, patient1, 7), true).
step((true:+user_perm(_, view_patient_records, patient3)), user_perm(alice, view_patient_records, patient3), true).
step((true:+user_perm(_, view_patient_records, patient3)), user_perm(bob, view_patient_records, patient3), true).
step((true:+ \+user_perm(dave, view_patient_records, patient1)), \+user_perm(dave, view_patient_records, patient1), true).
