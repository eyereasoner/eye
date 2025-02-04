:- op(1200, xfx, :+).

answer('<https://eyereasoner.github.io/ns#userPermission>'(alice, view_patient_records, patient1)).
answer('<https://eyereasoner.github.io/ns#userPermission>'(alice, edit_patient_records, patient1)).
answer('<https://eyereasoner.github.io/ns#userPermission>'(alice, access_medications, patient1)).
answer('<https://eyereasoner.github.io/ns#userPermission>'(bob, view_patient_records, patient2)).
answer(\+'<https://eyereasoner.github.io/ns#userPermission>'([alice, 7], edit_patient_records, patient1)).
answer('<https://eyereasoner.github.io/ns#userPermission>'(alice, view_patient_records, patient3)).
answer('<https://eyereasoner.github.io/ns#userPermission>'(bob, view_patient_records, patient3)).
answer(\+'<https://eyereasoner.github.io/ns#userPermission>'(dave, view_patient_records, patient1)).

% proof steps
step((user_permission(A, view_patient_records, B):+user_role(A, C), role_permission(C, view_patient_records), assigned_patient(A, B)), (user_role(alice, doctor), role_permission(doctor, view_patient_records), assigned_patient(alice, patient1)), user_permission(alice, view_patient_records, patient1)).
step((user_permission(A, view_patient_records, B):+user_role(A, C), role_permission(C, view_patient_records), assigned_patient(A, B)), (user_role(alice, doctor), role_permission(doctor, view_patient_records), assigned_patient(alice, patient2)), user_permission(alice, view_patient_records, patient2)).
step((user_permission(A, view_patient_records, B):+user_role(A, C), role_permission(C, view_patient_records), assigned_patient(A, B)), (user_role(bob, nurse), role_permission(nurse, view_patient_records), assigned_patient(bob, patient2)), user_permission(bob, view_patient_records, patient2)).
step((user_permission(A, view_patient_records, B):+user_role(A, C), role_permission(C, view_patient_records), emergency(B)), (user_role(alice, doctor), role_permission(doctor, view_patient_records), emergency(patient3)), user_permission(alice, view_patient_records, patient3)).
step((user_permission(A, view_patient_records, B):+user_role(A, C), role_permission(C, view_patient_records), emergency(B)), (user_role(bob, nurse), role_permission(nurse, view_patient_records), emergency(patient3)), user_permission(bob, view_patient_records, patient3)).
step((user_permission(A, B, C):+user_role(A, D), role_permission(D, B), \+denied(A, B), assigned_patient(A, C)), (user_role(alice, doctor), role_permission(doctor, edit_patient_records), \+denied(alice, edit_patient_records), assigned_patient(alice, patient1)), user_permission(alice, edit_patient_records, patient1)).
step((user_permission(A, B, C):+user_role(A, D), role_permission(D, B), \+denied(A, B), assigned_patient(A, C)), (user_role(alice, doctor), role_permission(doctor, edit_patient_records), \+denied(alice, edit_patient_records), assigned_patient(alice, patient2)), user_permission(alice, edit_patient_records, patient2)).
step((user_permission(A, B, C):+user_role(A, D), role_permission(D, B), \+denied(A, B), assigned_patient(A, C)), (user_role(alice, doctor), role_permission(doctor, access_medications), \+denied(alice, access_medications), assigned_patient(alice, patient1)), user_permission(alice, access_medications, patient1)).
step((user_permission(A, B, C):+user_role(A, D), role_permission(D, B), \+denied(A, B), assigned_patient(A, C)), (user_role(alice, doctor), role_permission(doctor, access_medications), \+denied(alice, access_medications), assigned_patient(alice, patient2)), user_permission(alice, access_medications, patient2)).
step((user_permission(A, B, C):+user_role(A, D), role_permission(D, B), \+denied(A, B), assigned_patient(A, C)), (user_role(bob, nurse), role_permission(nurse, access_medications), \+denied(bob, access_medications), assigned_patient(bob, patient2)), user_permission(bob, access_medications, patient2)).
step((true:+'<https://eyereasoner.github.io/ns#userPermission>'(alice, _, patient1)), '<https://eyereasoner.github.io/ns#userPermission>'(alice, view_patient_records, patient1), true).
step((true:+'<https://eyereasoner.github.io/ns#userPermission>'(alice, _, patient1)), '<https://eyereasoner.github.io/ns#userPermission>'(alice, edit_patient_records, patient1), true).
step((true:+'<https://eyereasoner.github.io/ns#userPermission>'(alice, _, patient1)), '<https://eyereasoner.github.io/ns#userPermission>'(alice, access_medications, patient1), true).
step((true:+'<https://eyereasoner.github.io/ns#userPermission>'(bob, view_patient_records, patient2)), '<https://eyereasoner.github.io/ns#userPermission>'(bob, view_patient_records, patient2), true).
step((true:+ \+'<https://eyereasoner.github.io/ns#userPermission>'([alice, 7], edit_patient_records, patient1)), \+'<https://eyereasoner.github.io/ns#userPermission>'([alice, 7], edit_patient_records, patient1), true).
step((true:+'<https://eyereasoner.github.io/ns#userPermission>'(_, view_patient_records, patient3)), '<https://eyereasoner.github.io/ns#userPermission>'(alice, view_patient_records, patient3), true).
step((true:+'<https://eyereasoner.github.io/ns#userPermission>'(_, view_patient_records, patient3)), '<https://eyereasoner.github.io/ns#userPermission>'(bob, view_patient_records, patient3), true).
step((true:+ \+'<https://eyereasoner.github.io/ns#userPermission>'(dave, view_patient_records, patient1)), \+'<https://eyereasoner.github.io/ns#userPermission>'(dave, view_patient_records, patient1), true).
