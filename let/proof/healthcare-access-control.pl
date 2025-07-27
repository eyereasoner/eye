\+userPermission([alice,7],edit_patient_records,patient1).
\+userPermission(dave,view_patient_records,patient1).
userPermission(alice,view_patient_records,patient1).
userPermission(alice,edit_patient_records,patient1).
userPermission(alice,access_medications,patient1).
userPermission(bob,view_patient_records,patient2).
userPermission(alice,view_patient_records,patient3).
userPermission(bob,view_patient_records,patient3).

step(rule(\+userPermission([alice, 7], edit_patient_records, patient1), answer(\+userPermission([alice, 7], edit_patient_records, patient1))), \+userPermission([alice, 7], edit_patient_records, patient1), answer(\+userPermission([alice, 7], edit_patient_records, patient1))).
step(rule(\+userPermission(dave, view_patient_records, patient1), answer(\+userPermission(dave, view_patient_records, patient1))), \+userPermission(dave, view_patient_records, patient1), answer(\+userPermission(dave, view_patient_records, patient1))).
step(rule((user_role(A, B), role_permission(B, view_patient_records), assigned_patient(A, C)), user_permission(A, view_patient_records, C)), (user_role(alice, doctor), role_permission(doctor, view_patient_records), assigned_patient(alice, patient1)), user_permission(alice, view_patient_records, patient1)).
step(rule((user_role(A, B), role_permission(B, view_patient_records), assigned_patient(A, C)), user_permission(A, view_patient_records, C)), (user_role(alice, doctor), role_permission(doctor, view_patient_records), assigned_patient(alice, patient2)), user_permission(alice, view_patient_records, patient2)).
step(rule((user_role(A, B), role_permission(B, view_patient_records), assigned_patient(A, C)), user_permission(A, view_patient_records, C)), (user_role(bob, nurse), role_permission(nurse, view_patient_records), assigned_patient(bob, patient2)), user_permission(bob, view_patient_records, patient2)).
step(rule((user_role(A, B), role_permission(B, view_patient_records), emergency(C)), user_permission(A, view_patient_records, C)), (user_role(alice, doctor), role_permission(doctor, view_patient_records), emergency(patient3)), user_permission(alice, view_patient_records, patient3)).
step(rule((user_role(A, B), role_permission(B, view_patient_records), emergency(C)), user_permission(A, view_patient_records, C)), (user_role(bob, nurse), role_permission(nurse, view_patient_records), emergency(patient3)), user_permission(bob, view_patient_records, patient3)).
step(rule((user_role(A, B), role_permission(B, C), \+denied(A, C), assigned_patient(A, D)), user_permission(A, C, D)), (user_role(alice, doctor), role_permission(doctor, edit_patient_records), \+denied(alice, edit_patient_records), assigned_patient(alice, patient1)), user_permission(alice, edit_patient_records, patient1)).
step(rule((user_role(A, B), role_permission(B, C), \+denied(A, C), assigned_patient(A, D)), user_permission(A, C, D)), (user_role(alice, doctor), role_permission(doctor, edit_patient_records), \+denied(alice, edit_patient_records), assigned_patient(alice, patient2)), user_permission(alice, edit_patient_records, patient2)).
step(rule((user_role(A, B), role_permission(B, C), \+denied(A, C), assigned_patient(A, D)), user_permission(A, C, D)), (user_role(alice, doctor), role_permission(doctor, access_medications), \+denied(alice, access_medications), assigned_patient(alice, patient1)), user_permission(alice, access_medications, patient1)).
step(rule((user_role(A, B), role_permission(B, C), \+denied(A, C), assigned_patient(A, D)), user_permission(A, C, D)), (user_role(alice, doctor), role_permission(doctor, access_medications), \+denied(alice, access_medications), assigned_patient(alice, patient2)), user_permission(alice, access_medications, patient2)).
step(rule((user_role(A, B), role_permission(B, C), \+denied(A, C), assigned_patient(A, D)), user_permission(A, C, D)), (user_role(bob, nurse), role_permission(nurse, access_medications), \+denied(bob, access_medications), assigned_patient(bob, patient2)), user_permission(bob, access_medications, patient2)).
step(rule(userPermission(alice, A, patient1), answer(userPermission(alice, A, patient1))), userPermission(alice, view_patient_records, patient1), answer(userPermission(alice, view_patient_records, patient1))).
step(rule(userPermission(alice, A, patient1), answer(userPermission(alice, A, patient1))), userPermission(alice, edit_patient_records, patient1), answer(userPermission(alice, edit_patient_records, patient1))).
step(rule(userPermission(alice, A, patient1), answer(userPermission(alice, A, patient1))), userPermission(alice, access_medications, patient1), answer(userPermission(alice, access_medications, patient1))).
step(rule(userPermission(bob, view_patient_records, patient2), answer(userPermission(bob, view_patient_records, patient2))), userPermission(bob, view_patient_records, patient2), answer(userPermission(bob, view_patient_records, patient2))).
step(rule(userPermission(A, view_patient_records, patient3), answer(userPermission(A, view_patient_records, patient3))), userPermission(alice, view_patient_records, patient3), answer(userPermission(alice, view_patient_records, patient3))).
step(rule(userPermission(A, view_patient_records, patient3), answer(userPermission(A, view_patient_records, patient3))), userPermission(bob, view_patient_records, patient3), answer(userPermission(bob, view_patient_records, patient3))).
