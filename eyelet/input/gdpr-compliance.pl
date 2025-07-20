% A simple Prolog-based rule system to check GDPR compliance for data subjects
% Original code from https://chatgpt.com/share/6810c429-f588-800b-92f6-772b3bd116be

:- op(1200, xfx, :+).

% Sample facts about data subjects and processing
% Define data subjects
data_subject(john_doe).
data_subject(jane_smith).

% Define types of personal data collected for each subject
personal_data(john_doe, email).
personal_data(john_doe, name).
personal_data(jane_smith, address).

% Define purposes for which data is processed
processing_purpose(john_doe, marketing).
processing_purpose(john_doe, analytics).
processing_purpose(jane_smith, support).

% Define lawful bases for processing
% Possible bases: consent, contract, legal_obligation, vital_interest, public_task, legitimate_interest
lawful_basis(john_doe, marketing, consent).
lawful_basis(john_doe, analytics, legitimate_interest).
lawful_basis(jane_smith, support, contract).

% Define retention periods in days for each purpose
retention_period(john_doe, marketing, 180).
retention_period(john_doe, analytics, 730).
retention_period(jane_smith, support, 365).

% Define that a processor has signed a Data Processing Agreement (DPA)
processor(dpo_corp).
dpa_signed(dpo_corp).
processes(dpo_corp, john_doe).
processes(dpo_corp, jane_smith).

% Define data minimization: only minimal data collected for each purpose
% For simplicity, list subjects for which minimization is satisfied
data_minimal(john_doe, marketing).
data_minimal(jane_smith, support).

% Define data subject rights exercised
% e.g., right to access, rectify, erase
right_exercised(john_doe, access).
right_exercised(john_doe, rectify).
right_exercised(jane_smith, access).
right_exercised(jane_smith, rectify).

% GDPR rules

% 1. Lawful basis must exist for each purpose
has_lawful_basis(Subject) :-
    processing_purpose(Subject, Purpose),
    lawful_basis(Subject, Purpose, _).

% 2. Retention period must not exceed allowed maximum (e.g., 2 years = 730 days)
max_retention_days(730).
retention_ok(Subject) :-
    processing_purpose(Subject, Purpose),
    retention_period(Subject, Purpose, Days),
    max_retention_days(Max),
    Days =< Max.

% 3. Data minimization must be satisfied for each purpose
minimization_ok(Subject) :-
    processing_purpose(Subject, Purpose),
    data_minimal(Subject, Purpose).

% 4. Processor must have signed a DPA
processor_ok :-
    processor(Proc),
    dpa_signed(Proc).

% 5. Data subject rights enforcement (access, rectify, erase)
rights_ok(Subject) :-
    right_exercised(Subject, access),
    right_exercised(Subject, rectify).

% Final compliance check for a given subject
gdpr_compliant(Subject) :-
    data_subject(Subject),
    has_lawful_basis(Subject),
    retention_ok(Subject),
    minimization_ok(Subject),
    processor_ok,
    rights_ok(Subject).

% List all compliant data subjects
compliant_subjects(Subjects) :-
    findall(Subject, gdpr_compliant(Subject), List),
    list_to_set(List, Subjects).

% query
true :+ compliant_subjects(_).
