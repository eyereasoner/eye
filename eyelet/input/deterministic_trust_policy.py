"""
deterministic_trust-policy.py
─────────────────────────────
Deterministic (probability-free) trust policy demo in ProbLog.

Highlights
----------
• Transitive delegation  (Admin ➜ DeptHead ➜ Manager ➜ User)
• Revocation certificates
• Role & clearance checks
• Context constraint (work_hours)
• Optional emergency “break-glass” override
"""

from problog.program import PrologString
from problog import get_evaluatable


POLICY = r"""
% ===============================================================
%  Delegation certificates (deterministic)
% ===============================================================
delegates(admin, dept_head).
delegates(dept_head, mgr_alice).
delegates(mgr_alice,  alice).
delegates(dept_head,  bob).          % Bob bypasses mgr_alice
delegates(admin,     vendor_mgr).
delegates(vendor_mgr, vendor_eve).

% ===============================================================
%  Revocations
% ===============================================================
revoked(bob).                        % Bob's cert is revoked

% ===============================================================
%  Attributes
% ===============================================================
role(alice, employee).
role(bob,   employee).
role(eve,   contractor).
role(mgr_alice, employee).
role(dept_head, employee).

clearance(alice,    secret).
clearance(bob,      secret).
clearance(mgr_alice,secret).
clearance(dept_head,secret).
clearance(eve,      confidential).

% ===============================================================
%  Context predicate (true when request happens in work hours)
% ===============================================================
work_hours.                          % stub: assume it is office time

% ===============================================================
%  Optional break-glass override
%  (off by default – define fact below to activate)
% ===============================================================
break_glass :- fail.                 % always false unless overridden
% To activate the emergency override, just uncomment the next line:
% break_glass.

% ===============================================================
%  Derived trust (transitive, revocation-aware)
% ===============================================================
trusted(X) :- delegates(admin, X), \+ revoked(X).
trusted(X) :- delegates(Y, X), trusted(Y), \+ revoked(X).

% ===============================================================
%  Authorisation rule
% ===============================================================
allow(read_secret_data, U) :-
        (   trusted(U),
            role(U, employee),
            clearance(U, secret),
            work_hours
        )
        ;
        break_glass.

% ===============================================================
%  Queries
% ===============================================================
query(allow(read_secret_data, alice)).
query(allow(read_secret_data, bob)).
query(allow(read_secret_data, eve)).
query(allow(read_secret_data, mgr_alice)).
query(allow(read_secret_data, dept_head)).
"""


def main():
    model = PrologString(POLICY)
    answers = (
        get_evaluatable()
        .create_from(model)
        .evaluate()
    )

    print("Authorisation results (1 = allowed, 0 = denied)")
    for q, val in answers.items():
        print(f"{q}: {int(val)}")


if __name__ == "__main__":
    main()
