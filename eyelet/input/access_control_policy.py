"""
access_control_policy.py
────────────────────────
Decision-Theoretic ProbLog (DTProbLog) demo in *access control*.

Task
----
When Alice logs in, decide whether to
  • **grant immediately**  (fast, no user friction)  or
  • **require MFA**        (step-up authentication)

Uncertainties
-------------
• user_is_attacker           – 15 % chance (stolen creds, session hijack …)
• MFA may be bypassed by an attacker (20 % success)
• Legitimate users pass MFA 95 % of the time

Utilities (illustrative, euros)
-------------------------------
+30   successful legitimate session
-100  successful attacker session   (breach)
-10   lock-out of a legitimate user (MFA fails)
-5    MFA friction / operational cost   (charged whenever we ask for MFA)
"""

from problog.program import PrologString
from problog.tasks.dtproblog import dtproblog
from problog.logic import Term


# ───────────────────────────────────────────────────────────────
# 1 ▸  ProbLog model
# ───────────────────────────────────────────────────────────────
MODEL = r"""
% ============== Decision ======================================
?::grant_direct.                 % True ⇒ allow immediately
                                  % False ⇒ require MFA

% ============== Prior attacker probability ====================
0.15::user_is_attacker.

% ============== MFA success probabilities =====================
% Legitimate users succeed 95 %
0.95::mfa_success :- \+grant_direct, \+user_is_attacker.

% Attackers still have a 20 % chance of bypassing MFA
0.20::mfa_success :- \+grant_direct,     user_is_attacker.

% ============== Outcomes ======================================
% ── Breach cases ───────────────────────────────────────────────
breach :-  grant_direct, user_is_attacker.
breach :- \+grant_direct, user_is_attacker, mfa_success.

% ── Legitimate success cases ──────────────────────────────────
legit_session :-  grant_direct, \+user_is_attacker.
legit_session :- \+grant_direct, \+user_is_attacker, mfa_success.

% ── Lock-out (false reject) ───────────────────────────────────
lockout :- \+grant_direct, \+user_is_attacker, \+mfa_success.

% ============== Utilities =====================================
utility(breach,        -100).
utility(legit_session,   30).
utility(lockout,        -10).

% Asking for MFA costs −5 regardless of the outcome
utility(\+grant_direct,  -5).
"""

# ───────────────────────────────────────────────────────────────
# 2 ▸  Solve with DTProbLog
# ───────────────────────────────────────────────────────────────
decisions, max_eu, _ = dtproblog(PrologString(MODEL))


def truth(atom_name: str) -> bool:
    """Return True/False for the positive literal `atom_name`,
       even if DTProbLog supplied its negation instead."""
    pos = decisions.get(Term(atom_name))
    if pos is not None:
        return bool(pos)
    neg = decisions.get(Term(r'\+' + atom_name))
    return not bool(neg) if neg is not None else False


grant_immediately = truth('grant_direct')
require_mfa       = not grant_immediately    # easier to read

# ───────────────────────────────────────────────────────────────
# 3 ▸  Present results
# ───────────────────────────────────────────────────────────────
print("Access-control policy selected by DTProbLog")
print("-------------------------------------------")
print(f"Grant immediately? : {grant_immediately}")
print(f"Require MFA?       : {require_mfa}")
print(f"Expected utility   : {max_eu:.2f} €\n")

print("Raw DTProbLog decision literals:")
for lit, val in decisions.items():
    print(f"  {lit}: {val}")
