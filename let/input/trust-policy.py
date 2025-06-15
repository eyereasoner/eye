"""
trust-policy.py
────────────────
Decision-Theoretic ProbLog example (policies & trust).

Task: decide whether to run an expensive verification on Bob’s token.

Decisions
---------
?::verify.          % True → run extra verification (costly but safe)
                    % False → trust the token as is

Trust model
-----------
Admin holds probabilistic trust edges to Org1 and Bob.
Org1, in turn, may trust Bob.  Trust edges are uncertain, so Bob's
token might be genuine or malicious with some probability.

Utilities
---------
+10  grant access to a *good* token
-50  grant access to a *bad*  token      (compromise)
-5   cost of running verification
"""

from problog.program import PrologString
from problog.tasks.dtproblog import dtproblog
from problog.logic import Term   # Term class for robust look-ups

MODEL = r"""
% ─── Decision ─────────────────────────────────────────────────────
?::verify.                        % extra verification step?

% ─── Probabilistic trust network ──────────────────────────────────
0.8::trust(admin, org1).
0.6::trust(org1,  bob).
0.2::trust(admin, bob).

trusted(X) :- trust(admin, X).
trusted(X) :- trust(admin, Y), trust(Y, X).

good_token :- trusted(bob).
bad_token  :- \+good_token.

% ─── Outcomes depending on decision ───────────────────────────────
grant_access :- good_token.
compromise   :- bad_token, \+verify.   % only if we skip verification

% ─── Utilities (illustrative) ─────────────────────────────────────
utility(verify,        -5).    % verification cost
utility(grant_access,  10).    % benefit of legitimate access
utility(compromise,   -50).    % damage if bad token slips through
"""

# ──────────────────────────────────────────────────────────────────
# 1 ▸  Run optimisation
# ──────────────────────────────────────────────────────────────────
decisions, max_utility, _ = dtproblog(PrologString(MODEL))

# ──────────────────────────────────────────────────────────────────
# 2 ▸  Helper: truth value of ANY literal (positive or negated)
# ──────────────────────────────────────────────────────────────────
def truth(atom_name: str) -> bool:
    """Return True if the positive literal is true (1), regardless
       of whether DTProbLog supplied that literal directly or its negation."""
    pos = decisions.get(Term(atom_name))
    if pos is not None:
        return bool(pos)
    neg = decisions.get(Term(r'\+' + atom_name))
    return not bool(neg) if neg is not None else False


should_verify = truth('verify')

# ──────────────────────────────────────────────────────────────────
# 3 ▸  Present results
# ──────────────────────────────────────────────────────────────────
print("Policy selected by DTProbLog")
print("----------------------------")
print(f"Verify first?    : {should_verify}")
print(f"Expected utility : {max_utility:.2f}\n")

print("Raw DTProbLog literals:")
for lit, val in decisions.items():
    print(f"  {lit}: {val}")
