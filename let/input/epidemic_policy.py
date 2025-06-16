"""
epidemic_policy.py – choose the best combination of
vaccination and indoor-mask mandates when a local outbreak
is possible.

Illustrates DTProbLog from Python:

    • multiple decision facts         (?::X.)
    • probabilistic dependencies      (p::h :- body.)
    • utilities / disutilities        utility(term, Value).
    • automatic MEU computation       dtproblog()
"""

# Requires:  pip install problog

from problog.tasks.dtproblog import dtproblog
from problog.program import PrologString


def optimise_policy():
    model = r"""
    % ===================
    %  Uncertain world
    % ===================
    0.10::outbreak.              % 5 % chance of a local outbreak this season

    % ===================
    %  Decisions
    % ===================
    ?::vaccinate.                % mass-vaccination programme?
    ?::mask.                     % indoor mask mandate?

    % ===================
    %  Infection dynamics
    %  (four annotated rules so DTProbLog can combine them)
    % ===================
    % NB: Probabilities are *conditional* on Outbreak.
    0.05::infect :- outbreak,  vaccinate,  mask.
    0.05::infect :- outbreak,  vaccinate, \+mask.
    0.30::infect :- outbreak, \+vaccinate, mask.
    0.60::infect :- outbreak, \+vaccinate, \+mask.

    % If there is **no** outbreak, nobody gets infected.
    infect :- false, \+outbreak.

    % ===================
    %  Utilities
    % ===================
    utility(vaccinate,     -2).     % programme cost / inconvenience
    utility(mask,          -1).     % mandate cost
    utility(infect,      -200).     % heavy loss if infection occurs
    utility(healthy,        0).     % neutral (implicit baseline)

    % 'healthy' holds when no infection occurs.
    healthy :- \+infect.
    """

    # Compile & solve
    program = PrologString(model)
    decisions, max_util, _stats = dtproblog(program)

    return decisions, max_util


if __name__ == "__main__":
    policy, expected_utility = optimise_policy()

    print("Optimal decisions (MEU):")
    for dec, chosen in policy.items():
        print(f"  • {dec}: {chosen}")

    print(f"\nMaximum expected utility: {expected_utility:.2f}")

