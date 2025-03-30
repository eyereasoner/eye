% fallacy detection

:- op(1200, xfx, :+).

% context
'urn:example:fallacy'(Fallacy, Premises) :-
    fallacy(Fallacy, Premises).

% begging the question (circular reasoning)
fallacy(circular_reasoning, Premises) :-
    member(supports(A, B), Premises),
    member(supports(B, A), Premises).

% affirming the consequent
fallacy(affirming_the_consequent, Premises) :-
    member(implies(P, Q), Premises),
    member(Q, Premises),
    member(conclusion(P), Premises).

% false dilemma
fallacy(false_dilemma, Premises) :-
    member(false_dilemma(_, _, _), Premises),
    \+ member(extra_option(_), Premises).

% slippery slope
fallacy(slippery_slope, Premises) :-
    member(slippery_slope(_, B), Premises),
    member(slippery_slope(B, _), Premises).

% ad hominem
fallacy(ad_hominem, Premises) :-
    member(ad_hominem(_, personal_attack(_, _)), Premises).

% straw man
fallacy(straw_man, Premises) :-
    member(straw_man(real_argument(_), distorted_argument(_)), Premises).

% post hoc (false cause)
fallacy(post_hoc, Premises) :-
    member(post_hoc_event(_, _), Premises).

% bandwagon (argumentum ad populum)
fallacy(bandwagon, Premises) :-
    member(bandwagon_argument(_, _), Premises).

% appeal to authority (argumentum ad verecundiam)
fallacy(appeal_to_authority, Premises) :-
    member(appeal_to_authority(_, _), Premises).

% red herring
fallacy(red_herring, Premises) :-
    member(red_herring(argument(_), distraction(_)), Premises).

% query
true :+ 'urn:example:fallacy'(
    circular_reasoning,
    [   supports(bible_true, god_wrote_it),
        supports(god_wrote_it, bible_true)
    ]
).

true :+ 'urn:example:fallacy'(
    affirming_the_consequent,
    [   implies(p, q),
        q,
        conclusion(p)
    ]
).

true :+ 'urn:example:fallacy'(
    false_dilemma,
    [   false_dilemma(either_or, with_us, against_us)
    ]
).

true :+ 'urn:example:fallacy'(
    slippery_slope,
    [   slippery_slope(allow_calculators, forget_basic_math),
        slippery_slope(forget_basic_math, society_collapses)
    ]
).

true :+ 'urn:example:fallacy'(
    ad_hominem,
    [   ad_hominem(johns_argument, personal_attack(john, dropout))
    ]
).

true :+ 'urn:example:fallacy'(
    straw_man,
    [   straw_man(real_argument(better_gun_regulation), distorted_argument(ban_all_guns))
    ]
).

true :+ 'urn:example:fallacy'(
    post_hoc,
    [   post_hoc_event(wore_lucky_socks, team_won)
    ]
).

true :+ 'urn:example:fallacy'(
    bandwagon,
    [   bandwagon_argument(many_people_believe_astrology, astrology_is_true)
    ]
).

true :+ 'urn:example:fallacy'(
    appeal_to_authority,
    [   appeal_to_authority(famous_actor, best_diet)
    ]
).

true :+ 'urn:example:fallacy'(
    red_herring,
    [   red_herring(argument(climate_change), distraction(economy))
    ]
).
