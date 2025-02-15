:- op(1200, xfx, :+).

answer('<https://eyereasoner.github.io/ns#fallacy>'(circular_reasoning, [supports(bible_true, god_wrote_it), supports(god_wrote_it, bible_true)])).
answer('<https://eyereasoner.github.io/ns#fallacy>'(affirming_the_consequent, [implies(p, q), q, conclusion(p)])).
answer('<https://eyereasoner.github.io/ns#fallacy>'(false_dilemma, [false_dilemma(either_or, with_us, against_us)])).
answer('<https://eyereasoner.github.io/ns#fallacy>'(slippery_slope, [slippery_slope(allow_calculators, forget_basic_math), slippery_slope(forget_basic_math, society_collapses)])).
answer('<https://eyereasoner.github.io/ns#fallacy>'(ad_hominem, [ad_hominem(johns_argument, personal_attack(john, dropout))])).
answer('<https://eyereasoner.github.io/ns#fallacy>'(straw_man, [straw_man(real_argument(better_gun_regulation), distorted_argument(ban_all_guns))])).
answer('<https://eyereasoner.github.io/ns#fallacy>'(post_hoc, [post_hoc_event(wore_lucky_socks, team_won)])).
answer('<https://eyereasoner.github.io/ns#fallacy>'(bandwagon, [bandwagon_argument(many_people_believe_astrology, astrology_is_true)])).
answer('<https://eyereasoner.github.io/ns#fallacy>'(appeal_to_authority, [appeal_to_authority(famous_actor, best_diet)])).
answer('<https://eyereasoner.github.io/ns#fallacy>'(red_herring, [red_herring(argument(climate_change), distraction(economy))])).

% proof steps
step((true:+'<https://eyereasoner.github.io/ns#fallacy>'(circular_reasoning, [supports(bible_true, god_wrote_it), supports(god_wrote_it, bible_true)])), '<https://eyereasoner.github.io/ns#fallacy>'(circular_reasoning, [supports(bible_true, god_wrote_it), supports(god_wrote_it, bible_true)]), true).
step((true:+'<https://eyereasoner.github.io/ns#fallacy>'(affirming_the_consequent, [implies(p, q), q, conclusion(p)])), '<https://eyereasoner.github.io/ns#fallacy>'(affirming_the_consequent, [implies(p, q), q, conclusion(p)]), true).
step((true:+'<https://eyereasoner.github.io/ns#fallacy>'(false_dilemma, [false_dilemma(either_or, with_us, against_us)])), '<https://eyereasoner.github.io/ns#fallacy>'(false_dilemma, [false_dilemma(either_or, with_us, against_us)]), true).
step((true:+'<https://eyereasoner.github.io/ns#fallacy>'(slippery_slope, [slippery_slope(allow_calculators, forget_basic_math), slippery_slope(forget_basic_math, society_collapses)])), '<https://eyereasoner.github.io/ns#fallacy>'(slippery_slope, [slippery_slope(allow_calculators, forget_basic_math), slippery_slope(forget_basic_math, society_collapses)]), true).
step((true:+'<https://eyereasoner.github.io/ns#fallacy>'(ad_hominem, [ad_hominem(johns_argument, personal_attack(john, dropout))])), '<https://eyereasoner.github.io/ns#fallacy>'(ad_hominem, [ad_hominem(johns_argument, personal_attack(john, dropout))]), true).
step((true:+'<https://eyereasoner.github.io/ns#fallacy>'(straw_man, [straw_man(real_argument(better_gun_regulation), distorted_argument(ban_all_guns))])), '<https://eyereasoner.github.io/ns#fallacy>'(straw_man, [straw_man(real_argument(better_gun_regulation), distorted_argument(ban_all_guns))]), true).
step((true:+'<https://eyereasoner.github.io/ns#fallacy>'(post_hoc, [post_hoc_event(wore_lucky_socks, team_won)])), '<https://eyereasoner.github.io/ns#fallacy>'(post_hoc, [post_hoc_event(wore_lucky_socks, team_won)]), true).
step((true:+'<https://eyereasoner.github.io/ns#fallacy>'(bandwagon, [bandwagon_argument(many_people_believe_astrology, astrology_is_true)])), '<https://eyereasoner.github.io/ns#fallacy>'(bandwagon, [bandwagon_argument(many_people_believe_astrology, astrology_is_true)]), true).
step((true:+'<https://eyereasoner.github.io/ns#fallacy>'(appeal_to_authority, [appeal_to_authority(famous_actor, best_diet)])), '<https://eyereasoner.github.io/ns#fallacy>'(appeal_to_authority, [appeal_to_authority(famous_actor, best_diet)]), true).
step((true:+'<https://eyereasoner.github.io/ns#fallacy>'(red_herring, [red_herring(argument(climate_change), distraction(economy))])), '<https://eyereasoner.github.io/ns#fallacy>'(red_herring, [red_herring(argument(climate_change), distraction(economy))]), true).
