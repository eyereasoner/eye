:- op(1200, xfx, :+).

answer('urn:example:fallacy'(circular_reasoning,[supports(bible_true,god_wrote_it),supports(god_wrote_it,bible_true)])).
answer('urn:example:fallacy'(affirming_the_consequent,[implies(p,q),q,conclusion(p)])).
answer('urn:example:fallacy'(false_dilemma,[false_dilemma(either_or,with_us,against_us)])).
answer('urn:example:fallacy'(slippery_slope,[slippery_slope(allow_calculators,forget_basic_math),slippery_slope(forget_basic_math,society_collapses)])).
answer('urn:example:fallacy'(ad_hominem,[ad_hominem(johns_argument,personal_attack(john,dropout))])).
answer('urn:example:fallacy'(straw_man,[straw_man(real_argument(better_gun_regulation),distorted_argument(ban_all_guns))])).
answer('urn:example:fallacy'(post_hoc,[post_hoc_event(wore_lucky_socks,team_won)])).
answer('urn:example:fallacy'(bandwagon,[bandwagon_argument(many_people_believe_astrology,astrology_is_true)])).
answer('urn:example:fallacy'(appeal_to_authority,[appeal_to_authority(famous_actor,best_diet)])).
answer('urn:example:fallacy'(red_herring,[red_herring(argument(climate_change),distraction(economy))])).

step((true:+'urn:example:fallacy'(circular_reasoning,[supports(bible_true,god_wrote_it),supports(god_wrote_it,bible_true)])),'urn:example:fallacy'(circular_reasoning,[supports(bible_true,god_wrote_it),supports(god_wrote_it,bible_true)]),true).
step((true:+'urn:example:fallacy'(affirming_the_consequent,[implies(p,q),q,conclusion(p)])),'urn:example:fallacy'(affirming_the_consequent,[implies(p,q),q,conclusion(p)]),true).
step((true:+'urn:example:fallacy'(false_dilemma,[false_dilemma(either_or,with_us,against_us)])),'urn:example:fallacy'(false_dilemma,[false_dilemma(either_or,with_us,against_us)]),true).
step((true:+'urn:example:fallacy'(slippery_slope,[slippery_slope(allow_calculators,forget_basic_math),slippery_slope(forget_basic_math,society_collapses)])),'urn:example:fallacy'(slippery_slope,[slippery_slope(allow_calculators,forget_basic_math),slippery_slope(forget_basic_math,society_collapses)]),true).
step((true:+'urn:example:fallacy'(ad_hominem,[ad_hominem(johns_argument,personal_attack(john,dropout))])),'urn:example:fallacy'(ad_hominem,[ad_hominem(johns_argument,personal_attack(john,dropout))]),true).
step((true:+'urn:example:fallacy'(straw_man,[straw_man(real_argument(better_gun_regulation),distorted_argument(ban_all_guns))])),'urn:example:fallacy'(straw_man,[straw_man(real_argument(better_gun_regulation),distorted_argument(ban_all_guns))]),true).
step((true:+'urn:example:fallacy'(post_hoc,[post_hoc_event(wore_lucky_socks,team_won)])),'urn:example:fallacy'(post_hoc,[post_hoc_event(wore_lucky_socks,team_won)]),true).
step((true:+'urn:example:fallacy'(bandwagon,[bandwagon_argument(many_people_believe_astrology,astrology_is_true)])),'urn:example:fallacy'(bandwagon,[bandwagon_argument(many_people_believe_astrology,astrology_is_true)]),true).
step((true:+'urn:example:fallacy'(appeal_to_authority,[appeal_to_authority(famous_actor,best_diet)])),'urn:example:fallacy'(appeal_to_authority,[appeal_to_authority(famous_actor,best_diet)]),true).
step((true:+'urn:example:fallacy'(red_herring,[red_herring(argument(climate_change),distraction(economy))])),'urn:example:fallacy'(red_herring,[red_herring(argument(climate_change),distraction(economy))]),true).
