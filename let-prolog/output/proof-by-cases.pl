:- op(1200, xfx, :+).

answer(ascribed(water, observable)).

step((allPossibleCases([A], [ascribed(A, solid), ascribed(A, liquid), ascribed(A, gas)]):+inorganic_compound(A)), inorganic_compound(water), allPossibleCases([water], [ascribed(water, solid), ascribed(water, liquid), ascribed(water, gas)])).
step((true:+ascribed(_, _)), ascribed(water, observable), true).
