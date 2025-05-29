:- op(1200, xfx, :+).

answer(compliant_subjects([john_doe, jane_smith])).

step((true:+compliant_subjects(_)), compliant_subjects([john_doe, jane_smith]), true).
