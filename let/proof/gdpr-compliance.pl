compliant_subjects([john_doe,jane_smith]).

step(rule(compliant_subjects(A), answer(compliant_subjects(A))), compliant_subjects([john_doe, jane_smith]), answer(compliant_subjects([john_doe, jane_smith]))).
