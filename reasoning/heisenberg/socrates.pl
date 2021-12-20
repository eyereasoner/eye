% Socrates is a mortal

man(socrates).

human(X) :- man(X).
mortal(X) :- human(X).

% query implies goal
mortal(_IND) => goal.
