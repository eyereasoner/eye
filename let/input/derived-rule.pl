% example of rule derivation

% facts
type('Minka', 'Cat').
type('Charly', 'Dog').

% rule
(
    ascribed(test, true) :+
        type(_, 'Dog')
) :+
    type(_, 'Cat').

% query
true :+ ascribed(test, true).
