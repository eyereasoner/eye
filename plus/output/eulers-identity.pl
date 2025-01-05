:- op(1200, xfx, :+).

% answers
  answer(('<urn:example:complex:exponentiation>'([[e, 0], [0, pi]], [-1.0, 1.2246467991473532e-16]), '<urn:example:complex:sum>'([[-1.0, 1.2246467991473532e-16], [1, 0]], [0.0, 1.2246467991473532e-16]))).

% proof steps
  step((true:+'<urn:example:complex:exponentiation>'([[e, 0], [0, pi]], A), '<urn:example:complex:sum>'([A, [1, 0]], _)),
       ('<urn:example:complex:exponentiation>'([[e, 0], [0, pi]], [-1.0, 1.2246467991473532e-16]), '<urn:example:complex:sum>'([[-1.0, 1.2246467991473532e-16], [1, 0]], [0.0, 1.2246467991473532e-16])),
       true).
