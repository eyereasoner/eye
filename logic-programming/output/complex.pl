:- op(1200, xfx, :+).

% answers
  answer('<urn:example:complex:quotient>'([[1, 0], [0, 1]], [0, -1])).
  answer('<urn:example:complex:exponentiation>'([[-1, 0], [0.5, 0]],
                                                [6.123233995736766e-17, 1.0])).
  answer('<urn:example:complex:exponentiation>'([[e, 0], [0, pi]],
                                                [-1.0, 1.2246467991473532e-16])).
  answer('<urn:example:complex:log>'([[e, 0], [-1, 0]], [0.0, 3.141592653589793])).
  answer('<urn:example:complex:log>'([[0, 1], [0, 1]], [1.0, 0.0])).
  answer('<urn:example:complex:sin>'([1.570796326794897, 1.316957896924817],
                                     [ 2.0000000000000004,
                                       -6.631275506809351e-16
                                     ])).
  answer('<urn:example:complex:cos>'([0, -1.316957896924817],
                                     [2.0000000000000004, 0.0])).
  answer('<urn:example:complex:tan>'([1.338972522294493, 0.4023594781085251],
                                     [1.000000000000001, 1.9999999999999982])).
  answer('<urn:example:complex:asin>'([2, 0],
                                      [1.5707963267948966, 1.3169578969248166])).
  answer('<urn:example:complex:acos>'([2, 0], [0.0, -1.3169578969248166])).
  answer('<urn:example:complex:atan>'([1, 2],
                                      [1.3389725222944935, 0.40235947810852507])).

% proof steps
  step((true:+'<urn:example:complex:quotient>'([[1, 0], [0, 1]], _)),
       '<urn:example:complex:quotient>'([[1, 0], [0, 1]], [0, -1]),
       true).
  step((true:+'<urn:example:complex:exponentiation>'([[-1, 0], [0.5, 0]], _)),
       '<urn:example:complex:exponentiation>'([[-1, 0], [0.5, 0]],
                                              [6.123233995736766e-17, 1.0]),
       true).
  step((true:+'<urn:example:complex:exponentiation>'([[e, 0], [0, pi]], _)),
       '<urn:example:complex:exponentiation>'([[e, 0], [0, pi]],
                                              [-1.0, 1.2246467991473532e-16]),
       true).
  step((true:+'<urn:example:complex:log>'([[e, 0], [-1, 0]], _)),
       '<urn:example:complex:log>'([[e, 0], [-1, 0]], [0.0, 3.141592653589793]),
       true).
  step((true:+'<urn:example:complex:log>'([[0, 1], [0, 1]], _)),
       '<urn:example:complex:log>'([[0, 1], [0, 1]], [1.0, 0.0]),
       true).
  step((true:+'<urn:example:complex:sin>'([1.570796326794897, 1.316957896924817], _)),
       '<urn:example:complex:sin>'([1.570796326794897, 1.316957896924817],
                                   [2.0000000000000004, -6.631275506809351e-16]),
       true).
  step((true:+'<urn:example:complex:cos>'([0, -1.316957896924817], _)),
       '<urn:example:complex:cos>'([0, -1.316957896924817],
                                   [2.0000000000000004, 0.0]),
       true).
  step((true:+'<urn:example:complex:tan>'([1.338972522294493, 0.4023594781085251], _)),
       '<urn:example:complex:tan>'([1.338972522294493, 0.4023594781085251],
                                   [1.000000000000001, 1.9999999999999982]),
       true).
  step((true:+'<urn:example:complex:asin>'([2, 0], _)),
       '<urn:example:complex:asin>'([2, 0],
                                    [1.5707963267948966, 1.3169578969248166]),
       true).
  step((true:+'<urn:example:complex:acos>'([2, 0], _)),
       '<urn:example:complex:acos>'([2, 0], [0.0, -1.3169578969248166]),
       true).
  step((true:+'<urn:example:complex:atan>'([1, 2], _)),
       '<urn:example:complex:atan>'([1, 2],
                                    [1.3389725222944935, 0.40235947810852507]),
       true).
