:- op(1200, xfx, :+).

% answers
  answer('<urn:example:permutation>'([1, 2, 3, 4], [1, 2, 3, 4])).
  answer('<urn:example:permutation>'([1, 2, 3, 4], [1, 2, 4, 3])).
  answer('<urn:example:permutation>'([1, 2, 3, 4], [1, 3, 2, 4])).
  answer('<urn:example:permutation>'([1, 2, 3, 4], [1, 3, 4, 2])).
  answer('<urn:example:permutation>'([1, 2, 3, 4], [1, 4, 2, 3])).
  answer('<urn:example:permutation>'([1, 2, 3, 4], [1, 4, 3, 2])).
  answer('<urn:example:permutation>'([1, 2, 3, 4], [2, 1, 3, 4])).
  answer('<urn:example:permutation>'([1, 2, 3, 4], [2, 1, 4, 3])).
  answer('<urn:example:permutation>'([1, 2, 3, 4], [2, 3, 1, 4])).
  answer('<urn:example:permutation>'([1, 2, 3, 4], [2, 3, 4, 1])).
  answer('<urn:example:permutation>'([1, 2, 3, 4], [2, 4, 1, 3])).
  answer('<urn:example:permutation>'([1, 2, 3, 4], [2, 4, 3, 1])).
  answer('<urn:example:permutation>'([1, 2, 3, 4], [3, 1, 2, 4])).
  answer('<urn:example:permutation>'([1, 2, 3, 4], [3, 1, 4, 2])).
  answer('<urn:example:permutation>'([1, 2, 3, 4], [3, 2, 1, 4])).
  answer('<urn:example:permutation>'([1, 2, 3, 4], [3, 2, 4, 1])).
  answer('<urn:example:permutation>'([1, 2, 3, 4], [3, 4, 1, 2])).
  answer('<urn:example:permutation>'([1, 2, 3, 4], [3, 4, 2, 1])).
  answer('<urn:example:permutation>'([1, 2, 3, 4], [4, 1, 2, 3])).
  answer('<urn:example:permutation>'([1, 2, 3, 4], [4, 1, 3, 2])).
  answer('<urn:example:permutation>'([1, 2, 3, 4], [4, 2, 1, 3])).
  answer('<urn:example:permutation>'([1, 2, 3, 4], [4, 2, 3, 1])).
  answer('<urn:example:permutation>'([1, 2, 3, 4], [4, 3, 1, 2])).
  answer('<urn:example:permutation>'([1, 2, 3, 4], [4, 3, 2, 1])).

% proof steps
  step((true:+'<urn:example:permutation>'([1, 2, 3, 4], _)),
       '<urn:example:permutation>'([1, 2, 3, 4], [1, 2, 3, 4]),
       true).
  step((true:+'<urn:example:permutation>'([1, 2, 3, 4], _)),
       '<urn:example:permutation>'([1, 2, 3, 4], [1, 2, 4, 3]),
       true).
  step((true:+'<urn:example:permutation>'([1, 2, 3, 4], _)),
       '<urn:example:permutation>'([1, 2, 3, 4], [1, 3, 2, 4]),
       true).
  step((true:+'<urn:example:permutation>'([1, 2, 3, 4], _)),
       '<urn:example:permutation>'([1, 2, 3, 4], [1, 3, 4, 2]),
       true).
  step((true:+'<urn:example:permutation>'([1, 2, 3, 4], _)),
       '<urn:example:permutation>'([1, 2, 3, 4], [1, 4, 2, 3]),
       true).
  step((true:+'<urn:example:permutation>'([1, 2, 3, 4], _)),
       '<urn:example:permutation>'([1, 2, 3, 4], [1, 4, 3, 2]),
       true).
  step((true:+'<urn:example:permutation>'([1, 2, 3, 4], _)),
       '<urn:example:permutation>'([1, 2, 3, 4], [2, 1, 3, 4]),
       true).
  step((true:+'<urn:example:permutation>'([1, 2, 3, 4], _)),
       '<urn:example:permutation>'([1, 2, 3, 4], [2, 1, 4, 3]),
       true).
  step((true:+'<urn:example:permutation>'([1, 2, 3, 4], _)),
       '<urn:example:permutation>'([1, 2, 3, 4], [2, 3, 1, 4]),
       true).
  step((true:+'<urn:example:permutation>'([1, 2, 3, 4], _)),
       '<urn:example:permutation>'([1, 2, 3, 4], [2, 3, 4, 1]),
       true).
  step((true:+'<urn:example:permutation>'([1, 2, 3, 4], _)),
       '<urn:example:permutation>'([1, 2, 3, 4], [2, 4, 1, 3]),
       true).
  step((true:+'<urn:example:permutation>'([1, 2, 3, 4], _)),
       '<urn:example:permutation>'([1, 2, 3, 4], [2, 4, 3, 1]),
       true).
  step((true:+'<urn:example:permutation>'([1, 2, 3, 4], _)),
       '<urn:example:permutation>'([1, 2, 3, 4], [3, 1, 2, 4]),
       true).
  step((true:+'<urn:example:permutation>'([1, 2, 3, 4], _)),
       '<urn:example:permutation>'([1, 2, 3, 4], [3, 1, 4, 2]),
       true).
  step((true:+'<urn:example:permutation>'([1, 2, 3, 4], _)),
       '<urn:example:permutation>'([1, 2, 3, 4], [3, 2, 1, 4]),
       true).
  step((true:+'<urn:example:permutation>'([1, 2, 3, 4], _)),
       '<urn:example:permutation>'([1, 2, 3, 4], [3, 2, 4, 1]),
       true).
  step((true:+'<urn:example:permutation>'([1, 2, 3, 4], _)),
       '<urn:example:permutation>'([1, 2, 3, 4], [3, 4, 1, 2]),
       true).
  step((true:+'<urn:example:permutation>'([1, 2, 3, 4], _)),
       '<urn:example:permutation>'([1, 2, 3, 4], [3, 4, 2, 1]),
       true).
  step((true:+'<urn:example:permutation>'([1, 2, 3, 4], _)),
       '<urn:example:permutation>'([1, 2, 3, 4], [4, 1, 2, 3]),
       true).
  step((true:+'<urn:example:permutation>'([1, 2, 3, 4], _)),
       '<urn:example:permutation>'([1, 2, 3, 4], [4, 1, 3, 2]),
       true).
  step((true:+'<urn:example:permutation>'([1, 2, 3, 4], _)),
       '<urn:example:permutation>'([1, 2, 3, 4], [4, 2, 1, 3]),
       true).
  step((true:+'<urn:example:permutation>'([1, 2, 3, 4], _)),
       '<urn:example:permutation>'([1, 2, 3, 4], [4, 2, 3, 1]),
       true).
  step((true:+'<urn:example:permutation>'([1, 2, 3, 4], _)),
       '<urn:example:permutation>'([1, 2, 3, 4], [4, 3, 1, 2]),
       true).
  step((true:+'<urn:example:permutation>'([1, 2, 3, 4], _)),
       '<urn:example:permutation>'([1, 2, 3, 4], [4, 3, 2, 1]),
       true).
