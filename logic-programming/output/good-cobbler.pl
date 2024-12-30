:- op(1200, xfx, :+).

% answers
  answer(('<urn:example:is>'(_, '<urn:example:good>'('<urn:example:Cobbler>')):+true)).

% proof steps
  step((true:+('<urn:example:is>'(_, '<urn:example:good>'(_)):+true)),
       ('<urn:example:is>'(_, '<urn:example:good>'('<urn:example:Cobbler>')):+true),
       true).
