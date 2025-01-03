:- op(1200, xfx, :+).

% answers
  answer('<urn:example:is>'(some0, '<urn:example:good>'('<urn:example:Cobbler>'))).

% proof steps
  step(('<urn:example:is>'(_, '<urn:example:good>'('<urn:example:Cobbler>')):+true),
       true,
       '<urn:example:is>'(some0, '<urn:example:good>'('<urn:example:Cobbler>'))).
  step((true:+'<urn:example:is>'(_, '<urn:example:good>'(_))),
       '<urn:example:is>'(some0, '<urn:example:good>'('<urn:example:Cobbler>')),
       true).
