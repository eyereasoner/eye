:- op(1200, xfx, :+).

answer('<urn:example:zebra>'(norwegian, japanese)).

% proof steps
step((true:+'<urn:example:zebra>'(_, _)), '<urn:example:zebra>'(norwegian, japanese), true).
