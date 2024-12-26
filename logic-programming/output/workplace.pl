:- op(1200, xfx, :=).

answer('urn:example:complies'('urn:example:alice', true)).
answer('urn:example:complies'('urn:example:carol', false)).
answer('urn:example:complies'('urn:example:bob', false)).

%
% Proof steps
%

step(('urn:example:complies'(A, true):='urn:example:does'(A, 'urn:example:log_off_at_end_of_shift')), 'urn:example:does'('urn:example:alice', 'urn:example:log_off_at_end_of_shift'), 'urn:example:complies'('urn:example:alice', true)).
step(('urn:example:complies'(A, false):='urn:example:does'(A, 'urn:example:access_social_media')), 'urn:example:does'('urn:example:carol', 'urn:example:access_social_media'), 'urn:example:complies'('urn:example:carol', false)).
step(('urn:example:complies'(A, false):='urn:example:does'(A, 'urn:example:work_related_task'), stable(1), \+'urn:example:does'(A, 'urn:example:log_off_at_end_of_shift')), ('urn:example:does'('urn:example:bob', 'urn:example:work_related_task'), stable(1), \+'urn:example:does'('urn:example:bob', 'urn:example:log_off_at_end_of_shift')), 'urn:example:complies'('urn:example:bob', false)).
