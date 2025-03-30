:- op(1200, xfx, :+).

answer('urn:example:findpath'('urn:example:map_be', ['urn:example:location'('urn:example:i1', 'urn:example:oostende'), ['urn:example:drive_gent_brugge', 'urn:example:drive_brugge_oostende'], 2400.0, 0.01, 0.9408, 0.99, [5000.0, 5.0, 0.2, 0.4, 1]])).
answer('urn:example:findpath'('urn:example:map_be', ['urn:example:location'('urn:example:i1', 'urn:example:oostende'), ['urn:example:drive_gent_kortrijk', 'urn:example:drive_kortrijk_brugge', 'urn:example:drive_brugge_oostende'], 4100.0, 0.018000000000000002, 0.903168, 0.9801, [5000.0, 5.0, 0.2, 0.4, 1]])).

step(('urn:example:location'('urn:example:i1', 'urn:example:gent'):+true), true, 'urn:example:location'('urn:example:i1', 'urn:example:gent')).
step((true:+'urn:example:findpath'('urn:example:map_be', ['urn:example:location'(_, 'urn:example:oostende'), _, _, _, _, _, [5000.0, 5.0, 0.2, 0.4, 1]])), 'urn:example:findpath'('urn:example:map_be', ['urn:example:location'('urn:example:i1', 'urn:example:oostende'), ['urn:example:drive_gent_brugge', 'urn:example:drive_brugge_oostende'], 2400.0, 0.01, 0.9408, 0.99, [5000.0, 5.0, 0.2, 0.4, 1]]), true).
step((true:+'urn:example:findpath'('urn:example:map_be', ['urn:example:location'(_, 'urn:example:oostende'), _, _, _, _, _, [5000.0, 5.0, 0.2, 0.4, 1]])), 'urn:example:findpath'('urn:example:map_be', ['urn:example:location'('urn:example:i1', 'urn:example:oostende'), ['urn:example:drive_gent_kortrijk', 'urn:example:drive_kortrijk_brugge', 'urn:example:drive_brugge_oostende'], 4100.0, 0.018000000000000002, 0.903168, 0.9801, [5000.0, 5.0, 0.2, 0.4, 1]]), true).
