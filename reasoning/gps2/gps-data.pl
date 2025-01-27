% test data: partial map of Belgium

:- op(1200, xfx, :+).

:- dynamic('<urn:example:location>'/2).

'<urn:example:description>'(
    '<urn:example:map_be>',
    [   '<urn:example:location>'(S, '<urn:example:gent>'),
        true,
        '<urn:example:location>'(S, '<urn:example:brugge>'),
        '<urn:example:drive_gent_brugge>',
        1500.0,
        0.006,
        0.96,
        0.99
    ]
).
'<urn:example:description>'(
    '<urn:example:map_be>',
    [   '<urn:example:location>'(S, '<urn:example:gent>'),
        true,
        '<urn:example:location>'(S, '<urn:example:kortrijk>'),
        '<urn:example:drive_gent_kortrijk>',
        1600.0,
        0.007,
        0.96,
        0.99
    ]
).
'<urn:example:description>'(
    '<urn:example:map_be>',
    [   '<urn:example:location>'(S, '<urn:example:kortrijk>'),
        true,
        '<urn:example:location>'(S, '<urn:example:brugge>'),
        '<urn:example:drive_kortrijk_brugge>',
        1600.0,
        0.007,
        0.96,
        0.99
    ]
).
'<urn:example:description>'(
    '<urn:example:map_be>',
    [   '<urn:example:location>'(S, '<urn:example:brugge>'),
        true,
        '<urn:example:location>'(S, '<urn:example:oostende>'),
        '<urn:example:drive_brugge_oostende>',
        900.0,
        0.004,
        0.98,
        1.0
    ]
).

% current state
'<urn:example:location>'('<urn:example:i1>', '<urn:example:gent>').
