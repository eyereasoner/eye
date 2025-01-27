% gps query

:- op(1200, xfx, :+).

true :+
    '<urn:example:findpath>'(
        '<urn:example:map_be>',
        [   '<urn:example:location>'(_SUBJECT, '<urn:example:oostende>'),
            _PATH,
            _DURATION,
            _COST,
            _BELIEF,
            _COMFORT,
            [5000.0, 5.0, 0.2, 0.4, 1]
        ]
    ).
