% gps query

:- op(1200, xfx, :+).

true :+
    '<https://eyereasoner.github.io/ns#findpath>'(
        '<https://eyereasoner.github.io/ns#map_be>',
        [   '<https://eyereasoner.github.io/ns#location>'(_SUBJECT, '<https://eyereasoner.github.io/ns#oostende>'),
            _PATH,
            _DURATION,
            _COST,
            _BELIEF,
            _COMFORT,
            [5000.0, 5.0, 0.2, 0.4, 1]
        ]
    ).
