:- op(1200, xfx, :+).

answer(findpath(map_be, [location(i1, oostende), [drive_gent_brugge, drive_brugge_oostende], 2400.0, 0.01, 0.9408, 0.99, [5000.0, 5.0, 0.2, 0.4, 1]])).
answer(findpath(map_be, [location(i1, oostende), [drive_gent_kortrijk, drive_kortrijk_brugge, drive_brugge_oostende], 4100.0, 0.018000000000000002, 0.903168, 0.9801, [5000.0, 5.0, 0.2, 0.4, 1]])).

step((location(i1, gent):+true), true, location(i1, gent)).
step((true:+findpath(map_be, [location(_, oostende), _, _, _, _, _, [5000.0, 5.0, 0.2, 0.4, 1]])), findpath(map_be, [location(i1, oostende), [drive_gent_brugge, drive_brugge_oostende], 2400.0, 0.01, 0.9408, 0.99, [5000.0, 5.0, 0.2, 0.4, 1]]), true).
step((true:+findpath(map_be, [location(_, oostende), _, _, _, _, _, [5000.0, 5.0, 0.2, 0.4, 1]])), findpath(map_be, [location(i1, oostende), [drive_gent_kortrijk, drive_kortrijk_brugge, drive_brugge_oostende], 4100.0, 0.018000000000000002, 0.903168, 0.9801, [5000.0, 5.0, 0.2, 0.4, 1]]), true).
