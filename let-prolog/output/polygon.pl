:- op(1200, xfx, :+).

answer(area([[3, 2], [6, 2], [7, 6], [4, 6], [5, 5], [5, 3], [3, 2]], 7.5)).

step((true:+area([[3, 2], [6, 2], [7, 6], [4, 6], [5, 5], [5, 3], [3, 2]], _)), area([[3, 2], [6, 2], [7, 6], [4, 6], [5, 5], [5, 3], [3, 2]], 7.5), true).
