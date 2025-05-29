:- op(1200, xfx, :+).

answer(compute([1,0,1,0,0,1],[1,0,1,0,1,0|"#"])).
answer(compute([1,0,1,1,1,1],[1,1,0,0,0,0|"#"])).
answer(compute([1,1,1,1,1,1],[1,0,0,0,0,0,0|"#"])).
answer(compute([],[1|"#"])).

step((true:+compute([1,0,1,0,0,1],A)),compute([1,0,1,0,0,1],[1,0,1,0,1,0|"#"]),true).
step((true:+compute([1,0,1,1,1,1],A)),compute([1,0,1,1,1,1],[1,1,0,0,0,0|"#"]),true).
step((true:+compute([1,1,1,1,1,1],A)),compute([1,1,1,1,1,1],[1,0,0,0,0,0,0|"#"]),true).
step((true:+compute([],A)),compute([],[1|"#"]),true).
