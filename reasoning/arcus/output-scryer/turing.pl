:- op(1200, xfx, :+).

answer('urn:example:compute'([1,0,1,0,0,1],[1,0,1,0,1,0|"#"])).
answer('urn:example:compute'([1,0,1,1,1,1],[1,1,0,0,0,0|"#"])).
answer('urn:example:compute'([1,1,1,1,1,1],[1,0,0,0,0,0,0|"#"])).
answer('urn:example:compute'([],[1|"#"])).

step((true:+'urn:example:compute'([1,0,1,0,0,1],A)),'urn:example:compute'([1,0,1,0,0,1],[1,0,1,0,1,0|"#"]),true).
step((true:+'urn:example:compute'([1,0,1,1,1,1],A)),'urn:example:compute'([1,0,1,1,1,1],[1,1,0,0,0,0|"#"]),true).
step((true:+'urn:example:compute'([1,1,1,1,1,1],A)),'urn:example:compute'([1,1,1,1,1,1],[1,0,0,0,0,0,0|"#"]),true).
step((true:+'urn:example:compute'([],A)),'urn:example:compute'([],[1|"#"]),true).
