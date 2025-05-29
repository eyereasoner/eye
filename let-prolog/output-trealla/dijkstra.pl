:- op(1200, xfx, :+).

answer(dijkstra("af",["acbdef",13])).
answer(dijkstra("af",["acbdf",14])).
answer(dijkstra("af",["abdef",14])).
answer(dijkstra("af",["abdf",15])).
answer(dijkstra("af",["acef",15])).
answer(dijkstra("af",["acdef",15])).
answer(dijkstra("af",["acdf",16])).

step((edge([A,B],C):+edge([B,A],C)),edge("ab",4),edge("ba",4)).
step((edge([A,B],C):+edge([B,A],C)),edge("ac",2),edge("ca",2)).
step((edge([A,B],C):+edge([B,A],C)),edge("bc",1),edge("cb",1)).
step((edge([A,B],C):+edge([B,A],C)),edge("bd",5),edge("db",5)).
step((edge([A,B],C):+edge([B,A],C)),edge("cd",8),edge("dc",8)).
step((edge([A,B],C):+edge([B,A],C)),edge("ce",10),edge("ec",10)).
step((edge([A,B],C):+edge([B,A],C)),edge("de",2),edge("ed",2)).
step((edge([A,B],C):+edge([B,A],C)),edge("df",6),edge("fd",6)).
step((edge([A,B],C):+edge([B,A],C)),edge("ef",3),edge("fe",3)).
step((true:+dijkstra("af",[A,B])),dijkstra("af",["acbdef",13]),true).
step((true:+dijkstra("af",[A,B])),dijkstra("af",["acbdf",14]),true).
step((true:+dijkstra("af",[A,B])),dijkstra("af",["abdef",14]),true).
step((true:+dijkstra("af",[A,B])),dijkstra("af",["abdf",15]),true).
step((true:+dijkstra("af",[A,B])),dijkstra("af",["acef",15]),true).
step((true:+dijkstra("af",[A,B])),dijkstra("af",["acdef",15]),true).
step((true:+dijkstra("af",[A,B])),dijkstra("af",["acdf",16]),true).
