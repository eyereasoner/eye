dijkstra([a,f],[[a,b,d,e,f],14]).
dijkstra([a,f],[[a,b,d,f],15]).
dijkstra([a,f],[[a,c,e,f],15]).
dijkstra([a,f],[[a,c,d,e,f],15]).
dijkstra([a,f],[[a,c,d,f],16]).
dijkstra([a,f],[[a,c,b,d,e,f],13]).
dijkstra([a,f],[[a,c,b,d,f],14]).

step(rule(dijkstra([a, f], [A, B]), answer(dijkstra([a, f], [A, B]))), dijkstra([a, f], [[a, b, d, e, f], 14]), answer(dijkstra([a, f], [[a, b, d, e, f], 14]))).
step(rule(dijkstra([a, f], [A, B]), answer(dijkstra([a, f], [A, B]))), dijkstra([a, f], [[a, b, d, f], 15]), answer(dijkstra([a, f], [[a, b, d, f], 15]))).
step(rule(dijkstra([a, f], [A, B]), answer(dijkstra([a, f], [A, B]))), dijkstra([a, f], [[a, c, e, f], 15]), answer(dijkstra([a, f], [[a, c, e, f], 15]))).
step(rule(dijkstra([a, f], [A, B]), answer(dijkstra([a, f], [A, B]))), dijkstra([a, f], [[a, c, d, e, f], 15]), answer(dijkstra([a, f], [[a, c, d, e, f], 15]))).
step(rule(dijkstra([a, f], [A, B]), answer(dijkstra([a, f], [A, B]))), dijkstra([a, f], [[a, c, d, f], 16]), answer(dijkstra([a, f], [[a, c, d, f], 16]))).
step(rule(edge([A, B], C), edge([B, A], C)), edge([a, b], 4), edge([b, a], 4)).
step(rule(edge([A, B], C), edge([B, A], C)), edge([a, c], 2), edge([c, a], 2)).
step(rule(edge([A, B], C), edge([B, A], C)), edge([b, c], 1), edge([c, b], 1)).
step(rule(edge([A, B], C), edge([B, A], C)), edge([b, d], 5), edge([d, b], 5)).
step(rule(edge([A, B], C), edge([B, A], C)), edge([c, d], 8), edge([d, c], 8)).
step(rule(edge([A, B], C), edge([B, A], C)), edge([c, e], 10), edge([e, c], 10)).
step(rule(edge([A, B], C), edge([B, A], C)), edge([d, e], 2), edge([e, d], 2)).
step(rule(edge([A, B], C), edge([B, A], C)), edge([d, f], 6), edge([f, d], 6)).
step(rule(edge([A, B], C), edge([B, A], C)), edge([e, f], 3), edge([f, e], 3)).
step(rule(dijkstra([a, f], [A, B]), answer(dijkstra([a, f], [A, B]))), dijkstra([a, f], [[a, c, b, d, e, f], 13]), answer(dijkstra([a, f], [[a, c, b, d, e, f], 13]))).
step(rule(dijkstra([a, f], [A, B]), answer(dijkstra([a, f], [A, B]))), dijkstra([a, f], [[a, c, b, d, f], 14]), answer(dijkstra([a, f], [[a, c, b, d, f], 14]))).
