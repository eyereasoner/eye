% Calculating the area of a polygon

area([_],0).
area([[A,B],[C,D]|E],F) :-
    area([[C,D]|E],G),
    F is (A*D-B*C)/2+G.

% query
query(area([[3,2],[6,2],[7,6],[4,6],[5,5],[5,3],[3,2]],_ANSWER)).
