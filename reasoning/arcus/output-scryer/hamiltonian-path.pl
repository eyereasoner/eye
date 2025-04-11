:- op(1200, xfx, :+).

answer('urn:example:hamiltonianPath'([2,1,7,5,8,6,4,3])).
answer('urn:example:hamiltonianPath'([2,7,5,8,6,4,3,1])).

step((true:+'urn:example:hamiltonianPath'(A)),'urn:example:hamiltonianPath'([2,1,7,5,8,6,4,3]),true).
step((true:+'urn:example:hamiltonianPath'(A)),'urn:example:hamiltonianPath'([2,7,5,8,6,4,3,1]),true).
