:- op(1200, xfx, :+).

answer('urn:example:sum'(4096,8390656)).

step((true:+'urn:example:sum'(4096,A)),'urn:example:sum'(4096,8390656),true).
