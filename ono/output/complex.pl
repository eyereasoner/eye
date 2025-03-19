:- op(1200, xfx, :+).

answer('urn:example:quotient'([[1,0],[0,1]],[0.0,-1.0])).
answer('urn:example:exponentiation'([[-1,0],[0.5,0]],[6.123233995736766e-17,1.0])).
answer('urn:example:exponentiation'([[2.718281828459045,0],[0,pi]],[-1.0,1.2246467991473532e-16])).
answer('urn:example:log'([[2.718281828459045,0],[-1,0]],[0.0,3.141592653589793])).
answer('urn:example:log'([[0,1],[0,1]],[1.0,0.0])).
answer('urn:example:sin'([1.5707963267949,1.316957896924817],[2.0,-6.6312755068093511e-16])).
answer('urn:example:cos'([0,-1.316957896924817],[2.0,0.0])).
answer('urn:example:tan'([1.338972522294493,0.402359478108525],[1.0,2.0])).
answer('urn:example:asin'([2,0],[1.5707963267949,1.31695789692482])).
answer('urn:example:acos'([2,0],[0.0,-1.31695789692482])).
answer('urn:example:atan'([1,2],[1.33897252229449,0.402359478108525])).

step((true:+'urn:example:quotient'([[1,0],[0,1]],A)),'urn:example:quotient'([[1,0],[0,1]],[0.0,-1.0]),true).
step((true:+'urn:example:exponentiation'([[-1,0],[0.5,0]],A)),'urn:example:exponentiation'([[-1,0],[0.5,0]],[6.123233995736766e-17,1.0]),true).
step((true:+'urn:example:exponentiation'([[2.718281828459045,0],[0,pi]],A)),'urn:example:exponentiation'([[2.718281828459045,0],[0,pi]],[-1.0,1.2246467991473532e-16]),true).
step((true:+'urn:example:log'([[2.718281828459045,0],[-1,0]],A)),'urn:example:log'([[2.718281828459045,0],[-1,0]],[0.0,3.141592653589793]),true).
step((true:+'urn:example:log'([[0,1],[0,1]],A)),'urn:example:log'([[0,1],[0,1]],[1.0,0.0]),true).
step((true:+'urn:example:sin'([1.5707963267949,1.316957896924817],A)),'urn:example:sin'([1.5707963267949,1.316957896924817],[2.0,-6.6312755068093511e-16]),true).
step((true:+'urn:example:cos'([0,-1.316957896924817],A)),'urn:example:cos'([0,-1.316957896924817],[2.0,0.0]),true).
step((true:+'urn:example:tan'([1.338972522294493,0.402359478108525],A)),'urn:example:tan'([1.338972522294493,0.402359478108525],[1.0,2.0]),true).
step((true:+'urn:example:asin'([2,0],A)),'urn:example:asin'([2,0],[1.5707963267949,1.31695789692482]),true).
step((true:+'urn:example:acos'([2,0],A)),'urn:example:acos'([2,0],[0.0,-1.31695789692482]),true).
step((true:+'urn:example:atan'([1,2],A)),'urn:example:atan'([1,2],[1.33897252229449,0.402359478108525]),true).
