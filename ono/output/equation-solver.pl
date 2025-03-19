:- op(1200, xfx, :+).

answer('urn:example:equation'(x^2-3*x+2=0,x,[2.0,1.0])).
answer('urn:example:equation'(cos(x)*(1-2*sin(x))=0,x,[1.5707963267949,-1.5707963267949,-1.5707963267949,0.523598775598299,179.476401224402])).
answer('urn:example:equation'(2^(2*x)-5*2^(x+1)+16=0,x,[3.0,1.0])).

step((true:+'urn:example:equation'(x^2-3*x+2=0,x,A)),'urn:example:equation'(x^2-3*x+2=0,x,[2.0,1.0]),true).
step((true:+'urn:example:equation'(cos(x)*(1-2*sin(x))=0,x,A)),'urn:example:equation'(cos(x)*(1-2*sin(x))=0,x,[1.5707963267949,-1.5707963267949,-1.5707963267949,0.523598775598299,179.476401224402]),true).
step((true:+'urn:example:equation'(2^(2*x)-5*2^(x+1)+16=0,x,A)),'urn:example:equation'(2^(2*x)-5*2^(x+1)+16=0,x,[3.0,1.0]),true).
