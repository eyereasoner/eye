:- op(1200, xfx, :+).

answer('urn:example:optimalTour'([aaa,bbb,ccc,ddd,eee,fff,ggg],[[aaa,bbb,fff,ccc,eee,ddd,ggg],75])).

step((true:+'urn:example:optimalTour'([aaa,bbb,ccc,ddd,eee,fff,ggg],A)),'urn:example:optimalTour'([aaa,bbb,ccc,ddd,eee,fff,ggg],[[aaa,bbb,fff,ccc,eee,ddd,ggg],75]),true).
