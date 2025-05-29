:- op(1200, xfx, :+).

answer(optimalTour([aaa,bbb,ccc,ddd,eee,fff,ggg],[[aaa,bbb,fff,ccc,eee,ddd,ggg],75])).

step((true:+optimalTour([aaa,bbb,ccc,ddd,eee,fff,ggg],A)),optimalTour([aaa,bbb,ccc,ddd,eee,fff,ggg],[[aaa,bbb,fff,ccc,eee,ddd,ggg],75]),true).
