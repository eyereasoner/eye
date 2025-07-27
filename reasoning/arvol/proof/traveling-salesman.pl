optimalTour([aaa,bbb,ccc,ddd,eee,fff,ggg],[[aaa,bbb,fff,ccc,eee,ddd,ggg],75]).

step(rule(optimalTour([aaa, bbb, ccc, ddd, eee, fff, ggg], A), answer(optimalTour([aaa, bbb, ccc, ddd, eee, fff, ggg], A))), optimalTour([aaa, bbb, ccc, ddd, eee, fff, ggg], [[aaa, bbb, fff, ccc, eee, ddd, ggg], 75]), answer(optimalTour([aaa, bbb, ccc, ddd, eee, fff, ggg], [[aaa, bbb, fff, ccc, eee, ddd, ggg], 75]))).
