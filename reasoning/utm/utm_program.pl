% a Turing machine to add 1 to a binary number

start(0).

t( 0, 0, 0, r, 0 ).
t( 0, 1, 1, r, 0 ).
t( 0, #, #, l, 1 ).
t( 1, 0, 1, s, halt ).
t( 1, 1, 0, l, 1 ).
t( 1, #, 1, s, halt ).
