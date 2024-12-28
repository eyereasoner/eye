% Crypo tests

% queries
true :+ '<http://www.w3.org/2000/10/swap/crypto#md5>'(literal('blargh', _), _).
true :+ '<http://www.w3.org/2000/10/swap/crypto#sha>'(literal('blargh', _), _).
true :+ '<http://www.w3.org/2000/10/swap/crypto#sha256>'(literal('blargh', _), _).
true :+ '<http://www.w3.org/2000/10/swap/crypto#sha512>'(literal('blargh', _), _).
