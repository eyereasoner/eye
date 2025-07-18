# 📐 Pythagoras Theorem – Eyelet Logic Demo

This tiny example shows how the **Pythagorean relation**

> a² + b² = c²

can be captured in pure **Notation 3 / eyelet** rules and proven automatically
by the [**EYE** reasoner](https://github.com/eyereasoner/eye).

Everything here is *symbolic*: we declare three sides `:a :b :c` and their
squares `:a2 :b2 :c2`, state that `a² + b² = c²`, and let one generic rule
conclude `Pythagoras(a,b,c)` for *any* triple of legs and hypotenuse that obey
those facts.
