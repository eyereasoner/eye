# Euler Yet another proof Engine - EYE

![EYE](https://josd.github.io/images/eye.png)

EYE is a reasoning engine supporting the [Semantic Web layers](http://www.w3.org/DesignIssues/diagrams/sweb-stack/2006a) and implementing [Notation3](https://w3c.github.io/N3/spec/).

EYE performs _forward_ and _backward chaining_ along Euler paths.  Forward chaining is applied for rules using `=>` in Notation3 and backward chaining is applied for rules using `<=` in Notation3 which one can imagine as user defined built-ins. Euler paths are roughly "don't step in your own steps" which is inspired by what [Leonhard Euler](https://en.wikipedia.org/wiki/Leonhard_Euler) discovered in 1736 for the [Königsberg Bridge Problem](http://mathworld.wolfram.com/KoenigsbergBridgeProblem.html). 

## Installation

- Install SWI-Prolog from http://www.swi-prolog.org/Download.html
- Test the SWI-Prolog installation via command line `swipl --version` and it should return the installed version number.

- Run the installation script `install.sh [--prefix=Prefix]`.  The default prefix is /usr/local.  This will
    - create the EYE image file at `$prefix/lib/eye.pvm`
    - create the EYE launch script eye ub `$prefix/bin/eye`

Test the EYE installation via command line `eye --version` and it should return the version which is in the file VERSION.

## Usage

Create a test Notation3 file. We use the file `socrates.n3` as example:

```(Turtle)
$ cat socrates.n3
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>.
@prefix : <http://example.org/socrates#>.

:Socrates a :Human.
:Human rdfs:subClassOf :Mortal.

{
    ?S a ?A .
    ?A rdfs:subClassOf ?B . 
} 
=> 
{
    ?S a ?B .
} .
```

Run the EYE reasoner without proof explanation, in quiet mode and passing all deductive closures 
to the output:

```
$ eye --nope --quiet --pass socrates.n3
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>.
@prefix : <http://example.org/socrates#>.

:Socrates a :Human.
:Socrates a :Mortal.
:Human rdfs:subClassOf :Mortal.
``` 

## Tutorial and example scripts

- [Eye command line arguments and flags](https://github.com/eyereasoner/eye/tree/master/documentation/command_line.md)
- [EYE reasoning examples](https://github.com/eyereasoner/eye/tree/master/reasoning)
- [Notation3 by example](https://github.com/eyereasoner/Notation3-By-Example)
- [RDF Surfaces aka BLOGIC](https://github.com/eyereasoner/eye/tree/master/documentation/blogic.md)
- [Running EYE using Docker](https://github.com/eyereasoner/eye/tree/master/documentation/docker.md)

## Online versions of EYE

- Notation3 Editor https://editor.notation3.org/
- Semantic Web Reasoning With N3 https://n3.restdesc.org/rules/executing-rules/
- Eyebrow https://github.com/eyereasoner/eyebrow

## References

- Home page of EYE https://eyereasoner.github.io/eye/
- Former home page of EYE http://eulersharp.sourceforge.net/
- Notation3 W3C Draft Community Group Report https://w3c.github.io/N3/spec/
- More EYE tools and scripts https://github.com/eyereasoner/
- Design Issues of Tim Berners-Lee: [The Semantic Web as a language of logic](https://www.w3.org/DesignIssues/Logic.html)
- PhD thesis of Dörthe Arndt: [Notation3 as the Unifying Logic for the Semantic Web](https://biblio.ugent.be/publication/8634507)

## Publications

Verborgh, R. , De Roo, J. : Drawing Conclusions from Linked Data on the Web: The EYE Reasoner. IEEE Software (2015) [Online Version](https://ieeexplore.ieee.org/abstract/document/7093047?casa_token=LL6C9FqiqAQAAAAA:ykrmxL6lxFn5KyHZDj8HkcMuME3DXrOEYmgao3XXeFUp5kPXW2hyCI7MmE9zBuvohYqOo7WnSCFF)

## License & copyright

MIT License

Copyright 2006-2023 Jos De Roo

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
