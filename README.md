# EYE - Euler Yet another proof Engine

### Elaborating Unifying Logic with EYE Reasoning

<img align="left" src="http://josd.github.io/images/eye.png" alt="EYE"/> EYE is a reasoning engine supporting the [Semantic Web layers](http://www.w3.org/DesignIssues/diagrams/sweb-stack/2006a).  
It performs controlled chaining and it supports Euler paths.  
Via [N3](http://www.w3.org/TeamSubmission/n3/) it is interoperable with [Cwm](http://www.w3.org/2000/10/swap/doc/cwm).  

__Controlled chaining__ is backward chaining for rules using <= in [N3](http://www.w3.org/TeamSubmission/n3/)  
and forward chaining for rules using => in [N3](http://www.w3.org/TeamSubmission/n3/).  
This can be seen in [EYE reasoning](http://github.com/josd/eye/tree/master/reasoning).  

__Euler paths__ are roughly "_don't step in your own steps_" which is inspired by  
what [Leonhard Euler](http://en.wikipedia.org/wiki/Leonhard_Euler) discovered in 1736 for the [Königsberg Bridge Problem](http://mathworld.wolfram.com/KoenigsbergBridgeProblem.html).  
EYE sees the rule _P => C_ as _P & NOT(C) => C_.  

EYE can be [installed manually](http://github.com/josd/eye/blob/master/INSTALL) on Linux, Windows and MacOSX.  
EYE is also available in a [Docker container for command line use](http://hub.docker.com/r/bdevloed/eye/)  
and in a [Docker container for HTTP client use](http://hub.docker.com/r/bdevloed/eyeserver/).  

### Architecture and design

Here are the layers of the EYE Stack:  
<img src="http://josd.github.io/images/EYE-stack.png" width="60%" height="60%" alt="EYE Stack"/>  

This is what the basic EAM (Euler Abstract Machine) does in a nutshell:
1. Select rule P => C  
2. Prove P & NOT(C) (backward chaining) and if it fails backtrack to 1.  
3. If P & NOT(C) assert C (forward chaining) and remove brake  
4. If C = answer(A) and tactic limited-answer stop, else backtrack to 2.  
5. If brake or tactic linear-select stop, else start again at 1.  

### See also

EYE paper
* [Drawing Conclusions from Linked Data on the Web: The EYE Reasoner](http://josd.github.io/Papers/EYE.pdf)

EYE tutorial
* [Semantic Web Reasoning With EYE](http://n3.restdesc.org/)

EYE talk
* [EYE looking through N3 glasses](http://josd.github.io/Talks/2012/04swig/index.html)
