# Euler Yet another proof Engine - EYE

### Engine for Unifying Logic and Explainable Reasoning - EULER

<img align="left" src="http://josd.github.io/images/eye.png" alt="EYE"/> EYE is a reasoning engine supporting the [Semantic Web layers](http://www.w3.org/DesignIssues/diagrams/sweb-stack/2006a).  
It performs controlled chaining of data and it supports Euler paths.  
Via [N3](http://www.w3.org/TeamSubmission/n3/) it is interoperable with [Cwm](http://www.w3.org/2000/10/swap/doc/cwm).  

__Controlled chaining of data__ is backward chaining for rules using `<=` in [N3](http://www.w3.org/TeamSubmission/n3/)  
or `:-` in POPEYE and forward chaining for rules using `=>` in [N3](http://www.w3.org/TeamSubmission/n3/).  
This is also called CCD and can be seen in [EYE reasoning](http://github.com/josd/eye/tree/master/reasoning).  

__Euler paths__ are roughly _"don't step in your own steps"_ which is inspired by  
what [Leonhard Euler](http://en.wikipedia.org/wiki/Leonhard_Euler) discovered in 1736 for the [Königsberg Bridge Problem](http://mathworld.wolfram.com/KoenigsbergBridgeProblem.html).  
EYE sees the rule `P => C` as `P & NOT(C) => C`.  

EYE can be [installed manually](http://github.com/josd/eye/blob/master/INSTALL) on Linux, Windows and MacOSX.  
EYE is also available in a [Docker container for command line use](http://hub.docker.com/r/bdevloed/eye/)  
and in a [Docker container for HTTP client use](http://hub.docker.com/r/bdevloed/eyeserver/).  

### Glossary

Term   | Description
-------|------------
CCD    | Controlled Chaining of Data
CPU    | Central Processing Unit
CWM    | Closed World Machine
EAM    | Euler Abstract Machine
EULER  | Engine for Unifying Logic and Explainable Reasoning
EYE    | Euler Yet another proof Engine
FLUID  | Formalizing, Linking, Unifying and Inferring Data
GRE    | Generic Reasoning Engine
N3     | Notation3
N3P    | Notation3 Prolog code
POPEYE | Plugin Of Prolog code for EYE
PVM    | Prolog Virtual Machine
RDF    | Resource Description Framework

### Architecture and design

The __EYE stack__ comprises the following Software and Machines:  
<img src="http://josd.github.io/images/EYE-stack.png" width="60%" height="60%" alt="EYE-stack"/>  

This is what the basic __EAM (Euler Abstract Machine)__ does in a nutshell:
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
