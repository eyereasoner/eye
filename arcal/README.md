# arcal

The ARC triad is short for Answer, Reason, Check. It is presented as a simple way to write small, trustworthy programs that read like a short story. First you give the answer to a clear question, then you explain in everyday language why that answer follows, and finally you run a check that can fail loudly if an assumption is wrong or an edge case bites. The point is not only to compute a result but to carry an auditable trail that shows what was done, why it was valid, and how the program verifies itself.

ARC also comes with a compact discipline called P3: Prompt → Program → Proof. You start with a prompt that bundles the question, the data, and the rules you are willing to use; you write a small program to answer it; and you insist on a proof-like obligation made of two parts: the “reason why” in words (e.g. mathematical English) and the check that runs. In this framing, proof is practical, “Proof = Reason Why + Check.”

An ARC case is meant to be self-contained, readable, and repeatable. A good way to approach one is to skim for the question and the final output, then read the reasoning, and finally look at what the check actually verifies—perhaps a known identity, a conservation law, or a bound on error. If the check fails, the program should say so, and that failure becomes part of the learning, not something to hide. This posture makes the artifact easy to audit and safe to tinker with: change an input and rerun, and the checks will complain if something important goes off the rails.

