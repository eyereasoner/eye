# See https://en.wikipedia.org/wiki/Evolutionary_algorithm
# Original code from https://rosettacode.org/wiki/Evolutionary_algorithm#Python

from random import choice, random
 
target  = list("METHINKS IT IS LIKE A WEASEL")
alphabet = " ABCDEFGHIJLKLMNOPQRSTUVWXYZ"
p = 0.05 # mutation probability
c = 100  # number of children in each generation
 
def neg_fitness(trial):
    return sum(t != h for t,h in zip(trial, target))
 
def mutate(parent):
    return [(choice(alphabet) if random() < p else ch) for ch in parent]
 
parent = [choice(alphabet) for _ in range(len(target))]
i = 0
while parent != target:
    copies = (mutate(parent) for _ in range(c))
    parent = min(copies, key=neg_fitness)
    # print("%3d" % i)
    i += 1
print("".join(parent))
