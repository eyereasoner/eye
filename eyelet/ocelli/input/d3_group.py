from itertools import permutations
import pprint

# ---- 1.  Helper: permutation compose ----------------------------------------
def compose(p, q):
    """Return the composition p∘q (apply q, then p).  Both p and q are tuples of length 3."""
    return tuple(p[i] for i in q)

# ---- 2.  Build D3 ------------------------------------------------------------
# Vertex labels: 0, 1, 2  (counter-clockwise around the triangle)
rot0 = (0, 1, 2)               # identity
rot1 = (1, 2, 0)               # rotation by 120°
rot2 = (2, 0, 1)               # rotation by 240°

ref0 = (0, 2, 1)               # reflection over axis through vertex 0
ref1 = (2, 1, 0)               # reflection over axis through vertex 1
ref2 = (1, 0, 2)               # reflection over axis through vertex 2

elements = {
    "e" : rot0,
    "r" : rot1,
    "r²": rot2,
    "s" : ref0,      # reflection labels (you can rename as you like)
    "sr": ref1,
    "sr²": ref2,
}

# ---- 3.  Cayley table --------------------------------------------------------
labels = list(elements.keys())
table = {a: {b: None for b in labels} for a in labels}

for a in labels:
    for b in labels:
        prod = compose(elements[a], elements[b])
        # find the key whose permutation matches prod
        prod_label = next(k for k, v in elements.items() if v == prod)
        table[a][b] = prod_label

print("Cayley table for D3 (row ∘ column):")
w = max(len(l) for l in labels)
header = " " * (w+1) + " ".join(label.rjust(w) for label in labels)
print(header)
for row in labels:
    line = row.rjust(w) + " "
    line += " ".join(table[row][col].rjust(w) for col in labels)
    print(line)

# ---- 4.  Demo: applying symmetries ------------------------------------------
triangle = [(0,0), (1,0), (0.5,0.866)]   # coordinates for vertices 0,1,2

def apply(sym, pts):
    """Return new list of points according to the permutation `sym`."""
    return [pts[i] for i in sym]

print("\nExample: where each symmetry sends the triangle’s vertices:")
for name, perm in elements.items():
    print(f"{name:>3}: {apply(perm, triangle)}")

