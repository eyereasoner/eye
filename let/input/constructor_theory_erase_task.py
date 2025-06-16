from constructor_theory import Substrate, Task, ParallelTask, ConstructorTheory

# ──────────────────────────────────────────────────────────────────────────
# 1.  Two separate 1-bit substrates
# ──────────────────────────────────────────────────────────────────────────
a = Substrate("a", {0, 1})
b = Substrate("b", {0, 1})

# 2.  A SWAP task – reversible: (x, y) → (y, x)
swap_sub = Substrate("pair", {(0,0), (0,1), (1,0), (1,1)})
swap_map = {(0,1):(1,0), (1,0):(0,1)}           # Only need to say what changes
SWAP = Task(swap_sub, swap_map, name="SWAP")

print("SWAP reversible? ", SWAP.is_reversible)   # ==> True

# 3.  An ERASE task on ‘a’ – irreversible: 0→0, 1→0
ERASE = Task(a, {1: 0}, name="ERASE")
print("ERASE reversible?", ERASE.is_reversible)  # ==> False

# 4.  Compose: ERASE on ‘a’ in parallel with identity on ‘b’
ID_b = Task(b, {}, name="ID_b")
erase_and_keep = ERASE * ID_b                    # ParallelTask

# 5.  Series-compose SWAP after ERASE×ID
combo = SWAP @ erase_and_keep                    # still works: different substrates → ParallelTask

print("Composite class:", combo.__class__.__name__)
print("Some mappings:")
for k in sorted(list(combo.mapping)[:4]):
    print(f"   {k} → {combo.mapping[k]}")

# 6.  Put the statements into a ledger
ledger = ConstructorTheory()
ledger.declare_possible(SWAP,  "Assume we have a perfect swap constructor.")
ledger.declare_possible(ID_b,  "Doing nothing to b is possible.")
# — We do **not** declare ERASE possible (Landauer’s principle might forbid a perfect constructor!)

ledger.declare_impossible(ERASE, "Perfect bit erasure violates reversibility + energy balance.")

print("\nIs ERASE possible?        ", ledger.is_possible(ERASE))
print("Is SWAP∘(ERASE×ID) possible?", ledger.is_possible(combo))
