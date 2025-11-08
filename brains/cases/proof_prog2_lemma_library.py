#!/usr/bin/env python3
"""
P3 — Program 2: Lemma Library (Facts the proofs will use)

Short story (header):
  Collect the tiny facts needed for each claim type (definitions + standard lemmas).

Overview:
  - Reads './cases/bus/proof/normalized_claims.json'.
  - Emits a minimal library of facts keyed by claim type.
  - Prints Answer/Reason/Check; writes './cases/bus/proof/lemma_library.json' for Program 3.

Facts included (informal):
  - Divisibility: a|b ⇔ ∃k, b = a·k ; transitivity if a|b & b|c ⇒ a|c
  - Evenness: x is even ⇔ ∃p, x = 2p ; even+even=even via x=2p, y=2q ⇒ x+y=2(p+q)
  - Bézout: ∃s,t, s·a + t·b = gcd(a,b)
  - Coprime product: gcd(a,b)=1 & a|(b·c) ⇒ a|c
  - Mod addition: a≡a' (mod m), b≡b' (mod m) ⇒ a+b≡a'+b' (mod m)
"""
from __future__ import annotations
import argparse, json
from typing import Dict, Any, List

### LOGIC
def build_library(norm: Dict[str,Any]) -> Dict[str,Any]:
    facts = {
        "divisibility_transitive": [
            "def_divides:b=ak", "def_divides:c=bℓ", "compose: c=akℓ ⇒ a|c"
        ],
        "even_plus_even_is_even": [
            "def_even:x=2p", "def_even:y=2q", "sum_even:x+y=2(p+q)"
        ],
        "gcd_linear_combination": [
            "bezout: ∃s,t. s·a+t·b=gcd(a,b)"
        ],
        "coprime_product_divides_implies_divides_factor": [
            "coprime_gcd1:gcd(a,b)=1", "a|bc ⇒ a|c"
        ],
        "mod_addition_compatible": [
            "def_mod:a≡a'(m) ⇒ m|(a−a')", "def_mod:b≡b'(m) ⇒ m|(b−b')",
            "closure: m|(a−a') & m|(b−b') ⇒ m|((a+b)−(a'+b'))"
        ]
    }
    used_types = sorted({c["type"] for c in norm["claims"]})
    reasons = [f"Loaded facts for {t}" for t in used_types]
    return {"facts": facts, "used_types": used_types, "reasons": reasons}

### CHECK
def run_harness(lib: Dict[str,Any], norm: Dict[str,Any]) -> None:
    print("Check — every claim type has a fact list:")
    for c in norm["claims"]:
        assert c["type"] in lib["facts"] and isinstance(lib["facts"][c["type"]], list)

def main():
    ap = argparse.ArgumentParser(description="Produce lemma library for claims.")
    ap.add_argument("--infile", default="./cases/bus/proof/normalized_claims.json")
    ap.add_argument("--out", default="./cases/bus/proof/lemma_library.json")
    args = ap.parse_args()

    norm = json.load(open(args.infile,"r",encoding="utf-8"))
    lib = build_library(norm)

    print("# ANSWER"); print(json.dumps({"used_types": lib["used_types"]}, indent=2))
    print("\n# REASONS"); [print("-", r) for r in lib["reasons"]]

    json.dump(lib, open(args.out,"w",encoding="utf-8"), indent=2)
    print(f"\nWrote {args.out}")

    print("\n# CHECK"); run_harness(lib, norm); print("✔ All checks passed.")

if __name__ == "__main__":
    main()

