#!/usr/bin/env python3
"""
P3 — Program 3: Proof Sketch Builder (witnesses & steps)

Short story (header):
  Using normalized claims + lemma facts, build a small step list per claim,
  producing witnesses (k,ℓ,p,q,s,t,…) where needed.

Overview:
  - Reads './bus/proof/normalized_claims.json' + './bus/proof/lemma_library.json'.
  - For each claim, constructs a proof sketch with concrete witnesses.
  - Prints Answer/Reason/Check; writes './bus/proof/proof_sketches.json' for Program 4.
"""
from __future__ import annotations
import argparse, json, math
from typing import Dict, Any, List, Tuple

def egcd(a:int,b:int)->Tuple[int,int,int]:
    """Extended Euclid: returns (g,s,t) with s*a+t*b=g=gcd(a,b)."""
    if b==0:
        return (abs(a), 1 if a>0 else -1, 0)
    g,x1,y1 = egcd(b, a%b)
    return (g, y1, x1 - (a//b)*y1)

def build_sketches(norm: Dict[str,Any], lib: Dict[str,Any]) -> Dict[str,Any]:
    sketches = []
    reasons = []
    for c in norm["claims"]:
        t = c["type"]; d = c["data"]; steps = []; witnesses = {}
        if t=="divisibility_transitive":
            a,b,cnum = d["a"], d["b"], d["c"]
            # find k,ℓ such that b=a·k and c=b·ℓ (witnesses from division)
            assert b%a==0 and cnum%b==0
            k = b//a; l = cnum//b
            witnesses = {"k":k, "l":l}
            steps = [f"b=a·k with k={k}", f"c=b·ℓ with ℓ={l}", "compose: c=a·(kℓ) ⇒ a|c"]
        elif t=="even_plus_even_is_even":
            x,y = d["x"], d["y"]; assert x%2==0 and y%2==0
            p,q = x//2, y//2
            witnesses = {"p":p,"q":q}
            steps = [f"x=2p with p={p}", f"y=2q with q={q}", f"x+y=2(p+q) with p+q={p+q}"]
        elif t=="gcd_linear_combination":
            a,b = d["a"], d["b"]
            g,s,tv = egcd(a,b)
            witnesses = {"g":g,"s":s,"t":tv}
            steps = [f"g=gcd(a,b)={g}", f"Compute s,t via extended Euclid: s={s}, t={tv}", f"s·a+t·b=g"]
        elif t=="coprime_product_divides_implies_divides_factor":
            a,b,cnum = d["a"], d["b"], d["c"]
            # premise: gcd(a,b)=1 and a | b·c
            assert math.gcd(a,b)==1 and (b*cnum)%a==0
            k = (b*cnum)//a
            witnesses = {"k":k}
            steps = [f"gcd(a,b)=1", f"a|(b·c) with witness k={k}", "⇒ a|c (standard lemma)"]
        elif t=="mod_addition_compatible":
            m,a,a1,b,b1 = d["m"], d["a"], d["a_equiv"], d["b"], d["b_equiv"]
            # witnesses r,s: a-a' = m·r, b-b' = m·s
            assert (a-a1)%m==0 and (b-b1)%m==0
            r = (a-a1)//m; s = (b-b1)//m
            witnesses = {"r":r,"s":s}
            steps = [f"a−a' = m·r with r={r}", f"b−b' = m·s with s={s}",
                     "⇒ (a+b)−(a'+b') = m·(r+s) ⇒ a+b ≡ a'+b' (mod m)"]
        else:
            raise ValueError("unknown type")
        sketches.append({"id":c["id"],"type":t,"data":d,"steps":steps,"witnesses":witnesses})
        reasons.append(f"{c['id']}: built sketch with witnesses {witnesses}")
    return {"sketches": sketches, "reasons": reasons}

### CHECK
def run_harness(sk: Dict[str,Any]) -> None:
    print("Check — every sketch has steps and (when relevant) witnesses:")
    for s in sk["sketches"]:
        assert len(s["steps"])>=1
        if s["type"] in {"divisibility_transitive","even_plus_even_is_even","gcd_linear_combination","mod_addition_compatible"}:
            assert len(s["witnesses"])>=1

def main():
    ap = argparse.ArgumentParser(description="Build proof sketches with witnesses.")
    ap.add_argument("--in_norm", default="./bus/proof/normalized_claims.json")
    ap.add_argument("--in_lib",  default="./bus/proof/lemma_library.json")
    ap.add_argument("--out",     default="./bus/proof/proof_sketches.json")
    args = ap.parse_args()

    norm = json.load(open(args.in_norm,"r",encoding="utf-8"))
    lib  = json.load(open(args.in_lib,"r",encoding="utf-8"))
    sk   = build_sketches(norm, lib)

    print("# ANSWER"); print(json.dumps({"n_sketches": len(sk["sketches"])}, indent=2))
    print("\n# REASONS"); [print("-", r) for r in sk["reasons"]]

    json.dump(sk, open(args.out,"w",encoding="utf-8"), indent=2)
    print(f"\nWrote {args.out}")

    print("\n# CHECK"); run_harness(sk); print("✔ All checks passed.")

if __name__ == "__main__":
    main()

