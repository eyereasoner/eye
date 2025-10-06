#!/usr/bin/env python3
"""
P3 — Program 4: Mechanical Proof Checker (Model-check by computation)

Short story (header):
  Verify each proof sketch step-by-step using definitions and arithmetic.
  For each claim we produce a verdict and a trace.

Overview:
  - Reads './bus/proof/proof_sketches.json'.
  - Checks witnesses and equalities by computation; recomputes gcd and mod conditions.
  - Prints Answer/Reason/Check; writes './bus/proof/proof_verdicts.json' for Program 5.
"""
from __future__ import annotations
import argparse, json, math
from typing import Dict, Any, List

def check_one(sk: Dict[str,Any]) -> Dict[str,Any]:
    t = sk["type"]; d = sk["data"]; W = sk["witnesses"]
    ok = False; trace = []
    try:
        if t=="divisibility_transitive":
            a,b,cnum = d["a"], d["b"], d["c"]; k,l = W["k"], W["l"]
            trace.append(f"b == a*k ? {b} == {a}*{k} → {a*k==b}")
            trace.append(f"c == b*l ? {cnum} == {b}*{l} → {b*l==cnum}")
            ok = (a*k==b) and (b*l==cnum) and (cnum%(a)==0)
        elif t=="even_plus_even_is_even":
            x,y = d["x"], d["y"]; p,q = W["p"], W["q"]
            trace.append(f"x==2p ? {x}==2*{p} → {2*p==x}")
            trace.append(f"y==2q ? {y}==2*{q} → {2*q==y}")
            trace.append(f"x+y even? {(x+y)%2==0}")
            ok = (2*p==x) and (2*q==y) and ((x+y)%2==0)
        elif t=="gcd_linear_combination":
            a,b = d["a"], d["b"]; g,s,tv = W["g"], W["s"], W["t"]
            trace.append(f"g==gcd(a,b) ? {g}=={math.gcd(a,b)}")
            trace.append(f"s*a + t*b == g ? {s*a + tv*b} == {g}")
            ok = (g==math.gcd(a,b)) and (s*a + tv*b == g)
        elif t=="coprime_product_divides_implies_divides_factor":
            a,b,cnum = d["a"], d["b"], d["c"]; k = W.get("k", None)
            trace.append(f"gcd(a,b)==1 ? {math.gcd(a,b)}")
            trace.append(f"a|(b*c) ? remainder {(b*cnum)%a}")
            ok = (math.gcd(a,b)==1) and ((b*cnum)%a==0) and (cnum%a==0)
            # (We check the *conclusion* a|c computationally.)
        elif t=="mod_addition_compatible":
            m,a,a1,b,b1 = d["m"], d["a"], d["a_equiv"], d["b"], d["b_equiv"]
            r,s = W["r"], W["s"]
            trace.append(f"a−a'==m*r ? {a-a1}=={m}*{r} → {a-a1==m*r}")
            trace.append(f"b−b'==m*s ? {b-b1}=={m}*{s} → {b-b1==m*s}")
            lhs = (a+b - (a1+b1)) % m
            trace.append(f"(a+b)−(a'+b') divisible by m? remainder {lhs}")
            ok = (a-a1==m*r) and (b-b1==m*s) and (lhs==0)
        else:
            trace.append("Unknown type")
            ok=False
    except Exception as e:
        trace.append(f"Exception: {e}")
        ok=False
    return {"id": sk["id"], "type": t, "ok": ok, "trace": trace}

def check_all(sketches: Dict[str,Any]) -> Dict[str,Any]:
    verdicts = [check_one(s) for s in sketches["sketches"]]
    reasons = [f"{v['id']}: {'OK' if v['ok'] else 'FAIL'}" for v in verdicts]
    return {"verdicts": verdicts, "reasons": reasons}

### CHECK
def run_harness(res: Dict[str,Any]) -> None:
    print("Check — every verdict has id/type/ok/trace:")
    for v in res["verdicts"]:
        assert {"id","type","ok","trace"} <= set(v.keys())

def main():
    ap = argparse.ArgumentParser(description="Mechanically check proof sketches.")
    ap.add_argument("--infile", default="./bus/proof/proof_sketches.json")
    ap.add_argument("--out", default="./bus/proof/proof_verdicts.json")
    args = ap.parse_args()

    sketches = json.load(open(args.infile,"r",encoding="utf-8"))
    res = check_all(sketches)

    print("# ANSWER"); print(json.dumps({"ok_count": sum(v["ok"] for v in res["verdicts"]), "total": len(res["verdicts"])}, indent=2))
    print("\n# REASONS"); [print("-", r) for r in res["reasons"]]

    json.dump(res, open(args.out,"w",encoding="utf-8"), indent=2)
    print(f"\nWrote {args.out}")

    print("\n# CHECK"); run_harness(res); print("✔ All checks passed.")

if __name__ == "__main__":
    main()

