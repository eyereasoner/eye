#!/usr/bin/env python3
"""
P3 — Program 5: Final Report & TeX-ish Summary

Short story (header):
  Summarize which claims were proved, show short human sentences, and emit a
  tiny TeX-ish file you could paste into a note.

Overview:
  - Reads './bus/proof/normalized_claims.json' + './bus/proof/proof_verdicts.json'.
  - Emits './bus/proof/proof_report.json' and './bus/proof/proof_report.tex.txt'.
  - Prints Answer/Reason/Check.
"""
from __future__ import annotations
import argparse, json

def explain(claim):
    t, d = claim["type"], claim["data"]
    if t=="divisibility_transitive":
        return f"If {d['a']}|{d['b']} and {d['b']}|{d['c']}, then {d['a']}|{d['c']}."
    if t=="even_plus_even_is_even":
        return f"If {d['x']} and {d['y']} are even, then {d['x']}+{d['y']} is even."
    if t=="gcd_linear_combination":
        return f"There exist s,t with s·{d['a']} + t·{d['b']} = gcd({d['a']},{d['b']})."
    if t=="coprime_product_divides_implies_divides_factor":
        return f"If gcd({d['a']},{d['b']})=1 and {d['a']} divides {d['b']}·{d['c']}, then {d['a']} divides {d['c']}."
    if t=="mod_addition_compatible":
        return f"If {d['a']}≡{d['a_equiv']} (mod {d['m']}) and {d['b']}≡{d['b_equiv']} (mod {d['m']}), then sums are congruent."
    return "Claim."

def make_tex(report):
    lines = [r"\section*{Elementary Number Theory Proof Report}"]
    for item in report["items"]:
        status = r"\textbf{OK}" if item["ok"] else r"\textbf{FAIL}"
        lines.append(rf"\paragraph{{{item['id']}}} {item['text']} Status: {status}.")
    return "\n".join(lines)

def build_report(norm, verdicts):
    by_id = {c["id"]: c for c in norm["claims"]}
    items = []
    for v in verdicts["verdicts"]:
        c = by_id[v["id"]]
        items.append({"id": v["id"], "type": v["type"], "ok": bool(v["ok"]), "text": explain(c)})
    return {"items": items}

### CHECK
def run_harness(rep):
    print("Check — items non-empty, have id/type/ok/text:")
    assert len(rep["items"])>=1
    for it in rep["items"]:
        assert {"id","type","ok","text"} <= set(it.keys())

def main():
    ap = argparse.ArgumentParser(description="Summarize proof results and emit TeX-ish note.")
    ap.add_argument("--in_norm", default="./bus/proof/normalized_claims.json")
    ap.add_argument("--in_ver",  default="./bus/proof/proof_verdicts.json")
    args = ap.parse_args()

    norm = json.load(open(args.in_norm,"r",encoding="utf-8"))
    ver  = json.load(open(args.in_ver,"r",encoding="utf-8"))
    rep  = build_report(norm, ver)
    tex  = make_tex(rep)

    print("# ANSWER"); print(json.dumps({"ok_total": sum(1 for i in rep["items"] if i["ok"]), "total": len(rep["items"])}, indent=2))
    print("\n# REASONS"); [print("-", f"{i['id']}: {'OK' if i['ok'] else 'FAIL'}") for i in rep["items"]]

    json.dump(rep, open("./bus/proof/proof_report.json","w",encoding="utf-8"), indent=2)
    open("./bus/proof/proof_report.tex.txt","w",encoding="utf-8").write(tex)
    print("\nWrote ./bus/proof/proof_report.json and ./bus/proof/proof_report.tex.txt")

    print("\n# CHECK"); run_harness(rep); print("✔ All checks passed.")

if __name__ == "__main__":
    main()

