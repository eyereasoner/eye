from __future__ import annotations
"""
BeerAdvisor — EYE-style mixed computation (final)
=================================================
Static RDF + N3 intent → Agent (partial eval, Ershov) → specialized Driver → ranked beers.

One-liner: a compact EYE-style advisor that partially evaluates static style/price facts and
N3 rule intent into a specialized driver which consumes dynamic taste/context/constraints and
outputs a feasible, preference/price/context–aware ranking with explanation traces.

ASCII schematic
---------------
  ME (drinker) -------------------+
                                  |         +-------------------+
                                  |         |   Behaviors       |  (rule docs)
                                  |         |   (N3 templates)  |
                                  |         +-------------------+
                                  |                  ^    ^
                                  |                  |    |
                                  v                  |    | context (dynamic)
+-------------------+   AC    +----------+           |    |
| data (RDF, static)| <-----> | context  |-----------+    |
| styles, prices    | (access)+----------+                |
+-------------------+                                     v
                         Agent (partial evaluator / specialization)
                                     |
                                     v
                           Driver (specialized scorer)
                                     |
                                     v
             +-------------------------------------------------------------+
             | Targets / Capabilities / Context (beers & preferences)      | --> ranked beers
             +-------------------------------------------------------------+
                                     |
                                     v
                          actionable insight + feedback (trace)
"""

import os, csv, json, math
from typing import Any, Dict, List, Tuple

# -----------------------------------------------------------------------------
# Artifact directory
# -----------------------------------------------------------------------------
BASE = os.path.join(os.path.dirname(__file__), "output/beeradvisor_artifacts")
os.makedirs(BASE, exist_ok=True)

EX = "http://example.org/beer#"

# -----------------------------------------------------------------------------
# STATIC RDF (compile-time): catalog of beers + base prices
# IBU ~ bitterness (0-100), SRM ~ color (2-40), ABV %, price arbitrary currency
# -----------------------------------------------------------------------------
STATIC_TTL = """@prefix ex: <http://example.org/beer#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# Policy weights (sum ~ 1.0) for score = wTaste*tasteDistN + wPrice*priceN + wAbv*abvPenaltyN + wCtx*contextN
ex:policy ex:wTaste 0.55 .
ex:policy ex:wPrice 0.20 .
ex:policy ex:wAbv   0.15 .
ex:policy ex:wCtx   0.10 .

# Catalog (synthetic)
ex:IPA       a ex:Beer . ex:IPA       ex:style "IPA" .       ex:IBU 60 . ex:SRM 10 . ex:ABV 6.5 . ex:price 4.5 . ex:containsGluten true .
ex:Stout     a ex:Beer . ex:Stout     ex:style "Stout" .     ex:IBU 40 . ex:SRM 35 . ex:ABV 6.0 . ex:price 4.0 . ex:containsGluten true .
ex:Pilsner   a ex:Beer . ex:Pilsner   ex:style "Pilsner" .   ex:IBU 25 . ex:SRM 4  . ex:ABV 4.8 . ex:price 3.0 . ex:containsGluten true .
ex:Saison    a ex:Beer . ex:Saison    ex:style "Saison" .    ex:IBU 30 . ex:SRM 7  . ex:ABV 6.5 . ex:price 4.2 . ex:containsGluten true .
ex:Witbier   a ex:Beer . ex:Witbier   ex:style "Witbier" .   ex:IBU 12 . ex:SRM 3  . ex:ABV 5.0 . ex:price 3.8 . ex:containsGluten true .
ex:GF_Lager  a ex:Beer . ex:GF_Lager  ex:style "GF Lager" .  ex:IBU 18 . ex:SRM 5  . ex:ABV 4.5 . ex:price 3.6 . ex:containsGluten false . ex:glutenFree true .
ex:LowIPA    a ex:Beer . ex:LowIPA    ex:style "Session IPA" . ex:IBU 45 . ex:SRM 9 . ex:ABV 3.8 . ex:price 4.0 . ex:containsGluten true .

# Expose the catalog list (for convenience)
ex:catalog ex:hasBeer ex:IPA .
ex:catalog ex:hasBeer ex:Stout .
ex:catalog ex:hasBeer ex:Pilsner .
ex:catalog ex:hasBeer ex:Saison .
ex:catalog ex:hasBeer ex:Witbier .
ex:catalog ex:hasBeer ex:GF_Lager .
ex:catalog ex:hasBeer ex:LowIPA .
"""

# -----------------------------------------------------------------------------
# DYNAMIC RDF (run-time): user preferences + constraints + context
# - taste targets: IBU, SRM, ABV targets (soft)
# - hard bans: e.g., gluten
# - context: meal, weather (affects context penalty)
# -----------------------------------------------------------------------------
def make_dynamic_ttl() -> str:
    ln = [
        "@prefix ex: <http://example.org/beer#> .",
        "@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .",
        "",
        "# Preferences (targets are soft; distances are penalized)",
        "ex:me ex:targetIBU 30 .",
        "ex:me ex:targetSRM 8 .",
        "ex:me ex:targetABV 5.0 .",
        "",
        "# Hard constraints",
        "ex:me ex:banGluten true .",
        "ex:me ex:maxAbvHard 8.0 .",   # beyond this is infeasible
        "",
        "# Context",
        "ex:ctx ex:meal \"spicy\" .",
        "ex:ctx ex:weather \"hot\" .",
    ]
    return "\n".join(ln)

DYNAMIC_TTL = make_dynamic_ttl()

# -----------------------------------------------------------------------------
# Behaviors as N3 (valid triple-only math built-ins) — documentation mirror
# -----------------------------------------------------------------------------
RULES_N3 = """@prefix ex:   <http://example.org/beer#> .
@prefix math: <http://www.w3.org/2000/10/swap/math#> .
@prefix xsd:  <http://www.w3.org/2001/XMLSchema#> .

# Feasibility: gluten ban or hard ABV cap
{ ?b ex:containsGluten true . ex:me ex:banGluten true . } => { ?b ex:feasible false } .
{ ?b ex:ABV ?a . ex:me ex:maxAbvHard ?m . ?a math:greaterThan ?m . } => { ?b ex:feasible false } .

# Taste distance (driver computes normalization to [0..1]; N3 expresses intent)
# dist = sqrt( (IBUN-IBU_prefN)^2 + (SRMN-SRM_prefN)^2 + (ABVN-ABV_prefN)^2 )
# Score = wTaste*distN + wPrice*priceN + wAbv*abvPenaltyN + wCtx*contextN
{ ?b ex:distN ?dN .
  ?b ex:priceN ?pN .
  ?b ex:abvPenaltyN ?aN .
  ?b ex:contextN ?cN .
  ex:policy ex:wTaste ?wT .
  ex:policy ex:wPrice ?wP .
  ex:policy ex:wAbv   ?wA .
  ex:policy ex:wCtx   ?wC .
  ( ?wT ?dN ) math:product ?tTerm .
  ( ?wP ?pN ) math:product ?pTerm .
  ( ?wA ?aN ) math:product ?aTerm .
  ( ?wC ?cN ) math:product ?cTerm .
  ( ?tTerm ?pTerm ) math:sum ?tp .
  ( ?aTerm ?cTerm ) math:sum ?ac .
  ( ?tp ?ac ) math:sum ?score .
} => { ?b ex:score ?score } .
"""

def _write(path: str, content: str):
    with open(path, "w", encoding="utf-8") as f:
        f.write(content)

_write(os.path.join(BASE, "static.ttl"), STATIC_TTL)
_write(os.path.join(BASE, "dynamic.ttl"), DYNAMIC_TTL)
_write(os.path.join(BASE, "rules.n3"),   RULES_N3)

# -----------------------------------------------------------------------------
# Tiny Turtle Reader (robust to inline '#' and '#' inside IRIs; supports multi-triple lines)
# -----------------------------------------------------------------------------
def parse_ttl(ttl_text: str):
    prefixes, triples = {}, []

    def strip_inline_comments(line: str) -> str:
        in_quotes = False
        in_angle = False
        out = []
        for ch in line.rstrip():
            if ch == '"' and not in_angle:
                in_quotes = not in_quotes
            elif ch == '<' and not in_quotes:
                in_angle = True
            elif ch == '>' and not in_quotes:
                in_angle = False
            if ch == '#' and not in_quotes and not in_angle:
                break
            out.append(ch)
        return "".join(out).strip()

    def split_statements(line: str) -> List[str]:
        stmts, buf = [], []
        in_quotes = False
        in_angle = False
        for ch in line:
            if ch == '"' and not in_angle:
                in_quotes = not in_quotes; buf.append(ch); continue
            if ch == '<' and not in_quotes:
                in_angle = True; buf.append(ch); continue
            if ch == '>' and not in_quotes:
                in_angle = False; buf.append(ch); continue
            if ch == '.' and not in_quotes and not in_angle:
                stmts.append("".join(buf).strip()); buf = []
                continue
            buf.append(ch)
        if buf and "".join(buf).strip():
            stmts.append("".join(buf).strip())
        return [s for s in stmts if s]

    for raw in ttl_text.splitlines():
        line = strip_inline_comments(raw)
        if not line:
            continue

        if line.startswith("@prefix"):
            parts = line.split()
            if len(parts) >= 3:
                prefixes[parts[1].rstrip(":")] = parts[2].strip("<>")
            continue

        for stmt in split_statements(line):
            toks, buf = [], ""
            in_quotes = False
            for ch in stmt.strip():
                if ch == '"':
                    in_quotes = not in_quotes; buf += ch
                elif ch == ' ' and not in_quotes:
                    if buf: toks.append(buf); buf = ""
                else:
                    buf += ch
            if buf: toks.append(buf)
            if len(toks) < 3:
                continue

            s, p, o = toks[0], toks[1], " ".join(toks[2:])

            def expand(t: str) -> str:
                if t.startswith("<") and t.endswith(">"):
                    return t[1:-1]
                if ":" in t and not t.startswith('"'):
                    pref, local = t.split(":", 1)
                    return prefixes.get(pref, pref + ":") + local
                return t

            s_e, p_e = expand(s), expand(p)

            if o.startswith('"') and o.endswith('"'):
                obj: Any = o.strip('"')
            elif o in ("true", "false"):
                obj = (o == "true")
            else:
                try:
                    obj = float(o) if "." in o else int(o)
                except Exception:
                    obj = expand(o)

            triples.append((s_e, p_e, obj))
    return prefixes, triples

def index_triples(triples: List[Tuple[str,str,Any]]):
    idx: Dict[str, Dict[str, List[Any]]] = {}
    for s,p,o in triples:
        idx.setdefault(s, {}).setdefault(p, []).append(o)
    return idx

def get1(idx: Dict[str, Dict[str, List[Any]]], s: str, p: str, default=None):
    vals = idx.get(s, {}).get(p, [])
    return vals[0] if vals else default

# Load graphs
with open(os.path.join(BASE, "static.ttl"), encoding="utf-8") as f:
    _, S_tr = parse_ttl(f.read())
with open(os.path.join(BASE, "dynamic.ttl"), encoding="utf-8") as f:
    _, D_tr = parse_ttl(f.read())

S = index_triples(S_tr)
D = index_triples(D_tr)

# -----------------------------------------------------------------------------
# Agent → Driver (Ershov mixed computation)
# -----------------------------------------------------------------------------
def norm(xs: List[float]) -> List[float]:
    lo, hi = min(xs), max(xs)
    if hi - lo < 1e-9:
        return [0.0]*len(xs)
    return [(x-lo)/(hi-lo) for x in xs]

def clamp01(x: float) -> float:
    return max(0.0, min(1.0, x))

def make_driver(S):
    # Static policy weights
    wT = float(get1(S, EX+"policy", EX+"wTaste", 0.55))
    wP = float(get1(S, EX+"policy", EX+"wPrice", 0.20))
    wA = float(get1(S, EX+"policy", EX+"wAbv",   0.15))
    wC = float(get1(S, EX+"policy", EX+"wCtx",   0.10))

    # Catalog (list of beers)
    catalog = [b for b in S.get(EX+"catalog", {}).get(EX+"hasBeer", [])]

    # Feature domains (for normalization to [0..1])
    def domains():
        ibus = [float(get1(S, b, EX+"IBU", 0.0)) for b in catalog]
        srms = [float(get1(S, b, EX+"SRM", 2.0)) for b in catalog]
        abvs = [float(get1(S, b, EX+"ABV", 0.0)) for b in catalog]
        prices = [float(get1(S, b, EX+"price", 0.0)) for b in catalog]
        return (min(ibus), max(ibus)), (min(srms), max(srms)), (min(abvs), max(abvs)), (min(prices), max(prices))

    (IBU_lo, IBU_hi), (SRM_lo, SRM_hi), (ABV_lo, ABV_hi), (P_lo, P_hi) = domains()

    def to_N(x, lo, hi):
        if hi - lo < 1e-9:
            return 0.0
        return clamp01((x - lo) / (hi - lo))

    def score_and_rank(D):
        # Preferences & context
        pref_ibu = float(get1(D, EX+"me", EX+"targetIBU", 30))
        pref_srm = float(get1(D, EX+"me", EX+"targetSRM", 8))
        pref_abv = float(get1(D, EX+"me", EX+"targetABV", 5.0))
        ban_gluten = bool(get1(D, EX+"me", EX+"banGluten", False))
        max_abv_hard = float(get1(D, EX+"me", EX+"maxAbvHard", 100))
        meal = str(get1(D, EX+"ctx", EX+"meal", "neutral"))
        weather = str(get1(D, EX+"ctx", EX+"weather", "mild"))

        assert catalog, "No beers in the static catalog"

        # Normalize preference targets to same [0..1] as features
        pref_ibuN = to_N(pref_ibu, IBU_lo, IBU_hi)
        pref_srmN = to_N(pref_srm, SRM_lo, SRM_hi)
        pref_abvN = to_N(pref_abv, ABV_lo, ABV_hi)

        def infeasible(b: str) -> bool:
            # Gluten ban
            if ban_gluten and bool(get1(S, b, EX+"containsGluten", False)) and not bool(get1(S, b, EX+"glutenFree", False)):
                return True
            # Hard ABV limit
            if float(get1(S, b, EX+"ABV", 0.0)) > max_abv_hard:
                return True
            return False

        # Compute metrics per beer
        metrics = {}
        for b in catalog:
            IBU  = float(get1(S, b, EX+"IBU", 0.0))
            SRM  = float(get1(S, b, EX+"SRM", 2.0))
            ABV  = float(get1(S, b, EX+"ABV", 0.0))
            PRICE= float(get1(S, b, EX+"price", 0.0))
            ibuN = to_N(IBU, IBU_lo, IBU_hi)
            srmN = to_N(SRM, SRM_lo, SRM_hi)
            abvN = to_N(ABV, ABV_lo, ABV_hi)
            priceN = to_N(PRICE, P_lo, P_hi)

            # Taste distance (Euclidean in normalized space, then normalize by max possible sqrt(3))
            d = math.sqrt((ibuN - pref_ibuN)**2 + (srmN - pref_srmN)**2 + (abvN - pref_abvN)**2)
            distN = clamp01(d / math.sqrt(3.0))

            # ABV penalty (soft): deviation from preferred ABV (normalized absolute diff)
            abvPenaltyN = abs(abvN - pref_abvN)

            # Context penalty: simple heuristics
            ctx_pen = 0.0
            if weather.lower() == "hot":
                # prefer lower ABV, higher IBU crispness → penalize >6.5% and very dark SRM
                if ABV > 6.5: ctx_pen += 0.4
                if SRM > 20:  ctx_pen += 0.2
            if meal.lower() == "spicy":
                # prefer crisper/refreshing: penalize very sweet/dark (high SRM) a bit
                if SRM > 15:  ctx_pen += 0.2
            contextN = clamp01(ctx_pen)

            metrics[b] = {
                "beer": b, "style": get1(S, b, EX+"style", "Beer"),
                "feasible": (not infeasible(b)),
                "IBU": IBU, "SRM": SRM, "ABV": ABV, "price": PRICE,
                "ibuN": ibuN, "srmN": srmN, "abvN": abvN, "priceN": priceN,
                "distN": distN, "abvPenaltyN": abvPenaltyN, "contextN": contextN,
                "containsGluten": bool(get1(S, b, EX+"containsGluten", True)),
                "glutenFree": bool(get1(S, b, EX+"glutenFree", False)),
                "pref": {"ibuN": pref_ibuN, "srmN": pref_srmN, "abvN": pref_abvN},
                "ctx": {"meal": meal, "weather": weather}
            }

        # Normalize price across feasible only (so a single super expensive infeasible item won't skew)
        feas_beers = [b for b in catalog if metrics[b]["feasible"]]
        assert feas_beers, "No feasible beers given current bans/limits."
        # Re-normalize priceN on feasible set
        pvals = [metrics[b]["price"] for b in feas_beers]
        Plo, Phi = min(pvals), max(pvals)
        for b in catalog:
            metrics[b]["priceN"] = to_N(metrics[b]["price"], Plo, Phi)

        # Compose score
        results = []
        for b in catalog:
            m = metrics[b]
            if not m["feasible"]:
                score = float("+inf")
                note = "infeasible (ban or hard ABV cap)"
            else:
                score = (wT*m["distN"] + wP*m["priceN"] + wA*m["abvPenaltyN"] + wC*m["contextN"])
                reasons = []
                # Taste alignment clues
                if m["distN"] <= 0.15: reasons.append("close to taste target")
                elif m["distN"] <= 0.30: reasons.append("near taste target")
                # Price
                if m["priceN"] <= 0.2: reasons.append("good price")
                # ABV closeness
                if m["abvPenaltyN"] <= 0.15: reasons.append("ABV near preference")
                # Context
                if m["contextN"] <= 0.05: reasons.append("fits context")
                if m["glutenFree"] and bool(get1(D, EX+"me", EX+"banGluten", False)):
                    reasons.append("gluten-free")
                note = "; ".join(reasons)
            results.append({**m, "score": score, "note": note})

        ranked = sorted(results, key=lambda r: r["score"])
        weights = {"wTaste": wT, "wPrice": wP, "wAbv": wA, "wCtx": wC}
        return ranked, results, weights

    return score_and_rank

# Build specialized driver
driver = make_driver(S)

# -----------------------------------------------------------------------------
# Execute
# -----------------------------------------------------------------------------
ranked, all_results, weights = driver(D)

# Basic checks
feasible = [r for r in ranked if r["feasible"]]
assert feasible, "No feasible beers after scoring."

best = feasible[0]

# Monotonicity wrt price weight: increasing wPrice should not pick a MORE expensive best beer
def rerun_with_price_bonus(delta=0.20):
    S_mod = {k:{kk:list(vv) for kk,vv in props.items()} for k, props in S.items()}
    S_mod[EX+"policy"][EX+"wPrice"] = [min(0.99, weights["wPrice"] + delta)]
    # Renormalize the other weights proportionally (sum ~ 1.0)
    remain = 1.0 - S_mod[EX+"policy"][EX+"wPrice"][0]
    others = [weights["wTaste"], weights["wAbv"], weights["wCtx"]]
    tot = sum(others)
    scale = remain / tot if tot > 0 else 0.0
    S_mod[EX+"policy"][EX+"wTaste"] = [weights["wTaste"] * scale]
    S_mod[EX+"policy"][EX+"wAbv"]   = [weights["wAbv"]   * scale]
    S_mod[EX+"policy"][EX+"wCtx"]   = [weights["wCtx"]   * scale]
    ranked2, _, _ = make_driver(S_mod)(D)
    best2 = [r for r in ranked2 if r["feasible"]][0]
    return best2

best_after = rerun_with_price_bonus(0.20)
assert best_after["price"] <= best["price"] + 1e-9, \
    "Increasing price weight should not produce a more expensive chosen beer."

# -----------------------------------------------------------------------------
# Emit beers.csv (score breakdown) + machine- and human-readable traces
# -----------------------------------------------------------------------------
csv_path = os.path.join(BASE, "beers.csv")
with open(csv_path, "w", newline="", encoding="utf-8") as f:
    w = csv.writer(f)
    w.writerow([
        "rank","beer","style","feasible",
        "IBU","SRM","ABV","price",
        "distN","priceN","abvPenaltyN","contextN","score","note"
    ])
    for i,r in enumerate(ranked, start=1):
        w.writerow([
            i, r["beer"].split("#")[-1], r["style"], r["feasible"],
            int(r["IBU"]), int(r["SRM"]), f"{r['ABV']:.1f}", f"{r['price']:.2f}",
            (None if math.isinf(r["score"]) else f"{r['distN']:.3f}"),
            (None if math.isinf(r["score"]) else f"{r['priceN']:.3f}"),
            (None if math.isinf(r["score"]) else f"{r['abvPenaltyN']:.3f}"),
            (None if math.isinf(r["score"]) else f"{r['contextN']:.3f}"),
            (None if math.isinf(r["score"]) else f"{r['score']:.3f}"),
            r["note"],
        ])

# Machine-readable trace (JSON)
trace = {
    "weights": weights,
    "chosen": best["beer"],
    "preferences": all_results[0]["pref"] if all_results else {},
    "context": all_results[0]["ctx"] if all_results else {},
    "beers": [
        {
            "beer": r["beer"],
            "style": r["style"],
            "feasible": r["feasible"],
            "IBU": r["IBU"], "SRM": r["SRM"], "ABV": r["ABV"], "price": r["price"],
            "distN": (None if math.isinf(r["score"]) else r["distN"]),
            "priceN": (None if math.isinf(r["score"]) else r["priceN"]),
            "abvPenaltyN": (None if math.isinf(r["score"]) else r["abvPenaltyN"]),
            "contextN": (None if math.isinf(r["score"]) else r["contextN"]),
            "score": (None if math.isinf(r["score"]) else r["score"]),
            "glutenFree": r["glutenFree"],
            "containsGluten": r["containsGluten"],
            "note": r["note"]
        } for r in ranked
    ]
}
with open(os.path.join(BASE, "trace.json"), "w", encoding="utf-8") as jf:
    json.dump(trace, jf, indent=2)

# Human-readable “Reason why”
with open(os.path.join(BASE, "reason-why.txt"), "w", encoding="utf-8") as tf:
    tf.write("Reason why / explanation summary — BeerAdvisor\n")
    tf.write("--------------------------------------------\n")
    tf.write(f"Weights: wTaste={weights['wTaste']}, wPrice={weights['wPrice']}, "
             f"wAbv={weights['wAbv']}, wCtx={weights['wCtx']}\n")
    tf.write(f"Chosen beer: {best['beer']} ({best['style']})\n")
    tf.write(f"IBU/SRM/ABV/Price: {int(best['IBU'])}/{int(best['SRM'])}/{best['ABV']:.1f}%/€{best['price']:.2f}\n")
    if not math.isinf(best["score"]):
        tf.write(f"Components — distN={best['distN']:.3f}, priceN={best['priceN']:.3f}, "
                 f"abvPenaltyN={best['abvPenaltyN']:.3f}, contextN={best['contextN']:.3f}, "
                 f"score={best['score']:.3f}\n")
    tf.write(f"Notes: {best['note']}\n\n")
    tf.write("Top feasible beers:\n")
    rank = 1
    for r in [x for x in ranked if x["feasible"]][:5]:
        tf.write(f"  {rank}. {r['beer']}  score={r['score']:.3f}  "
                 f"IBU={int(r['IBU'])}  SRM={int(r['SRM'])}  ABV={r['ABV']:.1f}%  price=€{r['price']:.2f}\n")
        rank += 1

print("ALL TESTS PASSED")
print("Artifacts in:", BASE)
print("CSV:", csv_path)

