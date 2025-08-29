
from __future__ import annotations
"""
Beer Advisor — EYE-style mixed computation (self-contained + self-checking)
===========================================================================

  ME (you) ---------------------------+
                                     |        +-------------------+
                                     |        |   Behaviors       |  (policy weights,
                                     |        |   (rule docs)     |   pairing heuristics)
                                     |        +-------------------+
                                     |                 ^    ^
                                     |                 |    |
                                     v                 |    | context (dynamic)
+-------------+     AC           +----------+          |    |
| data        | <--------------> | context  |----------+    |
| (RDF)       |   (access)       +----------+               |
+-------------+                                            v
                             Agent (partial evaluator / specialization)
                                        |
                                        v
                              Driver (specialized scoring function)
                                        |
                                        v
            +---------------------------------------------------------+
            | Targets / Capabilities / Context (beer candidates list) | --> ranked beers
            +---------------------------------------------------------+
                                        |
                                        v
                              actionable insight + feedback (trace)

Legend
------
- data (RDF, static): style trait vectors, default weights, availability bonus,
  pairing weight templates. Think: long-lived knowledge.
- context (RDF, dynamic): your taste prefs, constraints (vegan/lactose/gluten),
  ABV range, budget, weather (hot?), meal type; and the beer candidates.
- behaviors (N3 docs): human-readable rule templates (e.g., hot→crisp bonus,
  spicy meal→hoppy+crisp bonus, constraint rejections). We *document* them as N3
  but implement them in Python for this demo.
- agent: closes over the static policy + style maps to create a specialized
  scorer — this is Ershov mixed computation (partial evaluation).
- driver: the specialized scoring function that consumes only per-beer facts.
- targets/capabilities: the actual beers you can choose from.
- actionable insight: ranked list with an explanation trace for each beer.

USAGE
-----
    python beeradvisor_mixed.py

OUTPUTS
-------
A folder 'beeradvisor_artifacts' alongside this script, containing:
    - static.ttl          (static data: style trait maps + default policy)
    - dynamic.ttl         (dynamic context: prefs, constraints, beers)
    - rules.n3            (N3 rule documentation of the intended behaviors)
    - ranked_beers.csv    (final ranking with trace columns)

Notes
-----
- Tiny Turtle reader supports: prefix lines, simple triples terminated by '.', quoted
  strings, numbers, booleans, and prefix expansion. It ignores full Turtle features
  (no semicolons, lists, etc.), so the TTL blocks below stick to a minimal subset.
"""

import os, csv
from typing import Any, Dict, List, Tuple

# ----------------------------------------------------------------------------
# Shared artifact directory (like a small "knowledge store")
# ----------------------------------------------------------------------------
BASE = os.path.join(os.path.dirname(__file__), "output/beeradvisor_artifacts")
os.makedirs(BASE, exist_ok=True)

EX = "http://example.org/beer#"

# ----------------------------------------------------------------------------
# STATIC RDF (compile-time knowledge)
# - Style trait vectors: hoppy/malty/sour/crisp in [0..1]
# - Policy weights and bonuses
# Keep to simple S P O '.' form; comment lines (starting with '#') are allowed.
# ----------------------------------------------------------------------------
STATIC_TTL = """@prefix ex: <http://example.org/beer#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# Policy weights (defaults)
ex:policy ex:wHoppy 1.0 .
ex:policy ex:wMalty 0.7 .
ex:policy ex:wSour  0.6 .
ex:policy ex:wCrisp 0.8 .
ex:policy ex:wABVPenalty 0.5 .
ex:policy ex:wPricePenalty 0.4 .
ex:policy ex:availabilityBonus 0.2 .

# Food pairing templates (how meal nudges traits)
ex:pairingSpicy ex:hoppy 0.6 .
ex:pairingSpicy ex:crisp 0.4 .
ex:pairingDessert ex:malty 0.7 .
ex:pairingSalad ex:sour 0.6 .

# Style trait vectors
ex:pilsner ex:hoppy 0.3 .
ex:pilsner ex:malty 0.3 .
ex:pilsner ex:sour  0.0 .
ex:pilsner ex:crisp 0.9 .

ex:pale_ale ex:hoppy 0.7 .
ex:pale_ale ex:malty 0.4 .
ex:pale_ale ex:sour  0.0 .
ex:pale_ale ex:crisp 0.5 .

ex:ipa ex:hoppy 0.9 .
ex:ipa ex:malty 0.4 .
ex:ipa ex:sour  0.0 .
ex:ipa ex:crisp 0.4 .

ex:neipa ex:hoppy 0.85 .
ex:neipa ex:malty 0.5 .
ex:neipa ex:sour  0.0 .
ex:neipa ex:crisp 0.3 .

ex:milk_stout ex:hoppy 0.2 .
ex:milk_stout ex:malty 0.9 .
ex:milk_stout ex:sour  0.0 .
ex:milk_stout ex:crisp 0.1 .

ex:porter ex:hoppy 0.3 .
ex:porter ex:malty 0.8 .
ex:porter ex:sour  0.0 .
ex:porter ex:crisp 0.2 .

ex:gose ex:hoppy 0.1 .
ex:gose ex:malty 0.2 .
ex:gose ex:sour  0.9 .
ex:gose ex:crisp 0.6 .

ex:hefeweizen ex:hoppy 0.2 .
ex:hefeweizen ex:malty 0.4 .
ex:hefeweizen ex:sour  0.1 .
ex:hefeweizen ex:crisp 0.7 .

ex:tripel ex:hoppy 0.5 .
ex:tripel ex:malty 0.7 .
ex:tripel ex:sour  0.0 .
ex:tripel ex:crisp 0.4 .
"""

# ----------------------------------------------------------------------------
# DYNAMIC RDF (run-time knowledge)
# - Your taste prefs, constraints, budget, ABV range
# - Weather + meal context
# - Beer candidates (targets/capabilities)
# ----------------------------------------------------------------------------
DYNAMIC_TTL = """@prefix ex: <http://example.org/beer#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# ME: prefs & constraints
ex:me ex:prefHoppy 0.8 .
ex:me ex:prefMalty 0.4 .
ex:me ex:prefSour  0.2 .
ex:me ex:prefABVmin 4.0 .
ex:me ex:prefABVmax 6.5 .
ex:me ex:budgetMax 6.0 .
ex:me ex:wantsVegan true .
ex:me ex:wantsLactoseFree true .
ex:me ex:wantsGlutenFree false .

# Context
ex:today ex:isHot true .
ex:meal ex:type "spicy" .

# Beer candidates
ex:beer_pils ex:label "Pilsner Urquell" .
ex:beer_pils ex:style ex:pilsner .
ex:beer_pils ex:abv 4.4 .
ex:beer_pils ex:ibu 35 .
ex:beer_pils ex:price 4.5 .
ex:beer_pils ex:availability "wide" .
ex:beer_pils ex:lactose false .
ex:beer_pils ex:vegan true .
ex:beer_pils ex:glutenFree false .

ex:beer_pale ex:label "Sierra Nevada Pale Ale" .
ex:beer_pale ex:style ex:pale_ale .
ex:beer_pale ex:abv 5.6 .
ex:beer_pale ex:ibu 38 .
ex:beer_pale ex:price 4.0 .
ex:beer_pale ex:availability "wide" .
ex:beer_pale ex:lactose false .
ex:beer_pale ex:vegan true .
ex:beer_pale ex:glutenFree false .

ex:beer_neipa ex:label "Hazy NEIPA" .
ex:beer_neipa ex:style ex:neipa .
ex:beer_neipa ex:abv 6.4 .
ex:beer_neipa ex:ibu 50 .
ex:beer_neipa ex:price 6.5 .
ex:beer_neipa ex:availability "seasonal" .
ex:beer_neipa ex:lactose false .
ex:beer_neipa ex:vegan true .
ex:beer_neipa ex:glutenFree false .

ex:beer_milkstout ex:label "Milk Stout" .
ex:beer_milkstout ex:style ex:milk_stout .
ex:beer_milkstout ex:abv 6.0 .
ex:beer_milkstout ex:ibu 25 .
ex:beer_milkstout ex:price 5.0 .
ex:beer_milkstout ex:availability "wide" .
ex:beer_milkstout ex:lactose true .
ex:beer_milkstout ex:vegan false .
ex:beer_milkstout ex:glutenFree false .

ex:beer_porter ex:label "Robust Porter" .
ex:beer_porter ex:style ex:porter .
ex:beer_porter ex:abv 5.6 .
ex:beer_porter ex:ibu 35 .
ex:beer_porter ex:price 4.8 .
ex:beer_porter ex:availability "wide" .
ex:beer_porter ex:lactose false .
ex:beer_porter ex:vegan true .
ex:beer_porter ex:glutenFree false .

ex:beer_gose ex:label "Lemon Gose" .
ex:beer_gose ex:style ex:gose .
ex:beer_gose ex:abv 4.2 .
ex:beer_gose ex:ibu 10 .
ex:beer_gose ex:price 5.5 .
ex:beer_gose ex:availability "limited" .
ex:beer_gose ex:lactose false .
ex:beer_gose ex:vegan true .
ex:beer_gose ex:glutenFree false .

ex:beer_hefe ex:label "Hefeweizen" .
ex:beer_hefe ex:style ex:hefeweizen .
ex:beer_hefe ex:abv 5.2 .
ex:beer_hefe ex:ibu 18 .
ex:beer_hefe ex:price 4.2 .
ex:beer_hefe ex:availability "wide" .
ex:beer_hefe ex:lactose false .
ex:beer_hefe ex:vegan true .
ex:beer_hefe ex:glutenFree false .

ex:beer_tripel ex:label "Belgian Tripel" .
ex:beer_tripel ex:style ex:tripel .
ex:beer_tripel ex:abv 9.0 .
ex:beer_tripel ex:ibu 30 .
ex:beer_tripel ex:price 7.0 .
ex:beer_tripel ex:availability "seasonal" .
ex:beer_tripel ex:lactose false .
ex:beer_tripel ex:vegan true .
ex:beer_tripel ex:glutenFree false .

ex:beer_gf_lager ex:label "Gluten-Free Lager" .
ex:beer_gf_lager ex:style ex:pilsner .
ex:beer_gf_lager ex:abv 4.5 .
ex:beer_gf_lager ex:ibu 18 .
ex:beer_gf_lager ex:price 5.0 .
ex:beer_gf_lager ex:availability "limited" .
ex:beer_gf_lager ex:lactose false .
ex:beer_gf_lager ex:vegan true .
ex:beer_gf_lager ex:glutenFree true .
"""

# ----------------------------------------------------------------------------
# Behaviors documented in N3 (declarative mirror of what Driver enforces)
# ----------------------------------------------------------------------------
RULES_N3 = """@prefix ex: <http://example.org/beer#> .
# If hot -> add bonus proportional to style crispness
{ ex:today ex:isHot true . ?b ex:style ?s . ?s ex:crisp ?c . } => { ?b ex:hotBonus ?c } .
# If meal is spicy -> add bonus proportional to style hoppy and crisp
{ ex:meal ex:type "spicy" . ?b ex:style ?s . ?s ex:hoppy ?h . } => { ?b ex:spicyBonusH ?h } .
{ ex:meal ex:type "spicy" . ?b ex:style ?s . ?s ex:crisp ?c . } => { ?b ex:spicyBonusC ?c } .
# Dietary constraints -> rejections
{ ex:me ex:wantsLactoseFree true . ?b ex:lactose true } => { ?b ex:reject true } .
{ ex:me ex:wantsVegan true . ?b ex:vegan false } => { ?b ex:reject true } .
{ ex:me ex:wantsGlutenFree true . ?b ex:glutenFree false } => { ?b ex:reject true } .
"""

def _write(name: str, content: str):
    path = os.path.join(BASE, name)
    with open(path, "w", encoding="utf-8") as f:
        f.write(content)
    return path

_write("static.ttl", STATIC_TTL)
_write("dynamic.ttl", DYNAMIC_TTL)
_write("rules.n3", RULES_N3)

# ----------------------------------------------------------------------------
# Tiny Turtle reader (same subset as other demos)
# ----------------------------------------------------------------------------
def parse_ttl(ttl_text: str):
    prefixes, triples = {}, []
    for raw in ttl_text.splitlines():
        line = raw.strip()
        if not line or line.startswith("#"): 
            continue
        if line.startswith("@prefix"):
            parts = line.split()
            prefixes[parts[1].rstrip(":")] = parts[2].strip("<>")
            continue
        if line.endswith("."): 
            line = line[:-1].strip()
        toks, buf, q = [], "", False
        for ch in line:
            if ch == '"':
                q = not q; buf += ch; continue
            if ch == ' ' and not q:
                if buf: toks.append(buf); buf = ''
            else:
                buf += ch
        if buf: toks.append(buf)
        if len(toks) < 3: continue
        s, p, o = toks[0], toks[1], ' '.join(toks[2:])
        def expand(t):
            if t.startswith('<') and t.endswith('>'): return t[1:-1]
            if ':' in t and not t.startswith('"'):
                pref, local = t.split(':', 1)
                return prefixes.get(pref, pref + ':') + local
            return t
        s_e, p_e = expand(s), expand(p)
        if o.startswith('"') and o.endswith('"'):
            obj = o.strip('"')
        elif o in ('true','false'):
            obj = (o == 'true')
        else:
            try:
                obj = float(o) if '.' in o else int(o)
            except Exception:
                obj = expand(o)
        triples.append((s_e, p_e, obj))
    return prefixes, triples

def index_triples(triples):
    idx = {}
    for s,p,o in triples:
        idx.setdefault(s, {}).setdefault(p, []).append(o)
    return idx

def get1(idx, s, p, default=None):
    vals = idx.get(s, {}).get(p, [])
    return vals[0] if vals else default

# Load graphs (Agent reads Data + Context)
with open(os.path.join(BASE, "static.ttl"), encoding="utf-8") as f:
    _, S_triples = parse_ttl(f.read())
with open(os.path.join(BASE, "dynamic.ttl"), encoding="utf-8") as f:
    _, D_triples = parse_ttl(f.read())

S = index_triples(S_triples)
D = index_triples(D_triples)

# ----------------------------------------------------------------------------
# AGENT -> DRIVER (Ershov mixed computation)
# Close over static constants to create a specialized beer scorer.
# ----------------------------------------------------------------------------
def trait(style, name, default=0.0):
    return float(get1(S, style, EX+name, default))

# Build style trait cache (static)
STYLE_TRAITS = {}
for s in S.keys():
    if s.startswith(EX) and any(k in S[s] for k in (EX+"hoppy", EX+"malty", EX+"sour", EX+"crisp")):
        STYLE_TRAITS[s] = {
            "hoppy": trait(s, "hoppy", 0.0),
            "malty": trait(s, "malty", 0.0),
            "sour":  trait(s, "sour",  0.0),
            "crisp": trait(s, "crisp", 0.0),
        }

# Policy constants (static)
POLICY = {
    "wHoppy": float(get1(S, EX+"policy", EX+"wHoppy", 1.0)),
    "wMalty": float(get1(S, EX+"policy", EX+"wMalty", 0.7)),
    "wSour":  float(get1(S, EX+"policy", EX+"wSour",  0.6)),
    "wCrisp": float(get1(S, EX+"policy", EX+"wCrisp", 0.8)),
    "wABVPenalty": float(get1(S, EX+"policy", EX+"wABVPenalty", 0.5)),
    "wPricePenalty": float(get1(S, EX+"policy", EX+"wPricePenalty", 0.4)),
    "availabilityBonus": float(get1(S, EX+"policy", EX+"availabilityBonus", 0.2)),
    "pairingSpicy": {"hoppy": float(get1(S, EX+"pairingSpicy", EX+"hoppy", 0.6)),
                     "crisp": float(get1(S, EX+"pairingSpicy", EX+"crisp", 0.4))},
    "pairingDessert": {"malty": float(get1(S, EX+"pairingDessert", EX+"malty", 0.7))},
    "pairingSalad": {"sour": float(get1(S, EX+"pairingSalad", EX+"sour", 0.6))},
}

# Dynamic preferences/context
PREFS = {
    "prefHoppy": float(get1(D, EX+"me", EX+"prefHoppy", 0.5)),
    "prefMalty": float(get1(D, EX+"me", EX+"prefMalty", 0.5)),
    "prefSour":  float(get1(D, EX+"me", EX+"prefSour",  0.5)),
    "abvMin": float(get1(D, EX+"me", EX+"prefABVmin", 4.0)),
    "abvMax": float(get1(D, EX+"me", EX+"prefABVmax", 6.5)),
    "budgetMax": float(get1(D, EX+"me", EX+"budgetMax", 6.0)),
    "wantsVegan": bool(get1(D, EX+"me", EX+"wantsVegan", False)),
    "wantsLactoseFree": bool(get1(D, EX+"me", EX+"wantsLactoseFree", False)),
    "wantsGlutenFree": bool(get1(D, EX+"me", EX+"wantsGlutenFree", False)),
}
CONTEXT = {
    "isHot": bool(get1(D, EX+"today", EX+"isHot", False)),
    "mealType": get1(D, EX+"meal", EX+"type", "none"),
}

def make_beer_driver(policy, style_traits, prefs, context):
    """Return a scorer that only needs per-beer facts at run time."""
    wH, wM, wS, wC = policy["wHoppy"], policy["wMalty"], policy["wSour"], policy["wCrisp"]
    wABV, wPrice = policy["wABVPenalty"], policy["wPricePenalty"]
    avail_bonus = policy["availabilityBonus"]
    meal = context["mealType"]
    is_hot = context["isHot"]
    meal_weights = policy.get("pairing"+meal.capitalize(), {}) if meal != "none" else {}

    def traits_for(style_uri: str):
        return style_traits.get(style_uri, {"hoppy":0,"malty":0,"sour":0,"crisp":0})

    def reject_due_to_constraints(b: Dict[str,Any], why: List[str]) -> bool:
        # Hard constraints derived from context/preferences
        if prefs["wantsLactoseFree"] and b.get("lactose", False):
            why.append("reject:lactose"); return True
        if prefs["wantsVegan"] and not b.get("vegan", True):
            why.append("reject:not_vegan"); return True
        if prefs["wantsGlutenFree"] and not b.get("glutenFree", False):
            why.append("reject:gluten"); return True
        return False

    def score_beer(b: Dict[str,Any]) -> Tuple[float, List[str]]:
        why = []
        if reject_due_to_constraints(b, why):
            return -1e9, why

        st = traits_for(b["style"])

        # 1) Base taste alignment (H/M/S weighted by your preferences)
        taste = (wH*prefs["prefHoppy"]*st["hoppy"] +
                 wM*prefs["prefMalty"]*st["malty"] +
                 wS*prefs["prefSour"] *st["sour"])
        why.append(f"taste={taste:.2f} (H{st['hoppy']:.2f} M{st['malty']:.2f} S{st['sour']:.2f})")

        # 2) Context bonuses
        if is_hot:
            crisp_bonus = wC*st["crisp"]
            taste += crisp_bonus
            why.append(f"hot+crisp=+{crisp_bonus:.2f}")
        if meal_weights:
            meal_bump = sum(st.get(k,0.0)*v for k,v in meal_weights.items())
            taste += meal_bump
            why.append(f"meal({meal})=+{meal_bump:.2f}")

        # 3) ABV & price penalties
        abv = b["abv"]
        abv_pen = 0.0
        if abv < prefs["abvMin"]:
            abv_pen = wABV*(prefs["abvMin"]-abv)
        elif abv > prefs["abvMax"]:
            abv_pen = wABV*(abv - prefs["abvMax"])
        taste -= abv_pen
        if abv_pen>0: why.append(f"abv_penalty=-{abv_pen:.2f}")

        price_pen = 0.0
        if b["price"] > prefs["budgetMax"]:
            price_pen = wPrice*(b["price"]-prefs["budgetMax"])
            taste -= price_pen
            why.append(f"price_penalty=-{price_pen:.2f}")

        # 4) Availability bump (quality-of-life)
        if b["availability"] == "wide":
            taste += avail_bonus
            why.append(f"availability=+{avail_bonus:.2f}")

        return taste, why

    return score_beer

# Build the specialized Driver
driver = make_beer_driver(POLICY, STYLE_TRAITS, PREFS, CONTEXT)

# ----------------------------------------------------------------------------
# Targets/Capabilities: gather beer candidates and score
# ----------------------------------------------------------------------------
beers = []
for s, props in D.items():
    if EX+"style" in props:
        beers.append({
            "id": s,
            "label": get1(D, s, EX+"label", s.split("#")[-1]),
            "style": get1(D, s, EX+"style", ""),
            "abv": float(get1(D, s, EX+"abv", 0.0)),
            "ibu": float(get1(D, s, EX+"ibu", 0.0)),
            "price": float(get1(D, s, EX+"price", 0.0)),
            "availability": get1(D, s, EX+"availability", "limited"),
            "lactose": bool(get1(D, s, EX+"lactose", False)),
            "vegan": bool(get1(D, s, EX+"vegan", True)),
            "glutenFree": bool(get1(D, s, EX+"glutenFree", False)),
        })

scored = []
for b in beers:
    score, why = driver(b)
    scored.append({**b, "score": score, "trace": " | ".join(why)})
scored.sort(key=lambda x: x["score"], reverse=True)

# ----------------------------------------------------------------------------
# Self-checks (guardrails on the Driver's behavior)
# ----------------------------------------------------------------------------
# 1) Milk Stout rejected due to lactose + vegan preference
milk = [x for x in scored if x["id"].endswith("beer_milkstout")][0]
assert milk["score"] < -1e8 and "reject:lactose" in milk["trace"], "Milk stout not rejected"

# 2) Hot + spicy: a crisp/hoppy option should be in the top 3; stout shouldn't
top_labels = [x["label"] for x in scored[:3]]
assert not any("Stout" in lbl for lbl in top_labels), "Dark sweet stout should not be top in hot+spicy"
assert any(("Pilsner" in lbl) or ("Pale" in lbl) or ("IPA" in lbl) for lbl in top_labels), "Expected a crisp/hoppy beer in top 3"

# 3) Tripel penalized for high ABV and price over budget
tripel = [x for x in scored if x["id"].endswith("beer_tripel")][0]
assert "abv_penalty" in tripel["trace"] and "price_penalty" in tripel["trace"], "Tripel penalties missing"

# 4) At least one viable beer
assert any(x["score"] > -1e8 for x in scored), "No viable beers"

# ----------------------------------------------------------------------------
# Actionable insight: write ranked CSV with traces
# ----------------------------------------------------------------------------
csv_path = os.path.join(BASE, "ranked_beers.csv")
with open(csv_path, "w", newline="", encoding="utf-8") as f:
    w = csv.writer(f)
    w.writerow(["label","style","abv","price","availability","score","trace"])
    for row in scored:
        w.writerow([row["label"], row["style"].split("#")[-1], row["abv"], row["price"], row["availability"], f"{row['score']:.3f}", row["trace"]])

print("ALL TESTS PASSED")
print("Top picks:")
for r in scored[:5]:
    print(" -", r["label"], "=>", round(r["score"],3))

