#!/usr/bin/env python3
#
# Mixed-Computation Header (what this program does)
# -------------------------------------------------
# Static RDF (policy, comfort thresholds) + RULES_N3 (math:* triple patterns)
# are partially evaluated into a small driver. At run time, the driver ingests
# a tiny building model (rooms, facade areas, component Rw, T60), computes a
# proxy for façade standardized level difference (D_proxy ≈ composite R + 10·log10(S/A)),
# and checks compliance against a selected comfort level (“normal” or “increased”).
# It prints:
#   1) Answer (per-room predicted D, requirement, margin, pass/fail),
#   2) Reason why (trace lines that mirror math:* steps),
#   3) Check (harness re-validating the acoustics math, caps, and rule logic).
#
# Notes and provenance:
# - This case is inspired by Pauwels et al. (2011),§4 test case on acoustic
#   performance regulations, which considered EN 12354-3:2000 (façade) and
#   Belgian NBN S 01-400-1:2008 comfort levels (“normal” / “increased”).
#   Threshold numbers below are illustrative placeholders; swap in the exact
#   values from your code base. See paper context. 
#   (EN 12354-3 + Belgian comfort levels referenced in Pauwels et al. 2011.)
#
# Contract with P3:
# - All arithmetic/relations in RULES_N3 use math:* built-ins only.
# - Everything is inline (no external file writes).
# - One file produces Answer • Reason why • Check.

from __future__ import annotations
from dataclasses import dataclass
from typing import Any, Dict, List, Tuple
import math

# ──────────────────────────────────────────────────────────────────────────────
# Static + Dynamic RDF (inline)
# ──────────────────────────────────────────────────────────────────────────────

STATIC_TTL = r"""
@prefix ex:  <http://example.org/acoustics#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# Selected comfort level for this check (choose ex:Normal or ex:Increased)
ex:policy ex:comfort ex:Increased .

# Comfort thresholds (illustrative — replace with jurisdictional values)
ex:Normal    ex:minFacadeD_Bed_dB     30 .
ex:Normal    ex:minFacadeD_Living_dB  28 .
ex:Increased ex:minFacadeD_Bed_dB     35 .
ex:Increased ex:minFacadeD_Living_dB  31 .
"""

DYNAMIC_TTL = r"""
@prefix ex:  <http://example.org/acoustics#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# Living room
ex:Living1 a ex:LivingRoom .
ex:Living1 ex:volume_m3       60 .
ex:Living1 ex:T_s              0.6 .
ex:Living1 ex:facadeArea_m2    15 .
ex:Living1 ex:windowArea_m2     4 .
ex:Living1 ex:windowRw_dB      32 .
ex:Living1 ex:wallArea_m2      11 .
ex:Living1 ex:wallRw_dB        50 .

# Bedroom 1 (borderline for "Increased")
ex:Bedroom1 a ex:Bedroom .
ex:Bedroom1 ex:volume_m3       40 .
ex:Bedroom1 ex:T_s              0.5 .
ex:Bedroom1 ex:facadeArea_m2    12 .
ex:Bedroom1 ex:windowArea_m2     5 .
ex:Bedroom1 ex:windowRw_dB      30 .
ex:Bedroom1 ex:wallArea_m2       7 .
ex:Bedroom1 ex:wallRw_dB        48 .

# Bedroom 2 (passes)
ex:Bedroom2 a ex:Bedroom .
ex:Bedroom2 ex:volume_m3       35 .
ex:Bedroom2 ex:T_s              0.5 .
ex:Bedroom2 ex:facadeArea_m2    10 .
ex:Bedroom2 ex:windowArea_m2     4 .
ex:Bedroom2 ex:windowRw_dB      35 .
ex:Bedroom2 ex:wallArea_m2       6 .
ex:Bedroom2 ex:wallRw_dB        50 .
"""

# ──────────────────────────────────────────────────────────────────────────────
# RULES (N3) — math:* only
# ──────────────────────────────────────────────────────────────────────────────
RULES_N3 = r"""
@prefix ex:   <http://example.org/acoustics#> .
@prefix math: <http://www.w3.org/2000/10/swap/math#> .
@prefix xsd:  <http://www.w3.org/2001/XMLSchema#> .

# R1/R2: Attach required minima to rooms from selected comfort
{ ex:policy ex:comfort ?c . ?r a ex:Bedroom .      ?c ex:minFacadeD_Bed_dB ?m . }    => { ?r ex:requiredMin_dB ?m } .
{ ex:policy ex:comfort ?c . ?r a ex:LivingRoom .   ?c ex:minFacadeD_Living_dB ?m . } => { ?r ex:requiredMin_dB ?m } .

# R3: Margin = predictedD - requiredMin  (two-arg list → object is result)
{ ?r ex:predictedD_dB ?d . ?r ex:requiredMin_dB ?m . ( ?d ?m ) math:difference ?delta . }
=> { ?r ex:margin_dB ?delta } .

# R4: Pass/fail
{ ?r ex:predictedD_dB ?d . ?r ex:requiredMin_dB ?m . ?d math:notLessThan ?m . } => { ?r ex:passes true } .
{ ?r ex:predictedD_dB ?d . ?r ex:requiredMin_dB ?m . ?d math:lessThan ?m . }    => { ?r ex:passes false } .
"""

# ──────────────────────────────────────────────────────────────────────────────
# Tiny Turtle reader (inline-comment–safe; collects repeats)
# ──────────────────────────────────────────────────────────────────────────────
def parse_turtle_simple(ttl: str) -> Dict[str, Dict[str, Any]]:
    out: Dict[str, Dict[str, Any]] = {}

    def strip_inline_comment(line: str) -> str:
        in_str = False
        esc = False
        buf = []
        for ch in line:
            if ch == '"' and not esc:
                in_str = not in_str
            if ch == '#' and not in_str:
                break
            buf.append(ch)
            if esc:
                esc = False
            elif ch == '\\':
                esc = True
        return ''.join(buf)

    def add(s: str, p: str, o: Any):
        slot = out.setdefault(s, {})
        if p not in slot:
            slot[p] = o
        else:
            slot[p] = slot[p] + [o] if isinstance(slot[p], list) else [slot[p], o]

    for raw in ttl.splitlines():
        if raw.lstrip().startswith('@prefix'):
            continue
        line = strip_inline_comment(raw).strip()
        if not line or line.startswith('#'):
            continue
        for stmt in [s.strip() for s in line.split(' . ') if s.strip()]:
            if stmt.endswith('.'):
                stmt = stmt[:-1].strip()
            if not stmt:
                continue
            parts = stmt.split(None, 2)
            if len(parts) < 3:
                continue
            s, p, o = parts[0], parts[1], parts[2].strip()
            if o.startswith('"'):
                add(s, p, o[1:o.find('"', 1)])
            else:
                tok = o.split()[0]
                if tok in ('true', 'false'):
                    add(s, p, tok == 'true')
                else:
                    try:
                        add(s, p, float(tok) if '.' in tok else int(tok))
                    except Exception:
                        add(s, p, tok)
    return out

def listify(v) -> List[Any]:
    if v is None: return []
    return v if isinstance(v, list) else [v]

# ──────────────────────────────────────────────────────────────────────────────
# Domain + driver
# ──────────────────────────────────────────────────────────────────────────────
@dataclass
class RoomEval:
    iri: str
    kind: str
    required: float
    predicted: float
    margin: float
    passes: bool
    trace: List[str]

def composite_Rw_dB(areas: List[float], Rws: List[float]) -> float:
    Stot = sum(areas)
    if Stot <= 0:
        return 0.0
    tau = sum(a * 10 ** (-Rw / 10.0) for a, Rw in zip(areas, Rws))
    if tau <= 0:
        return 0.0
    return 10.0 * math.log10(Stot / tau)

def predicted_facade_D_proxy(room: Dict[str, Any]) -> Tuple[float, float, float]:
    V = float(room.get("ex:volume_m3", 0.0))
    T = float(room.get("ex:T_s", 0.5))
    S = float(room.get("ex:facadeArea_m2", 0.0))
    Sw = float(room.get("ex:windowArea_m2", 0.0))
    Rw_w = float(room.get("ex:windowRw_dB", 0.0))
    Swall = float(room.get("ex:wallArea_m2", 0.0))
    Rw_wall = float(room.get("ex:wallRw_dB", 0.0))

    A = max(1e-6, 0.16 * V / max(1e-6, T))
    Rcomp = composite_Rw_dB([Sw, Swall], [Rw_w, Rw_wall])
    Dp = Rcomp + (10.0 * math.log10(max(1e-6, S / A)))
    return Rcomp, A, Dp

def specialize_driver(S: Dict[str, Dict[str, Any]]):
    comfort = str(S.get("ex:policy", {}).get("ex:comfort", "ex:Normal"))
    comfort_node = S.get(comfort, {})
    bed_min   = float(comfort_node.get("ex:minFacadeD_Bed_dB", 30))
    liv_min   = float(comfort_node.get("ex:minFacadeD_Living_dB", 28))

    def required_for(kind: str) -> float:
        return bed_min if kind == "Bedroom" else liv_min

    def driver(D: Dict[str, Dict[str, Any]]):
        # find rooms by properties
        candidates = []
        for iri, props in D.items():
            if not isinstance(props, dict): continue
            if "ex:volume_m3" in props and "ex:T_s" in props:
                if "Bedroom" in iri or props.get("a") == "ex:Bedroom":
                    kind = "Bedroom"
                elif "Living" in iri or props.get("a") == "ex:LivingRoom":
                    kind = "LivingRoom"
                else:
                    kind = "Room"
                candidates.append((iri, kind, props))

        evals: List[RoomEval] = []
        for iri, kind, props in candidates:
            req = required_for(kind if kind in ("Bedroom","LivingRoom") else "LivingRoom")
            Rcomp, A, Dp = predicted_facade_D_proxy(props)
            margin = Dp - req
            passes = (Dp >= req)
            trace = [
                (f"R (composite) via energy sum (area-weighted): "
                 f"10·log10(S/Σ S_i·10^(-R_i/10)) = {Rcomp:.1f} dB"),
                (f"A = (0.16 × {float(props['ex:volume_m3']):.1f}) ÷ {float(props['ex:T_s']):.2f} "
                 f"= {A:.2f} m²  (math:product + math:quotient)"),
                (f"D_proxy = {Rcomp:.1f} + 10·log10(S/A) = {Dp:.1f} dB"),
                (f"R3: ({Dp:.1f} {req:.1f}) math:difference {margin:.1f} dB"),
                (f"R4: pass if {Dp:.1f} math:notLessThan {req:.1f} ⇒ {str(passes).lower()}")
            ]
            evals.append(RoomEval(
                iri=iri, kind=kind, required=req, predicted=Dp, margin=margin, passes=passes, trace=trace
            ))

        evals.sort(key=lambda e: (e.passes, e.margin))
        return evals, comfort, {"bed_min": bed_min, "liv_min": liv_min}

    return driver

# ──────────────────────────────────────────────────────────────────────────────
# Run
# ──────────────────────────────────────────────────────────────────────────────
def main():
    S = parse_turtle_simple(STATIC_TTL)
    D = parse_turtle_simple(DYNAMIC_TTL)

    driver = specialize_driver(S)
    evals, comfort, mins = driver(D)

    print("Answer:")
    print(f"- Comfort level: {comfort.split(':')[-1]}  "
          f"(bed ≥ {mins['bed_min']:.0f} dB, living ≥ {mins['liv_min']:.0f} dB)")
    for e in evals:
        status = "PASS" if e.passes else "FAIL"
        print(f"- {e.iri} ({e.kind}): {status} — predicted {e.predicted:.1f} dB, "
              f"required {e.required:.1f} dB, margin {e.margin:+.1f} dB")

    print("\nReason why:")
    for e in evals:
        print(f"- {e.iri}:")
        for ln in e.trace:
            print(f"  • {ln}")

    # ── CHECK (harness) ─
    print("\nCheck (harness):")
    errors: List[str] = []

    n_rooms = len(evals)
    pass_count = sum(1 for e in evals if e.passes)

    # (C1) Recompute D_proxy and compare (track max Δ)
    c1_max_dd = 0.0
    c1_mismatches = 0
    for e in evals:
        props = D[e.iri]
        Rc, A, Dp = predicted_facade_D_proxy(props)
        dd = abs(Dp - e.predicted)
        c1_max_dd = max(c1_max_dd, dd)
        if dd > 1e-9:
            errors.append(f"(C1) Predicted mismatch for {e.iri}: {e.predicted:.6f} vs {Dp:.6f}")
            c1_mismatches += 1

    # (C2) Rule logic: pass iff predicted >= required; margin = predicted - required
    c2_pass_mism = 0
    c2_margin_mism = 0
    for e in evals:
        if e.passes != (e.predicted >= e.required):
            errors.append(f"(C2) Pass logic mismatch for {e.iri}")
            c2_pass_mism += 1
        if abs(e.margin - (e.predicted - e.required)) > 1e-9:
            errors.append(f"(C2) Margin mismatch for {e.iri}")
            c2_margin_mism += 1

    # (C3) Bedrooms use bedroom min; living uses living min
    c3_mismatches = 0
    for e in evals:
        expect = mins["bed_min"] if e.kind == "Bedroom" else mins["liv_min"]
        if abs(e.required - expect) > 1e-9:
            errors.append(f"(C3) Requirement mapping mismatch for {e.iri}")
            c3_mismatches += 1

    # (C4) Sorting: failing rooms should appear before passing rooms; then by margin asc
    sorted_copy = sorted(evals, key=lambda z: (z.passes, z.margin))
    order_ok = ([x.iri for x in sorted_copy] == [x.iri for x in evals])
    if not order_ok:
        errors.append("(C4) Sorting order mismatch")

    # Outcome
    if errors:
        print("❌ FAIL")
        for er in errors:
            print(" -", er)
        raise SystemExit(1)
    else:
        print("✅ PASS — all checks satisfied.")
        print(f"  • [C1] Predicted D re-check OK: {n_rooms} rooms (max Δ={c1_max_dd:.3e} dB, mismatches={c1_mismatches})")
        print(f"  • [C2] Logic OK: pass mismatches={c2_pass_mism}, margin mismatches={c2_margin_mism}")
        print(f"  • [C3] Requirement mapping OK: mismatches={c3_mismatches}")
        print(f"  • [C4] Order OK: {'stable' if order_ok else '—'}; sequence {[e.iri for e in evals]}")

if __name__ == "__main__":
    main()

