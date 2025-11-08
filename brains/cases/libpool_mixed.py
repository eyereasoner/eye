#!/usr/bin/env python3
#
# Mixed-Computation Header (what this program does)
# -------------------------------------------------
# Static RDF policy (target pool size, per-sample min/max µL) + RULES_N3
# (math:* triple patterns only) are partially evaluated into a compact Driver.
# At runtime, the Driver ingests dynamic RDF (samples with measured conc,
# fragment length, available volume, QC flags), computes inverse-molarity
# weights w_i ∝ bp_i / conc_i, then water-fills volumes to:
#   - meet total target µL,
#   - respect per-sample min/max µL and availability,
#   - zero-out QC failures.
# The program prints:
#   1) Answer — per-sample µL and fractions + totals,
#   2) Reason why — traces that mirror math:* steps and clipping reasons,
#   3) Check (harness) — revalidates totals, bounds, QC, anti-correlation
#      between molarity and volume (for unclipped samples), and a monotonicity
#      probe (raising total target should not reduce any volume).
#
# Contract with P3:
# - All arithmetic/relations in RULES_N3 use math:* built-ins only.
# - Everything is inline (no external file writes).
# - One file produces Answer • Reason why • Check.

from __future__ import annotations
from dataclasses import dataclass
from typing import Any, Dict, List, Tuple

# ──────────────────────────────────────────────────────────────────────────────
# Static + Dynamic RDF (inline)
# ──────────────────────────────────────────────────────────────────────────────

STATIC_TTL = r"""
@prefix ex:  <http://example.org/libpool#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# Pooling policy
ex:policy ex:targetPool_uL 40.0 .
ex:policy ex:minVol_uL      2.0 .
ex:policy ex:maxVol_uL     15.0 .

# R1/2 in RULES_N3 will expose min/max as fractions of the target:
#   minFrac = minVol / targetPool,  maxFrac = maxVol / targetPool

"""

DYNAMIC_TTL = r"""
@prefix ex:  <http://example.org/libpool#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# Candidate samples (concentration ng/µL, fragment length bp, available volume µL, QC)
ex:S1 ex:name "Sample_01" . ex:S1 ex:conc_ng_uL 8.0  . ex:S1 ex:frag_bp 450 . ex:S1 ex:avail_uL 20.0 . ex:S1 ex:qcFail false .
ex:S2 ex:name "Sample_02" . ex:S2 ex:conc_ng_uL 20.0 . ex:S2 ex:frag_bp 300 . ex:S2 ex:avail_uL 10.0 . ex:S2 ex:qcFail false .
ex:S3 ex:name "Sample_03" . ex:S3 ex:conc_ng_uL 2.5  . ex:S3 ex:frag_bp 500 . ex:S3 ex:avail_uL 15.0 . ex:S3 ex:qcFail false .
ex:S4 ex:name "Sample_04" . ex:S4 ex:conc_ng_uL 0.0  . ex:S4 ex:frag_bp 400 . ex:S4 ex:avail_uL  6.0 . ex:S4 ex:qcFail true  .

# Membership list
ex:pool ex:include ex:S1 .
ex:pool ex:include ex:S2 .
ex:pool ex:include ex:S3 .
ex:pool ex:include ex:S4 .
"""

# ──────────────────────────────────────────────────────────────────────────────
# RULES (N3) — only math:* built-ins for arithmetic/relations
# ──────────────────────────────────────────────────────────────────────────────
RULES_N3 = r"""
@prefix ex:   <http://example.org/libpool#> .
@prefix math: <http://www.w3.org/2000/10/swap/math#> .
@prefix xsd:  <http://www.w3.org/2001/XMLSchema#> .

# R1: minFrac = minVol_uL / targetPool_uL
{ ex:policy ex:minVol_uL ?m . ex:policy ex:targetPool_uL ?t . ( ?m ?t ) math:quotient ?f . }
=>
{ ex:policy ex:minFrac ?f } .

# R2: maxFrac = maxVol_uL / targetPool_uL
{ ex:policy ex:maxVol_uL ?M . ex:policy ex:targetPool_uL ?t . ( ?M ?t ) math:quotient ?F . }
=>
{ ex:policy ex:maxFrac ?F } .

# R3: QC fail ⇒ allowedMaxFrac = 0
{ ?s ex:qcFail true . } => { ?s ex:allowedMaxFrac 0 } .

# R4: Availability as a fraction cap: availFrac = avail_uL / targetPool_uL
{ ?s ex:avail_uL ?av . ex:policy ex:targetPool_uL ?t . ( ?av ?t ) math:quotient ?aF . }
=>
{ ?s ex:availFrac ?aF } .

"""

# ──────────────────────────────────────────────────────────────────────────────
# Tiny Turtle reader (inline-comment–safe; collects repeated predicates)
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
            if not stmt: continue
            parts = stmt.split(None, 2)
            if len(parts) < 3: continue
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
# Domain and driver
# ──────────────────────────────────────────────────────────────────────────────
@dataclass
class SampleEval:
    iri: str
    name: str
    conc_ng_uL: float
    frag_bp: float
    avail_uL: float
    qcFail: bool
    weight: float            # bp/conc (inverse molarity up to constant)
    raw_uL: float            # un-clipped allocation before bounds
    vol_uL: float            # final volume
    frac: float              # vol / target
    clipped: str             # '', 'min', 'max', 'avail', or 'qc'
    trace: List[str]

def waterfill(weights: List[float], target: float,
              mins: List[float], maxs: List[float]) -> Tuple[List[float], str]:
    """
    Water-filling with lower/upper bounds.
    Returns (volumes, status_string). If target < sum(mins), lower bounds are
    proportionally scaled down.
    """
    n = len(weights)
    # Guard
    mins = [max(0.0, m) for m in mins]
    maxs = [max(mins[i], maxs[i]) for i in range(n)]
    # If target < sum(mins), proportionally scale mins
    sum_min = sum(mins)
    if target < sum_min - 1e-12:
        scale = target / sum_min if sum_min > 0 else 0.0
        return [m * scale for m in mins], "scaled_mins"
    # Iterative clipping
    fixed = [False]*n
    alloc = [0.0]*n
    remaining = target
    while True:
        free_idx = [i for i in range(n) if not fixed[i]]
        if not free_idx:
            break
        wsum = sum(max(0.0, weights[i]) for i in free_idx)
        if wsum < 1e-15:
            # No weights left → spread evenly within remaining and max bounds (respect mins already satisfied)
            even = remaining / len(free_idx)
            changed = False
            for i in free_idx:
                take = min(maxs[i]-alloc[i], max(0.0, even))
                alloc[i] += take
                if abs(take - even) > 1e-9:
                    changed = True
                fixed[i] = True
                remaining -= take
            if not changed:
                break
            else:
                continue
        k = remaining / wsum
        changed = False
        for i in free_idx:
            v = weights[i]*k
            if v < mins[i] - 1e-12:
                alloc[i] = mins[i]
                remaining -= alloc[i]
                fixed[i] = True
                changed = True
            elif v > maxs[i] + 1e-12:
                alloc[i] = maxs[i]
                remaining -= alloc[i]
                fixed[i] = True
                changed = True
            else:
                alloc[i] = v
        if not changed:
            break
    # If any remaining due to rounding, distribute within headroom (should be tiny)
    if remaining > 1e-9:
        headroom = [max(0.0, maxs[i]-alloc[i]) for i in range(n)]
        hsum = sum(headroom)
        if hsum > 0:
            for i in range(n):
                take = remaining * (headroom[i]/hsum)
                alloc[i] += take
            remaining = 0.0
    return alloc, "ok"

def specialize_driver(S: Dict[str, Dict[str, Any]]):
    target = float(S["ex:policy"]["ex:targetPool_uL"])
    minVol = float(S["ex:policy"]["ex:minVol_uL"])
    maxVol = float(S["ex:policy"]["ex:maxVol_uL"])
    # Fractions from RULES_N3 (conceptually): minFrac=minVol/target, maxFrac=maxVol/target

    def driver(D: Dict[str, Dict[str, Any]]):
        sample_iris = [str(x) for x in listify(D["ex:pool"]["ex:include"])]
        evals: List[SampleEval] = []

        weights = []
        mins = []
        maxs = []
        names = []
        raws = []
        avail_caps = []

        # Build rows and weights (bp / conc), guard conc>0
        for iri in sample_iris:
            P = D[iri]
            name = str(P.get("ex:name", iri.split("#")[-1]))
            conc = float(P.get("ex:conc_ng_uL", 0.0))
            frag = float(P.get("ex:frag_bp", 0.0))
            avail = float(P.get("ex:avail_uL", 0.0))
            qcFail = bool(P.get("ex:qcFail", False))

            weight = (frag / max(conc, 1e-9)) if conc > 0 else 1.0  # if conc=0, treat as weak (will be clipped by max/avail)
            min_i = 0.0 if qcFail else minVol
            max_i = 0.0 if qcFail else min(maxVol, avail)

            weights.append(weight if not qcFail else 0.0)
            mins.append(min_i)
            maxs.append(max_i)
            names.append(name)
            avail_caps.append(max_i)

            raw_uL = 0.0  # to be filled after scaling
            raws.append(raw_uL)

            evals.append(SampleEval(
                iri=iri, name=name, conc_ng_uL=conc, frag_bp=frag, avail_uL=avail,
                qcFail=qcFail, weight=weight, raw_uL=0.0, vol_uL=0.0, frac=0.0,
                clipped=("qc" if qcFail else ""), trace=[]
            ))

        # First-pass raw volumes from weights
        wsum = sum(weights)
        if wsum < 1e-15:
            # No usable weights (e.g., all qcFail) → everyone gets 0
            alloc = [0.0]*len(weights)
            status = "all_zero"
        else:
            k = target / wsum
            alloc = [weights[i]*k for i in range(len(weights))]
            status = "scaled"

        # Water-fill with mins/maxs
        alloc_wf, wf_status = waterfill(weights, target, mins, maxs)

        # Fill back evals and traces
        total = sum(alloc_wf)
        for i, e in enumerate(evals):
            e.raw_uL = alloc[i]
            e.vol_uL = alloc_wf[i]
            e.frac = (e.vol_uL / target) if target > 0 else 0.0
            # clipping reason
            if e.qcFail and e.vol_uL == 0.0:
                e.clipped = "qc"
            elif abs(e.vol_uL - mins[i]) < 1e-9 and mins[i] > 0:
                e.clipped = "min"
            elif abs(e.vol_uL - maxs[i]) < 1e-9 and maxs[i] >= 0:
                e.clipped = "max" if maxs[i] < e.avail_uL else "avail"
            else:
                e.clipped = ""
            # trace mirroring math:* ideas
            minFrac = minVol / target if target > 0 else 0.0
            maxFrac = maxVol / target if target > 0 else 0.0
            e.trace = [
                f"Inverse-molar weight: w=frag/conc = {e.frag_bp:.0f} / {max(e.conc_ng_uL,1e-9):.3f}",
                f"Scale to target: k={target:.1f}/∑w → raw={e.raw_uL:.2f} µL",
                f"R1: ({minVol:.1f} {target:.1f}) math:quotient minFrac={minFrac:.3f}",
                f"R2: ({maxVol:.1f} {target:.1f}) math:quotient maxFrac={maxFrac:.3f}",
                f"R3: qcFail={str(e.qcFail).lower()} ⇒ allowedMaxFrac={(0 if e.qcFail else maxFrac):.3f}",
                f"R4: availFrac={e.avail_uL:.1f}/{target:.1f} → {(e.avail_uL/target if target>0 else 0):.3f}",
                f"Clip to [min,max,avail]: {mins[i]:.2f}≤vol≤{maxs[i]:.2f} → vol={e.vol_uL:.2f} µL ({e.clipped or 'ok'})"
            ]

        # Sort by name for tidy output
        evals.sort(key=lambda z: z.name)
        totals = {"target_uL": target, "total_uL": total}
        ctx = {"minVol": minVol, "maxVol": maxVol, "status": status, "wf_status": wf_status}
        return evals, totals, ctx

    return driver

# ──────────────────────────────────────────────────────────────────────────────
# Run
# ──────────────────────────────────────────────────────────────────────────────
def main():
    S = parse_turtle_simple(STATIC_TTL)
    D = parse_turtle_simple(DYNAMIC_TTL)

    driver = specialize_driver(S)
    evals, totals, ctx = driver(D)

    # ── ANSWER ──
    print("Answer:")
    for e in evals:
        print(f"- {e.name:10s} • {e.vol_uL:6.2f} µL ({e.frac*100:5.1f}%) "
              f"• conc {e.conc_ng_uL:5.2f} ng/µL • bp {e.frag_bp:4.0f} • {('CLIP:'+e.clipped) if e.clipped else 'ok'}")
    print(f"  • Total: {totals['total_uL']:.2f} µL (target {totals['target_uL']:.2f} µL)")

    # ── REASON WHY ──
    print("\nReason why:")
    print(f"- Policy: minVol={ctx['minVol']:.1f} µL, maxVol={ctx['maxVol']:.1f} µL; status={ctx['status']}/{ctx['wf_status']}")
    for e in evals:
        print(f"- {e.name}:")
        for ln in e.trace:
            print(f"  • {ln}")

    # ── CHECK (harness) ─
    print("\nCheck (harness):")
    errors: List[str] = []

    target = float(S["ex:policy"]["ex:targetPool_uL"])
    minVol = float(S["ex:policy"]["ex:minVol_uL"])
    maxVol = float(S["ex:policy"]["ex:maxVol_uL"])

    # Helper maps
    dyn = D
    # Caps per sample
    caps = {}
    qc_map = {}
    weights = {}
    molarity = {}
    for e in evals:
        node = dyn[e.iri]
        avail = float(node.get("ex:avail_uL", 0.0))
        qc = bool(node.get("ex:qcFail", False))
        caps[e.iri] = min(maxVol, avail) if not qc else 0.0
        qc_map[e.iri] = qc
        conc = float(node.get("ex:conc_ng_uL", 0.0))
        bp = float(node.get("ex:frag_bp", 0.0))
        weights[e.iri] = (bp / max(conc, 1e-9)) if conc > 0 else 1.0
        molarity[e.iri] = conc / max(bp, 1e-9)  # proportional to molar concentration

    # (C1) Totals: delivered equals min(target, sum caps)
    total = sum(e.vol_uL for e in evals)
    deliverable_cap = sum(caps[e.iri] for e in evals)
    expected_total = min(target, deliverable_cap)
    if abs(total - expected_total) > 1e-6:
        errors.append(f"(C1) Total volume {total:.6f} ≠ expected {expected_total:.6f} (cap {deliverable_cap:.2f})")

    # (C2) Bounds and non-negativity; if target ≥ Σ(minVol for non-QC), enforce per-sample min
    min_sum = sum((minVol if not qc_map[e.iri] else 0.0) for e in evals)
    for e in evals:
        cap = caps[e.iri]
        if e.vol_uL < -1e-9:
            errors.append(f"(C2) Negative volume for {e.name}")
        if e.vol_uL - cap > 1e-9:
            errors.append(f"(C2) Cap exceeded for {e.name}: {e.vol_uL:.3f} > {cap:.3f}")
        if target >= min_sum - 1e-9 and not qc_map[e.iri]:
            if e.vol_uL + 1e-9 < minVol:
                errors.append(f"(C2) Below per-sample minimum for {e.name}: {e.vol_uL:.3f} < {minVol:.3f}")

    # (C3) QC fail ⇒ zero volume
    for e in evals:
        if qc_map[e.iri] and e.vol_uL > 1e-9:
            errors.append(f"(C3) QC-failed {e.name} got non-zero volume")

    # (C4) Anti-correlation: for any *unclipped* pair i,j with mol_i > mol_j, expect vol_i ≤ vol_j
    unclipped = [e for e in evals if not e.clipped]  # neither min/max/avail/qc
    for i in range(len(unclipped)):
        for j in range(i+1, len(unclipped)):
            ei, ej = unclipped[i], unclipped[j]
            if molarity[ei.iri] > molarity[ej.iri] + 1e-12 and ei.vol_uL > ej.vol_uL + 1e-6:
                errors.append(f"(C4) Anti-correlation violated: {ei.name} (higher molarity) > {ej.name} volume")

    # (C5) Monotonicity: increase target by 20% → no sample should get *less* volume (respecting caps)
    S2 = {k: (v.copy() if isinstance(v, dict) else v) for k, v in S.items()}
    S2["ex:policy"]["ex:targetPool_uL"] = float(S2["ex:policy"]["ex:targetPool_uL"]) * 1.20
    evals2, totals2, _ = specialize_driver(S2)(D)
    vols2 = {e.iri: e.vol_uL for e in evals2}
    for e in evals:
        if vols2.get(e.iri, 0.0) + 1e-9 < e.vol_uL:
            errors.append(f"(C5) Monotonicity: {e.name} decreased with higher target ({e.vol_uL:.2f} → {vols2[e.iri]:.2f})")

    # Outcome
    if errors:
        print("❌ FAIL")
        for er in errors:
            print(" -", er)
        raise SystemExit(1)
    else:
        print("✅ PASS — all checks satisfied.")
        print(f"  • [C1] Totals OK: total {total:.2f} µL (cap {deliverable_cap:.2f}, target {target:.2f})")
        print(f"  • [C2] Bounds OK across {len(evals)} samples (min {minVol:.1f} µL, max {maxVol:.1f} µL, QC respected)")
        if unclipped:
            print(f"  • [C4] Anti-correlation OK among {len(unclipped)} unclipped samples (molarity↑ ⇒ volume↓)")
        else:
            print("  • [C4] Anti-correlation not applicable (all samples clipped)")
        print(f"  • [C5] Monotonicity OK: +20% target did not reduce any individual volume")

if __name__ == "__main__":
    main()

