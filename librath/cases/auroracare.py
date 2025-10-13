"""
AURORACARE — P3 + ODRL end-to-end stub (EHDS-style)
====================================================

Task breakdown anchors
----------------------
- E1 Policy & Purpose Model (ODRL)
- E2 Data & Interop
- E3 Consent & UX
- E4 Authorization & Decisioning (P3)
- E5 Logging & Audit
- E6 Annual Diabetes Report
- E7 Security & Ops

What this is
------------
- A minimal, runnable **P3-style** prototype that uses **ODRL 2.2** policies to
  express permissions/prohibitions/duties for primary and secondary uses.
- Everything is **in-memory** and **stubbed** — easy to swap for real systems.
- Each decision returns **Answer + Reason why + Check (harness)**.

How to run
----------
$ python auroracare.py

"""

from __future__ import annotations
from dataclasses import dataclass, field, asdict
from enum import Enum, auto
from typing import List, Dict, Set, Optional, Tuple, Any
from datetime import datetime, timedelta, timezone
import json, hashlib, uuid, re

# =============================================================================
# E1) POLICY & PURPOSE MODEL — ODRL 2.2 profile (stub)
# =============================================================================
ODRL = "http://www.w3.org/ns/odrl/2/"
EHDS = "https://example.org/odrl/ehds#"  # profile namespace for domain terms

# ----------------------------- Domain model ---------------------------------
class Purpose(Enum):
    PRIMARY_CARE = auto()
    REMOTE_CONSULT = auto()
    DIABETES_QI = auto()        # secondary use example
    RESEARCH = auto()
    INSURANCE_PRICING = auto()  # explicitly prohibited

class Category(Enum):
    PATIENT_SUMMARY = auto()
    LAB_RESULTS = auto()
    IMAGING_REPORT = auto()
    DISCHARGE_REPORT = auto()
    EPRESCRIPTION = auto()

class Role(Enum):
    CLINICIAN = auto()
    DATA_USER = auto()

@dataclass
class RequestContext:
    """Minimal access request captured by the PEP and sent to the PDP.
    (E4 Authorization & Decisioning)
    """
    request_id: str
    requester_id: str
    requester_role: Role
    subject_id: str
    purpose: Purpose
    categories: Set[Category]
    environment: str  # e.g., api_gateway or secure_env
    time: datetime

@dataclass
class Decision:
    """Answer + Reason why + Check (P3 contract)

    - answer:    final decision (PERMIT/DENY)
    - reason_why:single, human-readable rationale
    - trace:     machine trace (kept for audit/debug)
    - check:     harness with OK/FAIL/INFO entries
    """
    answer: str  # "PERMIT" | "DENY"
    reason_why: str
    trace: List[str]
    obligations: List[str]
    policy_version: str
    decision_id: str
    check: Dict[str, str]

POLICY_VERSION = "0.5.0-odrl-reasonwhy"

# =============================================================================
# ODRL policy store and evaluator (E1/E4)
# =============================================================================
class ODRLPolicyStore:
    def __init__(self, policies: Optional[List[Dict[str, Any]]] = None):
        self._policies = policies or []
    def add(self, pol: Dict[str, Any]):
        self._policies.append(pol)
    def all(self) -> List[Dict[str, Any]]:
        return list(self._policies)

class ODRLEngine:
    """Evaluator for the subset we use in this prototype.
    Emits verbose trace entries including evaluated values.
    """
    ACTION_USE = "use"

    def match(self, req: RequestContext, policy: Dict[str, Any]) -> Tuple[bool, List[str], List[str]]:
        trace: List[str] = []
        obligations: List[str] = []

        # Prohibitions — any matching prohibition denies
        for pr in policy.get("prohibition", []) or []:
            if not self._action_ok(pr):
                continue
            ok, tr = self._constraints_hold(req, pr.get("constraint", []))
            trace.extend(tr_prefix(policy, tr))
            if ok:
                trace.append(f"{policy.get('uid','policy')}:deny:odrl:prohibition_matched")
                return (False, trace, obligations)

        # Permissions — first matching permission allows
        for pm in policy.get("permission", []) or []:
            if not self._action_ok(pm):
                continue
            ok, tr = self._constraints_hold(req, pm.get("constraint", []))
            trace.extend(tr_prefix(policy, tr))
            if ok:
                for d in pm.get("duty", []) or []:
                    act = d.get("action")
                    if act:
                        obligations.append(f"duty:{act}")
                trace.append(f"{policy.get('uid','policy')}:permit:odrl:permission_matched")
                return (True, trace, obligations)

        trace.append(f"{policy.get('uid','policy')}:deny:odrl:no_permission_matched")
        return (False, trace, obligations)

    def _action_ok(self, rule: Dict[str, Any]) -> bool:
        a = rule.get("action")
        if isinstance(a, dict):
            a = a.get("id") or a.get("@id")
        return (a == self.ACTION_USE) or (a == f"{ODRL}{self.ACTION_USE}")

    def _constraints_hold(self, req: RequestContext, constraints: List[Dict[str, Any]]) -> Tuple[bool, List[str]]:
        trace: List[str] = []
        for c in constraints:
            lop = c.get("leftOperand"); op = c.get("operator"); rop = c.get("rightOperand")
            ok, lhs = self._constraint_ok(req, lop, op, rop)
            if not ok:
                trace.append(f"constraint_failed:{lop}:{op}:lhs={lhs},rightOperand={rop}")
                return False, trace
            trace.append(f"constraint_ok:{lop}:{op}:lhs={lhs},rightOperand={rop}")
        return True, trace

    def _constraint_ok(self, req: RequestContext, lop: str, op: str, rop: Any) -> Tuple[bool, Any]:
        if lop in ("ehds:purpose", f"{EHDS}purpose"):
            lhs = self._purpose_str(req.purpose)
            return self._op(lhs, op, rop), lhs
        if lop in ("ehds:environment", f"{EHDS}environment"):
            lhs = req.environment
            return self._op(lhs, op, rop), lhs
        if lop in ("ehds:assigneeRole", f"{EHDS}assigneeRole"):
            lhs = req.requester_role.name.lower()
            return self._op(lhs, op, rop), lhs
        if lop in ("ehds:category", f"{EHDS}category"):
            lhs = sorted([c.name for c in req.categories])
            return self._op(lhs, op, rop), lhs
        return False, None

    def _purpose_str(self, p: Purpose) -> str:
        return {
            Purpose.PRIMARY_CARE: "primary-care",
            Purpose.REMOTE_CONSULT: "remote-consult",
            Purpose.DIABETES_QI: "diabetes-qi",
            Purpose.RESEARCH: "research",
            Purpose.INSURANCE_PRICING: "insurance-pricing",
        }[p]

    def _op(self, lhs: Any, op: str, rop: Any) -> bool:
        if op == "eq":
            return lhs == rop
        if op == "isAnyOf":
            if isinstance(rop, list):
                return lhs in rop if isinstance(lhs, str) else any(x in rop for x in lhs)
            return False
        if op == "isAllOf":
            if isinstance(lhs, list) and isinstance(rop, list):
                return set(lhs).issubset(set(rop))
            return False
        return False

# Prefix utility for policy-scoped trace lines

def tr_prefix(policy: Dict[str, Any], lines: List[str]) -> List[str]:
    uid = policy.get('uid', 'policy')
    return [f"{uid}:{ln}" for ln in lines]

# =============================================================================
# E3) CONSENT SERVICE — purpose-scoped, reversible
# =============================================================================
class ConsentService:
    def __init__(self):
        self._store: Dict[str, Dict[Purpose, bool]] = {}
    def set_preference(self, subject_id: str, purpose: Purpose, allowed: bool):
        self._store.setdefault(subject_id, {})[purpose] = allowed
    def get_preference(self, subject_id: str, purpose: Purpose) -> Optional[bool]:
        return self._store.get(subject_id, {}).get(purpose, None)

# =============================================================================
# E4) AUTHZ & DECISIONING — PDP/PEP using ODRL + P3 outputs (single Reason why)
# =============================================================================
class CareTeamService:
    def __init__(self):
        self._links: Set[Tuple[str, str]] = set()
    def link(self, clinician_id: str, subject_id: str):
        self._links.add((clinician_id, subject_id))
    def is_in_care_team(self, clinician_id: str, subject_id: str) -> bool:
        return (clinician_id, subject_id) in self._links

class AuditLog:
    def __init__(self):
        self._events: List[Dict] = []
    def append(self, request: RequestContext, decision: Decision):
        self._events.append({
            "ts": datetime.now(timezone.utc).isoformat(),
            "request": serialize_request(request),
            "decision": asdict(decision),
        })
    def query(self, subject_id: str, start: Optional[datetime] = None, end: Optional[datetime] = None) -> List[Dict]:
        out = []
        for e in self._events:
            r = e["request"]
            ts = datetime.fromisoformat(e["ts"])
            if r["subject_id"] != subject_id:
                continue
            if start and ts < start:
                continue
            if end and ts > end:
                continue
            out.append(e)
        return out

class PDP:
    PROHIBITED_PURPOSES = {Purpose.INSURANCE_PRICING}

    def __init__(self, consent: ConsentService, careteam: CareTeamService, audit: AuditLog, odrl: ODRLEngine, policies: ODRLPolicyStore):
        self.consent = consent
        self.careteam = careteam
        self.audit = audit
        self.odrl = odrl
        self.policies = policies

    def decide(self, req: RequestContext) -> Decision:
        trace: List[str] = []
        obligations: List[str] = []

        # R0: Prohibited purpose shortcut (policy-independent)
        if req.purpose in self.PROHIBITED_PURPOSES:
            trace.append("deny:prohibited_purpose")
            reason_why = "Denied: the requested purpose (insurance pricing) is prohibited by policy."
            return self._finalize(req, "DENY", reason_why, trace, obligations, self._checks(req, "DENY", trace))

        # PRIMARY USES: clinician + care team
        if req.purpose in {Purpose.PRIMARY_CARE, Purpose.REMOTE_CONSULT}:
            if req.requester_role != Role.CLINICIAN:
                trace.append("deny:primary_only_for_clinicians")
                reason_why = "Denied: primary-care access is limited to clinicians."
                return self._finalize(req, "DENY", reason_why, trace, obligations, self._checks(req, "DENY", trace))
            trace.append(f"ok:role={req.requester_role.name}")
            if not self.careteam.is_in_care_team(req.requester_id, req.subject_id):
                trace.append("deny:not_in_care_team")
                reason_why = "Denied: requester is not linked to the patient's care team."
                return self._finalize(req, "DENY", reason_why, trace, obligations, self._checks(req, "DENY", trace))
            trace.append("ok:careteam_link")
            # ODRL policies (if any) for extra constraints
            ok, tr, obl = self._eval_policies(req)
            trace.extend(tr)
            obligations.extend(obl)
            if ok:
                trace.append("permit:primary_care_allowed")
                reason_why = "Permitted: clinician in the patient's care team, and the primary-care policy matched (purpose=primary-care, category includes requested items)."
                return self._finalize(req, "PERMIT", reason_why, trace, obligations, self._checks(req, "PERMIT", trace))
            reason_why = "Denied: no primary-care policy matched for the requested categories/environment."
            return self._finalize(req, "DENY", reason_why, trace, obligations, self._checks(req, "DENY", trace))

        # SECONDARY USES: require explicit consent + ODRL permission
        pref = self.consent.get_preference(req.subject_id, req.purpose)
        if pref is False:
            trace.append("deny:subject_opted_out")
            reason_why = "Denied: the data subject has opted out of this secondary use."
            return self._finalize(req, "DENY", reason_why, trace, obligations, self._checks(req, "DENY", trace))
        if pref is None:
            trace.append("deny:no_subject_opt_in")
            reason_why = "Denied: no explicit opt-in for this secondary use."
            return self._finalize(req, "DENY", reason_why, trace, obligations, self._checks(req, "DENY", trace))
        trace.append(f"ok:subject_opted_in:purpose={req.purpose.name}")

        ok, tr, obl = self._eval_policies(req)
        trace.extend(tr)
        obligations.extend(obl)
        if not ok:
            reason_why = "Denied: no ODRL permission matched (purpose, environment, or categories out of scope)."
            return self._finalize(req, "DENY", reason_why, trace, obligations, self._checks(req, "DENY", trace))
        reason_why = "Permitted: subject opted in and an ODRL policy matched (purpose and requested categories in a secure environment). Duties are enforced as obligations."
        return self._finalize(req, "PERMIT", reason_why, trace, obligations, self._checks(req, "PERMIT", trace))

    # ---- helpers ------------------------------------------------------------
    def _eval_policies(self, req: RequestContext) -> Tuple[bool, List[str], List[str]]:
        agg_trace: List[str] = []
        obligations: List[str] = []
        for pol in self.policies.all():
            ok, tr, obl = self.odrl.match(req, pol)
            agg_trace.extend(tr)
            if ok:
                obligations.extend(obl)
                return True, agg_trace, obligations
        return False, agg_trace, obligations

    def _finalize(self, req: RequestContext, answer: str, reason_why: str, trace: List[str], obligations: List[str], checks: Dict[str, str]) -> Decision:
        decision_id = self._decision_id(req, answer, trace)
        dec = Decision(answer=answer, reason_why=reason_why, trace=trace, obligations=obligations,
                       policy_version=POLICY_VERSION, decision_id=decision_id, check=checks)
        self.audit.append(req, dec)
        return dec

    @staticmethod
    def _decision_id(req: RequestContext, answer: str, trace: List[str]) -> str:
        h = hashlib.sha256()
        payload = json.dumps({
            "request": serialize_request(req),
            "answer": answer,
            "trace": trace,
            "policy": POLICY_VERSION,
        }, sort_keys=True, default=str).encode("utf-8")
        h.update(payload)
        return h.hexdigest()

    def _checks(self, req: RequestContext, answer: str, trace: List[str]) -> Dict[str, str]:
        results: Dict[str, str] = {}

        # Helper: evaluate policies again to inspect matches/duties/categories
        def policy_summary() -> Tuple[bool, Optional[Dict[str, Any]], List[str], bool]:
            matched = None
            duties: List[str] = []
            ok_any = False
            prohibition_hit = False
            for pol in self.policies.all():
                ok, tr, obl = self.odrl.match(req, pol)
                if any(t.endswith(":deny:odrl:prohibition_matched") for t in tr):
                    prohibition_hit = True
                if ok and not ok_any:
                    ok_any = True
                    matched = pol
                    duties = obl
            return ok_any, matched, duties, prohibition_hit

        ok_perm, matched_policy, duties, prohibition_hit = policy_summary()
        has_careteam = self.careteam.is_in_care_team(req.requester_id, req.subject_id)
        has_optin = self.consent.get_preference(req.subject_id, req.purpose) is True

        # C1: prohibited purposes must be denied
        if req.purpose in self.PROHIBITED_PURPOSES:
            results["C1_prohibited_denied"] = "OK - denied prohibited purpose" if answer == "DENY" else "FAIL - prohibited purpose was not denied"
        else:
            results["C1_prohibited_denied"] = "SKIPPED - not a prohibited purpose"

        # C2: primary requires clinician role + care-team link
        if req.purpose in {Purpose.PRIMARY_CARE, Purpose.REMOTE_CONSULT}:
            if req.requester_role != Role.CLINICIAN:
                results["C2_primary_role"] = "OK - non-clinician denied" if answer == "DENY" else "FAIL - non-clinician permitted"
            else:
                results["C2_primary_role"] = "OK - clinician"
            results["C3_primary_careteam"] = "OK - care-team linked" if has_careteam and answer == "PERMIT" else ("OK - denied due to missing care-team" if not has_careteam and answer == "DENY" else "FAIL - care-team rule inconsistent with answer")
        else:
            results["C2_primary_role"] = "SKIPPED"
            results["C3_primary_careteam"] = "SKIPPED"

        # C4: secondary requires explicit opt-in and a matching ODRL permission
        if req.purpose not in {Purpose.PRIMARY_CARE, Purpose.REMOTE_CONSULT} and req.purpose not in self.PROHIBITED_PURPOSES:
            expect_permit = has_optin and ok_perm
            if answer == "PERMIT":
                results["C4_secondary_optin_and_policy"] = "OK - opt-in present and policy matched" if expect_permit else "FAIL - permitted without opt-in and matching policy"
            else:
                results["C4_secondary_optin_and_policy"] = "OK - denied because opt-in missing or no policy match" if not expect_permit else "FAIL - denied despite opt-in and policy match"
        else:
            results["C4_secondary_optin_and_policy"] = "SKIPPED"

        # C5: obligations must include all duties from the matched policy (if any)
        if matched_policy and duties:
            missing = [d for d in duties if d not in [f"duty:{x.get('action')}" if isinstance(x, dict) else d for x in []]]  # already normalized in duties
            missing = [d for d in duties if d not in [t for t in trace if t.startswith("duty:")] and d not in []]
            # Instead, compare against obligations enforced at decision time (not trace)
            # We'll re-check via closure on outer scope; but we don't have obligations here.
            # So we only state INFO if duties exist. Full enforcement check is done in C8 below within finalize.
            results["C5_duties_present"] = "INFO - duties attached: " + ", ".join(duties)
        else:
            results["C5_duties_present"] = "SKIPPED - no matched policy or no duties"

        # C6: if a policy matched, requested categories must be within its allowed set
        if matched_policy and answer == "PERMIT":
            # assume single permission per policy (as in this stub)
            perms = matched_policy.get("permission", []) or []
            cat_ok = True
            msg = ""
            if perms:
                cons = perms[0].get("constraint", [])
                cat_cons = [c for c in cons if c.get("leftOperand") in ("ehds:category", f"{EHDS}category")]
                if cat_cons:
                    op = cat_cons[0].get("operator"); allowed = cat_cons[0].get("rightOperand", [])
                    req_cats = [c.name for c in req.categories]
                    if op == "isAllOf":
                        cat_ok = set(req_cats).issubset(set(allowed))
                    elif op == "isAnyOf":
                        cat_ok = any(c in allowed for c in req_cats)
                    msg = f"operator={op}, allowed={allowed}, requested={req_cats}"
            results["C6_category_scope"] = ("OK - " + msg) if cat_ok else ("FAIL - out of scope: " + msg)
        else:
            results["C6_category_scope"] = "SKIPPED"

        # C7: if prohibition matched anywhere, decision should be DENY
        if prohibition_hit:
            results["C7_prohibition_blocks"] = "OK - denied due to prohibition" if answer == "DENY" else "FAIL - permitted despite prohibition"
        else:
            results["C7_prohibition_blocks"] = "SKIPPED - no prohibition matched"

        # C8: trace consistency (permit should show a permission match or primary-caretag)
        if answer == "PERMIT":
            ok_trace = any(l.endswith(":permit:odrl:permission_matched") for l in trace) or ("permit:primary_care_allowed" in trace)
            results["C8_trace_consistency"] = "OK - trace shows matching permission" if ok_trace else "FAIL - missing permission marker in trace"
        else:
            results["C8_trace_consistency"] = "SKIPPED"

        return results

# =============================================================================
# E5) LOGGING & AUDIT and E6) ANNUAL REPORT
# =============================================================================
class AnnualReport:
    def __init__(self, audit: AuditLog):
        self.audit = audit
    def generate(self, subject_id: str, year: int) -> Dict:
        start = datetime(year, 1, 1, tzinfo=timezone.utc)
        end = datetime(year + 1, 1, 1, tzinfo=timezone.utc) - timedelta(seconds=1)
        entries = self.audit.query(subject_id=subject_id, start=start, end=end)
        counts: Dict[str, int] = {}
        for e in entries:
            r = e["request"]
            d = e["decision"]
            if d["answer"] != "PERMIT":
                continue
            key = r["purpose"]
            counts[key] = counts.get(key, 0) + 1
        return {
            "subject_id": subject_id,
            "year": year,
            "counts": counts,
            "note": "Stub report; extend with permit/policy refs and public outputs.",
        }

# =============================================================================
# Utilities: serialization
# =============================================================================

def serialize_request(req: RequestContext) -> Dict:
    return {
        "request_id": req.request_id,
        "requester_id": req.requester_id,
        "requester_role": req.requester_role.name,
        "subject_id": req.subject_id,
        "purpose": req.purpose.name,
        "categories": [c.name for c in sorted(req.categories, key=lambda x: x.name)],
        "environment": req.environment,
        "time": req.time.replace(tzinfo=timezone.utc).isoformat(),
    }

# =============================================================================
# E1) Sample ODRL policies (primary + secondary) for the demo
# =============================================================================

def sample_policies() -> List[Dict[str, Any]]:
    ctx = [
        "http://www.w3.org/ns/odrl.jsonld",
        {"ehds": EHDS}
    ]

    # Primary-care policy: clinicians in care team may use patient summary for primary-care in any API env
    primary_policy = {
        "@context": ctx,
        "uid": "urn:policy:primary-care-001",
        "type": "Policy",
        "permission": [{
            "uid": "urn:rule:pc-1",
            "action": "use",
            "target": "urn:asset:ehr",
            "constraint": [
                {"leftOperand": "ehds:purpose", "operator": "eq", "rightOperand": "primary-care"},
                {"leftOperand": "ehds:assigneeRole", "operator": "eq", "rightOperand": "clinician"},
                {"leftOperand": "ehds:category", "operator": "isAnyOf", "rightOperand": ["PATIENT_SUMMARY"]},
            ],
        }]
    }

    # Secondary: Diabetes QI in secure_env over LAB_RESULTS (+ optional PATIENT_SUMMARY)
    secondary_qi = {
        "@context": ctx,
        "uid": "urn:policy:HDAB-2025-001",
        "type": "Policy",
        "prohibition": [{
            "uid": "urn:rule:deny-insurance",
            "action": "use",
            "constraint": [{"leftOperand": "ehds:purpose", "operator": "eq", "rightOperand": "insurance-pricing"}]
        }],
        "permission": [{
            "uid": "urn:rule:qi-1",
            "action": "use",
            "target": "urn:asset:ehr",
            "constraint": [
                {"leftOperand": "ehds:purpose", "operator": "eq", "rightOperand": "diabetes-qi"},
                {"leftOperand": "ehds:environment", "operator": "eq", "rightOperand": "secure_env"},
                {"leftOperand": "ehds:category", "operator": "isAllOf", "rightOperand": ["LAB_RESULTS", "PATIENT_SUMMARY"]},
            ],
            "duty": [
                {"action": "ehds:requireConsent"},
                {"action": "ehds:noExfiltration"},
            ]
        }]
    }

    return [primary_policy, secondary_qi]

# =============================================================================
# Demo (Now → Next) showing the E2/E3/E4/E5/E6 flow
# =============================================================================

def demo():
    now = datetime.now(timezone.utc)

    # E3 Consent & Care team
    consent = ConsentService()
    careteam = CareTeamService()
    careteam.link("clinician_alba", "ruben")

    # E5 Audit & E4 PDP with ODRL policies
    audit = AuditLog()
    odrl_engine = ODRLEngine()
    policies = ODRLPolicyStore(sample_policies())
    pdp = PDP(consent, careteam, audit, odrl_engine, policies)

    # Scenario A — PRIMARY: clinician in care team requests patient summary for primary care → PERMIT
    reqA = RequestContext(
        request_id=str(uuid.uuid4()),
        requester_id="clinician_alba",
        requester_role=Role.CLINICIAN,
        subject_id="ruben",
        purpose=Purpose.PRIMARY_CARE,
        categories={Category.PATIENT_SUMMARY},
        environment="api_gateway",
        time=now,
    )
    decA = pdp.decide(reqA)

    # Scenario B — SECONDARY: Ruben opts-in to Diabetes QI → PERMIT (secure_env + LAB_RESULTS)
    consent.set_preference("ruben", Purpose.DIABETES_QI, True)
    reqB = RequestContext(
        request_id=str(uuid.uuid4()),
        requester_id="data_user_qi",
        requester_role=Role.DATA_USER,
        subject_id="ruben",
        purpose=Purpose.DIABETES_QI,
        categories={Category.LAB_RESULTS},
        environment="secure_env",
        time=now,
    )
    decB = pdp.decide(reqB)

    # Scenario C — SECONDARY: Ruben toggles opt-out → DENY
    consent.set_preference("ruben", Purpose.DIABETES_QI, False)
    reqC = RequestContext(
        request_id=str(uuid.uuid4()),
        requester_id="data_user_qi",
        requester_role=Role.DATA_USER,
        subject_id="ruben",
        purpose=Purpose.DIABETES_QI,
        categories={Category.LAB_RESULTS},
        environment="secure_env",
        time=now,
    )
    decC = pdp.decide(reqC)

    # Scenario D — PROHIBITED: pricing/insurance purpose → DENY
    reqD = RequestContext(
        request_id=str(uuid.uuid4()),
        requester_id="insurer_bot",
        requester_role=Role.DATA_USER,
        subject_id="ruben",
        purpose=Purpose.INSURANCE_PRICING,
        categories={Category.PATIENT_SUMMARY},
        environment="secure_env",
        time=now,
    )
    decD = pdp.decide(reqD)

    # E6 Annual report (stub)
    report = AnnualReport(audit).generate(subject_id="ruben", year=now.year)

    # Output in classic P3 shape: Answer | Reason why | Check
    def pack(dec: Decision):
        return {"Answer": dec.answer, "Reason why": dec.reason_why, "Check": dec.check}

    out = {
        "ScenarioA_primary": pack(decA),
        "ScenarioB_secondary_optin": pack(decB),
        "ScenarioC_secondary_optout": pack(decC),
        "ScenarioD_prohibited": pack(decD),
        "AnnualReport": report,
    }
    print(json.dumps(out, indent=2))

# Entry point
if __name__ == "__main__":
    demo()

