#!/usr/bin/env python3
# AuroraCare — Purpose-based Medical Data Exchange (ODRL/DPV + DPV-Health)
# -----------------------------------------------------------------------------
# This Python demo mirrors the behaviour of the auroracare.html page:
# - ODRL/DPV policies incl. DPV-Health purposes for primary care and remote consult
# - EHDS purposes for QI and research
# - Prohibition on DPV-Health InsuranceManagement
# - Constraint operators: eq, isAnyOf, isAllOf (with correct "required ⊆ requested" semantics)
# - CURIE expansion for ehds:, dpv:, hlth:, ex:, ac:, odrl:
# - Answer / Reason why / Check (C1..C10) output + trace for debugging
#
# NOTE: This is a stubbed PDP: no real identities, consent or EHRs.
# -----------------------------------------------------------------------------

import json
from datetime import datetime
from typing import Any, Dict, List, Tuple
import uuid

# Prefix IRIs
ODRL = "http://www.w3.org/ns/odrl/2/"
DPV  = "https://w3id.org/dpv#"
EHDS = "https://w3id.org/dpv/legal/eu/ehds#"
HLTH = "https://w3id.org/dpv/sector/health#"
EX   = "https://example.org/health#"
AC   = "https://example.org/auroracare#"

# Purposes (IRIs)
PURPOSE = {
    "PRIMARY_CARE": HLTH + "PrimaryCareManagement",
    "REMOTE_CONSULT": HLTH + "PatientRemoteMonitoring",
    "RESEARCH": EHDS + "HealthcareScientificResearch",
    "QI": EHDS + "EnsureQualitySafetyHealthcare",
    "AI_TRAINING": EHDS + "TrainTestAndEvaluateAISystemsAlgorithms",
    "INSURANCE": HLTH + "InsuranceManagement",
}
PRIMARY_SET = {PURPOSE["PRIMARY_CARE"], PURPOSE["REMOTE_CONSULT"]}

# Categories (local -> IRI)
CATEGORIES = ["PATIENT_SUMMARY","LAB_RESULTS","IMAGING_REPORT","DISCHARGE_REPORT","EPRESCRIPTION"]
def ex_iri(local: str) -> str: return EX + local

# ----------------------------- Policies (JSON-LD-ish) -----------------------------
def build_policies() -> List[Dict[str, Any]]:
    """ODRL/DPV + DPV-Health policies mirroring auroracare.html"""
    return [
        # Primary care / remote consult — allow either DPV-Health purpose
        {
            "@context": { "odrl": ODRL, "dpv": DPV, "ehds": EHDS, "hlth": HLTH, "ex": EX, "ac": AC },
            "uid": "urn:policy:primary-care-001",
            "type": "odrl:Policy",
            "odrl:permission": [{
                "uid": "urn:rule:pc-1",
                "odrl:action": "odrl:use",
                "odrl:target": "urn:asset:ehr",
                "odrl:constraint": [
                    { "odrl:leftOperand": "dpv:hasPurpose", "odrl:operator": "odrl:isAnyOf",
                      "odrl:rightOperandReference": ["hlth:PrimaryCareManagement","hlth:PatientRemoteMonitoring"] },
                    { "odrl:leftOperand": "dpv:hasRole",    "odrl:operator": "odrl:eq",
                      "odrl:rightOperandReference": "ex:Clinician" },
                    { "odrl:leftOperand": "dpv:hasPersonalDataCategory", "odrl:operator": "odrl:isAnyOf",
                      "odrl:rightOperandReference": ["ex:PATIENT_SUMMARY","ex:LAB_RESULTS"] }
                ]
            }]
        },
        # QI: secure_env + isAllOf(LAB_RESULTS,PATIENT_SUMMARY)
        {
            "@context": { "odrl": ODRL, "dpv": DPV, "ehds": EHDS, "hlth": HLTH, "ex": EX, "ac": AC },
            "uid": "urn:policy:qi-2025-aurora",
            "type": "odrl:Policy",
            "odrl:permission": [{
                "uid": "urn:rule:qi-1",
                "odrl:action": "odrl:use",
                "odrl:target": "urn:asset:ehr",
                "odrl:constraint": [
                    { "odrl:leftOperand": "dpv:hasPurpose", "odrl:operator": "odrl:eq",
                      "odrl:rightOperandReference": "ehds:EnsureQualitySafetyHealthcare" },
                    { "odrl:leftOperand": "ac:environment", "odrl:operator": "odrl:eq",
                      "odrl:rightOperand": "secure_env" },
                    { "odrl:leftOperand": "dpv:hasPersonalDataCategory", "odrl:operator": "odrl:isAllOf",
                      "odrl:rightOperandReference": ["ex:LAB_RESULTS","ex:PATIENT_SUMMARY"] }
                ],
                "odrl:duty": [ { "odrl:action": "ehds:requireConsent" }, { "odrl:action": "ehds:noExfiltration" } ]
            }]
        },
        # Research: secure_env + TOM Anonymisation + categories
        {
            "@context": { "odrl": ODRL, "dpv": DPV, "ehds": EHDS, "hlth": HLTH, "ex": EX, "ac": AC },
            "uid": "urn:policy:research-aurora-diabetes",
            "type": "odrl:Policy",
            "odrl:permission": [{
                "uid": "urn:rule:res-1",
                "odrl:action": "odrl:use",
                "odrl:target": "urn:asset:ehr",
                "odrl:constraint": [
                    { "odrl:leftOperand": "dpv:hasPurpose", "odrl:operator": "odrl:eq",
                      "odrl:rightOperandReference": "ehds:HealthcareScientificResearch" },
                    { "odrl:leftOperand": "ac:environment", "odrl:operator": "odrl:eq",
                      "odrl:rightOperand": "secure_env" },
                    { "odrl:leftOperand": "dpv:hasTechnicalOrganisationalMeasure", "odrl:operator": "odrl:isAnyOf",
                      "odrl:rightOperandReference": ["dpv:Anonymisation"] },
                    { "odrl:leftOperand": "dpv:hasPersonalDataCategory", "odrl:operator": "odrl:isAnyOf",
                      "odrl:rightOperandReference": ["ex:LAB_RESULTS","ex:PATIENT_SUMMARY","ex:IMAGING_REPORT"] }
                ],
                "odrl:duty": [ { "odrl:action": "ehds:annualOutcomeReport" }, { "odrl:action": "ehds:noReidentification" }, { "odrl:action": "ehds:noExfiltration" } ]
            }]
        },
        # Prohibition: Insurance management (DPV-Health)
        {
            "@context": { "odrl": ODRL, "dpv": DPV, "ehds": EHDS, "hlth": HLTH, "ex": EX, "ac": AC },
            "uid": "urn:policy:deny-insurance",
            "type": "odrl:Policy",
            "odrl:prohibition": [{
                "uid": "urn:rule:deny-insurance",
                "odrl:action": "odrl:use",
                "odrl:constraint": [
                    { "odrl:leftOperand": "dpv:hasPurpose", "odrl:operator": "odrl:eq",
                      "odrl:rightOperandReference": "hlth:InsuranceManagement" }
                ]
            }]
        }
    ]

# ----------------------------- Stubs: Consent & CareTeam -----------------------------
class ConsentService:
    # store[subject][purpose_iri] = True (opt-in) / False (opt-out)
    store: Dict[str, Dict[str, bool]] = {}

    @classmethod
    def set(cls, subject: str, purpose_iri: str, allowed: bool) -> None:
        cls.store.setdefault(subject, {})[purpose_iri] = allowed

    @classmethod
    def get(cls, subject: str, purpose_iri: str):
        return cls.store.get(subject, {}).get(purpose_iri)

class CareTeamService:
    links = set(["clinician_alba|ruben"])  # seed default link so primary care A works

    @classmethod
    def link(cls, clinician: str, subject: str) -> None:
        cls.links.add(f"{clinician}|{subject}")

    @classmethod
    def is_linked(cls, clinician: str, subject: str) -> bool:
        return f"{clinician}|{subject}" in cls.links

# ----------------------------- Engine -----------------------------
class ODRLEngine:
    ACTION_USE = "use"

    def __init__(self):
        pass

    @staticmethod
    def _expand_one(s: Any) -> Any:
        if not isinstance(s, str): return s
        mapping = [("ehds:", EHDS), ("dpv:", DPV), ("hlth:", HLTH), ("ex:", EX), ("ac:", AC), ("odrl:", ODRL)]
        for pfx, base in mapping:
            if s.startswith(pfx): return base + s[len(pfx):]
        return s

    @classmethod
    def _expand_operand(cls, rop: Any) -> Any:
        if isinstance(rop, list): return [cls._expand_one(v) for v in rop]
        return cls._expand_one(rop)

    @staticmethod
    def _op_name(op: str) -> str:
        if not op: return ""
        if op.startswith(ODRL): return op.split("/")[-1]
        if ":" in op: return op.split(":")[-1]
        return op

    def match(self, req: Dict[str, Any], policy: Dict[str, Any]) -> Tuple[bool, List[str], List[str]]:
        trace: List[str] = []
        obligations: List[str] = []

        # Prohibitions
        for pr in policy.get("odrl:prohibition", []) or []:
            if not self._action_ok(pr): continue
            ok, tr = self._constraints_hold(req, pr.get("odrl:constraint", []) or [])
            trace += self._prefix(policy, tr)
            if ok:
                trace.append(f'{policy["uid"]}:deny:odrl:prohibition_matched')
                return False, trace, obligations

        # Permissions
        for pm in policy.get("odrl:permission", []) or []:
            if not self._action_ok(pm): continue
            ok, tr = self._constraints_hold(req, pm.get("odrl:constraint", []) or [])
            trace += self._prefix(policy, tr)
            if ok:
                for d in pm.get("odrl:duty", []) or []:
                    act = d.get("odrl:action")
                    if act: obligations.append(f"duty:{act}")
                trace.append(f'{policy["uid"]}:permit:odrl:permission_matched')
                return True, trace, obligations

        trace.append(f'{policy["uid"]}:deny:odrl:no_permission_matched')
        return False, trace, obligations

    def _action_ok(self, rule: Dict[str, Any]) -> bool:
        a = rule.get("odrl:action")
        if isinstance(a, str):
            return a == "odrl:use" or a == ODRL + "use" or a == self.ACTION_USE
        return False

    def _constraints_hold(self, req: Dict[str, Any], constraints: List[Dict[str, Any]]) -> Tuple[bool, List[str]]:
        tr: List[str] = []
        for c in constraints:
            lop = c.get("odrl:leftOperand", "")
            op = self._op_name(c.get("odrl:operator", ""))
            rop_raw = c.get("odrl:rightOperandReference")
            if rop_raw is None: rop_raw = c.get("odrl:rightOperand")
            rop = self._expand_operand(rop_raw)
            ok, lhs = self._constraint_ok(req, lop, op, rop)
            if not ok:
                tr.append(f"constraint_failed:{lop}:{op}:lhs={json.dumps(lhs)},rightOperand={json.dumps(rop)}")
                return False, tr
            tr.append(f"constraint_ok:{lop}:{op}:lhs={json.dumps(lhs)},rightOperand={json.dumps(rop)}")
        return True, tr

    def _constraint_ok(self, req: Dict[str, Any], lop: str, op: str, rop: Any) -> Tuple[bool, Any]:
        def short(s: str) -> str:
            if not isinstance(s, str): return s
            if s.startswith(DPV): return "dpv:" + s[len(DPV):]
            if s.startswith(EHDS): return "ehds:" + s[len(EHDS):]
            if s.startswith(HLTH): return "hlth:" + s[len(HLTH):]
            if s.startswith(AC): return "ac:" + s[len(AC):]
            if s.startswith(ODRL): return "odrl:" + s[len(ODRL):]
            return s
        L = short(lop)
        if L == "dpv:hasPurpose":
            lhs = req["purpose"]
            return self._eval_op(lhs, op, rop), lhs
        if L == "dpv:hasRole":
            lhs = req["requester_role_iri"]
            return self._eval_op(lhs, op, rop), lhs
        if L == "dpv:hasPersonalDataCategory":
            lhs = req["categories_iri"]
            return self._eval_op(lhs, op, rop), lhs
        if L == "dpv:hasTechnicalOrganisationalMeasure":
            lhs = req.get("toms", [])
            return self._eval_op(lhs, op, rop), lhs
        if L == "ac:environment":
            lhs = req["environment"]
            return self._eval_op(lhs, op, rop), lhs
        return False, None

    def _eval_op(self, lhs: Any, op: str, rop: Any) -> bool:
        if op == "eq":
            return lhs == rop
        if op == "isAnyOf":
            if isinstance(rop, list):
                if isinstance(lhs, list): return any(x in rop for x in lhs)
                return lhs in rop
            return False
        if op == "isAllOf":
            # Correct semantics: required set (rop) must be subset of requested set (lhs)
            if isinstance(lhs, list) and isinstance(rop, list):
                return all(x in lhs for x in rop)
            if not isinstance(lhs, list) and isinstance(rop, list):
                return len(rop) == 1 and rop[0] == lhs
            return False
        return False

    @staticmethod
    def _prefix(policy: Dict[str, Any], lines: List[str]) -> List[str]:
        return [f'{policy["uid"]}:{ln}' for ln in lines]

# ----------------------------- PDP -----------------------------
class PDP:
    def __init__(self, policies: List[Dict[str, Any]]):
        self.policies = policies
        self.engine = ODRLEngine()
        self.PROHIBITED_PURPOSES = {PURPOSE["INSURANCE"]}

    def decide(self, req: Dict[str, Any]) -> Dict[str, Any]:
        trace: List[str] = []
        obligations: List[str] = []

        # R0: prohibited purpose
        if req["purpose"] in self.PROHIBITED_PURPOSES:
            trace.append("deny:prohibited_purpose")
            return self._finalize(req, "DENY", "Denied: the requested purpose (insurance management) is prohibited by policy.", trace, obligations)

        # Primary uses
        if req["purpose"] in PRIMARY_SET:
            if req["requester_role"] != "clinician":
                trace.append("deny:primary_only_for_clinicians")
                return self._finalize(req, "DENY", "Denied: primary-care access is limited to clinicians.", trace, obligations)
            trace.append("ok:role=CLINICIAN")
            if not CareTeamService.is_linked(req["requester_id"], req["subject_id"]):
                trace.append("deny:not_in_care_team")
                return self._finalize(req, "DENY", "Denied: requester is not linked to the patient's care team.", trace, obligations)
            trace.append("ok:careteam_link")
            ok, tr, obl = self._eval_policies(req)
            trace += tr; obligations += obl
            if ok:
                trace.append("permit:primary_care_allowed")
                return self._finalize(req, "PERMIT", "Permitted: clinician in the patient's care team, and the primary-care policy matched.", trace, obligations)
            return self._finalize(req, "DENY", "Denied: no primary-care policy matched for the requested scope.", trace, obligations)

        # AI training specific opt-out
        if req["purpose"] == PURPOSE["AI_TRAINING"]:
            ai_pref = ConsentService.get(req["subject_id"], PURPOSE["AI_TRAINING"])
            if ai_pref is False:
                trace.append("deny:subject_opted_out_ai_training")
                return self._finalize(req, "DENY", "Denied: you opted out of your data being used to train AI systems.", trace, obligations)

        # Secondary uses: check subject preference
        pref = ConsentService.get(req["subject_id"], req["purpose"])
        # For research we require explicit opt-in
        if req["purpose"] == PURPOSE["RESEARCH"] and pref is not True:
            trace.append("deny:no_subject_opt_in")
            return self._finalize(req, "DENY", "Denied: no explicit opt-in for this secondary use.", trace, obligations)
        # Generic opt-out always denies
        if pref is False:
            trace.append("deny:subject_opted_out")
            return self._finalize(req, "DENY", "Denied: the data subject has opted out of this secondary use.", trace, obligations)

        ok, tr, obl = self._eval_policies(req)
        trace += tr; obligations += obl
        if not ok:
            return self._finalize(req, "DENY", "Denied: no policy matched (purpose, environment, TOMs, or categories out of scope).", trace, obligations)
        return self._finalize(req, "PERMIT",
                              "Permitted: an ODRL/DPV policy matched for this secondary use." if req["purpose"] != PURPOSE["RESEARCH"]
                              else "Permitted: subject opted in and an ODRL/DPV policy matched (anonymised dataset in secure environment).",
                              trace, obligations)

    def _eval_policies(self, req: Dict[str, Any]) -> Tuple[bool, List[str], List[str]]:
        agg: List[str] = []
        obligations: List[str] = []
        for pol in self.policies:
            ok, tr, obl = self.engine.match(req, pol)
            agg += tr
            if ok:
                obligations += obl
                return True, agg, obligations
        return False, agg, obligations

    def _finalize(self, req: Dict[str, Any], answer: str, reason_why: str,
                  trace: List[str], obligations: List[str]) -> Dict[str, Any]:
        check = self._checks(req, answer, trace, obligations)
        decision = { "Answer": answer, "Reason why": reason_why, "Check": check, "Trace": trace }
        return decision

    def _checks(self, req: Dict[str, Any], answer: str, trace: List[str], obligations: List[str]) -> Dict[str, str]:
        # Helper: summarise matched policy / prohibition
        def summary():
            matched = None; duties = []; okAny = False; prohibition = False; matchedUid = None
            for pol in self.policies:
                ok, tr, obl = self.engine.match(req, pol)
                if any(t.endswith(":deny:odrl:prohibition_matched") for t in tr): prohibition = True
                if ok and not okAny:
                    okAny = True; matched = pol; duties = obl; matchedUid = pol["uid"]
            return okAny, matched, duties, prohibition, matchedUid

        okAny, matched, duties, prohibition, matchedUid = summary()
        hasCareteam = CareTeamService.is_linked(req["requester_id"], req["subject_id"])
        hasOptin = ConsentService.get(req["subject_id"], req["purpose"]) is True

        out: Dict[str, str] = {}

        # C1
        out["C1_prohibited_denied"] = ("OK - denied prohibited purpose" if (req["purpose"] in self.PROHIBITED_PURPOSES and answer == "DENY")
                                       else ("FAIL - prohibited purpose was not denied" if req["purpose"] in self.PROHIBITED_PURPOSES else "SKIPPED - not a prohibited purpose"))

        # C2/C3
        if req["purpose"] in PRIMARY_SET:
            out["C2_primary_role"] = ("OK - clinician" if req["requester_role"] == "clinician"
                                      else ("OK - non-clinician denied" if answer == "DENY" else "FAIL - non-clinician permitted"))
            out["C3_primary_careteam"] = ("OK - care-team linked" if (hasCareteam and answer == "PERMIT")
                                          else ("OK - denied due to missing care-team" if (not hasCareteam and answer == "DENY") else "FAIL - care-team rule inconsistent with answer"))
        else:
            out["C2_primary_role"] = "SKIPPED"
            out["C3_primary_careteam"] = "SKIPPED"

        # C4
        if req["purpose"] not in PRIMARY_SET and req["purpose"] != PURPOSE["INSURANCE"]:
            expectPermit = ((hasOptin or req["purpose"] == PURPOSE["QI"]) and okAny)
            out["C4_secondary_optin_and_policy"] = ("OK - opt-in present and policy matched" if (answer == "PERMIT" and expectPermit)
                                                    else ("OK - denied because opt-in missing or no policy match" if (answer == "DENY" and not expectPermit)
                                                          else ("FAIL - permitted without opt-in and matching policy" if answer == "PERMIT"
                                                                else "FAIL - denied despite opt-in and policy match")))
        else:
            out["C4_secondary_optin_and_policy"] = "SKIPPED"

        # C5 category scope
        if matched and answer == "PERMIT":
            perms = matched.get("odrl:permission") or []
            catOk = True; msg = ""
            if perms:
                cons = perms[0].get("odrl:constraint") or []
                catCons = [c for c in cons if (c.get("odrl:leftOperand","").endswith("hasPersonalDataCategory"))]
                if catCons:
                    op_raw = catCons[0].get("odrl:operator","")
                    op = op_raw.split(":")[-1] if ":" in op_raw else op_raw
                    allowed = ODRLEngine._expand_operand(catCons[0].get("odrl:rightOperandReference") or [])
                    reqCats = list(req["categories_iri"])
                    if op == "isAllOf":
                        catOk = all(x in reqCats for x in allowed)
                    elif op == "isAnyOf":
                        catOk = any(x in allowed for x in reqCats)
                    msg = f"operator={op}, allowed={json.dumps(allowed)}, requested={json.dumps(reqCats)}"
            out["C5_category_scope"] = (f"OK - {msg}" if catOk else f"FAIL - out of scope: {msg}")
        else:
            out["C5_category_scope"] = "SKIPPED"

        # C6 prohibition
        out["C6_prohibition_blocks"] = ("OK - denied due to prohibition" if (prohibition and answer == "DENY")
                                        else ("FAIL - permitted despite prohibition" if prohibition else "SKIPPED - no prohibition matched"))

        # C7 trace
        out["C7_trace_consistency"] = ("OK - trace shows matching permission" if (answer == "PERMIT" and (any(l.endswith(":permit:odrl:permission_matched") for l in trace) or "permit:primary_care_allowed" in trace))
                                       else ("SKIPPED" if answer != "PERMIT" else "FAIL - missing permission marker in trace"))

        # C8 duties
        out["C8_duties_present"] = (f"INFO - duties attached: {', '.join(duties)}" if duties else "SKIPPED - no matched policy or no duties")

        # C9 environment
        if matched and answer == "PERMIT":
            perms = matched.get("odrl:permission") or []
            envOk = True; msg = ""; had = False
            if perms:
                cons = perms[0].get("odrl:constraint") or []
                envCons = [c for c in cons if c.get("odrl:leftOperand") == "ac:environment"]
                if envCons:
                    had = True
                    op_raw = envCons[0].get("odrl:operator","")
                    op = op_raw.split(":")[-1] if ":" in op_raw else op_raw
                    allowed = envCons[0].get("odrl:rightOperand")
                    envOk = (req["environment"] == allowed) if op == "eq" else (allowed and req["environment"] in allowed)
                    msg = f"operator={op}, allowed={json.dumps(allowed)}, requested={json.dumps(req['environment'])}"
            out["C9_environment_scope"] = (f"OK - {msg}" if (had and envOk) else (f"FAIL - out of scope: {msg}" if had else "SKIPPED - policy has no environment constraint"))
        else:
            out["C9_environment_scope"] = "SKIPPED"

        # C10 policy UID
        out["C10_policy_uid"] = (f"INFO - matched policy: {matchedUid}" if matched else "SKIPPED - no matched policy")
        return out

# ----------------------------- Demo / scenarios -----------------------------
def build_request(requester_id: str, requester_role: str, subject_id: str,
                  purpose: str, environment: str, categories_local: List[str],
                  anonymised: bool=False) -> Dict[str, Any]:
    categories_iri = [EX + c for c in categories_local]
    toms = [DPV + "Anonymisation"] if anonymised else []
    return {
        "request_id": str(uuid.uuid4()),
        "requester_id": requester_id,
        "requester_role": requester_role,  # "clinician" | "data_user"
        "requester_role_iri": EX + ("Clinician" if requester_role == "clinician" else "DataUser"),
        "subject_id": subject_id,
        "purpose": purpose,
        "categories": categories_local,
        "categories_iri": categories_iri,
        "environment": environment,  # "api_gateway" | "secure_env"
        "toms": toms,
        "time": datetime.utcnow().isoformat() + "Z",
    }


def demo():
    policies = build_policies()
    pdp = PDP(policies)

    # Reset stubs
    ConsentService.store.clear()
    # seed default care-team links so A and E pass
    CareTeamService.links = set(["clinician_alba|ruben", "gp_ruben|ruben"])

    # Preferences
    # Research opt-in
    ConsentService.set("ruben", PURPOSE["RESEARCH"], True)
    # AI training opt-out (user preference)
    ConsentService.set("ruben", PURPOSE["AI_TRAINING"], False)

    scenarios = {
        "A_primary_care": build_request("clinician_alba", "clinician", "ruben",
                                        PURPOSE["PRIMARY_CARE"], "api_gateway", ["PATIENT_SUMMARY"]),
        "B_qi_in_scope": build_request("qi_analyst", "data_user", "ruben",
                                       PURPOSE["QI"], "secure_env", ["LAB_RESULTS","PATIENT_SUMMARY"]),
        "C_qi_out_of_scope": build_request("qi_analyst", "data_user", "ruben",
                                           PURPOSE["QI"], "secure_env", ["LAB_RESULTS"]),
        "D_insurance": build_request("insurer_bot", "data_user", "ruben",
                                     PURPOSE["INSURANCE"], "secure_env", ["PATIENT_SUMMARY"]),
        "E_gp_checks_labs": build_request("gp_ruben", "clinician", "ruben",
                                          PURPOSE["PRIMARY_CARE"], "api_gateway", ["LAB_RESULTS"]),
        "F_research_anon": build_request("researcher_aurora", "data_user", "ruben",
                                         PURPOSE["RESEARCH"], "secure_env", ["PATIENT_SUMMARY","LAB_RESULTS"], anonymised=True),
        "G_ai_training": build_request("ml_ops", "data_user", "ruben",
                                       PURPOSE["AI_TRAINING"], "secure_env", ["PATIENT_SUMMARY","LAB_RESULTS"]),
    }

    for name, req in scenarios.items():
        dec = pdp.decide(req)
        print("="*88)
        print(f"Scenario {name}")
        print(json.dumps(dec, indent=2))

if __name__ == "__main__":
    demo()
