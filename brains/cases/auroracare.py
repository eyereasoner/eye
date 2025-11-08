#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
AuroraCare — Purpose-based Medical Data Exchange (ODRL/DPV + DPV-Health)
Self-contained Python CLI demo using Turtle policies (no external deps).

- Policies are authored in TURTLE (POLICY_TTL) and compiled at runtime into
  the in-memory structures expected by the ODRL engine below.
- Mirrors the behaviour of auroracare.html:
  * DPV/DPV-Health for primary uses (PrimaryCareManagement, PatientRemoteMonitoring)
  * EHDS purposes for QI / Research / AI training
  * Prohibition for InsuranceManagement
  * Category / Environment constraints
  * Duties surfaced as informational
  * Consent: research requires opt-in; AI training denied if opted-out
  * Primary care requires clinician role + care-team link
  * “Answer / Reason why / Check (C1..C10)” output
"""

from __future__ import annotations
import re
import json
from dataclasses import dataclass, field
from typing import Any, Dict, List, Optional, Tuple


# ----------------------------- Namespaces -----------------------------
ODRL = "http://www.w3.org/ns/odrl/2/"
DPV  = "https://w3id.org/dpv#"
EHDS = "https://w3id.org/dpv/legal/eu/ehds#"
HLTH = "https://w3id.org/dpv/sector/health#"
EX   = "https://example.org/health#"
AC   = "https://example.org/auroracare#"
RDF  = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
XSD  = "http://www.w3.org/2001/XMLSchema#"

PURPOSE = {
    "PRIMARY_CARE": HLTH + "PrimaryCareManagement",
    "REMOTE_CONSULT": HLTH + "PatientRemoteMonitoring",
    "RESEARCH": EHDS + "HealthcareScientificResearch",
    "QI": EHDS + "EnsureQualitySafetyHealthcare",
    "AI_TRAINING": EHDS + "TrainTestAndEvaluateAISystemsAlgorithms",
    "INSURANCE": HLTH + "InsuranceManagement",
}
PRIMARY_PURPOSES = {PURPOSE["PRIMARY_CARE"], PURPOSE["REMOTE_CONSULT"]}

CATEGORIES = ["PATIENT_SUMMARY", "LAB_RESULTS", "IMAGING_REPORT", "DISCHARGE_REPORT", "EPRESCRIPTION"]


# ----------------------------- Turtle policies (source of truth) -----------------------------
POLICY_TTL = r"""
@prefix odrl: <http://www.w3.org/ns/odrl/2/> .
@prefix dpv:  <https://w3id.org/dpv#> .
@prefix ehds: <https://w3id.org/dpv/legal/eu/ehds#> .
@prefix hlth: <https://w3id.org/dpv/sector/health#> .
@prefix ex:   <https://example.org/health#> .
@prefix ac:   <https://example.org/auroracare#> .
@prefix rdf:  <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix xsd:  <http://www.w3.org/2001/XMLSchema#> .

<urn:policy:primary-care-001> a odrl:Policy ;
  odrl:permission [
    a odrl:Permission ;
    odrl:action odrl:use ;
    odrl:target <urn:asset:ehr> ;
    odrl:constraint
      [ odrl:leftOperand dpv:hasPurpose ;
        odrl:operator odrl:isAnyOf ;
        odrl:rightOperandReference ( hlth:PrimaryCareManagement hlth:PatientRemoteMonitoring )
      ],
      [ odrl:leftOperand dpv:hasRole ;
        odrl:operator odrl:eq ;
        odrl:rightOperandReference ex:Clinician
      ],
      [ odrl:leftOperand dpv:hasPersonalDataCategory ;
        odrl:operator odrl:isAnyOf ;
        odrl:rightOperandReference ( ex:PATIENT_SUMMARY ex:LAB_RESULTS )
      ]
  ] .

<urn:policy:qi-2025-aurora> a odrl:Policy ;
  odrl:permission [
    a odrl:Permission ;
    odrl:action odrl:use ;
    odrl:target <urn:asset:ehr> ;
    odrl:constraint
      [ odrl:leftOperand dpv:hasPurpose ;
        odrl:operator odrl:eq ;
        odrl:rightOperandReference ehds:EnsureQualitySafetyHealthcare
      ],
      [ odrl:leftOperand ac:environment ;
        odrl:operator odrl:eq ;
        odrl:rightOperand "secure_env"^^xsd:string
      ],
      [ odrl:leftOperand dpv:hasPersonalDataCategory ;
        odrl:operator odrl:isAllOf ;
        odrl:rightOperandReference ( ex:LAB_RESULTS ex:PATIENT_SUMMARY )
      ] ;
    odrl:duty [ odrl:action ehds:requireConsent ],
              [ odrl:action ehds:noExfiltration ]
  ] .

<urn:policy:research-aurora-diabetes> a odrl:Policy ;
  odrl:permission [
    a odrl:Permission ;
    odrl:action odrl:use ;
    odrl:target <urn:asset:ehr> ;
    odrl:constraint
      [ odrl:leftOperand dpv:hasPurpose ;
        odrl:operator odrl:eq ;
        odrl:rightOperandReference ehds:HealthcareScientificResearch
      ],
      [ odrl:leftOperand ac:environment ;
        odrl:operator odrl:eq ;
        odrl:rightOperand "secure_env"^^xsd:string
      ],
      [ odrl:leftOperand dpv:hasTechnicalOrganisationalMeasure ;
        odrl:operator odrl:isAnyOf ;
        odrl:rightOperandReference ( dpv:Anonymisation )
      ],
      [ odrl:leftOperand dpv:hasPersonalDataCategory ;
        odrl:operator odrl:isAnyOf ;
        odrl:rightOperandReference ( ex:LAB_RESULTS ex:PATIENT_SUMMARY ex:IMAGING_REPORT )
      ] ;
    odrl:duty [ odrl:action ehds:annualOutcomeReport ],
              [ odrl:action ehds:noReidentification ],
              [ odrl:action ehds:noExfiltration ]
  ] .

<urn:policy:deny-insurance> a odrl:Policy ;
  odrl:prohibition [
    a odrl:Prohibition ;
    odrl:action odrl:use ;
    odrl:constraint
      [ odrl:leftOperand dpv:hasPurpose ;
        odrl:operator odrl:eq ;
        odrl:rightOperandReference hlth:InsuranceManagement
      ]
  ] .
"""


# ----------------------------- Tiny Turtle → Engine compiler (subset) -----------------------------
class TurtleCompileError(Exception):
    pass


@dataclass
class Rule:
    action: str
    target: Optional[str]
    constraints: List[Dict[str, Any]] = field(default_factory=list)
    duty: List[Dict[str, str]] = field(default_factory=list)


@dataclass
class Policy:
    uid: str
    permission: List[Rule] = field(default_factory=list)
    prohibition: List[Rule] = field(default_factory=list)


def _parse_prefixes(ttl: str) -> Dict[str, str]:
    prefixes: Dict[str, str] = {}
    for m in re.finditer(r'@prefix\s+([A-Za-z][\w\-]*):\s*<([^>]+)>\s*\.', ttl):
        prefixes[m.group(1)] = m.group(2)
    return prefixes


def _expand(token: str, prefixes: Dict[str, str]) -> str:
    token = token.strip()
    if not token:
        return token
    if token.startswith("<") and token.endswith(">"):
        return token[1:-1]
    if token.startswith('"'):  # "literal"^^xsd:string
        lit = re.match(r'^"([^"]*)"(?:\^\^([A-Za-z][\w\-]*):([A-Za-z][\w\-]*))?$', token)
        if lit:
            return lit.group(1)
        return token.strip('"')
    if ":" in token:
        pfx, local = token.split(":", 1)
        if pfx in prefixes:
            return prefixes[pfx] + local
    return token


def _tokenize_list(src: str) -> Tuple[List[str], int]:
    """Given a string starting with '(', return list of items and chars consumed."""
    assert src.lstrip().startswith("(")
    i = src.find("(")
    depth = 0
    buf = ""
    items: List[str] = []
    j = i
    while j < len(src):
        ch = src[j]
        if ch == "(":
            depth += 1
            if depth > 1:
                buf += ch
        elif ch == ")":
            depth -= 1
            if depth == 0:
                tok = buf.strip()
                if tok:
                    items.extend([t for t in re.split(r"\s+", tok) if t])
                j += 1
                break
            else:
                buf += ch
        else:
            buf += ch
        j += 1
    return items, j  # j is first char after ')'


def _extract_bracket_block(src: str, start: int, open_ch: str = "[", close_ch: str = "]") -> Tuple[str, int]:
    """Return content inside matching brackets starting at src[start] which must be open_ch."""
    if src[start] != open_ch:
        raise TurtleCompileError("Expected bracket at position {}".format(start))
    depth = 0
    j = start
    content = ""
    while j < len(src):
        ch = src[j]
        if ch == open_ch:
            depth += 1
            if depth > 1:
                content += ch
        elif ch == close_ch:
            depth -= 1
            if depth == 0:
                return content, j + 1
            else:
                content += ch
        else:
            content += ch
        j += 1
    raise TurtleCompileError("Unclosed bracket block")


def _re_find(token, t):
    """Find `token <value>` where the value may end with ';' or ']' or ',' or end-of-string."""
    return re.search(r'\b' + re.escape(token) + r'\s+([^\s;\],]+)\s*(?:[;\],]|$)', t)


def _parse_constraint_block(text: str, prefixes: Dict[str, str]) -> Dict[str, Any]:
    st: Dict[str, Any] = {}
    # normalize whitespace to make regex easier
    t = " ".join(text.replace("\n", " ").split())

    # leftOperand (in your TTL it always ends with ';')
    m = re.search(r'\bodrl:leftOperand\s+([^\s;]+)\s*;', t)
    if m:
        st["odrl:leftOperand"] = _expand(m.group(1), prefixes)

    # operator (accept ; or ] or ,)
    m = _re_find('odrl:operator', t)
    if m:
        st["odrl:operator"] = _expand(m.group(1), prefixes)

    # rightOperandReference: list form `( … )` OR single IRI without ';'
    m = re.search(r'\bodrl:rightOperandReference\s+\(', t)
    if m:
        s = t[m.end()-1:]                          # slice starting at '('
        items, _ = _tokenize_list(s)
        st["odrl:rightOperandReference"] = [_expand(it, prefixes) for it in items]
    else:
        m = _re_find('odrl:rightOperandReference', t)
        if m:
            st["odrl:rightOperandReference"] = _expand(m.group(1), prefixes)

    # rightOperand literal (accept ; or ] or ,)
    m = re.search(r'\bodrl:rightOperand\s+("[^"]*"(?:\^\^[A-Za-z][\w\-]*:[A-Za-z][\w\-]*)?)\s*(?:[;\],]|$)', t)
    if m:
        st["odrl:rightOperand"] = _expand(m.group(1), prefixes)

    return st


def _parse_rule_block(text: str, prefixes: Dict[str, str]) -> Rule:
    # Extract odrl:action, odrl:target, odrl:constraint [ ... ], odrl:duty [ ... ]
    action = None
    target = None
    constraints: List[Dict[str, Any]] = []
    duties: List[Dict[str, str]] = []

    # Make a scanning copy
    u = text

    # action
    m = re.search(r'\bodrl:action\s+([^\s;]+)\s*;', u)
    if m:
        action = _expand(m.group(1), prefixes)
    # target (optional)
    m = re.search(r'\bodrl:target\s+([^\s;]+)\s*;', u)
    if m:
        target = _expand(m.group(1), prefixes)

    # constraints: one or more [ ... ] blocks after 'odrl:constraint'
    for m in re.finditer(r'\bodrl:constraint\b', u):
        j = m.end()
        while j < len(u):
            while j < len(u) and u[j] in " \t\r\n,":
                j += 1
            if j >= len(u):
                break
            if u[j] == '[':
                content, j2 = _extract_bracket_block(u, j, "[", "]")
                constraints.append(_parse_constraint_block(content, prefixes))
                j = j2
                continue
            # stop when we reach ';' OR when the enclosing node ends with ']'
            if u[j] in ';]':
                break
            break

    # duties: odrl:duty [ odrl:action ... ] , [ ... ]
    for m in re.finditer(r'\bodrl:duty\b', u):
        idx = m.end()
        j = idx
        while j < len(u):
            while j < len(u) and u[j] in " \t\r\n,":
                j += 1
            if j >= len(u):
                break
            if u[j] == '[':
                content, j2 = _extract_bracket_block(u, j, "[", "]")
                mm = re.search(r'\bodrl:action\s+([^\s;]+)\s*', content)
                if mm:
                    duties.append({"odrl:action": _expand(mm.group(1), prefixes)})
                j = j2
                continue
            if u[j] == ';':
                break
            break

    if not action:
        raise TurtleCompileError("Rule missing odrl:action")
    return Rule(action=action, target=target, constraints=constraints, duty=duties)


def compile_policies_from_turtle(ttl: str) -> List[Policy]:
    prefixes = _parse_prefixes(ttl)
    # find each policy block
    blocks: List[Tuple[str, str]] = []  # (uid, text)
    for m in re.finditer(r'(?m)^(<urn:policy:[^>]+>)', ttl):
        start = m.start()
        uid_tok = m.group(1)
        # end at next policy start or EOF
        m2 = re.search(r'(?m)^(<urn:policy:[^>]+>)', ttl[m.end():])
        end = (m.end() + m2.start()) if m2 else len(ttl)
        block = ttl[start:end]
        blocks.append((_expand(uid_tok, prefixes), block))

    policies: List[Policy] = []
    for uid, block in blocks:
        pol = Policy(uid=uid)
        # permissions
        for m in re.finditer(r'\bodrl:permission\s*\[', block):
            content, j2 = _extract_bracket_block(block, m.end()-1, "[", "]")
            pol.permission.append(_parse_rule_block(content, prefixes))
        # prohibitions
        for m in re.finditer(r'\bodrl:prohibition\s*\[', block):
            content, j2 = _extract_bracket_block(block, m.end()-1, "[", "]")
            pol.prohibition.append(_parse_rule_block(content, prefixes))
        policies.append(pol)
    return policies


# ----------------------------- Services (stubs) -----------------------------
class ConsentService:
    store: Dict[str, Dict[str, bool]] = {}

    @classmethod
    def set(cls, subject: str, purpose_iri: str, allowed: bool) -> None:
        cls.store.setdefault(subject, {})[purpose_iri] = allowed

    @classmethod
    def get(cls, subject: str, purpose_iri: str) -> Optional[bool]:
        return cls.store.get(subject, {}).get(purpose_iri)


class CareTeamService:
    links: set[str] = set()

    @classmethod
    def link(cls, clinician: str, subject: str) -> None:
        cls.links.add(f"{clinician}|{subject}")

    @classmethod
    def unlink(cls, clinician: str, subject: str) -> None:
        cls.links.discard(f"{clinician}|{subject}")

    @classmethod
    def is_linked(cls, clinician: str, subject: str) -> bool:
        return f"{clinician}|{subject}" in cls.links


# ----------------------------- ODRL Engine -----------------------------
class ODRLEngine:
    @staticmethod
    def _op_name(op: str) -> str:
        if op is None:
            return ""
        if op.startswith(ODRL):
            return op.split("/")[-1]
        if ":" in op:
            return op.split(":")[-1]
        return op

    @staticmethod
    def _constraint_ok(req: Dict[str, Any], lop: str, op: str, rop: Any) -> Tuple[bool, Any]:
        """Return (ok, lhs)."""
        # Normalize lop to short for matching
        def short(s: str) -> str:
            if not isinstance(s, str):
                return s
            if s.startswith(DPV): return "dpv:" + s[len(DPV):]
            if s.startswith(EHDS): return "ehds:" + s[len(EHDS):]
            if s.startswith(HLTH): return "hlth:" + s[len(HLTH):]
            if s.startswith(AC): return "ac:" + s[len(AC):]
            if s.startswith(ODRL): return "odrl:" + s[len(ODRL):]
            return s

        L = short(lop)
        if L == "dpv:hasPurpose":
            lhs = req["purpose"]; return (ODRLEngine._eval_op(lhs, op, rop), lhs)
        if L == "dpv:hasRole":
            lhs = req["requester_role_iri"]; return (ODRLEngine._eval_op(lhs, op, rop), lhs)
        if L == "dpv:hasPersonalDataCategory":
            lhs = req["categories_iri"]; return (ODRLEngine._eval_op(lhs, op, rop), lhs)
        if L == "dpv:hasTechnicalOrganisationalMeasure":
            lhs = req.get("toms", []); return (ODRLEngine._eval_op(lhs, op, rop), lhs)
        if L == "ac:environment":
            lhs = req["environment"]; return (ODRLEngine._eval_op(lhs, op, rop), lhs)
        return (False, None)

    @staticmethod
    def _eval_op(lhs: Any, op: str, rop: Any) -> bool:
        opn = ODRLEngine._op_name(op)
        if opn == "eq":
            return lhs == rop
        if opn == "isAnyOf":
            if isinstance(rop, list):
                return (any(x in rop for x in lhs) if isinstance(lhs, list) else lhs in rop)
            return False
        if opn == "isAllOf":
            if isinstance(lhs, list) and isinstance(rop, list):
                return all(x in lhs for x in rop)
            if not isinstance(lhs, list) and isinstance(rop, list):
                return len(rop) == 1 and rop[0] == lhs
            return False
        return False

    @staticmethod
    def _constraints_hold(req: Dict[str, Any], constraints: List[Dict[str, Any]]) -> Tuple[bool, List[str]]:
        trace: List[str] = []
        for c in constraints:
            lop = c.get("odrl:leftOperand")
            op = c.get("odrl:operator")
            rop = c.get("odrl:rightOperandReference", c.get("odrl:rightOperand"))
            ok, lhs = ODRLEngine._constraint_ok(req, lop, op, rop)
            if not ok:
                trace.append(f"constraint_failed:{lop}:{ODRLEngine._op_name(op)}:lhs={json.dumps(lhs)},rightOperand={json.dumps(rop)}")
                return False, trace
            trace.append(f"constraint_ok:{lop}:{ODRLEngine._op_name(op)}:lhs={json.dumps(lhs)},rightOperand={json.dumps(rop)}")
        return True, trace

    @staticmethod
    def match(req: Dict[str, Any], policy: Policy) -> Tuple[bool, List[str], List[str]]:
        trace: List[str] = []
        obligations: List[str] = []

        # Prohibitions first
        for pr in policy.prohibition:
            if not (pr.action == ODRL + "use" or pr.action == "odrl:use" or pr.action == "use"):
                continue
            ok, tr = ODRLEngine._constraints_hold(req, pr.constraints)
            trace += [f"{policy.uid}:{ln}" for ln in tr]
            if ok:
                trace.append(f"{policy.uid}:deny:odrl:prohibition_matched")
                return False, trace, obligations

        # Permissions
        for pm in policy.permission:
            if not (pm.action == ODRL + "use" or pm.action == "odrl:use" or pm.action == "use"):
                continue
            ok, tr = ODRLEngine._constraints_hold(req, pm.constraints)
            trace += [f"{policy.uid}:{ln}" for ln in tr]
            if ok:
                for d in pm.duty:
                    act = d.get("odrl:action")
                    if act:
                        obligations.append(f"duty:{act}")
                trace.append(f"{policy.uid}:permit:odrl:permission_matched")
                return True, trace, obligations

        trace.append(f"{policy.uid}:deny:odrl:no_permission_matched")
        return False, trace, obligations


# ----------------------------- PDP with “Answer / Reason why / Check” -----------------------------
class PDP:
    PROHIBITED_PURPOSES = {PURPOSE["INSURANCE"]}

    def __init__(self, policies: List[Policy]) -> None:
        self.policies = policies

    def _eval_policies(self, req: Dict[str, Any]) -> Tuple[bool, List[str], List[str]]:
        agg: List[str] = []
        obligations: List[str] = []
        for pol in self.policies:
            ok, tr, obl = ODRLEngine.match(req, pol)
            agg += tr
            if ok:
                obligations += obl
                return True, agg, obligations
        return False, agg, obligations

    def _checks(self, req: Dict[str, Any], answer: str, trace: List[str], obligations: List[str]) -> Dict[str, str]:
        # summarize policy match
        def summary():
            matched = None
            duties: List[str] = []
            okAny = False
            prohibition = False
            matchedUid = None
            for pol in self.policies:
                ok, tr, obl = ODRLEngine.match(req, pol)
                if any(t.endswith(":deny:odrl:prohibition_matched") for t in tr):
                    prohibition = True
                if ok and not okAny:
                    okAny = True; matched = pol; duties = obl; matchedUid = pol.uid
            return okAny, matched, duties, prohibition, matchedUid

        okAny, matched, duties, prohibition, matchedUid = summary()
        hasCareteam = CareTeamService.is_linked(req["requester_id"], req["subject_id"])
        hasOptin = ConsentService.get(req["subject_id"], req["purpose"]) is True

        out: Dict[str, str] = {}

        # C1
        if req["purpose"] in self.PROHIBITED_PURPOSES:
            out["C1_prohibited_denied"] = "OK - denied prohibited purpose" if answer == "DENY" else "FAIL - prohibited purpose was not denied"
        else:
            out["C1_prohibited_denied"] = "SKIPPED - not a prohibited purpose"

        # C2/C3 (primary)
        if req["purpose"] in PRIMARY_PURPOSES:
            out["C2_primary_role"] = ("OK - clinician" if req["requester_role"] == "clinician"
                                      else ("OK - non-clinician denied" if answer == "DENY" else "FAIL - non-clinician permitted"))
            out["C3_primary_careteam"] = ("OK - care-team linked" if hasCareteam and answer == "PERMIT"
                                          else ("OK - denied due to missing care-team" if (not hasCareteam and answer == "DENY")
                                                else "FAIL - care-team rule inconsistent with answer"))
        else:
            out["C2_primary_role"] = "SKIPPED"
            out["C3_primary_careteam"] = "SKIPPED"

        # C4 (secondary opt-in + policy)
        if (req["purpose"] not in PRIMARY_PURPOSES) and (req["purpose"] not in self.PROHIBITED_PURPOSES):
            expectPermit = ((hasOptin or req["purpose"] == PURPOSE["QI"]) and okAny)
            out["C4_secondary_optin_and_policy"] = (
                "OK - opt-in present and policy matched" if (answer == "PERMIT" and expectPermit)
                else ("OK - denied because opt-in missing or no policy match" if (answer == "DENY" and not expectPermit)
                      else ("FAIL - permitted without opt-in and matching policy" if answer == "PERMIT" else "FAIL - denied despite opt-in and policy match"))
            )
        else:
            out["C4_secondary_optin_and_policy"] = "SKIPPED"

        # C5 (category scope when permitted)
        if matched and answer == "PERMIT":
            perms = matched.permission or []
            catOk = True; msg = ""
            if perms:
                cons = perms[0].constraints or []
                catCons = [c for c in cons if (c.get("odrl:leftOperand") or "").endswith("hasPersonalDataCategory")]
                if catCons:
                    op = ODRLEngine._op_name(catCons[0].get("odrl:operator"))
                    allowed = catCons[0].get("odrl:rightOperandReference") or []
                    reqCats = list(req["categories_iri"])
                    if op == "isAllOf":
                        catOk = all(x in reqCats for x in allowed)
                    elif op == "isAnyOf":
                        catOk = any(x in allowed for x in reqCats)
                    msg = f"operator={op}, allowed={json.dumps(allowed)}, requested={json.dumps(reqCats)}"
            out["C5_category_scope"] = f"OK - {msg}" if catOk else f"FAIL - out of scope: {msg}"
        else:
            out["C5_category_scope"] = "SKIPPED"

        # C6 prohibition
        out["C6_prohibition_blocks"] = ("OK - denied due to prohibition" if (prohibition and answer == "DENY")
                                        else ("FAIL - permitted despite prohibition" if (prohibition and answer == "PERMIT")
                                              else "SKIPPED - no prohibition matched"))

        # C7 trace consistency for permits
        out["C7_trace_consistency"] = ("OK - trace shows matching permission"
                                       if (answer == "PERMIT" and (any(l.endswith(":permit:odrl:permission_matched") for l in trace)
                                                                   or "permit:primary_care_allowed" in trace))
                                       else ("SKIPPED" if answer != "PERMIT" else "FAIL - missing permission marker in trace"))

        # C8 duties info
        out["C8_duties_present"] = (f"INFO - duties attached: {', '.join(duties)}" if duties else "SKIPPED - no matched policy or no duties")

        # C9 environment scope (if constrained)
        if matched and answer == "PERMIT":
            perms = matched.permission or []
            envOk = True; msg = ""; had = False
            if perms:
                cons = perms[0].constraints or []
                envCons = [c for c in cons if (c.get("odrl:leftOperand") == AC + "environment")]
                if envCons:
                    had = True
                    op = ODRLEngine._op_name(envCons[0].get("odrl:operator"))
                    allowed = envCons[0].get("odrl:rightOperand")
                    if op == "eq":
                        envOk = (req["environment"] == allowed)
                    elif op == "isAnyOf":
                        envOk = (req["environment"] in (allowed if isinstance(allowed, list) else [allowed]))
                    msg = f"operator={op}, allowed={json.dumps(allowed)}, requested={json.dumps(req['environment'])}"
            out["C9_environment_scope"] = (f"OK - {msg}" if had and envOk else
                                           (f"FAIL - out of scope: {msg}" if had else "SKIPPED - policy has no environment constraint"))
        else:
            out["C9_environment_scope"] = "SKIPPED"

        # C10 policy uid surfaced
        out["C10_policy_uid"] = (f"INFO - matched policy: {matchedUid}" if matched else "SKIPPED - no matched policy")

        return out

    def _finalize(self, req: Dict[str, Any], answer: str, reason_why: str, trace: List[str], obligations: List[str]) -> Dict[str, Any]:
        return {"Answer": answer, "Reason why": reason_why, "Check": self._checks(req, answer, trace, obligations)}

    def decide(self, req: Dict[str, Any]) -> Dict[str, Any]:
        trace: List[str] = []
        obligations: List[str] = []

        # Prohibited outright
        if req["purpose"] in self.PROHIBITED_PURPOSES:
            trace.append("deny:prohibited_purpose")
            return self._finalize(req, "DENY", "Denied: the requested purpose (insurance management) is prohibited by policy.", trace, obligations)

        # Primary uses (clinician + care-team link)
        if req["purpose"] in PRIMARY_PURPOSES:
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

        # AI training opt-out check
        if req["purpose"] == PURPOSE["AI_TRAINING"]:
            ai_pref = ConsentService.get(req["subject_id"], PURPOSE["AI_TRAINING"])
            if ai_pref is False:
                trace.append("deny:subject_opted_out_ai_training")
                return self._finalize(req, "DENY", "Denied: you opted out of your data being used to train AI systems.", trace, obligations)

        # Secondary: research requires opt-in
        pref = ConsentService.get(req["subject_id"], req["purpose"])
        if req["purpose"] == PURPOSE["RESEARCH"] and pref is not True:
            trace.append("deny:no_subject_opt_in")
            return self._finalize(req, "DENY", "Denied: no explicit opt-in for this secondary use.", trace, obligations)
        if pref is False:
            trace.append("deny:subject_opted_out")
            return self._finalize(req, "DENY", "Denied: the data subject has opted out of this secondary use.", trace, obligations)

        ok, tr, obl = self._eval_policies(req)
        trace += tr; obligations += obl
        if not ok:
            return self._finalize(req, "DENY", "Denied: no policy matched (purpose, environment, TOMs, or categories out of scope).", trace, obligations)

        return self._finalize(
            req, "PERMIT",
            ("Permitted: subject opted in and an ODRL/DPV policy matched (anonymised dataset in secure environment)."
             if req["purpose"] == PURPOSE["RESEARCH"]
             else "Permitted: ODRL/DPV policy matched for secondary use."),
            trace, obligations
        )


# ----------------------------- Request builder & scenarios -----------------------------
def build_request(
    requester_id: str,
    requester_role: str,
    subject_id: str,
    purpose_iri: str,
    categories: List[str],
    environment: str,
    anonymised: bool = False
) -> Dict[str, Any]:
    return {
        "request_id": f"req_{requester_id}_{purpose_iri.split('#')[-1]}",
        "requester_id": requester_id,
        "requester_role": requester_role,  # "clinician" | "data_user"
        "requester_role_iri": EX + ("Clinician" if requester_role == "clinician" else "DataUser"),
        "subject_id": subject_id,
        "purpose": purpose_iri,
        "categories": list(categories),
        "categories_iri": [EX + c for c in categories],
        "environment": environment,
        "toms": [DPV + "Anonymisation"] if anonymised else [],
    }


def build_policies() -> List[Policy]:
    return compile_policies_from_turtle(POLICY_TTL)


# ----------------------------- Demo / CLI -----------------------------
def demo() -> None:
    policies = build_policies()
    pdp = PDP(policies)

    # Reset services
    ConsentService.store.clear()
    CareTeamService.links = set(["clinician_alba|ruben", "gp_ruben|ruben"])

    # Preferences
    ConsentService.set("ruben", PURPOSE["RESEARCH"], True)      # default: research opt-in
    ConsentService.set("ruben", PURPOSE["AI_TRAINING"], False)  # opted-out of AI training

    # Scenarios mirroring the browser:
    scenarios = {
        "A_primary": build_request("clinician_alba", "clinician", "ruben", PURPOSE["PRIMARY_CARE"], ["PATIENT_SUMMARY"], "api_gateway"),
        "B_qi_in_scope": build_request("qi_analyst", "data_user", "ruben", PURPOSE["QI"], ["LAB_RESULTS", "PATIENT_SUMMARY"], "secure_env"),
        "C_qi_out_scope": build_request("qi_analyst", "data_user", "ruben", PURPOSE["QI"], ["LAB_RESULTS"], "secure_env"),
        "D_insurance_prohibited": build_request("insurer_bot", "data_user", "ruben", PURPOSE["INSURANCE"], ["PATIENT_SUMMARY"], "secure_env"),
        "E_gp_checks_labs": build_request("gp_ruben", "clinician", "ruben", PURPOSE["PRIMARY_CARE"], ["LAB_RESULTS"], "api_gateway"),
        "F_research_anonymised": build_request("researcher_aurora", "data_user", "ruben", PURPOSE["RESEARCH"], ["PATIENT_SUMMARY", "LAB_RESULTS"], "secure_env", anonymised=True),
        "G_ai_training_optout": build_request("ml_ops", "data_user", "ruben", PURPOSE["AI_TRAINING"], ["PATIENT_SUMMARY", "LAB_RESULTS"], "secure_env"),
    }

    expected = {
        "A_primary": "PERMIT",
        "B_qi_in_scope": "PERMIT",
        "C_qi_out_scope": "DENY",
        "D_insurance_prohibited": "DENY",
        "E_gp_checks_labs": "PERMIT",
        "F_research_anonymised": "PERMIT",
        "G_ai_training_optout": "DENY",
    }

    for name, req in scenarios.items():
        dec = pdp.decide(req)
        ok = "✓" if dec["Answer"] == expected[name] else "✗"
        print(f"\n=== {name} === [{ok}]")
        print(json.dumps(dec, indent=2))


# ----------------------------- Entry -----------------------------
if __name__ == "__main__":
    demo()

