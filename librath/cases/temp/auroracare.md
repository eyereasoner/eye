# AuroraCare — “Answer / Reason why / Check” harness (C1..C10)

This doc describes the **10-check harness** used by AuroraCare and gives **drop‑in code** for both the Python PDP and the HTML/JS demo. It matches what you now have in `auroracare.py` and `auroracare.html` (with C9/C10 added).

---

## TL;DR — What the harness does

Every decision returns three parts:

- **Answer** — `PERMIT` or `DENY`  
- **Reason why** — one human sentence aligned with the outcome  
- **Check** — a fixed set of sanity checks (`C1..C10`) with values like `OK`, `FAIL`, `SKIPPED`, or `INFO`

---

## The 10 checks

| Code | What it asserts | Applies when | OK when | Notes |
|---|---|---|---|---|
| **C1_prohibited_denied** | Prohibited purposes are denied | Always | Decision is `DENY` if prohibited | Short‑circuit gate |
| **C2_primary_role** | Primary use = clinician‑only | primary-care/remote-consult | Clinician → OK, else DENY | Role rule |
| **C3_primary_careteam** | Primary use requires care‑team link | primary-care/remote-consult | Linked+PERMIT, or Unlinked+DENY | Context rule |
| **C4_secondary_optin_and_policy** | Secondary use needs opt‑in **and** matching policy | Secondary, non‑prohibited | PERMIT only if (opt‑in ∧ policy match) | Consent × ODRL |
| **C5_category_scope** | Requested categories are within matched policy scope | If PERMIT & policy matched | Categories satisfy `isAnyOf`/`isAllOf` | From ODRL constraint |
| **C6_prohibition_blocks** | Any matched prohibition forces DENY | If any prohibition matched | Decision is `DENY` | ODRL prohibition |
| **C7_trace_consistency** | PERMIT decisions show a permission match in trace | If PERMIT | Trace has `...:permit:odrl:permission_matched` (or primary permit marker) | Sanity |
| **C8_duties_present** | Show matched duties (informational) | If duties exist | Lists duties | INFO/ SKIPPED |
| **C9_environment_scope** | Environment meets policy constraint | If PERMIT & policy constrains env | Requested env satisfies `eq`/`isAnyOf` | New |
| **C10_policy_uid** | Surface matched policy UID | If policy matched | `INFO - matched policy: <uid>` | New |

---

## 🔧 Drop‑in: Python (`auroracare.py`) — `_checks` method

> Replace the entire `_checks` method in class `PDP` with this.  
> **Important:** ensure all calls pass `obligations` as the 4th argument, e.g. `self._checks(req, "PERMIT", trace, obligations)`.

```python
def _checks(self, req: RequestContext, answer: str, trace: List[str], obligations: List[str]) -> Dict[str, str]:
    """
    C1..C10 harness
    """
    results: Dict[str, str] = {}

    # Summarize policy evaluation (first match)
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

    # C2/C3: primary requires clinician role + care-team link
    if req.purpose in {Purpose.PRIMARY_CARE, Purpose.REMOTE_CONSULT}:
        if req.requester_role != Role.CLINICIAN:
            results["C2_primary_role"] = "OK - non-clinician denied" if answer == "DENY" else "FAIL - non-clinician permitted"
        else:
            results["C2_primary_role"] = "OK - clinician"
        results["C3_primary_careteam"] = (
            "OK - care-team linked" if has_careteam and answer == "PERMIT"
            else ("OK - denied due to missing care-team" if (not has_careteam and answer == "DENY") else "FAIL - care-team rule inconsistent with answer")
        )
    else:
        results["C2_primary_role"] = "SKIPPED"
        results["C3_primary_careteam"] = "SKIPPED"

    # C4: secondary requires explicit opt-in + a matching ODRL permission
    if req.purpose not in {Purpose.PRIMARY_CARE, Purpose.REMOTE_CONSULT} and req.purpose not in self.PROHIBITED_PURPOSES:
        expect_permit = has_optin and ok_perm
        if answer == "PERMIT":
            results["C4_secondary_optin_and_policy"] = "OK - opt-in present and policy matched" if expect_permit else "FAIL - permitted without opt-in and matching policy"
        else:
            results["C4_secondary_optin_and_policy"] = "OK - denied because opt-in missing or no policy match" if not expect_permit else "FAIL - denied despite opt-in and policy match"
    else:
        results["C4_secondary_optin_and_policy"] = "SKIPPED"

    # C5: category scope (only if permitted and a policy matched)
    if matched_policy and answer == "PERMIT":
        perms = matched_policy.get("permission", []) or []
        cat_ok = True
        msg = ""
        if perms:
            cons = perms[0].get("constraint", [])
            cat_cons = [c for c in cons if c.get("leftOperand") in ("ehds:category", f"{EHDS}category") ]
            if cat_cons:
                op = cat_cons[0].get("operator"); allowed = cat_cons[0].get("rightOperand", [])
                req_cats = [c.name for c in req.categories]
                if op == "isAllOf":
                    cat_ok = set(req_cats).issubset(set(allowed))
                elif op == "isAnyOf":
                    cat_ok = any(c in allowed for c in req_cats)
                msg = f"operator={op}, allowed={allowed}, requested={req_cats}"
        results["C5_category_scope"] = ("OK - " + msg) if cat_ok else ("FAIL - out of scope: " + msg)
    else:
        results["C5_category_scope"] = "SKIPPED"

    # C6: if any prohibition matched, decision should be DENY
    if prohibition_hit:
        results["C6_prohibition_blocks"] = "OK - denied due to prohibition" if answer == "DENY" else "FAIL - permitted despite prohibition"
    else:
        results["C6_prohibition_blocks"] = "SKIPPED - no prohibition matched"

    # C7: trace consistency for PERMIT decisions
    if answer == "PERMIT":
        ok_trace = any(l.endswith(":permit:odrl:permission_matched") for l in trace) or ("permit:primary_care_allowed" in trace)
        results["C7_trace_consistency"] = "OK - trace shows matching permission" if ok_trace else "FAIL - missing permission marker in trace"
    else:
        results["C7_trace_consistency"] = "SKIPPED"

    # C8: duties presence (informational)
    results["C8_duties_present"] = ("INFO - duties attached: " + ", ".join(duties)) if duties else "SKIPPED - no matched policy or no duties"

    # C9: environment scope (if policy constrains environment)
    if matched_policy and answer == "PERMIT":
        perms = matched_policy.get("permission", []) or []
        env_ok = True
        msg = ""
        had_env = False
        if perms:
            cons = perms[0].get("constraint", [])
            env_cons = [c for c in cons if c.get("leftOperand") in ("ehds:environment", f"{EHDS}environment") ]
            if env_cons:
                had_env = True
                op = env_cons[0].get("operator"); allowed = env_cons[0].get("rightOperand")
                req_env = req.environment
                if op == "eq":
                    env_ok = (req_env == allowed)
                elif op == "isAnyOf":
                    env_ok = (req_env in allowed) if isinstance(allowed, list) else (req_env == allowed)
                msg = f"operator={op}, allowed={allowed}, requested={req_env}"
        results["C9_environment_scope"] = ("OK - " + msg) if (had_env and env_ok) else ("FAIL - out of scope: " + msg if had_env else "SKIPPED - policy has no environment constraint")
    else:
        results["C9_environment_scope"] = "SKIPPED"

    # C10: surface matched policy UID for audit
    results["C10_policy_uid"] = f"INFO - matched policy: {matched_policy.get('uid')}" if matched_policy else "SKIPPED - no matched policy"

    return results
```

---

## 🔧 Drop‑in: HTML/JS (`auroracare.html`) — `_checks` function

> In the `PDP` object, replace the `_checks(req, answer, trace, obligations)` function with this version.

```js
  _checks(req, answer, trace, obligations) {
    // policy summary helper
    const summary = () => {
      let matched = null, duties = [], okAny = false, prohibition = false, matchedUid = null;
      for (const pol of policies) {
        const [ok, tr, obl] = ODRLEngine.match(req, pol);
        if (tr.some(t => t.endsWith(":deny:odrl:prohibition_matched"))) prohibition = true;
        if (ok && !okAny) { okAny = true; matched = pol; duties = obl; matchedUid = pol.uid; }
      }
      return { okAny, matched, duties, prohibition, matchedUid };
    };
    const { okAny, matched, duties, prohibition, matchedUid } = summary();
    const hasCareteam = CareTeamService.isLinked(req.requester_id, req.subject_id);
    const hasOptin = ConsentService.get(req.subject_id, req.purpose) === true;

    const out = {};

    // C1
    if (this.PROHIBITED_PURPOSES.has(req.purpose)) out["C1_prohibited_denied"] = (answer === "DENY") ? "OK - denied prohibited purpose" : "FAIL - prohibited purpose was not denied";
    else out["C1_prohibited_denied"] = "SKIPPED - not a prohibited purpose";

    // C2/C3
    if (req.purpose === "primary-care" || req.purpose === "remote-consult") {
      out["C2_primary_role"] = (req.requester_role === "clinician") ? "OK - clinician" : ((answer === "DENY") ? "OK - non-clinician denied" : "FAIL - non-clinician permitted");
      out["C3_primary_careteam"] = (hasCareteam && answer === "PERMIT") ? "OK - care-team linked" : ((!hasCareteam && answer === "DENY") ? "OK - denied due to missing care-team" : "FAIL - care-team rule inconsistent with answer");
    } else {
      out["C2_primary_role"] = "SKIPPED"; out["C3_primary_careteam"] = "SKIPPED";
    }

    // C4
    if (!(req.purpose === "primary-care" || req.purpose === "remote-consult") && req.purpose !== "insurance-pricing") {
      const expectPermit = hasOptin && okAny;
      out["C4_secondary_optin_and_policy"] =
        (answer === "PERMIT")
          ? (expectPermit ? "OK - opt-in present and policy matched" : "FAIL - permitted without opt-in and matching policy")
          : (!expectPermit ? "OK - denied because opt-in missing or no policy match" : "FAIL - denied despite opt-in and policy match");
    } else out["C4_secondary_optin_and_policy"] = "SKIPPED";

    // C5
    if (matched && answer === "PERMIT") {
      const perms = matched.permission ?? [];
      let catOk = true, msg = "";
      if (perms.length) {
        const cons = perms[0].constraint ?? [];
        const catCons = cons.filter(c => c.leftOperand === "ehds:category" || c.leftOperand === `${EHDS}category`);
        if (catCons.length) {
          const op = catCons[0].operator, allowed = catCons[0].rightOperand ?? [];
          const reqCats = [...req.categories];
          if (op === "isAllOf") catOk = reqCats.every(x => allowed.includes(x));
          else if (op === "isAnyOf") catOk = reqCats.some(x => allowed.includes(x));
          msg = `operator=${op}, allowed=${JSON.stringify(allowed)}, requested=${JSON.stringify(reqCats)}`;
        }
      }
      out["C5_category_scope"] = catOk ? `OK - ${msg}` : `FAIL - out of scope: ${msg}`;
    } else out["C5_category_scope"] = "SKIPPED";

    // C6
    if (prohibition) out["C6_prohibition_blocks"] = (answer === "DENY") ? "OK - denied due to prohibition" : "FAIL - permitted despite prohibition";
    else out["C6_prohibition_blocks"] = "SKIPPED - no prohibition matched";

    // C7
    if (answer === "PERMIT") {
      const okTrace = trace.some(l => l.endsWith(":permit:odrl:permission_matched")) || trace.includes("permit:primary_care_allowed");
      out["C7_trace_consistency"] = okTrace ? "OK - trace shows matching permission" : "FAIL - missing permission marker in trace";
    } else out["C7_trace_consistency"] = "SKIPPED";

    // C8
    out["C8_duties_present"] = (duties && duties.length) ? `INFO - duties attached: ${duties.join(", ")}` : "SKIPPED - no matched policy or no duties";

    // C9
    if (matched && answer === "PERMIT") {
      const perms = matched.permission ?? [];
      let envOk = true, msg = ""; let hadEnv = false;
      if (perms.length) {
        const cons = perms[0].constraint ?? [];
        const envCons = cons.filter(c => c.leftOperand === "ehds:environment" || c.leftOperand === `${EHDS}environment`);
        if (envCons.length) {
          hadEnv = true;
          const op = envCons[0].operator, allowed = envCons[0].rightOperand;
          if (op === "eq") envOk = req.environment === allowed;
          else if (op === "isAnyOf") envOk = Array.isArray(allowed) ? allowed.includes(req.environment) : req.environment === allowed;
          msg = `operator=${op}, allowed=${JSON.stringify(allowed)}, requested=${JSON.stringify(req.environment)}`;
        }
      }
      out["C9_environment_scope"] = hadEnv ? (envOk ? `OK - ${msg}` : `FAIL - out of scope: ${msg}`) : "SKIPPED - policy has no environment constraint";
    } else out["C9_environment_scope"] = "SKIPPED";

    // C10
    out["C10_policy_uid"] = matched ? `INFO - matched policy: ${matchedUid}` : "SKIPPED - no matched policy";

    return out;
  }
```

---

## Example decision payload

```json
{
  "Answer": "PERMIT",
  "Reason why": "Permitted: subject opted in and an ODRL policy matched (purpose and requested categories in a secure environment). Duties are enforced as obligations.",
  "Check": {
    "C1_prohibited_denied": "SKIPPED - not a prohibited purpose",
    "C2_primary_role": "SKIPPED",
    "C3_primary_careteam": "SKIPPED",
    "C4_secondary_optin_and_policy": "OK - opt-in present and policy matched",
    "C5_category_scope": "OK - operator=isAllOf, allowed=["LAB_RESULTS","PATIENT_SUMMARY"], requested=["LAB_RESULTS"]",
    "C6_prohibition_blocks": "SKIPPED - no prohibition matched",
    "C7_trace_consistency": "OK - trace shows matching permission",
    "C8_duties_present": "INFO - duties attached: duty:ehds:requireConsent, duty:ehds:noExfiltration",
    "C9_environment_scope": "OK - operator=eq, allowed="secure_env", requested="secure_env"",
    "C10_policy_uid": "INFO - matched policy: urn:policy:HDAB-2025-001"
  }
}
```
