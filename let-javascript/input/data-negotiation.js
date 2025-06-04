#!/usr/bin/env node
/* ------------------------------------------------------------------
 *  Clinical-data Negotiation Demo (hard-coded tests, no I/O)
 *  ---------------------------------------------------------------
 *  ❶ Enumerations for data fields, purposes, results
 *  ❷ Provider-side policy table (one demo hospital)
 *  ❸ Core negotiate() function
 *  ❹ A bundle of canned requests to exercise the logic
 * ------------------------------------------------------------------ */

// ── ❶ Vocab ────────────────────────────────────────────────────────
const DataField = Object.freeze({
  PATIENT_ID:   'PATIENT_ID',
  DOB:          'DOB',
  DIAGNOSIS:    'DIAGNOSIS',
  LAB_RESULTS:  'LAB_RESULTS'
});

const FieldLabel = Object.freeze({
  [DataField.PATIENT_ID]:  'Patient-ID',
  [DataField.DOB]:         'Date-of-Birth',
  [DataField.DIAGNOSIS]:   'Diagnosis',
  [DataField.LAB_RESULTS]: 'Lab-Results'
});

const Purpose = Object.freeze({
  TREATMENT: 'TREATMENT',
  RESEARCH:  'RESEARCH',
  BILLING:   'BILLING'
});

const PurposeLabel = Object.freeze({
  [Purpose.TREATMENT]: 'Treatment',
  [Purpose.RESEARCH]:  'Research',
  [Purpose.BILLING]:   'Billing'
});

const Result = Object.freeze({
  GRANTED:       'GRANTED',
  GRANTED_DEID:  'GRANTED-WITH-DEID',
  DENIED:        'DENIED'
});

// ── ❷ Provider policy table ───────────────────────────────────────
/* For each field:
 *   allow[purpose]       → true / false
 *   requireConsent       → bool
 *   allowIdentifiable    → bool
 */
function makeDemoProvider() {
  return {
    name: 'Hospital-A',
    rules: {
      [DataField.PATIENT_ID]: {
        allow: { [Purpose.TREATMENT]: true, [Purpose.RESEARCH]: false, [Purpose.BILLING]: true },
        requireConsent: true,
        allowIdentifiable: true
      },
      [DataField.DOB]: {
        allow: { [Purpose.TREATMENT]: true, [Purpose.RESEARCH]: true, [Purpose.BILLING]: true },
        requireConsent: false,
        allowIdentifiable: false
      },
      [DataField.DIAGNOSIS]: {
        allow: { [Purpose.TREATMENT]: true, [Purpose.RESEARCH]: true, [Purpose.BILLING]: true },
        requireConsent: false,
        allowIdentifiable: true
      },
      [DataField.LAB_RESULTS]: {
        allow: { [Purpose.TREATMENT]: true, [Purpose.RESEARCH]: true, [Purpose.BILLING]: false },
        requireConsent: true,
        allowIdentifiable: false
      }
    }
  };
}

// ── ❸ Core negotiation algorithm ─────────────────────────────────
function negotiate(provider, request) {
  const rule = provider.rules[request.field];

  // 1️⃣ Purpose gate
  if (!rule.allow[request.purpose]) {
    return Result.DENIED;
  }

  // 2️⃣ Consent check
  if (rule.requireConsent && !request.patientConsent) {
    return Result.DENIED;
  }

  // 3️⃣ Identifiability check
  if (request.wantsIdentifiable) {
    return rule.allowIdentifiable ? Result.GRANTED : Result.DENIED;
  }
  return Result.GRANTED_DEID;
}

// ── ❹ Hard-coded test cases ───────────────────────────────────────
const provider = makeDemoProvider();

const tests = [
  /* 1 */ { requester: 'Dr.Smith',   field: DataField.DIAGNOSIS,   purpose: Purpose.TREATMENT,
            wantsIdentifiable: true,  patientConsent: false },

  /* 2 */ { requester: 'ResearchLab', field: DataField.DOB,         purpose: Purpose.RESEARCH,
            wantsIdentifiable: true,  patientConsent: true  },

  /* 3 */ { requester: 'ResearchLab', field: DataField.DOB,         purpose: Purpose.RESEARCH,
            wantsIdentifiable: false, patientConsent: true  },

  /* 4 */ { requester: 'ResearchLab', field: DataField.LAB_RESULTS, purpose: Purpose.RESEARCH,
            wantsIdentifiable: false, patientConsent: false },

  /* 5 */ { requester: 'BillingSvc',  field: DataField.PATIENT_ID,  purpose: Purpose.BILLING,
            wantsIdentifiable: true,  patientConsent: false },

  /* 6 */ { requester: 'ResearchLab', field: DataField.PATIENT_ID,  purpose: Purpose.RESEARCH,
            wantsIdentifiable: true,  patientConsent: false },

  /* 7 */ { requester: 'ResearchLab', field: DataField.PATIENT_ID,  purpose: Purpose.RESEARCH,
            wantsIdentifiable: false, patientConsent: true  }
];

// ── Runner -- pretty-print results ────────────────────────────────
console.log('=== Clinical-data Negotiation Demo =========================\n');

for (const req of tests) {
  const res = negotiate(provider, req);

  console.log(`Request by ${req.requester.padEnd(12)}  `
    + `Field=${FieldLabel[req.field].padEnd(13)}  `
    + `Purpose=${PurposeLabel[req.purpose].padEnd(9)}  `
    + `Identifiable=${req.wantsIdentifiable ? 'yes' : 'no '}  `
    + `Consent=${req.patientConsent ? 'yes' : 'no '}  ->  ${res}`);
}

console.log('\n==============================================================');
