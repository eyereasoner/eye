/* ------------------------------------------------------------------
 *  Clinical-data Negotiation Demo  (TypeScript 4.x)
 *  ---------------------------------------------------------------
 *  $ tsc data-negotiation.ts --target es2020 --lib es2020
 *  $ node data-negotiation.js
 * ------------------------------------------------------------------ */

enum DataField {
  PATIENT_ID  = 'PATIENT_ID',
  DOB         = 'DOB',
  DIAGNOSIS   = 'DIAGNOSIS',
  LAB_RESULTS = 'LAB_RESULTS'
}

const FieldLabel: Record<DataField, string> = {
  [DataField.PATIENT_ID]:  'Patient-ID',
  [DataField.DOB]:         'Date-of-Birth',
  [DataField.DIAGNOSIS]:   'Diagnosis',
  [DataField.LAB_RESULTS]: 'Lab-Results'
};

enum Purpose {
  TREATMENT = 'TREATMENT',
  RESEARCH  = 'RESEARCH',
  BILLING   = 'BILLING'
}

const PurposeLabel: Record<Purpose, string> = {
  [Purpose.TREATMENT]: 'Treatment',
  [Purpose.RESEARCH]:  'Research',
  [Purpose.BILLING]:   'Billing'
};

enum Result {
  GRANTED      = 'GRANTED',
  GRANTED_DEID = 'GRANTED-WITH-DEID',
  DENIED       = 'DENIED'
}

/* ---------- Provider-side policy types --------------------------- */
type AllowMap = Record<Purpose, boolean>;

interface FieldPolicy {
  allow: AllowMap;
  requireConsent: boolean;
  allowIdentifiable: boolean;
}

interface ProviderPolicy {
  name: string;
  rules: Record<DataField, FieldPolicy>;
}

/* ---------- Build the fixed rule matrix -------------------------- */
function makeDemoProvider(): ProviderPolicy {
  const allowAll: AllowMap = {
    [Purpose.TREATMENT]: true,
    [Purpose.RESEARCH]:  true,
    [Purpose.BILLING]:   true
  };

  return {
    name: 'Hospital-A',
    rules: {
      [DataField.PATIENT_ID]: {
        allow:            { ...allowAll },
        requireConsent:   true,
        allowIdentifiable:true
      },
      [DataField.DOB]: {
        allow:            { ...allowAll },
        requireConsent:   false,
        allowIdentifiable:false
      },
      [DataField.DIAGNOSIS]: {
        allow:            { ...allowAll },
        requireConsent:   false,
        allowIdentifiable:true
      },
      [DataField.LAB_RESULTS]: {
        allow: {
          [Purpose.TREATMENT]: true,
          [Purpose.RESEARCH]:  true,
          [Purpose.BILLING]:   false
        },
        requireConsent:   true,
        allowIdentifiable:false
      }
    }
  };
}

/* ---------- Our own request type (renamed!) ---------------------- */
interface DataRequest {
  requester: string;
  field: DataField;
  purpose: Purpose;
  wantsIdentifiable: boolean;
  patientConsent: boolean;
}

/* ---------- Negotiation algorithm -------------------------------- */
function negotiate(rule: FieldPolicy, req: DataRequest): Result {
  if (!rule.allow[req.purpose]) return Result.DENIED;          // 1. purpose
  if (rule.requireConsent && !req.patientConsent) return Result.DENIED; // 2. consent
  if (req.wantsIdentifiable)
    return rule.allowIdentifiable ? Result.GRANTED : Result.DENIED;     // 3a raw
  return Result.GRANTED_DEID;                                           // 3b de-id
}

/* ---------- Hard-coded test cases -------------------------------- */
function main(): void {
  const provider = makeDemoProvider();

  const tests: DataRequest[] = [
    { requester:'Dr.Smith',    field:DataField.DIAGNOSIS,   purpose:Purpose.TREATMENT,
      wantsIdentifiable:true,  patientConsent:false },

    { requester:'ResearchLab', field:DataField.DOB,         purpose:Purpose.RESEARCH,
      wantsIdentifiable:true,  patientConsent:true },

    { requester:'ResearchLab', field:DataField.DOB,         purpose:Purpose.RESEARCH,
      wantsIdentifiable:false, patientConsent:true },

    { requester:'ResearchLab', field:DataField.LAB_RESULTS, purpose:Purpose.RESEARCH,
      wantsIdentifiable:false, patientConsent:false },

    { requester:'BillingSvc',  field:DataField.PATIENT_ID,  purpose:Purpose.BILLING,
      wantsIdentifiable:true,  patientConsent:false },

    { requester:'ResearchLab', field:DataField.PATIENT_ID,  purpose:Purpose.RESEARCH,
      wantsIdentifiable:true,  patientConsent:false },

    { requester:'ResearchLab', field:DataField.PATIENT_ID,  purpose:Purpose.RESEARCH,
      wantsIdentifiable:false, patientConsent:true }
  ];

  console.log('=== Clinical-data Negotiation Demo =========================\n');
  for (const req of tests) {
    const res = negotiate(provider.rules[req.field], req);
    console.log(
      `Request by ${req.requester.padEnd(12)}  ` +
      `Field=${FieldLabel[req.field].padEnd(13)}  ` +
      `Purpose=${PurposeLabel[req.purpose].padEnd(9)}  ` +
      `Identifiable=${req.wantsIdentifiable ? 'yes' : 'no '}  ` +
      `Consent=${req.patientConsent ? 'yes' : 'no '}  ->  ${res}`
    );
  }
  console.log('\n=============================================================');
}

main();
