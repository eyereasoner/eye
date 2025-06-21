/*
 * ---------------------------------------------------------------
 *  Clinical-data Negotiation Demo
 *  --------------------------------
 *  • Hard-coded policies & requests (no interactive input)
 *  • Very small subset of real-world rules, but shows the pattern:
 *      − per-field privacy requirements
 *      − purpose-of-use checks (Treatment / Research / Billing)
 *      − patient-consent gating
 *      − “identifiable” vs “de-identified” negotiation outcome
 *
 *  Compile:  gcc -std=c11 -Wall -Wextra -O2 negotiate.c -o negotiate
 *  Run:      ./negotiate
 * ---------------------------------------------------------------
 */

#include <stdio.h>
#include <stdbool.h>

/* ---------- vocab ------------------------------------------------ */
typedef enum {
    FIELD_ID,
    FIELD_DOB,          /* date of birth */
    FIELD_DIAGNOSIS,
    FIELD_LAB_RESULTS,
    NUM_FIELDS
} DataField;

typedef enum {
    PURP_TREATMENT,
    PURP_RESEARCH,
    PURP_BILLING,
    NUM_PURPOSES
} Purpose;

typedef enum {
    NEG_GRANTED,            /* request satisfied exactly    */
    NEG_GRANTED_DEID,       /* granted but must be de-id    */
    NEG_DENIED
} NegotiationResult;

/* ---------- provider-side policy definitions -------------------- */
typedef struct {
    DataField field;

    /* sharing switches (per purpose) */
    bool allow[NUM_PURPOSES];

    /* does the provider require patient consent for this field?    */
    bool requireConsent;

    /* may the field leave the org in identifiable form?            */
    bool allowIdentifiable;
} FieldPolicy;

/* One provider holds a small, fixed table of policies */
typedef struct {
    const char      *name;
    FieldPolicy      rules[NUM_FIELDS];
} ProviderPolicy;

/* ---------- requester-side negotiation request ------------------ */
typedef struct {
    const char *requester;
    DataField   field;
    Purpose     purpose;
    bool        wantsIdentifiable;    /* true ⇒ raw; false ⇒ OK with de-id   */
    bool        patientConsent;       /* documentation already obtained?     */
} DataRequest;

/* ---------- helpers for pretty-printing ------------------------- */
static const char *fieldName(DataField f)
{
    static const char *tbl[] = {
        "Patient-ID", "Date-of-Birth", "Diagnosis", "Lab-Results"
    };
    return tbl[f];
}

static const char *purposeName(Purpose p)
{
    static const char *tbl[] = { "Treatment", "Research", "Billing" };
    return tbl[p];
}

static const char *resultName(NegotiationResult r)
{
    static const char *tbl[] = { "GRANTED", "GRANTED-WITH-DEID", "DENIED" };
    return tbl[r];
}

/* ---------- core negotiation algorithm -------------------------- */
NegotiationResult negotiate(const ProviderPolicy *prov,
                            const DataRequest    *req)
{
    const FieldPolicy *fp = &prov->rules[req->field];

    /* 1) Purpose allowed? */
    if (!fp->allow[req->purpose])
        return NEG_DENIED;

    /* 2) Consent needed? */
    if (fp->requireConsent && !req->patientConsent)
        return NEG_DENIED;

    /* 3) Identifiability check */
    if (req->wantsIdentifiable) {
        if (fp->allowIdentifiable)
            return NEG_GRANTED;           /* fine: identifiable data allowed   */
        else
            return NEG_DENIED;            /* provider forbids raw identifiers  */
    } else {
        /* requester is OK with de-id (or already de-id) */
        return NEG_GRANTED_DEID;
    }
}

/* ---------- assemble provider policies -------------------------- */
static ProviderPolicy makeDemoProvider(void)
{
    ProviderPolicy p = { .name = "Hospital-A" };

    /* defaults: deny everything */
    for (int f = 0; f < NUM_FIELDS; ++f) {
        for (int u = 0; u < NUM_PURPOSES; ++u) p.rules[f].allow[u] = false;
        p.rules[f].requireConsent  = false;
        p.rules[f].allowIdentifiable = false;
        p.rules[f].field = (DataField)f;
    }

    /* Field-specific rules (purely illustrative!):

         ┌────────────────────────┬───────────┬──────────┬──────────┬────────┐
         │ Field                  │ Treatment │ Research │ Billing  │ Ident? │
         ├────────────────────────┼───────────┼──────────┼──────────┼────────┤
         │ Patient-ID             │ yes       │ NO       │ yes      │ yes    │
         │ DOB                    │ yes       │ yes      │ yes      │ NO     │
         │ Diagnosis              │ yes       │ yes      │ yes      │ yes    │
         │ Lab-Results            │ yes       │ yes      │ NO       │ NO     │
         └────────────────────────┴───────────┴──────────┴──────────┴────────┘
       * Consent required for Research on Patient-ID and Lab-Results.
    */

    /* Patient-ID */
    p.rules[FIELD_ID].allow[PURP_TREATMENT] = true;
    p.rules[FIELD_ID].allow[PURP_BILLING]   = true;
    p.rules[FIELD_ID].allowIdentifiable     = true;
    p.rules[FIELD_ID].requireConsent        = true;  /* for any external share */

    /* DOB */
    p.rules[FIELD_DOB].allow[PURP_TREATMENT] = true;
    p.rules[FIELD_DOB].allow[PURP_RESEARCH]  = true;
    p.rules[FIELD_DOB].allow[PURP_BILLING]   = true;
    p.rules[FIELD_DOB].allowIdentifiable     = false;

    /* Diagnosis */
    p.rules[FIELD_DIAGNOSIS].allow[PURP_TREATMENT] = true;
    p.rules[FIELD_DIAGNOSIS].allow[PURP_RESEARCH]  = true;
    p.rules[FIELD_DIAGNOSIS].allow[PURP_BILLING]   = true;
    p.rules[FIELD_DIAGNOSIS].allowIdentifiable     = true;

    /* Lab-Results */
    p.rules[FIELD_LAB_RESULTS].allow[PURP_TREATMENT] = true;
    p.rules[FIELD_LAB_RESULTS].allow[PURP_RESEARCH]  = true;
    p.rules[FIELD_LAB_RESULTS].allowIdentifiable     = false;
    p.rules[FIELD_LAB_RESULTS].requireConsent        = true;

    return p;
}

/* ---------- hard-coded test suite ------------------------------- */
static void runTest(const ProviderPolicy *prov, const DataRequest *req)
{
    NegotiationResult res = negotiate(prov, req);

    printf("Request by %-12s  Field=%-13s  Purpose=%-9s  "
           "Identifiable=%-3s  Consent=%s  ->  %s\n",
           req->requester,
           fieldName(req->field),
           purposeName(req->purpose),
           req->wantsIdentifiable ? "yes" : "no",
           req->patientConsent   ? "yes" : "no",
           resultName(res));
}

int main(void)
{
    ProviderPolicy provider = makeDemoProvider();

    puts("=== Clinical-data Negotiation Demo ============================\n");

    DataRequest tests[] = {
        /* 1 */ { "Dr.Smith",  FIELD_DIAGNOSIS,  PURP_TREATMENT,
                  true,  false },

        /* 2 */ { "ResearchLab", FIELD_DOB,      PURP_RESEARCH,
                  true,  true  },

        /* 3 */ { "ResearchLab", FIELD_DOB,      PURP_RESEARCH,
                  false, true  },

        /* 4 */ { "ResearchLab", FIELD_LAB_RESULTS, PURP_RESEARCH,
                  false, false },     /* consent missing */

        /* 5 */ { "BillingSvc", FIELD_ID,       PURP_BILLING,
                  true,  false },

        /* 6 */ { "ResearchLab", FIELD_ID,      PURP_RESEARCH,
                  true,  false },     /* no consent, purpose not allowed */

        /* 7 */ { "ResearchLab", FIELD_ID,      PURP_RESEARCH,
                  false, true  },     /* consent ok, de-id ok */
    };

    for (unsigned i = 0; i < sizeof tests / sizeof tests[0]; ++i)
        runTest(&provider, &tests[i]);

    puts("\n===============================================================");
    return 0;
}

