// --------------------------------------------------------------
//  Clinical-data Negotiation Demo  (Rust 1.77, edition 2021)
// --------------------------------------------------------------

use std::collections::HashMap;

// ── Taxonomy ──────────────────────────────────────────────────
#[derive(Copy, Clone, Eq, PartialEq, Hash)]
enum DataField {
    PatientId,
    Dob,
    Diagnosis,
    LabResults,
}

impl DataField {
    fn label(self) -> &'static str {
        match self {
            DataField::PatientId  => "Patient-ID",
            DataField::Dob        => "Date-of-Birth",
            DataField::Diagnosis  => "Diagnosis",
            DataField::LabResults => "Lab-Results",
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
enum Purpose {
    Treatment,
    Research,
    Billing,
}

impl Purpose {
    fn label(self) -> &'static str {
        match self {
            Purpose::Treatment => "Treatment",
            Purpose::Research  => "Research",
            Purpose::Billing   => "Billing",
        }
    }
}

#[derive(Clone, Copy)]
enum ResultKind {
    Granted,
    GrantedDeId,
    Denied,
}

impl std::fmt::Display for ResultKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ResultKind::Granted      => write!(f, "GRANTED"),
            ResultKind::GrantedDeId  => write!(f, "GRANTED-WITH-DEID"),
            ResultKind::Denied       => write!(f, "DENIED"),
        }
    }
}

// ── Policy structs ────────────────────────────────────────────
#[derive(Clone)]
struct FieldPolicy {
    allow: HashMap<Purpose, bool>,
    require_consent: bool,
    allow_identifiable: bool,
}

struct ProviderPolicy {
    name: &'static str,
    rules: HashMap<DataField, FieldPolicy>,
}

// ── Request struct ────────────────────────────────────────────
struct DataRequest {
    requester: &'static str,
    field: DataField,
    purpose: Purpose,
    wants_identifiable: bool,
    patient_consent: bool,
}

// ── Negotiation algorithm ─────────────────────────────────────
fn negotiate(rule: &FieldPolicy, req: &DataRequest) -> ResultKind {
    // 1️⃣ purpose gate
    if !rule.allow.get(&req.purpose).copied().unwrap_or(false) {
        return ResultKind::Denied;
    }
    // 2️⃣ consent check
    if rule.require_consent && !req.patient_consent {
        return ResultKind::Denied;
    }
    // 3️⃣ identifiability
    if req.wants_identifiable {
        if rule.allow_identifiable {
            ResultKind::Granted
        } else {
            ResultKind::Denied
        }
    } else {
        ResultKind::GrantedDeId
    }
}

// ── Build demo provider rules ─────────────────────────────────
fn make_demo_provider() -> ProviderPolicy {
    use DataField::*;
    use Purpose::*;

    // convenience macro for per-purpose map
    macro_rules! allow {
        ( $($p:ident => $v:expr),* $(,)? ) => {{
            let mut m = HashMap::new();
            $( m.insert(Purpose::$p, $v); )*
            m
        }};
    }

    let mut rules = HashMap::new();

    // Patient-ID
    rules.insert(
        PatientId,
        FieldPolicy {
            allow: allow!(Treatment => true, Research => true, Billing => true),
            require_consent: true,
            allow_identifiable: true,
        },
    );
    // DOB
    rules.insert(
        Dob,
        FieldPolicy {
            allow: allow!(Treatment => true, Research => true, Billing => true),
            require_consent: false,
            allow_identifiable: false,
        },
    );
    // Diagnosis
    rules.insert(
        Diagnosis,
        FieldPolicy {
            allow: allow!(Treatment => true, Research => true, Billing => true),
            require_consent: false,
            allow_identifiable: true,
        },
    );
    // Lab-Results
    rules.insert(
        LabResults,
        FieldPolicy {
            allow: allow!(Treatment => true, Research => true, Billing => false),
            require_consent: true,
            allow_identifiable: false,
        },
    );

    ProviderPolicy { name: "Hospital-A", rules }
}

// ── Main with seven hard-coded tests ──────────────────────────
fn main() {
    let provider = make_demo_provider();

    use DataField::*;
    use Purpose::*;

    let tests = vec![
        DataRequest { requester: "Dr.Smith",    field: Diagnosis,   purpose: Treatment, wants_identifiable: true,  patient_consent: false },
        DataRequest { requester: "ResearchLab", field: Dob,         purpose: Research,  wants_identifiable: true,  patient_consent: true  },
        DataRequest { requester: "ResearchLab", field: Dob,         purpose: Research,  wants_identifiable: false, patient_consent: true  },
        DataRequest { requester: "ResearchLab", field: LabResults,  purpose: Research,  wants_identifiable: false, patient_consent: false },
        DataRequest { requester: "BillingSvc",  field: PatientId,   purpose: Billing,   wants_identifiable: true,  patient_consent: false },
        DataRequest { requester: "ResearchLab", field: PatientId,   purpose: Research,  wants_identifiable: true,  patient_consent: false },
        DataRequest { requester: "ResearchLab", field: PatientId,   purpose: Research,  wants_identifiable: false, patient_consent: true  },
    ];

    println!("=== Clinical-data Negotiation Demo =========================\n");

    for req in &tests {
        let rule = &provider.rules[&req.field];
        let res  = negotiate(rule, req);

        // rudimentary padding: width specifiers
        println!(
            "Request by {req:<12}  Field={field:<13}  Purpose={purp:<9}  \
             Identifiable={id:<3}  Consent={cons:<3}  ->  {res}",
            req   = req.requester,
            field = req.field.label(),
            purp  = req.purpose.label(),
            id    = if req.wants_identifiable { "yes" } else { "no" },
            cons  = if req.patient_consent     { "yes" } else { "no" },
            res   = res
        );
    }

    println!("\n============================================================");
}
