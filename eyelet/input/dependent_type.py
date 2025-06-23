# ------------------------------------------------------------
# 0.  Core universe and meta-predicates
# ------------------------------------------------------------
from __future__ import annotations
from collections import defaultdict

class Type:
    """
    A minimal representation of an RDF/N3 class.
    - name …… its URI-local part (kept simple here)
    - kind …… if this type itself belongs to some meta-type
    - good_of … the profession that the automatically-generated
                Good<Type> depends on (None for ordinary types)
    """
    def __init__(self, name, *, kind=None, good_of=None):
        self.name = name
        self.kind = kind
        self.good_of = good_of

    def __repr__(self):                       # for pretty printing
        return self.name


TYPES: dict[str, Type] = {}

def new_type(name: str, *, kind: Type | None = None,
             good_of: Type | None = None) -> Type:
    """Create and register a new Type object."""
    t = Type(name, kind=kind, good_of=good_of)
    TYPES[name] = t
    return t


# ------------------------------------------------------------
# 1.  Professions are (first-class) types
# ------------------------------------------------------------
Type_ = new_type("Type")              # meta-meta ‘everything’ type
Profession = new_type("Profession", kind=None)

Cobbler   = new_type("Cobbler",   kind=Profession)
Physician = new_type("Physician", kind=Profession)

# ------------------------------------------------------------
# 2.  Domain notions each profession cares about
# ------------------------------------------------------------
Quality   = new_type("Quality")
Excellent = new_type("Excellent")     # instance of Quality (simplified)

Shoe = new_type("Shoe")

Outcome   = new_type("Outcome")
Recovered = new_type("Recovered")     # instance of Outcome

# Property names (treated just as strings for this toy model)
PRODUCES   = "produces"      # cobbler  ––>  shoe
QUALITY    = "quality"
TREATS     = "treats"        # physician ––> medical case
OUTCOME    = "outcome"
PATIENT_OF = "patientOf"     # convenience inverse


# ------------------------------------------------------------
# Helper class representing an RDF/N3 individual
# ------------------------------------------------------------
class Individual:
    def __init__(self, name: str):
        self.name = name
        self.types: set[Type] = set()
        self.props: dict[str, set["Individual" | Type]] = defaultdict(set)

    # --- RDF-style mutators ---------------------------------
    def add_type(self, t: Type):
        self.types.add(t)

    def add_prop(self, prop: str, value: "Individual" | Type):
        self.props[prop].add(value)

    # --- convenience ----------------------------------------
    def has_type(self, t: Type) -> bool:
        return t in self.types

    def __repr__(self):
        return self.name


# ------------------------------------------------------------
# 3.  (Dependent) type constructor   Good : Profession → Type
# ------------------------------------------------------------
GOOD_TYPES: dict[Type, Type] = {}

def create_good_types():
    """
    For every P : Profession, create the dependent type GoodP.
    (Exact analogue of N3 rule block #3.)
    """
    for t in list(TYPES.values()):
        if t.kind is Profession:           # ‘P :kind Profession’
            good_t = new_type(f"Good{t.name}", good_of=t)
            GOOD_TYPES[t] = good_t

create_good_types()


# ------------------------------------------------------------
# 5.  Concrete individuals that satisfy the criteria
# ------------------------------------------------------------
alice  = Individual("alice")
shoe_1 = Individual("shoe₁")
bob    = Individual("bob")
case_1 = Individual("case₁")

# A cobbler who makes excellent shoes
alice.add_type(Cobbler)
shoe_1.add_type(Shoe)
shoe_1.add_prop(QUALITY, Excellent)
alice.add_prop(PRODUCES, shoe_1)

# A physician who cures patients
bob.add_type(Physician)
case_1.add_prop(OUTCOME, Recovered)
case_1.add_prop(PATIENT_OF, bob)
bob.add_prop(TREATS, case_1)

INDIVIDUALS = [alice, shoe_1, bob, case_1]


# ------------------------------------------------------------
# 4.  Excellence rules  (Good Cobbler / Good Physician)
# ------------------------------------------------------------
def infer_good_professionals():
    """
    Implements the two N3 rule blocks in section 4.
    Whenever the condition holds, we attach the relevant Good<Type>.
    """
    for ind in INDIVIDUALS:
        # ————— Good Cobbler —————
        if ind.has_type(Cobbler):
            for shoe in ind.props.get(PRODUCES, []):
                if Excellent in shoe.props.get(QUALITY, []):
                    ind.add_type(GOOD_TYPES[Cobbler])

        # ———— Good Physician ————
        if ind.has_type(Physician):
            for case in ind.props.get(TREATS, []):
                if Recovered in case.props.get(OUTCOME, []):
                    ind.add_type(GOOD_TYPES[Physician])

infer_good_professionals()


# ------------------------------------------------------------
# Demonstration
# ------------------------------------------------------------
print("Inferred ‘good’ professionals:\n")
for ind in INDIVIDUALS:
    goodies = [t.name for t in ind.types if t in GOOD_TYPES.values()]
    if goodies:
        print(f" • {ind.name}  ⟶  {', '.join(goodies)}")

