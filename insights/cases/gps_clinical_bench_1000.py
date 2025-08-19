#!/usr/bin/env python3
# ──────────────────────────────────────────────────────────────
#  GPS-style reasoner for weighted state transitions
#  -------------------------------------------------------------
#  • Reads a block of N3 rules (gps:description … <= { … }.)
#  • Parses every transition (pre-state, post-state, action,
#    duration, cost, success-probability, comfort-probability)
#    plus its *patient filter* (gender / age).
#  • Loads one concrete Patient instance (you can add more).
#  • Runs a breadth-first search that
#       – accumulates the 4 metrics,
#       – enforces exactly the same limits that appeared in the
#         original N3 query (≤ P180D, ≤ €500, ≥ 0.35, ≥ 0.35),
#       – prevents literal loops,
#       – returns *every distinct ordered action list* that
#         reaches GOAL_STATE within MAX_STAGECOUNT stages.
#  • Finally prints the solutions sorted by:
#       1. highest success probability
#       2. lowest cost
#       3. shortest duration
#
#  The code purposefully keeps the parsing & reasoning strictly
#  separated, so you can re-use `search_all()` with *any*
#  TransitionRule list you like (e.g. from Turtle, SPARQL, CSV…).
# ──────────────────────────────────────────────────────────────

from __future__ import annotations

import re
import textwrap
from dataclasses import dataclass
from typing import Callable, List, Tuple, Deque
from collections import deque

# ╔═══════════════════════════════════════╗
# ║ 1. PATIENT KNOWLEDGE BASE             ║
# ╚═══════════════════════════════════════╝
# A minimal RDF-ish fact set is encoded as a Python dataclass.
# You can of course load such facts from Turtle in the same way,
# but keeping it inline makes the example 100 % self-contained.

@dataclass(frozen=True)
class Patient:
    """All patient facts that may appear in rule pre-conditions."""
    uri: str
    name: str
    gender: str   # "Male" | "Female"
    age: int      # in years
    weight: float # kg – not used in current rules, but useful later
    diagnosis: str
    state: str    # current care:state_n

# Example patient from the very first message
patient_1 = Patient(
    uri       = "data:patient_1",
    name      = "Jane Doe",
    gender    = "Female",
    age       = 14,
    weight    = 75.0,
    diagnosis = "49049000",     # SNOMED code
    state     = "state_2",      # she starts in care:state_2
)

# ╔═══════════════════════════════════════╗
# ║ 2.  N3 RULE BLOCK                     ║
# ╚═══════════════════════════════════════╝
# Paste the *COMPLETE* set of gps:description rules here.
# The parser will read them verbatim – no further editing
# anywhere else in the script is required.

N3_RULES = textwrap.dedent("""
PREFIX math: <http://www.w3.org/2000/10/swap/math#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX e: <http://eulersharp.sourceforge.net/2003/03swap/log-rules#>
PREFIX action: <http://josd.github.io/eye/reasoning/gps/action#>
PREFIX medication: <http://josd.github.io/eye/reasoning/gps/medication#>
PREFIX sct: <http://snomed.info/id/>
PREFIX care: <http://josd.github.io/eye/reasoning/gps/care#>
PREFIX gps: <http://josd.github.io/eye/reasoning/gps/gps-schema#>

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_0.}
     {?patient care:state care:state_6.}
     action:take_pill_Medication_0
     "P1D"^^xsd:dayTimeDuration
     20
     0.465717891569164
     0.3025355437145813
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_1.}
     {?patient care:state care:state_3.}
     action:take_pill_Medication_1
     "P1D"^^xsd:dayTimeDuration
     93
     0.6740422493772897
     0.6973367904631385
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_2.}
     {?patient care:state care:state_19.}
     action:take_pill_Medication_2
     "P1D"^^xsd:dayTimeDuration
     65
     0.34583132447806364
     0.46855302245960595
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_3.}
     {?patient care:state care:state_17.}
     action:take_pill_Medication_3
     "P1D"^^xsd:dayTimeDuration
     95
     0.8328180231211615
     0.9871190113946524
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_4.}
     {?patient care:state care:state_13.}
     action:take_pill_Medication_4
     "P1D"^^xsd:dayTimeDuration
     90
     0.34453545742655134
     0.6623999242290436
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_5.}
     {?patient care:state care:state_20.}
     action:take_pill_Medication_5
     "P1D"^^xsd:dayTimeDuration
     53
     0.7290232669907336
     0.3493737375628685
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_6.}
     {?patient care:state care:state_20.}
     action:take_pill_Medication_6
     "P1D"^^xsd:dayTimeDuration
     47
     0.2546733692949675
     0.9362481647831721
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_7.}
     {?patient care:state care:state_5.}
     action:take_pill_Medication_7
     "P1D"^^xsd:dayTimeDuration
     18
     0.7561515714341688
     0.27746131871393676
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_8.}
     {?patient care:state care:state_15.}
     action:take_pill_Medication_8
     "P1D"^^xsd:dayTimeDuration
     30
     0.3480524359896117
     0.8343300246598554
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_9.}
     {?patient care:state care:state_8.}
     action:take_pill_Medication_9
     "P1D"^^xsd:dayTimeDuration
     2
     0.3050505855520276
     0.597652861604021
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_10.}
     {?patient care:state care:state_8.}
     action:take_pill_Medication_10
     "P1D"^^xsd:dayTimeDuration
     78
     0.25235904691314304
     0.31593041827470764
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_11.}
     {?patient care:state care:state_5.}
     action:take_pill_Medication_11
     "P1D"^^xsd:dayTimeDuration
     31
     0.7294653201350865
     0.4754114978287537
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_12.}
     {?patient care:state care:state_18.}
     action:take_pill_Medication_12
     "P1D"^^xsd:dayTimeDuration
     86
     0.9806432744537633
     0.6215843675402378
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_13.}
     {?patient care:state care:state_19.}
     action:take_pill_Medication_13
     "P1D"^^xsd:dayTimeDuration
     19
     0.9156500482048546
     0.29610457252723293
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_14.}
     {?patient care:state care:state_10.}
     action:take_pill_Medication_14
     "P1D"^^xsd:dayTimeDuration
     81
     0.7420067778083788
     0.6831359335581597
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_15.}
     {?patient care:state care:state_11.}
     action:take_pill_Medication_15
     "P1D"^^xsd:dayTimeDuration
     49
     0.4595379791702132
     0.41668351785040764
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_16.}
     {?patient care:state care:state_19.}
     action:take_pill_Medication_16
     "P1D"^^xsd:dayTimeDuration
     25
     0.4222159944381769
     0.8982640208314032
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_17.}
     {?patient care:state care:state_18.}
     action:take_pill_Medication_17
     "P1D"^^xsd:dayTimeDuration
     64
     0.3576338100333216
     0.6757256884236226
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_18.}
     {?patient care:state care:state_14.}
     action:take_pill_Medication_18
     "P1D"^^xsd:dayTimeDuration
     23
     0.9911301101888806
     0.552741780022824
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_19.}
     {?patient care:state care:state_18.}
     action:take_pill_Medication_19
     "P1D"^^xsd:dayTimeDuration
     73
     0.6822949065524662
     0.8260950857910462
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_20.}
     {?patient care:state care:state_9.}
     action:take_pill_Medication_20
     "P1D"^^xsd:dayTimeDuration
     70
     0.8027843551479326
     0.631044723171167
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_21.}
     {?patient care:state care:state_3.}
     action:take_pill_Medication_21
     "P1D"^^xsd:dayTimeDuration
     34
     0.497170642201217
     0.7113792355892264
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_22.}
     {?patient care:state care:state_10.}
     action:take_pill_Medication_22
     "P1D"^^xsd:dayTimeDuration
     84
     0.8886340329667293
     0.4972940719584476
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_23.}
     {?patient care:state care:state_10.}
     action:take_pill_Medication_23
     "P1D"^^xsd:dayTimeDuration
     87
     0.8101210744065674
     0.5763219981817378
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_24.}
     {?patient care:state care:state_4.}
     action:take_pill_Medication_24
     "P1D"^^xsd:dayTimeDuration
     60
     0.2504134834771163
     0.542545329151948
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_25.}
     {?patient care:state care:state_13.}
     action:take_pill_Medication_25
     "P1D"^^xsd:dayTimeDuration
     81
     0.5590636276039664
     0.7388795264916019
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_26.}
     {?patient care:state care:state_5.}
     action:take_pill_Medication_26
     "P1D"^^xsd:dayTimeDuration
     32
     0.8582772636668423
     0.6669317782120898
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_27.}
     {?patient care:state care:state_15.}
     action:take_pill_Medication_27
     "P1D"^^xsd:dayTimeDuration
     78
     0.9006216100853658
     0.5907637570132014
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_28.}
     {?patient care:state care:state_4.}
     action:take_pill_Medication_28
     "P1D"^^xsd:dayTimeDuration
     86
     0.7226474740968996
     0.8794949003535182
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_29.}
     {?patient care:state care:state_15.}
     action:take_pill_Medication_29
     "P1D"^^xsd:dayTimeDuration
     73
     0.7171550778331066
     0.2585486027776301
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_30.}
     {?patient care:state care:state_5.}
     action:take_pill_Medication_30
     "P1D"^^xsd:dayTimeDuration
     98
     0.9554113899087956
     0.6830122298511409
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_31.}
     {?patient care:state care:state_7.}
     action:take_pill_Medication_31
     "P1D"^^xsd:dayTimeDuration
     6
     0.8560264458594464
     0.7482341650427685
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_32.}
     {?patient care:state care:state_15.}
     action:take_pill_Medication_32
     "P1D"^^xsd:dayTimeDuration
     71
     0.7317086170594969
     0.5960678771119461
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_33.}
     {?patient care:state care:state_16.}
     action:take_pill_Medication_33
     "P1D"^^xsd:dayTimeDuration
     22
     0.5783420371480852
     0.8828401104927625
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_34.}
     {?patient care:state care:state_9.}
     action:take_pill_Medication_34
     "P1D"^^xsd:dayTimeDuration
     79
     0.459037399864728
     0.46312166963445
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_35.}
     {?patient care:state care:state_3.}
     action:take_pill_Medication_35
     "P1D"^^xsd:dayTimeDuration
     96
     0.8392805459572492
     0.9041054260158756
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_36.}
     {?patient care:state care:state_4.}
     action:take_pill_Medication_36
     "P1D"^^xsd:dayTimeDuration
     7
     0.7912727582348695
     0.3060452925497237
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_37.}
     {?patient care:state care:state_19.}
     action:take_pill_Medication_37
     "P1D"^^xsd:dayTimeDuration
     84
     0.947350221177684
     0.3032303273181381
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_38.}
     {?patient care:state care:state_4.}
     action:take_pill_Medication_38
     "P1D"^^xsd:dayTimeDuration
     8
     0.8410344677112118
     0.42579528449379234
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_39.}
     {?patient care:state care:state_17.}
     action:take_pill_Medication_39
     "P1D"^^xsd:dayTimeDuration
     60
     0.7994245898062894
     0.3439940306664041
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_40.}
     {?patient care:state care:state_2.}
     action:take_pill_Medication_40
     "P1D"^^xsd:dayTimeDuration
     90
     0.7975848502536895
     0.8089354395260078
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_41.}
     {?patient care:state care:state_6.}
     action:take_pill_Medication_41
     "P1D"^^xsd:dayTimeDuration
     89
     0.45134465355311343
     0.6886440538593578
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_42.}
     {?patient care:state care:state_10.}
     action:take_pill_Medication_42
     "P1D"^^xsd:dayTimeDuration
     14
     0.37035802007560514
     0.26303956003396345
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_43.}
     {?patient care:state care:state_14.}
     action:take_pill_Medication_43
     "P1D"^^xsd:dayTimeDuration
     3
     0.423639720381911
     0.8937621058434605
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_44.}
     {?patient care:state care:state_13.}
     action:take_pill_Medication_44
     "P1D"^^xsd:dayTimeDuration
     28
     0.33414168767665425
     0.5606781925264207
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_45.}
     {?patient care:state care:state_11.}
     action:take_pill_Medication_45
     "P1D"^^xsd:dayTimeDuration
     27
     0.6148075072006308
     0.6380204642349029
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_46.}
     {?patient care:state care:state_16.}
     action:take_pill_Medication_46
     "P1D"^^xsd:dayTimeDuration
     24
     0.5285284544950806
     0.47271142235428165
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_47.}
     {?patient care:state care:state_5.}
     action:take_pill_Medication_47
     "P1D"^^xsd:dayTimeDuration
     15
     0.9779698218309321
     0.8777665906260649
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_48.}
     {?patient care:state care:state_15.}
     action:take_pill_Medication_48
     "P1D"^^xsd:dayTimeDuration
     81
     0.5302639127129547
     0.33277513194892705
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_49.}
     {?patient care:state care:state_2.}
     action:take_pill_Medication_49
     "P1D"^^xsd:dayTimeDuration
     24
     0.8599959718550982
     0.5299408462051493
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_50.}
     {?patient care:state care:state_8.}
     action:take_pill_Medication_50
     "P1D"^^xsd:dayTimeDuration
     32
     0.7407574492633611
     0.252681722319644
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_51.}
     {?patient care:state care:state_5.}
     action:take_pill_Medication_51
     "P1D"^^xsd:dayTimeDuration
     80
     0.6898639258507993
     0.9155777170542785
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_52.}
     {?patient care:state care:state_7.}
     action:take_pill_Medication_52
     "P1D"^^xsd:dayTimeDuration
     54
     0.6037482361902836
     0.3011416863646894
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_53.}
     {?patient care:state care:state_4.}
     action:take_pill_Medication_53
     "P1D"^^xsd:dayTimeDuration
     27
     0.3141422181530153
     0.7967077700883471
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_54.}
     {?patient care:state care:state_6.}
     action:take_pill_Medication_54
     "P1D"^^xsd:dayTimeDuration
     10
     0.3025550351578413
     0.9049857049934551
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_55.}
     {?patient care:state care:state_10.}
     action:take_pill_Medication_55
     "P1D"^^xsd:dayTimeDuration
     73
     0.5489722534323953
     0.7008867066002604
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_56.}
     {?patient care:state care:state_18.}
     action:take_pill_Medication_56
     "P1D"^^xsd:dayTimeDuration
     17
     0.8422094773626871
     0.7574535304911314
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_57.}
     {?patient care:state care:state_5.}
     action:take_pill_Medication_57
     "P1D"^^xsd:dayTimeDuration
     39
     0.5958660196710776
     0.6894399887021381
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_58.}
     {?patient care:state care:state_8.}
     action:take_pill_Medication_58
     "P1D"^^xsd:dayTimeDuration
     43
     0.6628222471844167
     0.4231929573672714
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_59.}
     {?patient care:state care:state_16.}
     action:take_pill_Medication_59
     "P1D"^^xsd:dayTimeDuration
     92
     0.3538757962737874
     0.9022158392841144
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_60.}
     {?patient care:state care:state_4.}
     action:take_pill_Medication_60
     "P1D"^^xsd:dayTimeDuration
     51
     0.3806642150608152
     0.6899409399880754
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_61.}
     {?patient care:state care:state_14.}
     action:take_pill_Medication_61
     "P1D"^^xsd:dayTimeDuration
     82
     0.43379376624218724
     0.6313707972197778
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_62.}
     {?patient care:state care:state_20.}
     action:take_pill_Medication_62
     "P1D"^^xsd:dayTimeDuration
     81
     0.9568800010375927
     0.9534984131345079
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_63.}
     {?patient care:state care:state_4.}
     action:take_pill_Medication_63
     "P1D"^^xsd:dayTimeDuration
     79
     0.5992305070878914
     0.8160690661858296
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_64.}
     {?patient care:state care:state_6.}
     action:take_pill_Medication_64
     "P1D"^^xsd:dayTimeDuration
     17
     0.9122714092572541
     0.5436162284222494
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_65.}
     {?patient care:state care:state_5.}
     action:take_pill_Medication_65
     "P1D"^^xsd:dayTimeDuration
     3
     0.3690712779369829
     0.8371780441234205
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_66.}
     {?patient care:state care:state_4.}
     action:take_pill_Medication_66
     "P1D"^^xsd:dayTimeDuration
     10
     0.93261460010992
     0.5625198519784703
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_67.}
     {?patient care:state care:state_8.}
     action:take_pill_Medication_67
     "P1D"^^xsd:dayTimeDuration
     68
     0.27461971647468053
     0.4825820709024589
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_68.}
     {?patient care:state care:state_14.}
     action:take_pill_Medication_68
     "P1D"^^xsd:dayTimeDuration
     19
     0.279051142586049
     0.43471804564946936
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_69.}
     {?patient care:state care:state_10.}
     action:take_pill_Medication_69
     "P1D"^^xsd:dayTimeDuration
     15
     0.6910409636927066
     0.9738850917903403
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_70.}
     {?patient care:state care:state_8.}
     action:take_pill_Medication_70
     "P1D"^^xsd:dayTimeDuration
     79
     0.3990207718619533
     0.7479397184911787
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_71.}
     {?patient care:state care:state_16.}
     action:take_pill_Medication_71
     "P1D"^^xsd:dayTimeDuration
     31
     0.686369909350598
     0.7872780932369263
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_72.}
     {?patient care:state care:state_11.}
     action:take_pill_Medication_72
     "P1D"^^xsd:dayTimeDuration
     26
     0.5788247626531041
     0.7892305503321257
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_73.}
     {?patient care:state care:state_9.}
     action:take_pill_Medication_73
     "P1D"^^xsd:dayTimeDuration
     7
     0.39007305978533935
     0.6863849624772779
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_74.}
     {?patient care:state care:state_19.}
     action:take_pill_Medication_74
     "P1D"^^xsd:dayTimeDuration
     53
     0.5964984436238282
     0.32199417758431104
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_75.}
     {?patient care:state care:state_8.}
     action:take_pill_Medication_75
     "P1D"^^xsd:dayTimeDuration
     97
     0.4711446141833867
     0.3097460588591836
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_76.}
     {?patient care:state care:state_17.}
     action:take_pill_Medication_76
     "P1D"^^xsd:dayTimeDuration
     83
     0.6187447713366957
     0.48510478070675045
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_77.}
     {?patient care:state care:state_10.}
     action:take_pill_Medication_77
     "P1D"^^xsd:dayTimeDuration
     96
     0.7496879217622046
     0.7142980837295743
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_78.}
     {?patient care:state care:state_2.}
     action:take_pill_Medication_78
     "P1D"^^xsd:dayTimeDuration
     80
     0.38887292558821107
     0.2872249568066695
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_79.}
     {?patient care:state care:state_2.}
     action:take_pill_Medication_79
     "P1D"^^xsd:dayTimeDuration
     76
     0.27730397754927477
     0.4850113004026169
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_80.}
     {?patient care:state care:state_11.}
     action:take_pill_Medication_80
     "P1D"^^xsd:dayTimeDuration
     26
     0.930036817304261
     0.7240324946391107
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_81.}
     {?patient care:state care:state_10.}
     action:take_pill_Medication_81
     "P1D"^^xsd:dayTimeDuration
     37
     0.6416399925787659
     0.9417830032504191
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_82.}
     {?patient care:state care:state_6.}
     action:take_pill_Medication_82
     "P1D"^^xsd:dayTimeDuration
     17
     0.9206481539462658
     0.9155956466435331
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_83.}
     {?patient care:state care:state_12.}
     action:take_pill_Medication_83
     "P1D"^^xsd:dayTimeDuration
     73
     0.28076327275828455
     0.445423640473086
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_84.}
     {?patient care:state care:state_11.}
     action:take_pill_Medication_84
     "P1D"^^xsd:dayTimeDuration
     25
     0.36861113559048986
     0.44434179999243545
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_85.}
     {?patient care:state care:state_4.}
     action:take_pill_Medication_85
     "P1D"^^xsd:dayTimeDuration
     23
     0.6945667589998454
     0.5332545485844407
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_86.}
     {?patient care:state care:state_12.}
     action:take_pill_Medication_86
     "P1D"^^xsd:dayTimeDuration
     78
     0.8004440562900044
     0.7043492447469111
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_87.}
     {?patient care:state care:state_7.}
     action:take_pill_Medication_87
     "P1D"^^xsd:dayTimeDuration
     14
     0.8039013848943759
     0.5345025433394979
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_88.}
     {?patient care:state care:state_6.}
     action:take_pill_Medication_88
     "P1D"^^xsd:dayTimeDuration
     60
     0.9363341916579663
     0.48951819906605665
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_89.}
     {?patient care:state care:state_8.}
     action:take_pill_Medication_89
     "P1D"^^xsd:dayTimeDuration
     29
     0.44826635939673176
     0.6279539333319343
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_90.}
     {?patient care:state care:state_11.}
     action:take_pill_Medication_90
     "P1D"^^xsd:dayTimeDuration
     96
     0.30655883027434516
     0.317119728978559
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_91.}
     {?patient care:state care:state_15.}
     action:take_pill_Medication_91
     "P1D"^^xsd:dayTimeDuration
     7
     0.5858120760377888
     0.4391544766202978
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_92.}
     {?patient care:state care:state_10.}
     action:take_pill_Medication_92
     "P1D"^^xsd:dayTimeDuration
     87
     0.629332797591409
     0.3452795383513899
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_93.}
     {?patient care:state care:state_20.}
     action:take_pill_Medication_93
     "P1D"^^xsd:dayTimeDuration
     71
     0.29309660266529214
     0.75795049992039
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_94.}
     {?patient care:state care:state_8.}
     action:take_pill_Medication_94
     "P1D"^^xsd:dayTimeDuration
     92
     0.6435205790854912
     0.9414814176103763
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_95.}
     {?patient care:state care:state_8.}
     action:take_pill_Medication_95
     "P1D"^^xsd:dayTimeDuration
     37
     0.36943104006601185
     0.44043756923463895
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_96.}
     {?patient care:state care:state_15.}
     action:take_pill_Medication_96
     "P1D"^^xsd:dayTimeDuration
     44
     0.668948546174877
     0.29155583660607276
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_97.}
     {?patient care:state care:state_6.}
     action:take_pill_Medication_97
     "P1D"^^xsd:dayTimeDuration
     53
     0.8560390362434394
     0.8268597030966126
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_98.}
     {?patient care:state care:state_8.}
     action:take_pill_Medication_98
     "P1D"^^xsd:dayTimeDuration
     23
     0.5682539920770189
     0.36086731341571066
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_99.}
     {?patient care:state care:state_15.}
     action:take_pill_Medication_99
     "P1D"^^xsd:dayTimeDuration
     57
     0.4389247947919508
     0.5787952677361754
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_100.}
     {?patient care:state care:state_12.}
     action:take_pill_Medication_100
     "P1D"^^xsd:dayTimeDuration
     96
     0.41516985517210975
     0.9947837683358334
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_101.}
     {?patient care:state care:state_20.}
     action:take_pill_Medication_101
     "P1D"^^xsd:dayTimeDuration
     76
     0.846555405711879
     0.4238972758879048
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_102.}
     {?patient care:state care:state_4.}
     action:take_pill_Medication_102
     "P1D"^^xsd:dayTimeDuration
     14
     0.4511109559251728
     0.657784460094976
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_103.}
     {?patient care:state care:state_12.}
     action:take_pill_Medication_103
     "P1D"^^xsd:dayTimeDuration
     67
     0.6545706039895638
     0.571408591717118
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_104.}
     {?patient care:state care:state_2.}
     action:take_pill_Medication_104
     "P1D"^^xsd:dayTimeDuration
     22
     0.8892027037968089
     0.2837705182934574
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_105.}
     {?patient care:state care:state_15.}
     action:take_pill_Medication_105
     "P1D"^^xsd:dayTimeDuration
     55
     0.5843920056599086
     0.48056771389193875
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_106.}
     {?patient care:state care:state_14.}
     action:take_pill_Medication_106
     "P1D"^^xsd:dayTimeDuration
     37
     0.25242406504683723
     0.8453086935335892
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_107.}
     {?patient care:state care:state_17.}
     action:take_pill_Medication_107
     "P1D"^^xsd:dayTimeDuration
     41
     0.29794663707842733
     0.4828801711684475
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_108.}
     {?patient care:state care:state_19.}
     action:take_pill_Medication_108
     "P1D"^^xsd:dayTimeDuration
     38
     0.888634408362305
     0.45760967567837174
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_109.}
     {?patient care:state care:state_10.}
     action:take_pill_Medication_109
     "P1D"^^xsd:dayTimeDuration
     84
     0.35162976074652097
     0.4234844205467263
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_110.}
     {?patient care:state care:state_3.}
     action:take_pill_Medication_110
     "P1D"^^xsd:dayTimeDuration
     44
     0.9325185421206242
     0.53129436594775
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_111.}
     {?patient care:state care:state_10.}
     action:take_pill_Medication_111
     "P1D"^^xsd:dayTimeDuration
     48
     0.3408585148633547
     0.574304653224166
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_112.}
     {?patient care:state care:state_12.}
     action:take_pill_Medication_112
     "P1D"^^xsd:dayTimeDuration
     46
     0.5318604261836113
     0.7658163276671626
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_113.}
     {?patient care:state care:state_3.}
     action:take_pill_Medication_113
     "P1D"^^xsd:dayTimeDuration
     59
     0.4056922693730315
     0.48117251869534017
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_114.}
     {?patient care:state care:state_5.}
     action:take_pill_Medication_114
     "P1D"^^xsd:dayTimeDuration
     10
     0.3343518243532494
     0.40086232600030525
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_115.}
     {?patient care:state care:state_8.}
     action:take_pill_Medication_115
     "P1D"^^xsd:dayTimeDuration
     19
     0.464676530877235
     0.4646307091899885
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_116.}
     {?patient care:state care:state_15.}
     action:take_pill_Medication_116
     "P1D"^^xsd:dayTimeDuration
     89
     0.8595011504935379
     0.9204264478277094
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_117.}
     {?patient care:state care:state_2.}
     action:take_pill_Medication_117
     "P1D"^^xsd:dayTimeDuration
     32
     0.5459246941452125
     0.6456144528438963
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_118.}
     {?patient care:state care:state_14.}
     action:take_pill_Medication_118
     "P1D"^^xsd:dayTimeDuration
     30
     0.5100010893346087
     0.9077601140533444
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_119.}
     {?patient care:state care:state_10.}
     action:take_pill_Medication_119
     "P1D"^^xsd:dayTimeDuration
     18
     0.6664723540640094
     0.4309669087972998
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_120.}
     {?patient care:state care:state_8.}
     action:take_pill_Medication_120
     "P1D"^^xsd:dayTimeDuration
     93
     0.46884999154528206
     0.8983072721918515
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_121.}
     {?patient care:state care:state_18.}
     action:take_pill_Medication_121
     "P1D"^^xsd:dayTimeDuration
     77
     0.3957857206525218
     0.8119931206546994
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_122.}
     {?patient care:state care:state_10.}
     action:take_pill_Medication_122
     "P1D"^^xsd:dayTimeDuration
     22
     0.8206902829752478
     0.9659040273150747
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_123.}
     {?patient care:state care:state_15.}
     action:take_pill_Medication_123
     "P1D"^^xsd:dayTimeDuration
     14
     0.5136608196294632
     0.5688548723649898
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_124.}
     {?patient care:state care:state_6.}
     action:take_pill_Medication_124
     "P1D"^^xsd:dayTimeDuration
     33
     0.5041256845824668
     0.50447284566635
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_125.}
     {?patient care:state care:state_3.}
     action:take_pill_Medication_125
     "P1D"^^xsd:dayTimeDuration
     53
     0.4216347555414125
     0.8025735141362841
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_126.}
     {?patient care:state care:state_13.}
     action:take_pill_Medication_126
     "P1D"^^xsd:dayTimeDuration
     75
     0.6381837900957671
     0.7910104377438867
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_127.}
     {?patient care:state care:state_5.}
     action:take_pill_Medication_127
     "P1D"^^xsd:dayTimeDuration
     97
     0.8035237027146899
     0.5264355772069176
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_128.}
     {?patient care:state care:state_17.}
     action:take_pill_Medication_128
     "P1D"^^xsd:dayTimeDuration
     55
     0.969721840469691
     0.40157729007616505
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_129.}
     {?patient care:state care:state_16.}
     action:take_pill_Medication_129
     "P1D"^^xsd:dayTimeDuration
     67
     0.38056501690404154
     0.4937292741678767
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_130.}
     {?patient care:state care:state_5.}
     action:take_pill_Medication_130
     "P1D"^^xsd:dayTimeDuration
     17
     0.5730474017780193
     0.7391164436014479
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_131.}
     {?patient care:state care:state_20.}
     action:take_pill_Medication_131
     "P1D"^^xsd:dayTimeDuration
     35
     0.9587557893078089
     0.6568034732760042
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_132.}
     {?patient care:state care:state_11.}
     action:take_pill_Medication_132
     "P1D"^^xsd:dayTimeDuration
     77
     0.252609540865087
     0.7542437839089385
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_133.}
     {?patient care:state care:state_16.}
     action:take_pill_Medication_133
     "P1D"^^xsd:dayTimeDuration
     75
     0.5252076121941891
     0.9348668874374613
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_134.}
     {?patient care:state care:state_17.}
     action:take_pill_Medication_134
     "P1D"^^xsd:dayTimeDuration
     38
     0.2539735425491749
     0.6466585832783295
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_135.}
     {?patient care:state care:state_19.}
     action:take_pill_Medication_135
     "P1D"^^xsd:dayTimeDuration
     56
     0.40681931771356133
     0.4823621598692409
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_136.}
     {?patient care:state care:state_6.}
     action:take_pill_Medication_136
     "P1D"^^xsd:dayTimeDuration
     63
     0.8825817238237108
     0.889197464345522
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_137.}
     {?patient care:state care:state_9.}
     action:take_pill_Medication_137
     "P1D"^^xsd:dayTimeDuration
     67
     0.9241752188163541
     0.5677173962946711
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_138.}
     {?patient care:state care:state_10.}
     action:take_pill_Medication_138
     "P1D"^^xsd:dayTimeDuration
     32
     0.3089976979681903
     0.7425270707802456
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_139.}
     {?patient care:state care:state_19.}
     action:take_pill_Medication_139
     "P1D"^^xsd:dayTimeDuration
     32
     0.6867274276384027
     0.9808271726664815
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_140.}
     {?patient care:state care:state_17.}
     action:take_pill_Medication_140
     "P1D"^^xsd:dayTimeDuration
     88
     0.28881795227246676
     0.3091006156331656
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_141.}
     {?patient care:state care:state_3.}
     action:take_pill_Medication_141
     "P1D"^^xsd:dayTimeDuration
     82
     0.25362689446390435
     0.2799834501714309
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_142.}
     {?patient care:state care:state_12.}
     action:take_pill_Medication_142
     "P1D"^^xsd:dayTimeDuration
     69
     0.47316299809616896
     0.8105912973884224
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_143.}
     {?patient care:state care:state_16.}
     action:take_pill_Medication_143
     "P1D"^^xsd:dayTimeDuration
     67
     0.8704097401088383
     0.28632642342523174
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_144.}
     {?patient care:state care:state_14.}
     action:take_pill_Medication_144
     "P1D"^^xsd:dayTimeDuration
     27
     0.36983752941996334
     0.9641605336092842
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_145.}
     {?patient care:state care:state_6.}
     action:take_pill_Medication_145
     "P1D"^^xsd:dayTimeDuration
     88
     0.2618796864982841
     0.6578576738308899
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_146.}
     {?patient care:state care:state_18.}
     action:take_pill_Medication_146
     "P1D"^^xsd:dayTimeDuration
     84
     0.9613122190490793
     0.3003435627844947
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_147.}
     {?patient care:state care:state_7.}
     action:take_pill_Medication_147
     "P1D"^^xsd:dayTimeDuration
     100
     0.6647232479355542
     0.4152136999656638
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_148.}
     {?patient care:state care:state_9.}
     action:take_pill_Medication_148
     "P1D"^^xsd:dayTimeDuration
     3
     0.6239739775249351
     0.930072100119151
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_149.}
     {?patient care:state care:state_18.}
     action:take_pill_Medication_149
     "P1D"^^xsd:dayTimeDuration
     79
     0.6241709607579161
     0.4171567125282586
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_150.}
     {?patient care:state care:state_5.}
     action:take_pill_Medication_150
     "P1D"^^xsd:dayTimeDuration
     1
     0.6781086382749805
     0.34972580524595526
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_151.}
     {?patient care:state care:state_14.}
     action:take_pill_Medication_151
     "P1D"^^xsd:dayTimeDuration
     42
     0.4485908946286394
     0.9726269535603348
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_152.}
     {?patient care:state care:state_16.}
     action:take_pill_Medication_152
     "P1D"^^xsd:dayTimeDuration
     100
     0.9051268276214202
     0.26221806070737474
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_153.}
     {?patient care:state care:state_6.}
     action:take_pill_Medication_153
     "P1D"^^xsd:dayTimeDuration
     63
     0.6694083933184439
     0.3840931564498614
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_154.}
     {?patient care:state care:state_6.}
     action:take_pill_Medication_154
     "P1D"^^xsd:dayTimeDuration
     82
     0.5930522772031932
     0.9638709839387605
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_155.}
     {?patient care:state care:state_18.}
     action:take_pill_Medication_155
     "P1D"^^xsd:dayTimeDuration
     21
     0.8371070615793443
     0.9765405948477631
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_156.}
     {?patient care:state care:state_16.}
     action:take_pill_Medication_156
     "P1D"^^xsd:dayTimeDuration
     4
     0.4405127020589099
     0.6898820966808166
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_157.}
     {?patient care:state care:state_13.}
     action:take_pill_Medication_157
     "P1D"^^xsd:dayTimeDuration
     99
     0.9219630752007306
     0.5312896044283348
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_158.}
     {?patient care:state care:state_8.}
     action:take_pill_Medication_158
     "P1D"^^xsd:dayTimeDuration
     67
     0.6598819756975389
     0.44151698800278555
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_159.}
     {?patient care:state care:state_15.}
     action:take_pill_Medication_159
     "P1D"^^xsd:dayTimeDuration
     96
     0.9974134323167831
     0.7688239290498016
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_160.}
     {?patient care:state care:state_20.}
     action:take_pill_Medication_160
     "P1D"^^xsd:dayTimeDuration
     80
     0.6927516710036898
     0.7535062711388019
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_161.}
     {?patient care:state care:state_9.}
     action:take_pill_Medication_161
     "P1D"^^xsd:dayTimeDuration
     90
     0.3101924433952338
     0.7572160652940161
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_162.}
     {?patient care:state care:state_7.}
     action:take_pill_Medication_162
     "P1D"^^xsd:dayTimeDuration
     25
     0.5701341225882934
     0.7518013385154241
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_163.}
     {?patient care:state care:state_7.}
     action:take_pill_Medication_163
     "P1D"^^xsd:dayTimeDuration
     6
     0.7305425321547987
     0.3579827555931868
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_164.}
     {?patient care:state care:state_8.}
     action:take_pill_Medication_164
     "P1D"^^xsd:dayTimeDuration
     26
     0.5714448660735894
     0.5304152510355863
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_165.}
     {?patient care:state care:state_19.}
     action:take_pill_Medication_165
     "P1D"^^xsd:dayTimeDuration
     52
     0.9344331128818807
     0.385505726030918
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_166.}
     {?patient care:state care:state_10.}
     action:take_pill_Medication_166
     "P1D"^^xsd:dayTimeDuration
     61
     0.700978196609553
     0.74722903834657
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_167.}
     {?patient care:state care:state_14.}
     action:take_pill_Medication_167
     "P1D"^^xsd:dayTimeDuration
     81
     0.49313608025800904
     0.7838671333254056
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_168.}
     {?patient care:state care:state_19.}
     action:take_pill_Medication_168
     "P1D"^^xsd:dayTimeDuration
     71
     0.6928222892709153
     0.9490407252762247
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_169.}
     {?patient care:state care:state_9.}
     action:take_pill_Medication_169
     "P1D"^^xsd:dayTimeDuration
     65
     0.3814065600245955
     0.5437552339356555
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_170.}
     {?patient care:state care:state_13.}
     action:take_pill_Medication_170
     "P1D"^^xsd:dayTimeDuration
     27
     0.9078126879403078
     0.4251470492675155
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_171.}
     {?patient care:state care:state_4.}
     action:take_pill_Medication_171
     "P1D"^^xsd:dayTimeDuration
     11
     0.8090269598304088
     0.8205972536856943
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_172.}
     {?patient care:state care:state_19.}
     action:take_pill_Medication_172
     "P1D"^^xsd:dayTimeDuration
     64
     0.27036490224022913
     0.9907585737861158
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_173.}
     {?patient care:state care:state_4.}
     action:take_pill_Medication_173
     "P1D"^^xsd:dayTimeDuration
     95
     0.3022069219598502
     0.41202520706928547
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_174.}
     {?patient care:state care:state_8.}
     action:take_pill_Medication_174
     "P1D"^^xsd:dayTimeDuration
     13
     0.8390785021313503
     0.7987847333163989
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_175.}
     {?patient care:state care:state_2.}
     action:take_pill_Medication_175
     "P1D"^^xsd:dayTimeDuration
     53
     0.5410274344794976
     0.6051851507799064
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_176.}
     {?patient care:state care:state_7.}
     action:take_pill_Medication_176
     "P1D"^^xsd:dayTimeDuration
     85
     0.6248691982032584
     0.9236275008644397
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_177.}
     {?patient care:state care:state_3.}
     action:take_pill_Medication_177
     "P1D"^^xsd:dayTimeDuration
     48
     0.82126946531196
     0.5126913924567681
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_178.}
     {?patient care:state care:state_6.}
     action:take_pill_Medication_178
     "P1D"^^xsd:dayTimeDuration
     20
     0.37145811523678696
     0.4055523413623561
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_179.}
     {?patient care:state care:state_14.}
     action:take_pill_Medication_179
     "P1D"^^xsd:dayTimeDuration
     28
     0.7920231456268862
     0.6323592686345715
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_180.}
     {?patient care:state care:state_16.}
     action:take_pill_Medication_180
     "P1D"^^xsd:dayTimeDuration
     92
     0.5500862540245259
     0.46079210955172156
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_181.}
     {?patient care:state care:state_10.}
     action:take_pill_Medication_181
     "P1D"^^xsd:dayTimeDuration
     88
     0.9404520288083758
     0.4970025097598649
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_182.}
     {?patient care:state care:state_19.}
     action:take_pill_Medication_182
     "P1D"^^xsd:dayTimeDuration
     56
     0.6102012886594557
     0.874397340049832
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_183.}
     {?patient care:state care:state_14.}
     action:take_pill_Medication_183
     "P1D"^^xsd:dayTimeDuration
     1
     0.8117359787636175
     0.8407133440526728
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_184.}
     {?patient care:state care:state_4.}
     action:take_pill_Medication_184
     "P1D"^^xsd:dayTimeDuration
     46
     0.849314827704327
     0.3717415032287231
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_185.}
     {?patient care:state care:state_16.}
     action:take_pill_Medication_185
     "P1D"^^xsd:dayTimeDuration
     3
     0.8525798594990811
     0.8913021423959223
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_186.}
     {?patient care:state care:state_3.}
     action:take_pill_Medication_186
     "P1D"^^xsd:dayTimeDuration
     54
     0.5681818396692526
     0.3720893786802816
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_187.}
     {?patient care:state care:state_11.}
     action:take_pill_Medication_187
     "P1D"^^xsd:dayTimeDuration
     35
     0.31177084527230015
     0.4422311250394999
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_188.}
     {?patient care:state care:state_12.}
     action:take_pill_Medication_188
     "P1D"^^xsd:dayTimeDuration
     96
     0.5130889406384567
     0.47391953759113437
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_189.}
     {?patient care:state care:state_10.}
     action:take_pill_Medication_189
     "P1D"^^xsd:dayTimeDuration
     25
     0.3635402356067174
     0.8686984964421454
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_190.}
     {?patient care:state care:state_9.}
     action:take_pill_Medication_190
     "P1D"^^xsd:dayTimeDuration
     6
     0.6627988029726889
     0.3017785968662718
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_191.}
     {?patient care:state care:state_17.}
     action:take_pill_Medication_191
     "P1D"^^xsd:dayTimeDuration
     35
     0.43483984783435814
     0.7233839670280395
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_192.}
     {?patient care:state care:state_12.}
     action:take_pill_Medication_192
     "P1D"^^xsd:dayTimeDuration
     29
     0.3939036156700153
     0.2712503098876563
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_193.}
     {?patient care:state care:state_7.}
     action:take_pill_Medication_193
     "P1D"^^xsd:dayTimeDuration
     64
     0.2798617734319341
     0.8023328074234195
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_194.}
     {?patient care:state care:state_9.}
     action:take_pill_Medication_194
     "P1D"^^xsd:dayTimeDuration
     76
     0.32200687432164554
     0.6002531987493287
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_195.}
     {?patient care:state care:state_20.}
     action:take_pill_Medication_195
     "P1D"^^xsd:dayTimeDuration
     8
     0.6504848540558865
     0.30114387554992744
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_196.}
     {?patient care:state care:state_11.}
     action:take_pill_Medication_196
     "P1D"^^xsd:dayTimeDuration
     14
     0.795906940266965
     0.8998285973215148
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_197.}
     {?patient care:state care:state_11.}
     action:take_pill_Medication_197
     "P1D"^^xsd:dayTimeDuration
     29
     0.7138078288773779
     0.9795047740193589
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_198.}
     {?patient care:state care:state_5.}
     action:take_pill_Medication_198
     "P1D"^^xsd:dayTimeDuration
     33
     0.7982796748471847
     0.9247386109269038
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_199.}
     {?patient care:state care:state_10.}
     action:take_pill_Medication_199
     "P1D"^^xsd:dayTimeDuration
     31
     0.919223539296846
     0.5673605352446325
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_200.}
     {?patient care:state care:state_11.}
     action:take_pill_Medication_200
     "P1D"^^xsd:dayTimeDuration
     19
     0.27266848302687563
     0.7620222575988771
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_201.}
     {?patient care:state care:state_8.}
     action:take_pill_Medication_201
     "P1D"^^xsd:dayTimeDuration
     96
     0.7746284007563017
     0.4478437411087428
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_202.}
     {?patient care:state care:state_9.}
     action:take_pill_Medication_202
     "P1D"^^xsd:dayTimeDuration
     13
     0.850975738385408
     0.6956591382326345
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_203.}
     {?patient care:state care:state_7.}
     action:take_pill_Medication_203
     "P1D"^^xsd:dayTimeDuration
     74
     0.5378447345494964
     0.7168481552879803
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_204.}
     {?patient care:state care:state_19.}
     action:take_pill_Medication_204
     "P1D"^^xsd:dayTimeDuration
     25
     0.65234891955874
     0.261333580786605
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_205.}
     {?patient care:state care:state_6.}
     action:take_pill_Medication_205
     "P1D"^^xsd:dayTimeDuration
     61
     0.3628818473960268
     0.4544643561469339
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_206.}
     {?patient care:state care:state_3.}
     action:take_pill_Medication_206
     "P1D"^^xsd:dayTimeDuration
     82
     0.8009594593918524
     0.9358585150469352
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_207.}
     {?patient care:state care:state_18.}
     action:take_pill_Medication_207
     "P1D"^^xsd:dayTimeDuration
     7
     0.8588126862454386
     0.6866509699795086
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_208.}
     {?patient care:state care:state_16.}
     action:take_pill_Medication_208
     "P1D"^^xsd:dayTimeDuration
     38
     0.736254751222627
     0.818239473981075
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_209.}
     {?patient care:state care:state_13.}
     action:take_pill_Medication_209
     "P1D"^^xsd:dayTimeDuration
     64
     0.9868615372970855
     0.9055678161548931
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_210.}
     {?patient care:state care:state_4.}
     action:take_pill_Medication_210
     "P1D"^^xsd:dayTimeDuration
     54
     0.5336399650735676
     0.3195636690091713
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_211.}
     {?patient care:state care:state_11.}
     action:take_pill_Medication_211
     "P1D"^^xsd:dayTimeDuration
     50
     0.4036265490788905
     0.2959196600937106
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_212.}
     {?patient care:state care:state_18.}
     action:take_pill_Medication_212
     "P1D"^^xsd:dayTimeDuration
     35
     0.6008868493735311
     0.3444478365435034
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_213.}
     {?patient care:state care:state_14.}
     action:take_pill_Medication_213
     "P1D"^^xsd:dayTimeDuration
     73
     0.5288580577759823
     0.4751876771299137
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_214.}
     {?patient care:state care:state_16.}
     action:take_pill_Medication_214
     "P1D"^^xsd:dayTimeDuration
     39
     0.6249716119945781
     0.986468484606417
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_215.}
     {?patient care:state care:state_9.}
     action:take_pill_Medication_215
     "P1D"^^xsd:dayTimeDuration
     93
     0.6112544271299581
     0.510493146924547
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_216.}
     {?patient care:state care:state_14.}
     action:take_pill_Medication_216
     "P1D"^^xsd:dayTimeDuration
     33
     0.7402603445235637
     0.3086007674072472
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_217.}
     {?patient care:state care:state_11.}
     action:take_pill_Medication_217
     "P1D"^^xsd:dayTimeDuration
     50
     0.4671400569647294
     0.6769948874604256
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_218.}
     {?patient care:state care:state_16.}
     action:take_pill_Medication_218
     "P1D"^^xsd:dayTimeDuration
     96
     0.7847778964945815
     0.6238160749523841
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_219.}
     {?patient care:state care:state_5.}
     action:take_pill_Medication_219
     "P1D"^^xsd:dayTimeDuration
     48
     0.9989839039785808
     0.8226131373083234
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_220.}
     {?patient care:state care:state_18.}
     action:take_pill_Medication_220
     "P1D"^^xsd:dayTimeDuration
     83
     0.5602065058769715
     0.32412374610114625
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_221.}
     {?patient care:state care:state_11.}
     action:take_pill_Medication_221
     "P1D"^^xsd:dayTimeDuration
     68
     0.9128351336700558
     0.3305904322155051
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_222.}
     {?patient care:state care:state_4.}
     action:take_pill_Medication_222
     "P1D"^^xsd:dayTimeDuration
     72
     0.33889435938691975
     0.888554240152065
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_223.}
     {?patient care:state care:state_18.}
     action:take_pill_Medication_223
     "P1D"^^xsd:dayTimeDuration
     29
     0.3237108069356727
     0.6887516947800862
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_224.}
     {?patient care:state care:state_11.}
     action:take_pill_Medication_224
     "P1D"^^xsd:dayTimeDuration
     20
     0.7198184237027367
     0.8255279525192631
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_225.}
     {?patient care:state care:state_12.}
     action:take_pill_Medication_225
     "P1D"^^xsd:dayTimeDuration
     29
     0.28907940922133424
     0.2727672268895538
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_226.}
     {?patient care:state care:state_20.}
     action:take_pill_Medication_226
     "P1D"^^xsd:dayTimeDuration
     75
     0.5845180245718044
     0.7437297427728633
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_227.}
     {?patient care:state care:state_19.}
     action:take_pill_Medication_227
     "P1D"^^xsd:dayTimeDuration
     22
     0.8326105245553292
     0.9239914652690676
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_228.}
     {?patient care:state care:state_13.}
     action:take_pill_Medication_228
     "P1D"^^xsd:dayTimeDuration
     72
     0.8841796313699793
     0.9348186539283453
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_229.}
     {?patient care:state care:state_17.}
     action:take_pill_Medication_229
     "P1D"^^xsd:dayTimeDuration
     6
     0.752683765983052
     0.4072431233058652
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_230.}
     {?patient care:state care:state_9.}
     action:take_pill_Medication_230
     "P1D"^^xsd:dayTimeDuration
     11
     0.344597310325524
     0.2984356816254485
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_231.}
     {?patient care:state care:state_17.}
     action:take_pill_Medication_231
     "P1D"^^xsd:dayTimeDuration
     28
     0.5620954727576397
     0.7859706386383067
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_232.}
     {?patient care:state care:state_5.}
     action:take_pill_Medication_232
     "P1D"^^xsd:dayTimeDuration
     17
     0.6833773681861884
     0.6571810643326754
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_233.}
     {?patient care:state care:state_6.}
     action:take_pill_Medication_233
     "P1D"^^xsd:dayTimeDuration
     16
     0.6801563156113483
     0.3505247729104727
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_234.}
     {?patient care:state care:state_3.}
     action:take_pill_Medication_234
     "P1D"^^xsd:dayTimeDuration
     34
     0.6941365134031753
     0.3861339679257745
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_235.}
     {?patient care:state care:state_5.}
     action:take_pill_Medication_235
     "P1D"^^xsd:dayTimeDuration
     4
     0.38980299596013906
     0.345422912760066
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_236.}
     {?patient care:state care:state_18.}
     action:take_pill_Medication_236
     "P1D"^^xsd:dayTimeDuration
     7
     0.6782807673646268
     0.6005920913871894
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_237.}
     {?patient care:state care:state_11.}
     action:take_pill_Medication_237
     "P1D"^^xsd:dayTimeDuration
     99
     0.5676126453904649
     0.5824737206000622
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_238.}
     {?patient care:state care:state_12.}
     action:take_pill_Medication_238
     "P1D"^^xsd:dayTimeDuration
     60
     0.8936075614602926
     0.3135709988886276
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_239.}
     {?patient care:state care:state_7.}
     action:take_pill_Medication_239
     "P1D"^^xsd:dayTimeDuration
     1
     0.38530724433309227
     0.4970948256507529
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_240.}
     {?patient care:state care:state_4.}
     action:take_pill_Medication_240
     "P1D"^^xsd:dayTimeDuration
     73
     0.9765097928004947
     0.3323195166126484
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_241.}
     {?patient care:state care:state_9.}
     action:take_pill_Medication_241
     "P1D"^^xsd:dayTimeDuration
     9
     0.9570478585280452
     0.8241696796135203
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_242.}
     {?patient care:state care:state_15.}
     action:take_pill_Medication_242
     "P1D"^^xsd:dayTimeDuration
     95
     0.8437278796034563
     0.8088899094520723
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_243.}
     {?patient care:state care:state_11.}
     action:take_pill_Medication_243
     "P1D"^^xsd:dayTimeDuration
     88
     0.8691540642828112
     0.5069912903968032
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_244.}
     {?patient care:state care:state_5.}
     action:take_pill_Medication_244
     "P1D"^^xsd:dayTimeDuration
     10
     0.45713644266944764
     0.8610170202039553
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_245.}
     {?patient care:state care:state_4.}
     action:take_pill_Medication_245
     "P1D"^^xsd:dayTimeDuration
     28
     0.9704288562612916
     0.29020470840978807
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_246.}
     {?patient care:state care:state_10.}
     action:take_pill_Medication_246
     "P1D"^^xsd:dayTimeDuration
     57
     0.511708788001501
     0.8877114759387807
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_247.}
     {?patient care:state care:state_4.}
     action:take_pill_Medication_247
     "P1D"^^xsd:dayTimeDuration
     27
     0.5021359280126488
     0.391148007469512
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_248.}
     {?patient care:state care:state_17.}
     action:take_pill_Medication_248
     "P1D"^^xsd:dayTimeDuration
     95
     0.43833785522212737
     0.8884425797216097
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_249.}
     {?patient care:state care:state_9.}
     action:take_pill_Medication_249
     "P1D"^^xsd:dayTimeDuration
     87
     0.7160440287289732
     0.28579071078004564
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_250.}
     {?patient care:state care:state_13.}
     action:take_pill_Medication_250
     "P1D"^^xsd:dayTimeDuration
     32
     0.45633344603109177
     0.65838198243492
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_251.}
     {?patient care:state care:state_7.}
     action:take_pill_Medication_251
     "P1D"^^xsd:dayTimeDuration
     42
     0.5389800480203214
     0.54896050761623
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_252.}
     {?patient care:state care:state_16.}
     action:take_pill_Medication_252
     "P1D"^^xsd:dayTimeDuration
     10
     0.707545748394874
     0.3403990543147941
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_253.}
     {?patient care:state care:state_19.}
     action:take_pill_Medication_253
     "P1D"^^xsd:dayTimeDuration
     33
     0.8334926208653954
     0.7661251131988688
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_254.}
     {?patient care:state care:state_12.}
     action:take_pill_Medication_254
     "P1D"^^xsd:dayTimeDuration
     76
     0.8653380801109615
     0.5599179363698714
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_255.}
     {?patient care:state care:state_14.}
     action:take_pill_Medication_255
     "P1D"^^xsd:dayTimeDuration
     26
     0.34898466777408366
     0.25470152521344486
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_256.}
     {?patient care:state care:state_15.}
     action:take_pill_Medication_256
     "P1D"^^xsd:dayTimeDuration
     6
     0.5443817737682766
     0.9675213169954274
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_257.}
     {?patient care:state care:state_17.}
     action:take_pill_Medication_257
     "P1D"^^xsd:dayTimeDuration
     87
     0.3963941251285529
     0.8232670279117347
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_258.}
     {?patient care:state care:state_15.}
     action:take_pill_Medication_258
     "P1D"^^xsd:dayTimeDuration
     65
     0.6777656797325763
     0.6836708331515562
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_259.}
     {?patient care:state care:state_18.}
     action:take_pill_Medication_259
     "P1D"^^xsd:dayTimeDuration
     80
     0.43184538370613995
     0.9854643612424923
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_260.}
     {?patient care:state care:state_10.}
     action:take_pill_Medication_260
     "P1D"^^xsd:dayTimeDuration
     83
     0.9788525617297111
     0.8161567213212267
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_261.}
     {?patient care:state care:state_16.}
     action:take_pill_Medication_261
     "P1D"^^xsd:dayTimeDuration
     7
     0.9788066505118087
     0.986117715435486
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_262.}
     {?patient care:state care:state_6.}
     action:take_pill_Medication_262
     "P1D"^^xsd:dayTimeDuration
     44
     0.3597675795926454
     0.48472030573314323
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_263.}
     {?patient care:state care:state_10.}
     action:take_pill_Medication_263
     "P1D"^^xsd:dayTimeDuration
     13
     0.30499482064381084
     0.4050367017358055
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_264.}
     {?patient care:state care:state_4.}
     action:take_pill_Medication_264
     "P1D"^^xsd:dayTimeDuration
     99
     0.43902761978975563
     0.5880368181149707
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_265.}
     {?patient care:state care:state_17.}
     action:take_pill_Medication_265
     "P1D"^^xsd:dayTimeDuration
     52
     0.7360715800067288
     0.28398155665281466
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_266.}
     {?patient care:state care:state_19.}
     action:take_pill_Medication_266
     "P1D"^^xsd:dayTimeDuration
     29
     0.3518918257886149
     0.9143747189354086
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_267.}
     {?patient care:state care:state_8.}
     action:take_pill_Medication_267
     "P1D"^^xsd:dayTimeDuration
     51
     0.3437268452706641
     0.27724661680478324
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_268.}
     {?patient care:state care:state_16.}
     action:take_pill_Medication_268
     "P1D"^^xsd:dayTimeDuration
     75
     0.7922494651326417
     0.28205920090892267
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_269.}
     {?patient care:state care:state_8.}
     action:take_pill_Medication_269
     "P1D"^^xsd:dayTimeDuration
     31
     0.7569414825636418
     0.25359617839733983
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_270.}
     {?patient care:state care:state_3.}
     action:take_pill_Medication_270
     "P1D"^^xsd:dayTimeDuration
     43
     0.35791277309691144
     0.5682121948749744
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_271.}
     {?patient care:state care:state_20.}
     action:take_pill_Medication_271
     "P1D"^^xsd:dayTimeDuration
     72
     0.2692752234603699
     0.8629911911572344
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_272.}
     {?patient care:state care:state_13.}
     action:take_pill_Medication_272
     "P1D"^^xsd:dayTimeDuration
     57
     0.48286365182404173
     0.9305531807944614
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_273.}
     {?patient care:state care:state_13.}
     action:take_pill_Medication_273
     "P1D"^^xsd:dayTimeDuration
     46
     0.8638653570999633
     0.48707255097225327
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_274.}
     {?patient care:state care:state_8.}
     action:take_pill_Medication_274
     "P1D"^^xsd:dayTimeDuration
     59
     0.5040268638522275
     0.42083890353250475
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_275.}
     {?patient care:state care:state_11.}
     action:take_pill_Medication_275
     "P1D"^^xsd:dayTimeDuration
     82
     0.8358959161572995
     0.8486866461991204
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_276.}
     {?patient care:state care:state_9.}
     action:take_pill_Medication_276
     "P1D"^^xsd:dayTimeDuration
     59
     0.9007273416052484
     0.40476406636988566
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_277.}
     {?patient care:state care:state_8.}
     action:take_pill_Medication_277
     "P1D"^^xsd:dayTimeDuration
     28
     0.6060556875406639
     0.9164599532861959
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_278.}
     {?patient care:state care:state_8.}
     action:take_pill_Medication_278
     "P1D"^^xsd:dayTimeDuration
     79
     0.6764896862041525
     0.7400155048308615
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_279.}
     {?patient care:state care:state_4.}
     action:take_pill_Medication_279
     "P1D"^^xsd:dayTimeDuration
     61
     0.9731821645767755
     0.8152976252836499
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_280.}
     {?patient care:state care:state_16.}
     action:take_pill_Medication_280
     "P1D"^^xsd:dayTimeDuration
     24
     0.457473475640069
     0.5922727522879527
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_281.}
     {?patient care:state care:state_17.}
     action:take_pill_Medication_281
     "P1D"^^xsd:dayTimeDuration
     2
     0.464150748456964
     0.48583493045886417
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_282.}
     {?patient care:state care:state_9.}
     action:take_pill_Medication_282
     "P1D"^^xsd:dayTimeDuration
     31
     0.7571252415270273
     0.42883583862878516
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_283.}
     {?patient care:state care:state_12.}
     action:take_pill_Medication_283
     "P1D"^^xsd:dayTimeDuration
     96
     0.8825132550425966
     0.955872600825284
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_284.}
     {?patient care:state care:state_14.}
     action:take_pill_Medication_284
     "P1D"^^xsd:dayTimeDuration
     15
     0.3491521645460363
     0.5124940202376276
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_285.}
     {?patient care:state care:state_20.}
     action:take_pill_Medication_285
     "P1D"^^xsd:dayTimeDuration
     7
     0.5960355161496441
     0.625891427239983
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_286.}
     {?patient care:state care:state_14.}
     action:take_pill_Medication_286
     "P1D"^^xsd:dayTimeDuration
     57
     0.5337360001979233
     0.7150392416536353
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_287.}
     {?patient care:state care:state_9.}
     action:take_pill_Medication_287
     "P1D"^^xsd:dayTimeDuration
     31
     0.7342759830880172
     0.30925693301424595
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_288.}
     {?patient care:state care:state_4.}
     action:take_pill_Medication_288
     "P1D"^^xsd:dayTimeDuration
     15
     0.6143425229449168
     0.47928070853736987
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_289.}
     {?patient care:state care:state_8.}
     action:take_pill_Medication_289
     "P1D"^^xsd:dayTimeDuration
     91
     0.9206581272793337
     0.47895126932217325
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_290.}
     {?patient care:state care:state_19.}
     action:take_pill_Medication_290
     "P1D"^^xsd:dayTimeDuration
     29
     0.5172016342645023
     0.8585093573675425
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_291.}
     {?patient care:state care:state_12.}
     action:take_pill_Medication_291
     "P1D"^^xsd:dayTimeDuration
     31
     0.5092354196905536
     0.7911246797389945
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_292.}
     {?patient care:state care:state_6.}
     action:take_pill_Medication_292
     "P1D"^^xsd:dayTimeDuration
     80
     0.39706328867999274
     0.26765500813067444
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_293.}
     {?patient care:state care:state_3.}
     action:take_pill_Medication_293
     "P1D"^^xsd:dayTimeDuration
     52
     0.7855938496176764
     0.9402452327441733
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_294.}
     {?patient care:state care:state_9.}
     action:take_pill_Medication_294
     "P1D"^^xsd:dayTimeDuration
     55
     0.5572860514850385
     0.5146658951272666
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_295.}
     {?patient care:state care:state_12.}
     action:take_pill_Medication_295
     "P1D"^^xsd:dayTimeDuration
     93
     0.8223730188443388
     0.7707587869598946
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_296.}
     {?patient care:state care:state_8.}
     action:take_pill_Medication_296
     "P1D"^^xsd:dayTimeDuration
     83
     0.902303587316126
     0.6341342064262633
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_297.}
     {?patient care:state care:state_8.}
     action:take_pill_Medication_297
     "P1D"^^xsd:dayTimeDuration
     10
     0.559676534677422
     0.6429957338862926
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_298.}
     {?patient care:state care:state_6.}
     action:take_pill_Medication_298
     "P1D"^^xsd:dayTimeDuration
     85
     0.5871479895276848
     0.33126024149260436
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_299.}
     {?patient care:state care:state_12.}
     action:take_pill_Medication_299
     "P1D"^^xsd:dayTimeDuration
     1
     0.5355489049771177
     0.6326368866531094
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_300.}
     {?patient care:state care:state_19.}
     action:take_pill_Medication_300
     "P1D"^^xsd:dayTimeDuration
     29
     0.8467642992508304
     0.5725568484022516
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_301.}
     {?patient care:state care:state_4.}
     action:take_pill_Medication_301
     "P1D"^^xsd:dayTimeDuration
     76
     0.8709971360280995
     0.3821724909698557
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_302.}
     {?patient care:state care:state_18.}
     action:take_pill_Medication_302
     "P1D"^^xsd:dayTimeDuration
     97
     0.7131327205810797
     0.3068540640429125
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_303.}
     {?patient care:state care:state_13.}
     action:take_pill_Medication_303
     "P1D"^^xsd:dayTimeDuration
     78
     0.4660687582787675
     0.7835726162882584
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_304.}
     {?patient care:state care:state_13.}
     action:take_pill_Medication_304
     "P1D"^^xsd:dayTimeDuration
     14
     0.2871926787149116
     0.9641688589182009
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_305.}
     {?patient care:state care:state_13.}
     action:take_pill_Medication_305
     "P1D"^^xsd:dayTimeDuration
     81
     0.5128225700334184
     0.4721709078260077
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_306.}
     {?patient care:state care:state_10.}
     action:take_pill_Medication_306
     "P1D"^^xsd:dayTimeDuration
     19
     0.9695453377021191
     0.49917705531264944
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_307.}
     {?patient care:state care:state_11.}
     action:take_pill_Medication_307
     "P1D"^^xsd:dayTimeDuration
     33
     0.26203918749655564
     0.3709824265291174
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_308.}
     {?patient care:state care:state_16.}
     action:take_pill_Medication_308
     "P1D"^^xsd:dayTimeDuration
     35
     0.4925710646317424
     0.6751108417152825
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_309.}
     {?patient care:state care:state_6.}
     action:take_pill_Medication_309
     "P1D"^^xsd:dayTimeDuration
     16
     0.9054222466172789
     0.8655908186610572
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_310.}
     {?patient care:state care:state_7.}
     action:take_pill_Medication_310
     "P1D"^^xsd:dayTimeDuration
     65
     0.2622834578702255
     0.6919031555618002
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_311.}
     {?patient care:state care:state_9.}
     action:take_pill_Medication_311
     "P1D"^^xsd:dayTimeDuration
     63
     0.8026142950604149
     0.9153334028845552
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_312.}
     {?patient care:state care:state_17.}
     action:take_pill_Medication_312
     "P1D"^^xsd:dayTimeDuration
     56
     0.2636629394855737
     0.4788970858369364
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_313.}
     {?patient care:state care:state_5.}
     action:take_pill_Medication_313
     "P1D"^^xsd:dayTimeDuration
     83
     0.8059456595693912
     0.38727693456087386
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_314.}
     {?patient care:state care:state_14.}
     action:take_pill_Medication_314
     "P1D"^^xsd:dayTimeDuration
     10
     0.4574969912668897
     0.7205181087284565
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_315.}
     {?patient care:state care:state_18.}
     action:take_pill_Medication_315
     "P1D"^^xsd:dayTimeDuration
     86
     0.8183833695399256
     0.28375457163656026
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_316.}
     {?patient care:state care:state_6.}
     action:take_pill_Medication_316
     "P1D"^^xsd:dayTimeDuration
     49
     0.692199934611493
     0.33229711185708655
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_317.}
     {?patient care:state care:state_15.}
     action:take_pill_Medication_317
     "P1D"^^xsd:dayTimeDuration
     92
     0.9110351971340036
     0.719225785218669
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_318.}
     {?patient care:state care:state_10.}
     action:take_pill_Medication_318
     "P1D"^^xsd:dayTimeDuration
     93
     0.45432325680377844
     0.8731796177351803
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_319.}
     {?patient care:state care:state_15.}
     action:take_pill_Medication_319
     "P1D"^^xsd:dayTimeDuration
     35
     0.4219561066131477
     0.5541469744866361
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_320.}
     {?patient care:state care:state_15.}
     action:take_pill_Medication_320
     "P1D"^^xsd:dayTimeDuration
     9
     0.4221389266230215
     0.2647540815358888
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_321.}
     {?patient care:state care:state_14.}
     action:take_pill_Medication_321
     "P1D"^^xsd:dayTimeDuration
     94
     0.3833461902678637
     0.47396212698113277
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_322.}
     {?patient care:state care:state_4.}
     action:take_pill_Medication_322
     "P1D"^^xsd:dayTimeDuration
     50
     0.8568663179479461
     0.9887873597630511
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_323.}
     {?patient care:state care:state_7.}
     action:take_pill_Medication_323
     "P1D"^^xsd:dayTimeDuration
     86
     0.9200669897188618
     0.7965826813195559
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_324.}
     {?patient care:state care:state_3.}
     action:take_pill_Medication_324
     "P1D"^^xsd:dayTimeDuration
     20
     0.5341884032162633
     0.7028474539207985
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_325.}
     {?patient care:state care:state_14.}
     action:take_pill_Medication_325
     "P1D"^^xsd:dayTimeDuration
     67
     0.5026458439784669
     0.6273854328716949
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_326.}
     {?patient care:state care:state_9.}
     action:take_pill_Medication_326
     "P1D"^^xsd:dayTimeDuration
     96
     0.5674114271229447
     0.74584370043583
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_327.}
     {?patient care:state care:state_18.}
     action:take_pill_Medication_327
     "P1D"^^xsd:dayTimeDuration
     46
     0.9778709630818924
     0.3149882567816068
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_328.}
     {?patient care:state care:state_15.}
     action:take_pill_Medication_328
     "P1D"^^xsd:dayTimeDuration
     12
     0.4173452607852344
     0.5960322950236039
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_329.}
     {?patient care:state care:state_7.}
     action:take_pill_Medication_329
     "P1D"^^xsd:dayTimeDuration
     30
     0.2792846170657232
     0.36654467976252175
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_330.}
     {?patient care:state care:state_17.}
     action:take_pill_Medication_330
     "P1D"^^xsd:dayTimeDuration
     53
     0.4849401301322188
     0.8930559565388456
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_331.}
     {?patient care:state care:state_20.}
     action:take_pill_Medication_331
     "P1D"^^xsd:dayTimeDuration
     44
     0.9080521413778502
     0.8073762853583091
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_332.}
     {?patient care:state care:state_15.}
     action:take_pill_Medication_332
     "P1D"^^xsd:dayTimeDuration
     25
     0.9900010007282356
     0.28952266892268513
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_333.}
     {?patient care:state care:state_10.}
     action:take_pill_Medication_333
     "P1D"^^xsd:dayTimeDuration
     76
     0.9592089013887455
     0.49408445786062816
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_334.}
     {?patient care:state care:state_7.}
     action:take_pill_Medication_334
     "P1D"^^xsd:dayTimeDuration
     24
     0.9558984864113995
     0.5705090736483845
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_335.}
     {?patient care:state care:state_6.}
     action:take_pill_Medication_335
     "P1D"^^xsd:dayTimeDuration
     95
     0.4900065626940022
     0.9373174481368622
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_336.}
     {?patient care:state care:state_7.}
     action:take_pill_Medication_336
     "P1D"^^xsd:dayTimeDuration
     98
     0.3172568382184952
     0.2950633514710781
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_337.}
     {?patient care:state care:state_20.}
     action:take_pill_Medication_337
     "P1D"^^xsd:dayTimeDuration
     40
     0.8498184413690195
     0.3097487979317828
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_338.}
     {?patient care:state care:state_13.}
     action:take_pill_Medication_338
     "P1D"^^xsd:dayTimeDuration
     3
     0.8046170085882611
     0.7419744504795953
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_339.}
     {?patient care:state care:state_20.}
     action:take_pill_Medication_339
     "P1D"^^xsd:dayTimeDuration
     40
     0.25156616289663875
     0.5609552299255991
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_340.}
     {?patient care:state care:state_15.}
     action:take_pill_Medication_340
     "P1D"^^xsd:dayTimeDuration
     89
     0.5755279286925358
     0.42070194179989573
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_341.}
     {?patient care:state care:state_10.}
     action:take_pill_Medication_341
     "P1D"^^xsd:dayTimeDuration
     90
     0.9546948208430499
     0.36715966367566
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_342.}
     {?patient care:state care:state_4.}
     action:take_pill_Medication_342
     "P1D"^^xsd:dayTimeDuration
     15
     0.747562910659046
     0.8956801086164068
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_343.}
     {?patient care:state care:state_13.}
     action:take_pill_Medication_343
     "P1D"^^xsd:dayTimeDuration
     59
     0.6935313921133085
     0.8883885011315034
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_344.}
     {?patient care:state care:state_19.}
     action:take_pill_Medication_344
     "P1D"^^xsd:dayTimeDuration
     56
     0.7569314719013394
     0.7938032736802226
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_345.}
     {?patient care:state care:state_10.}
     action:take_pill_Medication_345
     "P1D"^^xsd:dayTimeDuration
     99
     0.4407453313831369
     0.8291842834082178
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_346.}
     {?patient care:state care:state_7.}
     action:take_pill_Medication_346
     "P1D"^^xsd:dayTimeDuration
     58
     0.7920557683880909
     0.8492008995833589
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_347.}
     {?patient care:state care:state_14.}
     action:take_pill_Medication_347
     "P1D"^^xsd:dayTimeDuration
     49
     0.6796058560312945
     0.2631623405799115
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_348.}
     {?patient care:state care:state_20.}
     action:take_pill_Medication_348
     "P1D"^^xsd:dayTimeDuration
     50
     0.9712540177305309
     0.46270162278789606
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_349.}
     {?patient care:state care:state_15.}
     action:take_pill_Medication_349
     "P1D"^^xsd:dayTimeDuration
     14
     0.43054288481202363
     0.8675300735115246
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_350.}
     {?patient care:state care:state_7.}
     action:take_pill_Medication_350
     "P1D"^^xsd:dayTimeDuration
     44
     0.29974840399218505
     0.40000987694621115
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_351.}
     {?patient care:state care:state_7.}
     action:take_pill_Medication_351
     "P1D"^^xsd:dayTimeDuration
     30
     0.7644665315454947
     0.6537082566365
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_352.}
     {?patient care:state care:state_8.}
     action:take_pill_Medication_352
     "P1D"^^xsd:dayTimeDuration
     75
     0.7867485368746505
     0.9301035086172033
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_353.}
     {?patient care:state care:state_19.}
     action:take_pill_Medication_353
     "P1D"^^xsd:dayTimeDuration
     89
     0.8824130615705255
     0.6410986948657585
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_354.}
     {?patient care:state care:state_12.}
     action:take_pill_Medication_354
     "P1D"^^xsd:dayTimeDuration
     65
     0.7257171659060919
     0.4205922342309574
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_355.}
     {?patient care:state care:state_13.}
     action:take_pill_Medication_355
     "P1D"^^xsd:dayTimeDuration
     36
     0.6529489320294309
     0.7981181107028241
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_356.}
     {?patient care:state care:state_6.}
     action:take_pill_Medication_356
     "P1D"^^xsd:dayTimeDuration
     2
     0.6527247434771746
     0.7768661113662352
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_357.}
     {?patient care:state care:state_12.}
     action:take_pill_Medication_357
     "P1D"^^xsd:dayTimeDuration
     14
     0.8081245795907179
     0.3307281633516441
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_358.}
     {?patient care:state care:state_18.}
     action:take_pill_Medication_358
     "P1D"^^xsd:dayTimeDuration
     38
     0.634771934112053
     0.582889085313289
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_359.}
     {?patient care:state care:state_19.}
     action:take_pill_Medication_359
     "P1D"^^xsd:dayTimeDuration
     38
     0.4536559577726286
     0.5865720077962127
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_360.}
     {?patient care:state care:state_10.}
     action:take_pill_Medication_360
     "P1D"^^xsd:dayTimeDuration
     62
     0.7806851086215063
     0.9918795776673316
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_361.}
     {?patient care:state care:state_11.}
     action:take_pill_Medication_361
     "P1D"^^xsd:dayTimeDuration
     62
     0.5757075037246698
     0.9749333224080625
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_362.}
     {?patient care:state care:state_5.}
     action:take_pill_Medication_362
     "P1D"^^xsd:dayTimeDuration
     14
     0.32818861181912296
     0.4810418445871737
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_363.}
     {?patient care:state care:state_18.}
     action:take_pill_Medication_363
     "P1D"^^xsd:dayTimeDuration
     12
     0.658877710190783
     0.7377155387884972
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_364.}
     {?patient care:state care:state_11.}
     action:take_pill_Medication_364
     "P1D"^^xsd:dayTimeDuration
     4
     0.6976708201656584
     0.989478087863023
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_365.}
     {?patient care:state care:state_13.}
     action:take_pill_Medication_365
     "P1D"^^xsd:dayTimeDuration
     53
     0.5467988359632628
     0.7882427664222864
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_366.}
     {?patient care:state care:state_5.}
     action:take_pill_Medication_366
     "P1D"^^xsd:dayTimeDuration
     59
     0.7333951612950185
     0.2657126080713871
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_367.}
     {?patient care:state care:state_9.}
     action:take_pill_Medication_367
     "P1D"^^xsd:dayTimeDuration
     38
     0.6816747641286048
     0.6652985631155887
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_368.}
     {?patient care:state care:state_8.}
     action:take_pill_Medication_368
     "P1D"^^xsd:dayTimeDuration
     60
     0.6989018914345619
     0.578375058045569
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_369.}
     {?patient care:state care:state_18.}
     action:take_pill_Medication_369
     "P1D"^^xsd:dayTimeDuration
     68
     0.6227109993677812
     0.7920972211543462
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_370.}
     {?patient care:state care:state_13.}
     action:take_pill_Medication_370
     "P1D"^^xsd:dayTimeDuration
     80
     0.5586458531298908
     0.5377966405214971
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_371.}
     {?patient care:state care:state_12.}
     action:take_pill_Medication_371
     "P1D"^^xsd:dayTimeDuration
     61
     0.8943148343585183
     0.4273544663584069
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_372.}
     {?patient care:state care:state_7.}
     action:take_pill_Medication_372
     "P1D"^^xsd:dayTimeDuration
     17
     0.760546332985232
     0.7761763712221385
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_373.}
     {?patient care:state care:state_3.}
     action:take_pill_Medication_373
     "P1D"^^xsd:dayTimeDuration
     60
     0.625763968476955
     0.9171096509986546
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_374.}
     {?patient care:state care:state_15.}
     action:take_pill_Medication_374
     "P1D"^^xsd:dayTimeDuration
     95
     0.4587997250940593
     0.5393424326051698
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_375.}
     {?patient care:state care:state_16.}
     action:take_pill_Medication_375
     "P1D"^^xsd:dayTimeDuration
     34
     0.5331223821229957
     0.6839382258197405
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_376.}
     {?patient care:state care:state_8.}
     action:take_pill_Medication_376
     "P1D"^^xsd:dayTimeDuration
     71
     0.690801560366259
     0.7763260269233401
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_377.}
     {?patient care:state care:state_19.}
     action:take_pill_Medication_377
     "P1D"^^xsd:dayTimeDuration
     2
     0.26803168519637066
     0.38202545218172346
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_378.}
     {?patient care:state care:state_13.}
     action:take_pill_Medication_378
     "P1D"^^xsd:dayTimeDuration
     25
     0.7957853676165413
     0.850619703914245
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_379.}
     {?patient care:state care:state_11.}
     action:take_pill_Medication_379
     "P1D"^^xsd:dayTimeDuration
     86
     0.43339047620002746
     0.9557900131993673
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_380.}
     {?patient care:state care:state_14.}
     action:take_pill_Medication_380
     "P1D"^^xsd:dayTimeDuration
     89
     0.30938413798650455
     0.7152814888477173
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_381.}
     {?patient care:state care:state_7.}
     action:take_pill_Medication_381
     "P1D"^^xsd:dayTimeDuration
     1
     0.7658677783184946
     0.6183457764556477
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_382.}
     {?patient care:state care:state_9.}
     action:take_pill_Medication_382
     "P1D"^^xsd:dayTimeDuration
     64
     0.9828013102257334
     0.7127190890142627
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_383.}
     {?patient care:state care:state_15.}
     action:take_pill_Medication_383
     "P1D"^^xsd:dayTimeDuration
     71
     0.8270191187684839
     0.44829643691786164
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_384.}
     {?patient care:state care:state_14.}
     action:take_pill_Medication_384
     "P1D"^^xsd:dayTimeDuration
     38
     0.6839464679429633
     0.8617388622827415
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_385.}
     {?patient care:state care:state_8.}
     action:take_pill_Medication_385
     "P1D"^^xsd:dayTimeDuration
     79
     0.8557391376954435
     0.970896679937597
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_386.}
     {?patient care:state care:state_19.}
     action:take_pill_Medication_386
     "P1D"^^xsd:dayTimeDuration
     98
     0.937233676900658
     0.48658733999481885
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_387.}
     {?patient care:state care:state_18.}
     action:take_pill_Medication_387
     "P1D"^^xsd:dayTimeDuration
     67
     0.8283246082487016
     0.7308763364521247
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_388.}
     {?patient care:state care:state_11.}
     action:take_pill_Medication_388
     "P1D"^^xsd:dayTimeDuration
     17
     0.401059477055147
     0.7285524454384587
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_389.}
     {?patient care:state care:state_13.}
     action:take_pill_Medication_389
     "P1D"^^xsd:dayTimeDuration
     34
     0.43351386550015564
     0.32894127676046303
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_390.}
     {?patient care:state care:state_20.}
     action:take_pill_Medication_390
     "P1D"^^xsd:dayTimeDuration
     1
     0.49966539552292877
     0.30607709442519526
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_391.}
     {?patient care:state care:state_6.}
     action:take_pill_Medication_391
     "P1D"^^xsd:dayTimeDuration
     37
     0.991216905116997
     0.626464272954153
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_392.}
     {?patient care:state care:state_19.}
     action:take_pill_Medication_392
     "P1D"^^xsd:dayTimeDuration
     93
     0.9333091667531962
     0.97084686671933
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_393.}
     {?patient care:state care:state_5.}
     action:take_pill_Medication_393
     "P1D"^^xsd:dayTimeDuration
     45
     0.7230080121156386
     0.7358510244751091
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_394.}
     {?patient care:state care:state_2.}
     action:take_pill_Medication_394
     "P1D"^^xsd:dayTimeDuration
     32
     0.4496157258569368
     0.37561291242521444
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_395.}
     {?patient care:state care:state_20.}
     action:take_pill_Medication_395
     "P1D"^^xsd:dayTimeDuration
     7
     0.666195566606036
     0.8519040875898323
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_396.}
     {?patient care:state care:state_19.}
     action:take_pill_Medication_396
     "P1D"^^xsd:dayTimeDuration
     8
     0.2639627509944283
     0.6110799259057755
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_397.}
     {?patient care:state care:state_2.}
     action:take_pill_Medication_397
     "P1D"^^xsd:dayTimeDuration
     70
     0.7405640152014457
     0.9799143343173623
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_398.}
     {?patient care:state care:state_12.}
     action:take_pill_Medication_398
     "P1D"^^xsd:dayTimeDuration
     20
     0.322148777249307
     0.7888321276234613
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_399.}
     {?patient care:state care:state_14.}
     action:take_pill_Medication_399
     "P1D"^^xsd:dayTimeDuration
     9
     0.30042545627442097
     0.9753643728368147
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_400.}
     {?patient care:state care:state_13.}
     action:take_pill_Medication_400
     "P1D"^^xsd:dayTimeDuration
     51
     0.28866864456583474
     0.26986472276210416
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_401.}
     {?patient care:state care:state_6.}
     action:take_pill_Medication_401
     "P1D"^^xsd:dayTimeDuration
     51
     0.3953969326407136
     0.9887536587647909
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_402.}
     {?patient care:state care:state_14.}
     action:take_pill_Medication_402
     "P1D"^^xsd:dayTimeDuration
     85
     0.8803347718160867
     0.7328181490158271
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_403.}
     {?patient care:state care:state_14.}
     action:take_pill_Medication_403
     "P1D"^^xsd:dayTimeDuration
     58
     0.9566813248214596
     0.9400709364382943
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_404.}
     {?patient care:state care:state_9.}
     action:take_pill_Medication_404
     "P1D"^^xsd:dayTimeDuration
     42
     0.3846864246663736
     0.6278738443807893
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_405.}
     {?patient care:state care:state_16.}
     action:take_pill_Medication_405
     "P1D"^^xsd:dayTimeDuration
     50
     0.6372295908319868
     0.6003818547400399
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_406.}
     {?patient care:state care:state_17.}
     action:take_pill_Medication_406
     "P1D"^^xsd:dayTimeDuration
     68
     0.5236806970436446
     0.9619484874769682
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_407.}
     {?patient care:state care:state_17.}
     action:take_pill_Medication_407
     "P1D"^^xsd:dayTimeDuration
     74
     0.7861352856405556
     0.6627921680851029
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_408.}
     {?patient care:state care:state_10.}
     action:take_pill_Medication_408
     "P1D"^^xsd:dayTimeDuration
     74
     0.8747196018094541
     0.6782326638068469
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_409.}
     {?patient care:state care:state_17.}
     action:take_pill_Medication_409
     "P1D"^^xsd:dayTimeDuration
     60
     0.673161700326331
     0.8095453121320798
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_410.}
     {?patient care:state care:state_19.}
     action:take_pill_Medication_410
     "P1D"^^xsd:dayTimeDuration
     22
     0.65849761255297
     0.8603265862241767
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_411.}
     {?patient care:state care:state_18.}
     action:take_pill_Medication_411
     "P1D"^^xsd:dayTimeDuration
     70
     0.7064786336226955
     0.9729288782369014
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_412.}
     {?patient care:state care:state_18.}
     action:take_pill_Medication_412
     "P1D"^^xsd:dayTimeDuration
     52
     0.9161594735235346
     0.3990495171692212
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_413.}
     {?patient care:state care:state_4.}
     action:take_pill_Medication_413
     "P1D"^^xsd:dayTimeDuration
     71
     0.7294996663462586
     0.34746695598720256
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_414.}
     {?patient care:state care:state_15.}
     action:take_pill_Medication_414
     "P1D"^^xsd:dayTimeDuration
     97
     0.5908946228682712
     0.8757356442194857
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_415.}
     {?patient care:state care:state_5.}
     action:take_pill_Medication_415
     "P1D"^^xsd:dayTimeDuration
     1
     0.6252992972670075
     0.9399701900630206
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_416.}
     {?patient care:state care:state_7.}
     action:take_pill_Medication_416
     "P1D"^^xsd:dayTimeDuration
     64
     0.6108151164633672
     0.8459631864164122
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_417.}
     {?patient care:state care:state_15.}
     action:take_pill_Medication_417
     "P1D"^^xsd:dayTimeDuration
     38
     0.4333659869541737
     0.3938706121007306
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_418.}
     {?patient care:state care:state_3.}
     action:take_pill_Medication_418
     "P1D"^^xsd:dayTimeDuration
     73
     0.719994744346317
     0.8438757937361573
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_419.}
     {?patient care:state care:state_12.}
     action:take_pill_Medication_419
     "P1D"^^xsd:dayTimeDuration
     47
     0.7573259008704482
     0.7604086025769053
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_420.}
     {?patient care:state care:state_4.}
     action:take_pill_Medication_420
     "P1D"^^xsd:dayTimeDuration
     19
     0.8348105939598037
     0.683676793830108
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_421.}
     {?patient care:state care:state_9.}
     action:take_pill_Medication_421
     "P1D"^^xsd:dayTimeDuration
     50
     0.4150651303428732
     0.4340621872202345
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_422.}
     {?patient care:state care:state_19.}
     action:take_pill_Medication_422
     "P1D"^^xsd:dayTimeDuration
     25
     0.4033212273942878
     0.6158938948457893
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_423.}
     {?patient care:state care:state_12.}
     action:take_pill_Medication_423
     "P1D"^^xsd:dayTimeDuration
     32
     0.4890668841037096
     0.2799016153223879
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_424.}
     {?patient care:state care:state_5.}
     action:take_pill_Medication_424
     "P1D"^^xsd:dayTimeDuration
     50
     0.8210038228304852
     0.6194566386939555
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_425.}
     {?patient care:state care:state_8.}
     action:take_pill_Medication_425
     "P1D"^^xsd:dayTimeDuration
     6
     0.7913746393861625
     0.5487617156334852
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_426.}
     {?patient care:state care:state_8.}
     action:take_pill_Medication_426
     "P1D"^^xsd:dayTimeDuration
     15
     0.7154845950892387
     0.28174756351517627
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_427.}
     {?patient care:state care:state_18.}
     action:take_pill_Medication_427
     "P1D"^^xsd:dayTimeDuration
     90
     0.4045862951485465
     0.4289735783001195
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_428.}
     {?patient care:state care:state_18.}
     action:take_pill_Medication_428
     "P1D"^^xsd:dayTimeDuration
     7
     0.668747271403633
     0.2890899171771975
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_429.}
     {?patient care:state care:state_8.}
     action:take_pill_Medication_429
     "P1D"^^xsd:dayTimeDuration
     48
     0.32298801420046586
     0.878012241902879
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_430.}
     {?patient care:state care:state_17.}
     action:take_pill_Medication_430
     "P1D"^^xsd:dayTimeDuration
     87
     0.47997236755480144
     0.27992625345142
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_431.}
     {?patient care:state care:state_14.}
     action:take_pill_Medication_431
     "P1D"^^xsd:dayTimeDuration
     39
     0.351907986015764
     0.2842778396605973
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_432.}
     {?patient care:state care:state_11.}
     action:take_pill_Medication_432
     "P1D"^^xsd:dayTimeDuration
     98
     0.6138011994792748
     0.6051774691159213
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_433.}
     {?patient care:state care:state_4.}
     action:take_pill_Medication_433
     "P1D"^^xsd:dayTimeDuration
     66
     0.3362024919992643
     0.4151515914562634
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_434.}
     {?patient care:state care:state_20.}
     action:take_pill_Medication_434
     "P1D"^^xsd:dayTimeDuration
     42
     0.8495774841684961
     0.9456578814379468
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_435.}
     {?patient care:state care:state_7.}
     action:take_pill_Medication_435
     "P1D"^^xsd:dayTimeDuration
     3
     0.8061352527447615
     0.9360305222862376
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_436.}
     {?patient care:state care:state_6.}
     action:take_pill_Medication_436
     "P1D"^^xsd:dayTimeDuration
     70
     0.9302920229045291
     0.9919626077170434
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_437.}
     {?patient care:state care:state_12.}
     action:take_pill_Medication_437
     "P1D"^^xsd:dayTimeDuration
     90
     0.5361608096737257
     0.2579243618513155
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_438.}
     {?patient care:state care:state_11.}
     action:take_pill_Medication_438
     "P1D"^^xsd:dayTimeDuration
     32
     0.6942336562157245
     0.8655752811518439
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_439.}
     {?patient care:state care:state_12.}
     action:take_pill_Medication_439
     "P1D"^^xsd:dayTimeDuration
     55
     0.7580632565385388
     0.5472604571674551
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_440.}
     {?patient care:state care:state_12.}
     action:take_pill_Medication_440
     "P1D"^^xsd:dayTimeDuration
     98
     0.2888563991741173
     0.3605074453555802
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_441.}
     {?patient care:state care:state_18.}
     action:take_pill_Medication_441
     "P1D"^^xsd:dayTimeDuration
     78
     0.7896169768139663
     0.45793713559014826
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_442.}
     {?patient care:state care:state_19.}
     action:take_pill_Medication_442
     "P1D"^^xsd:dayTimeDuration
     75
     0.8251736271528913
     0.2506483605783638
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_443.}
     {?patient care:state care:state_12.}
     action:take_pill_Medication_443
     "P1D"^^xsd:dayTimeDuration
     8
     0.5739424460186738
     0.8353921185950348
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_444.}
     {?patient care:state care:state_8.}
     action:take_pill_Medication_444
     "P1D"^^xsd:dayTimeDuration
     91
     0.2982960040933481
     0.3558429368262377
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_445.}
     {?patient care:state care:state_7.}
     action:take_pill_Medication_445
     "P1D"^^xsd:dayTimeDuration
     29
     0.2984778563472127
     0.8763276303671956
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_446.}
     {?patient care:state care:state_3.}
     action:take_pill_Medication_446
     "P1D"^^xsd:dayTimeDuration
     47
     0.7671575363719713
     0.35988104166864865
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_447.}
     {?patient care:state care:state_16.}
     action:take_pill_Medication_447
     "P1D"^^xsd:dayTimeDuration
     93
     0.8994745528283401
     0.752692194308084
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_448.}
     {?patient care:state care:state_19.}
     action:take_pill_Medication_448
     "P1D"^^xsd:dayTimeDuration
     20
     0.4105811281332841
     0.4901045458324625
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_449.}
     {?patient care:state care:state_7.}
     action:take_pill_Medication_449
     "P1D"^^xsd:dayTimeDuration
     14
     0.9725068820608315
     0.5401465219740735
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_450.}
     {?patient care:state care:state_3.}
     action:take_pill_Medication_450
     "P1D"^^xsd:dayTimeDuration
     83
     0.7363162961817308
     0.6050311450224068
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_451.}
     {?patient care:state care:state_18.}
     action:take_pill_Medication_451
     "P1D"^^xsd:dayTimeDuration
     15
     0.4628355915884282
     0.2730424217964711
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_452.}
     {?patient care:state care:state_11.}
     action:take_pill_Medication_452
     "P1D"^^xsd:dayTimeDuration
     54
     0.6159126026507611
     0.5836654378416722
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_453.}
     {?patient care:state care:state_19.}
     action:take_pill_Medication_453
     "P1D"^^xsd:dayTimeDuration
     26
     0.5247082376857578
     0.6986878497451764
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_454.}
     {?patient care:state care:state_6.}
     action:take_pill_Medication_454
     "P1D"^^xsd:dayTimeDuration
     93
     0.5007220247936226
     0.4510677910033768
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_455.}
     {?patient care:state care:state_10.}
     action:take_pill_Medication_455
     "P1D"^^xsd:dayTimeDuration
     100
     0.9962660298111404
     0.8159994489385574
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_456.}
     {?patient care:state care:state_4.}
     action:take_pill_Medication_456
     "P1D"^^xsd:dayTimeDuration
     64
     0.44666367131403206
     0.7428738483429134
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_457.}
     {?patient care:state care:state_15.}
     action:take_pill_Medication_457
     "P1D"^^xsd:dayTimeDuration
     5
     0.8454073083435754
     0.34728217737443867
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_458.}
     {?patient care:state care:state_18.}
     action:take_pill_Medication_458
     "P1D"^^xsd:dayTimeDuration
     89
     0.9371764707976238
     0.7129188229908426
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_459.}
     {?patient care:state care:state_10.}
     action:take_pill_Medication_459
     "P1D"^^xsd:dayTimeDuration
     18
     0.37093338499740536
     0.9890292370605549
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_460.}
     {?patient care:state care:state_17.}
     action:take_pill_Medication_460
     "P1D"^^xsd:dayTimeDuration
     13
     0.6535333319561517
     0.3177856236642297
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_461.}
     {?patient care:state care:state_12.}
     action:take_pill_Medication_461
     "P1D"^^xsd:dayTimeDuration
     37
     0.43497632956909515
     0.9817208374021827
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_462.}
     {?patient care:state care:state_14.}
     action:take_pill_Medication_462
     "P1D"^^xsd:dayTimeDuration
     23
     0.8552891849723534
     0.5318438425165148
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_463.}
     {?patient care:state care:state_5.}
     action:take_pill_Medication_463
     "P1D"^^xsd:dayTimeDuration
     63
     0.9048626916835631
     0.8494970065242127
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_464.}
     {?patient care:state care:state_15.}
     action:take_pill_Medication_464
     "P1D"^^xsd:dayTimeDuration
     12
     0.7392590452387223
     0.9482726420403501
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_465.}
     {?patient care:state care:state_20.}
     action:take_pill_Medication_465
     "P1D"^^xsd:dayTimeDuration
     65
     0.42473910786909497
     0.32517477741509004
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_466.}
     {?patient care:state care:state_3.}
     action:take_pill_Medication_466
     "P1D"^^xsd:dayTimeDuration
     49
     0.8677168500045931
     0.5693898874836257
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_467.}
     {?patient care:state care:state_18.}
     action:take_pill_Medication_467
     "P1D"^^xsd:dayTimeDuration
     69
     0.8709830679178825
     0.9079517238803868
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_468.}
     {?patient care:state care:state_14.}
     action:take_pill_Medication_468
     "P1D"^^xsd:dayTimeDuration
     51
     0.41822920900558413
     0.8523210148421609
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_469.}
     {?patient care:state care:state_15.}
     action:take_pill_Medication_469
     "P1D"^^xsd:dayTimeDuration
     14
     0.7382095211381678
     0.6662054800395212
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_470.}
     {?patient care:state care:state_11.}
     action:take_pill_Medication_470
     "P1D"^^xsd:dayTimeDuration
     49
     0.6184417857200903
     0.6376833457284117
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_471.}
     {?patient care:state care:state_16.}
     action:take_pill_Medication_471
     "P1D"^^xsd:dayTimeDuration
     84
     0.8404828412252436
     0.5995310460271395
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_472.}
     {?patient care:state care:state_20.}
     action:take_pill_Medication_472
     "P1D"^^xsd:dayTimeDuration
     69
     0.4796882734253026
     0.3182362674005534
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_473.}
     {?patient care:state care:state_20.}
     action:take_pill_Medication_473
     "P1D"^^xsd:dayTimeDuration
     63
     0.9711559865443129
     0.33590914477600253
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_474.}
     {?patient care:state care:state_13.}
     action:take_pill_Medication_474
     "P1D"^^xsd:dayTimeDuration
     31
     0.44641893493400225
     0.8916061573661286
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_475.}
     {?patient care:state care:state_4.}
     action:take_pill_Medication_475
     "P1D"^^xsd:dayTimeDuration
     90
     0.9227770930680144
     0.6929790312068949
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_476.}
     {?patient care:state care:state_19.}
     action:take_pill_Medication_476
     "P1D"^^xsd:dayTimeDuration
     57
     0.8163154328442046
     0.8347843407257122
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_477.}
     {?patient care:state care:state_13.}
     action:take_pill_Medication_477
     "P1D"^^xsd:dayTimeDuration
     44
     0.32131909674325804
     0.6171350988801038
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_478.}
     {?patient care:state care:state_18.}
     action:take_pill_Medication_478
     "P1D"^^xsd:dayTimeDuration
     14
     0.5811031625738801
     0.490379834950314
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_479.}
     {?patient care:state care:state_12.}
     action:take_pill_Medication_479
     "P1D"^^xsd:dayTimeDuration
     12
     0.8979333310329191
     0.9002838712026354
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_480.}
     {?patient care:state care:state_5.}
     action:take_pill_Medication_480
     "P1D"^^xsd:dayTimeDuration
     92
     0.4223697634377322
     0.4027399809896335
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_481.}
     {?patient care:state care:state_17.}
     action:take_pill_Medication_481
     "P1D"^^xsd:dayTimeDuration
     14
     0.8999547410412744
     0.27163981633482165
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_482.}
     {?patient care:state care:state_3.}
     action:take_pill_Medication_482
     "P1D"^^xsd:dayTimeDuration
     7
     0.7864309081352173
     0.8133939969432759
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_483.}
     {?patient care:state care:state_20.}
     action:take_pill_Medication_483
     "P1D"^^xsd:dayTimeDuration
     95
     0.9279788394330273
     0.5092352377436486
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_484.}
     {?patient care:state care:state_9.}
     action:take_pill_Medication_484
     "P1D"^^xsd:dayTimeDuration
     69
     0.6954287510594412
     0.530314784921601
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_485.}
     {?patient care:state care:state_9.}
     action:take_pill_Medication_485
     "P1D"^^xsd:dayTimeDuration
     63
     0.9805446218020321
     0.3098532022548985
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_486.}
     {?patient care:state care:state_10.}
     action:take_pill_Medication_486
     "P1D"^^xsd:dayTimeDuration
     3
     0.39388780012103897
     0.6418351708965159
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_487.}
     {?patient care:state care:state_16.}
     action:take_pill_Medication_487
     "P1D"^^xsd:dayTimeDuration
     44
     0.33298591746647355
     0.3790788548509462
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_488.}
     {?patient care:state care:state_15.}
     action:take_pill_Medication_488
     "P1D"^^xsd:dayTimeDuration
     36
     0.48603829954182515
     0.5642913890597305
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_489.}
     {?patient care:state care:state_19.}
     action:take_pill_Medication_489
     "P1D"^^xsd:dayTimeDuration
     86
     0.9924973458418755
     0.6917561254720083
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_490.}
     {?patient care:state care:state_16.}
     action:take_pill_Medication_490
     "P1D"^^xsd:dayTimeDuration
     84
     0.8424886097623505
     0.81556247055803
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_491.}
     {?patient care:state care:state_19.}
     action:take_pill_Medication_491
     "P1D"^^xsd:dayTimeDuration
     6
     0.284347054935024
     0.536441482099051
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_492.}
     {?patient care:state care:state_14.}
     action:take_pill_Medication_492
     "P1D"^^xsd:dayTimeDuration
     19
     0.750700312860225
     0.7032352138315002
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_493.}
     {?patient care:state care:state_10.}
     action:take_pill_Medication_493
     "P1D"^^xsd:dayTimeDuration
     73
     0.7469969175140009
     0.8654414986858344
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_494.}
     {?patient care:state care:state_15.}
     action:take_pill_Medication_494
     "P1D"^^xsd:dayTimeDuration
     42
     0.9922615602169387
     0.9734818169757679
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_495.}
     {?patient care:state care:state_7.}
     action:take_pill_Medication_495
     "P1D"^^xsd:dayTimeDuration
     62
     0.7878239529216932
     0.7283120351721176
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_496.}
     {?patient care:state care:state_9.}
     action:take_pill_Medication_496
     "P1D"^^xsd:dayTimeDuration
     19
     0.3935734884184128
     0.4330261503043493
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_497.}
     {?patient care:state care:state_3.}
     action:take_pill_Medication_497
     "P1D"^^xsd:dayTimeDuration
     5
     0.3212838049630279
     0.27421075261702144
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_498.}
     {?patient care:state care:state_16.}
     action:take_pill_Medication_498
     "P1D"^^xsd:dayTimeDuration
     89
     0.7194608280337574
     0.7380975361640393
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_499.}
     {?patient care:state care:state_12.}
     action:take_pill_Medication_499
     "P1D"^^xsd:dayTimeDuration
     42
     0.3253964555598061
     0.5241150658587752
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_500.}
     {?patient care:state care:state_17.}
     action:take_pill_Medication_500
     "P1D"^^xsd:dayTimeDuration
     20
     0.5654651507501148
     0.7324315612834357
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_501.}
     {?patient care:state care:state_15.}
     action:take_pill_Medication_501
     "P1D"^^xsd:dayTimeDuration
     39
     0.41975715419384385
     0.6917497875224043
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_502.}
     {?patient care:state care:state_4.}
     action:take_pill_Medication_502
     "P1D"^^xsd:dayTimeDuration
     92
     0.5578518363319038
     0.46352724097762044
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_503.}
     {?patient care:state care:state_19.}
     action:take_pill_Medication_503
     "P1D"^^xsd:dayTimeDuration
     5
     0.788934037677416
     0.4648090085181934
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_504.}
     {?patient care:state care:state_10.}
     action:take_pill_Medication_504
     "P1D"^^xsd:dayTimeDuration
     15
     0.8977017910074994
     0.25466087900068024
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_505.}
     {?patient care:state care:state_14.}
     action:take_pill_Medication_505
     "P1D"^^xsd:dayTimeDuration
     37
     0.8808248921371746
     0.7528315140022719
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_506.}
     {?patient care:state care:state_6.}
     action:take_pill_Medication_506
     "P1D"^^xsd:dayTimeDuration
     88
     0.4002221247250469
     0.7328984541947402
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_507.}
     {?patient care:state care:state_16.}
     action:take_pill_Medication_507
     "P1D"^^xsd:dayTimeDuration
     81
     0.5678586983321887
     0.947173898656576
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_508.}
     {?patient care:state care:state_4.}
     action:take_pill_Medication_508
     "P1D"^^xsd:dayTimeDuration
     19
     0.7841564685014468
     0.7082029861968122
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_509.}
     {?patient care:state care:state_12.}
     action:take_pill_Medication_509
     "P1D"^^xsd:dayTimeDuration
     97
     0.9047214449284198
     0.4228917153836731
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_510.}
     {?patient care:state care:state_10.}
     action:take_pill_Medication_510
     "P1D"^^xsd:dayTimeDuration
     76
     0.5230953217442363
     0.8748259602474547
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_511.}
     {?patient care:state care:state_18.}
     action:take_pill_Medication_511
     "P1D"^^xsd:dayTimeDuration
     43
     0.7427813284679127
     0.6683782644467834
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_512.}
     {?patient care:state care:state_3.}
     action:take_pill_Medication_512
     "P1D"^^xsd:dayTimeDuration
     32
     0.8640074373121718
     0.6028303244621697
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_513.}
     {?patient care:state care:state_12.}
     action:take_pill_Medication_513
     "P1D"^^xsd:dayTimeDuration
     58
     0.3788206087088957
     0.4810090287207993
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_514.}
     {?patient care:state care:state_18.}
     action:take_pill_Medication_514
     "P1D"^^xsd:dayTimeDuration
     80
     0.4837800001352366
     0.7303771795683327
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_515.}
     {?patient care:state care:state_11.}
     action:take_pill_Medication_515
     "P1D"^^xsd:dayTimeDuration
     87
     0.5257136377376561
     0.34795896955107497
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_516.}
     {?patient care:state care:state_14.}
     action:take_pill_Medication_516
     "P1D"^^xsd:dayTimeDuration
     10
     0.9217723566503422
     0.325049615312955
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_517.}
     {?patient care:state care:state_11.}
     action:take_pill_Medication_517
     "P1D"^^xsd:dayTimeDuration
     74
     0.9607909236846288
     0.7383204731886656
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_518.}
     {?patient care:state care:state_13.}
     action:take_pill_Medication_518
     "P1D"^^xsd:dayTimeDuration
     71
     0.3599901048891287
     0.36644986521485795
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_519.}
     {?patient care:state care:state_14.}
     action:take_pill_Medication_519
     "P1D"^^xsd:dayTimeDuration
     53
     0.6510377047462133
     0.3962656094072352
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_520.}
     {?patient care:state care:state_11.}
     action:take_pill_Medication_520
     "P1D"^^xsd:dayTimeDuration
     71
     0.9955556958795572
     0.813062543852231
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_521.}
     {?patient care:state care:state_15.}
     action:take_pill_Medication_521
     "P1D"^^xsd:dayTimeDuration
     4
     0.36972650532728724
     0.4776240833230827
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_522.}
     {?patient care:state care:state_19.}
     action:take_pill_Medication_522
     "P1D"^^xsd:dayTimeDuration
     54
     0.7159316608135552
     0.4612792876903886
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_523.}
     {?patient care:state care:state_13.}
     action:take_pill_Medication_523
     "P1D"^^xsd:dayTimeDuration
     63
     0.7907405011420067
     0.41988730721897294
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_524.}
     {?patient care:state care:state_8.}
     action:take_pill_Medication_524
     "P1D"^^xsd:dayTimeDuration
     63
     0.5941472215804364
     0.31964729530452063
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_525.}
     {?patient care:state care:state_6.}
     action:take_pill_Medication_525
     "P1D"^^xsd:dayTimeDuration
     44
     0.577362299823011
     0.6401456999306858
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_526.}
     {?patient care:state care:state_11.}
     action:take_pill_Medication_526
     "P1D"^^xsd:dayTimeDuration
     13
     0.8415406922188076
     0.5494747832371804
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_527.}
     {?patient care:state care:state_7.}
     action:take_pill_Medication_527
     "P1D"^^xsd:dayTimeDuration
     54
     0.6947837400607013
     0.5883565983708674
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_528.}
     {?patient care:state care:state_6.}
     action:take_pill_Medication_528
     "P1D"^^xsd:dayTimeDuration
     25
     0.7738077970017081
     0.9957255630146239
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_529.}
     {?patient care:state care:state_4.}
     action:take_pill_Medication_529
     "P1D"^^xsd:dayTimeDuration
     31
     0.6159222159688613
     0.4360119856104894
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_530.}
     {?patient care:state care:state_9.}
     action:take_pill_Medication_530
     "P1D"^^xsd:dayTimeDuration
     93
     0.3081368525593182
     0.9962475188702722
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_531.}
     {?patient care:state care:state_16.}
     action:take_pill_Medication_531
     "P1D"^^xsd:dayTimeDuration
     97
     0.6007890017742936
     0.4167914637315868
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_532.}
     {?patient care:state care:state_11.}
     action:take_pill_Medication_532
     "P1D"^^xsd:dayTimeDuration
     14
     0.7188895552623327
     0.9984797312222888
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_533.}
     {?patient care:state care:state_4.}
     action:take_pill_Medication_533
     "P1D"^^xsd:dayTimeDuration
     54
     0.3660338562546712
     0.8524974817824246
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_534.}
     {?patient care:state care:state_5.}
     action:take_pill_Medication_534
     "P1D"^^xsd:dayTimeDuration
     10
     0.913411073938389
     0.31461713595843427
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_535.}
     {?patient care:state care:state_14.}
     action:take_pill_Medication_535
     "P1D"^^xsd:dayTimeDuration
     2
     0.7083687167446154
     0.7288867479018312
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_536.}
     {?patient care:state care:state_4.}
     action:take_pill_Medication_536
     "P1D"^^xsd:dayTimeDuration
     8
     0.8418400499034732
     0.7977420108432559
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_537.}
     {?patient care:state care:state_6.}
     action:take_pill_Medication_537
     "P1D"^^xsd:dayTimeDuration
     93
     0.9665491341957009
     0.5356934636560274
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_538.}
     {?patient care:state care:state_14.}
     action:take_pill_Medication_538
     "P1D"^^xsd:dayTimeDuration
     47
     0.29417460337631685
     0.5697761326486452
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_539.}
     {?patient care:state care:state_4.}
     action:take_pill_Medication_539
     "P1D"^^xsd:dayTimeDuration
     42
     0.9055148721122506
     0.6392883571090562
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_540.}
     {?patient care:state care:state_18.}
     action:take_pill_Medication_540
     "P1D"^^xsd:dayTimeDuration
     48
     0.762805306694663
     0.3231649375810087
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_541.}
     {?patient care:state care:state_9.}
     action:take_pill_Medication_541
     "P1D"^^xsd:dayTimeDuration
     15
     0.8965063579214718
     0.7723087459505541
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_542.}
     {?patient care:state care:state_14.}
     action:take_pill_Medication_542
     "P1D"^^xsd:dayTimeDuration
     90
     0.9652323474538019
     0.6725924909636878
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_543.}
     {?patient care:state care:state_2.}
     action:take_pill_Medication_543
     "P1D"^^xsd:dayTimeDuration
     16
     0.9207654834022874
     0.26443438016622944
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_544.}
     {?patient care:state care:state_11.}
     action:take_pill_Medication_544
     "P1D"^^xsd:dayTimeDuration
     26
     0.6957801230201943
     0.8703292995088037
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_545.}
     {?patient care:state care:state_15.}
     action:take_pill_Medication_545
     "P1D"^^xsd:dayTimeDuration
     78
     0.9456015001189183
     0.5120175127933686
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_546.}
     {?patient care:state care:state_15.}
     action:take_pill_Medication_546
     "P1D"^^xsd:dayTimeDuration
     15
     0.27790509835342325
     0.6954891740377436
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_547.}
     {?patient care:state care:state_14.}
     action:take_pill_Medication_547
     "P1D"^^xsd:dayTimeDuration
     29
     0.7508307616699785
     0.8354596198252703
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_548.}
     {?patient care:state care:state_16.}
     action:take_pill_Medication_548
     "P1D"^^xsd:dayTimeDuration
     98
     0.9717792067555313
     0.8863784778425169
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_549.}
     {?patient care:state care:state_12.}
     action:take_pill_Medication_549
     "P1D"^^xsd:dayTimeDuration
     46
     0.8952852119663384
     0.6492688162423588
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_550.}
     {?patient care:state care:state_14.}
     action:take_pill_Medication_550
     "P1D"^^xsd:dayTimeDuration
     55
     0.7737254441279868
     0.5547495473792312
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_551.}
     {?patient care:state care:state_17.}
     action:take_pill_Medication_551
     "P1D"^^xsd:dayTimeDuration
     46
     0.5949899335776268
     0.8321359969070552
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_552.}
     {?patient care:state care:state_18.}
     action:take_pill_Medication_552
     "P1D"^^xsd:dayTimeDuration
     49
     0.7551572773069117
     0.9737628608401352
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_553.}
     {?patient care:state care:state_13.}
     action:take_pill_Medication_553
     "P1D"^^xsd:dayTimeDuration
     54
     0.8035090815669073
     0.75792256098453
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_554.}
     {?patient care:state care:state_3.}
     action:take_pill_Medication_554
     "P1D"^^xsd:dayTimeDuration
     60
     0.8408752495254821
     0.9546473178352075
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_555.}
     {?patient care:state care:state_5.}
     action:take_pill_Medication_555
     "P1D"^^xsd:dayTimeDuration
     51
     0.7189562873438726
     0.44634182222075647
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_556.}
     {?patient care:state care:state_17.}
     action:take_pill_Medication_556
     "P1D"^^xsd:dayTimeDuration
     46
     0.32716906980950833
     0.6327403444598614
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_557.}
     {?patient care:state care:state_12.}
     action:take_pill_Medication_557
     "P1D"^^xsd:dayTimeDuration
     71
     0.2506864796291153
     0.2651283082500515
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_558.}
     {?patient care:state care:state_7.}
     action:take_pill_Medication_558
     "P1D"^^xsd:dayTimeDuration
     7
     0.9081605911925943
     0.6758576024598713
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_559.}
     {?patient care:state care:state_14.}
     action:take_pill_Medication_559
     "P1D"^^xsd:dayTimeDuration
     27
     0.9906246999113222
     0.6102686197637869
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_560.}
     {?patient care:state care:state_2.}
     action:take_pill_Medication_560
     "P1D"^^xsd:dayTimeDuration
     96
     0.39550316548598596
     0.7305947996912342
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_561.}
     {?patient care:state care:state_17.}
     action:take_pill_Medication_561
     "P1D"^^xsd:dayTimeDuration
     50
     0.717665741699458
     0.7417210658605562
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_562.}
     {?patient care:state care:state_14.}
     action:take_pill_Medication_562
     "P1D"^^xsd:dayTimeDuration
     67
     0.8040850078669439
     0.7056750015108146
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_563.}
     {?patient care:state care:state_16.}
     action:take_pill_Medication_563
     "P1D"^^xsd:dayTimeDuration
     48
     0.6853685614031068
     0.5118736601673161
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_564.}
     {?patient care:state care:state_10.}
     action:take_pill_Medication_564
     "P1D"^^xsd:dayTimeDuration
     95
     0.8191773737926572
     0.693887314256076
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_565.}
     {?patient care:state care:state_9.}
     action:take_pill_Medication_565
     "P1D"^^xsd:dayTimeDuration
     77
     0.9145296832179229
     0.6705966020761379
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_566.}
     {?patient care:state care:state_5.}
     action:take_pill_Medication_566
     "P1D"^^xsd:dayTimeDuration
     8
     0.40589128636313254
     0.6313118871511973
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_567.}
     {?patient care:state care:state_7.}
     action:take_pill_Medication_567
     "P1D"^^xsd:dayTimeDuration
     8
     0.9844660845502082
     0.8286584163331665
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_568.}
     {?patient care:state care:state_11.}
     action:take_pill_Medication_568
     "P1D"^^xsd:dayTimeDuration
     49
     0.44938576784380196
     0.28249875518791284
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_569.}
     {?patient care:state care:state_8.}
     action:take_pill_Medication_569
     "P1D"^^xsd:dayTimeDuration
     21
     0.9407125541170193
     0.6198314608164925
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_570.}
     {?patient care:state care:state_13.}
     action:take_pill_Medication_570
     "P1D"^^xsd:dayTimeDuration
     15
     0.37086756001936805
     0.7978194048634254
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_571.}
     {?patient care:state care:state_8.}
     action:take_pill_Medication_571
     "P1D"^^xsd:dayTimeDuration
     97
     0.5415270736713017
     0.26593573123951364
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_572.}
     {?patient care:state care:state_12.}
     action:take_pill_Medication_572
     "P1D"^^xsd:dayTimeDuration
     100
     0.8746086260524639
     0.7895686023642381
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_573.}
     {?patient care:state care:state_9.}
     action:take_pill_Medication_573
     "P1D"^^xsd:dayTimeDuration
     57
     0.6720407534254298
     0.2569000713362223
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_574.}
     {?patient care:state care:state_13.}
     action:take_pill_Medication_574
     "P1D"^^xsd:dayTimeDuration
     1
     0.3260860878474941
     0.5025765414997416
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_575.}
     {?patient care:state care:state_5.}
     action:take_pill_Medication_575
     "P1D"^^xsd:dayTimeDuration
     82
     0.4352623765841573
     0.36651307962344815
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_576.}
     {?patient care:state care:state_10.}
     action:take_pill_Medication_576
     "P1D"^^xsd:dayTimeDuration
     87
     0.6376280603110297
     0.5539675234746223
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_577.}
     {?patient care:state care:state_20.}
     action:take_pill_Medication_577
     "P1D"^^xsd:dayTimeDuration
     13
     0.9119288942055491
     0.6321013159678219
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_578.}
     {?patient care:state care:state_19.}
     action:take_pill_Medication_578
     "P1D"^^xsd:dayTimeDuration
     30
     0.5766136877221126
     0.43885060561266603
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_579.}
     {?patient care:state care:state_14.}
     action:take_pill_Medication_579
     "P1D"^^xsd:dayTimeDuration
     69
     0.7440859664943715
     0.9138094783527378
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_580.}
     {?patient care:state care:state_4.}
     action:take_pill_Medication_580
     "P1D"^^xsd:dayTimeDuration
     98
     0.26580307289652466
     0.9886508552267441
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_581.}
     {?patient care:state care:state_16.}
     action:take_pill_Medication_581
     "P1D"^^xsd:dayTimeDuration
     13
     0.29650130263011243
     0.4417785386177557
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_582.}
     {?patient care:state care:state_15.}
     action:take_pill_Medication_582
     "P1D"^^xsd:dayTimeDuration
     77
     0.5539343330816103
     0.9522561922417174
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_583.}
     {?patient care:state care:state_18.}
     action:take_pill_Medication_583
     "P1D"^^xsd:dayTimeDuration
     5
     0.6684319375331695
     0.3626502272466476
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_584.}
     {?patient care:state care:state_6.}
     action:take_pill_Medication_584
     "P1D"^^xsd:dayTimeDuration
     80
     0.8825360493766575
     0.42955395753137393
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_585.}
     {?patient care:state care:state_6.}
     action:take_pill_Medication_585
     "P1D"^^xsd:dayTimeDuration
     78
     0.28243778922404894
     0.9322014341554097
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_586.}
     {?patient care:state care:state_10.}
     action:take_pill_Medication_586
     "P1D"^^xsd:dayTimeDuration
     90
     0.3886475480514302
     0.2817123061911221
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_587.}
     {?patient care:state care:state_19.}
     action:take_pill_Medication_587
     "P1D"^^xsd:dayTimeDuration
     90
     0.8330795921684577
     0.5837085043213794
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_588.}
     {?patient care:state care:state_8.}
     action:take_pill_Medication_588
     "P1D"^^xsd:dayTimeDuration
     69
     0.8440308324214102
     0.4327829678138443
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_589.}
     {?patient care:state care:state_19.}
     action:take_pill_Medication_589
     "P1D"^^xsd:dayTimeDuration
     87
     0.672107833789876
     0.36918965691094763
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_590.}
     {?patient care:state care:state_13.}
     action:take_pill_Medication_590
     "P1D"^^xsd:dayTimeDuration
     40
     0.8569653161519635
     0.9423593227895142
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_591.}
     {?patient care:state care:state_17.}
     action:take_pill_Medication_591
     "P1D"^^xsd:dayTimeDuration
     87
     0.2811341918511673
     0.25420385599702366
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_592.}
     {?patient care:state care:state_12.}
     action:take_pill_Medication_592
     "P1D"^^xsd:dayTimeDuration
     91
     0.9241916604846613
     0.2656094450506259
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_593.}
     {?patient care:state care:state_3.}
     action:take_pill_Medication_593
     "P1D"^^xsd:dayTimeDuration
     94
     0.6974766052618668
     0.6325216652622735
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_594.}
     {?patient care:state care:state_16.}
     action:take_pill_Medication_594
     "P1D"^^xsd:dayTimeDuration
     62
     0.9570126047441759
     0.3660720736983667
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_595.}
     {?patient care:state care:state_20.}
     action:take_pill_Medication_595
     "P1D"^^xsd:dayTimeDuration
     11
     0.9756661340219916
     0.4346094618225874
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_596.}
     {?patient care:state care:state_20.}
     action:take_pill_Medication_596
     "P1D"^^xsd:dayTimeDuration
     26
     0.5446643310330656
     0.9805023212801087
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_597.}
     {?patient care:state care:state_18.}
     action:take_pill_Medication_597
     "P1D"^^xsd:dayTimeDuration
     11
     0.8255985621920263
     0.6045501770192424
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_598.}
     {?patient care:state care:state_11.}
     action:take_pill_Medication_598
     "P1D"^^xsd:dayTimeDuration
     6
     0.5641989123710229
     0.339926396207557
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_599.}
     {?patient care:state care:state_7.}
     action:take_pill_Medication_599
     "P1D"^^xsd:dayTimeDuration
     49
     0.9380208331759001
     0.6826649724424643
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_600.}
     {?patient care:state care:state_19.}
     action:take_pill_Medication_600
     "P1D"^^xsd:dayTimeDuration
     60
     0.8312854620611299
     0.9891096598724659
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_601.}
     {?patient care:state care:state_9.}
     action:take_pill_Medication_601
     "P1D"^^xsd:dayTimeDuration
     78
     0.8140626132748002
     0.3080163283138908
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_602.}
     {?patient care:state care:state_11.}
     action:take_pill_Medication_602
     "P1D"^^xsd:dayTimeDuration
     79
     0.6228687265781202
     0.4200375689932193
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_603.}
     {?patient care:state care:state_9.}
     action:take_pill_Medication_603
     "P1D"^^xsd:dayTimeDuration
     5
     0.8851222937288651
     0.8089960390872052
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_604.}
     {?patient care:state care:state_20.}
     action:take_pill_Medication_604
     "P1D"^^xsd:dayTimeDuration
     67
     0.5194793049276302
     0.939842680758024
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_605.}
     {?patient care:state care:state_13.}
     action:take_pill_Medication_605
     "P1D"^^xsd:dayTimeDuration
     94
     0.8585024895916458
     0.3715016605029825
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_606.}
     {?patient care:state care:state_15.}
     action:take_pill_Medication_606
     "P1D"^^xsd:dayTimeDuration
     94
     0.4787449734238793
     0.4046467066589855
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_607.}
     {?patient care:state care:state_10.}
     action:take_pill_Medication_607
     "P1D"^^xsd:dayTimeDuration
     99
     0.8613398353949541
     0.8994076710654042
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_608.}
     {?patient care:state care:state_14.}
     action:take_pill_Medication_608
     "P1D"^^xsd:dayTimeDuration
     98
     0.6460045525723133
     0.7050555765232397
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_609.}
     {?patient care:state care:state_6.}
     action:take_pill_Medication_609
     "P1D"^^xsd:dayTimeDuration
     98
     0.5510655514623152
     0.268285919505143
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_610.}
     {?patient care:state care:state_20.}
     action:take_pill_Medication_610
     "P1D"^^xsd:dayTimeDuration
     16
     0.8717613718179877
     0.7064980177102499
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_611.}
     {?patient care:state care:state_6.}
     action:take_pill_Medication_611
     "P1D"^^xsd:dayTimeDuration
     5
     0.48785568812247604
     0.820089096436446
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_612.}
     {?patient care:state care:state_19.}
     action:take_pill_Medication_612
     "P1D"^^xsd:dayTimeDuration
     1
     0.47150118651000733
     0.8284319978716255
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_613.}
     {?patient care:state care:state_15.}
     action:take_pill_Medication_613
     "P1D"^^xsd:dayTimeDuration
     54
     0.3297715275034514
     0.7668435095723114
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_614.}
     {?patient care:state care:state_9.}
     action:take_pill_Medication_614
     "P1D"^^xsd:dayTimeDuration
     77
     0.6807193477828881
     0.5639408557654498
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_615.}
     {?patient care:state care:state_17.}
     action:take_pill_Medication_615
     "P1D"^^xsd:dayTimeDuration
     88
     0.5226223325001081
     0.7856655014571656
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_616.}
     {?patient care:state care:state_5.}
     action:take_pill_Medication_616
     "P1D"^^xsd:dayTimeDuration
     68
     0.7596757119266571
     0.7535398811343992
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_617.}
     {?patient care:state care:state_20.}
     action:take_pill_Medication_617
     "P1D"^^xsd:dayTimeDuration
     12
     0.9446313047338117
     0.8338994659417623
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_618.}
     {?patient care:state care:state_15.}
     action:take_pill_Medication_618
     "P1D"^^xsd:dayTimeDuration
     34
     0.9239887875621366
     0.7425620968488799
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_619.}
     {?patient care:state care:state_13.}
     action:take_pill_Medication_619
     "P1D"^^xsd:dayTimeDuration
     58
     0.6694451511672529
     0.33433652028257643
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_620.}
     {?patient care:state care:state_12.}
     action:take_pill_Medication_620
     "P1D"^^xsd:dayTimeDuration
     77
     0.2693800510304331
     0.760616270210978
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_621.}
     {?patient care:state care:state_4.}
     action:take_pill_Medication_621
     "P1D"^^xsd:dayTimeDuration
     76
     0.7699768908072699
     0.6786687990972612
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_622.}
     {?patient care:state care:state_10.}
     action:take_pill_Medication_622
     "P1D"^^xsd:dayTimeDuration
     2
     0.642067329080848
     0.9039596963573419
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_623.}
     {?patient care:state care:state_7.}
     action:take_pill_Medication_623
     "P1D"^^xsd:dayTimeDuration
     84
     0.7056362606927206
     0.6883093336620908
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_624.}
     {?patient care:state care:state_18.}
     action:take_pill_Medication_624
     "P1D"^^xsd:dayTimeDuration
     21
     0.8960035938952298
     0.9476672710278231
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_625.}
     {?patient care:state care:state_10.}
     action:take_pill_Medication_625
     "P1D"^^xsd:dayTimeDuration
     10
     0.8981011060443784
     0.8517160672789306
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_626.}
     {?patient care:state care:state_16.}
     action:take_pill_Medication_626
     "P1D"^^xsd:dayTimeDuration
     41
     0.2661099844527671
     0.4526455675671677
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_627.}
     {?patient care:state care:state_13.}
     action:take_pill_Medication_627
     "P1D"^^xsd:dayTimeDuration
     41
     0.43397033647818933
     0.9045194693886602
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_628.}
     {?patient care:state care:state_4.}
     action:take_pill_Medication_628
     "P1D"^^xsd:dayTimeDuration
     18
     0.3711242041864779
     0.8983792274727944
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_629.}
     {?patient care:state care:state_19.}
     action:take_pill_Medication_629
     "P1D"^^xsd:dayTimeDuration
     98
     0.7739245490941206
     0.5985407294216638
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_630.}
     {?patient care:state care:state_20.}
     action:take_pill_Medication_630
     "P1D"^^xsd:dayTimeDuration
     25
     0.5775871547909619
     0.7965672170215904
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_631.}
     {?patient care:state care:state_9.}
     action:take_pill_Medication_631
     "P1D"^^xsd:dayTimeDuration
     88
     0.8416789422690008
     0.8266215807589614
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_632.}
     {?patient care:state care:state_7.}
     action:take_pill_Medication_632
     "P1D"^^xsd:dayTimeDuration
     13
     0.28513332514085976
     0.570017176398633
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_633.}
     {?patient care:state care:state_9.}
     action:take_pill_Medication_633
     "P1D"^^xsd:dayTimeDuration
     92
     0.25576142907352906
     0.3620079734968117
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_634.}
     {?patient care:state care:state_13.}
     action:take_pill_Medication_634
     "P1D"^^xsd:dayTimeDuration
     11
     0.7242199471468432
     0.3278465797576396
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_635.}
     {?patient care:state care:state_15.}
     action:take_pill_Medication_635
     "P1D"^^xsd:dayTimeDuration
     21
     0.27880859561639704
     0.7821063531405644
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_636.}
     {?patient care:state care:state_16.}
     action:take_pill_Medication_636
     "P1D"^^xsd:dayTimeDuration
     60
     0.38545546956891236
     0.4649488915742608
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_637.}
     {?patient care:state care:state_6.}
     action:take_pill_Medication_637
     "P1D"^^xsd:dayTimeDuration
     6
     0.705114317133325
     0.9928125906016613
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_638.}
     {?patient care:state care:state_4.}
     action:take_pill_Medication_638
     "P1D"^^xsd:dayTimeDuration
     53
     0.589163613121733
     0.4496634712749121
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_639.}
     {?patient care:state care:state_4.}
     action:take_pill_Medication_639
     "P1D"^^xsd:dayTimeDuration
     2
     0.3886718671897673
     0.46235830326168
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_640.}
     {?patient care:state care:state_2.}
     action:take_pill_Medication_640
     "P1D"^^xsd:dayTimeDuration
     49
     0.549691078618158
     0.5400829668760286
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_641.}
     {?patient care:state care:state_3.}
     action:take_pill_Medication_641
     "P1D"^^xsd:dayTimeDuration
     90
     0.7264001342699599
     0.9824313455562246
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_642.}
     {?patient care:state care:state_13.}
     action:take_pill_Medication_642
     "P1D"^^xsd:dayTimeDuration
     11
     0.42836651314046115
     0.7960857570883596
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_643.}
     {?patient care:state care:state_7.}
     action:take_pill_Medication_643
     "P1D"^^xsd:dayTimeDuration
     48
     0.8220497960595643
     0.8399654545705496
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_644.}
     {?patient care:state care:state_2.}
     action:take_pill_Medication_644
     "P1D"^^xsd:dayTimeDuration
     87
     0.5315856021135765
     0.6011302910738784
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_645.}
     {?patient care:state care:state_16.}
     action:take_pill_Medication_645
     "P1D"^^xsd:dayTimeDuration
     84
     0.7528661772565257
     0.3194416956692108
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_646.}
     {?patient care:state care:state_14.}
     action:take_pill_Medication_646
     "P1D"^^xsd:dayTimeDuration
     16
     0.7418161371530334
     0.3098161167595864
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_647.}
     {?patient care:state care:state_12.}
     action:take_pill_Medication_647
     "P1D"^^xsd:dayTimeDuration
     63
     0.7485816050881042
     0.5213983600753754
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_648.}
     {?patient care:state care:state_4.}
     action:take_pill_Medication_648
     "P1D"^^xsd:dayTimeDuration
     72
     0.6787035437949047
     0.28989300785599764
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_649.}
     {?patient care:state care:state_19.}
     action:take_pill_Medication_649
     "P1D"^^xsd:dayTimeDuration
     86
     0.576931167043445
     0.6631255434105962
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_650.}
     {?patient care:state care:state_18.}
     action:take_pill_Medication_650
     "P1D"^^xsd:dayTimeDuration
     100
     0.5144896291451448
     0.5391931463562358
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_651.}
     {?patient care:state care:state_12.}
     action:take_pill_Medication_651
     "P1D"^^xsd:dayTimeDuration
     92
     0.8646622134247787
     0.9825234553057212
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_652.}
     {?patient care:state care:state_3.}
     action:take_pill_Medication_652
     "P1D"^^xsd:dayTimeDuration
     71
     0.9434573586774188
     0.5874278120691476
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_653.}
     {?patient care:state care:state_15.}
     action:take_pill_Medication_653
     "P1D"^^xsd:dayTimeDuration
     44
     0.5501594972158239
     0.27572599161948974
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_654.}
     {?patient care:state care:state_5.}
     action:take_pill_Medication_654
     "P1D"^^xsd:dayTimeDuration
     98
     0.2708525058349436
     0.9823624146506503
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_655.}
     {?patient care:state care:state_17.}
     action:take_pill_Medication_655
     "P1D"^^xsd:dayTimeDuration
     48
     0.866280495243247
     0.35758465230086767
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_656.}
     {?patient care:state care:state_12.}
     action:take_pill_Medication_656
     "P1D"^^xsd:dayTimeDuration
     38
     0.3941684814160592
     0.5033193783305949
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_657.}
     {?patient care:state care:state_15.}
     action:take_pill_Medication_657
     "P1D"^^xsd:dayTimeDuration
     82
     0.5951457894128398
     0.43764294419663125
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_658.}
     {?patient care:state care:state_12.}
     action:take_pill_Medication_658
     "P1D"^^xsd:dayTimeDuration
     54
     0.3187710837032861
     0.728915217232653
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_659.}
     {?patient care:state care:state_17.}
     action:take_pill_Medication_659
     "P1D"^^xsd:dayTimeDuration
     40
     0.8741179946548964
     0.7891353045901774
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_660.}
     {?patient care:state care:state_18.}
     action:take_pill_Medication_660
     "P1D"^^xsd:dayTimeDuration
     87
     0.49677446814118414
     0.4306929494348414
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_661.}
     {?patient care:state care:state_5.}
     action:take_pill_Medication_661
     "P1D"^^xsd:dayTimeDuration
     96
     0.676951923284872
     0.9739471240887263
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_662.}
     {?patient care:state care:state_3.}
     action:take_pill_Medication_662
     "P1D"^^xsd:dayTimeDuration
     12
     0.5110738766850673
     0.7178061955828245
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_663.}
     {?patient care:state care:state_4.}
     action:take_pill_Medication_663
     "P1D"^^xsd:dayTimeDuration
     3
     0.6767086314304623
     0.8028609542372909
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_664.}
     {?patient care:state care:state_19.}
     action:take_pill_Medication_664
     "P1D"^^xsd:dayTimeDuration
     32
     0.6948470755502651
     0.9223257254519128
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_665.}
     {?patient care:state care:state_19.}
     action:take_pill_Medication_665
     "P1D"^^xsd:dayTimeDuration
     74
     0.2859066781765237
     0.3707110479003045
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_666.}
     {?patient care:state care:state_13.}
     action:take_pill_Medication_666
     "P1D"^^xsd:dayTimeDuration
     45
     0.9040492267156006
     0.30993178906778074
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_667.}
     {?patient care:state care:state_7.}
     action:take_pill_Medication_667
     "P1D"^^xsd:dayTimeDuration
     16
     0.6506938892222676
     0.5845478661701163
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_668.}
     {?patient care:state care:state_10.}
     action:take_pill_Medication_668
     "P1D"^^xsd:dayTimeDuration
     97
     0.28248609544639713
     0.9124973960886579
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_669.}
     {?patient care:state care:state_4.}
     action:take_pill_Medication_669
     "P1D"^^xsd:dayTimeDuration
     99
     0.43163425537288924
     0.616607622580287
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_670.}
     {?patient care:state care:state_4.}
     action:take_pill_Medication_670
     "P1D"^^xsd:dayTimeDuration
     47
     0.4410807229536989
     0.3936575306519895
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_671.}
     {?patient care:state care:state_13.}
     action:take_pill_Medication_671
     "P1D"^^xsd:dayTimeDuration
     2
     0.8070319394333323
     0.44928356028209016
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_672.}
     {?patient care:state care:state_12.}
     action:take_pill_Medication_672
     "P1D"^^xsd:dayTimeDuration
     86
     0.38308105846951934
     0.5977585156701487
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_673.}
     {?patient care:state care:state_12.}
     action:take_pill_Medication_673
     "P1D"^^xsd:dayTimeDuration
     15
     0.44970872256643335
     0.39522582465845724
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_674.}
     {?patient care:state care:state_19.}
     action:take_pill_Medication_674
     "P1D"^^xsd:dayTimeDuration
     35
     0.7304008701785466
     0.44523931086149426
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_675.}
     {?patient care:state care:state_4.}
     action:take_pill_Medication_675
     "P1D"^^xsd:dayTimeDuration
     18
     0.37889585212953086
     0.919816598062186
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_676.}
     {?patient care:state care:state_15.}
     action:take_pill_Medication_676
     "P1D"^^xsd:dayTimeDuration
     40
     0.6307849434370125
     0.8640771509687744
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_677.}
     {?patient care:state care:state_5.}
     action:take_pill_Medication_677
     "P1D"^^xsd:dayTimeDuration
     77
     0.25179554544001936
     0.9886823311734452
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_678.}
     {?patient care:state care:state_11.}
     action:take_pill_Medication_678
     "P1D"^^xsd:dayTimeDuration
     85
     0.7730886912621167
     0.33889687649116695
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_679.}
     {?patient care:state care:state_20.}
     action:take_pill_Medication_679
     "P1D"^^xsd:dayTimeDuration
     79
     0.7145149511458494
     0.5076812725605708
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_680.}
     {?patient care:state care:state_13.}
     action:take_pill_Medication_680
     "P1D"^^xsd:dayTimeDuration
     96
     0.7078520540429942
     0.5350338860409851
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_681.}
     {?patient care:state care:state_18.}
     action:take_pill_Medication_681
     "P1D"^^xsd:dayTimeDuration
     6
     0.34184127835664946
     0.7426472606724552
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_682.}
     {?patient care:state care:state_8.}
     action:take_pill_Medication_682
     "P1D"^^xsd:dayTimeDuration
     72
     0.31045794804277976
     0.42876316005169535
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_683.}
     {?patient care:state care:state_10.}
     action:take_pill_Medication_683
     "P1D"^^xsd:dayTimeDuration
     21
     0.5610135854312346
     0.6063899045967962
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_684.}
     {?patient care:state care:state_11.}
     action:take_pill_Medication_684
     "P1D"^^xsd:dayTimeDuration
     27
     0.407830444701035
     0.4109538509076811
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_685.}
     {?patient care:state care:state_20.}
     action:take_pill_Medication_685
     "P1D"^^xsd:dayTimeDuration
     12
     0.3250404546621214
     0.5462262655119136
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_686.}
     {?patient care:state care:state_3.}
     action:take_pill_Medication_686
     "P1D"^^xsd:dayTimeDuration
     32
     0.7163792091362429
     0.78695861099554
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_687.}
     {?patient care:state care:state_12.}
     action:take_pill_Medication_687
     "P1D"^^xsd:dayTimeDuration
     95
     0.2579762447881099
     0.9080137701781041
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_688.}
     {?patient care:state care:state_11.}
     action:take_pill_Medication_688
     "P1D"^^xsd:dayTimeDuration
     93
     0.6226345092186846
     0.6766700585712584
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_689.}
     {?patient care:state care:state_20.}
     action:take_pill_Medication_689
     "P1D"^^xsd:dayTimeDuration
     52
     0.944491853720075
     0.9413168594779066
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_690.}
     {?patient care:state care:state_4.}
     action:take_pill_Medication_690
     "P1D"^^xsd:dayTimeDuration
     37
     0.4899620914620174
     0.2744726577306292
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_691.}
     {?patient care:state care:state_15.}
     action:take_pill_Medication_691
     "P1D"^^xsd:dayTimeDuration
     88
     0.9443581466654325
     0.6717393826793526
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_692.}
     {?patient care:state care:state_4.}
     action:take_pill_Medication_692
     "P1D"^^xsd:dayTimeDuration
     26
     0.33586398578454113
     0.7442032941791926
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_693.}
     {?patient care:state care:state_6.}
     action:take_pill_Medication_693
     "P1D"^^xsd:dayTimeDuration
     29
     0.973809841907839
     0.42083859562873505
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_694.}
     {?patient care:state care:state_16.}
     action:take_pill_Medication_694
     "P1D"^^xsd:dayTimeDuration
     87
     0.9271633789394259
     0.3208421773805996
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_695.}
     {?patient care:state care:state_10.}
     action:take_pill_Medication_695
     "P1D"^^xsd:dayTimeDuration
     68
     0.7971791654895327
     0.9657765987243017
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_696.}
     {?patient care:state care:state_13.}
     action:take_pill_Medication_696
     "P1D"^^xsd:dayTimeDuration
     86
     0.6530178708589318
     0.547412842677979
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_697.}
     {?patient care:state care:state_20.}
     action:take_pill_Medication_697
     "P1D"^^xsd:dayTimeDuration
     73
     0.731268688125936
     0.7605763978062048
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_698.}
     {?patient care:state care:state_20.}
     action:take_pill_Medication_698
     "P1D"^^xsd:dayTimeDuration
     13
     0.6121540898343061
     0.25373746418902265
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_699.}
     {?patient care:state care:state_2.}
     action:take_pill_Medication_699
     "P1D"^^xsd:dayTimeDuration
     11
     0.5821777756630406
     0.5602540586746291
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_700.}
     {?patient care:state care:state_8.}
     action:take_pill_Medication_700
     "P1D"^^xsd:dayTimeDuration
     43
     0.6717582471639125
     0.6404918703249061
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_701.}
     {?patient care:state care:state_18.}
     action:take_pill_Medication_701
     "P1D"^^xsd:dayTimeDuration
     20
     0.7693410771157317
     0.43149080107530635
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_702.}
     {?patient care:state care:state_3.}
     action:take_pill_Medication_702
     "P1D"^^xsd:dayTimeDuration
     12
     0.9050569493302304
     0.6698979401687957
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_703.}
     {?patient care:state care:state_13.}
     action:take_pill_Medication_703
     "P1D"^^xsd:dayTimeDuration
     44
     0.8052027514036507
     0.5645387968168705
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_704.}
     {?patient care:state care:state_19.}
     action:take_pill_Medication_704
     "P1D"^^xsd:dayTimeDuration
     56
     0.7135635553935273
     0.4897424460547406
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_705.}
     {?patient care:state care:state_6.}
     action:take_pill_Medication_705
     "P1D"^^xsd:dayTimeDuration
     4
     0.7306780813970675
     0.9609496674465479
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_706.}
     {?patient care:state care:state_7.}
     action:take_pill_Medication_706
     "P1D"^^xsd:dayTimeDuration
     60
     0.414752807930185
     0.5676434538181866
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_707.}
     {?patient care:state care:state_4.}
     action:take_pill_Medication_707
     "P1D"^^xsd:dayTimeDuration
     88
     0.4341626307111549
     0.6881118841671879
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_708.}
     {?patient care:state care:state_16.}
     action:take_pill_Medication_708
     "P1D"^^xsd:dayTimeDuration
     8
     0.5050366076255723
     0.7476540357097077
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_709.}
     {?patient care:state care:state_16.}
     action:take_pill_Medication_709
     "P1D"^^xsd:dayTimeDuration
     12
     0.7702222233463525
     0.31080441288849225
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_710.}
     {?patient care:state care:state_3.}
     action:take_pill_Medication_710
     "P1D"^^xsd:dayTimeDuration
     30
     0.945609597185344
     0.5707402294720465
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_711.}
     {?patient care:state care:state_5.}
     action:take_pill_Medication_711
     "P1D"^^xsd:dayTimeDuration
     65
     0.9185883367089542
     0.2749079829981666
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_712.}
     {?patient care:state care:state_17.}
     action:take_pill_Medication_712
     "P1D"^^xsd:dayTimeDuration
     45
     0.6043670526557452
     0.7224697540249714
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_713.}
     {?patient care:state care:state_18.}
     action:take_pill_Medication_713
     "P1D"^^xsd:dayTimeDuration
     5
     0.6808661194709753
     0.4841056785242218
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_714.}
     {?patient care:state care:state_6.}
     action:take_pill_Medication_714
     "P1D"^^xsd:dayTimeDuration
     9
     0.6160046621206249
     0.5423371270551511
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_715.}
     {?patient care:state care:state_5.}
     action:take_pill_Medication_715
     "P1D"^^xsd:dayTimeDuration
     12
     0.5677067834614565
     0.8216816030561324
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_716.}
     {?patient care:state care:state_17.}
     action:take_pill_Medication_716
     "P1D"^^xsd:dayTimeDuration
     23
     0.5464006813201904
     0.8218733595001171
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_717.}
     {?patient care:state care:state_20.}
     action:take_pill_Medication_717
     "P1D"^^xsd:dayTimeDuration
     3
     0.39017110677051936
     0.8718380862928877
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_718.}
     {?patient care:state care:state_13.}
     action:take_pill_Medication_718
     "P1D"^^xsd:dayTimeDuration
     98
     0.27835536492078206
     0.46667277353524506
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_719.}
     {?patient care:state care:state_20.}
     action:take_pill_Medication_719
     "P1D"^^xsd:dayTimeDuration
     75
     0.9377370359614863
     0.7111036999219115
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_720.}
     {?patient care:state care:state_11.}
     action:take_pill_Medication_720
     "P1D"^^xsd:dayTimeDuration
     38
     0.8133529048614212
     0.2945380149539523
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_721.}
     {?patient care:state care:state_19.}
     action:take_pill_Medication_721
     "P1D"^^xsd:dayTimeDuration
     7
     0.4134946503036936
     0.7197565118098305
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_722.}
     {?patient care:state care:state_15.}
     action:take_pill_Medication_722
     "P1D"^^xsd:dayTimeDuration
     94
     0.27120920884579425
     0.8490968870438299
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_723.}
     {?patient care:state care:state_19.}
     action:take_pill_Medication_723
     "P1D"^^xsd:dayTimeDuration
     55
     0.27273545611902544
     0.8758194015916005
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_724.}
     {?patient care:state care:state_14.}
     action:take_pill_Medication_724
     "P1D"^^xsd:dayTimeDuration
     69
     0.32427688994529225
     0.31738734572276034
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_725.}
     {?patient care:state care:state_17.}
     action:take_pill_Medication_725
     "P1D"^^xsd:dayTimeDuration
     85
     0.4009220663229041
     0.39807424845571354
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_726.}
     {?patient care:state care:state_4.}
     action:take_pill_Medication_726
     "P1D"^^xsd:dayTimeDuration
     85
     0.25924361257527095
     0.2787134086373755
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_727.}
     {?patient care:state care:state_17.}
     action:take_pill_Medication_727
     "P1D"^^xsd:dayTimeDuration
     15
     0.41669234965503926
     0.8220882294803376
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_728.}
     {?patient care:state care:state_6.}
     action:take_pill_Medication_728
     "P1D"^^xsd:dayTimeDuration
     61
     0.7026090483401783
     0.9174400292860455
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_729.}
     {?patient care:state care:state_7.}
     action:take_pill_Medication_729
     "P1D"^^xsd:dayTimeDuration
     49
     0.9972592068244936
     0.26011281233994965
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_730.}
     {?patient care:state care:state_12.}
     action:take_pill_Medication_730
     "P1D"^^xsd:dayTimeDuration
     7
     0.8228647639877462
     0.9720696343423201
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_731.}
     {?patient care:state care:state_6.}
     action:take_pill_Medication_731
     "P1D"^^xsd:dayTimeDuration
     85
     0.40640804006568976
     0.30709342615665447
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_732.}
     {?patient care:state care:state_14.}
     action:take_pill_Medication_732
     "P1D"^^xsd:dayTimeDuration
     44
     0.8203742901101458
     0.4150471890721892
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_733.}
     {?patient care:state care:state_8.}
     action:take_pill_Medication_733
     "P1D"^^xsd:dayTimeDuration
     6
     0.9490931848560502
     0.3469424849882176
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_734.}
     {?patient care:state care:state_10.}
     action:take_pill_Medication_734
     "P1D"^^xsd:dayTimeDuration
     25
     0.33957866232655376
     0.49734928472824114
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_735.}
     {?patient care:state care:state_5.}
     action:take_pill_Medication_735
     "P1D"^^xsd:dayTimeDuration
     63
     0.8012626392053882
     0.9810543994136165
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_736.}
     {?patient care:state care:state_18.}
     action:take_pill_Medication_736
     "P1D"^^xsd:dayTimeDuration
     66
     0.4128051511499089
     0.5015525917771738
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_737.}
     {?patient care:state care:state_16.}
     action:take_pill_Medication_737
     "P1D"^^xsd:dayTimeDuration
     13
     0.6118883038996179
     0.6601531354136166
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_738.}
     {?patient care:state care:state_20.}
     action:take_pill_Medication_738
     "P1D"^^xsd:dayTimeDuration
     81
     0.8817512361184663
     0.39299308115240517
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_739.}
     {?patient care:state care:state_17.}
     action:take_pill_Medication_739
     "P1D"^^xsd:dayTimeDuration
     54
     0.7088825068566089
     0.3539650895820089
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_740.}
     {?patient care:state care:state_9.}
     action:take_pill_Medication_740
     "P1D"^^xsd:dayTimeDuration
     49
     0.9936010486774166
     0.9151426481874462
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_741.}
     {?patient care:state care:state_20.}
     action:take_pill_Medication_741
     "P1D"^^xsd:dayTimeDuration
     50
     0.34405086815672464
     0.30517528932020654
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_742.}
     {?patient care:state care:state_12.}
     action:take_pill_Medication_742
     "P1D"^^xsd:dayTimeDuration
     16
     0.359755276811656
     0.34018793361106026
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_743.}
     {?patient care:state care:state_20.}
     action:take_pill_Medication_743
     "P1D"^^xsd:dayTimeDuration
     32
     0.7909620771591896
     0.6302680080543028
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_744.}
     {?patient care:state care:state_10.}
     action:take_pill_Medication_744
     "P1D"^^xsd:dayTimeDuration
     16
     0.5707733817806894
     0.914653924632473
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_745.}
     {?patient care:state care:state_15.}
     action:take_pill_Medication_745
     "P1D"^^xsd:dayTimeDuration
     71
     0.8251485606701792
     0.3917808478936038
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_746.}
     {?patient care:state care:state_9.}
     action:take_pill_Medication_746
     "P1D"^^xsd:dayTimeDuration
     23
     0.6407926999539649
     0.7407785746028405
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_747.}
     {?patient care:state care:state_11.}
     action:take_pill_Medication_747
     "P1D"^^xsd:dayTimeDuration
     96
     0.49059045672767393
     0.7260727846993036
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_748.}
     {?patient care:state care:state_18.}
     action:take_pill_Medication_748
     "P1D"^^xsd:dayTimeDuration
     55
     0.7324154934520751
     0.9996537255502559
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_749.}
     {?patient care:state care:state_16.}
     action:take_pill_Medication_749
     "P1D"^^xsd:dayTimeDuration
     16
     0.32328157654438067
     0.6508828922307818
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_750.}
     {?patient care:state care:state_5.}
     action:take_pill_Medication_750
     "P1D"^^xsd:dayTimeDuration
     25
     0.547452119478799
     0.8088591594665326
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_751.}
     {?patient care:state care:state_17.}
     action:take_pill_Medication_751
     "P1D"^^xsd:dayTimeDuration
     84
     0.5217636042814765
     0.46059000448958254
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_752.}
     {?patient care:state care:state_6.}
     action:take_pill_Medication_752
     "P1D"^^xsd:dayTimeDuration
     9
     0.9699011224624026
     0.5512232671483067
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_753.}
     {?patient care:state care:state_17.}
     action:take_pill_Medication_753
     "P1D"^^xsd:dayTimeDuration
     13
     0.3160376870923931
     0.6205752391716327
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_754.}
     {?patient care:state care:state_16.}
     action:take_pill_Medication_754
     "P1D"^^xsd:dayTimeDuration
     14
     0.8095031700618126
     0.29477037491970093
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_755.}
     {?patient care:state care:state_19.}
     action:take_pill_Medication_755
     "P1D"^^xsd:dayTimeDuration
     6
     0.6135301682642617
     0.6412741186846276
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_756.}
     {?patient care:state care:state_14.}
     action:take_pill_Medication_756
     "P1D"^^xsd:dayTimeDuration
     73
     0.6149359565717517
     0.5394798710726834
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_757.}
     {?patient care:state care:state_19.}
     action:take_pill_Medication_757
     "P1D"^^xsd:dayTimeDuration
     67
     0.8529769603917285
     0.31838037587536167
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_758.}
     {?patient care:state care:state_17.}
     action:take_pill_Medication_758
     "P1D"^^xsd:dayTimeDuration
     90
     0.9311890484669165
     0.5684882773316221
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_759.}
     {?patient care:state care:state_4.}
     action:take_pill_Medication_759
     "P1D"^^xsd:dayTimeDuration
     92
     0.44631497094404915
     0.894943697400477
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_760.}
     {?patient care:state care:state_11.}
     action:take_pill_Medication_760
     "P1D"^^xsd:dayTimeDuration
     75
     0.33797178284120827
     0.5053724964003816
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_761.}
     {?patient care:state care:state_10.}
     action:take_pill_Medication_761
     "P1D"^^xsd:dayTimeDuration
     86
     0.9660840055798676
     0.706990301259655
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_762.}
     {?patient care:state care:state_2.}
     action:take_pill_Medication_762
     "P1D"^^xsd:dayTimeDuration
     42
     0.8120369734186724
     0.8937291072353184
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_763.}
     {?patient care:state care:state_6.}
     action:take_pill_Medication_763
     "P1D"^^xsd:dayTimeDuration
     59
     0.482329276491868
     0.31343768693396346
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_764.}
     {?patient care:state care:state_18.}
     action:take_pill_Medication_764
     "P1D"^^xsd:dayTimeDuration
     30
     0.7168821664101441
     0.4850375106934196
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_765.}
     {?patient care:state care:state_4.}
     action:take_pill_Medication_765
     "P1D"^^xsd:dayTimeDuration
     26
     0.5804097286741414
     0.5600210176386339
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_766.}
     {?patient care:state care:state_20.}
     action:take_pill_Medication_766
     "P1D"^^xsd:dayTimeDuration
     17
     0.443243431170375
     0.8811651267238294
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_767.}
     {?patient care:state care:state_8.}
     action:take_pill_Medication_767
     "P1D"^^xsd:dayTimeDuration
     35
     0.6565993177511951
     0.5892262864373337
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_768.}
     {?patient care:state care:state_5.}
     action:take_pill_Medication_768
     "P1D"^^xsd:dayTimeDuration
     100
     0.2649714152502274
     0.9067860440839222
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_769.}
     {?patient care:state care:state_9.}
     action:take_pill_Medication_769
     "P1D"^^xsd:dayTimeDuration
     31
     0.7497033098532216
     0.763956465724388
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_770.}
     {?patient care:state care:state_19.}
     action:take_pill_Medication_770
     "P1D"^^xsd:dayTimeDuration
     33
     0.32853314662486566
     0.7403309335949788
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_771.}
     {?patient care:state care:state_17.}
     action:take_pill_Medication_771
     "P1D"^^xsd:dayTimeDuration
     11
     0.7312798660003118
     0.5829274201948593
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_772.}
     {?patient care:state care:state_11.}
     action:take_pill_Medication_772
     "P1D"^^xsd:dayTimeDuration
     87
     0.7059081598596737
     0.8955967918498273
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_773.}
     {?patient care:state care:state_17.}
     action:take_pill_Medication_773
     "P1D"^^xsd:dayTimeDuration
     97
     0.29236066039052433
     0.6656670867089729
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_774.}
     {?patient care:state care:state_11.}
     action:take_pill_Medication_774
     "P1D"^^xsd:dayTimeDuration
     22
     0.8670270735221364
     0.5195842759228582
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_775.}
     {?patient care:state care:state_6.}
     action:take_pill_Medication_775
     "P1D"^^xsd:dayTimeDuration
     6
     0.9812842220575098
     0.8487338284024295
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_776.}
     {?patient care:state care:state_4.}
     action:take_pill_Medication_776
     "P1D"^^xsd:dayTimeDuration
     54
     0.9607612601795491
     0.44881499627643284
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_777.}
     {?patient care:state care:state_7.}
     action:take_pill_Medication_777
     "P1D"^^xsd:dayTimeDuration
     12
     0.4855040671344715
     0.7709503706022225
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_778.}
     {?patient care:state care:state_5.}
     action:take_pill_Medication_778
     "P1D"^^xsd:dayTimeDuration
     47
     0.7100103610473097
     0.7159656982683587
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_779.}
     {?patient care:state care:state_5.}
     action:take_pill_Medication_779
     "P1D"^^xsd:dayTimeDuration
     60
     0.565566954333041
     0.8883652315573026
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_780.}
     {?patient care:state care:state_17.}
     action:take_pill_Medication_780
     "P1D"^^xsd:dayTimeDuration
     60
     0.32225672739212863
     0.6857677809551882
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_781.}
     {?patient care:state care:state_4.}
     action:take_pill_Medication_781
     "P1D"^^xsd:dayTimeDuration
     36
     0.2897276795500171
     0.441111639398667
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_782.}
     {?patient care:state care:state_20.}
     action:take_pill_Medication_782
     "P1D"^^xsd:dayTimeDuration
     72
     0.540360510698823
     0.644654903442418
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_783.}
     {?patient care:state care:state_20.}
     action:take_pill_Medication_783
     "P1D"^^xsd:dayTimeDuration
     44
     0.3285538877795433
     0.7469229081926246
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_784.}
     {?patient care:state care:state_20.}
     action:take_pill_Medication_784
     "P1D"^^xsd:dayTimeDuration
     87
     0.37747385133053235
     0.8452687081841501
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_785.}
     {?patient care:state care:state_10.}
     action:take_pill_Medication_785
     "P1D"^^xsd:dayTimeDuration
     12
     0.5579359342443435
     0.287404050867065
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_786.}
     {?patient care:state care:state_17.}
     action:take_pill_Medication_786
     "P1D"^^xsd:dayTimeDuration
     54
     0.46773654160294803
     0.6921954934347092
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_787.}
     {?patient care:state care:state_6.}
     action:take_pill_Medication_787
     "P1D"^^xsd:dayTimeDuration
     49
     0.9607378035488304
     0.6958610252235793
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_788.}
     {?patient care:state care:state_15.}
     action:take_pill_Medication_788
     "P1D"^^xsd:dayTimeDuration
     8
     0.25255934261696955
     0.36029456620119443
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_789.}
     {?patient care:state care:state_17.}
     action:take_pill_Medication_789
     "P1D"^^xsd:dayTimeDuration
     19
     0.9077064280897836
     0.2694156161356297
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_790.}
     {?patient care:state care:state_15.}
     action:take_pill_Medication_790
     "P1D"^^xsd:dayTimeDuration
     100
     0.8954324365347875
     0.956739704503029
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_791.}
     {?patient care:state care:state_3.}
     action:take_pill_Medication_791
     "P1D"^^xsd:dayTimeDuration
     49
     0.7545112221156083
     0.387759384572604
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_792.}
     {?patient care:state care:state_7.}
     action:take_pill_Medication_792
     "P1D"^^xsd:dayTimeDuration
     42
     0.4691426904182924
     0.9678483919776959
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_793.}
     {?patient care:state care:state_20.}
     action:take_pill_Medication_793
     "P1D"^^xsd:dayTimeDuration
     59
     0.36174953627938
     0.5289326842900346
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_794.}
     {?patient care:state care:state_17.}
     action:take_pill_Medication_794
     "P1D"^^xsd:dayTimeDuration
     77
     0.3168713225236788
     0.9904202522138404
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_795.}
     {?patient care:state care:state_9.}
     action:take_pill_Medication_795
     "P1D"^^xsd:dayTimeDuration
     2
     0.5472115645368537
     0.8980236332985367
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_796.}
     {?patient care:state care:state_16.}
     action:take_pill_Medication_796
     "P1D"^^xsd:dayTimeDuration
     29
     0.4228909021753169
     0.5040407447736828
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_797.}
     {?patient care:state care:state_9.}
     action:take_pill_Medication_797
     "P1D"^^xsd:dayTimeDuration
     28
     0.4589288995557599
     0.3966986550788802
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_798.}
     {?patient care:state care:state_4.}
     action:take_pill_Medication_798
     "P1D"^^xsd:dayTimeDuration
     89
     0.7603412723917985
     0.5147744069013962
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_799.}
     {?patient care:state care:state_6.}
     action:take_pill_Medication_799
     "P1D"^^xsd:dayTimeDuration
     45
     0.5559913166740014
     0.7817922613811842
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_800.}
     {?patient care:state care:state_9.}
     action:take_pill_Medication_800
     "P1D"^^xsd:dayTimeDuration
     43
     0.27203028191133105
     0.3573560583345662
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_801.}
     {?patient care:state care:state_6.}
     action:take_pill_Medication_801
     "P1D"^^xsd:dayTimeDuration
     88
     0.7479522225413423
     0.2991246556680626
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_802.}
     {?patient care:state care:state_14.}
     action:take_pill_Medication_802
     "P1D"^^xsd:dayTimeDuration
     75
     0.9707978859804252
     0.9579389322733708
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_803.}
     {?patient care:state care:state_9.}
     action:take_pill_Medication_803
     "P1D"^^xsd:dayTimeDuration
     49
     0.9173535517876663
     0.29993069012967527
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_804.}
     {?patient care:state care:state_11.}
     action:take_pill_Medication_804
     "P1D"^^xsd:dayTimeDuration
     64
     0.9112204609356312
     0.4884924503477981
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_805.}
     {?patient care:state care:state_19.}
     action:take_pill_Medication_805
     "P1D"^^xsd:dayTimeDuration
     4
     0.5484192064285665
     0.7533629496113539
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_806.}
     {?patient care:state care:state_16.}
     action:take_pill_Medication_806
     "P1D"^^xsd:dayTimeDuration
     55
     0.5842375715102478
     0.26203194078425834
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_807.}
     {?patient care:state care:state_7.}
     action:take_pill_Medication_807
     "P1D"^^xsd:dayTimeDuration
     32
     0.5743657799972524
     0.33529970968884515
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_808.}
     {?patient care:state care:state_19.}
     action:take_pill_Medication_808
     "P1D"^^xsd:dayTimeDuration
     25
     0.800008178389932
     0.8353271907230094
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_809.}
     {?patient care:state care:state_15.}
     action:take_pill_Medication_809
     "P1D"^^xsd:dayTimeDuration
     4
     0.5872635625948737
     0.8010610862771812
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_810.}
     {?patient care:state care:state_8.}
     action:take_pill_Medication_810
     "P1D"^^xsd:dayTimeDuration
     1
     0.722310579446533
     0.4905299256292298
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_811.}
     {?patient care:state care:state_5.}
     action:take_pill_Medication_811
     "P1D"^^xsd:dayTimeDuration
     2
     0.33147330913736406
     0.5934635512113637
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_812.}
     {?patient care:state care:state_8.}
     action:take_pill_Medication_812
     "P1D"^^xsd:dayTimeDuration
     33
     0.9135101087903865
     0.2520771660655675
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_813.}
     {?patient care:state care:state_5.}
     action:take_pill_Medication_813
     "P1D"^^xsd:dayTimeDuration
     1
     0.8093070015136457
     0.731737535135711
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_814.}
     {?patient care:state care:state_6.}
     action:take_pill_Medication_814
     "P1D"^^xsd:dayTimeDuration
     66
     0.7654802814457596
     0.871342730048378
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_815.}
     {?patient care:state care:state_18.}
     action:take_pill_Medication_815
     "P1D"^^xsd:dayTimeDuration
     42
     0.2513596073028719
     0.7152775043984748
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_816.}
     {?patient care:state care:state_14.}
     action:take_pill_Medication_816
     "P1D"^^xsd:dayTimeDuration
     21
     0.964995397068062
     0.7244727092107958
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_817.}
     {?patient care:state care:state_17.}
     action:take_pill_Medication_817
     "P1D"^^xsd:dayTimeDuration
     99
     0.8586912786855445
     0.464354355932611
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_818.}
     {?patient care:state care:state_19.}
     action:take_pill_Medication_818
     "P1D"^^xsd:dayTimeDuration
     3
     0.42371921321174055
     0.9922587629143161
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_819.}
     {?patient care:state care:state_14.}
     action:take_pill_Medication_819
     "P1D"^^xsd:dayTimeDuration
     27
     0.754909498856301
     0.2864503035677478
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_820.}
     {?patient care:state care:state_6.}
     action:take_pill_Medication_820
     "P1D"^^xsd:dayTimeDuration
     34
     0.9275444361239346
     0.9590267929993687
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_821.}
     {?patient care:state care:state_13.}
     action:take_pill_Medication_821
     "P1D"^^xsd:dayTimeDuration
     67
     0.6463188960538373
     0.9931240294488092
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_822.}
     {?patient care:state care:state_12.}
     action:take_pill_Medication_822
     "P1D"^^xsd:dayTimeDuration
     27
     0.9819024064665544
     0.43246446670568744
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_823.}
     {?patient care:state care:state_8.}
     action:take_pill_Medication_823
     "P1D"^^xsd:dayTimeDuration
     95
     0.5017475233892934
     0.2663876882564532
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_824.}
     {?patient care:state care:state_15.}
     action:take_pill_Medication_824
     "P1D"^^xsd:dayTimeDuration
     33
     0.8687438160345832
     0.2836676255067066
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_825.}
     {?patient care:state care:state_8.}
     action:take_pill_Medication_825
     "P1D"^^xsd:dayTimeDuration
     3
     0.44706019747154696
     0.703073992858681
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_826.}
     {?patient care:state care:state_13.}
     action:take_pill_Medication_826
     "P1D"^^xsd:dayTimeDuration
     65
     0.8500117431224681
     0.8186071266923113
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_827.}
     {?patient care:state care:state_16.}
     action:take_pill_Medication_827
     "P1D"^^xsd:dayTimeDuration
     73
     0.7770394183020451
     0.8338503975089033
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_828.}
     {?patient care:state care:state_5.}
     action:take_pill_Medication_828
     "P1D"^^xsd:dayTimeDuration
     48
     0.568619696893568
     0.42645616804533315
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_829.}
     {?patient care:state care:state_7.}
     action:take_pill_Medication_829
     "P1D"^^xsd:dayTimeDuration
     77
     0.8875671336226663
     0.53781054925526
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_830.}
     {?patient care:state care:state_4.}
     action:take_pill_Medication_830
     "P1D"^^xsd:dayTimeDuration
     52
     0.9518119401975349
     0.7502667614820354
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_831.}
     {?patient care:state care:state_16.}
     action:take_pill_Medication_831
     "P1D"^^xsd:dayTimeDuration
     14
     0.929566709554205
     0.9563570232264738
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_832.}
     {?patient care:state care:state_18.}
     action:take_pill_Medication_832
     "P1D"^^xsd:dayTimeDuration
     39
     0.8521875113199814
     0.4814036647309433
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_833.}
     {?patient care:state care:state_10.}
     action:take_pill_Medication_833
     "P1D"^^xsd:dayTimeDuration
     58
     0.6018079413035208
     0.6001447370157147
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_834.}
     {?patient care:state care:state_3.}
     action:take_pill_Medication_834
     "P1D"^^xsd:dayTimeDuration
     56
     0.3462440504493689
     0.5213154022087128
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_835.}
     {?patient care:state care:state_13.}
     action:take_pill_Medication_835
     "P1D"^^xsd:dayTimeDuration
     92
     0.34752261047307453
     0.30636449573306757
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_836.}
     {?patient care:state care:state_12.}
     action:take_pill_Medication_836
     "P1D"^^xsd:dayTimeDuration
     83
     0.6706979776611309
     0.34336671955247833
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_837.}
     {?patient care:state care:state_5.}
     action:take_pill_Medication_837
     "P1D"^^xsd:dayTimeDuration
     67
     0.565485475914755
     0.9022676885844431
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_838.}
     {?patient care:state care:state_14.}
     action:take_pill_Medication_838
     "P1D"^^xsd:dayTimeDuration
     28
     0.4440692687186434
     0.41322192917068984
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_839.}
     {?patient care:state care:state_17.}
     action:take_pill_Medication_839
     "P1D"^^xsd:dayTimeDuration
     82
     0.9371397343285988
     0.3568740763671917
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_840.}
     {?patient care:state care:state_16.}
     action:take_pill_Medication_840
     "P1D"^^xsd:dayTimeDuration
     32
     0.7749074811785726
     0.6901800285650508
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_841.}
     {?patient care:state care:state_20.}
     action:take_pill_Medication_841
     "P1D"^^xsd:dayTimeDuration
     39
     0.8491624228990703
     0.9783035396881096
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_842.}
     {?patient care:state care:state_18.}
     action:take_pill_Medication_842
     "P1D"^^xsd:dayTimeDuration
     58
     0.8651617968238857
     0.8893939222696089
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_843.}
     {?patient care:state care:state_14.}
     action:take_pill_Medication_843
     "P1D"^^xsd:dayTimeDuration
     96
     0.5767620716043345
     0.7447468375427602
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_844.}
     {?patient care:state care:state_11.}
     action:take_pill_Medication_844
     "P1D"^^xsd:dayTimeDuration
     10
     0.6578781626938676
     0.6832352932025672
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_845.}
     {?patient care:state care:state_13.}
     action:take_pill_Medication_845
     "P1D"^^xsd:dayTimeDuration
     61
     0.8314882907102644
     0.5710628189809072
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_846.}
     {?patient care:state care:state_17.}
     action:take_pill_Medication_846
     "P1D"^^xsd:dayTimeDuration
     2
     0.6570531891839148
     0.5538926181702948
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_847.}
     {?patient care:state care:state_14.}
     action:take_pill_Medication_847
     "P1D"^^xsd:dayTimeDuration
     96
     0.8289338999750506
     0.5965525746531735
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_848.}
     {?patient care:state care:state_18.}
     action:take_pill_Medication_848
     "P1D"^^xsd:dayTimeDuration
     9
     0.9290088320560422
     0.8208593028241523
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_849.}
     {?patient care:state care:state_9.}
     action:take_pill_Medication_849
     "P1D"^^xsd:dayTimeDuration
     65
     0.8507416159190953
     0.5211785863570362
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_850.}
     {?patient care:state care:state_6.}
     action:take_pill_Medication_850
     "P1D"^^xsd:dayTimeDuration
     77
     0.973004083537658
     0.8915374064543182
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_851.}
     {?patient care:state care:state_20.}
     action:take_pill_Medication_851
     "P1D"^^xsd:dayTimeDuration
     84
     0.3464014203724929
     0.6024732686371735
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_852.}
     {?patient care:state care:state_8.}
     action:take_pill_Medication_852
     "P1D"^^xsd:dayTimeDuration
     7
     0.8687115162472576
     0.7374628203969895
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_853.}
     {?patient care:state care:state_19.}
     action:take_pill_Medication_853
     "P1D"^^xsd:dayTimeDuration
     72
     0.6195762103405118
     0.7072337359404389
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_854.}
     {?patient care:state care:state_5.}
     action:take_pill_Medication_854
     "P1D"^^xsd:dayTimeDuration
     67
     0.6490743509601514
     0.8920471415234694
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_855.}
     {?patient care:state care:state_16.}
     action:take_pill_Medication_855
     "P1D"^^xsd:dayTimeDuration
     99
     0.9153178769858041
     0.5713804884164988
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_856.}
     {?patient care:state care:state_9.}
     action:take_pill_Medication_856
     "P1D"^^xsd:dayTimeDuration
     77
     0.8168198750091058
     0.9543232069738752
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_857.}
     {?patient care:state care:state_20.}
     action:take_pill_Medication_857
     "P1D"^^xsd:dayTimeDuration
     1
     0.9314573530190671
     0.3490455020372847
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_858.}
     {?patient care:state care:state_6.}
     action:take_pill_Medication_858
     "P1D"^^xsd:dayTimeDuration
     42
     0.3196394471076589
     0.9813388784926054
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_859.}
     {?patient care:state care:state_14.}
     action:take_pill_Medication_859
     "P1D"^^xsd:dayTimeDuration
     67
     0.28290038926944455
     0.8628440846950289
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_860.}
     {?patient care:state care:state_16.}
     action:take_pill_Medication_860
     "P1D"^^xsd:dayTimeDuration
     95
     0.8840929742886919
     0.9477401401721288
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_861.}
     {?patient care:state care:state_18.}
     action:take_pill_Medication_861
     "P1D"^^xsd:dayTimeDuration
     77
     0.2989975808949832
     0.3923721775113963
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_862.}
     {?patient care:state care:state_4.}
     action:take_pill_Medication_862
     "P1D"^^xsd:dayTimeDuration
     94
     0.41785097313027986
     0.6370592661120671
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_863.}
     {?patient care:state care:state_19.}
     action:take_pill_Medication_863
     "P1D"^^xsd:dayTimeDuration
     71
     0.9986550041450271
     0.3247177209131462
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_864.}
     {?patient care:state care:state_15.}
     action:take_pill_Medication_864
     "P1D"^^xsd:dayTimeDuration
     52
     0.8638933489967707
     0.7710812092204045
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_865.}
     {?patient care:state care:state_10.}
     action:take_pill_Medication_865
     "P1D"^^xsd:dayTimeDuration
     29
     0.36725988337174226
     0.7117105867800109
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_866.}
     {?patient care:state care:state_17.}
     action:take_pill_Medication_866
     "P1D"^^xsd:dayTimeDuration
     66
     0.28796552654611773
     0.6980643723267079
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_867.}
     {?patient care:state care:state_9.}
     action:take_pill_Medication_867
     "P1D"^^xsd:dayTimeDuration
     66
     0.40891973574927554
     0.9766286845638483
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_868.}
     {?patient care:state care:state_17.}
     action:take_pill_Medication_868
     "P1D"^^xsd:dayTimeDuration
     89
     0.7677819446910068
     0.6564677266903316
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_869.}
     {?patient care:state care:state_20.}
     action:take_pill_Medication_869
     "P1D"^^xsd:dayTimeDuration
     48
     0.6497802715447282
     0.8651711401691196
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_870.}
     {?patient care:state care:state_9.}
     action:take_pill_Medication_870
     "P1D"^^xsd:dayTimeDuration
     65
     0.9713398462061162
     0.6335401968762139
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_871.}
     {?patient care:state care:state_6.}
     action:take_pill_Medication_871
     "P1D"^^xsd:dayTimeDuration
     39
     0.8899290911381685
     0.9618062635155603
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_872.}
     {?patient care:state care:state_14.}
     action:take_pill_Medication_872
     "P1D"^^xsd:dayTimeDuration
     81
     0.5167708320886647
     0.8292675565960667
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_873.}
     {?patient care:state care:state_5.}
     action:take_pill_Medication_873
     "P1D"^^xsd:dayTimeDuration
     88
     0.4008157815018109
     0.49472175386446327
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_874.}
     {?patient care:state care:state_11.}
     action:take_pill_Medication_874
     "P1D"^^xsd:dayTimeDuration
     92
     0.6007215753255557
     0.4191206942232951
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_875.}
     {?patient care:state care:state_15.}
     action:take_pill_Medication_875
     "P1D"^^xsd:dayTimeDuration
     99
     0.9100890350153878
     0.7768353171663751
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_876.}
     {?patient care:state care:state_14.}
     action:take_pill_Medication_876
     "P1D"^^xsd:dayTimeDuration
     83
     0.3137986750539543
     0.7543146546362647
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_877.}
     {?patient care:state care:state_15.}
     action:take_pill_Medication_877
     "P1D"^^xsd:dayTimeDuration
     66
     0.7705216446384544
     0.827000391345448
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_878.}
     {?patient care:state care:state_11.}
     action:take_pill_Medication_878
     "P1D"^^xsd:dayTimeDuration
     91
     0.35099212555925796
     0.515888796735211
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_879.}
     {?patient care:state care:state_9.}
     action:take_pill_Medication_879
     "P1D"^^xsd:dayTimeDuration
     28
     0.9924875213846014
     0.5768360463871849
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_880.}
     {?patient care:state care:state_6.}
     action:take_pill_Medication_880
     "P1D"^^xsd:dayTimeDuration
     79
     0.30254999327188203
     0.9802961466177049
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_881.}
     {?patient care:state care:state_12.}
     action:take_pill_Medication_881
     "P1D"^^xsd:dayTimeDuration
     56
     0.29318695039749676
     0.878191163457509
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_882.}
     {?patient care:state care:state_12.}
     action:take_pill_Medication_882
     "P1D"^^xsd:dayTimeDuration
     99
     0.6114280714167734
     0.251062429811231
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_883.}
     {?patient care:state care:state_11.}
     action:take_pill_Medication_883
     "P1D"^^xsd:dayTimeDuration
     62
     0.6199610127011027
     0.6346233551020747
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_884.}
     {?patient care:state care:state_11.}
     action:take_pill_Medication_884
     "P1D"^^xsd:dayTimeDuration
     21
     0.5199615802351325
     0.3188159902505026
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_885.}
     {?patient care:state care:state_10.}
     action:take_pill_Medication_885
     "P1D"^^xsd:dayTimeDuration
     35
     0.38544919509791625
     0.8904935439866463
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_886.}
     {?patient care:state care:state_20.}
     action:take_pill_Medication_886
     "P1D"^^xsd:dayTimeDuration
     95
     0.2916290919941789
     0.3677788414746561
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_887.}
     {?patient care:state care:state_10.}
     action:take_pill_Medication_887
     "P1D"^^xsd:dayTimeDuration
     71
     0.33357288210165714
     0.33158004270321684
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_888.}
     {?patient care:state care:state_16.}
     action:take_pill_Medication_888
     "P1D"^^xsd:dayTimeDuration
     22
     0.374806156792281
     0.5318496988958797
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_889.}
     {?patient care:state care:state_8.}
     action:take_pill_Medication_889
     "P1D"^^xsd:dayTimeDuration
     76
     0.3625438271874275
     0.35482533870380945
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_890.}
     {?patient care:state care:state_18.}
     action:take_pill_Medication_890
     "P1D"^^xsd:dayTimeDuration
     52
     0.29756301327627344
     0.43995437052634406
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_891.}
     {?patient care:state care:state_20.}
     action:take_pill_Medication_891
     "P1D"^^xsd:dayTimeDuration
     67
     0.6825578270308716
     0.6944872261449464
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_892.}
     {?patient care:state care:state_3.}
     action:take_pill_Medication_892
     "P1D"^^xsd:dayTimeDuration
     99
     0.7782391617837379
     0.3297357203531882
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_893.}
     {?patient care:state care:state_15.}
     action:take_pill_Medication_893
     "P1D"^^xsd:dayTimeDuration
     57
     0.49375931634986814
     0.9930369675592886
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_894.}
     {?patient care:state care:state_15.}
     action:take_pill_Medication_894
     "P1D"^^xsd:dayTimeDuration
     25
     0.5605664650080884
     0.6004487200970261
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_895.}
     {?patient care:state care:state_14.}
     action:take_pill_Medication_895
     "P1D"^^xsd:dayTimeDuration
     27
     0.5039554150246401
     0.331755801060557
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_896.}
     {?patient care:state care:state_14.}
     action:take_pill_Medication_896
     "P1D"^^xsd:dayTimeDuration
     80
     0.5819335336714406
     0.4290233420405426
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_897.}
     {?patient care:state care:state_16.}
     action:take_pill_Medication_897
     "P1D"^^xsd:dayTimeDuration
     95
     0.3345114078200211
     0.2806136561653796
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_898.}
     {?patient care:state care:state_16.}
     action:take_pill_Medication_898
     "P1D"^^xsd:dayTimeDuration
     34
     0.3403386033596669
     0.8642493395826065
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_899.}
     {?patient care:state care:state_14.}
     action:take_pill_Medication_899
     "P1D"^^xsd:dayTimeDuration
     65
     0.30618148635147724
     0.7506343603585195
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_900.}
     {?patient care:state care:state_8.}
     action:take_pill_Medication_900
     "P1D"^^xsd:dayTimeDuration
     20
     0.5555955000423597
     0.5490530292691707
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_901.}
     {?patient care:state care:state_17.}
     action:take_pill_Medication_901
     "P1D"^^xsd:dayTimeDuration
     21
     0.44334789800661023
     0.8608230647053203
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_902.}
     {?patient care:state care:state_4.}
     action:take_pill_Medication_902
     "P1D"^^xsd:dayTimeDuration
     46
     0.7002605130014807
     0.5765214529008402
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_903.}
     {?patient care:state care:state_10.}
     action:take_pill_Medication_903
     "P1D"^^xsd:dayTimeDuration
     36
     0.38916293438998906
     0.6228719009067587
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_904.}
     {?patient care:state care:state_20.}
     action:take_pill_Medication_904
     "P1D"^^xsd:dayTimeDuration
     83
     0.8701923343800213
     0.7909798090854999
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_905.}
     {?patient care:state care:state_15.}
     action:take_pill_Medication_905
     "P1D"^^xsd:dayTimeDuration
     11
     0.6033821925452343
     0.6450106176416789
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_906.}
     {?patient care:state care:state_3.}
     action:take_pill_Medication_906
     "P1D"^^xsd:dayTimeDuration
     24
     0.8177116208972229
     0.7856205825307919
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_907.}
     {?patient care:state care:state_8.}
     action:take_pill_Medication_907
     "P1D"^^xsd:dayTimeDuration
     5
     0.5329704494434826
     0.48970313143264926
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_908.}
     {?patient care:state care:state_14.}
     action:take_pill_Medication_908
     "P1D"^^xsd:dayTimeDuration
     20
     0.3333370453980421
     0.4163323490944295
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_909.}
     {?patient care:state care:state_13.}
     action:take_pill_Medication_909
     "P1D"^^xsd:dayTimeDuration
     79
     0.8006111071701597
     0.3516536727396391
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_910.}
     {?patient care:state care:state_14.}
     action:take_pill_Medication_910
     "P1D"^^xsd:dayTimeDuration
     95
     0.9364935701397807
     0.9679384247996359
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_911.}
     {?patient care:state care:state_5.}
     action:take_pill_Medication_911
     "P1D"^^xsd:dayTimeDuration
     68
     0.3897914157835294
     0.3553728448786403
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_912.}
     {?patient care:state care:state_5.}
     action:take_pill_Medication_912
     "P1D"^^xsd:dayTimeDuration
     73
     0.3153899733725096
     0.9698322402358875
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_913.}
     {?patient care:state care:state_5.}
     action:take_pill_Medication_913
     "P1D"^^xsd:dayTimeDuration
     92
     0.8620851522309663
     0.8128046297542872
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_914.}
     {?patient care:state care:state_10.}
     action:take_pill_Medication_914
     "P1D"^^xsd:dayTimeDuration
     7
     0.5718013966701048
     0.48728844036295804
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_915.}
     {?patient care:state care:state_4.}
     action:take_pill_Medication_915
     "P1D"^^xsd:dayTimeDuration
     42
     0.38356469063130727
     0.6111715608199797
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_916.}
     {?patient care:state care:state_19.}
     action:take_pill_Medication_916
     "P1D"^^xsd:dayTimeDuration
     78
     0.32265214351426164
     0.351523909046007
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_917.}
     {?patient care:state care:state_15.}
     action:take_pill_Medication_917
     "P1D"^^xsd:dayTimeDuration
     99
     0.6986018525269115
     0.4012228877215119
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_918.}
     {?patient care:state care:state_18.}
     action:take_pill_Medication_918
     "P1D"^^xsd:dayTimeDuration
     34
     0.6608107726562122
     0.48686156003930403
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_919.}
     {?patient care:state care:state_13.}
     action:take_pill_Medication_919
     "P1D"^^xsd:dayTimeDuration
     24
     0.342291352526556
     0.7012880799502448
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_920.}
     {?patient care:state care:state_11.}
     action:take_pill_Medication_920
     "P1D"^^xsd:dayTimeDuration
     52
     0.6626298566647209
     0.3516902205764859
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_921.}
     {?patient care:state care:state_7.}
     action:take_pill_Medication_921
     "P1D"^^xsd:dayTimeDuration
     46
     0.6997744928975014
     0.8014567627680029
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_922.}
     {?patient care:state care:state_19.}
     action:take_pill_Medication_922
     "P1D"^^xsd:dayTimeDuration
     55
     0.35466009874022264
     0.35320323734797116
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_923.}
     {?patient care:state care:state_15.}
     action:take_pill_Medication_923
     "P1D"^^xsd:dayTimeDuration
     92
     0.6770508498107337
     0.33934624183436035
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_924.}
     {?patient care:state care:state_5.}
     action:take_pill_Medication_924
     "P1D"^^xsd:dayTimeDuration
     22
     0.8959114418665585
     0.8655318485370879
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_925.}
     {?patient care:state care:state_10.}
     action:take_pill_Medication_925
     "P1D"^^xsd:dayTimeDuration
     37
     0.5996869285943062
     0.7788265422192739
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_926.}
     {?patient care:state care:state_3.}
     action:take_pill_Medication_926
     "P1D"^^xsd:dayTimeDuration
     40
     0.8661416027893946
     0.669754885749325
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_927.}
     {?patient care:state care:state_19.}
     action:take_pill_Medication_927
     "P1D"^^xsd:dayTimeDuration
     84
     0.895404201295322
     0.8548493589247552
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_928.}
     {?patient care:state care:state_4.}
     action:take_pill_Medication_928
     "P1D"^^xsd:dayTimeDuration
     89
     0.6997932498682946
     0.7236877495451183
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_929.}
     {?patient care:state care:state_5.}
     action:take_pill_Medication_929
     "P1D"^^xsd:dayTimeDuration
     38
     0.7381772145691083
     0.9589399446374054
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_930.}
     {?patient care:state care:state_8.}
     action:take_pill_Medication_930
     "P1D"^^xsd:dayTimeDuration
     33
     0.9650151416628973
     0.9932334333341042
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_931.}
     {?patient care:state care:state_8.}
     action:take_pill_Medication_931
     "P1D"^^xsd:dayTimeDuration
     23
     0.27793444826967884
     0.6563396081296271
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_932.}
     {?patient care:state care:state_16.}
     action:take_pill_Medication_932
     "P1D"^^xsd:dayTimeDuration
     36
     0.5773359838925312
     0.5695635148090881
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_933.}
     {?patient care:state care:state_8.}
     action:take_pill_Medication_933
     "P1D"^^xsd:dayTimeDuration
     29
     0.5082539822854282
     0.26530188574151725
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_934.}
     {?patient care:state care:state_19.}
     action:take_pill_Medication_934
     "P1D"^^xsd:dayTimeDuration
     59
     0.8292979908103298
     0.7808820187990093
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_935.}
     {?patient care:state care:state_16.}
     action:take_pill_Medication_935
     "P1D"^^xsd:dayTimeDuration
     43
     0.6429101761526788
     0.906200895901981
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_936.}
     {?patient care:state care:state_4.}
     action:take_pill_Medication_936
     "P1D"^^xsd:dayTimeDuration
     50
     0.5543230031321342
     0.454385169231536
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_937.}
     {?patient care:state care:state_11.}
     action:take_pill_Medication_937
     "P1D"^^xsd:dayTimeDuration
     37
     0.5583365490787765
     0.8009008579703356
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_938.}
     {?patient care:state care:state_10.}
     action:take_pill_Medication_938
     "P1D"^^xsd:dayTimeDuration
     6
     0.4575968882866629
     0.6446321552096651
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_939.}
     {?patient care:state care:state_10.}
     action:take_pill_Medication_939
     "P1D"^^xsd:dayTimeDuration
     94
     0.9384109295098908
     0.681578280047953
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_940.}
     {?patient care:state care:state_18.}
     action:take_pill_Medication_940
     "P1D"^^xsd:dayTimeDuration
     63
     0.7450397227928227
     0.7466191541907752
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_941.}
     {?patient care:state care:state_20.}
     action:take_pill_Medication_941
     "P1D"^^xsd:dayTimeDuration
     48
     0.8875361925442284
     0.8096555004685895
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_942.}
     {?patient care:state care:state_10.}
     action:take_pill_Medication_942
     "P1D"^^xsd:dayTimeDuration
     2
     0.3658373846955282
     0.6259039586692285
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_943.}
     {?patient care:state care:state_3.}
     action:take_pill_Medication_943
     "P1D"^^xsd:dayTimeDuration
     26
     0.25737998373114157
     0.2919775050549205
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_944.}
     {?patient care:state care:state_19.}
     action:take_pill_Medication_944
     "P1D"^^xsd:dayTimeDuration
     47
     0.7965011543673194
     0.4081259937278409
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_945.}
     {?patient care:state care:state_13.}
     action:take_pill_Medication_945
     "P1D"^^xsd:dayTimeDuration
     81
     0.5163469774788758
     0.9579580682763359
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_946.}
     {?patient care:state care:state_19.}
     action:take_pill_Medication_946
     "P1D"^^xsd:dayTimeDuration
     77
     0.39974012561353517
     0.8515724371133989
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_947.}
     {?patient care:state care:state_8.}
     action:take_pill_Medication_947
     "P1D"^^xsd:dayTimeDuration
     72
     0.26204506466500915
     0.841440863024799
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_948.}
     {?patient care:state care:state_10.}
     action:take_pill_Medication_948
     "P1D"^^xsd:dayTimeDuration
     86
     0.9639116887330271
     0.31242627478314283
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_949.}
     {?patient care:state care:state_10.}
     action:take_pill_Medication_949
     "P1D"^^xsd:dayTimeDuration
     63
     0.48164465651349536
     0.9342100487964458
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_950.}
     {?patient care:state care:state_11.}
     action:take_pill_Medication_950
     "P1D"^^xsd:dayTimeDuration
     48
     0.7889636017274226
     0.7469090902982602
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_951.}
     {?patient care:state care:state_15.}
     action:take_pill_Medication_951
     "P1D"^^xsd:dayTimeDuration
     59
     0.6592862126750048
     0.6030598912259216
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_952.}
     {?patient care:state care:state_7.}
     action:take_pill_Medication_952
     "P1D"^^xsd:dayTimeDuration
     91
     0.8558334151222247
     0.7909370259033427
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_953.}
     {?patient care:state care:state_8.}
     action:take_pill_Medication_953
     "P1D"^^xsd:dayTimeDuration
     29
     0.8059391549376855
     0.6848718499036446
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_954.}
     {?patient care:state care:state_3.}
     action:take_pill_Medication_954
     "P1D"^^xsd:dayTimeDuration
     90
     0.8318802693128228
     0.5254653059534657
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_955.}
     {?patient care:state care:state_12.}
     action:take_pill_Medication_955
     "P1D"^^xsd:dayTimeDuration
     74
     0.5979732523000909
     0.717347135991532
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_956.}
     {?patient care:state care:state_5.}
     action:take_pill_Medication_956
     "P1D"^^xsd:dayTimeDuration
     95
     0.5247703587542051
     0.6984844302299857
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_957.}
     {?patient care:state care:state_8.}
     action:take_pill_Medication_957
     "P1D"^^xsd:dayTimeDuration
     27
     0.6182438281381818
     0.9438518642093104
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_958.}
     {?patient care:state care:state_10.}
     action:take_pill_Medication_958
     "P1D"^^xsd:dayTimeDuration
     10
     0.9230056874162343
     0.5423924303539728
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_959.}
     {?patient care:state care:state_7.}
     action:take_pill_Medication_959
     "P1D"^^xsd:dayTimeDuration
     2
     0.6500140256398218
     0.9486507299105016
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_960.}
     {?patient care:state care:state_7.}
     action:take_pill_Medication_960
     "P1D"^^xsd:dayTimeDuration
     63
     0.5934972683546891
     0.8280401981762875
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_961.}
     {?patient care:state care:state_7.}
     action:take_pill_Medication_961
     "P1D"^^xsd:dayTimeDuration
     35
     0.6570316638106296
     0.9976424226461933
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_962.}
     {?patient care:state care:state_10.}
     action:take_pill_Medication_962
     "P1D"^^xsd:dayTimeDuration
     66
     0.6395970913850624
     0.6164304637064096
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_963.}
     {?patient care:state care:state_14.}
     action:take_pill_Medication_963
     "P1D"^^xsd:dayTimeDuration
     73
     0.8858590391234137
     0.4002462361051711
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_964.}
     {?patient care:state care:state_14.}
     action:take_pill_Medication_964
     "P1D"^^xsd:dayTimeDuration
     8
     0.40992023049563886
     0.4398266521774665
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_965.}
     {?patient care:state care:state_3.}
     action:take_pill_Medication_965
     "P1D"^^xsd:dayTimeDuration
     11
     0.5757496509071782
     0.38708679995951545
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_966.}
     {?patient care:state care:state_12.}
     action:take_pill_Medication_966
     "P1D"^^xsd:dayTimeDuration
     59
     0.6779679720500279
     0.7851489546034724
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_967.}
     {?patient care:state care:state_19.}
     action:take_pill_Medication_967
     "P1D"^^xsd:dayTimeDuration
     15
     0.3359635455429877
     0.8716222114462835
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_968.}
     {?patient care:state care:state_7.}
     action:take_pill_Medication_968
     "P1D"^^xsd:dayTimeDuration
     91
     0.34855637480788493
     0.7335263576011047
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_969.}
     {?patient care:state care:state_20.}
     action:take_pill_Medication_969
     "P1D"^^xsd:dayTimeDuration
     12
     0.8544782306261149
     0.6841911461249398
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_970.}
     {?patient care:state care:state_17.}
     action:take_pill_Medication_970
     "P1D"^^xsd:dayTimeDuration
     6
     0.6719297904958407
     0.5969641355337
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_971.}
     {?patient care:state care:state_19.}
     action:take_pill_Medication_971
     "P1D"^^xsd:dayTimeDuration
     97
     0.6183103379004483
     0.504118788841172
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_972.}
     {?patient care:state care:state_14.}
     action:take_pill_Medication_972
     "P1D"^^xsd:dayTimeDuration
     27
     0.8083706635004604
     0.7119973155724237
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_973.}
     {?patient care:state care:state_4.}
     action:take_pill_Medication_973
     "P1D"^^xsd:dayTimeDuration
     7
     0.6788981271769622
     0.9203806942222348
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_974.}
     {?patient care:state care:state_14.}
     action:take_pill_Medication_974
     "P1D"^^xsd:dayTimeDuration
     25
     0.4331157717010492
     0.9055863452106476
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_975.}
     {?patient care:state care:state_20.}
     action:take_pill_Medication_975
     "P1D"^^xsd:dayTimeDuration
     2
     0.814438548896518
     0.6480382594519144
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_976.}
     {?patient care:state care:state_16.}
     action:take_pill_Medication_976
     "P1D"^^xsd:dayTimeDuration
     25
     0.46931109689852846
     0.47880718971744385
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_977.}
     {?patient care:state care:state_17.}
     action:take_pill_Medication_977
     "P1D"^^xsd:dayTimeDuration
     98
     0.5906744591472054
     0.5760819559817849
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_978.}
     {?patient care:state care:state_6.}
     action:take_pill_Medication_978
     "P1D"^^xsd:dayTimeDuration
     92
     0.28832444105410693
     0.291137877669357
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_979.}
     {?patient care:state care:state_10.}
     action:take_pill_Medication_979
     "P1D"^^xsd:dayTimeDuration
     15
     0.3436246628765895
     0.6661179060079325
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_980.}
     {?patient care:state care:state_10.}
     action:take_pill_Medication_980
     "P1D"^^xsd:dayTimeDuration
     57
     0.3529578597197452
     0.5925872472571561
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_981.}
     {?patient care:state care:state_8.}
     action:take_pill_Medication_981
     "P1D"^^xsd:dayTimeDuration
     78
     0.46917657025470993
     0.713678621086852
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_982.}
     {?patient care:state care:state_14.}
     action:take_pill_Medication_982
     "P1D"^^xsd:dayTimeDuration
     5
     0.339702514665119
     0.8404557914177224
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_983.}
     {?patient care:state care:state_16.}
     action:take_pill_Medication_983
     "P1D"^^xsd:dayTimeDuration
     86
     0.5770407134909257
     0.33376972295281687
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_984.}
     {?patient care:state care:state_18.}
     action:take_pill_Medication_984
     "P1D"^^xsd:dayTimeDuration
     93
     0.8036035657705649
     0.6496244879475729
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_985.}
     {?patient care:state care:state_11.}
     action:take_pill_Medication_985
     "P1D"^^xsd:dayTimeDuration
     47
     0.38866311531891745
     0.6267055018121068
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_986.}
     {?patient care:state care:state_14.}
     action:take_pill_Medication_986
     "P1D"^^xsd:dayTimeDuration
     54
     0.4438305385598508
     0.7377606605998792
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_987.}
     {?patient care:state care:state_11.}
     action:take_pill_Medication_987
     "P1D"^^xsd:dayTimeDuration
     84
     0.3256024033173244
     0.8193905078925906
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_988.}
     {?patient care:state care:state_7.}
     action:take_pill_Medication_988
     "P1D"^^xsd:dayTimeDuration
     33
     0.7991455458536176
     0.9118548431872995
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_989.}
     {?patient care:state care:state_10.}
     action:take_pill_Medication_989
     "P1D"^^xsd:dayTimeDuration
     60
     0.5983712455282136
     0.3934145175007301
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_990.}
     {?patient care:state care:state_16.}
     action:take_pill_Medication_990
     "P1D"^^xsd:dayTimeDuration
     36
     0.32082702083297066
     0.6472494330249163
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_991.}
     {?patient care:state care:state_12.}
     action:take_pill_Medication_991
     "P1D"^^xsd:dayTimeDuration
     60
     0.9755768120661227
     0.9901866823854704
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_992.}
     {?patient care:state care:state_19.}
     action:take_pill_Medication_992
     "P1D"^^xsd:dayTimeDuration
     92
     0.6966957833574913
     0.7583436029188911
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_993.}
     {?patient care:state care:state_12.}
     action:take_pill_Medication_993
     "P1D"^^xsd:dayTimeDuration
     26
     0.879970087906774
     0.885681417028134
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_994.}
     {?patient care:state care:state_3.}
     action:take_pill_Medication_994
     "P1D"^^xsd:dayTimeDuration
     93
     0.6018988025048732
     0.8941009980200615
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_995.}
     {?patient care:state care:state_16.}
     action:take_pill_Medication_995
     "P1D"^^xsd:dayTimeDuration
     97
     0.6844437860264231
     0.8598591581269348
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_996.}
     {?patient care:state care:state_9.}
     action:take_pill_Medication_996
     "P1D"^^xsd:dayTimeDuration
     49
     0.5009454920121625
     0.5586137908394795
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_997.}
     {?patient care:state care:state_6.}
     action:take_pill_Medication_997
     "P1D"^^xsd:dayTimeDuration
     63
     0.3351705569482052
     0.9208284882669985
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_998.}
     {?patient care:state care:state_14.}
     action:take_pill_Medication_998
     "P1D"^^xsd:dayTimeDuration
     91
     0.7577027344591687
     0.6115598021473914
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_999.}
     {?patient care:state care:state_11.}
     action:take_pill_Medication_999
     "P1D"^^xsd:dayTimeDuration
     35
     0.6708138565894357
     0.7727623352952387
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.
""").strip()

if not N3_RULES:
    raise RuntimeError("⚠️  You forgot to paste the N3 rules into N3_RULES!")

# ╔═══════════════════════════════════════╗
# ║ 3.  RULE STRUCTURE                    ║
# ╚═══════════════════════════════════════╝
# After parsing each gps:description we store its content
# in a lightweight dataclass so the search engine can use it
# like a normal Python object.

@dataclass(frozen=True)
class TransitionRule:
    """One weighted transition edge in the state space."""
    from_state : str
    to_state   : str
    action     : str
    duration_d : int          # whole days; typically 1
    cost       : float        # euros (or any currency)
    success_p  : float        # ∈ (0,1]
    comfort_p  : float        # ∈ (0,1]
    condition  : Callable[[Patient], bool]  # extra filters

    # Convenience: check if the rule can fire *now* for patient p
    def applies(self, p: Patient, cur_state: str) -> bool:
        return cur_state == self.from_state and self.condition(p)

# ╔═══════════════════════════════════════╗
# ║ 4.  RULE PARSER (very tolerant)       ║
# ╚═══════════════════════════════════════╝

RULE_RE = re.compile(
    r"""{\s*care:Parkinson\s+gps:description\s+\(\s*
        \{\?patient\s+care:state\s+care:(state_\d+)\.\}\s*         # FROM
        \{\?patient\s+gps:medication\s+medication:(Medication_\d+)\.\}\s*
        \{\?patient\s+care:state\s+care:(state_\d+)\.\}\s*         # TO
        action:([A-Za-z0-9_]+)\s*
        "P(\d+)D"\^\^xsd:dayTimeDuration\s*                       # duration
        (\d+)\s*                                                  # cost
        ([0-9.]+)\s*                                              # success
        ([0-9.]+)\s*                                              # comfort
        \)\s*\}\s*<=\s*\{\s*
        (?P<body>.*?)\}\s*\.\s*""",
    re.S | re.X,
)

GENDER_RE = re.compile(r"care:gender\s+care:(Male|Female)")
MINAGE_RE = re.compile(r"math:greaterThan\s+(\d+)")

def _parse_rules(n3: str) -> List[TransitionRule]:
    """Convert each gps:description {...} <= {...}. into a TransitionRule."""
    rules: List[TransitionRule] = []

    def _make_condition(gender: str | None, min_age: int | None):
        """Return a *callable* that tests a patient object."""
        return lambda p: (gender is None or p.gender == gender) and \
                         (min_age is None or p.age > min_age)

    for m in RULE_RE.finditer(n3):
        from_state, _med, to_state, action, dur, cost, succ, comfort, body = m.groups()

        gender: str | None = None
        min_age: int | None = None

        g = GENDER_RE.search(body)
        if g:
            gender = g.group(1)

        a = MINAGE_RE.search(body)
        if a:
            min_age = int(a.group(1))

        rules.append(
            TransitionRule(
                from_state  = from_state,
                to_state    = to_state,
                action      = action,
                duration_d  = int(dur),
                cost        = float(cost),
                success_p   = float(succ),
                comfort_p   = float(comfort),
                condition   = _make_condition(gender, min_age),
            )
        )

    return rules

# Parse immediately so that a failure is obvious on start-up
RULES = _parse_rules(N3_RULES)
print(f"→ Parsed {len(RULES)} transition rules from the N3 block.\n")
if len(RULES) == 0:
    raise RuntimeError("Parsed 0 rules. Did you paste the full N3 block into N3_RULES?")

# ╔═══════════════════════════════════════╗
# ║ 5.  SEARCH PARAMETERS (from query)    ║
# ╚═══════════════════════════════════════╝
GOAL_STATE      = "state_6"
MAX_DURATION_D  = 180      # "P180D"  – days
MAX_COST        = 500.0
MIN_SUCCESS_P   = 0.35
MIN_COMFORT_P   = 0.35
MAX_STAGECOUNT  = 10       # gps:stagecount limit

# ╔═══════════════════════════════════════╗
# ║ 6.  SEARCH NODE DEFINITION            ║
# ╚═══════════════════════════════════════╝
@dataclass
class Node:
    """One *partial* path in the BFS fringe."""
    state   : str
    duration: int          # total days so far
    cost    : float
    succ    : float
    comfort : float
    path    : List[str]    # ordered list of actions taken

# ╔═══════════════════════════════════════╗
# ║ 7.  LOOP-FREE, NO-PRUNING BFS         ║
# ╚═══════════════════════════════════════╝
def search_all(p: Patient) -> List[Node]:
    """
    Enumerate *every* distinct action sequence (up to MAX_STAGECOUNT)
    that satisfies the four numeric limits and ends in GOAL_STATE.
    """
    start = Node(p.state, duration=0, cost=0.0, succ=1.0, comfort=1.0, path=[])
    queue : Deque[Node] = deque([start])
    sols  : List[Node]  = []

    # prevent literal loops, but retain distinct action-lists
    visited: set[Tuple[str, Tuple[str, ...]]] = set()

    while queue:
        cur = queue.popleft()

        if cur.state == GOAL_STATE:
            sols.append(cur)
            # don't continue; longer non-looping paths may exist

        if len(cur.path) >= MAX_STAGECOUNT:
            continue

        for rule in RULES:
            if not rule.applies(p=patient_1, cur_state=cur.state):
                continue

            nd = cur.duration + rule.duration_d
            nc = cur.cost     + rule.cost
            ns = cur.succ     * rule.success_p
            nf = cur.comfort  * rule.comfort_p

            if not (nd <= MAX_DURATION_D and nc <= MAX_COST and ns >= MIN_SUCCESS_P and nf >= MIN_COMFORT_P):
                continue

            nxt_path = tuple(cur.path + [rule.action])
            sig = (rule.to_state, nxt_path)
            if sig in visited:
                continue
            visited.add(sig)

            queue.append(Node(rule.to_state, nd, nc, ns, nf, list(nxt_path)))

    return sols

# ╔═══════════════════════════════════════╗
# ║ 8.  PRETTY PRINTING OF RESULTS        ║
# ╚═══════════════════════════════════════╝
def print_solutions(sols: List[Node]) -> None:
    """Human-friendly table of every valid path."""
    if not sols:
        print("⛔  No path satisfies the limits.")
        return

    # sort: 1) highest success, 2) lowest cost, 3) shortest duration
    sols.sort(key=lambda n: (-n.succ, n.cost, n.duration))

    print(f"✅  Found {len(sols)} solution path(s):\n")
    for idx, n in enumerate(sols, 1):
        print(f"Solution #{idx}")
        print(f"  Steps      : {len(n.path)}  (≤ {MAX_STAGECOUNT})")
        print(f"  Duration   : {n.duration} day(s)  (≤ {MAX_DURATION_D})")
        print(f"  Cost       : {n.cost:.2f}          (≤ {MAX_COST})")
        print(f"  Success P  : {n.succ:.3f}          (≥ {MIN_SUCCESS_P})")
        print(f"  Comfort P  : {n.comfort:.3f}       (≥ {MIN_COMFORT_P})")
        for i, act in enumerate(n.path, 1):
            print(f"     {i}. {act}")
        print(f"  Final state: {n.state}\n")

# ╔═══════════════════════════════════════╗
# ║ 9.  ARC OUTPUT — Answer / Reason why / Check (harness)
# ╚═══════════════════════════════════════╝
def applicable_from_state(p: Patient, state: str) -> List[TransitionRule]:
    return [r for r in RULES if r.applies(p, state)]

def replay_totals(p: Patient, actions: List[str]) -> Tuple[int,float,float,float,str]:
    """Recompute totals by replaying actions from p.state (first matching rule each step)."""
    idx = {}
    for r in RULES:
        idx.setdefault((r.from_state, r.action), []).append(r)
    cur = p.state
    dur=0; cost=0.0; succ=1.0; comf=1.0
    for a in actions:
        cand = [r for r in idx.get((cur, a), []) if r.applies(p, cur)]
        if not cand:
            raise RuntimeError(f"Action {a} not applicable from state {cur}")
        r = cand[0]
        cur = r.to_state
        dur += r.duration_d; cost += r.cost; succ *= r.success_p; comf *= r.comfort_p
    return dur, cost, succ, comf, cur

# ——— Helper printing for Reason-why (caps output on huge rule sets) ———
def _print_reason_why(p: Patient, start_state: str):
    apps = applicable_from_state(p, start_state)
    print(f"From the start state {start_state}, applicable rules for this patient: {len(apps)}")
    if not apps:
        print("  • None — start is a dead-end for these demographics.\n")
        return

    # Show all one-step goal hits
    one_step_goals = [r for r in apps if r.to_state == GOAL_STATE]
    if one_step_goals:
        print("  One-step routes to the goal:")
        for r in one_step_goals:
            print(f"  • {r.action}: {r.from_state} → {r.to_state}  "
                  f"(+{r.duration_d}d, +€{r.cost:.0f}, ×succ {r.success_p:.3f}, ×comf {r.comfort_p:.3f})")
    else:
        print("  No single step reaches the goal.")

    # Also show up to 20 other applicable first steps as a sample
    others = [r for r in apps if r.to_state != GOAL_STATE]
    if others:
        print("\n  Sample of other applicable first steps (max 20 shown):")
        for r in others[:20]:
            print(f"  • {r.action}: {r.from_state} → {r.to_state}  "
                  f"(+{r.duration_d}d, +€{r.cost:.0f}, ×succ {r.success_p:.3f}, ×comf {r.comfort_p:.3f})")
    print()

# ──────────────────────────────────────────────────────────────
# 10.  CHECK — harness (multi-solution, prints result)
# ──────────────────────────────────────────────────────────────

def _approx(a, b, tol=1e-12):
    d = a - b
    if d < 0: d = -d
    return d <= tol * max(1.0, 1.0 + (a if a>=0 else -a) + (b if b>=0 else -b))

def _leq_by_key(a: Node, b: Node, tol=1e-12):
    # ordering key: (-succ, cost, duration)
    if a.succ > b.succ + tol: return True
    if b.succ > a.succ + tol: return False
    if a.cost < b.cost - tol: return True
    if b.cost < a.cost - tol: return False
    return a.duration <= b.duration + tol

def _replay_runs(patient: Patient, actions: List[str]):
    """Enumerate all feasible (end, dur, cost, succ, comf) runs following `actions` and staying within limits."""
    idx = {}
    for r in RULES:
        idx.setdefault((r.from_state, r.action), []).append(r)

    runs = []
    def dfs(i, cur_state, dur, cost, succ, comf):
        if i == len(actions):
            runs.append((cur_state, dur, cost, succ, comf))
            return
        a = actions[i]
        for r in idx.get((cur_state, a), []):
            if not r.applies(patient, cur_state):
                continue
            nd = dur  + r.duration_d
            nc = cost + r.cost
            ns = succ * r.success_p
            nf = comf * r.comfort_p
            if not (nd <= MAX_DURATION_D and nc <= MAX_COST and ns >= MIN_SUCCESS_P and nf >= MIN_COMFORT_P):
                continue
            dfs(i+1, r.to_state, nd, nc, ns, nf)

    dfs(0, patient.state, 0, 0.0, 1.0, 1.0)
    return runs

def harness(solutions: List[Node]) -> None:
    assert solutions, "Expected at least one solution."

    # 1) uniqueness of action lists
    paths = [tuple(n.path) for n in solutions]
    assert len(set(paths)) == len(paths), "Duplicate action lists found."

    # 2) every solution ends at goal, stays within limits, has length ≤ MAX_STAGECOUNT
    for n in solutions:
        assert n.state == GOAL_STATE, f"Wrong end state for {n.path}"
        assert len(n.path) <= MAX_STAGECOUNT, f"Path too long: {n.path}"
        assert n.duration <= MAX_DURATION_D and n.cost <= MAX_COST \
               and n.succ >= MIN_SUCCESS_P and n.comfort >= MIN_COMFORT_P, \
               f"Global limits violated by {n.path}"

    # 3) replay each path against rules; at least one consistent run must match the recorded totals
    for n in solutions:
        runs = _replay_runs(patient_1, n.path)
        assert runs, f"No feasible run for actions {n.path}"
        matched = False
        for end, D, C, S, F in runs:
            if end == n.state and _approx(D, n.duration) and _approx(C, n.cost, 1e-9) \
               and _approx(S, n.succ) and _approx(F, n.comfort):
                matched = True
                break
        assert matched, f"Recorded totals don’t match any feasible replay for {n.path}"

    # 4) verify sorting (non-decreasing by the key you specified)
    for a, b in zip(solutions, solutions[1:]):
        assert _leq_by_key(a, b), "Solutions are not sorted by (success↓, cost↑, duration↑)."

    # 5) verify that the first item is optimal by that key
    best = solutions[0]
    for n in solutions[1:]:
        assert _leq_by_key(best, n), "First solution is not optimal under the specified ordering."

    print(f"Harness: {len(solutions)} solutions verified — "
          "uniqueness, constraints, replay totals, and ordering. ✓")

# ╔═══════════════════════════════════════╗
# ║ 11.  MAIN ─ RUN EVERYTHING            ║
# ╚═══════════════════════════════════════╝
if __name__ == "__main__":
    print("============================================")
    print("GPS (N3 rules) — Answer / Reason why / Check (harness)")
    print("============================================\n")
    print(f"Patient     : {patient_1.name}  (gender={patient_1.gender}, age={patient_1.age})")
    print(f"Start state : {patient_1.state}")
    print(f"Goal state  : {GOAL_STATE}\n")

    # ---------- Answer ----------
    solutions = search_all(patient_1)
    print("Answer")
    print("======")
    print_solutions(solutions)
    if solutions:
        solutions.sort(key=lambda n: (-n.succ, n.cost, n.duration))
        best = solutions[0]
        print(f"Optimal by (succ↓, cost↑, dur↑): path = {best.path}, "
              f"succ={best.succ:.3f}, cost=€{best.cost:.2f}, dur={best.duration}d\n")

    # ---------- Reason why ----------
    print("Reason why")
    print("==========")
    print("Global limits (identical to N3 query):")
    print(f"  duration ≤ {MAX_DURATION_D} days, cost ≤ €{MAX_COST}, "
          f"success ≥ {MIN_SUCCESS_P}, comfort ≥ {MIN_COMFORT_P}, stagecount ≤ {MAX_STAGECOUNT}\n")
    _print_reason_why(patient_1, patient_1.state)

    # ---------- Check (harness) ----------
    print("Check (harness)")
    print("===============")
    harness(solutions)

