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
     {?patient care:state care:state_7.}
     action:take_pill_Medication_0
     "P1D"^^xsd:dayTimeDuration
     33
     0.36622812425783524
     0.9105285094845693
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_1.}
     {?patient care:state care:state_8.}
     action:take_pill_Medication_1
     "P1D"^^xsd:dayTimeDuration
     71
     0.4109054639971658
     0.9453244024828519
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_2.}
     {?patient care:state care:state_3.}
     action:take_pill_Medication_2
     "P1D"^^xsd:dayTimeDuration
     21
     0.2667006994165855
     0.4828436026647903
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_3.}
     {?patient care:state care:state_4.}
     action:take_pill_Medication_3
     "P1D"^^xsd:dayTimeDuration
     72
     0.8983659775611101
     0.8974278483276885
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_4.}
     {?patient care:state care:state_7.}
     action:take_pill_Medication_4
     "P1D"^^xsd:dayTimeDuration
     95
     0.4811965080847377
     0.7115706817095907
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_5.}
     {?patient care:state care:state_2.}
     action:take_pill_Medication_5
     "P1D"^^xsd:dayTimeDuration
     6
     0.4760129877005949
     0.9408962265856359
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_6.}
     {?patient care:state care:state_7.}
     action:take_pill_Medication_6
     "P1D"^^xsd:dayTimeDuration
     55
     0.8380411873055637
     0.9489656353887378
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
     81
     0.32714617352712805
     0.5598075871392556
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_8.}
     {?patient care:state care:state_9.}
     action:take_pill_Medication_8
     "P1D"^^xsd:dayTimeDuration
     93
     0.7273428934572153
     0.8764842159831163
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_9.}
     {?patient care:state care:state_4.}
     action:take_pill_Medication_9
     "P1D"^^xsd:dayTimeDuration
     44
     0.3542891359964912
     0.3617563585375574
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_10.}
     {?patient care:state care:state_7.}
     action:take_pill_Medication_10
     "P1D"^^xsd:dayTimeDuration
     50
     0.7465581555870124
     0.8468873689094923
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_11.}
     {?patient care:state care:state_5.}
     action:take_pill_Medication_11
     "P1D"^^xsd:dayTimeDuration
     93
     0.49933034123520126
     0.7761439171844592
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_12.}
     {?patient care:state care:state_4.}
     action:take_pill_Medication_12
     "P1D"^^xsd:dayTimeDuration
     86
     0.2847174405387392
     0.4415627769821937
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_13.}
     {?patient care:state care:state_9.}
     action:take_pill_Medication_13
     "P1D"^^xsd:dayTimeDuration
     73
     0.4170823217754115
     0.9196817867609735
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_14.}
     {?patient care:state care:state_10.}
     action:take_pill_Medication_14
     "P1D"^^xsd:dayTimeDuration
     95
     0.3778591113622131
     0.2716762154788545
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_15.}
     {?patient care:state care:state_5.}
     action:take_pill_Medication_15
     "P1D"^^xsd:dayTimeDuration
     21
     0.8241910843274908
     0.4907041666316164
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_16.}
     {?patient care:state care:state_3.}
     action:take_pill_Medication_16
     "P1D"^^xsd:dayTimeDuration
     69
     0.4890846698320697
     0.6316470204137609
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_17.}
     {?patient care:state care:state_7.}
     action:take_pill_Medication_17
     "P1D"^^xsd:dayTimeDuration
     46
     0.5185252599418597
     0.7169268089280825
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_18.}
     {?patient care:state care:state_5.}
     action:take_pill_Medication_18
     "P1D"^^xsd:dayTimeDuration
     25
     0.539694571361632
     0.45969772070179227
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_19.}
     {?patient care:state care:state_7.}
     action:take_pill_Medication_19
     "P1D"^^xsd:dayTimeDuration
     75
     0.9467800045918124
     0.6671741423521769
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_20.}
     {?patient care:state care:state_6.}
     action:take_pill_Medication_20
     "P1D"^^xsd:dayTimeDuration
     59
     0.7578277513499754
     0.8332211235886307
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_21.}
     {?patient care:state care:state_6.}
     action:take_pill_Medication_21
     "P1D"^^xsd:dayTimeDuration
     46
     0.3095444912557152
     0.2506963905869005
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_22.}
     {?patient care:state care:state_4.}
     action:take_pill_Medication_22
     "P1D"^^xsd:dayTimeDuration
     53
     0.9452849506102207
     0.5601302643605619
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_23.}
     {?patient care:state care:state_7.}
     action:take_pill_Medication_23
     "P1D"^^xsd:dayTimeDuration
     86
     0.26713433449707746
     0.6807107071018563
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_24.}
     {?patient care:state care:state_7.}
     action:take_pill_Medication_24
     "P1D"^^xsd:dayTimeDuration
     69
     0.5832623950218775
     0.6659526845754201
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_25.}
     {?patient care:state care:state_9.}
     action:take_pill_Medication_25
     "P1D"^^xsd:dayTimeDuration
     96
     0.46182380397343115
     0.693595669246886
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_26.}
     {?patient care:state care:state_8.}
     action:take_pill_Medication_26
     "P1D"^^xsd:dayTimeDuration
     15
     0.7202956346616978
     0.5255495236333481
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_27.}
     {?patient care:state care:state_9.}
     action:take_pill_Medication_27
     "P1D"^^xsd:dayTimeDuration
     66
     0.8918564765637071
     0.6705904632262046
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_28.}
     {?patient care:state care:state_9.}
     action:take_pill_Medication_28
     "P1D"^^xsd:dayTimeDuration
     7
     0.9295344288615968
     0.2651476556272499
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_29.}
     {?patient care:state care:state_6.}
     action:take_pill_Medication_29
     "P1D"^^xsd:dayTimeDuration
     53
     0.5174283479609647
     0.6670276236780329
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_30.}
     {?patient care:state care:state_6.}
     action:take_pill_Medication_30
     "P1D"^^xsd:dayTimeDuration
     63
     0.5419738792066486
     0.5694769543950723
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_31.}
     {?patient care:state care:state_9.}
     action:take_pill_Medication_31
     "P1D"^^xsd:dayTimeDuration
     88
     0.2721316442064574
     0.6656967035496009
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_32.}
     {?patient care:state care:state_9.}
     action:take_pill_Medication_32
     "P1D"^^xsd:dayTimeDuration
     24
     0.7763665903747188
     0.8363705740740794
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_33.}
     {?patient care:state care:state_8.}
     action:take_pill_Medication_33
     "P1D"^^xsd:dayTimeDuration
     69
     0.7895387907035833
     0.7580015159330282
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_34.}
     {?patient care:state care:state_6.}
     action:take_pill_Medication_34
     "P1D"^^xsd:dayTimeDuration
     24
     0.46787067270496185
     0.33475729294729756
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
     {?patient care:state care:state_9.}
     action:take_pill_Medication_35
     "P1D"^^xsd:dayTimeDuration
     68
     0.4032572059139633
     0.6785702721172662
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_36.}
     {?patient care:state care:state_6.}
     action:take_pill_Medication_36
     "P1D"^^xsd:dayTimeDuration
     50
     0.9268490403769082
     0.8698425574812589
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_37.}
     {?patient care:state care:state_5.}
     action:take_pill_Medication_37
     "P1D"^^xsd:dayTimeDuration
     27
     0.44012110893786305
     0.6688101921455137
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_38.}
     {?patient care:state care:state_6.}
     action:take_pill_Medication_38
     "P1D"^^xsd:dayTimeDuration
     47
     0.5816186372967517
     0.27475832022439395
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_39.}
     {?patient care:state care:state_8.}
     action:take_pill_Medication_39
     "P1D"^^xsd:dayTimeDuration
     37
     0.9291841201163343
     0.47138869116021415
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_40.}
     {?patient care:state care:state_6.}
     action:take_pill_Medication_40
     "P1D"^^xsd:dayTimeDuration
     98
     0.9118052232880963
     0.8540709410132148
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_41.}
     {?patient care:state care:state_9.}
     action:take_pill_Medication_41
     "P1D"^^xsd:dayTimeDuration
     94
     0.45824264967729045
     0.8795238969840418
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_42.}
     {?patient care:state care:state_4.}
     action:take_pill_Medication_42
     "P1D"^^xsd:dayTimeDuration
     52
     0.7112584943929599
     0.38933126372293614
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_43.}
     {?patient care:state care:state_6.}
     action:take_pill_Medication_43
     "P1D"^^xsd:dayTimeDuration
     82
     0.33977279290684453
     0.7296797573623686
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_44.}
     {?patient care:state care:state_3.}
     action:take_pill_Medication_44
     "P1D"^^xsd:dayTimeDuration
     31
     0.9664115123865664
     0.6548338732424491
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_45.}
     {?patient care:state care:state_4.}
     action:take_pill_Medication_45
     "P1D"^^xsd:dayTimeDuration
     36
     0.5082797767694324
     0.617512159257882
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_46.}
     {?patient care:state care:state_4.}
     action:take_pill_Medication_46
     "P1D"^^xsd:dayTimeDuration
     29
     0.45316207709279827
     0.5639411380607756
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_47.}
     {?patient care:state care:state_5.}
     action:take_pill_Medication_47
     "P1D"^^xsd:dayTimeDuration
     87
     0.8812755817817808
     0.3262195982209329
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_48.}
     {?patient care:state care:state_8.}
     action:take_pill_Medication_48
     "P1D"^^xsd:dayTimeDuration
     7
     0.5990081572047221
     0.47505511334049155
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_49.}
     {?patient care:state care:state_6.}
     action:take_pill_Medication_49
     "P1D"^^xsd:dayTimeDuration
     44
     0.37442865259058555
     0.4504020894316335
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_50.}
     {?patient care:state care:state_7.}
     action:take_pill_Medication_50
     "P1D"^^xsd:dayTimeDuration
     60
     0.430012691264264
     0.8129634990985635
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_51.}
     {?patient care:state care:state_10.}
     action:take_pill_Medication_51
     "P1D"^^xsd:dayTimeDuration
     70
     0.2974759353567495
     0.2504729278633666
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_52.}
     {?patient care:state care:state_8.}
     action:take_pill_Medication_52
     "P1D"^^xsd:dayTimeDuration
     57
     0.9293648964066066
     0.7693860210154684
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
     {?patient care:state care:state_5.}
     action:take_pill_Medication_53
     "P1D"^^xsd:dayTimeDuration
     88
     0.40838649735552035
     0.3912993291420428
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_54.}
     {?patient care:state care:state_9.}
     action:take_pill_Medication_54
     "P1D"^^xsd:dayTimeDuration
     18
     0.2892144963864729
     0.2630316991483213
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_55.}
     {?patient care:state care:state_3.}
     action:take_pill_Medication_55
     "P1D"^^xsd:dayTimeDuration
     2
     0.5406818163743916
     0.8769645179828887
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_56.}
     {?patient care:state care:state_10.}
     action:take_pill_Medication_56
     "P1D"^^xsd:dayTimeDuration
     23
     0.6448835223376228
     0.3714206313238681
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_57.}
     {?patient care:state care:state_8.}
     action:take_pill_Medication_57
     "P1D"^^xsd:dayTimeDuration
     79
     0.2825981320281489
     0.6782395222649295
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_58.}
     {?patient care:state care:state_10.}
     action:take_pill_Medication_58
     "P1D"^^xsd:dayTimeDuration
     87
     0.6896026197362309
     0.5496883142578176
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_59.}
     {?patient care:state care:state_7.}
     action:take_pill_Medication_59
     "P1D"^^xsd:dayTimeDuration
     99
     0.8234307459132633
     0.5561968077334533
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_60.}
     {?patient care:state care:state_5.}
     action:take_pill_Medication_60
     "P1D"^^xsd:dayTimeDuration
     12
     0.7230107215159753
     0.849974189581846
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_61.}
     {?patient care:state care:state_9.}
     action:take_pill_Medication_61
     "P1D"^^xsd:dayTimeDuration
     74
     0.5301281686565069
     0.5363482832535775
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_62.}
     {?patient care:state care:state_4.}
     action:take_pill_Medication_62
     "P1D"^^xsd:dayTimeDuration
     76
     0.42493515077755095
     0.8050917532044635
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_63.}
     {?patient care:state care:state_4.}
     action:take_pill_Medication_63
     "P1D"^^xsd:dayTimeDuration
     70
     0.44538275310075803
     0.29665038154811013
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_64.}
     {?patient care:state care:state_3.}
     action:take_pill_Medication_64
     "P1D"^^xsd:dayTimeDuration
     95
     0.9143849910742256
     0.7596016670310102
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_65.}
     {?patient care:state care:state_5.}
     action:take_pill_Medication_65
     "P1D"^^xsd:dayTimeDuration
     21
     0.2602972715462068
     0.5527001971259099
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_66.}
     {?patient care:state care:state_9.}
     action:take_pill_Medication_66
     "P1D"^^xsd:dayTimeDuration
     2
     0.5792304880591905
     0.5720726834942171
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_67.}
     {?patient care:state care:state_3.}
     action:take_pill_Medication_67
     "P1D"^^xsd:dayTimeDuration
     15
     0.47135669469579733
     0.5464686700197526
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_68.}
     {?patient care:state care:state_7.}
     action:take_pill_Medication_68
     "P1D"^^xsd:dayTimeDuration
     57
     0.8098018788684327
     0.26016480453245916
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_69.}
     {?patient care:state care:state_5.}
     action:take_pill_Medication_69
     "P1D"^^xsd:dayTimeDuration
     32
     0.7319024844264351
     0.7587677103850011
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_70.}
     {?patient care:state care:state_7.}
     action:take_pill_Medication_70
     "P1D"^^xsd:dayTimeDuration
     79
     0.9091641134517952
     0.7259140822690493
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_71.}
     {?patient care:state care:state_7.}
     action:take_pill_Medication_71
     "P1D"^^xsd:dayTimeDuration
     30
     0.853539206389353
     0.5327050657958544
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_72.}
     {?patient care:state care:state_7.}
     action:take_pill_Medication_72
     "P1D"^^xsd:dayTimeDuration
     17
     0.9471034448546852
     0.46008000623426804
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_73.}
     {?patient care:state care:state_3.}
     action:take_pill_Medication_73
     "P1D"^^xsd:dayTimeDuration
     80
     0.3556382493653935
     0.8462729174500453
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_74.}
     {?patient care:state care:state_10.}
     action:take_pill_Medication_74
     "P1D"^^xsd:dayTimeDuration
     21
     0.9547353523946092
     0.9054813468707963
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_75.}
     {?patient care:state care:state_8.}
     action:take_pill_Medication_75
     "P1D"^^xsd:dayTimeDuration
     79
     0.5163399300795812
     0.9547039749351954
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_76.}
     {?patient care:state care:state_10.}
     action:take_pill_Medication_76
     "P1D"^^xsd:dayTimeDuration
     97
     0.4050095984421735
     0.998259459874103
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_77.}
     {?patient care:state care:state_4.}
     action:take_pill_Medication_77
     "P1D"^^xsd:dayTimeDuration
     76
     0.32681757192622407
     0.5238863609706024
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_78.}
     {?patient care:state care:state_4.}
     action:take_pill_Medication_78
     "P1D"^^xsd:dayTimeDuration
     57
     0.5905551437762169
     0.4287990808103498
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_79.}
     {?patient care:state care:state_10.}
     action:take_pill_Medication_79
     "P1D"^^xsd:dayTimeDuration
     3
     0.7138727137966268
     0.4121441890384293
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_80.}
     {?patient care:state care:state_7.}
     action:take_pill_Medication_80
     "P1D"^^xsd:dayTimeDuration
     82
     0.404548384966258
     0.7240340439972839
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
     {?patient care:state care:state_5.}
     action:take_pill_Medication_81
     "P1D"^^xsd:dayTimeDuration
     63
     0.626292730447538
     0.7849467968580127
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_82.}
     {?patient care:state care:state_4.}
     action:take_pill_Medication_82
     "P1D"^^xsd:dayTimeDuration
     15
     0.3556335323596853
     0.7855400578837036
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_83.}
     {?patient care:state care:state_3.}
     action:take_pill_Medication_83
     "P1D"^^xsd:dayTimeDuration
     39
     0.5810674356418531
     0.653901705174511
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_84.}
     {?patient care:state care:state_5.}
     action:take_pill_Medication_84
     "P1D"^^xsd:dayTimeDuration
     3
     0.31985659381603493
     0.49837230638613483
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_85.}
     {?patient care:state care:state_8.}
     action:take_pill_Medication_85
     "P1D"^^xsd:dayTimeDuration
     83
     0.6205255797923125
     0.5888528269865236
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_86.}
     {?patient care:state care:state_10.}
     action:take_pill_Medication_86
     "P1D"^^xsd:dayTimeDuration
     64
     0.6416639392843279
     0.2854501885998023
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_87.}
     {?patient care:state care:state_4.}
     action:take_pill_Medication_87
     "P1D"^^xsd:dayTimeDuration
     6
     0.6148542877364822
     0.2695941409001358
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_88.}
     {?patient care:state care:state_7.}
     action:take_pill_Medication_88
     "P1D"^^xsd:dayTimeDuration
     55
     0.4198704288367975
     0.8151508039057901
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_89.}
     {?patient care:state care:state_4.}
     action:take_pill_Medication_89
     "P1D"^^xsd:dayTimeDuration
     95
     0.6644808485966249
     0.8452118135702289
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_90.}
     {?patient care:state care:state_8.}
     action:take_pill_Medication_90
     "P1D"^^xsd:dayTimeDuration
     54
     0.8800953376910678
     0.2648739180793569
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_91.}
     {?patient care:state care:state_8.}
     action:take_pill_Medication_91
     "P1D"^^xsd:dayTimeDuration
     12
     0.59746543117848
     0.7906909329336878
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_92.}
     {?patient care:state care:state_8.}
     action:take_pill_Medication_92
     "P1D"^^xsd:dayTimeDuration
     20
     0.8940411958789706
     0.39690492771487595
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_93.}
     {?patient care:state care:state_4.}
     action:take_pill_Medication_93
     "P1D"^^xsd:dayTimeDuration
     35
     0.8333988608062796
     0.856765447110615
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_94.}
     {?patient care:state care:state_7.}
     action:take_pill_Medication_94
     "P1D"^^xsd:dayTimeDuration
     91
     0.39411841253259794
     0.42511448918375516
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_95.}
     {?patient care:state care:state_6.}
     action:take_pill_Medication_95
     "P1D"^^xsd:dayTimeDuration
     58
     0.8396088438130145
     0.7646744360226699
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_96.}
     {?patient care:state care:state_5.}
     action:take_pill_Medication_96
     "P1D"^^xsd:dayTimeDuration
     17
     0.2819812900212057
     0.27516470230173884
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_1.}
     {?patient gps:medication medication:Medication_97.}
     {?patient care:state care:state_10.}
     action:take_pill_Medication_97
     "P1D"^^xsd:dayTimeDuration
     7
     0.7959058155466046
     0.38590947382197344
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Female.
     ?patient care:age ?age.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_3.}
     {?patient gps:medication medication:Medication_98.}
     {?patient care:state care:state_10.}
     action:take_pill_Medication_98
     "P1D"^^xsd:dayTimeDuration
     32
     0.9228053737781376
     0.46333128972472715
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
     ?patient care:age ?age.
     ?age math:greaterThan 18.
 }.

{care:Parkinson gps:description (
     {?patient care:state care:state_2.}
     {?patient gps:medication medication:Medication_99.}
     {?patient care:state care:state_4.}
     action:take_pill_Medication_99
     "P1D"^^xsd:dayTimeDuration
     53
     0.3999582722477937
     0.6189658161322502
     )
} <= {
     ?patient a care:Patient.
     ?patient care:gender care:Male.
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
    duration_d : int          # whole days; always 1 in examples
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
# We extract fields with one big regex.  If your rules evolve,
# just tweak the regular expression.

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

# ╔═══════════════════════════════════════╗
# ║ 5.  SEARCH PARAMETERS (from query)    ║
# ╚═══════════════════════════════════════╝
GOAL_STATE      = "state_6"
MAX_DURATION_D  = 180      # "P180D"  –  180 × 24 × 3600 seconds
MAX_COST        = 500.0
MIN_SUCCESS_P   = 0.35
MIN_COMFORT_P   = 0.35
MAX_STAGECOUNT  = 10       # taken from gps:stagecount limit

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

    # visited keeps pairs (state, tuple(action-list)) to prevent
    # infinite A→B→A loops but **never** discards alternative paths
    # that merely differ in actions taken.
    visited: set[Tuple[str, Tuple[str, ...]]] = set()

    while queue:
        cur = queue.popleft()

        if cur.state == GOAL_STATE:
            sols.append(cur)
            # Do NOT 'continue'; longer non-looping paths may exist.

        # stage count –  identical to the N3 gps:stagecount limit
        if len(cur.path) >= MAX_STAGECOUNT:
            continue

        for rule in RULES:
            if not rule.applies(p, cur.state):
                continue

            # --- accumulate metrics ---
            nxt_dur = cur.duration + rule.duration_d
            nxt_cos = cur.cost     + rule.cost
            nxt_suc = cur.succ     * rule.success_p
            nxt_com = cur.comfort  * rule.comfort_p

            # --- global constraint check (same as the N3 query) ---
            if not (nxt_dur <= MAX_DURATION_D and
                    nxt_cos <= MAX_COST and
                    nxt_suc >= MIN_SUCCESS_P and
                    nxt_com >= MIN_COMFORT_P):
                continue

            nxt_path = tuple(cur.path + [rule.action])
            signature = (rule.to_state, nxt_path)

            # if the *exact same* state+action sequence occurred -> loop
            if signature in visited:
                continue
            visited.add(signature)

            queue.append(
                Node(rule.to_state, nxt_dur, nxt_cos,
                     nxt_suc, nxt_com, list(nxt_path))
            )

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
            print(f"    {i:2d}. {act}")
        print(f"  Final state: {n.state}\n")

# ╔═══════════════════════════════════════╗
# ║ 9.  MAIN ─ RUN EVERYTHING             ║
# ╚═══════════════════════════════════════╝
if __name__ == "__main__":
    print(f"Patient starts in {patient_1.state}, goal is {GOAL_STATE}\n")
    solutions = search_all(patient_1)
    print_solutions(solutions)

