# ============================================================================
# DatalogMTL ➜ Python (ARC-style output) — Self-contained, no imports
# ----------------------------------------------------------------------------
# What this script shows
# - A reasonably complex DatalogMTL program for traffic analytics with metric
#   temporal operators (◇ "eventually within", ◻ "always within").
# - A **minimal interpreter** that reads numeric parameters (windows, thresholds)
#   from the DatalogMTL text and drives the evaluation logic (no external libs).
# - A small synthetic timeline (discrete minutes) with speed, braking, queue,
#   and accident facts.
# - Derived predicates:
#     * low_speed(V,R,KM)                 — based on speed threshold.
#     * queue_long(R,KM)                  — ◇_[0,w] some queue length ≥ L_min.
#     * accident_near(R,KM)               — ◇_[0,w] accident within ≤ D km.
#     * congestion(R,KM)                  — ◻_[0,w] at least K low-speed vehicles
#                                           + queue_long holds in that window.
#     * ghost_queue(R,KM)                 — ◻_[0,1] brakes observed in BOTH minutes
#                                           + ≥ M distinct brakers in [t-1..t]
#                                           + NOT accident_near(t).
#     * incident_congestion(R,KM)         — congestion ∧ recent same-km accident.
# - ARC-style output:
#     Answer: all derived minutes per predicate
#     Reason why: trace-like human-readable justification per minute
#     Check (harness): assertions for expected results
#
# Semantics & conventions
# - Time is discrete integer minutes.
# - A window ◇_[a,b] at time t looks back to closed interval [t-b, t-a].
#   For example, ◇_[0,9] means "sometime between t and t-9 inclusive".
# - A window ◻_[a,b] at time t requires a property to hold at **every** minute
#   in [t-b, t-a].
# - Distances are absolute differences on KM.
# - No imports used; everything implemented with core Python constructs.
# ============================================================================

# ----------
# 1) DatalogMTL program (specification; rules are active, not commented)
# ----------
datalog_mtl_program = r"""
% --- Signatures (timestamped facts) ---
speed(Vehicle, Road, Km, Speed)   @ t.
brake(Vehicle, Road, Km)          @ t.
queue(Road, Km, Length)           @ t.
accident(Road, Km)                @ t.

% --- Derived predicates ---
low_speed(V,R,KM) @ t :- speed(V,R,KM,S) @ t, S < 20.

queue_long(R,KM)  @ t :- ◇_[0,9] ( queue(R,KM,L) @ t' and L >= 10 ).
% "eventually in the last 10 minutes there's a long queue"

accident_near(R,KM) @ t :- ◇_[0,14] ( accident(R,KM2) @ t' and abs(KM - KM2) <= 1 ).
% "there was a nearby accident within the last 15 minutes"

congestion(R,KM) @ t :-
    ◻_[0,9] ( count_distinct V: low_speed(V,R,KM) @ t'' ) >= 3
    and queue_long(R,KM) @ t.
% "for every minute in the last 10 minutes, at least 3 vehicles have low speed,
%  and there exists a long queue in that 10-minute window"

ghost_queue(R,KM) @ t :-
    ◻_[0,1] ( exists brake(V,R,KM) @ t' )                                 % brakes in BOTH minutes
    and (count_distinct V: ◇_[0,1] brake(V,R,KM)) >= 5
    and not accident_near(R,KM) @ t.
% "≥5 distinct vehicles braked within the last 2 minutes, activity in both minutes, and no nearby accident"

incident_congestion(R,KM) @ t :- congestion(R,KM) @ t and ◇_[0,14] accident(R,KM) @ t'.
% "congestion coincides with a recent accident (same km)"
"""

# ----------
# 2) Minimal "interpreter" to read parameters from the program text
#    (parses windows and numeric thresholds used by the runtime)
# ----------

def _strip_spaces(s):
    out = ""
    i = 0
    while i < len(s):
        c = s[i]
        if c != " " and c != "\t":
            out += c
        i += 1
    return out

def _find_after(s, token, start=0):
    i = s.find(token, start)
    if i < 0:
        return -1
    return i + len(token)

def _parse_int_at(s, idx):
    # parse an integer starting at s[idx:]
    neg = False
    if idx < len(s) and s[idx] == '-':
        neg = True
        idx += 1
    j = idx
    while j < len(s) and s[j].isdigit():
        j += 1
    if j == idx:
        return None, idx
    val = int(s[idx:j])
    if neg:
        val = -val
    return val, j

def _parse_window(s, start_idx):
    # expects s[start_idx:] to start at '['a','b']'
    if start_idx < 0 or start_idx >= len(s):
        return None
    i = start_idx
    # find '['
    while i < len(s) and s[i] != '[':
        i += 1
    if i >= len(s):
        return None
    i += 1
    a, i2 = _parse_int_at(s, i)
    if a is None:
        return None
    # find ','
    while i2 < len(s) and s[i2] != ',':
        i2 += 1
    if i2 >= len(s):
        return None
    i3 = i2 + 1
    b, i4 = _parse_int_at(s, i3)
    if b is None:
        return None
    # find ']'
    while i4 < len(s) and s[i4] != ']':
        i4 += 1
    if i4 >= len(s):
        return None
    return (a, b)

def _parse_last_number_after(s, token):
    # returns the last integer after occurrences of token
    idx = 0
    last = None
    while True:
        pos = s.find(token, idx)
        if pos < 0:
            break
        # move to after token
        j = pos + len(token)
        # skip spaces
        while j < len(s) and (s[j] == ' ' or s[j] == '\t'):
            j += 1
        # parse int
        val, j2 = _parse_int_at(s, j)
        if val is not None:
            last = val
        idx = j2
    return last

def interpret_parameters(program_text):
    # Defaults (used if parsing misses)
    params = {
        "low_speed_threshold": 20,
        "queue_long_window": (0, 9),
        "queue_long_min": 10,
        "accident_near_window": (0, 14),
        "accident_near_distance": 1,
        "congestion_box_window": (0, 9),
        "congestion_min_vehicles": 3,
        "ghost_queue_box_window": (0, 1),     # both minutes if present
        "ghost_queue_min_distinct": 5,
        "ghost_queue_require_both_minutes": False,
        "incident_accident_window": (0, 14),
    }

    # Remove whitespace to simplify parsing
    flat = _strip_spaces(program_text)

    # low_speed threshold
    ls_idx = flat.find("low_speed(")
    if ls_idx >= 0:
        # try to find "S<" then number
        th = _parse_last_number_after(flat[ls_idx:flat.find(".", ls_idx) if flat.find(".", ls_idx) != -1 else len(flat)], "S<")
        if th is not None:
            params["low_speed_threshold"] = th

    # queue_long window and L >=
    ql_idx = flat.find("queue_long(")
    if ql_idx >= 0:
        # window after "◇_"
        dia = flat.find("◇_[", ql_idx)
        if dia < 0:
            dia = flat.find("\\u25C7_[", ql_idx)  # fallback if escaped
        if dia >= 0:
            w = _parse_window(flat, dia + 2)  # start at the '['
            if w is not None:
                params["queue_long_window"] = w
        Lmin = _parse_last_number_after(flat[ql_idx:flat.find(".", ql_idx) if flat.find(".", ql_idx) != -1 else len(flat)], "L>=")
        if Lmin is not None:
            params["queue_long_min"] = Lmin

    # accident_near window and distance
    an_idx = flat.find("accident_near(")
    if an_idx >= 0:
        dia = flat.find("◇_[", an_idx)
        if dia >= 0:
            w = _parse_window(flat, dia + 2)
            if w is not None:
                params["accident_near_window"] = w
        # distance: look for "<=" right after "abs(" ... ")<="
        le_pos = flat.find("<=", an_idx)
        if le_pos >= 0:
            val, _ = _parse_int_at(flat, le_pos + 2)
            if val is not None:
                params["accident_near_distance"] = val

    # congestion box window and min vehicles
    cg_idx = flat.find("congestion(")
    if cg_idx >= 0:
        box = flat.find("◻_[", cg_idx)
        if box < 0:
            box = flat.find("\\u25FB_[", cg_idx)  # fallback if escaped
        if box >= 0:
            w = _parse_window(flat, box + 2)
            if w is not None:
                params["congestion_box_window"] = w
        k = _parse_last_number_after(flat[cg_idx:flat.find(".", cg_idx) if flat.find(".", cg_idx) != -1 else len(flat)], ")>=")
        if k is not None:
            params["congestion_min_vehicles"] = k

    # ghost_queue windows and min distinct, both-minutes requirement
    gq_idx = flat.find("ghost_queue(")
    if gq_idx >= 0:
        # ◻_[0,1] presence implies both-minute activity requirement
        box = flat.find("◻_[", gq_idx)
        if box >= 0:
            w = _parse_window(flat, box + 2)
            if w is not None:
                params["ghost_queue_box_window"] = w
                # If box window is [0,1], require both minutes
                if w == (0,1):
                    params["ghost_queue_require_both_minutes"] = True
        m = _parse_last_number_after(flat[gq_idx:flat.find(".", gq_idx) if flat.find(".", gq_idx) != -1 else len(flat)], ")>=")
        if m is not None:
            params["ghost_queue_min_distinct"] = m

    # incident_congestion accident window
    ic_idx = flat.find("incident_congestion(")
    if ic_idx >= 0:
        dia = flat.find("◇_[", ic_idx)
        if dia >= 0:
            w = _parse_window(flat, dia + 2)
            if w is not None:
                params["incident_accident_window"] = w

    return params

PARAMS = interpret_parameters(datalog_mtl_program)

# ----------
# 3) Synthetic data (minutes as integer timestamps)
# ----------
Road = "A12"
KM = 12

# Facts
speeds = []    # (t, vehicle, road, km, speed)
brakes = []    # (t, vehicle, road, km)
queues = []    # (t, road, km, length)
accidents = [] # (t, road, km)

# Vehicles
vehicles = ["v1","v2","v3","v4","v5","v6","v7","v8"]

# Baseline: minutes 0..30, normal speeds
t = 0
while t <= 30:
    for v in vehicles:
        speeds.append((t, v, Road, KM, 45))
    t += 1

# Low-speed phase: minutes 31..45, v1..v4 crawl at ~12 km/h; others ~40
t = 31
while t <= 45:
    i = 0
    while i < 4:  # v1..v4
        v = vehicles[i]
        speeds.append((t, v, Road, KM, 12))
        i += 1
    while i < len(vehicles):  # v5..v8
        v = vehicles[i]
        speeds.append((t, v, Road, KM, 40))
        i += 1
    t += 1

# Queues: grow around minute 33..40
queues.append((33, Road, KM, 8))
queues.append((36, Road, KM, 12))
queues.append((40, Road, KM, 15))

# Accident at minute 35 at the same KM
accidents.append((35, Road, KM))

# Brake cluster #1 (near the accident): 5 vehicles brake at minutes 34-35
i = 0
while i < 5:
    v = vehicles[i]
    brakes.append((34, v, Road, KM))
    i += 1
i = 0
while i < 5:
    v = vehicles[i]
    brakes.append((35, v, Road, KM))
    i += 1

# Brake cluster #2 (later, no accident nearby): 5 vehicles brake at minutes 56-57
i = 0
while i < 5:
    v = vehicles[i]
    brakes.append((56, v, Road, KM))
    i += 1
i = 0
while i < 5:
    v = vehicles[i]
    brakes.append((57, v, Road, KM))
    i += 1

# ----------
# 4) Evaluation helpers driven by PARAMS (interpreted from the program)
# ----------

def _max_timestamp():
    m = -1
    i = 0
    while i < len(speeds):
        if speeds[i][0] > m:
            m = speeds[i][0]
        i += 1
    i = 0
    while i < len(brakes):
        if brakes[i][0] > m:
            m = brakes[i][0]
        i += 1
    i = 0
    while i < len(queues):
        if queues[i][0] > m:
            m = queues[i][0]
        i += 1
    i = 0
    while i < len(accidents):
        if accidents[i][0] > m:
            m = accidents[i][0]
        i += 1
    return m

def _window_bounds(t, win):
    a, b = win
    # [t-b .. t-a]
    return t - b, t - a

def _abs_int(x):
    return x if x >= 0 else -x

def low_speed_by_minute():
    """minute -> set of vehicles with low speed (< threshold)."""
    threshold = PARAMS["low_speed_threshold"]
    ls = {}
    i = 0
    while i < len(speeds):
        t, v, r, km, s = speeds[i]
        if r == Road and km == KM and s < threshold:
            if t not in ls:
                ls[t] = set()
            ls[t].add(v)
        i += 1
    return ls

def exists_long_queue_in_window(t, win, min_len):
    lo, hi = _window_bounds(t, win)
    j = 0
    while j < len(queues):
        t2, r, km, L = queues[j]
        if r == Road and km == KM and lo <= t2 <= hi and L >= min_len:
            return True
        j += 1
    return False

def accident_near_in_window(t, win, max_dist):
    lo, hi = _window_bounds(t, win)
    j = 0
    while j < len(accidents):
        t2, r, km = accidents[j]
        if r == Road and lo <= t2 <= hi and _abs_int(km - KM) <= max_dist:
            return True
        j += 1
    return False

def accident_same_km_in_window(t, win):
    lo, hi = _window_bounds(t, win)
    j = 0
    while j < len(accidents):
        t2, r, km = accidents[j]
        if r == Road and km == KM and lo <= t2 <= hi:
            return True
        j += 1
    return False

def distinct_brakers_in_last_2(t):
    # strictly uses [t-1..t]
    lo = t - 1
    hi = t
    window_vs = set()
    j = 0
    while j < len(brakes):
        t2, v, r, km = brakes[j]
        if r == Road and km == KM and lo <= t2 <= hi:
            window_vs.add(v)
        j += 1
    return len(window_vs)

def brakes_in_both_minutes(t):
    # require events at both t-1 and t
    seen_minus_1 = False
    seen_t = False
    j = 0
    while j < len(brakes):
        t2, v, r, km = brakes[j]
        if r == Road and km == KM:
            if t2 == t - 1:
                seen_minus_1 = True
            elif t2 == t:
                seen_t = True
        j += 1
    return seen_minus_1 and seen_t

def congestion_minutes():
    ls = low_speed_by_minute()
    box_win = PARAMS["congestion_box_window"]
    need_k = PARAMS["congestion_min_vehicles"]
    M = _max_timestamp()
    result = set()
    t = 0
    while t <= M:
        lo, hi = _window_bounds(t, box_win)
        all_ok = True
        m = lo
        while m <= hi:
            if m < 0 or (m not in ls) or (len(ls[m]) < need_k):
                all_ok = False
                break
            m += 1
        if all_ok:
            if exists_long_queue_in_window(t, PARAMS["queue_long_window"], PARAMS["queue_long_min"]):
                result.add(t)
        t += 1
    return result

def ghost_queue_minutes():
    M = _max_timestamp()
    result = set()
    t = 0
    while t <= M:
        both_minutes = True
        if PARAMS["ghost_queue_require_both_minutes"]:
            both_minutes = brakes_in_both_minutes(t)
        enough_distinct = distinct_brakers_in_last_2(t) >= PARAMS["ghost_queue_min_distinct"]
        no_accident_near = not accident_near_in_window(t, PARAMS["accident_near_window"], PARAMS["accident_near_distance"])
        if both_minutes and enough_distinct and no_accident_near:
            result.add(t)
        t += 1
    return result

def incident_congestion_minutes(cong):
    result = set()
    for t in cong:
        if accident_same_km_in_window(t, PARAMS["incident_accident_window"]):
            result.add(t)
    return result

# ----------
# 5) Compute answers
# ----------
cong = congestion_minutes()
gq = ghost_queue_minutes()
ic = incident_congestion_minutes(cong)

# ----------
# 6) Reason-why traces (human-readable)
# ----------
def reason_for_congestion(t):
    ls = low_speed_by_minute()
    lo, hi = _window_bounds(t, PARAMS["congestion_box_window"])
    # window counts
    window_counts = []
    m = lo
    while m <= hi:
        cnt = 0
        if m in ls:
            cnt = len(ls[m])
        window_counts.append((m, cnt))
        m += 1
    # queue hits
    q_win = PARAMS["queue_long_window"]
    q_lo, q_hi = _window_bounds(t, q_win)
    queue_hit = []
    j = 0
    while j < len(queues):
        (t2, r, km, L) = queues[j]
        if r == Road and km == KM and q_lo <= t2 <= q_hi and L >= PARAMS["queue_long_min"]:
            queue_hit.append((t2, L))
        j += 1
    return "At t=" + str(t) + ", every minute in [" + str(lo) + ".." + str(hi) + "] had >= " + str(PARAMS["congestion_min_vehicles"]) + " low-speed vehicles " + \
           "(counts=" + str(window_counts) + "); and queue>=" + str(PARAMS["queue_long_min"]) + " exists at " + str(queue_hit) + "."

def reason_for_ghost_queue(t):
    # events in [t-1..t]
    window = []
    j = 0
    while j < len(brakes):
        (t2, v, r, km) = brakes[j]
        if r == Road and km == KM and (t - 1) <= t2 <= t:
            window.append((t2, v))
        j += 1
    # count distinct vehicles
    seen = set()
    k = 0
    while k < len(window):
        seen.add(window[k][1])
        k += 1
    return "At t=" + str(t) + ", " + str(len(seen)) + " distinct vehicles braked within minutes [" + str(t-1) + ".." + str(t) + "] " + \
           "(events=" + str(sorted(window)) + "), brakes-in-both-minutes=" + str(brakes_in_both_minutes(t)) + \
           ", and no nearby accident in last " + str(PARAMS["accident_near_window"][1] - PARAMS["accident_near_window"][0] + 1) + " minutes."

def reason_for_incident_congestion(t):
    lo, hi = _window_bounds(t, PARAMS["incident_accident_window"])
    accs = []
    j = 0
    while j < len(accidents):
        (t2, r, km) = accidents[j]
        if r == Road and km == KM and lo <= t2 <= hi:
            accs.append(t2)
        j += 1
    return "At t=" + str(t) + ", congestion holds and there was an accident at KM=" + str(KM) + \
           " in the last window [" + str(lo) + ".." + str(hi) + "] at times=" + str(accs) + "."

# ----------
# 7) ARC output
# ----------
print("# --- Interpreted parameters ---")
print(PARAMS)

print("\n# --- ARC: Answer ---")
answer = {
    "congestion_minutes": sorted(list(cong)),
    "ghost_queue_minutes": sorted(list(gq)),
    "incident_congestion_minutes": sorted(list(ic)),
}
print(answer)

print("\n# --- ARC: Reason why ---")
for t in sorted(list(cong)):
    print("congestion:", reason_for_congestion(t))
for t in sorted(list(gq)):
    print("ghost_queue:", reason_for_ghost_queue(t))
for t in sorted(list(ic)):
    print("incident_congestion:", reason_for_incident_congestion(t))

print("\n# --- ARC: Check (harness) ---")
# Expected results based on how we constructed the synthetic data and interpreted windows:
# - Low speeds from 31..45 by 4 vehicles → the earliest 10-minute "always" window with >=3 is [31..40],
#   so congestion should start at t=40 and persist through t=45 (until the low-speed phase ends).
expected_congestion = set()
x = 40
while x <= 45:
    expected_congestion.add(x)
    x += 1

# - Brake cluster #1 (34-35) coincides with an accident at 35 → ghost_queue must NOT trigger then (accident_near true).
# - Brake cluster #2 (56-57) without any accident nearby → ghost_queue should trigger at t=57 only
#   (because [56..57] has ≥5 distinct brakers and both minutes have activity).
expected_ghost_queue = set([57])

# - incident_congestion is congestion ∧ accident at same km in last 15 minutes.
#   Accident at 35 → for t in 40..45 it's within 15 minutes.
expected_incident_congestion = set()
y = 40
while y <= 45:
    expected_incident_congestion.add(y)
    y += 1

print("Expected:")
print({
    "congestion_minutes": sorted(list(expected_congestion)),
    "ghost_queue_minutes": sorted(list(expected_ghost_queue)),
    "incident_congestion_minutes": sorted(list(expected_incident_congestion)),
})

# Assertions
if cong != expected_congestion:
    raise AssertionError("congestion mismatch: got " + str(sorted(list(cong))))
if gq != expected_ghost_queue:
    raise AssertionError("ghost_queue mismatch: got " + str(sorted(list(gq))))
if ic != expected_incident_congestion:
    raise AssertionError("incident_congestion mismatch: got " + str(sorted(list(ic))))

print("\nAll checks passed ✅")

