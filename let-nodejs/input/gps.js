#!/usr/bin/env node
/*  Goal-driven Parallel Sequences – Node.js port
    Original idea: Jos De Roo (Prolog)
    Python -> JS translation by ChatGPT
*/

/* ──────────────────────────────────────────────────────────
   Helpers (Prolog-style unification & instantiation)
   ────────────────────────────────────────────────────────── */
function isVar(token) {
  return typeof token === 'string' && (token[0] === '_' || /[A-Z]/.test(token[0]));
}

function unify(pattern, fact, env = {}) {
  if (pattern.length !== fact.length) return null;
  const e = { ...env };
  for (let i = 0; i < pattern.length; i++) {
    const p = pattern[i];
    const f = fact[i];
    if (isVar(p)) {
      if (p in e && e[p] !== f) return null;
      e[p] = f;
    } else if (p !== f) {
      return null;
    }
  }
  return e;
}

function instantiate(pattern, env) {
  return pattern.map(tok => (tok in env ? env[tok] : tok));
}

function factEquals(a, b) {
  if (a.length !== b.length) return false;
  return a.every((v, i) => v === b[i]);
}

/* ──────────────────────────────────────────────────────────
   Domain object
   ────────────────────────────────────────────────────────── */
class Transition {
  constructor(
    mapId,
    fromPattern,
    toPattern,
    action,
    duration,
    cost,
    belief,
    comfort
  ) {
    Object.assign(this, {
      mapId,
      fromPattern,
      toPattern,
      action,
      duration,
      cost,
      belief,
      comfort,
    });
  }
}

/* ──────────────────────────────────────────────────────────
   Utility predicates
   ────────────────────────────────────────────────────────── */
function stageCount(history) {
  if (history.length === 0) return 1;
  let stages = 1;
  for (let i = 1; i < history.length; i++) {
    if (history[i] !== history[i - 1]) stages += 1;
  }
  return stages;
}

function goalSatisfied(state, goalPattern) {
  return state.some(fact => unify(goalPattern, fact) !== null);
}

function applyTransition(state, trans, env) {
  const fromFact = instantiate(trans.fromPattern, env);
  const toFact = instantiate(trans.toPattern, env);

  const next = state.filter(f => !factEquals(f, fromFact)); // remove one
  next.push(toFact);
  return next;
}

function stateSignature(state) {
  // stable string key for loop-checking
  return state
    .map(f => JSON.stringify(f))
    .sort()
    .join('|');
}

/* ──────────────────────────────────────────────────────────
   Depth-first search generator
   ────────────────────────────────────────────────────────── */
function* dfs(
  state,
  transitions,
  goalPattern,
  limits,
  mapHist,
  pathHist,
  dur,
  cost,
  belief,
  comfort,
  visited
) {
  const sig = stateSignature(state);
  if (visited.has(sig)) return;
  visited.add(sig);

  if (goalSatisfied(state, goalPattern)) {
    yield [pathHist, dur, cost, belief, comfort];
  }

  const [maxDur, maxCost, minBelief, minComfort, maxStages] = limits;

  for (const tr of transitions) {
    if (stageCount([...mapHist, tr.mapId]) > maxStages) continue;

    for (const fact of state) {
      const env = unify(tr.fromPattern, fact);
      if (!env) continue;

      const dur2 = dur + tr.duration;
      if (dur2 > maxDur) continue;
      const cost2 = cost + tr.cost;
      if (cost2 > maxCost) continue;
      const belief2 = belief * tr.belief;
      if (belief2 < minBelief) continue;
      const comfort2 = comfort * tr.comfort;
      if (comfort2 < minComfort) continue;

      const nextState = applyTransition(state, tr, env);

      yield* dfs(
        nextState,
        transitions,
        goalPattern,
        limits,
        [...mapHist, tr.mapId],
        [...pathHist, tr.action],
        dur2,
        cost2,
        belief2,
        comfort2,
        new Set(visited) // local copy for back-tracking
      );
    }
  }
}

function* allPaths(state, transitions, goalPattern, limits) {
  yield* dfs(state, transitions, goalPattern, limits, [], [], 0, 0, 1, 1, new Set());
}

/* ──────────────────────────────────────────────────────────
   Example knowledge base (partial Belgium map)
   ────────────────────────────────────────────────────────── */
const transitions = [
  new Transition(
    'map_be',
    ['location', 'S', 'gent'],
    ['location', 'S', 'brugge'],
    'drive_gent_brugge',
    1500.0,
    0.006,
    0.96,
    0.99
  ),
  new Transition(
    'map_be',
    ['location', 'S', 'gent'],
    ['location', 'S', 'kortrijk'],
    'drive_gent_kortrijk',
    1600.0,
    0.007,
    0.96,
    0.99
  ),
  new Transition(
    'map_be',
    ['location', 'S', 'kortrijk'],
    ['location', 'S', 'brugge'],
    'drive_kortrijk_brugge',
    1600.0,
    0.007,
    0.96,
    0.99
  ),
  new Transition(
    'map_be',
    ['location', 'S', 'brugge'],
    ['location', 'S', 'oostende'],
    'drive_brugge_oostende',
    900.0,
    0.004,
    0.98,
    1.0
  ),
];

const initialState = [['location', 'i1', 'gent']];
const goalPattern = ['location', '_', 'oostende'];
const limits = [5000.0, 5.0, 0.2, 0.4, 1];

/* ──────────────────────────────────────────────────────────
   Demo driver (mirrors the original Prolog query)
   ────────────────────────────────────────────────────────── */
if (require.main === module) {
  let idx = 1;
  for (const [path, dur, cost, bel, comf] of allPaths(
    initialState,
    transitions,
    goalPattern,
    limits
  )) {
    console.log(`Solution ${idx++}`);
    console.log('  Path     :', path.join(' → '));
    console.log('  Duration :', dur);
    console.log('  Cost     :', cost);
    console.log('  Belief   :', bel);
    console.log('  Comfort  :', comf);
    console.log();
  }
}
