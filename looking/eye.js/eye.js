// ---------------------------------------
// Another eye in javascript -- Jos De Roo
// ---------------------------------------
//
// See https://github.com/josd/eye/tree/master/looking/eye.js#readme
//

module.exports = {
  prove: function (cases, goal, evidence) {
    var queue = [{ rule: goal, src: 0, ind: 0, parent: null, env: {}, ground: [] }]
    while (queue.length > 0) {
      var c = queue.pop()
      var g1 = aCopy(c.ground)
      if (c.ind >= c.rule.body.length) {
        if (c.parent == null) {
          for (var i = 0; i < c.rule.body.length; i++) {
            var t1 = evaluate(c.rule.body[i], c.env)
            if (typeof (evidence[t1.pred]) === 'undefined') evidence[t1.pred] = []
            evidence[t1.pred].push({ head: t1, body: [{ pred: 'GND', args: c.ground }] })
          }
          continue
        }
        if (c.rule.body.length !== 0) g1.push({ src: c.rule, env: c.env })
        var r1 = {
          rule: { head: c.parent.rule.head, body: c.parent.rule.body },
          src: c.parent.src,
          ind: c.parent.ind,
          parent: c.parent.parent != null ? new Copy(c.parent.parent) : null,
          env: new Copy(c.parent.env),
          ground: g1
        }
        unify(c.rule.head, c.env, r1.rule.body[r1.ind], r1.env, true)
        r1.ind++
        queue.push(r1)
        continue
      }
      var t2 = c.rule.body[c.ind]
      var b = builtin(t2, c)
      if (b === 1) {
        g1.push({ src: { head: evaluate(t2, c.env), body: [] }, env: {} })
        var r2 = { rule: { head: c.rule.head, body: c.rule.body }, src: c.src, ind: c.ind, parent: c.parent, env: c.env, ground: g1 }
        r2.ind++
        queue.push(r2)
        continue
      } else if (b === 0) continue
      if (cases[t2.pred] == null) continue
      var src = 0
      for (var k = 0; k < cases[t2.pred].length; k++) {
        var r3 = cases[t2.pred][k]
        src++
        var g2 = aCopy(c.ground)
        if (r3.body.length === 0) g2.push({ src: r3, env: {} })
        var r4 = { rule: r3, src: src, ind: 0, parent: c, env: {}, ground: g2 }
        if (unify(t2, c.env, r3.head, r4.env, true)) {
          var ep = c // euler path
          while ((ep = ep.parent)) if (ep.src === c.src && unify(ep.rule.head, ep.env, c.rule.head, c.env, false)) break
          if (ep == null) {
            queue.unshift(r4)
          }
        }
      }
    }
  }
}

function unify (s, senv, d, denv, f) {
  if (isVar(s.pred)) {
    var sval = evaluate(s, senv)
    if (sval != null) return unify(sval, senv, d, denv, f)
    else return true
  } else if (isVar(d.pred)) {
    var dval = evaluate(d, denv)
    if (dval != null) return unify(s, senv, dval, denv, f)
    else {
      if (f != null) denv[d.pred] = evaluate(s, senv)
      return true
    }
  } else if (s.pred === d.pred && s.args.length === d.args.length) {
    for (var i = 0; i < s.args.length; i++) if (!unify(s.args[i], senv, d.args[i], denv, f)) return false
    return true
  } else {
    return false
  }
}

function evaluate (t, env) {
  if (isVar(t.pred)) {
    var a1 = env[t.pred]
    if (a1 != null) return evaluate(a1, env)
    else return null
  } else if (t.args.length === 0) return t
  else {
    var n = []
    for (var i = 0; i < t.args.length; i++) {
      var a2 = evaluate(t.args[i], env)
      if (a2 != null) n.push(a2)
      else n.push({ pred: t.args[i].pred, args: [] })
    }
    return { pred: t.pred, args: n }
  }
}

function isVar (s) {
  return s.charAt(0) === '?'
}

function aCopy (t) {
  var a = []
  for (var i = 0; i < t.length; i++) a[i] = t[i]
  return a
}

function Copy (t) {
  for (var i in t) this[i] = t[i]
}

function builtin (t, c) {
  if (t.pred === 'GND') return 1
  var t0 = evaluate(t.args[0], c.env)
  var t1 = evaluate(t.args[1], c.env)
  var a = null
  var ti = null
  if (t.pred === 'log:equalTo') {
    if (t0 != null && t1 != null && t0.pred === t1.pred) return 1
    else return 0
  } else if (t.pred === 'log:notEqualTo') {
    if (t0 != null && t1 != null && t0.pred !== t1.pred) return 1
    else return 0
  } else if (t.pred === 'math:absoluteValue') {
    if (t0 != null && !isVar(t0.pred)) {
      a = Math.abs(parseFloat(t0.pred))
      if (unify({ pred: a.toString(), args: [] }, c.env, t.args[1], c.env, true)) return 1
    } else return 0
  } else if (t.pred === 'math:cos') {
    if (t0 != null && !isVar(t0.pred)) {
      a = Math.cos(parseFloat(t0.pred))
      if (unify({ pred: a.toString(), args: [] }, c.env, t.args[1], c.env, true)) return 1
    } else if (t1 != null && !isVar(t1.pred)) {
      a = Math.acos(parseFloat(t1.pred))
      if (unify({ pred: a.toString(), args: [] }, c.env, t.args[0], c.env, true)) return 1
    } else return 0
  } else if (t.pred === 'math:degrees') {
    if (t0 != null && !isVar(t0.pred)) {
      a = parseFloat(t0.pred) * 180 / Math.PI
      if (unify({ pred: a.toString(), args: [] }, c.env, t.args[1], c.env, true)) return 1
    } else if (t1 != null && !isVar(t1.pred)) {
      a = parseFloat(t0.pred) * Math.PI / 180
      if (unify({ pred: a.toString(), args: [] }, c.env, t.args[0], c.env, true)) return 1
    } else return 0
  } else if (t.pred === 'math:equalTo') {
    if (t0 != null && t1 != null && parseFloat(t0.pred) === parseFloat(t1.pred)) return 1
    else return 0
  } else if (t.pred === 'math:greaterThan') {
    if (t0 != null && t1 != null && parseFloat(t0.pred) > parseFloat(t1.pred)) return 1
    else return 0
  } else if (t.pred === 'math:lessThan') {
    if (t0 != null && t1 != null && parseFloat(t0.pred) < parseFloat(t1.pred)) return 1
    else return 0
  } else if (t.pred === 'math:notEqualTo') {
    if (t0 != null && t1 != null && parseFloat(t0.pred) !== parseFloat(t1.pred)) return 1
    else return 0
  } else if (t.pred === 'math:notLessThan') {
    if (t0 != null && t1 != null && parseFloat(t0.pred) >= parseFloat(t1.pred)) return 1
    else return 0
  } else if (t.pred === 'math:notGreaterThan') {
    if (t0 != null && t1 != null && parseFloat(t0.pred) <= parseFloat(t1.pred)) return 1
    else return 0
  } else if (t.pred === 'math:difference' && t0 != null) {
    a = parseFloat(evaluate(t0.args[0], c.env).pred)
    for (ti = t0.args[1]; ti.args.length !== 0; ti = ti.args[1]) a -= parseFloat(evaluate(ti.args[0], c.env).pred)
    if (unify({ pred: a.toString(), args: [] }, c.env, t.args[1], c.env, true)) return 1
    else return 0
  } else if (t.pred === 'math:exponentiation' && t0 != null) {
    a = parseFloat(evaluate(t0.args[0], c.env).pred)
    for (ti = t0.args[1]; ti.args.length !== 0; ti = ti.args[1]) a = Math.pow(a, parseFloat(evaluate(ti.args[0], c.env).pred))
    if (unify({ pred: a.toString(), args: [] }, c.env, t.args[1], c.env, true)) return 1
    else return 0
  } else if (t.pred === 'math:integerQuotient' && t0 != null) {
    a = parseFloat(evaluate(t0.args[0], c.env).pred)
    for (ti = t0.args[1]; ti.args.length !== 0; ti = ti.args[1]) a /= parseFloat(evaluate(ti.args[0], c.env).pred)
    if (unify({ pred: Math.floor(a).toString(), args: [] }, c.env, t.args[1], c.env, true)) return 1
    else return 0
  } else if (t.pred === 'math:negation') {
    if (t0 != null && !isVar(t0.pred)) {
      a = -parseFloat(t0.pred)
      if (unify({ pred: a.toString(), args: [] }, c.env, t.args[1], c.env, true)) return 1
    } else if (t1 != null && !isVar(t1.pred)) {
      a = -parseFloat(t1.pred)
      if (unify({ pred: a.toString(), args: [] }, c.env, t.args[0], c.env, true)) return 1
    } else return 0
  } else if (t.pred === 'math:product' && t0 != null) {
    a = parseFloat(evaluate(t0.args[0], c.env).pred)
    for (ti = t0.args[1]; ti.args.length !== 0; ti = ti.args[1]) a *= parseFloat(evaluate(ti.args[0], c.env).pred)
    if (unify({ pred: a.toString(), args: [] }, c.env, t.args[1], c.env, true)) return 1
    else return 0
  } else if (t.pred === 'math:quotient' && t0 != null) {
    a = parseFloat(evaluate(t0.args[0], c.env).pred)
    for (ti = t0.args[1]; ti.args.length !== 0; ti = ti.args[1]) a /= parseFloat(evaluate(ti.args[0], c.env).pred)
    if (unify({ pred: a.toString(), args: [] }, c.env, t.args[1], c.env, true)) return 1
    else return 0
  } else if (t.pred === 'math:remainder' && t0 != null) {
    a = parseFloat(evaluate(t0.args[0], c.env).pred)
    for (ti = t0.args[1]; ti.args.length !== 0; ti = ti.args[1]) a %= parseFloat(evaluate(ti.args[0], c.env).pred)
    if (unify({ pred: a.toString(), args: [] }, c.env, t.args[1], c.env, true)) return 1
    else return 0
  } else if (t.pred === 'math:rounded') {
    if (t0 != null && !isVar(t0.pred)) {
      a = Math.round(parseFloat(t0.pred))
      if (unify({ pred: a.toString(), args: [] }, c.env, t.args[1], c.env, true)) return 1
    } else return 0
  } else if (t.pred === 'math:sin') {
    if (t0 != null && !isVar(t0.pred)) {
      a = Math.sin(parseFloat(t0.pred))
      if (unify({ pred: a.toString(), args: [] }, c.env, t.args[1], c.env, true)) return 1
    } else if (t1 != null && !isVar(t1.pred)) {
      a = Math.asin(parseFloat(t1.pred))
      if (unify({ pred: a.toString(), args: [] }, c.env, t.args[0], c.env, true)) return 1
    } else return 0
  } else if (t.pred === 'math:sum' && t0 != null) {
    a = parseFloat(evaluate(t0.args[0], c.env).pred)
    for (ti = t0.args[1]; ti.args.length !== 0; ti = ti.args[1]) a += parseFloat(evaluate(ti.args[0], c.env).pred)
    if (unify({ pred: a.toString(), args: [] }, c.env, t.args[1], c.env, true)) return 1
    else return 0
  } else if (t.pred === 'math:tan') {
    if (t0 != null && !isVar(t0.pred)) {
      a = Math.tan(parseFloat(t0.pred))
      if (unify({ pred: a.toString(), args: [] }, c.env, t.args[1], c.env, true)) return 1
    } else if (t1 != null && !isVar(t1.pred)) {
      a = Math.atan(parseFloat(t1.pred))
      if (unify({ pred: a.toString(), args: [] }, c.env, t.args[0], c.env, true)) return 1
    } else return 0
  } else if (t.pred === 'rdf:first' && t0 != null && t0.pred === '.' && t0.args.length !== 0) {
    if (unify(t0.args[0], c.env, t.args[1], c.env, true)) return 1
    else return 0
  } else if (t.pred === 'rdf:rest' && t0 != null && t0.pred === '.' && t0.args.length !== 0) {
    if (unify(t0.args[1], c.env, t.args[1], c.env, true)) return 1
    else return 0
  } else if (t.pred === 'a' && t1 != null && t1.pred === 'rdf:List' && t0 != null && t0.pred === '.') return 1
  else if (t.pred === 'a' && t1 != null && t1.pred === 'rdfs:Resource') return 1
  else return -1
}
