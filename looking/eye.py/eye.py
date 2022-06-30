# -----------------------------------
# Another eye in python -- Jos De Roo
# -----------------------------------
#
# See https://github.com/josd/eye/tree/master/looking/eye.py#readme
#

import copy, math, string, sys, time, urllib.request, urllib.error, urllib.parse

class Triple:
    """A Triple seems to be one of...

    - a variable
    >>> Triple("?WHO").var
    1

    - a 0-ary function term
    >>> Triple(":Socrates").nodes
    []

    - in particular, the empty list
    >>> e=Triple(".")

    - a binary tree term
    >>> l=Triple("(:a :b :c :d :e :f :g)")
    >>> l.arc
    '.'
    >>> len(l.nodes)
    2

    
    @@there seems to be another case with / syntax in lists
    
    - an atomic formula
    >>> atom=Triple(":Socrates a :Man.")
    >>> atom.arc
    'a'
    >>> len(atom.nodes)
    2

    Terms in an atom can be lists.
    >>> atom2 = Triple(":case001 :wantGo (:Paris :Nantes)")
    >>> len(atom2.nodes[1].nodes)
    2
    """
    def __init__(self, s, nodes=[]):
        self.arc = s
        self.nodes = nodes
        if s[-1] != '}':
            pcs = tokenize(s, ' ')
            if len(pcs) == 3:
                self.arc = pcs[1]
                del pcs[1]
                self.nodes = list(map(Triple, pcs))
            elif s[-1] == ')':
                pcs = tokenize(s[1:-1], '/')
                if len(pcs) == 2:
                    self.arc = '.'
                    self.nodes = list(map(Triple, list(map(str.strip, pcs))))
                else:
                    pcs = tokenize(s[1:-1], ' ')
                    pcs.reverse()
                    l = Triple('.')
                    for p in pcs: l = Triple('.', [Triple(p), l])
                    self.arc = l.arc
                    self.nodes = l.nodes
        self.var = self.arc[0] == '?'

    def __repr__(self):
        if self.arc == '.':
            if len(self.nodes) == 0: return '()'
            n = self.nodes[1]
            if n.arc == '.' and n.nodes == []:
                return '(%s)' % str(self.nodes[0])
            elif n.arc == '.':
                return '(%s %s)' % (str(self.nodes[0]), str(self.nodes[1])[1:-1])
            else:
                return '(%s/%s)' % (str(self.nodes[0]), str(self.nodes[1]))
        elif self.nodes:
            return '%s %s %s' % (self.nodes[0], self.arc, self.nodes[1])
        else: return self.arc

class Rule:
    """A rule has a head and a body.

    In the usual case, the head is one triple and the body is N triples.
    >>> rule=Rule("{?WHO a :Man} => {?WHO a :Mortal}.")
    >>> rule.head.arc
    'a'
    >>> len(rule.body)
    1

    If there's no =>, the body is empty
    >>> rule2=Rule(":Socrates a :Man.")
    >>> len(rule2.body)
    0

    
    >>> fact=Triple(":Socrates a :Man.")
    >>> unify(rule2.head, {}, fact, {}, 0)
    1
    """
    def __init__(self, s):
        flds = s.split('=>')
        self.body = []
        if len(flds) == 2:
            self.head = Triple(flds[1].strip(' {}'))
            flds = tokenize(flds[0].strip(' {}'), '.')
            for fld in flds: self.body.append(Triple(fld.strip()))
        else: self.head = Triple(flds[0].strip())

    def __repr__(self):
        if self.body != []:
            s = ''
            for t in self.body:
                if s != '': s = s+'. '
                s = s+str(t)
            if self.head.arc == '[]': return '{%s} => []' % (s)
            else: return '{%s} => {%s}' % (s, self.head)
        else: return '%s' % (self.head)
        
class Step:
    def __init__(self, rule, src=0, parent=None, env={}, ind=0):
        self.rule = rule
        self.src = src
        self.parent = parent
        self.env = copy.copy(env)
        self.ind = ind

def prove(query, rules, why=False, once=False, thunkPerStep=None):
    queue = [Step(query)]
    ev = ''
    while queue:
        c = queue.pop()
        if thunkPerStep: thunkPerStep(c.rule, c.env)
        if c.ind >= len(c.rule.body):
            if not c.parent:
                for t in c.rule.body:
                    e = str(eval(t, c.env))
                    if t.arc == '.': e = e[1:-1]
                    if ev.find(e+'.\n') == -1: ev = ev+e+'.\n'
                if once: return ev
                continue
            r = Step(copy.copy(c.parent.rule), c.parent.src,
                  copy.copy(c.parent.parent), c.parent.env, c.parent.ind)
            unify(c.rule.head, c.env, r.rule.body[r.ind], r.env, 1)
            rh = eval(c.rule.head, c.env)
            if rh and why:
                cr = copy.deepcopy(c.rule)
                cr.head = rh
                r.rule.body = copy.deepcopy(c.parent.rule.body)
                r.rule.body[r.ind] = Triple(str(cr))
            r.ind = r.ind+1
            queue.append(r)
            continue
        t = c.rule.body[c.ind]
        b = builtin(t, c)
        if b == 1:
            r = Step(copy.copy(c.rule), c.src,
                  copy.copy(c.parent), c.env, c.ind)
            if why:
                r.rule.body = copy.deepcopy(r.rule.body)
                r.rule.body[r.ind] = eval(r.rule.body[r.ind], r.env)
            r.ind = r.ind+1
            queue.append(r)
            continue
        elif b == 0: continue
        if not rules.get(t.arc): continue
        i = len(queue)
        src = 0
        for rl in rules[t.arc]:
            src = src+1
            r = Step(rl, src, c)
            if unify(t, c.env, rl.head, r.env, 1):
                cp = c.parent
                while cp:
                    if cp.src == c.src and unify(cp.rule.head, cp.env,
                          c.rule.head, c.env, 0): break  ### euler path ###
                    cp = cp.parent
                if not cp: queue.insert(i, r)
    return ev

def unify(s, senv, d, denv, f):
    """
    >>> rule2=Rule(":Socrates a :Man.")
    >>> fact=Triple(":Socrates a :Man.")
    >>> unify(rule2.head, {}, fact, {}, 0)
    1

    TODO: change to match AIMA's logic.py, which returns the substitution
    """
    if s.var:
        sval = eval(s, senv)
        if sval: return unify(sval, senv, d, denv, f)
        else: return 1
    elif d.var:
        dval = eval(d, denv)
        if dval: return unify(s, senv, dval, denv, f)
        else:
            if f: denv[d.arc] = eval(s, senv)
            return 1
    elif s.arc == d.arc and len(s.nodes) == len(d.nodes):
        for i in range(len(s.nodes)):
            if not unify(s.nodes[i], senv, d.nodes[i], denv, f): return 0
        return 1
    else: return 0

def eval(t, env):
    """
    >>> v = Triple("?WHO")
    >>> t = Triple(":me")
    >>> eval(v, {"?WHO": t}) is t
    1

    >>> v2 = Triple("?WHAT")
    >>> eval(v2, {"?WHO": t}) is None
    1

    >>> expr = Triple(":Joe :knows ?WHO")
    >>> eval(expr, {"?WHO": t}).nodes[1].arc
    ':me'

    """
    
    if t.var:
        a = env.get(t.arc)
        if a: return eval(a, env)
        else: return None
    elif t.nodes == []: return t
    else:
        n = []
        for arg in t.nodes: 
            a = eval(arg, env)
            if a: n.append(a)
            else: n.append(Triple(arg.arc))
        return Triple(t.arc, n)

def builtin(t, c):
    t0 = eval(t.nodes[0], c.env)
    t1 = eval(t.nodes[1], c.env)
    if t.arc == 'log:equalTo':
        if t0 and t1 and t0.arc == t1.arc: return 1
        else: return 0
    elif t.arc == 'log:notEqualTo':
        if t0 and t1 and t0.arc != t1.arc: return 1
        else: return 0
    elif t.arc == 'math:absoluteValue':
        if t0 and not t0.var:
            a = abs(float(t0.arc))
            if unify(Triple(str(a)), c.env, t.nodes[1], c.env, 1): return 1
        else: return 0
    elif t.arc == 'math:cos':
        if t0 and not t0.var:
            a = math.cos(float(t0.arc))
            if unify(Triple(str(a)), c.env, t.nodes[1], c.env, 1): return 1
        elif t1 and not t1.var:
            a = math.acos(float(t1.arc))
            if unify(Triple(str(a)), c.env, t.nodes[0], c.env, 1): return 1
        else: return 0
    elif t.arc == 'math:degrees':
        if t0 and not t0.var:
            a = float(t0.arc)*180/math.pi
            if unify(Triple(str(a)), c.env, t.nodes[1], c.env, 1): return 1
        elif t1 and not t1.var:
            a = float(t0.arc)*math.pi/180
            if unify(Triple(str(a)), c.env, t.nodes[0], c.env, 1): return 1
        else: return 0
    elif t.arc == 'math:equalTo':
        if t0 and t1 and float(t0.arc) == float(t1.arc): return 1
        else: return 0
    elif t.arc == 'math:greaterThan':
        if t0 and t1 and float(t0.arc) > float(t1.arc): return 1
        else: return 0
    elif t.arc == 'math:lessThan':
        if t0 and t1 and float(t0.arc) < float(t1.arc): return 1
        else: return 0
    elif t.arc == 'math:notEqualTo':
        if t0 and t1 and float(t0.arc) != float(t1.arc): return 1
        else: return 0
    elif t.arc == 'math:notLessThan':
        if t0 and t1 and float(t0.arc) >= float(t1.arc): return 1
        else: return 0
    elif t.arc == 'math:notGreaterThan':
        if t0 and t1 and float(t0.arc) <= float(t1.arc): return 1
        else: return 0
    elif t.arc == 'math:difference' and t0:
        a = float(eval(t0.nodes[0], c.env).arc)
        ti = t0.nodes[1]
        while ti.nodes:
            a = a-float(eval(ti.nodes[0], c.env).arc)
            ti = ti.nodes[1]
        if unify(Triple(str(a)), c.env, t.nodes[1], c.env, 1): return 1
        else: return 0
    elif t.arc == 'math:exponentiation' and t0:
        a = float(eval(t0.nodes[0], c.env).arc)
        ti = t0.nodes[1]
        while ti.nodes:
            a = a**float(eval(ti.nodes[0], c.env).arc)
            ti = ti.nodes[1]
        if unify(Triple(str(a)), c.env, t.nodes[1], c.env, 1): return 1
        else: return 0
    elif t.arc == 'math:integerQuotient' and t0:
        a = float(eval(t0.nodes[0], c.env).arc)
        ti = t0.nodes[1]
        while ti.nodes:
            a = a/float(eval(ti.nodes[0], c.env).arc)
            ti = ti.nodes[1]
        if unify(Triple(str(int(a))), c.env, t.nodes[1], c.env, 1): return 1
        else: return 0
    elif t.arc == 'math:negation':
        if t0 and not t0.var:
            a = -float(t0.arc)
            if unify(Triple(str(a)), c.env, t.nodes[1], c.env, 1): return 1
        elif t1 and not t1.var:
            a = -float(t1.arc)
            if unify(Triple(str(a)), c.env, t.nodes[0], c.env, 1): return 1
        else: return 0
    elif t.arc == 'math:product' and t0:
        a = float(eval(t0.nodes[0], c.env).arc)
        ti = t0.nodes[1]
        while ti.nodes:
            a = a*float(eval(ti.nodes[0], c.env).arc)
            ti = ti.nodes[1]
        if unify(Triple(str(a)), c.env, t.nodes[1], c.env, 1): return 1
        else: return 0
    elif t.arc == 'math:quotient' and t0:
        a = float(eval(t0.nodes[0], c.env).arc)
        ti = t0.nodes[1]
        while ti.nodes:
            a = a/float(eval(ti.nodes[0], c.env).arc)
            ti = ti.nodes[1]
        if unify(Triple(str(a)), c.env, t.nodes[1], c.env, 1): return 1
        else: return 0
    elif t.arc == 'math:remainder' and t0:
        a = float(eval(t0.nodes[0], c.env).arc)
        ti = t0.nodes[1]
        while ti.nodes:
            a = a%float(eval(ti.nodes[0], c.env).arc)
            ti = ti.nodes[1]
        if unify(Triple(str(a)), c.env, t.nodes[1], c.env, 1): return 1
        else: return 0
    elif t.arc == 'math:rounded':
        if t0 and not t0.var:
            a = round(float(t0.arc))
            if unify(Triple(str(a)), c.env, t.nodes[1], c.env, 1): return 1
        else: return 0
    elif t.arc == 'math:sin':
        if t0 and not t0.var:
            a = math.sin(float(t0.arc))
            if unify(Triple(str(a)), c.env, t.nodes[1], c.env, 1): return 1
        elif t1 and not t1.var:
            a = math.asin(float(t1.arc))
            if unify(Triple(str(a)), c.env, t.nodes[0], c.env, 1): return 1
        else: return 0
    elif t.arc == 'math:sum' and t0:
        a = float(eval(t0.nodes[0], c.env).arc)
        ti = t0.nodes[1]
        while ti.nodes:
            a = a+float(eval(ti.nodes[0], c.env).arc)
            ti = ti.nodes[1]
        if unify(Triple(str(a)), c.env, t.nodes[1], c.env, 1): return 1
        else: return 0
    elif t.arc == 'math:tan':
        if t0 and not t0.var:
            a = math.tan(float(t0.arc))
            if unify(Triple(str(a)), c.env, t.nodes[1], c.env, 1): return 1
        elif t1 and not t1.var:
            a = math.atan(float(t1.arc))
            if unify(Triple(str(a)), c.env, t.nodes[0], c.env, 1): return 1
        else: return 0
    elif t.arc == 'rdf:first' and t0 and t0.arc == '.' and t0.nodes != []:
        if unify(t0.nodes[0], c.env, t.nodes[1], c.env, 1): return 1
        else: return 0
    elif t.arc == 'rdf:rest' and t0 and t0.arc == '.' and t0.nodes != []:
        if unify(t0.nodes[1], c.env, t.nodes[1], c.env, 1): return 1
        else: return 0
    elif t.arc == 'a' and t1 and t1.arc == 'rdf:List' and t0 and t0.arc == '.':
        return 1
    elif t.arc == 'a' and t1 and t1.arc == 'rdfs:Resource': return 1
    else: return -1

def tokenize(s, sep, all=1):
    n = 0
    sq = False
    dq = False
    ls = len(sep)
    if s == '': return []
    for i in range(len(s)):
        c = s[i]
        if n <= 0 and not sq and not dq and s[i:i+ls] == sep:
            if s[i] == '.' and s[i+1].isdigit() and s[i-1].isdigit(): continue
            elif all and s[:i] != '': return [s[:i]]+tokenize(s[i+ls:], sep)
            elif all and s[:i] == '': return tokenize(s[i+ls:], sep)
            elif s[:i] != '': return [s[:i], s[i+ls:]]
            else: return [s[i+ls:]]
        elif c == '(': n = n+1
        elif c == ')': n = n-1
        elif c == '\'': sq = not sq
        elif c == '"': dq = not dq
    return [s]

def load(path, rules, goals, np):
    f = urllib.request.urlopen(path)
    triple = 0
    while True:
        s = f.readline().decode('utf-8')
        if s == '': break
        s = s.strip()
        if s == '': continue
        elif s[0] == '#': continue
        elif s.find('@prefix') != -1:
            t = tokenize(s, ' ')
            u = t[2].strip('.')
            if np.get(t[1]) and np[t[1]] != u:
                sys.stderr.write('#FAIL @prefix %s %s.\n'%(t[1], u))
                break
            else: np[t[1]] = u
        elif s.find('=> []') != -1:
            goals.append(Rule(s.strip('.')))
        else:
            r = Rule(s.strip('.'))
            if not rules.get(r.head.arc): rules[r.head.arc] = []
            rules[r.head.arc].append(r)
            if not r.head.arc == '.' and r.body == []:
                r = Rule('('+s.strip('.')+')')
                if not rules.get(r.head.arc): rules[r.head.arc] = []
                rules[r.head.arc].append(r)
            triple = triple+1
    return triple

def run(args):
    rules = {}
    np = {}
    np['log:'] = '<http://www.w3.org/2000/10/swap/log#>'
    np['math:'] = '<http://www.w3.org/2000/10/swap/math#>'
    np['owl:'] = '<http://www.w3.org/2002/07/owl#>'
    np['rdf:'] = '<http://www.w3.org/1999/02/22-rdf-syntax-ns#>'
    np['rdfs:'] = '<http://www.w3.org/2000/01/rdf-schema#>'
    np['xsd:'] = '<http://www.w3.org/2001/XMLSchema#>'
    triple = 0
    goals = []
    why = False
    once = False
    debug = False
    for arg in args:
        if arg == '--why': why = True
        elif arg == '--once': once = True
        elif arg == '--debug': debug = True
        elif arg == '': pass
        else:
            triple = triple + load(arg, rules, goals, np)

    if goals == []: goals.append(Rule('{(?S ?P ?O)} => []]'))

    # odd; can't assign to step in count()
    # cf http://ivan.truemesh.com/archives/000411.html
    step = [0]
    def count(rule, env):
        step[0] += 1
        if debug: print('step %s %s %s' % (step[0], rule, env))

    for pfx, i in np.items():
        print("@prefix %s %s." % (pfx, i))
    print()
    for g in goals:
        print(prove(g, rules, why, once, count))
    sys.stdout.flush()
    
    sys.stderr.write('#ENDS %s [%s triples]\n' %
          (args[-1], triple))
    sys.stderr.flush()

def _test():
    import doctest
    doctest.testmod(verbose=True)

if __name__ == '__main__':
    if '--test' in sys.argv:
        _test()
        sys.exit(0)

    if len(sys.argv) == 1:
        print('Usage: eye.py [--why] [--once] [--debug] triples')
    else: run(sys.argv[1:])
