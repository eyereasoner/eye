var eye = require('./eye.js')

var cases = {
  GND: {
    ':': 'http://josd.github.io/eye#'
  },
  ':oneway': [
    { head: { pred: ':oneway', args: [{ pred: ':Paris', args: [] }, { pred: ':Orleans', args: [] }] }, body: [] },
    { head: { pred: ':oneway', args: [{ pred: ':Paris', args: [] }, { pred: ':Chartres', args: [] }] }, body: [] },
    { head: { pred: ':oneway', args: [{ pred: ':Paris', args: [] }, { pred: ':Amiens', args: [] }] }, body: [] },
    { head: { pred: ':oneway', args: [{ pred: ':Orleans', args: [] }, { pred: ':Blois', args: [] }] }, body: [] },
    { head: { pred: ':oneway', args: [{ pred: ':Orleans', args: [] }, { pred: ':Bourges', args: [] }] }, body: [] },
    { head: { pred: ':oneway', args: [{ pred: ':Blois', args: [] }, { pred: ':Tours', args: [] }] }, body: [] },
    { head: { pred: ':oneway', args: [{ pred: ':Chartres', args: [] }, { pred: ':Lemans', args: [] }] }, body: [] },
    { head: { pred: ':oneway', args: [{ pred: ':Lemans', args: [] }, { pred: ':Angers', args: [] }] }, body: [] },
    { head: { pred: ':oneway', args: [{ pred: ':Lemans', args: [] }, { pred: ':Tours', args: [] }] }, body: [] },
    { head: { pred: ':oneway', args: [{ pred: ':Tours', args: [] }, { pred: ':Lemans', args: [] }] }, body: [] },
    { head: { pred: ':oneway', args: [{ pred: ':Angers', args: [] }, { pred: ':Nantes', args: [] }] }, body: [] }
  ],
  ':path': [
    { head: { pred: ':path', args: [{ pred: '?X', args: [] }, { pred: '?Y', args: [] }] }, body: [{ pred: ':oneway', args: [{ pred: '?X', args: [] }, { pred: '?Y', args: [] }] }] },
    { head: { pred: ':path', args: [{ pred: '?X', args: [] }, { pred: '?Z', args: [] }] }, body: [{ pred: ':path', args: [{ pred: '?X', args: [] }, { pred: '?Y', args: [] }] }, { pred: ':path', args: [{ pred: '?Y', args: [] }, { pred: '?Z', args: [] }] }] }
  ],
  '': [
    { head: {}, body: [{ pred: ':path', args: [{ pred: ':Paris', args: [] }, { pred: ':Nantes', args: [] }] }] }
  ]
}

var evidence = {}
var i = null

for (i = 0; i < cases[''].length; i++) if (cases[''][i] != null) eye.prove(cases, cases[''][i], evidence)
for (i in evidence) evidence.GND = cases.GND

console.log(JSON.stringify(evidence, null, 2))
