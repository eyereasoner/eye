![EYE](https://josd.github.io/images/eye.png)

# Eye command line arguments and flags

This is an introduction into the command line arguments and flags that can be provided to the EYE reasoner.

Running the EYE reasoner requires at least two inputs:

- **data**: a description of the N3 data/rules to execute
- **query**: a description of the output that is expected from the reasoner
  
and optional:

- **options**: flags that influence the execution of the reasoner

Both the **data** and **query** arguments can be command line arguments or flags.

**Data arguments/flags**

Each data input should be one of:

- `<uri>` : the path or a URL to a N3 resource with data/rules
- `--turtle <uri>` : the path or URL to a Turtle resource with data

or, one of the more advanced inputs:

- `--n3p <uri>` : the path or URL to a N3P prolog file (that can be generated with the `--intermediate` flag)
- `--n3p-output` : request the output serialized as N3P prolog
- `--proof <uri>` : the path or URL to N3 proof lemmas 

Data inputs can be repeated. For instance, to provide a local and a remote N3 input and one Turtle input one can pass as command line arguments:

```
eye mydata.n3 http://somewhere.org/remote.n3 --turtle file1.ttl
```

**Query flags**

The eye command in the example above doesn't provide any output. Query flags need to be added to create a result output. 

The query flags should be one of:

- `--pass`  : output the deductive closures (all deductions when applying the N3 rules on the datas)
- `--pass-all` : output the deductive closures plus the N3 rules
- `--pass-only-new` : output only the new derived triples (not the original data)
- `--query <uri>` : output should be filtered with the rules in the query filter
     - `--query <uri>` arguments can be repeated

or, one of the more advanced queries:

- `--entail <rdf-graph>` : output true if RDF graph is entailed
- `--not-entail <rdf-graph>` : output true if RDF graph is not entailed
- `--pass-all-ground` : ground the rules and run `--pass-all`

Using one of the flags above these are all valid EYE commands:

```
eye mydata.n3 http://somewhere.org/remote.n3 --turtle file1.ttl --pass

eye mydata.n3 http://somewhere.org/remote.n3 --turtle file1.ttl --pass-all

eye mydata.n3 http://somewhere.org/remote.n3 --turtle file1.ttl --pass-only-new

eye mydata.n3 http://somewhere.org/remote.n3 --query filter1.n3

eye mydata.n3 http://somewhere.org/remote.n3 --query filter1.n3 --query filter2.n3
```

**Options**

These are the most important options that can be provided in every EYE reasoner run:

- `--nope`: skip outputing the proof explanation in the output
- `--quiet`: make the EYE reasoner not verbose
- `--restricted`: run the EYE reasoner in restricted more only allowing core built-ins without side-effects (without network/file access)
- `--strings`: allow the `log:outputString` to output objects on the stdout
- `--intermediate`: output all data and rules as N3P prolog to the standard output (do not calculate any inferences)

Flags that influence the processing of N3 rules:

- `--tactic linear-select` : run the EYE reasoner in a mode where each N3 can only be executed one time
- `--tactic limited-answer <nr>` : give only a limited number of answers

**Full example**

```
eye --nope --quiet \
    mydata.n3 http://somewhere.org/remote.n3 --turtle file1.ttl --pass

eye --nope --quiet --restricted \
    mydata.n3 http://somewhere.org/remote.n3 --query filter1.n3
```