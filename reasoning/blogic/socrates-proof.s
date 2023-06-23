skolem:proof a r:Proof, r:Conjunction;
    r:component skolem:lemma1;
    r:component skolem:lemma2;
    r:component skolem:lemma3;
    r:gives {
        <http://example.org/ns#Socrates> a <http://example.org/ns#Man>.
        <http://example.org/ns#Socrates> a <http://example.org/ns#Human>.
        <http://example.org/ns#Socrates> a <http://example.org/ns#Mortal>.
    }.

skolem:lemma1 a r:Inference;
    r:gives {
        <http://example.org/ns#Socrates> a <http://example.org/ns#Man>.
    };
    r:evidence (
        skolem:lemma4
    );
    r:binding [ r:variable [ n3:uri "http://eyereasoner.github.io/var#x_0"]; r:boundTo [ n3:uri "http://example.org/ns#Socrates"]];
    r:binding [ r:variable [ n3:uri "http://eyereasoner.github.io/var#x_1"]; r:boundTo [ n3:uri "http://example.org/ns#Man"]];
    r:rule [ a r:DerivedQuery; r:gives {
        {
            var:x_0 a var:x_1.
        } => {
            var:x_0 a var:x_1.
        }.
    }].

skolem:lemma2 a r:Inference;
    r:gives {
        <http://example.org/ns#Socrates> a <http://example.org/ns#Human>.
    };
    r:evidence (
        skolem:lemma5
    );
    r:binding [ r:variable [ n3:uri "http://eyereasoner.github.io/var#x_0"]; r:boundTo [ n3:uri "http://example.org/ns#Socrates"]];
    r:binding [ r:variable [ n3:uri "http://eyereasoner.github.io/var#x_1"]; r:boundTo [ n3:uri "http://example.org/ns#Human"]];
    r:rule [ a r:DerivedQuery; r:gives {
        {
            var:x_0 a var:x_1.
        } => {
            var:x_0 a var:x_1.
        }.
    }].

skolem:lemma3 a r:Inference;
    r:gives {
        <http://example.org/ns#Socrates> a <http://example.org/ns#Mortal>.
    };
    r:evidence (
        skolem:lemma6
    );
    r:binding [ r:variable [ n3:uri "http://eyereasoner.github.io/var#x_0"]; r:boundTo [ n3:uri "http://example.org/ns#Socrates"]];
    r:binding [ r:variable [ n3:uri "http://eyereasoner.github.io/var#x_1"]; r:boundTo [ n3:uri "http://example.org/ns#Mortal"]];
    r:rule [ a r:DerivedQuery; r:gives {
        {
            var:x_0 a var:x_1.
        } => {
            var:x_0 a var:x_1.
        }.
    }].

skolem:lemma4 a r:Extraction;
    r:gives {
        <http://example.org/ns#Socrates> a <http://example.org/ns#Man>.
    };
    r:because [ a r:Parsing; r:source <>].

skolem:lemma5 a r:Inference;
    r:gives {
        <http://example.org/ns#Socrates> a <http://example.org/ns#Human>.
    };
    r:evidence (
        skolem:lemma7
        skolem:lemma4
    );
    r:binding [ r:variable [ n3:uri "http://eyereasoner.github.io/var#x_0"]; r:boundTo [ n3:uri "http://example.org/ns#Man"]];
    r:binding [ r:variable [ n3:uri "http://eyereasoner.github.io/var#x_1"]; r:boundTo [ n3:uri "http://example.org/ns#Human"]];
    r:binding [ r:variable [ n3:uri "http://eyereasoner.github.io/var#x_2"]; r:boundTo [ n3:uri "http://example.org/ns#Socrates"]];
    r:rule [ a r:DerivedRule; r:gives {
        {
            var:x_0 <http://www.w3.org/2000/01/rdf-schema#subClassOf> var:x_1.
            var:x_2 a var:x_0.
        } => {
            var:x_2 a var:x_1.
        }.
    }].

skolem:lemma6 a r:Inference;
    r:gives {
        <http://example.org/ns#Socrates> a <http://example.org/ns#Mortal>.
    };
    r:evidence (
        skolem:lemma8
        skolem:lemma5
    );
    r:binding [ r:variable [ n3:uri "http://eyereasoner.github.io/var#x_0"]; r:boundTo [ n3:uri "http://example.org/ns#Human"]];
    r:binding [ r:variable [ n3:uri "http://eyereasoner.github.io/var#x_1"]; r:boundTo [ n3:uri "http://example.org/ns#Mortal"]];
    r:binding [ r:variable [ n3:uri "http://eyereasoner.github.io/var#x_2"]; r:boundTo [ n3:uri "http://example.org/ns#Socrates"]];
    r:rule [ a r:DerivedRule; r:gives {
        {
            var:x_0 <http://www.w3.org/2000/01/rdf-schema#subClassOf> var:x_1.
            var:x_2 a var:x_0.
        } => {
            var:x_2 a var:x_1.
        }.
    }].

skolem:lemma7 a r:Extraction;
    r:gives {
        <http://example.org/ns#Man> <http://www.w3.org/2000/01/rdf-schema#subClassOf> <http://example.org/ns#Human>.
    };
    r:because [ a r:Parsing; r:source <>].

skolem:lemma8 a r:Extraction;
    r:gives {
        <http://example.org/ns#Human> <http://www.w3.org/2000/01/rdf-schema#subClassOf> <http://example.org/ns#Mortal>.
    };
    r:because [ a r:Parsing; r:source <>].

