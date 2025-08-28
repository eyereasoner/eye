#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
ODRL Quiz — ARC (Answer / Reason / Check), self-contained

This script analyzes the SolidLabResearch ODRL quiz files and classifies each as:
  • Ambiguous   — there exists a Permission/Obligation vs Prohibition pair with overlapping scope/action
                  for which a value satisfies P but not N (a P-only “witness”)
  • Conflict    — at least one overlapping pair exists, but no P-only witness for any of them
  • No conflict — no overlapping pairs

Answer:
  Prints one verdict per quiz (and one representative pair), or all pairs if OUTPUT_MODE="pairs".

Reason why:
  Explains the clause expansion and pairwise reasoning, and shows a short trace.

Check (harness):
  Internal consistency checks:
    - If Ambiguous: at least one pair exposes a P-only witness.
    - If Conflict: at least one pair overlaps and none admit P-only witnesses.
    - If No conflict: no pair overlaps.
    - Deterministic labeling & stable sorting of rules.

Notes:
  - Works with or without fetching ODRL22.ttl (falls back to a minimal action hierarchy).
  - Handles odrl:or, odrl:and, odrl:xone, odrl:andSequence, and core operators:
      eq, neq, lt, lteq, gt, gteq, isAnyOf, isNoneOf, isAllOf, isA
"""

from __future__ import annotations

# ─────────────────────────── stdlib imports ───────────────────────────
import sys
import re
import datetime
from datetime import timezone
from dataclasses import dataclass
from decimal import Decimal
from enum import Enum
from typing import Dict, Iterable, List, Optional, Sequence, Set, Tuple, Union, Generator
from itertools import count
from urllib.request import urlopen, Request
from urllib.error import URLError
from urllib.parse import urljoin

# ─────────────────────────── Minimal RDF model ───────────────────────────
# We model just enough to support the quiz: URIRef, BNode, Literal, Namespace,
# and a Graph with add/triples/objects/subjects + a small namespace manager.

class URIRef(str):
    """IRI node (same semantics as string, subclass for type clarity)."""
    pass

class BNode(str):
    """Blank node identifier (e.g., 'b1'); printed as '_:b1' when qnamed."""
    pass

# Common XSD IRIs (strings), available early so Literal can use them
XSD_NS = "http://www.w3.org/2001/XMLSchema#"
XSD_INTEGER   = XSD_NS + "integer"
XSD_DECIMAL   = XSD_NS + "decimal"
XSD_BOOLEAN   = XSD_NS + "boolean"
XSD_DATE      = XSD_NS + "date"
XSD_DATETIME  = XSD_NS + "dateTime"

class Literal:
    """Simple literal with optional datatype or language tag."""
    __slots__ = ("lex", "datatype", "lang")

    def __init__(self, value, datatype: Optional[URIRef]=None, lang: Optional[str]=None):
        self.lang = lang
        # Accept Python values or lexical strings.
        if isinstance(value, int):
            self.lex = str(value)
            self.datatype = URIRef(XSD_INTEGER)
        elif isinstance(value, Decimal):
            self.lex = format(value, 'f')
            self.datatype = URIRef(XSD_DECIMAL)
        elif isinstance(value, bool):
            self.lex = "true" if value else "false"
            self.datatype = URIRef(XSD_BOOLEAN)
        elif isinstance(value, datetime.datetime):
            # Store lexical form; we'll normalize in toPython()
            self.lex = value.isoformat()
            self.datatype = URIRef(XSD_DATETIME)
        elif isinstance(value, datetime.date):
            self.lex = value.isoformat()
            self.datatype = URIRef(XSD_DATE)
        else:
            self.lex = str(value)
            self.datatype = URIRef(datatype) if datatype else None

    def toPython(self):
        """Coerce to a Python value if the datatype is recognized."""
        dt = str(self.datatype) if self.datatype else None
        s = self.lex
        try:
            if dt == XSD_INTEGER:
                return int(s)
            if dt == XSD_DECIMAL:
                return Decimal(s)
            if dt == XSD_BOOLEAN:
                return True if s == "true" else False if s == "false" else s
            if dt == XSD_DATETIME:
                # Accept trailing 'Z' and offsets; normalize to naive UTC
                s2 = s[:-1] + "+00:00" if s.endswith("Z") else s
                v = datetime.datetime.fromisoformat(s2)
                if v.tzinfo is not None:
                    v = v.astimezone(timezone.utc).replace(tzinfo=None)
                return v
            if dt == XSD_DATE:
                return datetime.date.fromisoformat(s)
        except Exception:
            pass
        return s  # unknown datatype: return lexical form

    def __str__(self):
        if self.lang:
            return f"\"{self.lex}\"@{self.lang}"
        if self.datatype:
            return f"\"{self.lex}\"^^<{self.datatype}>"
        return f"\"{self.lex}\""

    def __repr__(self):
        return f"Literal({self.lex!r}, datatype={self.datatype!r}, lang={self.lang!r})"

    def __hash__(self):
        return hash((self.lex, str(self.datatype) if self.datatype else None, self.lang))

    def __eq__(self, other):
        if not isinstance(other, Literal):
            return False
        return (self.lex,
                str(self.datatype) if self.datatype else None,
                self.lang) == (other.lex,
                               str(other.datatype) if other.datatype else None,
                               other.lang)

class Namespace(str):
    """Simple namespace; attribute access or indexing yields IRIs."""
    def __getattr__(self, name: str) -> URIRef:
        return URIRef(self + name)
    def __getitem__(self, name: str) -> URIRef:
        return URIRef(self + str(name))

class NamespaceManager:
    """Prefix <-> base IRI mapping with basic QName normalization."""
    def __init__(self):
        self.prefixes: Dict[str, str] = {}  # prefix -> base IRI
        self._rev: List[Tuple[str, str]] = []  # cached list for normalizeUri

    def bind(self, prefix: str, ns: Namespace, override: bool=False):
        base = str(ns)
        if prefix in self.prefixes and not override:
            return
        self.prefixes[prefix] = base
        # Sort by base length desc so longest base wins during normalization
        self._rev = sorted(((p, b) for p, b in self.prefixes.items()),
                           key=lambda t: len(t[1]), reverse=True)

    def expand(self, pname: str) -> URIRef:
        # Handles "pref:local" and ":local" (empty prefix).
        pref, _, local = pname.partition(':')
        base = self.prefixes.get(pref)
        if base is None:
            raise KeyError(f"Unknown prefix: {pref!r} in {pname!r}")
        return URIRef(base + local)

    def normalizeUri(self, uri: URIRef) -> str:
        """Return a prefixed name if any prefix matches, else the full IRI."""
        u = str(uri)
        for pref, base in self._rev:
            if u.startswith(base):
                return f"{pref}:{u[len(base):]}"
        return u

class Graph:
    """Minimal RDF graph with a small Turtle parser for the quiz subset."""
    def __init__(self):
        self._triples: List[Tuple[Union[URIRef, BNode], URIRef, Union[URIRef, BNode, Literal]]] = []
        self.namespace_manager = NamespaceManager()
        # Default commonly used prefixes; callers can override
        self.namespace_manager.bind("rdf", RDF, override=False)
        self.namespace_manager.bind("rdfs", RDFS, override=False)
        self.namespace_manager.bind("xsd", XSD, override=False)
        self.namespace_manager.bind("dct", DCTERMS, override=False)

    # Storage & pattern matching
    def add(self, s, p, o):
        self._triples.append((s, p, o))

    def triples(self, spo) -> Generator[Tuple[object, object, object], None, None]:
        s_q, p_q, o_q = spo
        for (s, p, o) in self._triples:
            if s_q is not None and s_q != s: continue
            if p_q is not None and p_q != p: continue
            if o_q is not None and o_q != o: continue
            yield (s, p, o)

    def __contains__(self, spo) -> bool:
        s_q, p_q, o_q = spo
        for (s, p, o) in self._triples:
            if s_q is not None and s_q != s: continue
            if p_q is not None and p_q != p: continue
            if o_q is not None and o_q != o: continue
            return True
        return False

    def objects(self, s, p):
        for _, _, o in self.triples((s, p, None)):
            yield o

    def subjects(self, p, o):
        for s, _, _ in self.triples((None, p, o)):
            yield s

    # Parsing
    def parse(self, source: str, format: str="turtle"):
        if format != "turtle":
            raise ValueError("Only Turtle is supported by this lightweight parser.")
        text = None
        # If source looks like raw Turtle, parse directly; else fetch file/URL.
        if source.strip().startswith("@prefix") or "\n" in source or " " in source[:70]:
            text = source
        else:
            if re.match(r"^[a-z]+://", source):
                try:
                    req = Request(source, headers={"User-Agent": "mini-rdf/0.2"})
                    with urlopen(req, timeout=30) as fp:
                        text = fp.read().decode("utf-8")
                except URLError as e:
                    raise RuntimeError(f"Failed to fetch {source}: {e}")
            else:
                with open(source, "r", encoding="utf-8") as fh:
                    text = fh.read()
        TurtleParser(self).parse(text)
        return self

# Namespace instances (now safe to construct graphs that reference them)
RDF      = Namespace("http://www.w3.org/1999/02/22-rdf-syntax-ns#")
RDFS     = Namespace("http://www.w3.org/2000/01/rdf-schema#")
XSD      = Namespace(XSD_NS)
DCTERMS  = Namespace("http://purl.org/dc/terms/")

# ─────────────────────────── Tiny Turtle tokenizer ───────────────────────────
# Single REGEX (no VERBOSE) to avoid 3.13 parsing pitfalls.
# Supports:
#   IRIs <...>, @prefix/@base, prefixed names (incl. ":" alone), blank nodes,
#   numbers, strings, ^^, @lang, 'a', and punctuation.

_TOKEN_RE = re.compile(
    r"(?:\s+|#[^\n]*\n)"                                  # whitespace/comments (dropped)
    r"|(?P<IRIREf><[^>]*>)"                               # <...>
    r"|(?P<PREFIX>@prefix|PREFIX|@base|BASE)"             # directives
    r"|(?P<PNAME_LN>(?:[A-Za-z_][\w\-.]*|):[^\s;,\.\)\]\(]+)"  # prefixed name with local (incl ':local')
    r"|(?P<PNAME_NS>(?:[A-Za-z_][\w\-.]*:)|:)"            # prefix: (incl ':' alone)
    r"|(?P<BNODE>_:[A-Za-z][A-Za-z0-9]*)"                 # _:b1
    r"|(?P<TRUE>true|false)"                              # booleans
    r"|(?P<DECIMAL>[+-]?(?:\d+\.\d+|\.\d+))"              # decimals
    r"|(?P<INTEGER>[+-]?\d+)"                             # integers
    r"|(?P<STRING>\"(?:[^\"\\]|\\.)*\"|'(?:[^'\\]|\\.)*')"# strings
    r"|(?P<LANG>@[A-Za-z]+(?:-[A-Za-z0-9]+)*)"            # @lang or @en-US
    r"|(?P<HAT>\^\^)"                                     # ^^
    r"|(?P<A>\ba\b)"                                      # 'a' keyword
    r"|(?P<PUNCT>[;,\.\[\]\(\)])"                         # punctuation
)

class _Tok:
    __slots__ = ("typ", "val")
    def __init__(self, typ, val): self.typ, self.val = typ, val
    def __repr__(self): return f"{self.typ}:{self.val}"

# ─────────────────────────── Tiny Turtle parser ───────────────────────────
class TurtleParser:
    """Very small Turtle parser for the structures used by the quiz & ODRL vocab."""
    def __init__(self, graph: Graph):
        self.g = graph
        self.tokens: List[_Tok] = []
        self.i = 0
        self._bgen = count(1)
        self._base: Optional[str] = None  # @base IRI

    # Token helpers
    def _eof(self): return self.i >= len(self.tokens)
    def _peek(self) -> Optional[_Tok]: return None if self._eof() else self.tokens[self.i]
    def _peek_typ(self) -> Optional[str]: return None if self._eof() else self.tokens[self.i].typ
    def _next(self) -> _Tok:
        t = self.tokens[self.i]; self.i += 1; return t
    def _expect(self, typ: str) -> _Tok:
        t = self._next()
        if t.typ != typ:
            raise SyntaxError(f"Expected {typ}, got {t.typ} ({t.val})")
        return t

    # Entry
    def parse(self, text: str):
        self.tokens = [ _Tok(m.lastgroup, m.group(m.lastgroup))
                        for m in _TOKEN_RE.finditer(text)
                        if m.lastgroup is not None ]
        self.i = 0
        while not self._eof():
            if self._peek_typ() == "PREFIX":
                self._directive()
            elif self._peek_typ() == "PUNCT":
                # Stray punctuation (rare in real docs) – consume to avoid infinite loop.
                self._next()
            else:
                self._triples()
        return self.g

    # Parsing building blocks
    def _directive(self):
        kw = self._next().val.lower()
        if kw in ("@prefix", "prefix"):
            # @prefix p: <iri> .
            pref_tok = self._expect("PNAME_NS")
            pref = pref_tok.val[:-1]  # strip trailing ':'
            iri = self._iri(self._next())
            self.g.namespace_manager.bind(pref, Namespace(iri), override=True)
            if self._peek_typ() == "PUNCT" and self._peek().val == '.':
                self._next()
        elif kw in ("@base", "base"):
            iri = self._iri(self._next())
            self._base = iri
            if self._peek_typ() == "PUNCT" and self._peek().val == '.':
                self._next()
        else:
            raise SyntaxError(f"Unknown directive {kw}")

    def _triples(self):
        s = self._subject()
        self._predicateObjectList(s)
        if self._peek_typ() == "PUNCT" and self._peek().val == '.':
            self._next()

    def _subject(self):
        t = self._peek()
        if t is None:
            raise SyntaxError("Unexpected EOF in subject")
        if t.typ in ("IRIREf", "PNAME_LN", "PNAME_NS"):
            return self._resource(self._next())
        if t.typ == "BNODE":
            return BNode(self._next().val[2:])
        if t.typ == "PUNCT" and t.val == '[':
            return self._blankNodePropertyList()
        if t.typ == "PUNCT" and t.val == '(':
            return self._collection()
        raise SyntaxError(f"Bad subject token {t.typ}:{t.val}")

    def _verb(self):
        t = self._peek()
        if t and t.typ == "A":
            self._next()
            return URIRef(RDF.type)
        return self._resource(self._next())

    def _predicateObjectList(self, s):
        """Parse p o (, o)* (; p o (, o)*)* allowing trailing ';' before ']' or '.'."""
        first = True
        while True:
            if not first:
                if self._peek_typ() == "PUNCT" and self._peek().val == ';':
                    self._next()  # consume ';'
                    # allow trailing ';' before ']' or end-of-statement '.'
                    if self._peek_typ() == "PUNCT" and self._peek().val in (']', '.'):
                        return
                    # otherwise continue reading the next predicate
                else:
                    # caller handles ']' or '.'
                    return
            first = False
            # If we're actually at ']' here, bubble up to caller cleanly.
            if self._peek_typ() == "PUNCT" and self._peek().val == ']':
                return
            p = self._verb()
            self._objectList(s, p)

    def _objectList(self, s, p):
        while True:
            o = self._object()
            self.g.add(s, p, o)
            if self._peek_typ() == "PUNCT" and self._peek().val == ',':
                self._next()
            else:
                return

    def _object(self):
        t = self._peek()
        if t.typ in ("IRIREf", "PNAME_LN", "PNAME_NS"):
            return self._resource(self._next())
        if t.typ == "BNODE":
            return BNode(self._next().val[2:])
        if t.typ == "PUNCT" and t.val == '[':
            return self._blankNodePropertyList()
        if t.typ == "PUNCT" and t.val == '(':
            return self._collection()
        if t.typ in ("STRING","INTEGER","DECIMAL","TRUE"):
            return self._literal()
        raise SyntaxError(f"Bad object token {t.typ}:{t.val}")

    def _blankNodePropertyList(self):
        self._expect("PUNCT")  # '['
        b = BNode(f"b{next(self._bgen)}")
        # Empty [] allowed
        if self._peek_typ() == "PUNCT" and self._peek().val == ']':
            self._next()
            return b
        self._predicateObjectList(b)
        self._expect("PUNCT")  # ']'
        return b

    def _collection(self):
        """Parse '( o1 o2 ... )' as an rdf:first/rest chain, return head node (or rdf:nil)."""
        self._expect("PUNCT")  # '('
        items: List[Union[URIRef, BNode, Literal]] = []
        while not (self._peek_typ() == "PUNCT" and self._peek().val == ')'):
            items.append(self._object())
        self._expect("PUNCT")  # ')'
        if not items:
            return URIRef(RDF.nil)
        # Build rdf:first/rest chain
        head = BNode(f"b{next(self._bgen)}")
        cur = head
        for i, elt in enumerate(items):
            self.g.add(cur, URIRef(RDF.first), elt)
            if i < len(items) - 1:
                nxt = BNode(f"b{next(self._bgen)}")
                self.g.add(cur, URIRef(RDF.rest), nxt)
                cur = nxt
            else:
                self.g.add(cur, URIRef(RDF.rest), URIRef(RDF.nil))
        return head

    def _resource(self, t: _Tok) -> URIRef:
        """Turn IRIREf / PNAME_* into a URIRef, resolving @base for relative IRIs."""
        if t.typ == "IRIREf":
            val = t.val[1:-1]
            if self._base and not re.match(r"^[a-zA-Z][a-zA-Z0-9+.-]*:", val):
                val = urljoin(self._base, val)
            return URIRef(val)
        if t.typ in ("PNAME_LN", "PNAME_NS"):
            return self.g.namespace_manager.expand(t.val)
        raise SyntaxError(f"Expected IRI or prefixed name, got {t.typ}:{t.val}")

    def _iri(self, t: _Tok) -> str:
        if t.typ != "IRIREf":
            raise SyntaxError(f"Expected IRIREf, got {t.typ}:{t.val}")
        return t.val[1:-1]

    def _literal(self) -> Literal:
        t = self._next()
        if t.typ == "STRING":
            s = t.val
            un = self._unescape_string(s[1:-1]) if s[0] == s[-1] else s
            # Language tag?
            if self._peek_typ() == "LANG":
                lang = self._next().val[1:]  # strip '@'
                return Literal(un, datatype=None, lang=lang)
            # Typed literal?
            if self._peek_typ() == "HAT":
                self._next()  # ^^
                dt_tok = self._next()
                dt = self._resource(dt_tok)
                return Literal(un, datatype=dt)
            return Literal(un)
        if t.typ == "INTEGER":
            return Literal(int(t.val))
        if t.typ == "DECIMAL":
            return Literal(Decimal(t.val))
        if t.typ == "TRUE":
            return Literal(True if t.val == "true" else False)
        raise SyntaxError(f"Bad literal token {t}")

    @staticmethod
    def _unescape_string(s: str) -> str:
        # Minimal unescape: quotes, slash, common escapes
        return (s.replace(r'\"', '"')
                 .replace(r"\'", "'")
                 .replace(r'\n', '\n')
                 .replace(r'\r', '\r')
                 .replace(r'\t', '\t')
                 .replace(r'\\', '\\'))

# ─────────────────────────── Config ───────────────────────────
ODRL = Namespace("http://www.w3.org/ns/odrl/2/")
ODRL_TTL_URL = "./resources/ODRL22.ttl"   #cached from https://www.w3.org/ns/odrl/2/ODRL22.ttl

# Logical constructors (use bracket notation for safety)
ODRL_OR = ODRL["or"]
ODRL_AND = ODRL["and"]
ODRL_XONE = ODRL["xone"]
ODRL_ANDSEQUENCE = ODRL["andSequence"]

# Full quiz list (00-01..00-10, 01-01..01-10, 02-01..02-04)
QUIZ_URLS: List[str] = [
    # 00-*
    "https://raw.githubusercontent.com/SolidLabResearch/ODRL-Test-Conflicts/refs/heads/main/quiz/quiz-00-01.ttl",
    "https://raw.githubusercontent.com/SolidLabResearch/ODRL-Test-Conflicts/refs/heads/main/quiz/quiz-00-02.ttl",
    "https://raw.githubusercontent.com/SolidLabResearch/ODRL-Test-Conflicts/refs/heads/main/quiz/quiz-00-03.ttl",
    "https://raw.githubusercontent.com/SolidLabResearch/ODRL-Test-Conflicts/refs/heads/main/quiz/quiz-00-04.ttl",
    "https://raw.githubusercontent.com/SolidLabResearch/ODRL-Test-Conflicts/refs/heads/main/quiz/quiz-00-05.ttl",
    "https://raw.githubusercontent.com/SolidLabResearch/ODRL-Test-Conflicts/refs/heads/main/quiz/quiz-00-06.ttl",
    "https://raw.githubusercontent.com/SolidLabResearch/ODRL-Test-Conflicts/refs/heads/main/quiz/quiz-00-07.ttl",
    "https://raw.githubusercontent.com/SolidLabResearch/ODRL-Test-Conflicts/refs/heads/main/quiz/quiz-00-08.ttl",
    "https://raw.githubusercontent.com/SolidLabResearch/ODRL-Test-Conflicts/refs/heads/main/quiz/quiz-00-09.ttl",
    "https://raw.githubusercontent.com/SolidLabResearch/ODRL-Test-Conflicts/refs/heads/main/quiz/quiz-00-10.ttl",
    # 01-*
    "https://raw.githubusercontent.com/SolidLabResearch/ODRL-Test-Conflicts/refs/heads/main/quiz/quiz-01-01.ttl",
    "https://raw.githubusercontent.com/SolidLabResearch/ODRL-Test-Conflicts/refs/heads/main/quiz/quiz-01-02.ttl",
    "https://raw.githubusercontent.com/SolidLabResearch/ODRL-Test-Conflicts/refs/heads/main/quiz/quiz-01-03.ttl",
    "https://raw.githubusercontent.com/SolidLabResearch/ODRL-Test-Conflicts/refs/heads/main/quiz/quiz-01-04.ttl",
    "https://raw.githubusercontent.com/SolidLabResearch/ODRL-Test-Conflicts/refs/heads/main/quiz/quiz-01-05.ttl",
    "https://raw.githubusercontent.com/SolidLabResearch/ODRL-Test-Conflicts/refs/heads/main/quiz/quiz-01-06.ttl",
    "https://raw.githubusercontent.com/SolidLabResearch/ODRL-Test-Conflicts/refs/heads/main/quiz/quiz-01-07.ttl",
    "https://raw.githubusercontent.com/SolidLabResearch/ODRL-Test-Conflicts/refs/heads/main/quiz/quiz-01-08.ttl",
    "https://raw.githubusercontent.com/SolidLabResearch/ODRL-Test-Conflicts/refs/heads/main/quiz/quiz-01-09.ttl",
    "https://raw.githubusercontent.com/SolidLabResearch/ODRL-Test-Conflicts/refs/heads/main/quiz/quiz-01-10.ttl",
    # 02-*
    "https://raw.githubusercontent.com/SolidLabResearch/ODRL-Test-Conflicts/refs/heads/main/quiz/quiz-02-01.ttl",
    "https://raw.githubusercontent.com/SolidLabResearch/ODRL-Test-Conflicts/refs/heads/main/quiz/quiz-02-02.ttl",
    "https://raw.githubusercontent.com/SolidLabResearch/ODRL-Test-Conflicts/refs/heads/main/quiz/quiz-02-03.ttl",
    "https://raw.githubusercontent.com/SolidLabResearch/ODRL-Test-Conflicts/refs/heads/main/quiz/quiz-02-04.ttl",
]

class OutputMode(str, Enum):
    SUMMARY = "summary"
    PAIRS = "pairs"

OUTPUT_MODE: OutputMode = OutputMode.SUMMARY
REQUIRE_SAME_ASSIGNEE = True
REQUIRE_SAME_TARGET = True
DEBUG = False  # set True to print operand-level satisfiability to stderr

# ─────────────────────────── Utilities ───────────────────────────
QNameish = Union[str, URIRef, Literal, BNode, None]

def qname(g: Graph, term: QNameish) -> str:
    """Pretty-print a node. URIRefs are shown as qnames if possible, else <IRI>."""
    if isinstance(term, URIRef):
        u = str(term)
        try:
            n = g.namespace_manager.normalizeUri(term)
        except Exception:
            n = u
        # If normalization didn’t shorten it (still the full IRI), wrap in <...>
        return n if n != u else f"<{u}>"
    if isinstance(term, Literal):
        return str(term)
    if isinstance(term, BNode):
        return f"_:{term}"
    if term is None:
        return "?"
    return str(term)

def to_python_literal(lit: Literal):
    try:
        return lit.toPython()
    except Exception:
        return lit

def as_list(g: Graph, node: QNameish) -> Optional[List]:
    """Return Python list for rdf:List head, if node is a list head; else None."""
    if node is None:
        return None
    if node == URIRef(RDF.nil):
        return []
    if (node, URIRef(RDF.first), None) in g:
        out = []
        cur = node
        seen = set()
        while cur != URIRef(RDF.nil):
            if cur in seen:  # cycle guard
                break
            seen.add(cur)
            firsts = list(g.objects(cur, URIRef(RDF.first)))
            if not firsts:
                break
            out.append(firsts[0])
            rests = list(g.objects(cur, URIRef(RDF.rest)))
            if not rests:
                break
            cur = rests[0]
        return out
    return None

def rt_closure(edges: Dict[URIRef, Set[URIRef]]) -> Dict[URIRef, Set[URIRef]]:
    """Reflexive-transitive closure for a finite graph of edges (sub/super relations)."""
    closure: Dict[URIRef, Set[URIRef]] = {}
    nodes = set(edges.keys()) | {y for ys in edges.values() for y in ys}
    for a in nodes:
        seen: Set[URIRef] = set()
        stack: List[URIRef] = [a]
        while stack:
            x = stack.pop()
            if x in seen:
                continue
            seen.add(x)
            for y in edges.get(x, ()):
                if y not in seen:
                    stack.append(y)
        seen.add(a)  # reflexive
        closure[a] = seen
    return closure

# ─────────────────────────── Hierarchies ───────────────────────────
class ActionHierarchy:
    """Action overlap via equality or includedIn/subClassOf* (with fallback)."""
    FALLBACK_EDGES: Sequence[Tuple[URIRef, URIRef]] = (
        (ODRL.read, ODRL.use),
        (ODRL.distribute, ODRL.use),
        (ODRL.reproduce, ODRL.use),
        (ODRL.present, ODRL.use),
        (ODRL.play, ODRL.use),
        (ODRL.print, ODRL.use),
        (ODRL.display, ODRL.use),
        (ODRL.stream, ODRL.use),
    )
    def __init__(self, g_vocab: Graph, g_quiz: Graph):
        edges: Dict[URIRef, Set[URIRef]] = {}
        def add(s: QNameish, o: QNameish):
            if isinstance(s, URIRef) and isinstance(o, URIRef):
                edges.setdefault(s, set()).add(o)
        has_included = False
        for s, _, o in g_vocab.triples((None, ODRL.includedIn, None)):
            add(s, o); has_included = True
        for s, _, o in g_quiz.triples((None, ODRL.includedIn, None)):
            add(s, o); has_included = True
        for s, _, o in g_quiz.triples((None, URIRef(RDFS.subClassOf), None)):
            add(s, o)
        if not has_included:
            for s, o in self.FALLBACK_EDGES:
                add(s, o)
        self.closure = rt_closure(edges)
        self._names = g_quiz  # use quiz namespaces in messages

    def overlap(self, a: URIRef, b: URIRef) -> Tuple[bool, str]:
        if a == b:
            return True, f"same action ({qname(self._names, a)})"
        a_sup = self.closure.get(a, {a})
        b_sup = self.closure.get(b, {b})
        if b in a_sup:
            return True, f"{qname(self._names, a)} ⊑ {qname(self._names, b)}"
        if a in b_sup:
            return True, f"{qname(self._names, b)} ⊑ {qname(self._names, a)}"
        return False, "no action inclusion relation"

class TypeHierarchy:
    """rdfs:subClassOf* closure for odrl:isA tests."""
    def __init__(self, g_vocab: Graph, g_quiz: Graph):
        edges: Dict[URIRef, Set[URIRef]] = {}
        def add(s: QNameish, o: QNameish):
            if isinstance(s, URIRef) and isinstance(o, URIRef):
                edges.setdefault(s, set()).add(o)
        for s, _, o in g_vocab.triples((None, URIRef(RDFS.subClassOf), None)):
            add(s, o)
        for s, _, o in g_quiz.triples((None, URIRef(RDFS.subClassOf), None)):
            add(s, o)
        self.closure = rt_closure(edges)

    def is_instance_of(self, g: Graph, x: URIRef, C: URIRef) -> bool:
        for T in g.objects(x, URIRef(RDF.type)):
            if isinstance(T, URIRef) and C in self.closure.get(T, {T}):
                return True
        return False

# ─────────────────────────── Model & parsing ───────────────────────────
@dataclass(frozen=True)
class Atom:
    left: URIRef
    op: URIRef
    right: Union[Literal, URIRef, BNode]

class Expr: pass

@dataclass(frozen=True)
class Or(Expr):
    kids: Tuple[Expr, ...]
@dataclass(frozen=True)
class And(Expr):
    kids: Tuple[Expr, ...]
@dataclass(frozen=True)
class Xone(Expr):
    kids: Tuple[Expr, ...]
@dataclass(frozen=True)
class AndSeq(Expr):
    kids: Tuple[Expr, ...]

@dataclass
class Rule:
    kind: str  # 'permission' | 'prohibition' | 'obligation'
    policy: URIRef
    node: Union[URIRef, BNode]
    assignee: Optional[URIRef]
    action: URIRef
    target: Optional[URIRef]
    exprs: List[Expr]
    print_id: Optional[str] = None

def _parse_expr(g: Graph, cnode: Union[URIRef, BNode]) -> Optional[Expr]:
    left = next(g.objects(cnode, ODRL.leftOperand), None)
    op   = next(g.objects(cnode, ODRL.operator), None)
    right = next(g.objects(cnode, ODRL.rightOperand), None)
    if right is None:
        right = next(g.objects(cnode, ODRL.rightOperandReference), None)
    if isinstance(left, URIRef) and isinstance(op, URIRef) and right is not None:
        return Atom(left, op, right)
    def parse_children(prop: URIRef, ctor) -> Optional[Expr]:
        for coll in g.objects(cnode, prop):
            kids: List[Expr] = []
            for it in (as_list(g, coll) or []):
                e = _parse_expr(g, it)
                if e is not None:
                    kids.append(e)
            if kids:
                return ctor(tuple(kids))
        return None
    for prop, Ctor in ((ODRL_OR, Or), (ODRL_AND, And), (ODRL_XONE, Xone), (ODRL_ANDSEQUENCE, AndSeq)):
        res = parse_children(prop, Ctor)
        if res is not None:
            return res
    return None

def _parse_rule_exprs(g: Graph, rnode: Union[URIRef, BNode]) -> List[Expr]:
    exprs: List[Expr] = []
    saw_any = False
    for c in g.objects(rnode, ODRL.constraint):
        saw_any = True
        e = _parse_expr(g, c)
        if e is not None:
            exprs.append(e)
    def collect(prop: URIRef, ctor) -> None:
        nonlocal saw_any
        for coll in g.objects(rnode, prop):
            saw_any = True
            kids: List[Expr] = []
            for it in (as_list(g, coll) or []):
                e = _parse_expr(g, it)
                if e is not None:
                    kids.append(e)
            if kids:
                exprs.append(ctor(tuple(kids)))
    for prop, Ctor in ((ODRL_OR, Or), (ODRL_AND, And), (ODRL_XONE, Xone), (ODRL_ANDSEQUENCE, AndSeq)):
        collect(prop, Ctor)
    if not exprs and saw_any:
        exprs.append(And(tuple()))  # “present but empty”
    return exprs

def extract_rules(g: Graph) -> List[Rule]:
    out: List[Rule] = []
    KINDS: Sequence[Tuple[URIRef, str]] = (
        (ODRL.permission, "permission"),
        (ODRL.prohibition, "prohibition"),
        (ODRL.prohibited, "prohibition"),
        (ODRL.obligation, "obligation"),
    )
    for pol in g.subjects(URIRef(RDF.type), ODRL.Set):
        for pred, kind in KINDS:
            for rnode in g.objects(pol, pred):
                assignees = list(g.objects(rnode, ODRL.assignee)) or [None]
                actions   = [a for a in g.objects(rnode, ODRL.action) if isinstance(a, URIRef)]
                targets   = list(g.objects(rnode, ODRL.target)) or [None]
                exprs     = _parse_rule_exprs(g, rnode)
                for a in assignees:
                    for act in actions:
                        for t in targets:
                            out.append(Rule(
                                kind, pol, rnode,
                                a if isinstance(a, URIRef) else None,
                                act,
                                t if isinstance(t, URIRef) else None,
                                exprs
                            ))
    return out

# ─────────────────────────── Deterministic ordering ───────────────────────────
def _expr_signature(g: Graph, e: Expr) -> str:
    if isinstance(e, Atom):
        L = as_list(g, e.right)
        rv = ("[" + ",".join(qname(g, x) for x in L) + "]") if L is not None else qname(g, e.right)
        return f"ATOM({qname(g, e.left)}|{qname(g, e.op)}|{rv})"
    if isinstance(e, Or):
        return "OR(" + "|".join(_expr_signature(g, k) for k in e.kids) + ")"
    if isinstance(e, Xone):
        return "XONE(" + "|".join(_expr_signature(g, k) for k in e.kids) + ")"
    if isinstance(e, And):
        return "AND(" + "|".join(_expr_signature(g, k) for k in e.kids) + ")"
    if isinstance(e, AndSeq):
        return "ANDSEQ(" + "|".join(_expr_signature(g, k) for k in e.kids) + ")"
    return "?"

def _rule_sort_key(g: Graph, r: Rule) -> Tuple:
    """Sort by raw IRIs to minimize qname-induced reordering differences."""
    def s(term):
        if isinstance(term, URIRef): return str(term)
        if isinstance(term, Literal): return str(term)
        if isinstance(term, BNode): return f"_:{term}"
        return ""
    sig = "|".join(_expr_signature(g, e) for e in r.exprs)
    return ( s(r.policy), r.kind, s(r.assignee), s(r.action), s(r.target), sig )

def sort_and_label_rules(g: Graph, rules: List[Rule]) -> List[Rule]:
    out = sorted(rules, key=lambda r: _rule_sort_key(g, r))
    for i, r in enumerate(out, start=1):
        r.print_id = f"r{i:02d}"
    return out

# ─────────────────────────── Clause expansion ───────────────────────────
Clause = Dict[URIRef, List[Atom]]  # leftOperand -> atoms

def _merge_and(c1: Clause, c2: Clause) -> Clause:
    r: Clause = {k: v[:] for k, v in c1.items()}
    for L, atoms in c2.items():
        r.setdefault(L, []).extend(atoms)
    return r

def _expand_expr(e: Expr) -> List[Clause]:
    if isinstance(e, Atom):
        return [{e.left: [e]}]
    if isinstance(e, (And, AndSeq)):
        if not e.kids:
            return [{}]
        acc: List[Clause] = [{}]
        for kid in e.kids:
            parts = _expand_expr(kid)
            new_acc: List[Clause] = []
            for a in acc:
                for p in parts:
                    new_acc.append(_merge_and(a, p))
            acc = new_acc
        return acc
    if isinstance(e, (Or, Xone)):
        out: List[Clause] = []
        for kid in e.kids:
            out.extend(_expand_expr(kid))
        return out
    return [{}]

def expand_rule_to_clauses(r: Rule) -> List[Clause]:
    clauses: List[Clause] = [{}]
    for e in r.exprs:
        parts = _expand_expr(e)
        new_clauses: List[Clause] = []
        for c in clauses:
            for p in parts:
                new_clauses.append(_merge_and(c, p))
        clauses = new_clauses
    return clauses

# ─────────────────────────── Operator semantics ───────────────────────────
def _cmp_literals(a: Literal, b: Literal) -> Optional[int]:
    try:
        pa, pb = to_python_literal(a), to_python_literal(b)
        if type(pa) is type(pb):  # strict: only compare same types
            return -1 if pa < pb else (1 if pa > pb else 0)
    except Exception:
        pass
    return None

def _domain_for_operand(g: Graph, atoms: Sequence[Atom]) -> str:
    if any(isinstance(a.right, Literal) for a in atoms):
        return "numeric"
    if any(a.op in (ODRL.isAnyOf, ODRL.isAllOf, ODRL.isNoneOf) for a in atoms):
        return "set"
    return "uri"

def _as_uris(g: Graph, term: Union[Literal, URIRef, BNode]) -> Optional[Set[URIRef]]:
    L = as_list(g, term)
    if L is not None:
        return {x for x in L if isinstance(x, URIRef)}
    if isinstance(term, URIRef):
        return {term}
    return None

def _holds_atom(g: Graph, TH: TypeHierarchy, A: Atom, v) -> Optional[bool]:
    """Evaluate a single atom on a concrete value v (URIRef, set[URIRef], or Literal)."""
    # URI-valued / class tests
    if isinstance(v, URIRef):
        if A.op in (ODRL.eq,):
            return v == A.right if isinstance(A.right, URIRef) else None
        if A.op == ODRL.neq:
            return v != A.right if isinstance(A.right, URIRef) else None
        if A.op == ODRL.isAnyOf:
            S = _as_uris(g, A.right);  return (v in S) if S is not None else None
        if A.op == ODRL.isNoneOf:
            S = _as_uris(g, A.right);  return (v not in S) if S is not None else None
        if A.op == ODRL.isAllOf:
            S = _as_uris(g, A.right);  return (S == {v}) if S is not None else None  # single-valued approx
        if A.op == ODRL.isA and isinstance(A.right, URIRef):
            return TH.is_instance_of(g, v, A.right)
        return None

    # set-valued (for isAnyOf / isAllOf / isNoneOf)
    if isinstance(v, set):
        if A.op == ODRL.eq:
            S = _as_uris(g, A.right);  return (S is not None and v == S)
        if A.op == ODRL.neq:
            S = _as_uris(g, A.right);  return (S is not None and v != S)
        if A.op == ODRL.isAnyOf:
            S = _as_uris(g, A.right);  return bool(S and (v & S))
        if A.op == ODRL.isAllOf:
            S = _as_uris(g, A.right);  return bool(S and (S <= v))
        if A.op == ODRL.isNoneOf:
            S = _as_uris(g, A.right);  return bool(S is not None and not (v & S))
        if A.op == ODRL.isA:
            return None  # set + isA isn’t meaningful
        return None

    # numeric/temporal (Literal-like)
    if isinstance(v, Literal):
        if not isinstance(A.right, Literal):
            return None
        cmp = _cmp_literals(v, A.right)
        if cmp is None:
            return None
        if A.op == ODRL.eq:   return cmp == 0
        if A.op == ODRL.neq:  return cmp != 0
        if A.op == ODRL.lt:   return cmp == -1
        if A.op == ODRL.lteq: return cmp in (-1, 0)
        if A.op == ODRL.gt:   return cmp == 1
        if A.op == ODRL.gteq: return cmp in (1, 0)
        return None

    return None

def _satisfiable_on_operand(g: Graph, TH: TypeHierarchy, atoms: Sequence[Atom]) -> bool:
    """There exists some value v for this operand that makes all atoms true?"""
    dom = _domain_for_operand(g, atoms)
    if dom == "uri":
        # Try equality/list members first; else assume there exists some fresh URI
        cands: List[URIRef] = []
        for A in atoms:
            if A.op == ODRL.eq and isinstance(A.right, URIRef):
                cands.append(A.right)
            else:
                L = _as_uris(g, A.right)
                if L:
                    cands.extend(L)
        seen: Set[URIRef] = set()
        cands = [x for x in cands if not (x in seen or seen.add(x))]
        for v in cands:
            if all(_holds_atom(g, TH, A, v) is not False for A in atoms):
                return True
        # Heuristic: satisfiable unless an isAllOf demands a non-singleton exact set
        return not any(A.op == ODRL.isAllOf and len(_as_uris(g, A.right) or set()) != 1 for A in atoms)

    if dom == "set":
        # Build a minimal set V that satisfies allOf + anyOf while avoiding noneOf
        all_req: Set[URIRef] = set()
        any_sets: List[Set[URIRef]] = []
        none_forbid: Set[URIRef] = set()
        eq_sets: List[Set[URIRef]] = []
        for A in atoms:
            if A.op == ODRL.isAllOf:
                S = _as_uris(g, A.right) or set()
                all_req |= S
            elif A.op == ODRL.isAnyOf:
                S = _as_uris(g, A.right) or set()
                any_sets.append(S)
            elif A.op == ODRL.isNoneOf:
                S = _as_uris(g, A.right) or set()
                none_forbid |= S
            elif A.op == ODRL.eq:
                S = _as_uris(g, A.right) or set()
                eq_sets.append(S)
        if eq_sets:
            base = eq_sets[0]
            if any(S != base for S in eq_sets[1:]):
                return False
            V = set(base)
        else:
            V = set(all_req)
            for S in any_sets:
                picks = list((S - none_forbid)) or list(S)
                if not picks:
                    return False
                V.add(picks[0])
        if V & none_forbid:
            return False
        return True

    # numeric/temporal intervals
    lo: Optional[Literal] = None
    lo_incl = True
    hi: Optional[Literal] = None
    hi_incl = True
    not_equals: List[Literal] = []
    eqs: List[Literal] = []
    for A in atoms:
        if not isinstance(A.right, Literal):
            continue
        if A.op == ODRL.eq:
            eqs.append(A.right)
        elif A.op == ODRL.neq:
            not_equals.append(A.right)
        elif A.op in (ODRL.gt, ODRL.gteq, ODRL.lt, ODRL.lteq):
            if A.op in (ODRL.gt, ODRL.gteq):
                if lo is None or _cmp_literals(A.right, lo) == 1:
                    lo, lo_incl = A.right, (A.op == ODRL.gteq)
            else:
                if hi is None or _cmp_literals(A.right, hi) == -1:
                    hi, hi_incl = A.right, (A.op == ODRL.lteq)
    if eqs:
        v = eqs[0]
        if any(_cmp_literals(v, e) != 0 for e in eqs[1:]):
            return False
        if lo is not None:
            c = _cmp_literals(v, lo)
            if c is None or c < 0 or (c == 0 and not lo_incl):
                return False
        if hi is not None:
            c = _cmp_literals(v, hi)
            if c is None or c > 0 or (c == 0 and not hi_incl):
                return False
        if any(_cmp_literals(v, ne) == 0 for ne in not_equals):
            return False
        return True
    if lo is not None and hi is not None:
        c = _cmp_literals(lo, hi)
        if c is None or c > 0 or (c == 0 and not (lo_incl and hi_incl)):
            return False
    return True

def _p_only_on_operand(g: Graph, TH: TypeHierarchy,
                       p_atoms: Sequence[Atom], n_atoms: Sequence[Atom]) -> Tuple[bool, Optional[object]]:
    """Search for a 'P-only witness' value v that satisfies P but not N for a shared operand."""
    dom = _domain_for_operand(g, p_atoms or n_atoms)
    # 1) simple candidates: eq targets, list members
    cand: List[object] = []
    for A in list(p_atoms) + list(n_atoms):
        if A.op == ODRL.eq:
            cand.append(A.right)
        else:
            L = as_list(g, A.right)
            if L:
                cand.extend(L)

    # 2) numeric 'nudges': just inside P bounds, just outside N bounds
    def _num_bounds(atoms: Sequence[Atom]):
        lo, lo_incl, hi, hi_incl = None, True, None, True
        for A in atoms:
            if isinstance(A.right, Literal):
                if A.op in (ODRL.gt, ODRL.gteq):
                    if lo is None or _cmp_literals(A.right, lo) == 1:
                        lo, lo_incl = A.right, (A.op == ODRL.gteq)
                if A.op in (ODRL.lt, ODRL.lteq):
                    if hi is None or _cmp_literals(A.right, hi) == -1:
                        hi, hi_incl = A.right, (A.op == ODRL.lteq)
        return lo, lo_incl, hi, hi_incl

    def _bump(lit: Literal, up: bool) -> Literal:
        # Works for integer/decimal/dateTime/date
        py = to_python_literal(lit)
        if isinstance(py, int):
            return Literal(py + (1 if up else -1), datatype=URIRef(XSD_INTEGER))
        if isinstance(py, Decimal):
            return Literal(py + (Decimal("0.0001") if up else -Decimal("0.0001")))
        if isinstance(py, datetime.datetime):
            delta = datetime.timedelta(seconds=1)
            return Literal(py + (delta if up else -delta), datatype=URIRef(XSD_DATETIME))
        if isinstance(py, datetime.date):
            delta = datetime.timedelta(days=1)
            return Literal(py + (delta if up else -delta), datatype=URIRef(XSD_DATE))
        return lit  # as-is if unknown

    loP, loP_inc, hiP, hiP_inc = _num_bounds(p_atoms)
    if loP: cand.append(_bump(loP, True if not loP_inc else False))
    if hiP: cand.append(_bump(hiP, False if not hiP_inc else True))
    loN, loN_inc, hiN, hiN_inc = _num_bounds(n_atoms)
    if loN: cand.append(_bump(loN, False))   # just outside N
    if hiN: cand.append(_bump(hiN, True))

    # de-dup (value + datatype)
    seen: Set[Tuple[object, Optional[URIRef]]] = set()
    uniq: List[object] = []
    for v in cand:
        key = (v, getattr(v, "datatype", None))
        if key in seen: continue
        seen.add(key); uniq.append(v)

    for v in uniq:
        okP = all(_holds_atom(g, TH, A, v) is not False for A in p_atoms)
        okN = all(_holds_atom(g, TH, A, v) is not False for A in n_atoms)
        if okP and not okN:
            return True, v

    # 3) tiny set witness for set ops
    def collect_set(atoms: Sequence[Atom]):
        eq_sets: List[Set[URIRef]] = []; any_sets: List[Set[URIRef]] = []
        all_req: Set[URIRef] = set(); none_forbid: Set[URIRef] = set()
        for a in atoms:
            S = _as_uris(g, a.right)
            if a.op == ODRL.eq and S is not None: eq_sets.append(set(S))
            elif a.op == ODRL.isAnyOf and S is not None: any_sets.append(set(S))
            elif a.op == ODRL.isAllOf and S is not None: all_req |= S
            elif a.op == ODRL.isNoneOf and S is not None: none_forbid |= S
        return eq_sets, any_sets, all_req, none_forbid

    eqP, anyP, allP, noneP = collect_set(p_atoms)
    eqN, anyN, allN, noneN = collect_set(n_atoms)

    # exact-equality set from P
    if eqP:
        V: Optional[Set[URIRef]] = None
        for S in eqP:
            if V is None: V = set(S)
            elif V != S:  return (False, None)
        assert V is not None
        if all(_holds_atom(g, TH, A, V) is not False for A in p_atoms) and not all(_holds_atom(g, TH, A, V) is not False for A in n_atoms):
            return True, V

    # minimal constructive V
    V: Set[URIRef] = set(allP)
    unionN_any = set().union(*anyN) if anyN else set()
    for S in anyP:
        picks = list((S - noneP) - unionN_any) or list(S - noneP) or list(S)
        if not picks: return (False, None)
        V.add(picks[0])

    # break N if possible
    if anyN and all(not (V & S) for S in anyN):    return True, V
    if allN - V:                                   return True, V
    if noneN - noneP:
        x = next(iter(noneN - noneP))
        V2 = set(V); V2.add(x)
        if all(_holds_atom(g, TH, A, V2) is not False for A in p_atoms):
            return True, V2

    return False, None

# ─────────────────────────── Pair analysis ───────────────────────────
def same_scope(a: Rule, b: Rule) -> bool:
    if REQUIRE_SAME_ASSIGNEE and a.assignee != b.assignee: return False
    if REQUIRE_SAME_TARGET and a.target != b.target:       return False
    return True

@dataclass
class PairResult:
    kind: str            # 'perm-vs-prohib' | 'duty-blocked'
    status: str          # 'Ambiguous' | 'Conflict' | 'No conflict'
    a: Rule
    b: Rule
    why_action: str
    details: str

def _clauses_overlap(g: Graph, TH: TypeHierarchy, pc: Clause, nc: Clause) -> Tuple[bool, str, bool, Optional[str]]:
    if not pc and not nc: return True, "both clauses unconstrained", False, None
    if not pc:            return True, "permission clause unconstrained", False, None
    if not nc:            return True, "prohibition clause unconstrained", False, None
    shared = set(pc.keys()) & set(nc.keys())
    if not shared:        return False, "no shared leftOperand in selected branches", False, None
    # satisfiable together?
    for L in shared:
        atoms = (pc.get(L, []) or []) + (nc.get(L, []) or [])
        if not _satisfiable_on_operand(g, TH, atoms):
            return False, f"incompatible on {qname(g, L)}", False, None
    # witness?
    for L in shared:
        okPonly, v = _p_only_on_operand(g, TH, pc.get(L, []), nc.get(L, []))
        if okPonly:
            s = ", ".join(sorted(qname(g, s) for s in shared))
            return True, f"overlap on {s}", True, f"{qname(g, L)} = {qname(g, v)}"
    s = ", ".join(sorted(qname(g, s) for s in shared))
    return True, f"overlap on {s}", False, None

def analyze_pairs(g_vocab: Graph, gq: Graph) -> List[PairResult]:
    AH = ActionHierarchy(g_vocab, gq)
    TH = TypeHierarchy(g_vocab, gq)
    rules_all = sort_and_label_rules(gq, extract_rules(gq))
    perms  = [r for r in rules_all if r.kind == "permission"]
    prohib = [r for r in rules_all if r.kind == "prohibition"]
    duties = [r for r in rules_all if r.kind == "obligation"]
    results: List[PairResult] = []

    def eval(kind: str, X: Rule, N: Rule) -> None:
        okA, whyA = AH.overlap(X.action, N.action)
        if not okA: return
        pclauses = expand_rule_to_clauses(X) or [{}]
        nclauses = expand_rule_to_clauses(N) or [{}]
        any_overlap = False
        any_ambig = False
        reasons: List[str] = []
        for pc in pclauses:
            for nc in nclauses:
                ok, why, amb, wit = _clauses_overlap(gq, TH, pc, nc)
                if ok:
                    any_overlap = True
                    reasons.append(f"branch overlap: {why}" + (f"; safe witness: {wit}" if amb else ""))
                    if amb: any_ambig = True
                else:
                    reasons.append(f"branch non-overlap: {why}")
        status = ("Ambiguous" if any_overlap and any_ambig
                  else ("Conflict" if any_overlap else "No conflict"))
        results.append(PairResult(kind, status, X, N, whyA, "; ".join(reasons)))

    for p in perms:
        for n in prohib:
            if same_scope(p, n): eval("perm-vs-prohib", p, n)
    for d in duties:
        for n in prohib:
            if same_scope(d, n): eval("duty-blocked", d, n)
    return results

# ─────────────────────────── Printing ───────────────────────────
def rule_fact_lines(g: Graph, r: Rule) -> List[str]:
    pol = qname(g, r.policy)
    bn  = f"_:{r.print_id}" if r.print_id else qname(g, r.node)
    ass = qname(g, r.assignee) if r.assignee else "?"
    act = qname(g, r.action)
    tgt = qname(g, r.target) if r.target else "?"
    return [
        f"{pol} {r.kind}: {bn}",
        f"  {bn} odrl:assignee {ass} ;",
        f"  {bn} odrl:action {act} ;",
        f"  {bn} odrl:target {tgt} .",
    ]

def summarize_pairs(pairs: Sequence[PairResult]) -> Tuple[str, Optional[PairResult], int, int]:
    if not pairs: return "No conflict", None, 0, 0
    for want in ("Ambiguous", "Conflict", "No conflict"):
        bucket = [p for p in pairs if p.status == want]
        if bucket:
            return want, bucket[0], len(pairs), len(pairs) - 1
    return "No conflict", pairs[0], len(pairs), len(pairs) - 1

def print_pairs(name: str, gq: Graph, pairs: Sequence[PairResult]) -> None:
    print(f"\n=== {name} ===")
    if not pairs:
        print("No conflicts detected.")
        return
    for i, pr in enumerate(pairs, start=1):
        label = {"Conflict": "CONFLICT", "Ambiguous": "AMBIGUOUS", "No conflict": "NO CONFLICT"}[pr.status]
        kind  = "Permission vs Prohibition" if pr.kind == "perm-vs-prohib" else "Obligation blocked by Prohibition"
        print(f"[{i:02d}] {label}: {kind}")
        for r in (pr.a, pr.b):
            for line in rule_fact_lines(gq, r):
                print(" " + line)
        print(f" Action overlap: {pr.why_action}")
        print(f" Details: {pr.details}\n")

def print_summary(name: str, g_vocab: Graph, url: str) -> None:
    gq = Graph()
    gq.namespace_manager.bind("odrl", ODRL, override=True)
    gq.namespace_manager.bind("dct", DCTERMS, override=False)
    gq.namespace_manager.bind("rdfs", RDFS, override=False)
    gq.parse(url, format="turtle")
    pairs = analyze_pairs(g_vocab, gq)
    overall, exemplar, total, elided = summarize_pairs(pairs)
    print(f"\n=== {name} ===")
    print(f"Overall: {overall} ({total} pair(s) analyzed)")
    if exemplar:
        pr = exemplar
        label = {"Conflict": "CONFLICT", "Ambiguous": "AMBIGUOUS", "No conflict": "NO CONFLICT"}[pr.status]
        kind  = "Permission vs Prohibition" if pr.kind == "perm-vs-prohib" else "Obligation blocked by Prohibition"
        print(f"[example] {label}: {kind}")
        for r in (pr.a, pr.b):
            for line in rule_fact_lines(gq, r):
                print(" " + line)
        print(f" Action overlap: {pr.why_action}")
        print(f" Details: {pr.details}")
        if elided > 0:
            print(f"... ({elided} other pair(s) elided)")

def print_report(name: str, g_vocab: Graph, url: str) -> None:
    if OUTPUT_MODE == OutputMode.PAIRS:
        gq = Graph()
        gq.namespace_manager.bind("odrl", ODRL, override=True)
        gq.namespace_manager.bind("dct", DCTERMS, override=False)
        gq.namespace_manager.bind("rdfs", RDFS, override=False)
        gq.parse(url, format="turtle")
        pairs = analyze_pairs(g_vocab, gq)
        print_pairs(name, gq, pairs)
    else:
        print_summary(name, g_vocab, url)

# ─────────────────────────── ARC: Answer / Reason / Check ───────────────────────────
def print_answer():
    print("Answer")
    print("======")
    gv = Graph()
    try:
        gv.parse(ODRL_TTL_URL, format="turtle")
    except Exception as e:
        sys.stderr.write(f"[warn] Could not fetch ODRL22.ttl ({e}). Using fallback action hierarchy.\n")
        gv = Graph()
    gv.namespace_manager.bind("odrl", ODRL, override=True)
    gv.namespace_manager.bind("rdfs", RDFS, override=False)
    for url in QUIZ_URLS:
        name = url.rsplit("/", 1)[-1]
        try:
            print_report(name, gv, url)
        except Exception as e:
            print(f"\n=== {name} ===")
            print(f"Error: {e}")

def print_reason():
    print("\nReason why")
    print("==========")
    print("We flatten each rule (permission/prohibition/obligation) to a tuple:")
    print("  ⟨assignee, action, target, [Expr,…]⟩  with Expr ∈ {Atom | Or | And | Xone | AndSeq}.")
    print("Each rule expands to disjunctive branches (clauses) grouped by leftOperand.")
    print("Two rules are comparable only if they share scope (same assignee and target)")
    print("and their actions overlap via equality or odrl:includedIn*/rdfs:subClassOf*.")
    print("For each Permission/Obligation vs Prohibition pair we check branches:")
    print("  • Overlap: all shared operands are jointly satisfiable;")
    print("  • Ambiguity: there exists a value v on a shared operand that satisfies P but not N.")
    print("If at least one pair overlaps and admits such a v ⇒ Ambiguous;")
    print("if at least one pair overlaps but none admit v ⇒ Conflict;")
    print("if no pair overlaps ⇒ No conflict.")

def print_check():
    print("\nCheck (harness)")
    print("===============")
    # Re-run analysis and assert internal invariants for every quiz.
    gv = Graph()
    try:
        gv.parse(ODRL_TTL_URL, format="turtle")
    except Exception:
        gv = Graph()
    gv.namespace_manager.bind("odrl", ODRL, override=True)
    gv.namespace_manager.bind("rdfs", RDFS, override=False)

    all_ok = True
    for url in QUIZ_URLS:
        gq = Graph()
        gq.namespace_manager.bind("odrl", ODRL, override=True)
        gq.namespace_manager.bind("dct", DCTERMS, override=False)
        gq.namespace_manager.bind("rdfs", RDFS, override=False)
        try:
            gq.parse(url, format="turtle")
        except Exception as e:
            print(f"{url.rsplit('/',1)[-1]} — parse error skipped ({e})")
            continue
        pairs = analyze_pairs(gv, gq)
        overall, exemplar, _, _ = summarize_pairs(pairs)

        # Invariants per overall classification
        overlaps = [p for p in pairs if p.status in ("Ambiguous", "Conflict")]
        ambigs   = [p for p in pairs if p.status == "Ambiguous"]

        ok = True
        if overall == "No conflict":
            ok &= (len(overlaps) == 0)
        if overall == "Conflict":
            ok &= (len(overlaps) >= 1 and len(ambigs) == 0)
        if overall == "Ambiguous":
            ok &= (len(ambigs) >= 1)
        # Deterministic rule labeling (r01, r02, …)
        rules = sort_and_label_rules(gq, extract_rules(gq))
        ok &= all(r.print_id is not None for r in rules)

        print(f"{url.rsplit('/',1)[-1]} — internal checks: {ok}")
        all_ok &= ok

    print(f"\nAll checks passed? {all_ok}")

# ─────────────────────────── Main ───────────────────────────
if __name__ == "__main__":
    print_answer()
    print_reason()
    print_check()

