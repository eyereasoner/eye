#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
codec.py
========

Translate a practical subset of Notation3 (N3/Turtle) ⇄ a plain Python-syntax file.

Features
--------
- @prefix declarations
- Explicit triples
- Abbreviations:
    * predicate-object lists with ';'
    * object lists with ','
- Blank-node property lists: [ ... ]
- Rules:
    * forward  : { ... } => { ... } .
    * backward : { ... } <= { ... } .   (log:isImpliedBy)
    * query    : { ... } =^ { ... } .   (log:impliesAnswer)
- Typed literals and language tags preserved (e.g., "2020-01-01"^^xsd:date, "hello"@en)
- N3 lists ( ... ) treated as a single atomic node (pass-through)

Roundtrip contract
------------------
- N3 → Python: produces a single dict `DOC = { "prefixes": {...}, "triples": [...], "rules": [...] }`.
- Python → N3: prints normalized N3 (sugar expanded), with rule operators preserved.

CLI
---
    # N3 -> Python-syntax file
    python3 codec.py n3-to-py input.n3 output.py

    # Python-syntax file -> N3
    python3 codec.py py-to-n3 input.py output.n3

    # Mini demo (writes example.py and example_out.n3)
    python3 codec.py --selftest

Python “syntax file”
--------------------
The generated .py contains one top-level `DOC` dict:

    DOC = {
      "prefixes": {"": "http://example.org/#", "xsd":"http://www.w3.org/2001/XMLSchema#"},
      "triples": [
        ":Ghent a :City",
        ":Ghent :inCountry :Belgium"
      ],
      "rules": [
        {"if": ["?x a :City"], "then": ["?x a :HumanCommunity"], "direction": "forward"},
        {"if": ["?x a :City", "?x :inCountry :Belgium"], "then": ["?x a :HumanCommunity"], "direction": "backward"},
        {"if": ["?x a :City"], "then": ["?x :answer \"Yes\""], "direction": "query"}
      ]
    }

Edit `DOC` freely and convert back to N3.

Limitations (by design, to keep it lightweight)
-----------------------------------------------
- No collection expansion of `( ... )` into `rdf:first/rest`; lists are passed through as a single term.
- String/escape handling is pragmatic (covers common cases, not every edge of Turtle grammar).
- Abbreviations are **expanded** on parse; pretty-printing back to abbreviated form is not attempted.
- We do not special-case `a` → `rdf:type`; it is left exactly as written.

"""

import argparse
import ast
import pprint
import re
from pathlib import Path
from typing import Dict, List, Tuple, Any


# ---------------------------------------------------------------------------
# Utilities
# ---------------------------------------------------------------------------

PREFIX_RE = re.compile(
    r'^\s*@prefix\s+([A-Za-z0-9_\-]*)?:\s*<([^>]+)>\s*\.\s*$',
    re.IGNORECASE,
)


def strip_comments(text: str) -> str:
    """
    Remove '#' comments from the input, but keep '#' when it appears
    inside <...> IRIs or quoted strings.
    """
    out_lines: List[str] = []
    for line in text.splitlines():
        res: List[str] = []
        in_angle = in_sq = in_dq = False
        i = 0
        while i < len(line):
            ch = line[i]
            if ch == '<' and not in_sq and not in_dq:
                in_angle = True
                res.append(ch)
            elif ch == '>' and in_angle:
                in_angle = False
                res.append(ch)
            elif ch == "'" and not in_angle and not in_dq:
                in_sq = not in_sq
                res.append(ch)
            elif ch == '"' and not in_angle and not in_sq:
                in_dq = not in_dq
                res.append(ch)
            elif ch == '#' and not in_angle and not in_sq and not in_dq:
                break  # comment start
            else:
                res.append(ch)
            i += 1
        out_lines.append(''.join(res))
    return '\n'.join(out_lines)


def smart_split_statements(text: str) -> List[str]:
    """
    Split text on '.' that terminates statements, ignoring dots inside:
      - <...> IRIs
      - "..." and '...'
      - [ ... ]  (blank-node property lists)
      - { ... }  (rule blocks)
      - ( ... )  (lists, kept as a term)
    Returns trimmed statements without the trailing dot.
    """
    out: List[str] = []
    buf: List[str] = []
    in_angle = in_sq = in_dq = False
    depth_brack = depth_brace = depth_paren = 0

    for ch in text:
        if ch == '<' and not in_sq and not in_dq:
            in_angle = True
            buf.append(ch)
            continue
        if ch == '>' and in_angle:
            in_angle = False
            buf.append(ch)
            continue
        if ch == "'" and not in_angle and not in_dq:
            in_sq = not in_sq
            buf.append(ch)
            continue
        if ch == '"' and not in_angle and not in_sq:
            in_dq = not in_dq
            buf.append(ch)
            continue

        if not (in_angle or in_sq or in_dq):
            if ch == '[':
                depth_brack += 1
                buf.append(ch)
                continue
            if ch == ']':
                depth_brack -= 1
                buf.append(ch)
                continue
            if ch == '{':
                depth_brace += 1
                buf.append(ch)
                continue
            if ch == '}':
                depth_brace -= 1
                buf.append(ch)
                continue
            if ch == '(':
                depth_paren += 1
                buf.append(ch)
                continue
            if ch == ')':
                depth_paren -= 1
                buf.append(ch)
                continue
            if ch == '.' and depth_brack == depth_brace == depth_paren == 0:
                stmt = ''.join(buf).strip()
                if stmt:
                    out.append(' '.join(stmt.split()))
                buf = []
                continue

        buf.append(ch)

    tail = ''.join(buf).strip()
    if tail:
        out.append(' '.join(tail.split()))
    return out


# ---------------------------------------------------------------------------
# Tokenizer (Turtle/N3-ish)
# ---------------------------------------------------------------------------

def tokenize(s: str) -> List[str]:
    """
    Tokenize a single statement into a list of tokens.

    Special handling:
      * Parenthesized list '( ... )' is captured as a SINGLE token, accounting
        for nested parentheses and quotes inside.
      * Literals may carry @lang or ^^datatype; these are included as one token.
      * <IRI> is one token.
      * Punctuation tokens: '[', ']', '{', '}', ';', ',', '.'
        (Note: '.' shouldn't appear here because we split statements first,
        but it's kept for completeness.)
    """
    tokens: List[str] = []
    i = 0
    n = len(s)
    WHITESPACE = ' \t\r\n'
    PUNCT = set(['[', ']', '{', '}', ';', ',', '.'])  # '(' handled separately

    while i < n:
        ch = s[i]

        # skip whitespace
        if ch in WHITESPACE:
            i += 1
            continue

        # ( ... ) as a single token
        if ch == '(':
            j = i + 1
            depth = 1
            in_angle = in_sq = in_dq = False
            while j < n and depth > 0:
                cj = s[j]
                if cj == '<' and not in_sq and not in_dq:
                    in_angle = True
                elif cj == '>' and in_angle:
                    in_angle = False
                elif cj == "'" and not in_angle and not in_dq:
                    in_sq = not in_sq
                elif cj == '"' and not in_angle and not in_sq:
                    in_dq = not in_dq
                elif not (in_angle or in_sq or in_dq):
                    if cj == '(':
                        depth += 1
                    elif cj == ')':
                        depth -= 1
                j += 1
            tokens.append(s[i:j])  # includes closing ')'
            i = j
            continue

        # simple punctuation
        if ch in PUNCT:
            tokens.append(ch)
            i += 1
            continue

        # <IRI>
        if ch == '<':
            j = i + 1
            while j < n and s[j] != '>':
                j += 1
            j = min(j + 1, n)
            tokens.append(s[i:j])
            i = j
            continue

        # "literal" [@lang | ^^datatype]
        if ch == '"':
            j = i + 1
            esc = False
            while j < n:
                cj = s[j]
                if esc:
                    esc = False
                    j += 1
                    continue
                if cj == '\\':
                    esc = True
                    j += 1
                    continue
                if cj == '"':
                    j += 1
                    break
                j += 1

            lit = s[i:j]

            # check for suffix after optional whitespace
            k = j
            while k < n and s[k] in WHITESPACE:
                k += 1

            # language tag
            if k < n and s[k] == '@':
                m = k + 1
                while m < n and s[m] not in WHITESPACE + '[],.{}();':
                    m += 1
                tokens.append(s[i:m])
                i = m
                continue

            # ^^datatype (QName or <IRI>)
            if k + 2 <= n and s[k:k + 2] == '^^':
                # ^^<IRI>
                if k + 2 < n and s[k + 2] == '<':
                    m = k + 3
                    while m < n and s[m] != '>':
                        m += 1
                    m = min(m + 1, n)
                    tokens.append(s[i:m])
                    i = m
                    continue
                # ^^QName
                m = k + 2
                while m < n and s[m] not in WHITESPACE + '[],.{}();':
                    m += 1
                tokens.append(s[i:m])
                i = m
                continue

            # plain literal
            tokens.append(lit)
            i = j
            continue

        # 'single-quoted' literal (kept simple)
        if ch == "'":
            j = i + 1
            while j < n and s[j] != "'":
                j += 1
            j = min(j + 1, n)
            tokens.append(s[i:j])
            i = j
            continue

        # bare token (QName, variable, _:bnode, a, numbers, etc.)
        j = i + 1
        while j < n and s[j] not in WHITESPACE and s[j] not in PUNCT and s[j] != '(':
            j += 1
        tokens.append(s[i:j])
        i = j

    return tokens


# ---------------------------------------------------------------------------
# Sugar expansion: ';', ',', and blank-node property lists [ ... ]
# ---------------------------------------------------------------------------

class BNodeGen:
    """Simple blank-node label generator for property list expansion."""
    def __init__(self) -> None:
        self.c = 0

    def new(self) -> str:
        self.c += 1
        return f"_:b{self.c}"


def parse_node(tokens: List[str], i: int, bgen: BNodeGen,
               out_triples: List[Tuple[str, str, str]]) -> Tuple[str, int]:
    """
    Parse a node and return (node_token, next_index).
    Supports nested blank-node property lists [ ... ] by creating a fresh _:bN
    and emitting its internal triples into out_triples.
    """
    if i < len(tokens) and tokens[i] == '[':
        bn = bgen.new()
        i += 1
        i = parse_predicate_object_list(bn, tokens, i, bgen, out_triples, stop_token=']')
        if i < len(tokens) and tokens[i] == ']':
            i += 1
        return bn, i

    if i >= len(tokens):
        raise ValueError("Unexpected end of tokens while parsing a node.")
    node = tokens[i]
    i += 1
    return node, i


def parse_object_list(tokens: List[str], i: int, bgen: BNodeGen,
                      out_triples: List[Tuple[str, str, str]]) -> Tuple[List[str], int]:
    """
    Parse objectList := object ( ',' object )* .
    """
    objs: List[str] = []
    while i < len(tokens):
        if tokens[i] in [']', ';', '.', '}']:
            break
        obj, i = parse_node(tokens, i, bgen, out_triples)
        objs.append(obj)
        if i < len(tokens) and tokens[i] == ',':
            i += 1
            continue
        else:
            break
    return objs, i


def parse_predicate_object_list(subject: str, tokens: List[str], i: int, bgen: BNodeGen,
                                out_triples: List[Tuple[str, str, str]],
                                stop_token: str = None) -> int:
    """
    Parse verb objectList pairs separated by ';' until stop_token is met.
    (Used for both top-level statements and [ ... ] property lists.)
    """
    while i < len(tokens) and (stop_token is None or tokens[i] != stop_token):
        if tokens[i] in [';', ',']:
            i += 1
            continue
        if stop_token is not None and tokens[i] == stop_token:
            break
        verb = tokens[i]
        i += 1
        objs, i = parse_object_list(tokens, i, bgen, out_triples)
        for obj in objs:
            out_triples.append((subject, verb, obj))
        while i < len(tokens) and tokens[i] == ';':
            i += 1
    return i


def expand_statement(stmt: str, bgen: BNodeGen) -> List[Tuple[str, str, str]]:
    """
    Expand one N3/Turtle statement (with sugar) into explicit (s, p, o) triples.
    The subject is a node; the rest is a predicate-object list.
    """
    tokens = tokenize(stmt)
    if not tokens:
        return []
    out: List[Tuple[str, str, str]] = []
    i = 0
    subj, i = parse_node(tokens, i, bgen, out)
    i = parse_predicate_object_list(subj, tokens, i, bgen, out, stop_token=None)
    return out


def expand_block_to_triples(block: str) -> List[str]:
    """
    Expand a block of statements (outside rules, or inside a rule's { ... })
    into a list of explicit triple strings "S P O".
    """
    bgen = BNodeGen()
    triples: List[str] = []
    for stmt in smart_split_statements(block):
        for s, p, o in expand_statement(stmt, bgen):
            triples.append(f"{s} {p} {o}")
    return triples


# ---------------------------------------------------------------------------
# Rule extraction (=>, <=, =^)
# ---------------------------------------------------------------------------

def extract_rules(text: str) -> Tuple[List[Dict[str, Any]], str]:
    """
    Extract top-level rules of the form:

      { ... } => { ... } .
      { ... } <= { ... } .
      { ... } =^ { ... } .

    Returns (rules, remaining_text_without_rules).
    Each rule is a dict:
        {"if": [...], "then": [...], "direction": "forward|backward|query"}
    """
    rules: List[Dict[str, Any]] = []
    rest: List[str] = []
    i = 0
    n = len(text)

    while i < n:
        if text[i] != '{':
            rest.append(text[i])
            i += 1
            continue

        # Parse left block { ... }
        start_left = i
        depth = 1
        i += 1
        while i < n and depth > 0:
            if text[i] == '{':
                depth += 1
            elif text[i] == '}':
                depth -= 1
            i += 1
        if depth:
            # Unbalanced; keep as text
            rest.append(text[start_left])
            continue
        left_inner = text[start_left + 1:i - 1]

        # Operator
        while i < n and text[i].isspace():
            i += 1
        op = text[i:i + 2] if i + 2 <= n else ''
        if op not in ('=>', '<=', '=^'):
            rest.append(text[start_left:i])
            continue
        i += 2

        # Parse right block { ... }
        while i < n and text[i].isspace():
            i += 1
        if i >= n or text[i] != '{':
            rest.append(text[start_left:i])
            continue

        start_right = i
        depth = 1
        i += 1
        while i < n and depth > 0:
            if text[i] == '{':
                depth += 1
            elif text[i] == '}':
                depth -= 1
            i += 1
        if depth:
            rest.append(text[start_left:i])
            continue
        right_inner = text[start_right + 1:i - 1]

        # Optional trailing dot
        while i < n and text[i].isspace():
            i += 1
        if i < n and text[i] == '.':
            i += 1

        # Normalize to ("if","then") with preserved direction
        if op == '=>':
            direction = 'forward'
            ante_raw, cons_raw = left_inner, right_inner
        elif op == '<=':
            direction = 'backward'
            ante_raw, cons_raw = right_inner, left_inner
        else:
            direction = 'query'
            ante_raw, cons_raw = left_inner, right_inner

        ante = expand_block_to_triples(ante_raw)
        cons = expand_block_to_triples(cons_raw)

        rules.append({"if": ante, "then": cons, "direction": direction})

    return rules, ''.join(rest)


# ---------------------------------------------------------------------------
# Public API: N3 <-> DOC
# ---------------------------------------------------------------------------

def n3_to_doc(n3_text: str) -> Dict[str, Any]:
    """
    Parse N3 text to a Python dict with keys:
      - "prefixes": {prefix: iri}
      - "triples":  ["S P O", ...]
      - "rules":    [{"if":[...], "then":[...], "direction": "..."}]
    """
    s = strip_comments(n3_text)
    rules, remainder = extract_rules(s)
    prefixes: Dict[str, str] = {}
    content_lines: List[str] = []

    for line in remainder.splitlines():
        m = PREFIX_RE.match(line.strip())
        if m:
            prefixes[m.group(1) or ""] = m.group(2)
        else:
            content_lines.append(line)

    triples = expand_block_to_triples('\n'.join(content_lines))
    return {"prefixes": prefixes, "triples": triples, "rules": rules}


def doc_to_n3(doc: Dict[str, Any]) -> str:
    """
    Render a DOC dict back to N3.
    Sugar is normalized (explicit triples), and rule operators are preserved.
    """
    lines: List[str] = []

    # prefixes
    for pfx, iri in doc.get("prefixes", {}).items():
        lines.append(f"@prefix {pfx + ':' if pfx else ':'} <{iri}> .")
    if lines:
        lines.append("")

    # triples
    for t in doc.get("triples", []):
        t = t.strip()
        lines.append(t if t.endswith('.') else f"{t} .")
    if doc.get("triples"):
        lines.append("")

    # rules (single-line blocks)
    for r in doc.get("rules", []):
        if_part = ' . '.join(s.rstrip('.') for s in r.get("if", []))
        then_part = ' . '.join(s.rstrip('.') for s in r.get("then", []))
        d = r.get('direction', 'forward')
        if d == 'backward':
            op, left, right = '<=', then_part, if_part
        elif d == 'query':
            op, left, right = '=^', if_part, then_part
        else:
            op, left, right = '=>', if_part, then_part
        if left and not left.endswith('.'):
            left += ' .'
        if right and not right.endswith('.'):
            right += ' .'
        lines.append("{ " + left + " } " + op + " { " + right + " } .")

    return '\n'.join(lines).strip() + '\n'


# ---------------------------------------------------------------------------
# Writer: emit a Python-syntax file with a small helper to print N3
# ---------------------------------------------------------------------------

def write_py(doc: Dict[str, Any], path: Path) -> None:
    """
    Write a Python syntax file that defines DOC and, when executed, prints N3.

    We embed a tiny `_doc_to_n3` inside the generated file so it can be run
    standalone (no import of this module required).
    """
    with open(path, "w", encoding="utf-8") as f:
        f.write("# Generated by codec.py\n")
        f.write("# Edit DOC and run this file to print N3 to stdout.\n\n")
        f.write("DOC = ")
        f.write(pprint.pformat(doc, width=88, sort_dicts=True))
        f.write("\n\n")
        f.write("def _doc_to_n3(doc):\n")
        f.write("    def _trip(s):\n")
        f.write("        s = s.strip()\n")
        f.write("        return s if s.endswith('.') else s + ' .'\n")
        f.write("    lines = []\n")
        f.write("    for pfx, iri in doc.get('prefixes', {}).items():\n")
        f.write("        lines.append('@prefix ' + (pfx+':' if pfx else ':') + ' <' + iri + '> .')\n")
        f.write("    if lines:\n")
        f.write("        lines.append('')\n")
        f.write("    for t in doc.get('triples', []):\n")
        f.write("        lines.append(_trip(t))\n")
        f.write("    if doc.get('triples'):\n")
        f.write("        lines.append('')\n")
        f.write("    for r in doc.get('rules', []):\n")
        f.write("        if_part = ' . '.join(s.rstrip('.') for s in r.get('if', []))\n")
        f.write("        then_part = ' . '.join(s.rstrip('.') for s in r.get('then', []))\n")
        f.write("        d = r.get('direction', 'forward')\n")
        f.write("        if d == 'backward':\n")
        f.write("            op, left, right = '<=', then_part, if_part\n")
        f.write("        elif d == 'query':\n")
        f.write("            op, left, right = '=^', if_part, then_part\n")
        f.write("        else:\n")
        f.write("            op, left, right = '=>', if_part, then_part\n")
        f.write("        if left and not left.endswith('.'):\n")
        f.write("            left += ' .'\n")
        f.write("        if right and not right.endswith('.'):\n")
        f.write("            right += ' .'\n")
        f.write("        lines.append('{ ' + left + ' } ' + op + ' { ' + right + ' } .')\n")
        f.write("    return '\\n'.join(lines).strip() + '\\n'\n\n")
        f.write("if __name__ == '__main__':\n")
        f.write("    print(_doc_to_n3(DOC), end='')\n")


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------

def read_py_to_doc(path: Path) -> dict:
    """
    Parse a generated Python-syntax file and extract the top-level DOC literal
    safely using ast.literal_eval.
    """
    source = Path(path).read_text(encoding='utf-8')
    tree = ast.parse(source, filename=str(path))
    for node in tree.body:
        if isinstance(node, ast.Assign):
            for target in node.targets:
                if isinstance(target, ast.Name) and target.id == 'DOC':
                    return ast.literal_eval(node.value)
    raise ValueError("DOC literal not found in the Python file.")


def main() -> None:
    ap = argparse.ArgumentParser(
        description="Translate subset N3 <-> Python syntax file "
                    "(supports =>, <=, =^, ';' ',', and [ ... ]; lists '( ... )' are preserved)")
    sub = ap.add_subparsers(dest='cmd', required=False)
    ap.add_argument('--selftest', action='store_true', help='Write example.py and example_out.n3')

    sp1 = sub.add_parser('n3-to-py', help='Convert N3 to Python syntax file')
    sp1.add_argument('input_n3')
    sp1.add_argument('output_py')

    sp2 = sub.add_parser('py-to-n3', help='Convert Python syntax file to N3')
    sp2.add_argument('input_py')
    sp2.add_argument('output_n3')

    args = ap.parse_args()

    if args.selftest:
        sample = """@prefix : <http://example.org/#> .
:Ghent a :City ; :inCountry :Belgium ; :hasName "Ghent", "Gent" .
:Antwerp :hasPart [ a :Port ; :name "Antwerp Port" ] .
{ ?x a :City ; :inCountry :Belgium . } => { ?x a :HumanCommunity . } .
{ ?x a :HumanCommunity . } <= { ?x a :City , :UrbanArea ; :inCountry :Belgium . } .
{ ?x a :City ; :inCountry :Belgium . } =^ { ?x :answer "Yes" . } .
"""
        doc = n3_to_doc(sample)
        write_py(doc, Path('example.py'))
        doc2 = read_py_to_doc(Path('example.py'))
        Path('example_out.n3').write_text(doc_to_n3(doc2), encoding='utf-8')
        print("Wrote example.py and example_out.n3")
        return

    if args.cmd == 'n3-to-py':
        doc = n3_to_doc(Path(args.input_n3).read_text(encoding='utf-8'))
        write_py(doc, Path(args.output_py))
    elif args.cmd == 'py-to-n3':
        doc = read_py_to_doc(Path(args.input_py))
        Path(args.output_n3).write_text(doc_to_n3(doc), encoding='utf-8')
    else:
        ap.print_help()


if __name__ == '__main__':
    main()

