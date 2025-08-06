#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
===============================================================================
self_reflecting_quine.py
===============================================================================

OVERVIEW
--------
This single file does two main things:

  1) Self-metrics
     It reconstructs its own exact source code in memory and reports a suite
     of metrics: line and byte counts, hashes, newline style, trailing
     whitespace, longest line, byte histogram, a rough entropy estimate,
     and Python AST-based statistics (functions, classes, node counts,
     a rough cyclomatic complexity proxy, and top identifiers).

  2) Full-file quine (no file I/O)
     It prints its entire source code without reading from disk. The
     mechanism is a classic self-referential template: a single placeholder
     (written here as %%r when discussed in comments) is replaced by the
     string-literal representation of the template itself.

WHY CARE
--------
Self-reference is the structural seed behind many fundamental limits in logic
and computation. A full-file quine demonstrates controlled self-reference in
code form; self-metrics show practical introspection on that very source.

HOW TO RUN
----------
    python self_reflecting_quine.py

OUTPUT STRUCTURE
----------------
1) SELF-METRICS      — counts, hashes, newline style, entropy, AST stats
2) FULL-FILE QUINE   — exact source printed with no disk reads
3) DONE

IMPORTANT NOTE ABOUT THE TEMPLATE
---------------------------------
Inside the FRAME string below, every literal percent sign must be written as
'%%' so it appears as a single percent in the final printed source. There is
exactly one active placeholder: '%%r'. Do not add lone percent signs inside
FRAME. Outside FRAME (normal Python code) you can use ordinary formatting.
===============================================================================
"""

# The quine template: this entire file’s text lives in FRAME with one placeholder
# %%r that will be replaced (by printf-style formatting) with the repr() of FRAME.
# Any literal percent that should appear in the printed source must be written as
# '%%' here so it survives formatting.
FRAME = '''#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
===============================================================================
self_reflecting_quine.py
===============================================================================

OVERVIEW
--------
This single file does two main things:

  1) Self-metrics
     It reconstructs its own exact source code in memory and reports a suite
     of metrics: line and byte counts, hashes, newline style, trailing
     whitespace, longest line, byte histogram, a rough entropy estimate,
     and Python AST-based statistics (functions, classes, node counts,
     a rough cyclomatic complexity proxy, and top identifiers).

  2) Full-file quine (no file I/O)
     It prints its entire source code without reading from disk. The
     mechanism is a classic self-referential template: a single placeholder
     (written here as %%r when discussed in comments) is replaced by the
     string-literal representation of the template itself.

WHY CARE
--------
Self-reference is the structural seed behind many fundamental limits in logic
and computation. A full-file quine demonstrates controlled self-reference in
code form; self-metrics show practical introspection on that very source.

HOW TO RUN
----------
    python self_reflecting_quine.py

OUTPUT STRUCTURE
----------------
1) SELF-METRICS      — counts, hashes, newline style, entropy, AST stats
2) FULL-FILE QUINE   — exact source printed with no disk reads
3) DONE

IMPORTANT NOTE ABOUT THE TEMPLATE
---------------------------------
Inside the FRAME string below, every literal percent sign must be written as
'%%' so it appears as a single percent in the final printed source. There is
exactly one active placeholder: '%%r'. Do not add lone percent signs inside
FRAME. Outside FRAME (normal Python code) you can use ordinary formatting.
===============================================================================
"""

# The quine template: this entire file’s text lives in FRAME with one placeholder
# %%r that will be replaced (by printf-style formatting) with the repr() of FRAME.
# Any literal percent that should appear in the printed source must be written as
# '%%' here so it survives formatting.
FRAME = %r

import ast
import hashlib
import math
import re
import sys
from collections import Counter
from textwrap import indent

# ------------------------------ Utilities ------------------------------------
def divider(title=None):
    bar = "=" * 72
    if title:
        print(f"\\n{bar}\\n{title}\\n{bar}")
    else:
        print(f"\\n{bar}")

def sha256_hex(data: str) -> str:
    return hashlib.sha256(data.encode("utf-8")).hexdigest()

# ------------------------------ Metrics --------------------------------------
def compute_text_metrics(src: str) -> dict:
    # Basic counts
    lines = src.splitlines()
    total_lines = len(lines)
    total_chars = len(src)
    total_bytes = len(src.encode("utf-8"))
    nonempty = sum(1 for ln in lines if ln.strip())
    blank = total_lines - nonempty
    comment = sum(1 for ln in lines if re.match(r"^\\s*#", ln))

    # Whitespace styles
    trailing_ws = sum(1 for ln in lines if len(ln) and ln.rstrip() != ln)
    leading_tabs = sum(1 for ln in lines if ln.startswith("\\t"))
    has_bom = 1 if src.startswith("\\ufeff") else 0

    # Newline style (heuristic counts)
    crlf = src.count("\\r\\n")
    lf = src.count("\\n") - crlf
    cr = src.count("\\r") - crlf

    # Line lengths
    longest = max((len(ln) for ln in lines), default=0)
    avg_line = (total_chars / total_lines) if total_lines else 0.0

    # Token-ish word counts
    words = re.findall(r"[A-Za-z0-9_]+", src)
    word_count = len(words)
    uniq_words = len(set(w.lower() for w in words))

    # Character classes
    classes = {
        "letters": sum(c.isalpha() for c in src),
        "digits": sum(c.isdigit() for c in src),
        "whitespace": sum(c.isspace() for c in src),
        "punct": total_chars - sum(c.isalnum() or c.isspace() for c in src),
    }

    # Byte stats
    b = src.encode("utf-8")
    byte_counts = Counter(b)
    top_bytes = byte_counts.most_common(10)
    unique_byte_values = len(byte_counts)

    # Rough Shannon entropy (bits per byte)
    total_b = len(b) or 1
    entropy = 0.0
    for _, cnt in byte_counts.items():
        p = cnt / total_b
        entropy -= p * math.log2(p)

    return {
        "total_lines": total_lines,
        "nonempty_lines": nonempty,
        "blank_lines": blank,
        "comment_lines": comment,
        "total_chars": total_chars,
        "total_bytes_utf8": total_bytes,
        "avg_line_length": round(avg_line, 3),
        "longest_line_length": longest,
        "trailing_whitespace_lines": trailing_ws,
        "leading_tab_lines": leading_tabs,
        "has_bom": bool(has_bom),
        "newline_counts": {"CRLF": crlf, "LF": lf, "CR": cr},
        "word_count": word_count,
        "unique_words": uniq_words,
        "char_classes": classes,
        "unique_byte_values": unique_byte_values,
        "entropy_bits_per_byte": round(entropy, 4),
        "top_bytes": top_bytes,
    }

def compute_ast_metrics(src: str) -> dict:
    tree = ast.parse(src)
    counts = Counter(type(n).__name__ for n in ast.walk(tree))

    funcs = [n for n in ast.walk(tree) if isinstance(n, ast.FunctionDef)]
    classes = [n for n in ast.walk(tree) if isinstance(n, ast.ClassDef)]
    imports = [n for n in ast.walk(tree) if isinstance(n, (ast.Import, ast.ImportFrom))]

    # Rough cyclomatic proxy: decisions + boolean ops + comprehensions + 1
    decisions = sum(isinstance(n, (ast.If, ast.For, ast.While, ast.Try, ast.With)) for n in ast.walk(tree))
    bool_ops = sum(isinstance(n, ast.BoolOp) for n in ast.walk(tree))
    comps = sum(isinstance(n, (ast.ListComp, ast.SetComp, ast.DictComp, ast.GeneratorExp)) for n in ast.walk(tree))
    rough_cyclomatic = 1 + decisions + bool_ops + comps

    # Identifier frequencies
    names = [n.id for n in ast.walk(tree) if isinstance(n, ast.Name)]
    top_identifiers = Counter(names).most_common(10)

    # Function line spans (if available)
    def_spans = []
    for n in funcs:
        start = getattr(n, "lineno", None)
        end = getattr(n, "end_lineno", None)
        if start is not None:
            def_spans.append((n.name, start, end))

    # Longest function by line span
    longest_fn = None
    if def_spans:
        longest_fn = max(def_spans, key=lambda t: (t[2] or t[1]) - t[1] + 1 if t[2] else 0)

    return {
        "node_type_counts_top10": counts.most_common(10),
        "function_count": len(funcs),
        "class_count": len(classes),
        "import_count": len(imports),
        "rough_cyclomatic": rough_cyclomatic,
        "top_identifiers": top_identifiers,
        "function_spans": def_spans,
        "longest_function_span": longest_fn,
    }

def print_metrics(src: str):
    divider("SELF-METRICS")
    print(f"SHA-256: {sha256_hex(src)}")

    t = compute_text_metrics(src)
    a = compute_ast_metrics(src)

    print("Text metrics:")
    for k in [
        "total_lines","nonempty_lines","blank_lines","comment_lines",
        "total_chars","total_bytes_utf8","avg_line_length","longest_line_length",
        "trailing_whitespace_lines","leading_tab_lines","has_bom",
        "word_count","unique_words","unique_byte_values","entropy_bits_per_byte",
    ]:
        print(f"  - {k}: {t[k]}")
    print("  - newline_counts:", t["newline_counts"])
    print("  - char_classes:", t["char_classes"])
    print("  - top_bytes (value, count):", t["top_bytes"])

    print("AST metrics:")
    print(f"  - function_count: {a['function_count']}")
    print(f"  - class_count: {a['class_count']}")
    print(f"  - import_count: {a['import_count']}")
    print(f"  - rough_cyclomatic: {a['rough_cyclomatic']}")
    print(f"  - node_type_counts_top10: {a['node_type_counts_top10']}")
    print(f"  - top_identifiers: {a['top_identifiers']}")
    if a["function_spans"]:
        print("  - function_spans:")
        for name, start, end in a["function_spans"]:
            print(f"      * {name}: lines {start}..{end}")
    print(f"  - longest_function_span: {a['longest_function_span']}")

# ------------------------------ Quine ----------------------------------------
def print_full_file_quine():
    divider("FULL-FILE QUINE (no file I/O)")
    # FRAME contains the entire file with a single %%r placeholder.
    # Passing FRAME into its own formatter fills that placeholder with repr(FRAME).
    src = FRAME %% FRAME
    print(src, end="")

# ---------------------------------- Main -------------------------------------
def main():
    # Build the exact source in memory using the quine mechanism:
    SOURCE = FRAME %% FRAME
    # 1) Self-metrics from in-memory source
    print_metrics(SOURCE)
    # 2) Print the full-file quine
    print_full_file_quine()
    divider("DONE")

if __name__ == "__main__":
    main()
'''

import ast
import hashlib
import math
import re
import sys
from collections import Counter
from textwrap import indent

# ------------------------------ Utilities ------------------------------------
def divider(title=None):
    bar = "=" * 72
    if title:
        print(f"\n{bar}\n{title}\n{bar}")
    else:
        print(f"\n{bar}")

def sha256_hex(data: str) -> str:
    return hashlib.sha256(data.encode("utf-8")).hexdigest()

# ------------------------------ Metrics --------------------------------------
def compute_text_metrics(src: str) -> dict:
    # Basic counts
    lines = src.splitlines()
    total_lines = len(lines)
    total_chars = len(src)
    total_bytes = len(src.encode("utf-8"))
    nonempty = sum(1 for ln in lines if ln.strip())
    blank = total_lines - nonempty
    comment = sum(1 for ln in lines if re.match(r"^\s*#", ln))

    # Whitespace styles
    trailing_ws = sum(1 for ln in lines if len(ln) and ln.rstrip() != ln)
    leading_tabs = sum(1 for ln in lines if ln.startswith("\t"))
    has_bom = 1 if src.startswith("\ufeff") else 0

    # Newline style (heuristic counts)
    crlf = src.count("\r\n")
    lf = src.count("\n") - crlf
    cr = src.count("\r") - crlf

    # Line lengths
    longest = max((len(ln) for ln in lines), default=0)
    avg_line = (total_chars / total_lines) if total_lines else 0.0

    # Token-ish word counts
    words = re.findall(r"[A-Za-z0-9_]+", src)
    word_count = len(words)
    uniq_words = len(set(w.lower() for w in words))

    # Character classes
    classes = {
        "letters": sum(c.isalpha() for c in src),
        "digits": sum(c.isdigit() for c in src),
        "whitespace": sum(c.isspace() for c in src),
        "punct": total_chars - sum(c.isalnum() or c.isspace() for c in src),
    }

    # Byte stats
    b = src.encode("utf-8")
    byte_counts = Counter(b)
    top_bytes = byte_counts.most_common(10)
    unique_byte_values = len(byte_counts)

    # Rough Shannon entropy (bits per byte)
    total_b = len(b) or 1
    entropy = 0.0
    for _, cnt in byte_counts.items():
        p = cnt / total_b
        entropy -= p * math.log2(p)

    return {
        "total_lines": total_lines,
        "nonempty_lines": nonempty,
        "blank_lines": blank,
        "comment_lines": comment,
        "total_chars": total_chars,
        "total_bytes_utf8": total_bytes,
        "avg_line_length": round(avg_line, 3),
        "longest_line_length": longest,
        "trailing_whitespace_lines": trailing_ws,
        "leading_tab_lines": leading_tabs,
        "has_bom": bool(has_bom),
        "newline_counts": {"CRLF": crlf, "LF": lf, "CR": cr},
        "word_count": word_count,
        "unique_words": uniq_words,
        "char_classes": classes,
        "unique_byte_values": unique_byte_values,
        "entropy_bits_per_byte": round(entropy, 4),
        "top_bytes": top_bytes,
    }

def compute_ast_metrics(src: str) -> dict:
    tree = ast.parse(src)
    counts = Counter(type(n).__name__ for n in ast.walk(tree))

    funcs = [n for n in ast.walk(tree) if isinstance(n, ast.FunctionDef)]
    classes = [n for n in ast.walk(tree) if isinstance(n, ast.ClassDef)]
    imports = [n for n in ast.walk(tree) if isinstance(n, (ast.Import, ast.ImportFrom))]

    # Rough cyclomatic proxy: decisions + boolean ops + comprehensions + 1
    decisions = sum(isinstance(n, (ast.If, ast.For, ast.While, ast.Try, ast.With)) for n in ast.walk(tree))
    bool_ops = sum(isinstance(n, ast.BoolOp) for n in ast.walk(tree))
    comps = sum(isinstance(n, (ast.ListComp, ast.SetComp, ast.DictComp, ast.GeneratorExp)) for n in ast.walk(tree))
    rough_cyclomatic = 1 + decisions + bool_ops + comps

    # Identifier frequencies
    names = [n.id for n in ast.walk(tree) if isinstance(n, ast.Name)]
    top_identifiers = Counter(names).most_common(10)

    # Function line spans (if available)
    def_spans = []
    for n in funcs:
        start = getattr(n, "lineno", None)
        end = getattr(n, "end_lineno", None)
        if start is not None:
            def_spans.append((n.name, start, end))

    # Longest function by line span
    longest_fn = None
    if def_spans:
        longest_fn = max(def_spans, key=lambda t: (t[2] or t[1]) - t[1] + 1 if t[2] else 0)

    return {
        "node_type_counts_top10": counts.most_common(10),
        "function_count": len(funcs),
        "class_count": len(classes),
        "import_count": len(imports),
        "rough_cyclomatic": rough_cyclomatic,
        "top_identifiers": top_identifiers,
        "function_spans": def_spans,
        "longest_function_span": longest_fn,
    }

def print_metrics(src: str):
    divider("SELF-METRICS")
    print(f"SHA-256: {sha256_hex(src)}")

    t = compute_text_metrics(src)
    a = compute_ast_metrics(src)

    print("Text metrics:")
    for k in [
        "total_lines","nonempty_lines","blank_lines","comment_lines",
        "total_chars","total_bytes_utf8","avg_line_length","longest_line_length",
        "trailing_whitespace_lines","leading_tab_lines","has_bom",
        "word_count","unique_words","unique_byte_values","entropy_bits_per_byte",
    ]:
        print(f"  - {k}: {t[k]}")
    print("  - newline_counts:", t["newline_counts"])
    print("  - char_classes:", t["char_classes"])
    print("  - top_bytes (value, count):", t["top_bytes"])

    print("AST metrics:")
    print(f"  - function_count: {a['function_count']}")
    print(f"  - class_count: {a['class_count']}")
    print(f"  - import_count: {a['import_count']}")
    print(f"  - rough_cyclomatic: {a['rough_cyclomatic']}")
    print(f"  - node_type_counts_top10: {a['node_type_counts_top10']}")
    print(f"  - top_identifiers: {a['top_identifiers']}")
    if a["function_spans"]:
        print("  - function_spans:")
        for name, start, end in a["function_spans"]:
            print(f"      * {name}: lines {start}..{end}")
    print(f"  - longest_function_span: {a['longest_function_span']}")

# ------------------------------ Quine ----------------------------------------
def print_full_file_quine():
    divider("FULL-FILE QUINE (no file I/O)")
    # FRAME contains the entire file with a single %r placeholder.
    # Passing FRAME into its own formatter fills that placeholder with repr(FRAME).
    src = FRAME % FRAME
    print(src, end="")

# ---------------------------------- Main -------------------------------------
def main():
    # Build the exact source in memory using the quine mechanism:
    SOURCE = FRAME % FRAME
    # 1) Self-metrics from in-memory source
    print_metrics(SOURCE)
    # 2) Print the full-file quine
    print_full_file_quine()
    divider("DONE")

if __name__ == "__main__":
    main()

