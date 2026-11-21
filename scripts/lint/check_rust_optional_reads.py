#!/usr/bin/env python3
"""Static ratchet: legacy optional YAML readers in the Rust bridge can only go down.

The Rust bridge YAML loader (`src/suews_bridge/src/yaml_config.rs`) used to start
every struct from a zero-initialised Fortran shadow and overwrite only keys that
matched an optional reader (`read_numeric`, `read_i32`, `read_string`,
`read_numeric_sequence`, `read_sized_numeric_sequence`). Any omitted or `null`
field silently stayed at `0.0` (#1991, #2077).

PR #2045 added the strict `require_*` readers; #2002 migrates the read sites
block by block. This lint pins per-function counts of the legacy optional readers
against `scripts/lint/rust_optional_reads_baseline.json`:

- a count above its baseline fails: use `require_numeric`, `require_i32`,
  `require_string`, `require_sized_numeric_sequence` or `require_sequence`
  instead;
- a count below its baseline also fails, so the baseline cannot drift above the
  tree: lower it in the same diff with `--write-baseline`.

Only production code (excluding `#[cfg(test)]`-annotated items) is scanned;
comments, string literals, and the optional-reader helper definitions
themselves are excluded.

Exits 0 when every count equals its baseline, 1 otherwise.
"""

from __future__ import annotations

import argparse
import json
from pathlib import Path
import re
import sys

TARGET_PATH = Path("src") / "suews_bridge" / "src" / "yaml_config.rs"
BASELINE_PATH = Path("scripts") / "lint" / "rust_optional_reads_baseline.json"

GATED_READERS: tuple[str, ...] = (
    "read_numeric",
    "read_i32",
    "read_string",
    "read_numeric_sequence",
    "read_sized_numeric_sequence",
)

# Definitions of the legacy optional readers themselves (plus thin wrappers that
# only forward finiteness). Callers inside these functions are implementation
# details of the helpers, not YAML read sites.
HELPER_FUNCTIONS: frozenset[str] = frozenset({
    "read_numeric",
    "read_finite_numeric",
    "read_i32",
    "read_string",
    "read_numeric_sequence",
    "read_sized_numeric_sequence",
})

_CALL_PATTERN = re.compile(r"(?<!\w)(" + "|".join(GATED_READERS) + r")\s*\(")
_FN_DEF_PATTERN = re.compile(
    r"\b(?:pub(?:\([^)]*\))?\s+)?fn\s+([A-Za-z_][A-Za-z0-9_]*)\b"
)
_CFG_TEST_PATTERN = re.compile(r"#\s*\[\s*cfg\s*\(\s*test\s*\)\s*\]")
_IDENT_PATTERN = re.compile(r"[A-Za-z_][A-Za-z0-9_]*")

REMEDIATION = """\
New YAML read sites in src/suews_bridge/src/yaml_config.rs must use the strict
require_* readers (see #1991, #2002, #2077 and
.claude/rules/rust/bridge-contract.md), not legacy optional read_* fallbacks.

  - count went up    ->  replace with require_numeric / require_i32 /
                         require_string / require_sized_numeric_sequence /
                         require_sequence (or handle inactive options explicitly
                         per BR-03).
  - count went down  ->  thank you; lower the baseline in this diff:
                         python scripts/lint/check_rust_optional_reads.py \\
                           --write-baseline\
"""


def strip_rust_comments_and_strings(source: str) -> str:
    """Replace Rust comments and string/char literals with spaces, preserving lines.

    Handles line comments (`//`), nested block comments (`/* ... */`), normal
    and byte string literals, raw string literals (`r#"..."#`), and char
    literals without confusing Rust lifetime annotations (`'a`, `'static`).
    """
    out: list[str] = []
    n = len(source)
    i = 0
    block_depth = 0

    while i < n:
        ch = source[i]

        if block_depth > 0:
            if ch == "/" and i + 1 < n and source[i + 1] == "*":
                block_depth += 1
                out.extend((" ", " "))
                i += 2
            elif ch == "*" and i + 1 < n and source[i + 1] == "/":
                block_depth -= 1
                out.extend((" ", " "))
                i += 2
            elif ch == "\n":
                out.append("\n")
                i += 1
            else:
                out.append(" ")
                i += 1
            continue

        # Line comment
        if ch == "/" and i + 1 < n and source[i + 1] == "/":
            while i < n and source[i] != "\n":
                out.append(" ")
                i += 1
            continue

        # Block comment start
        if ch == "/" and i + 1 < n and source[i + 1] == "*":
            block_depth = 1
            out.extend((" ", " "))
            i += 2
            continue

        # Raw string literal: r"...", r#"..."#, br#"..."#
        if (ch == "r" or (ch == "b" and i + 1 < n and source[i + 1] == "r")) and (
            i == 0 or not (source[i - 1].isalnum() or source[i - 1] == "_")
        ):
            j = i + (2 if ch == "b" else 1)
            hashes = 0
            while j < n and source[j] == "#":
                hashes += 1
                j += 1
            if j < n and source[j] == '"':
                out.extend(" " * (j - i + 1))
                i = j + 1
                closing = '"' + "#" * hashes
                while i < n:
                    if source.startswith(closing, i):
                        out.extend(" " * len(closing))
                        i += len(closing)
                        break
                    out.append("\n" if source[i] == "\n" else " ")
                    i += 1
                continue

        # Normal or byte string literal
        if ch == '"':
            out.append(" ")
            i += 1
            while i < n:
                c = source[i]
                if c == "\\" and i + 1 < n:
                    out.append(" ")
                    out.append("\n" if source[i + 1] == "\n" else " ")
                    i += 2
                elif c == '"':
                    out.append(" ")
                    i += 1
                    break
                else:
                    out.append("\n" if c == "\n" else " ")
                    i += 1
            continue

        # Character literal (not a lifetime like `'a` or `'static`)
        if ch == "'":
            if i + 2 < n and source[i + 1] != "\\" and source[i + 2] == "'":
                out.extend((" ", " ", " "))
                i += 3
                continue
            if i + 3 < n and source[i + 1] == "\\":
                end = i + 2
                while end < n and source[end] not in ("'", "\n") and end - i <= 10:
                    end += 1
                if end < n and source[end] == "'":
                    out.extend(" " * (end - i + 1))
                    i = end + 1
                    continue

        out.append(ch)
        i += 1

    return "".join(out)


def _skip_ws(text: str, pos: int) -> int:
    n = len(text)
    while pos < n and text[pos].isspace():
        pos += 1
    return pos


def _skip_balanced(text: str, pos: int, open_ch: str, close_ch: str) -> int:
    """Advance past a balanced `open_ch ... close_ch` group starting at `pos`."""
    n = len(text)
    depth = 1
    pos += 1
    while pos < n and depth > 0:
        if text[pos] == open_ch:
            depth += 1
        elif text[pos] == close_ch:
            depth -= 1
        pos += 1
    return pos


def _skip_outer_attributes(text: str, pos: int) -> int:
    """Skip any stacked outer `#[...]` attributes starting at `pos`."""
    n = len(text)
    pos = _skip_ws(text, pos)
    while pos < n and text[pos] == "#":
        bracket = _skip_ws(text, pos + 1)
        if bracket >= n or text[bracket] != "[":
            break
        pos = _skip_ws(text, _skip_balanced(text, bracket, "[", "]"))
    return pos


def _is_semicolon_terminated_item(text: str, pos: int) -> bool:
    """Return True for items that end at `;` even when they contain `{...}`."""
    n = len(text)
    pos = _skip_ws(text, pos)
    match = _IDENT_PATTERN.match(text, pos)
    if not match:
        return False
    kw = match.group(0)
    pos = _skip_ws(text, match.end())
    if kw == "pub":
        if pos < n and text[pos] == "(":
            pos = _skip_ws(text, _skip_balanced(text, pos, "(", ")"))
        match = _IDENT_PATTERN.match(text, pos)
        if not match:
            return False
        kw = match.group(0)
        pos = _skip_ws(text, match.end())

    if kw in {"use", "type", "static", "let"}:
        return True
    if kw == "const":
        next_ident = _IDENT_PATTERN.match(text, pos)
        return not (next_ident and next_ident.group(0) in {"fn", "unsafe", "extern"})
    return False


def _skip_to_semicolon(text: str, pos: int) -> int:
    """Scan past the terminating `;` at depth 0, respecting nested delimiters."""
    n = len(text)
    j = pos
    paren = bracket = brace = 0
    while j < n:
        ch = text[j]
        if ch == "(":
            paren += 1
        elif ch == ")":
            paren = max(0, paren - 1)
        elif ch == "[":
            bracket += 1
        elif ch == "]":
            bracket = max(0, bracket - 1)
        elif ch == "{":
            brace += 1
        elif ch == "}":
            if paren == 0 and bracket == 0 and brace == 0:
                return j
            brace = max(0, brace - 1)
        elif ch == ";" and paren == 0 and bracket == 0 and brace == 0:
            return j + 1
        j += 1
    return n


def _skip_annotated_item(text: str, pos: int) -> int:
    """Return the index immediately after the item annotated by `#[cfg(test)]`."""
    n = len(text)
    j = _skip_outer_attributes(text, pos)
    if j >= n:
        return n
    if _is_semicolon_terminated_item(text, j):
        return _skip_to_semicolon(text, j)

    paren = bracket = 0
    while j < n:
        ch = text[j]
        if ch == "(":
            paren += 1
        elif ch == ")":
            paren = max(0, paren - 1)
        elif ch == "[":
            bracket += 1
        elif ch == "]":
            bracket = max(0, bracket - 1)
        elif paren == 0 and bracket == 0:
            if ch == ";":
                return j + 1
            if ch == "}":
                return j
            if ch == "{":
                return _skip_balanced(text, j, "{", "}")
        j += 1
    return n


def _strip_cfg_test(cleaned: str) -> str:
    """Strip each `#[cfg(test)]`-annotated item while preserving surrounding code."""
    out: list[str] = []
    n = len(cleaned)
    i = 0

    while i < n:
        if cleaned[i] == "#":
            match = _CFG_TEST_PATTERN.match(cleaned, i)
            if match:
                end = _skip_annotated_item(cleaned, match.end())
                out.extend("\n" if c == "\n" else " " for c in cleaned[i:end])
                i = end
                continue
        out.append(cleaned[i])
        i += 1

    return "".join(out)


def _iter_top_level_functions(cleaned: str) -> tuple[list[tuple[str, str]], str]:
    """Return `([(fn_name, body_text), ...], residual_text)` for top-level fns."""
    functions: list[tuple[str, str]] = []
    residual_parts: list[str] = []
    n = len(cleaned)
    depth = 0
    i = 0

    while i < n:
        ch = cleaned[i]
        if depth == 0:
            match_fn = _FN_DEF_PATTERN.match(cleaned, i)
            if match_fn and (
                i == 0 or not (cleaned[i - 1].isalnum() or cleaned[i - 1] == "_")
            ):
                fn_name = match_fn.group(1)
                j = match_fn.end()
                paren_depth = 0
                while j < n:
                    cj = cleaned[j]
                    if cj == "(":
                        paren_depth += 1
                    elif cj == ")":
                        paren_depth = max(0, paren_depth - 1)
                    elif cj == "{" and paren_depth == 0:
                        break
                    elif cj == ";" and paren_depth == 0:
                        j = n
                        break
                    j += 1
                if j < n and cleaned[j] == "{":
                    body_start = j + 1
                    body_depth = 1
                    k = body_start
                    while k < n and body_depth > 0:
                        if cleaned[k] == "{":
                            body_depth += 1
                        elif cleaned[k] == "}":
                            body_depth -= 1
                        k += 1
                    functions.append((fn_name, cleaned[body_start : k - 1]))
                    i = k
                    continue
        if ch == "{":
            depth += 1
        elif ch == "}":
            depth = max(0, depth - 1)
        residual_parts.append(ch)
        i += 1

    return functions, "".join(residual_parts)


def count_optional_reads(source: str) -> dict[str, dict[str, int]]:
    """Count legacy optional reader calls per enclosing top-level function.

    Parameters
    ----------
    source : str
        Rust source text of `yaml_config.rs`.

    Returns
    -------
    dict[str, dict[str, int]]
        Non-zero counts keyed by enclosing top-level function name (or
        ``<other>`` for calls inside ``impl``/``mod`` blocks or macros), then
        reader name.
    """
    cleaned = _strip_cfg_test(strip_rust_comments_and_strings(source))

    counts: dict[str, dict[str, int]] = {}
    functions, residual = _iter_top_level_functions(cleaned)
    for fn_name, body in functions:
        if fn_name in HELPER_FUNCTIONS:
            continue
        fn_counts = counts.setdefault(fn_name, {})
        for match in _CALL_PATTERN.finditer(body):
            reader = match.group(1)
            fn_counts[reader] = fn_counts.get(reader, 0) + 1

    for match in _CALL_PATTERN.finditer(residual):
        reader = match.group(1)
        other_counts = counts.setdefault("<other>", {})
        other_counts[reader] = other_counts.get(reader, 0) + 1

    return {
        fn: dict(sorted(readers.items()))
        for fn, readers in sorted(counts.items())
        if readers
    }


def compare(
    actual: dict[str, dict[str, int]], baseline: dict[str, dict[str, int]]
) -> tuple[list[str], list[str]]:
    """Return `(increases, decreases)` as human-readable lines."""
    increases: list[str] = []
    decreases: list[str] = []
    for fn in sorted(set(actual) | set(baseline)):
        found = actual.get(fn, {})
        allowed = baseline.get(fn, {})
        for reader in sorted(set(found) | set(allowed)):
            now = found.get(reader, 0)
            then = allowed.get(reader, 0)
            if now > then:
                increases.append(f"{fn}: {reader} {then} -> {now}")
            elif now < then:
                decreases.append(f"{fn}: {reader} {then} -> {now}")
    return increases, decreases


def _totals(counts: dict[str, dict[str, int]]) -> dict[str, int]:
    totals: dict[str, int] = {}
    for per_fn in counts.values():
        for reader, n in per_fn.items():
            totals[reader] = totals.get(reader, 0) + n
    return dict(sorted(totals.items()))


def _write_baseline(path: Path, counts: dict[str, dict[str, int]]) -> None:
    payload = {
        "description": (
            "Per-function counts of legacy optional YAML readers in "
            "src/suews_bridge/src/yaml_config.rs; see "
            "scripts/lint/check_rust_optional_reads.py. Counts may only go down."
        ),
        "totals": _totals(counts),
        "functions": counts,
    }
    path.write_text(json.dumps(payload, indent=2) + "\n", encoding="utf-8")


def _load_baseline(path: Path) -> dict[str, dict[str, int]] | None:
    """Load the baseline JSON and confirm recorded totals match per-function sums."""
    if not path.is_file():
        print(f"[X] baseline {BASELINE_PATH.as_posix()} not found", file=sys.stderr)
        return None
    payload = json.loads(path.read_text(encoding="utf-8"))
    functions = payload["functions"]
    expected_totals = _totals(functions)
    recorded_totals = payload.get("totals")
    if recorded_totals != expected_totals:
        print(
            "[X] baseline totals out of sync with per-function counts: "
            f"recorded={recorded_totals}, expected={expected_totals}",
            file=sys.stderr,
        )
        print("", file=sys.stderr)
        print(REMEDIATION, file=sys.stderr)
        return None
    return functions


def _print_report(counts: dict[str, dict[str, int]]) -> None:
    totals = _totals(counts)
    print("Legacy optional readers (totals):")
    for reader, n in totals.items():
        print(f"  {reader:30s} {n:5d}")
    print(f"  {'TOTAL':30s} {sum(totals.values()):5d}")
    print("Per function:")
    for fn, readers in counts.items():
        items = ", ".join(f"{k} {v}" for k, v in readers.items())
        print(f"  {fn}: {items}")


def main(argv: list[str]) -> int:
    """Compare `yaml_config.rs` against the baseline, or rewrite or report it."""
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("repo_root", nargs="?", default=".")
    parser.add_argument(
        "--write-baseline",
        action="store_true",
        help="rewrite the baseline from the current tree",
    )
    parser.add_argument(
        "--report",
        action="store_true",
        help="print per-function optional-reader counts, then exit 0",
    )
    args = parser.parse_args(argv[1:])

    repo_root = Path(args.repo_root).resolve()
    target_path = repo_root / TARGET_PATH
    if not target_path.is_file():
        print(
            f"[X] {TARGET_PATH.as_posix()} not found under {repo_root}",
            file=sys.stderr,
        )
        return 1

    actual = count_optional_reads(target_path.read_text(encoding="utf-8"))
    if args.report:
        _print_report(actual)
        return 0

    baseline_path = repo_root / BASELINE_PATH
    if args.write_baseline:
        _write_baseline(baseline_path, actual)
        print(f"[OK] wrote {BASELINE_PATH.as_posix()}: {_totals(actual)}")
        return 0
    baseline = _load_baseline(baseline_path)
    if baseline is None:
        return 1

    increases, decreases = compare(actual, baseline)
    if increases or decreases:
        if increases:
            print(
                "[X] new legacy optional YAML readers in yaml_config.rs:",
                file=sys.stderr,
            )
            for line in increases:
                print(f"  - {line}", file=sys.stderr)
        if decreases:
            print("[X] baseline is above the tree (lower it):", file=sys.stderr)
            for line in decreases:
                print(f"  - {line}", file=sys.stderr)
        print("", file=sys.stderr)
        print(REMEDIATION, file=sys.stderr)
        return 1

    total = sum(_totals(actual).values())
    print(
        f"[OK] Rust bridge optional YAML readers match the baseline "
        f"({total} legacy calls across {len(actual)} functions)."
    )
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
