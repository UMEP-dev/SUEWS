#!/usr/bin/env python3
"""Guard: Rust YAML preprocessor registry must mirror the Python registry.

Usage
-----
    python scripts/lint/check_rust_yaml_aliases.py

The Rust bridge (`src/suews_bridge/src/field_renames.rs`) keeps a
`FIELD_RENAMES` constant that the YAML preprocessor consults to fold
new-style snake_case keys back to the legacy fused spellings the
hand-written parser indexes by. That constant must stay in lockstep
with the Python-side source of truth, `ALL_FIELD_RENAMES` in
`src/supy/data_model/core/field_renames.py`. If either side drifts,
the CLI and the SuPy/Pydantic pipeline disagree about which YAML
spellings are accepted — exactly the silent-fallback hole gh#1322
closes.

This script imports the Python registry directly (no regex) and
regex-parses the Rust constant (no `cargo` required). It compares
the two as sets of ``(new_name, old_name)`` pairs and exits non-zero
with a per-pair diff when they differ.

Exit codes
----------
* 0 — registries agree.
* 1 — registries disagree (drift).
* 2 — script failure (Python import error, Rust constant missing).
"""

from __future__ import annotations

import argparse
import ast
import os
from pathlib import Path
import re
import sys


_PY_REGISTRY_FILE = Path("src/supy/data_model/core/field_renames.py")
_RUST_REGISTRY_FILE = Path("src/suews_bridge/src/field_renames.rs")


def _collect_str_dict_assignments(tree: ast.Module) -> dict[str, dict[str, str]]:
    """Return every top-level ``NAME = {str: str, ...}`` assignment.

    Walks the module's top-level body (not nested scopes) and collects
    any assignment whose value is a flat mapping from string literals
    to string literals. Handles both ``Assign`` and ``AnnAssign`` nodes
    so the ``NAME: Dict[str, str] = {...}`` idiom used in the registry
    is picked up.
    """
    result: dict[str, dict[str, str]] = {}
    for node in tree.body:
        if isinstance(node, ast.AnnAssign):
            target, value = node.target, node.value
        elif isinstance(node, ast.Assign) and len(node.targets) == 1:
            target, value = node.targets[0], node.value
        else:
            continue
        if not isinstance(target, ast.Name) or not isinstance(value, ast.Dict):
            continue
        pairs: dict[str, str] = {}
        literal = True
        for key_node, value_node in zip(value.keys, value.values):
            if (
                isinstance(key_node, ast.Constant)
                and isinstance(key_node.value, str)
                and isinstance(value_node, ast.Constant)
                and isinstance(value_node.value, str)
            ):
                pairs[key_node.value] = value_node.value
            else:
                literal = False
                break
        if literal:
            result[target.id] = pairs
    return result


def _load_python_registry() -> dict[str, str]:
    """Return Python ALL_FIELD_RENAMES as a {new_name: old_name} dict.

    Parses ``src/supy/data_model/core/field_renames.py`` with :mod:`ast`
    rather than importing ``supy``. Importing the package pulls in
    ``supy._version_scm`` (generated at build time via ``make dev``),
    which is absent in a bare CI checkout and would make this lint
    depend on a full Rust+Fortran build — well out of scope for a
    registry-parity check.

    The Python registry stores ``{old_name: new_name}`` and composes
    ``ALL_FIELD_RENAMES`` from per-class sub-dicts via ``**`` unpacking.
    This function resolves each ``**SUBDICT`` reference and inverts
    the combined mapping to ``{new_name: old_name}`` to match the
    Rust constant's pair direction.
    """
    try:
        source = _PY_REGISTRY_FILE.read_text(encoding="utf-8")
    except FileNotFoundError as exc:
        raise SystemExit(
            f"[rust-yaml-aliases-audit] missing Python registry file: {_PY_REGISTRY_FILE}"
        ) from exc

    tree = ast.parse(source, filename=str(_PY_REGISTRY_FILE))
    subdicts = _collect_str_dict_assignments(tree)

    all_renames_node: ast.Dict | None = None
    for node in tree.body:
        if (
            isinstance(node, ast.AnnAssign)
            and isinstance(node.target, ast.Name)
            and node.target.id == "ALL_FIELD_RENAMES"
            and isinstance(node.value, ast.Dict)
        ):
            all_renames_node = node.value
            break
    if all_renames_node is None:
        raise SystemExit(
            f"[rust-yaml-aliases-audit] could not locate ALL_FIELD_RENAMES in "
            f"{_PY_REGISTRY_FILE}."
        )

    combined: dict[str, str] = {}
    for key_node, value_node in zip(all_renames_node.keys, all_renames_node.values):
        if key_node is not None:
            raise SystemExit(
                "[rust-yaml-aliases-audit] unexpected direct entry in "
                "ALL_FIELD_RENAMES — the lint only supports the documented "
                "`**SUBDICT` unpacking pattern."
            )
        if not isinstance(value_node, ast.Name):
            raise SystemExit(
                "[rust-yaml-aliases-audit] ALL_FIELD_RENAMES must unpack "
                "named sub-dicts only."
            )
        sub_pairs = subdicts.get(value_node.id)
        if sub_pairs is None:
            raise SystemExit(
                f"[rust-yaml-aliases-audit] could not resolve sub-dict "
                f"{value_node.id!r} referenced from ALL_FIELD_RENAMES."
            )
        combined.update(sub_pairs)

    if not combined:
        raise SystemExit(
            "[rust-yaml-aliases-audit] ALL_FIELD_RENAMES resolved to an empty "
            "mapping — check the Python registry source."
        )

    # Invert: Python stores {old: new}; we want {new: old}.
    return {new: old for old, new in combined.items()}


_RUST_CONST_PATTERN = re.compile(
    r"pub\s+const\s+FIELD_RENAMES\s*:\s*&\[\(&str,\s*&str\)\]\s*=\s*&\[(?P<body>.*?)\];",
    re.DOTALL,
)
_RUST_PAIR_PATTERN = re.compile(
    r'\(\s*"(?P<new>[^"]+)"\s*,\s*"(?P<old>[^"]+)"\s*\)'
)


def _load_rust_registry() -> dict[str, str]:
    """Return the Rust FIELD_RENAMES constant as a {new_name: old_name} dict.

    The constant body is parsed with a regex rather than executed — that
    keeps the lint free of a Rust toolchain dependency and unambiguous
    for CI environments that only ship Python.
    """
    try:
        source = _RUST_REGISTRY_FILE.read_text(encoding="utf-8")
    except FileNotFoundError as exc:
        raise SystemExit(
            f"[rust-yaml-aliases-audit] missing Rust registry file: {_RUST_REGISTRY_FILE}"
        ) from exc

    match = _RUST_CONST_PATTERN.search(source)
    if match is None:
        raise SystemExit(
            f"[rust-yaml-aliases-audit] could not locate FIELD_RENAMES constant in "
            f"{_RUST_REGISTRY_FILE}. Check that the declaration still starts with "
            "`pub const FIELD_RENAMES: &[(&str, &str)] = &[` and ends with `];`."
        )

    body = match.group("body")
    pairs = _RUST_PAIR_PATTERN.findall(body)
    rust_renames: dict[str, str] = {}
    for new, old in pairs:
        if new in rust_renames and rust_renames[new] != old:
            raise SystemExit(
                f"[rust-yaml-aliases-audit] duplicate new-name entry in Rust "
                f"registry: {new!r} -> ({rust_renames[new]!r}, {old!r})"
            )
        rust_renames[new] = old
    return rust_renames


def _format_diff(
    python_renames: dict[str, str], rust_renames: dict[str, str]
) -> str:
    """Return a human-readable per-pair diff between the two registries."""
    lines: list[str] = []
    missing_from_rust = sorted(set(python_renames) - set(rust_renames))
    missing_from_python = sorted(set(rust_renames) - set(python_renames))
    mismatched = sorted(
        name
        for name in set(python_renames) & set(rust_renames)
        if python_renames[name] != rust_renames[name]
    )

    if missing_from_rust:
        lines.append("Present in Python, missing in Rust:")
        for name in missing_from_rust:
            lines.append(f"  + {name!r} -> {python_renames[name]!r}")
    if missing_from_python:
        lines.append("Present in Rust, missing in Python:")
        for name in missing_from_python:
            lines.append(f"  - {name!r} -> {rust_renames[name]!r}")
    if mismatched:
        lines.append("Legacy-name mismatch:")
        for name in mismatched:
            lines.append(
                f"  ! {name!r}: python -> {python_renames[name]!r}, "
                f"rust -> {rust_renames[name]!r}"
            )
    return "\n".join(lines)


def main(argv: list[str] | None = None) -> int:
    """CLI entry point — returns a process exit code (0/1/2)."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.parse_args(argv)

    python_renames = _load_python_registry()
    rust_renames = _load_rust_registry()

    if python_renames == rust_renames:
        print(
            "[rust-yaml-aliases-audit] OK: "
            f"{len(python_renames)} (new, old) pairs match."
        )
        return 0

    diff = _format_diff(python_renames, rust_renames)
    print(
        "[rust-yaml-aliases-audit] FAILED — Python ALL_FIELD_RENAMES "
        "and Rust FIELD_RENAMES are out of sync.\n"
        "\n"
        f"{diff}\n"
        "\n"
        "Update the smaller registry to match the larger one, or add "
        "matching entries on both sides. The Python registry lives at "
        "src/supy/data_model/core/field_renames.py; the Rust registry "
        f"lives at {_RUST_REGISTRY_FILE}.",
        file=sys.stderr,
    )
    return 1


if __name__ == "__main__":
    # Pin CWD to the repo root so the relative paths to the Python and
    # Rust registry files resolve correctly even when the script is
    # invoked from a subdirectory.
    repo_root = Path(__file__).resolve().parent.parent.parent
    os.chdir(repo_root)
    sys.exit(main())
