#!/usr/bin/env python3
"""Report file-level test-contract tags without importing test modules.

Python tags must be literal ``pytest.mark.contract("C-...", ...)`` calls in
module-level pytestmark (single, list or tuple) or function/class decorators.
Aliases and computed ids are deliberately not evaluated. Rust tags are
standalone ``// contract: C-A, C-B`` lines in files containing ``#[test]``;
this is a textual convention, not a Rust parser or feature-gate analysis.
Counts describe tag presence, not executed tests or proof of a contract.

Report mode always returns zero for findings. Strict mode also fails on
registry/parse/marker errors, since an incomplete scan cannot establish coverage.
"""

import argparse
import ast
import json
from pathlib import Path
import re

import yaml

STAGES = {
    "surface",
    "config",
    "engine_input",
    "bridge",
    "capi",
    "kernel",
    "output",
    "continuation",
    "tooling",
}
ID_PATTERN = re.compile(r"C-[A-Z0-9-]+\Z")
RUST_TAG = re.compile(r"^[ \t]*//[ \t]*contract:[ \t]*(.*?)[ \t]*$", re.MULTILINE)
REGISTRY_PATH = Path("test/contracts.yml")


def _string_list(value: object) -> bool:
    return (
        isinstance(value, list)
        and bool(value)
        and all(isinstance(item, str) and item.strip() for item in value)
    )


def _entry_errors(entry: dict, label: str) -> list[str]:
    errors = []
    for field in ("id", "title", "definition"):
        value = entry.get(field)
        if not isinstance(value, str) or not value.strip():
            errors.append(f"{label}: {field} must be a non-empty string")
    identifier = entry.get("id")
    if isinstance(identifier, str) and not ID_PATTERN.fullmatch(identifier):
        errors.append(f"{label}: invalid id {identifier!r}")
    for field in ("stages", "implementation"):
        if not _string_list(entry.get(field)):
            errors.append(f"{label}: {field} must be a non-empty list of strings")
    if _string_list(entry.get("stages")):
        for stage in entry["stages"]:
            if stage not in STAGES:
                errors.append(f"{label}: unknown stage {stage!r}")
    return errors


def _load_registry(path: Path) -> tuple[dict[str, dict], list[str]]:
    try:
        payload = yaml.safe_load(path.read_text(encoding="utf-8"))
    except (OSError, UnicodeError, yaml.YAMLError) as exc:
        return {}, [f"{REGISTRY_PATH}: {exc}"]
    entries = payload.get("contracts") if isinstance(payload, dict) else None
    if not isinstance(entries, list) or not entries:
        return {}, [f"{REGISTRY_PATH}: contracts must be a non-empty list"]
    contracts = {}
    errors = []
    for index, entry in enumerate(entries, start=1):
        label = f"{REGISTRY_PATH}: entry {index}"
        if not isinstance(entry, dict):
            errors.append(f"{label}: must be a mapping")
            continue
        errors.extend(_entry_errors(entry, label))
        identifier = entry.get("id")
        if not isinstance(identifier, str):
            continue
        if identifier in contracts:
            errors.append(f"{label}: duplicate id {identifier}")
        else:
            contracts[identifier] = entry
    return contracts, errors


def _is_contract_call(node: ast.AST | None) -> bool:
    if not isinstance(node, ast.Call):
        return False
    func = node.func
    return (
        isinstance(func, ast.Attribute)
        and func.attr == "contract"
        and isinstance(func.value, ast.Attribute)
        and func.value.attr == "mark"
        and isinstance(func.value.value, ast.Name)
        and func.value.value.id == "pytest"
    )


def _python_tags(source: str) -> tuple[set[str], list[str]]:
    tree = ast.parse(source)
    candidates = []
    for node in tree.body:
        targets = []
        if isinstance(node, ast.Assign):
            targets = node.targets
        elif isinstance(node, ast.AnnAssign):
            targets = [node.target]
        if any(isinstance(t, ast.Name) and t.id == "pytestmark" for t in targets):
            value = node.value
            candidates.extend(
                value.elts if isinstance(value, (ast.List, ast.Tuple)) else [value]
            )
    for node in ast.walk(tree):
        if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef, ast.ClassDef)):
            candidates.extend(node.decorator_list)
    tags = set()
    errors = []
    for node in candidates:
        if not _is_contract_call(node):
            continue
        if not node.args or node.keywords:
            errors.append(f"line {node.lineno}: contract needs positional literal ids")
        for arg in node.args:
            if isinstance(arg, ast.Constant) and isinstance(arg.value, str):
                tags.add(arg.value)
            else:
                errors.append(f"line {node.lineno}: contract id must be a literal string")
    return tags, errors


def _rust_tags(source: str) -> tuple[set[str], list[str]]:
    tags = set()
    errors = []
    for match in RUST_TAG.finditer(source):
        for value in match.group(1).split(","):
            identifier = value.strip()
            if identifier:
                tags.add(identifier)
            else:
                errors.append("empty Rust contract id")
    return tags, errors


def _scan_files(root: Path) -> tuple[dict[str, list[str]], list[str]]:
    paths = sorted(
        {
            *(root / "test").rglob("test_*.py"),
            *(root / "src/suews_bridge/src").rglob("*.rs"),
            *(root / "src/suews_bridge/tests").glob("*.rs"),
        }
    )
    files = {}
    errors = []
    for path in paths:
        name = path.relative_to(root).as_posix()
        try:
            source = path.read_text(encoding="utf-8")
            if path.suffix == ".rs" and "#[test]" not in source:
                continue
            tags, issues = (
                _python_tags(source) if path.suffix == ".py" else _rust_tags(source)
            )
            files[name] = sorted(tags)
            errors.extend(f"{name}: {issue}" for issue in issues)
        except (OSError, UnicodeError, SyntaxError) as exc:
            files[name] = []
            errors.append(f"{name}: {exc}")
    return files, errors


def check_tree(root: Path) -> dict:
    """Collect registry diagnostics and unique file counts for each contract."""
    contracts, errors = _load_registry(root / REGISTRY_PATH)
    files, scan_errors = _scan_files(root)
    errors.extend(scan_errors)
    unknown = {}
    counts = dict.fromkeys(sorted(contracts), 0)
    for name, tags in files.items():
        for tag in tags:
            if tag in contracts:
                counts[tag] += 1
            else:
                unknown.setdefault(name, []).append(tag)
            if not ID_PATTERN.fullmatch(tag):
                errors.append(f"{name}: invalid contract id {tag!r}")
    untagged = [name for name, tags in files.items() if not tags]
    return {
        "summary": {
            "files": len(files),
            "tagged": len(files) - len(untagged),
            "untagged": len(untagged),
            "unknown_ids": len({tag for tags in unknown.values() for tag in tags}),
            "errors": len(errors),
        },
        "files": files,
        "untagged": untagged,
        "unknown_ids": unknown,
        "contract_file_counts": counts,
        "errors": errors,
    }


def render_report(report: dict) -> str:
    """Render the census as ASCII Markdown, including zero-count contracts."""
    summary = report["summary"]
    lines = [
        "# Test-contract report",
        "",
        "Summary: {files} files; {tagged} tagged; {untagged} untagged; "
        "{unknown_ids} unknown ids; {errors} errors.".format(**summary),
        "",
        "File counts measure tags, not test execution or contract fulfilment.",
        "",
        "## Contract by file count",
        "",
        "| Contract | Files |",
        "| --- | ---: |",
    ]
    lines.extend(
        f"| {identifier} | {count} |"
        for identifier, count in report["contract_file_counts"].items()
    )
    for title, entries in (
        ("Untagged files", report["untagged"]),
        (
            "Unknown ids",
            [
                f"{name}: {', '.join(tags)}"
                for name, tags in report["unknown_ids"].items()
            ],
        ),
        ("Errors", report["errors"]),
    ):
        lines.extend(["", f"## {title}", ""])
        lines.extend(f"- {entry}" for entry in entries)
        if not entries:
            lines.append("None.")
    return "\n".join(lines).encode("ascii", errors="backslashreplace").decode("ascii")


def main(argv: list[str] | None = None) -> int:
    """Print a report, optionally write JSON, and apply the requested exit policy."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--root",
        type=Path,
        default=Path(__file__).resolve().parents[2],
        help="repository root (default: this script's repository)",
    )
    mode = parser.add_mutually_exclusive_group()
    mode.add_argument("--report", action="store_true", help="report only (default)")
    mode.add_argument("--strict", action="store_true", help="fail on findings")
    parser.add_argument("--json", type=Path, help="also write the report as JSON")
    args = parser.parse_args(argv)
    report = check_tree(args.root)
    print(render_report(report))
    if args.json is not None:
        args.json.write_text(json.dumps(report, indent=2) + "\n", encoding="utf-8")
    failed = report["untagged"] or report["unknown_ids"] or report["errors"]
    return int(args.strict and bool(failed))


if __name__ == "__main__":
    raise SystemExit(main())
