#!/usr/bin/env python3
"""Report which current registry unit strings the SUEWS unit vocabulary rejects.

A report, not a check: it always exits 0. It reads the three places unit
strings live today -- every ``"unit"`` in ``json_schema_extra`` under
``src/supy/data_model/core`` (read statically, as text), the latest forcing
contract artefact and the latest output contract catalogue -- parses each
with :func:`supy.units.parse_unit`, and lists the strings it rejects, with
the reason and how many entries carry each.

The registries are not migrated to the vocabulary yet; this list is the
work the notation sweeps would do. Run it from the repository root:

    python scripts/lint/report_unit_strings.py [--json]

Needs no build: ``src/supy/units.py`` uses only the standard library and is
loaded from its file.
"""

from __future__ import annotations

import argparse
import ast
from collections import Counter, defaultdict
import importlib.util
import json
from pathlib import Path
import sys

REPO_ROOT = Path(__file__).resolve().parents[2]
DATA_MODEL = REPO_ROOT / "src" / "supy" / "data_model"


def _load_units():
    path = REPO_ROOT / "src" / "supy" / "units.py"
    spec = importlib.util.spec_from_file_location("suews_units", path)
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


def _version_key(name: str) -> tuple[int, ...]:
    return tuple(int(part) for part in name.removesuffix(".json").split("."))


def input_units() -> list[tuple[str, str]]:
    """Return (field, unit) for every unit in the input model's json_schema_extra."""
    found = []
    for path in sorted((DATA_MODEL / "core").rglob("*.py")):
        tree = ast.parse(path.read_text(encoding="utf-8"))
        for node in ast.walk(tree):
            if not isinstance(node, (ast.AnnAssign, ast.Assign)):
                continue
            targets = [node.target] if isinstance(node, ast.AnnAssign) else node.targets
            name = next((t.id for t in targets if isinstance(t, ast.Name)), "?")
            for sub in ast.walk(node):
                if not isinstance(sub, ast.Dict):
                    continue
                for key, value in zip(sub.keys, sub.values):
                    if (
                        isinstance(key, ast.Constant)
                        and key.value == "unit"
                        and isinstance(value, ast.Constant)
                        and isinstance(value.value, str)
                    ):
                        found.append((name, value.value))
    return found


def forcing_units() -> list[tuple[str, str]]:
    """Return (column, unit) from the latest forcing contract artefact."""
    artefacts = DATA_MODEL / "forcing" / "artefacts"
    latest = max(artefacts.glob("*.json"), key=lambda p: _version_key(p.name))
    found = []
    for variable in json.loads(latest.read_text(encoding="utf-8"))["variables"]:
        if variable["unit"] is not None:
            found.append((variable["name"], variable["unit"]))
        for value, unit in variable["units_by_value"].items():
            found.append((
                f"{variable['name']}[{variable['unit_selector']}={value}]",
                unit,
            ))
        if variable["runtime_unit"]:
            found.append((f"{variable['name']} (in memory)", variable["runtime_unit"]))
    return found


def output_units() -> list[tuple[str, str]]:
    """Return (group.name, unit) from the latest output contract catalogue."""
    artefacts = DATA_MODEL / "output" / "artefacts"
    latest = max(
        (p for p in artefacts.iterdir() if p.is_dir()),
        key=lambda p: _version_key(p.name),
    )
    catalogue = json.loads((latest / "catalogue.json").read_text(encoding="utf-8"))
    return [(f"{v['group']}.{v['name']}", v["unit"]) for v in catalogue["variables"]]


def build_report() -> dict:
    """Parse every registry string and collect the rejected ones."""
    units = _load_units()
    report = {}
    for registry, entries in (
        ("input", input_units()),
        ("forcing", forcing_units()),
        ("output", output_units()),
    ):
        rejected: dict[str, dict] = {}
        counts: Counter[str] = Counter()
        examples: dict[str, list[str]] = defaultdict(list)
        for name, unit in entries:
            try:
                units.parse_unit(unit)
            except units.UnitError as error:
                counts[unit] += 1
                examples[unit].append(name)
                rejected[unit] = {"reason": str(error).split(": ", 1)[-1]}
        for unit, detail in rejected.items():
            detail["count"] = counts[unit]
            detail["examples"] = examples[unit][:5]
        report[registry] = {
            "entries": len(entries),
            "rejected_entries": sum(counts.values()),
            "rejected_strings": dict(
                sorted(rejected.items(), key=lambda item: -item[1]["count"])
            ),
        }
    return report


def main(argv: list[str]) -> int:
    """Print the report; always exit 0."""
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--json", action="store_true", help="print JSON")
    args = parser.parse_args(argv[1:])
    report = build_report()
    if args.json:
        print(json.dumps(report, indent=2))
        return 0
    for registry, result in report.items():
        print(
            f"{registry}: {result['rejected_entries']} of {result['entries']} entries "
            f"rejected ({len(result['rejected_strings'])} distinct strings)"
        )
        for unit, detail in result["rejected_strings"].items():
            print(f"  {detail['count']:4d}  {unit!r}: {detail['reason']}")
            print(f"        e.g. {', '.join(detail['examples'])}")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
