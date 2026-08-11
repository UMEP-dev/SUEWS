#!/usr/bin/env python3
"""Render the tracked forcing-variable reference from the registry."""

from __future__ import annotations

import argparse
import importlib
from pathlib import Path
import sys
from types import ModuleType

PROJECT_ROOT = Path(__file__).resolve().parents[1]
_FORCING_SOURCE = PROJECT_ROOT / "src/supy/data_model/forcing"
_SOURCE_PACKAGE = "_suews_forcing_docs_source"
package = ModuleType(_SOURCE_PACKAGE)
package.__package__ = _SOURCE_PACKAGE
package.__path__ = [str(_FORCING_SOURCE)]
sys.modules[_SOURCE_PACKAGE] = package
FORCING_REGISTRY = importlib.import_module(
    f"{_SOURCE_PACKAGE}.variables"
).FORCING_REGISTRY

REFERENCE_PATH = PROJECT_ROOT / "docs/source/data-structures/df_forcing.rst"


def _unit(variable) -> str:
    if variable.unit is not None:
        return variable.unit
    return "; ".join(
        f"{variable.unit_selector}={value}: {unit}"
        for value, unit in variable.units_by_value.items()
    )


def _range(variable) -> str:
    if variable.validation_range is None:
        return "not range-checked"
    lower, upper = variable.validation_range
    if lower is None:
        return f"<= {upper}"
    if upper is None:
        return f">= {lower}"
    return f"{lower} to {upper}"


def _conditions(variable_name: str) -> str | None:
    labels: list[str] = []
    for rule in FORCING_REGISTRY.requirement_rules:
        if not any(variable_name in alternative for alternative in rule.alternatives):
            continue
        labels.append(
            " and ".join(
                f"{name} in {list(values)}" for name, values in rule.conditions.items()
            )
        )
    return "; or ".join(labels) or None


def render_forcing_reference() -> str:
    """Return the complete deterministic RST reference page."""
    lines = [
        ":orphan:",
        "",
        ".. _df_forcing_var:",
        "",
        "``df_forcing`` variables",
        "========================",
        "",
        "This reference is generated from the published forcing registry.",
        "File timestamps use the site's fixed-offset local standard time and",
        "identify the end of each forcing interval. Daylight-saving transitions",
        "are not part of this timestamp convention.",
        "",
        "File-header aliases and programmatic accessor aliases are separate",
        "namespaces. The latter are not accepted as forcing-file headers.",
        "",
    ]
    for variable in FORCING_REGISTRY.variables:
        lines.extend([
            f".. option:: {variable.name}",
            "",
            f"   :Description: {variable.description}",
            f"   :Input unit: {_unit(variable)}",
            f"   :Role: {variable.role}",
            f"   :Temporal semantics: {variable.temporal}",
            f"   :Requiredness: {variable.requiredness}",
            f"   :Missing-value policy: {variable.missing_policy}",
            f"   :Enforced input range: {_range(variable)}",
        ])
        if variable.legacy_position is not None:
            lines.append(f"   :Legacy position: {variable.legacy_position}")
        if variable.file_aliases:
            lines.append(f"   :File aliases: {', '.join(variable.file_aliases)}")
        if variable.accessor_aliases:
            lines.append(
                f"   :Accessor aliases: {', '.join(variable.accessor_aliases)}"
            )
        if variable.fallback is not None:
            lines.append(f"   :Fallback column: {variable.fallback}")
        conditions = _conditions(variable.name)
        if conditions is not None:
            lines.append(f"   :Active requirement: {conditions}")
        lines.append("")
    return "\n".join(lines).rstrip() + "\n"


def main(argv: list[str] | None = None) -> int:
    """Write the reference or verify that the tracked copy is current."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--check", action="store_true")
    args = parser.parse_args(argv)
    rendered = render_forcing_reference()
    if args.check:
        if (
            not REFERENCE_PATH.is_file()
            or REFERENCE_PATH.read_text(encoding="utf-8") != rendered
        ):
            print(f"stale forcing reference: {REFERENCE_PATH}", file=sys.stderr)
            return 1
    else:
        REFERENCE_PATH.write_text(rendered, encoding="utf-8")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
