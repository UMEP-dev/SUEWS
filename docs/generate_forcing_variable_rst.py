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
version_registry = importlib.import_module(f"{_SOURCE_PACKAGE}.version")
CURRENT_FORCING_VERSION = version_registry.CURRENT_FORCING_VERSION

REFERENCE_PATH = PROJECT_ROOT / "docs/source/data-structures/df_forcing.rst"

_UNIT_LABELS = {
    "1": "dimensionless",
    "W m-2": "W |m^-2|",
    "m s-1": "m |s^-1|",
    "m3 m-3": "|m^3| |m^-3|",
    "kg kg-1": "kg |kg^-1|",
    "m2 m-2": "|m^2| |m^-2|",
    "degC": r":math:`{}^{\circ}\mathrm{C}`",
    "degree": r":math:`{}^{\circ}`",
}

# ``temporal`` is also used by the loaders to select a resampling method. These
# interval-mean inputs retain instantaneous interpolation for compatibility, so
# their user-facing interval basis must be expressed separately.
_INTERVAL_MEANS_WITH_INSTANTANEOUS_RESAMPLING = frozenset({
    "U",
    "RH",
    "Tair",
    "pres",
    "fcld",
})

_REQUIREDNESS_LABELS = {
    "baseline": "always",
    "conditional": "only for selected physics; see the requirements above",
    "optional": "no",
}


def _heading(title: str, marker: str = "-") -> list[str]:
    return [title, marker * len(title), ""]


def _format_unit(unit: str) -> str:
    return _UNIT_LABELS.get(unit, unit)


def _unit(variable) -> str:
    if variable.unit is not None:
        return _format_unit(variable.unit)
    return "; ".join(
        f"``{variable.unit_selector}={value}``: {_format_unit(unit)}"
        for value, unit in variable.units_by_value.items()
    )


def _number(value: float) -> str:
    return f"{value:g}"


def _range(variable) -> str:
    if variable.validation_range is None:
        if variable.temporal == "time":
            return "integer calendar coordinate; calendar validity checked on load"
        return "not range-checked"
    lower, upper = variable.validation_range
    if lower is None:
        return rf":math:`\leq {_number(upper)}`"
    if upper is None:
        return rf":math:`\geq {_number(lower)}`"
    return f"{_number(lower)} to {_number(upper)} (inclusive)"


def _interval_basis(variable) -> str:
    if variable.temporal == "time":
        return "component of the interval-end timestamp"
    if variable.temporal == "sum":
        return "total accumulated over the forcing interval"
    if (
        variable.temporal == "avg"
        or variable.name in _INTERVAL_MEANS_WITH_INSTANTANEOUS_RESAMPLING
    ):
        return "mean over the forcing interval"
    return "state at the interval-end timestamp"


def _condition(rule) -> str:
    return " and ".join(
        f"``{name}`` in {list(values)}" for name, values in rule.conditions.items()
    )


def _alternative(columns: tuple[str, ...]) -> str:
    rendered = " + ".join(f"``{name}``" for name in columns)
    return rendered if len(columns) == 1 else f"({rendered})"


def _alternatives(rule) -> str:
    return " OR ".join(_alternative(columns) for columns in rule.alternatives)


def _missing_values(variable) -> str:
    if variable.missing_policy == "forbidden":
        return "not allowed"
    if variable.missing_policy == "sentinel":
        return "``-999`` only while this column is optional or inactive"
    return (
        f"use ``{variable.fallback}`` only when this column is absent; an explicit "
        "``-999`` remains missing"
    )


def _render_variable(variable) -> list[str]:
    lines = [
        f".. option:: {variable.name}",
        "",
        f"   :Description: {variable.description}",
        f"   :Input unit: {_unit(variable)}",
        f"   :Interval basis: {_interval_basis(variable)}",
        f"   :Required: {_REQUIREDNESS_LABELS[variable.requiredness]}",
        f"   :Missing values: {_missing_values(variable)}",
        f"   :Valid input range: {_range(variable)}",
    ]
    lines.append("")
    return lines


def render_forcing_reference() -> str:
    """Return the complete deterministic RST reference page."""
    lines = [
        ".. _df_forcing_var:",
        "",
        "``df_forcing`` variables",
        "========================",
        "",
        f"This reference is generated from forcing contract ``{CURRENT_FORCING_VERSION}``.",
        "It describes columns supplied in an external forcing file. For file",
        "layout and preparation, see :doc:`/inputs/forcing-data`; for the loaded",
        "Python object, see :doc:`/api/io-data-structures`.",
        "",
    ]
    lines.extend(_heading("How to read this page"))
    lines.extend([
        "All units and valid ranges describe values in the external forcing file.",
        "Use each canonical option name as the file header.",
        "",
        "File timestamps identify the end of each forcing interval. The default",
        "reference is the site's fixed-offset local standard time; UTC is accepted",
        "when ``model.control.forcing.timestamp_reference`` is ``utc``. Model output",
        "follows the declared forcing clock. Daylight-saving civil time is unsupported.",
        "",
        "The timestamp labels the interval; it is not an instantaneous sampling time.",
        "Weather, radiation, and energy-flux values are means over the interval",
        "ending at that timestamp. Rainfall and external water use are totals",
        "accumulated over the same interval. State inputs such as LAI, snow cover,",
        "and soil moisture apply at the interval end.",
        "",
        "Every row needs valid values for always-required columns and for any",
        "physics-conditional columns selected by the table below. Use ``-999`` only",
        "for optional or inactive columns. Land-cover-specific columns use their named",
        "bulk fallback only when that land-cover column is absent, not when it contains",
        "``-999``.",
        "",
    ])
    lines.extend([
        ".. _df_forcing_requirements:",
        "",
    ])
    lines.extend(_heading("Physics-conditional requirements"))
    lines.extend([
        "Each row below is active only when all listed selector conditions match. Any",
        "one complete alternative satisfies the rule; columns joined by ``+`` are",
        "jointly required. Selector definitions are in :ref:`modelphysics`.",
        "",
        ".. list-table::",
        "   :header-rows: 1",
        "   :widths: 45 55",
        "",
        "   * - Active when",
        "     - Valid forcing alternative",
    ])
    for rule in FORCING_REGISTRY.requirement_rules:
        lines.extend([
            f"   * - {_condition(rule)}",
            f"     - {_alternatives(rule)}",
        ])
    lines.append("")

    groups = (
        (
            "Timestamp columns",
            "Every forcing row must include these coordinates for the declared "
            "timestamp reference.",
            [
                variable
                for variable in FORCING_REGISTRY.variables
                if variable.role == "coordinate"
            ],
        ),
        (
            "Always-required weather inputs",
            "Every forcing row must contain valid values for these variables.",
            [
                variable
                for variable in FORCING_REGISTRY.variables
                if variable.role != "coordinate"
                and variable.requiredness == "baseline"
                and variable.legacy_position is not None
            ],
        ),
        (
            "Physics-conditional inputs",
            "These are required only when selected by the requirements table above.",
            [
                variable
                for variable in FORCING_REGISTRY.variables
                if variable.requiredness == "conditional"
                and variable.legacy_position is not None
            ],
        ),
        (
            "Optional accepted columns",
            "These columns are accepted but are never required by SUEWS.",
            [
                variable
                for variable in FORCING_REGISTRY.variables
                if variable.requiredness == "optional"
                and variable.legacy_position is not None
            ],
        ),
        (
            "Land-cover-specific alternatives",
            "These may replace the corresponding bulk LAI or water-use column.",
            [
                variable
                for variable in FORCING_REGISTRY.variables
                if variable.legacy_position is None
            ],
        ),
    )
    for title, introduction, variables in groups:
        lines.extend(_heading(title))
        lines.extend([introduction, ""])
        for variable in variables:
            lines.extend(_render_variable(variable))
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
