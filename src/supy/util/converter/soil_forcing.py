"""Transactional legacy soil-observation file conversion.

The YAML upgrade owner supplies migration handlers; this module owns the paired
file operation. No numerical simulation or reference refresh is performed.
"""

# Optional annotations retain the supported Python 3.9 API spelling.
# ruff: file-ignore[non-pep604-annotation-optional]

from copy import deepcopy
from hashlib import sha256
import json
import os
from pathlib import Path
import re
from typing import Optional
import warnings

import numpy as np
import pandas as pd
import yaml

from ...data_model.core.config import SUEWSConfig
from .._forcing import convert_observed_soil_moisture, soil_moisture_to_content
from .yaml_upgrade import (
    CURRENT_SCHEMA_VERSION,
    SOIL_MIGRATION_MESSAGE,
    SoilMigrationContext,
    YamlUpgradeError,
    _detect_signature,
    _resolve_handler_chain,
    _resolve_package_to_schema,
    _soil_method,
    _soil_raw,
    apply_migration_handler,
)

CONFLICT = "Both legacy and canonical soil-moisture inputs were supplied; select one contract before conversion."
RETIRED = "Sensor saturation capacity is retired; deficit now uses the configured model soil-store capacity"
COLUMNS = [
    "iy",
    "id",
    "it",
    "imin",
    "qn",
    "qh",
    "qe",
    "qs",
    "qf",
    "U",
    "RH",
    "Tair",
    "pres",
    "rain",
    "kdown",
    "snow",
    "ldown",
    "fcld",
    "wuh",
    "sm",
    "lai",
    "kdiff",
    "kdir",
    "wdir",
]


def _read_table(text: str, source_contract: Optional[str]):
    """Retain every token and separator outside the soil column."""
    lines = text.splitlines(keepends=True)
    nonempty = [i for i, line in enumerate(lines) if line.strip()]
    if not nonempty:
        raise YamlUpgradeError("Forcing table is empty.")
    first = nonempty[0]
    header = lines[first].split()
    named = bool({"iy", "id", "it", "imin", "sm", "xsmd"} & set(header))
    if source_contract not in {None, "1.0.0", "1.1.0", "1.2.0", "2.0.0"}:
        raise YamlUpgradeError(
            "Unsupported source_contract; select 1.0.0, 1.1.0, 1.2.0 or 2.0.0."
        )
    if not named:
        if source_contract is None:
            raise YamlUpgradeError(
                "Positional forcing requires an explicit source_contract; units are never inferred."
            )
        header = COLUMNS.copy()
        if source_contract.startswith("1."):
            header[19] = "xsmd"
    if "sm" in header and "xsmd" in header:
        raise YamlUpgradeError(CONFLICT)
    if len(header) != len(set(header)) or not ({"sm", "xsmd"} & set(header)):
        raise YamlUpgradeError(
            "Forcing requires exactly one named sm or legacy xsmd column."
        )
    if source_contract and (("sm" in header) != (source_contract == "2.0.0")):
        raise YamlUpgradeError(CONFLICT)
    rows = nonempty[1:] if named else nonempty
    tokens = [lines[i].split() for i in rows]
    if not tokens or any(len(row) != len(header) for row in tokens):
        raise YamlUpgradeError("Forcing rows must match the declared column count.")
    frame = pd.DataFrame(tokens, columns=header)
    if not {"iy", "id", "it", "imin"}.issubset(frame.columns):
        raise YamlUpgradeError("Forcing requires time columns iy, id, it and imin.")
    # Only the time and soil fields belong to this conversion. Extra columns
    # may carry textual quality flags and must survive without interpretation.
    for name in ("iy", "id", "it", "imin", "sm" if "sm" in header else "xsmd"):
        frame[name] = pd.to_numeric(
            frame[name].replace(["NaN", "nan", "NAN"], np.nan), errors="raise"
        )
    frame.index = (
        pd.to_datetime(frame.iy.astype(int).astype(str), format="%Y")
        + pd.to_timedelta(frame.id - 1, unit="D")
        + pd.to_timedelta(frame.it, unit="h")
        + pd.to_timedelta(frame.imin, unit="m")
    )
    if not frame.index.is_unique or not frame.index.is_monotonic_increasing:
        raise YamlUpgradeError("Forcing timestamps must be unique and increasing.")
    return lines, first if named else None, rows, header, frame


def _replace_token(line, column, replacement):
    tokens = list(re.finditer(r"\S+", line))
    match = tokens[column]
    return line[: match.start()] + replacement + line[match.end() :]


def _conversion_parameters(cfg, method, source_unit, layer_top, layer_depth):
    """Resolve explicit geometry and legacy fine-earth metadata at every site."""
    if layer_top is None or layer_depth is None:
        raise YamlUpgradeError(SOIL_MIGRATION_MESSAGE)
    parameters = []
    for site in cfg.get("sites", []):
        old = site.get("properties", {}).get("soil_observation", {})
        if "layer_top" in old or "layer_depth" in old:
            raise YamlUpgradeError(CONFLICT)
        fraction = float(_soil_raw(old.get("soil_not_rocks", 1)))
        density = old.get("bulk_density")
        density = None if density is None else float(_soil_raw(density)) * 1000
        parameters.append({
            "source_unit": source_unit,
            "layer_top": layer_top,
            "layer_depth": layer_depth,
            "soil_not_rocks": fraction,
            "bulk_density": density,
            "volume_basis": "fine_earth",
        })
    if not parameters:
        raise YamlUpgradeError("Soil conversion requires at least one configured site.")
    # One external series cannot describe different fine-earth conversion factors.
    relevant = ("layer_top", "layer_depth", "soil_not_rocks") + (
        ("bulk_density",) if method == 2 else ()
    )
    if any(any(p[k] != parameters[0][k] for k in relevant) for p in parameters[1:]):
        raise YamlUpgradeError(
            "Shared forcing requires matching legacy soil conversion metadata; run incompatible grids separately."
        )
    return parameters


# The paired contract branches are kept together to make refusal order explicit.
def _prepare(  # ruff: ignore[too-many-branches, too-many-locals, too-many-statements]
    cfg,
    text,
    *,
    source_schema,
    source_contract,
    source_unit,
    layer_top,
    layer_depth,
    legacy_depth_is_thickness,
    drop_legacy,
):
    lines, header_line, rows, header, frame = _read_table(text, source_contract)
    physics = cfg.get("model", {}).get("physics", {})
    names = [
        k
        for k in ("soil_moisture", "soil_moisture_deficit", "smdmethod", "smd_method")
        if k in physics
    ]
    if len(names) > 1:
        raise YamlUpgradeError(CONFLICT)
    method = _soil_method(physics[names[0]]) if names else 0
    canonical = source_schema == CURRENT_SCHEMA_VERSION
    legacy = "xsmd" in header
    changes, context, parameters = [], None, []
    original_method = method
    if drop_legacy and (method != 0 or not legacy):
        raise YamlUpgradeError(
            "Archival drop is only available for unused legacy method-0 xsmd."
        )
    if legacy:
        if canonical:
            raise YamlUpgradeError(CONFLICT)
        expected_unit = {1: "m3 m-3", 2: "kg kg-1"}.get(method)
        if method == "observed":
            if source_unit not in {"m3 m-3", "kg kg-1"}:
                raise YamlUpgradeError(
                    SOIL_MIGRATION_MESSAGE
                    + "; ambiguous observed selector requires source_unit"
                )
            method = 1 if source_unit == "m3 m-3" else 2
            expected_unit = source_unit
        if expected_unit and source_unit not in {None, expected_unit}:
            raise YamlUpgradeError(
                "Declared source_unit conflicts with the original legacy method."
            )
        source_unit = source_unit or expected_unit
        values = frame["xsmd"]
        present = values.notna() & values.ne(-999)
        if method == 0 and drop_legacy:
            converted = pd.Series(np.nan, index=frame.index)
            changes.append(
                "Archived non-missing legacy xsmd without unit provenance; values replaced by -999 by explicit request"
            )
        elif method == 0 and not present.any():
            converted = values.copy()
        else:
            if source_unit is None:
                raise YamlUpgradeError(
                    SOIL_MIGRATION_MESSAGE
                    + "; modelled xsmd values require explicit source_unit or archival drop"
                )
            parameters = _conversion_parameters(
                cfg, method, source_unit, layer_top, layer_depth
            )
            converted = soil_moisture_to_content(values, **parameters[0])
        column = header.index("xsmd")
        for n, row in enumerate(rows):
            if present.iloc[n]:
                token = (
                    "-999"
                    if pd.isna(converted.iloc[n])
                    else format(converted.iloc[n], ".17g")
                )
                lines[row] = _replace_token(lines[row], column, token)
        if header_line is None:
            new_header = header.copy()
            new_header[column] = "sm"
            lines.insert(0, " ".join(new_header) + "\n")
        else:
            lines[header_line] = _replace_token(lines[header_line], column, "sm")
        frame = frame.rename(columns={"xsmd": "sm"})
        frame["sm"] = converted.to_numpy()
        changes.append(
            "xsmd -> sm; canonical liquid-water content in kg m-2; no clipping"
        )
        if method != 0:
            forcing_manifest = {
                "completed": True,
                "original_method": method,
                "source_unit": source_unit,
                "input_sha256": sha256(text.encode()).hexdigest(),
                "output_sha256": sha256("".join(lines).encode()).hexdigest(),
            }
            context = SoilMigrationContext(
                dict.fromkeys(
                    range(len(cfg.get("sites", []))), (layer_top, layer_depth)
                ),
                method,
                forcing_manifest,
                legacy_depth_is_thickness,
            )
    elif not canonical and method != 0:
        raise YamlUpgradeError(CONFLICT)
    elif source_unit not in {None, "kg m-2"}:
        raise YamlUpgradeError(
            "Canonical sm uses kg m-2; convert volumetric or gravimetric observations before loading it."
        )
    result = deepcopy(cfg)
    for _, handler in _resolve_handler_chain(source_schema, CURRENT_SCHEMA_VERSION):
        result = apply_migration_handler(handler, result, migration_context=context)
    result["schema_version"] = CURRENT_SCHEMA_VERSION
    model = SUEWSConfig.model_validate(deepcopy(result))
    with warnings.catch_warnings(record=True) as caught:
        warnings.simplefilter("always")
        state = model.to_df_state()
        convert_observed_soil_moisture(frame, state)
    if legacy and any(
        "smcap" in s.get("properties", {}).get("soil_observation", {})
        for s in cfg.get("sites", [])
    ):
        changes.append(RETIRED)
    return (
        result,
        "".join(lines),
        {
            "source_field": names[0] if names else None,
            "original_method": original_method,
            "resolved_method": method,
            "source_unit": source_unit,
            "output_unit": "kg m-2",
            "parameters": parameters,
            "row_count": len(frame),
            "legacy_observation_metadata": [
                deepcopy(site.get("properties", {}).get("soil_observation"))
                for site in cfg.get("sites", [])
            ]
            if legacy
            else [],
            "changes": changes,
            "warnings": [str(w.message) for w in caught],
            "layer_bounds": {
                str(i): [layer_top, layer_depth] for i in range(len(parameters))
            },
            "water_phase": "liquid",
            "density_scale": 1000 if parameters else None,
            "legacy_depth_scale": 0.001 if legacy_depth_is_thickness else None,
        },
    )


def convert_soil_observation_files(  # ruff: ignore[too-many-locals]
    input_path: Path,
    forcing_path: Path,
    output_path: Path,
    forcing_output: Path,
    manifest_path: Path,
    *,
    from_ver: Optional[str] = None,
    source_contract: Optional[str] = None,
    source_unit: Optional[str] = None,
    layer_top: Optional[float] = None,
    layer_depth: Optional[float] = None,
    legacy_depth_is_thickness: bool = False,
    drop_legacy: bool = False,
    dry_run: bool = False,
) -> dict:
    """Convert a YAML/whitespace-forcing pair without changing either input.

    Explicit layer bounds are in metres. The returned manifest includes hashes,
    the original method, conversions and warnings. A dry run returns missing
    decisions in ``errors`` and never creates output files or directories.
    Destinations must be distinct, new paths. YAML is written last, after the
    forcing and manifest; validation failures therefore publish no runnable YAML.
    """
    inputs = [Path(input_path), Path(forcing_path)]
    outputs = [Path(output_path), Path(forcing_output), Path(manifest_path)]
    report = {
        "completed": False,
        "dry_run": dry_run,
        "errors": [],
        "proposed_changes": [
            "Validate and upgrade the YAML/forcing pair",
            "Retain inputs and record hashes before publishing separate outputs",
        ],
        "input_hashes": {p.name: sha256(p.read_bytes()).hexdigest() for p in inputs},
    }
    try:  # ruff: ignore[too-many-statements-in-try-clause] - one dry-run boundary encloses the transaction
        resolved = [p.resolve() for p in inputs + outputs]
        if len(set(resolved)) != len(resolved) or any(p.exists() for p in outputs):
            raise YamlUpgradeError(
                "Conversion outputs must be new, distinct paths; input files are never overwritten."
            )
        raw = yaml.safe_load(inputs[0].read_text(encoding="utf-8"))
        signature = from_ver or _detect_signature(raw)
        if signature is None:
            raise YamlUpgradeError(
                "Specify from_ver for a YAML without a schema version."
            )
        source_schema = _resolve_package_to_schema(signature)
        cfg, forcing, details = _prepare(
            raw,
            inputs[1].read_bytes().decode("utf-8"),
            source_schema=source_schema,
            source_contract=source_contract,
            source_unit=source_unit,
            layer_top=layer_top,
            layer_depth=layer_depth,
            legacy_depth_is_thickness=legacy_depth_is_thickness,
            drop_legacy=drop_legacy,
        )
        # Point the new configuration at its paired forcing, retaining references.
        control = cfg.setdefault("model", {}).setdefault("control", {})
        forcing_config = control.setdefault("forcing", {})
        relative = os.path.relpath(outputs[1].resolve(), outputs[0].resolve().parent)
        old_file = forcing_config.get("file")
        forcing_config["file"] = (
            dict(old_file, value=relative) if isinstance(old_file, dict) else relative
        )
        SUEWSConfig.model_validate(deepcopy(cfg))
        yaml_bytes = yaml.safe_dump(cfg, sort_keys=False, allow_unicode=False).encode(
            "utf-8"
        )
        forcing_bytes = forcing.encode("utf-8")
        report.update(
            details,
            source_schema=source_schema,
            target_schema=CURRENT_SCHEMA_VERSION,
            output_hashes={
                outputs[0].name: sha256(yaml_bytes).hexdigest(),
                outputs[1].name: sha256(forcing_bytes).hexdigest(),
            },
        )
        if dry_run:
            return report
        report["completed"] = True
        manifest_bytes = (
            json.dumps(report, indent=2, ensure_ascii=True, allow_nan=False) + "\n"
        ).encode()
        # Exclusive creation also prevents a concurrent converter overwriting files.
        for path, content in (
            (outputs[1], forcing_bytes),
            (outputs[2], manifest_bytes),
            (outputs[0], yaml_bytes),
        ):
            path.parent.mkdir(parents=True, exist_ok=True)
            with path.open("xb") as stream:
                stream.write(content)
        return report
    except (ValueError, TypeError, OSError, YamlUpgradeError) as exc:
        if not dry_run:
            raise
        report["errors"].append(str(exc))
        return report
