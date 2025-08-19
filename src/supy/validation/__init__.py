"""Validation facade for SUEWS tools.

This module exposes a small, stable API for use by higher-level tools
such as the suews-wizard, CI jobs, or GUIs. It wraps existing internal
implementations without re-implementing business logic.
"""

from __future__ import annotations

from pathlib import Path
from typing import Iterable, List, Dict, Any, Tuple

import yaml

from supy.data_model.schema.publisher import generate_json_schema
from supy.cmd.validate_config import validate_single_file  # reuse

# Optional orchestrated phases import
try:
    from supy.data_model.yaml_processor.orchestrator import (
        validate_input_file as _proc_validate_input_file,
        setup_output_paths as _proc_setup_output_paths,
        run_phase_a as _proc_run_phase_a,
        run_phase_b as _proc_run_phase_b,
        run_phase_c as _proc_run_phase_c,
    )
except Exception:  # pragma: no cover - optional path
    _proc_validate_input_file = None
    _proc_setup_output_paths = None
    _proc_run_phase_a = None
    _proc_run_phase_b = None
    _proc_run_phase_c = None


def validate(
    paths: Iterable[str], schema_version: str | None = None
) -> List[Dict[str, Any]]:
    """Validate one or more YAML configs.

    Returns a list of result dicts: {file, valid, errors, error_count}.
    """
    schema = generate_json_schema(version=schema_version)
    results: List[Dict[str, Any]] = []
    for p in paths:
        path = Path(p)
        ok, errors = validate_single_file(path, schema, show_details=True)
        results.append({
            "file": str(path),
            "valid": ok,
            "errors": errors if not ok else [],
            "error_count": len(errors) if not ok else 0,
        })
    return results


def phases(
    yaml_file: str, phase: str = "ABC", mode: str = "public"
) -> Tuple[bool, Dict[str, str]]:
    """Run orchestrated validation phases (A/B/C). Returns (success, outputs).

    outputs includes report and updated YAML paths for the phase executed.
    """
    if not all([
        _proc_validate_input_file,
        _proc_setup_output_paths,
        _proc_run_phase_a,
        _proc_run_phase_b,
        _proc_run_phase_c,
    ]):
        raise RuntimeError("yaml_processor orchestrator not available")

    user_yaml = _proc_validate_input_file(yaml_file)
    standard_yaml = "src/supy/sample_data/sample_config.yml"
    (uptodate, reportA, sci_yaml, reportB, pyd_yaml, reportC, _dir) = (
        _proc_setup_output_paths(user_yaml, phase)
    )

    outputs: Dict[str, str] = {}

    if phase == "A":
        ok = _proc_run_phase_a(
            user_yaml,
            standard_yaml,
            uptodate,
            reportA,
            mode=mode,
            phase="A",
            silent=True,
        )
        outputs.update({"report": reportA, "yaml": uptodate})
        return ok, outputs

    if phase == "B":
        ok = _proc_run_phase_b(
            user_yaml,
            user_yaml,
            standard_yaml,
            sci_yaml,
            reportB,
            None,
            phase_a_performed=False,
            mode=mode,
            phase="B",
            silent=True,
        )
        outputs.update({"report": reportB, "yaml": sci_yaml})
        return ok, outputs

    if phase == "C":
        ok = _proc_run_phase_c(
            user_yaml, pyd_yaml, reportC, mode=mode, phases_run=["C"], silent=True
        )
        outputs.update({"report": reportC, "yaml": pyd_yaml})
        return ok, outputs

    if phase == "AB":
        a_ok = _proc_run_phase_a(
            user_yaml,
            standard_yaml,
            uptodate,
            reportA,
            mode=mode,
            phase="AB",
            silent=True,
        )
        b_ok = False
        if a_ok:
            b_ok = _proc_run_phase_b(
                user_yaml,
                uptodate,
                standard_yaml,
                sci_yaml,
                reportB,
                reportA,
                phase_a_performed=True,
                mode=mode,
                phase="AB",
                silent=True,
            )
        outputs.update({"report": reportB, "yaml": sci_yaml})
        return a_ok and b_ok, outputs

    if phase == "AC":
        a_ok = _proc_run_phase_a(
            user_yaml,
            standard_yaml,
            uptodate,
            reportA,
            mode=mode,
            phase="AC",
            silent=True,
        )
        c_ok = False
        if a_ok:
            c_ok = _proc_run_phase_c(
                uptodate,
                pyd_yaml,
                reportC,
                mode=mode,
                phase_a_report_file=reportA,
                phases_run=["A", "C"],
                silent=True,
            )
        outputs.update({"report": reportC, "yaml": pyd_yaml})
        return a_ok and c_ok, outputs

    if phase == "BC":
        b_ok = _proc_run_phase_b(
            user_yaml,
            user_yaml,
            standard_yaml,
            sci_yaml,
            reportB,
            None,
            phase_a_performed=False,
            mode=mode,
            phase="BC",
            silent=True,
        )
        c_ok = False
        if b_ok:
            c_ok = _proc_run_phase_c(
                sci_yaml,
                pyd_yaml,
                reportC,
                mode=mode,
                phases_run=["B", "C"],
                silent=True,
            )
        outputs.update({"report": reportC, "yaml": pyd_yaml})
        return b_ok and c_ok, outputs

    # Default ABC
    a_ok = _proc_run_phase_a(
        user_yaml, standard_yaml, uptodate, reportA, mode=mode, phase="ABC", silent=True
    )
    b_ok = False
    c_ok = False
    if a_ok:
        b_ok = _proc_run_phase_b(
            user_yaml,
            uptodate,
            standard_yaml,
            sci_yaml,
            reportB,
            reportA,
            phase_a_performed=True,
            mode=mode,
            phase="ABC",
            silent=True,
        )
    if a_ok and b_ok:
        c_ok = _proc_run_phase_c(
            sci_yaml,
            pyd_yaml,
            reportC,
            mode=mode,
            phase_a_report_file=None,
            phases_run=["A", "B", "C"],
            silent=True,
        )
    outputs.update({"report": reportC, "yaml": pyd_yaml})
    return a_ok and b_ok and c_ok, outputs
