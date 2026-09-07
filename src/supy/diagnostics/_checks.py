"""Diagnostic checks for completed SUEWS run directories.

The aggregator ``check_run(run_dir)`` runs every Phase-1 check and returns
a list of ``CheckResult`` records. Each check is a small pure function
that takes a ``Path`` and returns one ``CheckResult``; this keeps the
checks individually testable and the aggregator trivial.

Phase-1 checks (intentionally minimal):

- ``check_provenance_present`` -- ``provenance.json`` sidecar exists.
- ``check_output_files_present`` -- at least one ``df_output*.csv``,
  ``*.parquet`` or legacy ``*_SUEWS_*.txt`` produced by ``suews run`` is
  present.
- ``check_nan_proportion`` -- missing fraction in QH/QE/QN below 5% in
  every output partition.
- ``check_energy_balance_closure`` -- mean
  ``|(QN + QF + QMRain) - (QH + QE + QS + QM + QMFreeze)| / |QN| < 0.10``.

Every partition of the run output is inspected and judged on its own:
all files of the highest-priority format present (parquet, then CSV,
then legacy text) and every grid within a multi-grid file. A run passes
a check only when every partition passes it. Values equal to the legacy
``-999`` sentinel are treated as missing.

Severity ladder: ``pass`` (passed=True), ``warning`` (passed=False but
non-fatal), ``fail`` (passed=False and the run is unusable).
"""

from __future__ import annotations

from collections.abc import Callable
from dataclasses import asdict, dataclass, field
from pathlib import Path
from typing import Any

import numpy as np
import pandas as pd

from .._run_output import _list_run_output_files, _load_run_output_partitions

__all__ = [
    "CheckResult",
    "check_energy_balance_closure",
    "check_nan_proportion",
    "check_output_files_present",
    "check_provenance_present",
    "check_run",
]

# Tunables for Phase-1 checks. Conservative defaults; tighten in later
# phases once the user-base has reported real-world distributions.
_NAN_THRESHOLD_FRACTION = 0.05
_ENERGY_BALANCE_THRESHOLD = 0.10
_FLUX_VARIABLES = ("QH", "QE", "QN")

# Legacy SUEWS text output marks missing values with -999.
_MISSING_SENTINEL = -999.0

# Energy-balance identity used by the model driver (``qh_residual`` in
# ``suews_ctrl_driver.f95``): QN + QF + QMRain = QH + QE + QS + QM + QMFreeze.
# Current writers emit every term; reduced or older outputs may lack QF
# or the snow terms, which are then treated as zero and reported.
_CLOSURE_REQUIRED = ("QN", "QH", "QE", "QS")
_CLOSURE_SOURCES_OPTIONAL = ("QF", "QMRain")
_CLOSURE_SINKS_OPTIONAL = ("QM", "QMFreeze")


@dataclass
class CheckResult:
    """Outcome of a single diagnostic check.

    Attributes
    ----------
    name : str
        Stable check identifier (snake_case).
    severity : str
        One of ``"pass"``, ``"warning"``, ``"fail"``.
    passed : bool
        ``True`` when the check completed without flagging an issue.
    message : str
        Short human-readable summary.
    details : dict
        Free-form structured payload (counts, paths, percentages, etc.).
    """

    name: str
    severity: str
    passed: bool
    message: str
    details: dict[str, Any] = field(default_factory=dict)

    def to_dict(self) -> dict[str, Any]:
        return asdict(self)


# ---------------------------------------------------------------------------
# Helpers (private)
# ---------------------------------------------------------------------------


def _list_output_files(path_run_dir: Path) -> list[Path]:
    """Return all candidate output files under ``path_run_dir``.

    Recognises CSV, parquet and legacy text output produced by
    ``suews run`` / ``SUEWSSimulation.save``. The current ``save``
    implementation may write files at the run-dir root or under a
    subdirectory keyed by site name; we accept both.
    """
    return _list_run_output_files(path_run_dir)


def _load_output_partitions(
    path_run_dir: Path,
) -> list[tuple[str, pd.DataFrame]] | None:
    """Best-effort load of every run-output partition.

    Returns ``None`` when no recognisable output file is present. Errors
    during read are deliberately propagated to the caller, which decides
    whether to mark the check as ``fail`` or ``warning``.
    """
    if not _list_output_files(path_run_dir):
        return None
    return _load_run_output_partitions(path_run_dir)


def _numeric_series(df_output: pd.DataFrame, name: str) -> pd.Series:
    """Return column ``name`` as floats with sentinels and infinities as NaN."""
    column = df_output[name]
    if isinstance(column, pd.DataFrame):  # duplicate labels: keep the first
        column = column.iloc[:, 0]
    ser = pd.to_numeric(column, errors="coerce").astype(float)
    ser = ser.mask(ser == _MISSING_SENTINEL)
    return ser.replace([np.inf, -np.inf], np.nan)


def _closure_terms(df_output: pd.DataFrame) -> tuple[pd.DataFrame, list[str]]:
    """Build the closure terms of one partition.

    Returns the numeric frame of every closure term (absent optional
    terms filled with zero) and the list of optional terms that were
    absent from the output.
    """
    dict_terms: dict[str, pd.Series] = {}
    for name in _CLOSURE_REQUIRED:
        dict_terms[name] = _numeric_series(df_output, name)
    list_missing: list[str] = []
    for name in (*_CLOSURE_SOURCES_OPTIONAL, *_CLOSURE_SINKS_OPTIONAL):
        if name in df_output.columns:
            dict_terms[name] = _numeric_series(df_output, name)
        else:
            list_missing.append(name)
            dict_terms[name] = pd.Series(0.0, index=df_output.index)
    return pd.DataFrame(dict_terms), list_missing


def _missing_required(df_output: pd.DataFrame) -> list[str]:
    return [name for name in _CLOSURE_REQUIRED if name not in df_output.columns]


def _evaluate_partition_closure(df_terms: pd.DataFrame) -> dict[str, Any]:
    """Evaluate closure for one partition and return its statistics.

    Rows enter the evaluation when ``|QN| > 1`` and every term is finite.
    ``status`` is ``"pass"`` when the mean ratio is below the threshold and
    at least ``1 - _NAN_THRESHOLD_FRACTION`` of the non-trivial rows were
    evaluable, ``"no_evaluable_rows"`` when nothing could be evaluated,
    ``"low_coverage"`` when too many rows were skipped, and
    ``"residual"`` when the mean ratio exceeds the threshold.
    """
    ser_qn = df_terms["QN"]
    mask_nontrivial = ser_qn.notna() & (ser_qn.abs() > 1.0)
    mask_finite = df_terms.notna().all(axis=1)
    mask_valid = mask_nontrivial & mask_finite
    n_nontrivial = int(mask_nontrivial.sum())
    n_evaluated = int(mask_valid.sum())
    stats: dict[str, Any] = {
        "n_rows": len(df_terms),
        "n_rows_evaluated": n_evaluated,
        "n_rows_skipped_nonfinite": n_nontrivial - n_evaluated,
        "ratio_mean": None,
        "status": "no_evaluable_rows",
    }
    if n_evaluated == 0:
        return stats

    ser_sources = df_terms["QN"] + df_terms["QF"] + df_terms["QMRain"]
    ser_sinks = df_terms["QH"] + df_terms["QE"] + df_terms["QS"]
    ser_sinks += df_terms["QM"] + df_terms["QMFreeze"]
    ser_ratio = (ser_sources - ser_sinks).abs()[mask_valid] / ser_qn[mask_valid].abs()
    ratio_mean = float(ser_ratio.mean())
    stats["ratio_mean"] = ratio_mean
    coverage = n_evaluated / n_nontrivial
    stats["coverage"] = coverage
    if ratio_mean >= _ENERGY_BALANCE_THRESHOLD:
        stats["status"] = "residual"
    elif coverage < 1.0 - _NAN_THRESHOLD_FRACTION:
        stats["status"] = "low_coverage"
    else:
        stats["status"] = "pass"
    return stats


def _describe_partition_problem(label: str, stats: dict[str, Any]) -> str:
    status = stats["status"]
    if status == "residual":
        return f"{label}: residual {stats['ratio_mean']:.3f}"
    if status == "low_coverage":
        return f"{label}: only {stats['coverage']:.1%} of rows evaluable"
    return f"{label}: no evaluable rows"


def _read_error(name: str, path_run_dir: Path, exc: Exception) -> CheckResult:
    return CheckResult(
        name=name,
        severity="warning",
        passed=False,
        message=f"Could not read run output: {exc}",
        details={"run_dir": str(path_run_dir)},
    )


def _no_output(name: str, path_run_dir: Path) -> CheckResult:
    return CheckResult(
        name=name,
        severity="warning",
        passed=False,
        message="No output dataframe to inspect.",
        details={"run_dir": str(path_run_dir)},
    )


# ---------------------------------------------------------------------------
# Individual checks
# ---------------------------------------------------------------------------


def check_provenance_present(path_run_dir: Path) -> CheckResult:
    """Verify ``provenance.json`` is present in the run directory."""
    path_provenance = path_run_dir / "provenance.json"
    if path_provenance.exists():
        return CheckResult(
            name="provenance_present",
            severity="pass",
            passed=True,
            message="provenance.json present.",
            details={"path": str(path_provenance)},
        )
    return CheckResult(
        name="provenance_present",
        severity="warning",
        passed=False,
        message=(
            "provenance.json missing; this run directory has no provenance sidecar."
        ),
        details={"expected": str(path_provenance)},
    )


def check_output_files_present(path_run_dir: Path) -> CheckResult:
    """Verify at least one recognisable output file is present."""
    list_paths = _list_output_files(path_run_dir)
    if list_paths:
        return CheckResult(
            name="output_files_present",
            severity="pass",
            passed=True,
            message=f"Found {len(list_paths)} output file(s).",
            details={"files": [str(p) for p in list_paths[:10]]},
        )
    return CheckResult(
        name="output_files_present",
        severity="fail",
        passed=False,
        message="No df_output*.csv / *.parquet / *_SUEWS_*.txt files found.",
        details={"run_dir": str(path_run_dir)},
    )


def check_nan_proportion(path_run_dir: Path) -> CheckResult:
    """Check the missing-value proportion in QH / QE / QN per partition.

    A value is missing when it is NaN, non-finite or equal to the legacy
    ``-999`` sentinel. Every partition (file, and grid within a file) is
    judged separately so one broken grid or year cannot hide behind the
    others.

    Severity:
    - ``pass`` when every flux column in every partition is below 5%.
    - ``warning`` when any flux column in any partition exceeds the threshold.
    - ``fail`` when none of the flux columns are present (cannot judge).
    """
    try:
        list_partitions = _load_output_partitions(path_run_dir)
    except (OSError, ValueError, pd.errors.ParserError) as exc:
        return _read_error("nan_proportion", path_run_dir, exc)

    if list_partitions is None:
        return _no_output("nan_proportion", path_run_dir)

    dict_fractions: dict[str, dict[str, float]] = {}
    list_offenders: list[str] = []
    set_found: set[str] = set()
    list_columns_seen: list[str] = []
    for label, df_output in list_partitions:
        list_columns_seen = [str(col) for col in df_output.columns]
        dict_partition = {
            var: float(_numeric_series(df_output, var).isna().mean())
            for var in _FLUX_VARIABLES
            if var in df_output.columns
        }
        set_found.update(dict_partition)
        dict_fractions[label] = dict_partition
        list_offenders.extend(
            f"{label}:{var}={frac:.3%}"
            for var, frac in dict_partition.items()
            if frac > _NAN_THRESHOLD_FRACTION
        )

    if not set_found:
        return CheckResult(
            name="nan_proportion",
            severity="fail",
            passed=False,
            message="None of QH / QE / QN present in output.",
            details={"available_columns": list_columns_seen[:30]},
        )

    details = {
        "threshold_fraction": _NAN_THRESHOLD_FRACTION,
        "n_partitions": len(list_partitions),
        "fractions": dict_fractions,
    }
    if list_offenders:
        return CheckResult(
            name="nan_proportion",
            severity="warning",
            passed=False,
            message=(
                f"Missing fraction exceeds {_NAN_THRESHOLD_FRACTION * 100:.0f}% "
                f"in: {', '.join(list_offenders)}"
            ),
            details=details,
        )
    return CheckResult(
        name="nan_proportion",
        severity="pass",
        passed=True,
        message=(
            f"Missing fractions within {_NAN_THRESHOLD_FRACTION * 100:.0f}% "
            f"on QH/QE/QN across {len(list_partitions)} partition(s)."
        ),
        details=details,
    )


def check_energy_balance_closure(path_run_dir: Path) -> CheckResult:
    """Energy-balance consistency check, judged per output partition.

    For every partition (file, and grid within a file) computes
    ``mean(|(QN + QF + QMRain) - (QH + QE + QS + QM + QMFreeze)| / |QN|)``
    over rows where every term is finite and ``|QN| > 1``. QN, QH, QE
    and QS must be present; QF and the snow terms are treated as zero
    when their column is absent, and the absent terms are listed in
    ``details``.

    A partition passes when its mean ratio is below 10% and at least 95%
    of its non-trivial rows could be evaluated. The run passes only when
    every partition passes: a partition with an excessive residual, too
    few evaluable rows, or none at all, is reported and blocks the pass,
    so a healthy partition cannot mask a broken one. ``details`` carries
    the per-partition statistics and the row-weighted aggregate ratio.

    SUEWS closes this identity by construction, so the check is a
    consistency test of the saved output rather than evidence of
    scientific skill.
    """
    try:
        list_partitions = _load_output_partitions(path_run_dir)
    except (OSError, ValueError, pd.errors.ParserError) as exc:
        return _read_error("energy_balance_closure", path_run_dir, exc)

    if list_partitions is None:
        return _no_output("energy_balance_closure", path_run_dir)

    dict_partitions: dict[str, dict[str, Any]] = {}
    set_missing_optional: set[str] = set()
    list_problems: list[str] = []
    for label, df_output in list_partitions:
        list_missing_required = _missing_required(df_output)
        if list_missing_required:
            return CheckResult(
                name="energy_balance_closure",
                severity="fail",
                passed=False,
                message=(
                    f"{', '.join(list_missing_required)} missing in {label}; "
                    "cannot compute energy balance closure."
                ),
                details={
                    "partition": label,
                    "missing": list_missing_required,
                    "available_columns": [str(c) for c in df_output.columns][:30],
                },
            )
        df_terms, list_missing_optional = _closure_terms(df_output)
        set_missing_optional.update(list_missing_optional)
        stats = _evaluate_partition_closure(df_terms)
        dict_partitions[label] = stats
        if stats["status"] != "pass":
            list_problems.append(_describe_partition_problem(label, stats))

    n_evaluated = sum(st["n_rows_evaluated"] for st in dict_partitions.values())
    ratio_mean = (
        sum(
            st["ratio_mean"] * st["n_rows_evaluated"]
            for st in dict_partitions.values()
            if st["ratio_mean"] is not None
        )
        / n_evaluated
        if n_evaluated
        else None
    )
    details: dict[str, Any] = {
        "threshold_ratio": _ENERGY_BALANCE_THRESHOLD,
        "n_partitions": len(list_partitions),
        "n_rows": sum(st["n_rows"] for st in dict_partitions.values()),
        "n_rows_evaluated": n_evaluated,
        "n_rows_skipped_nonfinite": sum(
            st["n_rows_skipped_nonfinite"] for st in dict_partitions.values()
        ),
        "ratio_mean": ratio_mean,
        "terms_missing": sorted(set_missing_optional),
        "partitions": dict_partitions,
    }
    note_missing = (
        f" (absent terms treated as zero: {', '.join(details['terms_missing'])})"
        if details["terms_missing"]
        else ""
    )

    if list_problems:
        return CheckResult(
            name="energy_balance_closure",
            severity="warning",
            passed=False,
            message=(
                f"Closure not confirmed in {len(list_problems)} of "
                f"{len(list_partitions)} partition(s): "
                f"{'; '.join(list_problems)}{note_missing}."
            ),
            details=details,
        )
    return CheckResult(
        name="energy_balance_closure",
        severity="pass",
        passed=True,
        message=(
            f"Mean closure residual {ratio_mean:.3f} < "
            f"{_ENERGY_BALANCE_THRESHOLD:.2f} in every one of "
            f"{len(list_partitions)} partition(s){note_missing}."
        ),
        details=details,
    )


# ---------------------------------------------------------------------------
# Aggregator
# ---------------------------------------------------------------------------


_REGISTERED_CHECKS: tuple[Callable[[Path], CheckResult], ...] = (
    check_provenance_present,
    check_output_files_present,
    check_nan_proportion,
    check_energy_balance_closure,
)


def check_run(run_dir: Path) -> list[CheckResult]:
    """Run every registered check against ``run_dir`` and return the list.

    Each check is wrapped in a defensive try/except so that one buggy
    check cannot mask the others. A check that raises is recorded as a
    ``fail`` with the exception message.
    """
    path_run_dir = Path(run_dir)
    list_results: list[CheckResult] = []
    for check_fn in _REGISTERED_CHECKS:
        try:
            list_results.append(check_fn(path_run_dir))
        except Exception as exc:
            list_results.append(
                CheckResult(
                    name=check_fn.__name__,
                    severity="fail",
                    passed=False,
                    message=f"Check raised {type(exc).__name__}: {exc}",
                    details={"run_dir": str(path_run_dir)},
                )
            )
    return list_results
