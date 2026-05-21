"""Diagnostic checks for completed SUEWS run directories.

The aggregator ``check_run(run_dir)`` runs every Phase-1 check and returns
a list of ``CheckResult`` records. Each check is a small pure function
that takes a ``Path`` and returns one ``CheckResult``; this keeps the
checks individually testable and the aggregator trivial.

Phase-1 checks (intentionally minimal):

- ``check_provenance_present`` — ``provenance.json`` sidecar exists.
- ``check_output_files_present`` — at least one ``df_output*.csv`` or
  ``*.parquet`` produced by ``suews run`` is present.
- ``check_nan_proportion`` — NaN fraction in QH/QE/QN below 5%.
- ``check_energy_balance_closure`` — mean
  ``|QN - (QH + QE + QS + QF)| / |QN| < 0.10``.

Severity ladder: ``pass`` (passed=True), ``warning`` (passed=False but
non-fatal), ``fail`` (passed=False and the run is unusable).
"""

from __future__ import annotations

from collections.abc import Callable
from dataclasses import asdict, dataclass, field
from pathlib import Path
from typing import Any

import pandas as pd

from .._run_output import _list_run_output_files, _load_run_output_dataframe

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

    Recognises CSV and parquet output produced by ``suews run`` /
    ``SUEWSSimulation.save``. The current ``save`` implementation may write
    files at the run-dir root or under a subdirectory keyed by site name —
    we accept both.
    """
    return _list_run_output_files(path_run_dir)


def _load_output_dataframe(path_run_dir: Path) -> pd.DataFrame | None:
    """Best-effort load of run output into a single DataFrame.

    Returns ``None`` when no recognisable output file is present. Errors
    during read are deliberately propagated to the caller — the caller
    decides whether to mark the check as ``fail`` or ``warning``.
    """
    if not _list_output_files(path_run_dir):
        return None
    return _load_run_output_dataframe(path_run_dir)


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
    """Check NaN proportion in QH / QE / QN flux columns.

    Severity:
    - ``pass`` when all three flux columns have NaN fraction below 5%.
    - ``warning`` when any flux column exceeds the threshold.
    - ``fail`` when none of the flux columns are present (cannot judge).
    """
    try:
        df_output = _load_output_dataframe(path_run_dir)
    except (OSError, ValueError, pd.errors.ParserError) as exc:
        return CheckResult(
            name="nan_proportion",
            severity="warning",
            passed=False,
            message=f"Could not read run output to compute NaN proportions: {exc}",
            details={"run_dir": str(path_run_dir)},
        )

    if df_output is None:
        return CheckResult(
            name="nan_proportion",
            severity="warning",
            passed=False,
            message="No output dataframe to inspect.",
            details={"run_dir": str(path_run_dir)},
        )

    # Tolerate MultiIndex columns from the canonical SUEWS output.
    list_columns = [
        col[0] if isinstance(col, tuple) else col for col in df_output.columns
    ]
    df_output = df_output.copy()
    df_output.columns = list_columns

    found = {var: var in df_output.columns for var in _FLUX_VARIABLES}
    if not any(found.values()):
        return CheckResult(
            name="nan_proportion",
            severity="fail",
            passed=False,
            message="None of QH / QE / QN present in output.",
            details={"available_columns": list_columns[:30]},
        )

    dict_fractions = {
        var: float(df_output[var].isna().mean())
        for var in _FLUX_VARIABLES
        if found[var]
    }
    list_offenders = [
        f"{var}={frac:.3%}"
        for var, frac in dict_fractions.items()
        if frac > _NAN_THRESHOLD_FRACTION
    ]
    if list_offenders:
        return CheckResult(
            name="nan_proportion",
            severity="warning",
            passed=False,
            message=(
                f"NaN fraction exceeds {_NAN_THRESHOLD_FRACTION * 100:.0f}% "
                f"in: {', '.join(list_offenders)}"
            ),
            details={
                "threshold_fraction": _NAN_THRESHOLD_FRACTION,
                "fractions": dict_fractions,
            },
        )
    return CheckResult(
        name="nan_proportion",
        severity="pass",
        passed=True,
        message=f"NaN fractions within {_NAN_THRESHOLD_FRACTION * 100:.0f}% on QH/QE/QN.",
        details={"fractions": dict_fractions},
    )


def check_energy_balance_closure(path_run_dir: Path) -> CheckResult:
    """Approximate energy balance closure check.

    Computes ``mean(|QN - (QH + QE + QS + QF)| / |QN|)`` over rows where
    ``QN`` is finite and non-zero, then flags the run when the ratio
    exceeds 10%. ``QF`` and ``QS`` may be missing for very simple
    configurations — the check treats them as zero in that case rather
    than refusing to run.
    """
    try:
        df_output = _load_output_dataframe(path_run_dir)
    except (OSError, ValueError, pd.errors.ParserError) as exc:
        return CheckResult(
            name="energy_balance_closure",
            severity="warning",
            passed=False,
            message=f"Could not read run output to compute energy balance: {exc}",
            details={"run_dir": str(path_run_dir)},
        )

    if df_output is None:
        return CheckResult(
            name="energy_balance_closure",
            severity="warning",
            passed=False,
            message="No output dataframe to inspect.",
            details={"run_dir": str(path_run_dir)},
        )

    list_columns = [
        col[0] if isinstance(col, tuple) else col for col in df_output.columns
    ]
    df_output = df_output.copy()
    df_output.columns = list_columns

    if "QN" not in df_output.columns:
        return CheckResult(
            name="energy_balance_closure",
            severity="fail",
            passed=False,
            message="QN missing — cannot compute energy balance closure.",
            details={"available_columns": list_columns[:30]},
        )

    ser_qn = pd.to_numeric(df_output["QN"], errors="coerce")
    ser_qh = pd.to_numeric(df_output.get("QH", 0.0), errors="coerce").fillna(0.0)
    ser_qe = pd.to_numeric(df_output.get("QE", 0.0), errors="coerce").fillna(0.0)
    ser_qs = pd.to_numeric(df_output.get("QS", 0.0), errors="coerce").fillna(0.0)
    ser_qf = pd.to_numeric(df_output.get("QF", 0.0), errors="coerce").fillna(0.0)

    mask_valid = ser_qn.notna() & (ser_qn.abs() > 1.0)
    if not mask_valid.any():
        return CheckResult(
            name="energy_balance_closure",
            severity="warning",
            passed=False,
            message="QN has no finite, non-trivial values to evaluate closure.",
            details={"n_rows": len(df_output)},
        )

    ser_residual = (ser_qn - (ser_qh + ser_qe + ser_qs + ser_qf)).abs()
    ser_ratio = ser_residual[mask_valid] / ser_qn[mask_valid].abs()
    ratio_mean = float(ser_ratio.mean())

    if ratio_mean < _ENERGY_BALANCE_THRESHOLD:
        return CheckResult(
            name="energy_balance_closure",
            severity="pass",
            passed=True,
            message=f"Mean closure residual {ratio_mean:.3f} < {_ENERGY_BALANCE_THRESHOLD:.2f}.",
            details={"ratio_mean": ratio_mean, "n_rows": int(mask_valid.sum())},
        )
    return CheckResult(
        name="energy_balance_closure",
        severity="warning",
        passed=False,
        message=(
            f"Mean closure residual {ratio_mean:.3f} exceeds "
            f"{_ENERGY_BALANCE_THRESHOLD:.2f}."
        ),
        details={"ratio_mean": ratio_mean, "n_rows": int(mask_valid.sum())},
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
