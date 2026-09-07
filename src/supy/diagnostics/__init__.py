"""Diagnostic checks for completed SUEWS run directories.

The aggregator :func:`check_run` runs every Phase-1 check and returns a
list of :class:`CheckResult` records. Each check is a small pure
function that takes a ``Path`` and returns one ``CheckResult``; this
keeps the checks individually testable and the aggregator trivial.

Phase-1 checks (intentionally minimal):

- :func:`check_provenance_present` -- ``provenance.json`` sidecar exists.
- :func:`check_output_files_present` -- at least one ``df_output*.csv``,
  ``*.parquet`` or legacy ``*_SUEWS_*.txt`` produced by ``suews run`` is
  present.
- :func:`check_nan_proportion` -- missing fraction in QH/QE/QN below 5%
  in every output partition.
- :func:`check_energy_balance_closure` -- mean
  ``|(QN + QF + QMRain) - (QH + QE + QS + QM + QMFreeze)| / |QN| < 0.10``.

Every partition of the run output is inspected (all files of the
highest-priority format present, and every grid within a file); the
legacy ``-999`` sentinel is treated as missing.

Severity ladder: ``pass`` (passed=True), ``warning`` (passed=False but
non-fatal), ``fail`` (passed=False and the run is unusable).
"""

from ._checks import (
    CheckResult,
    check_energy_balance_closure,
    check_nan_proportion,
    check_output_files_present,
    check_provenance_present,
    check_run,
)

__all__ = [
    "CheckResult",
    "check_energy_balance_closure",
    "check_nan_proportion",
    "check_output_files_present",
    "check_provenance_present",
    "check_run",
]
