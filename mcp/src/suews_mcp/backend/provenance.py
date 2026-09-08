"""Helpers for reading ``provenance.json`` and ``diagnostics.json`` sidecars."""

from __future__ import annotations

import json
from pathlib import Path
from typing import Any


def read_provenance(run_dir: Path) -> dict[str, Any]:
    """Return the parsed ``<run_dir>/provenance.json`` content.

    Raises :class:`FileNotFoundError` when the sidecar is missing.
    """
    path = Path(run_dir) / "provenance.json"
    if not path.exists():
        raise FileNotFoundError(
            f"provenance.json not found in {run_dir}. "
            "The sidecar is written by `suews run <config.yml>` and by "
            "`SUEWSSimulation.save()`; older runs and copies that kept only "
            "the data files have none. Re-run with `suews run <config.yml>` "
            "(`run` has no `--format` option) or save the run again."
        )
    return json.loads(path.read_text(encoding="utf-8"))


def read_diagnostics(run_dir: Path) -> dict[str, Any]:
    """Return the parsed ``<run_dir>/diagnostics.json`` content if present."""
    path = Path(run_dir) / "diagnostics.json"
    if not path.exists():
        raise FileNotFoundError(
            f"diagnostics.json not found in {run_dir} — "
            "run `suews diagnose <run_dir> --format json > {run_dir}/diagnostics.json` first."
        )
    return json.loads(path.read_text(encoding="utf-8"))
