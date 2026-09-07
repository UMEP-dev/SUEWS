"""Run provenance sidecar (``provenance.json``) for saved SUEWS runs.

``SUEWSSimulation.save()`` writes a compact ``provenance.json`` next to the
output files so that a saved run directory carries enough identity to be
audited later: which configuration and forcing files were used (by name and
content hash rather than by machine-specific absolute path), which SuPy build
ran, what period was requested and what period was actually simulated, and
how the timestamps should be read.

Consumers are :func:`supy.diagnostics.check_provenance_present` (via
``suews diagnose``) and the MCP ``suews://runs/{run_id}/provenance`` resource.

The file is a plain JSON object with ``format_version`` at the top level so
consumers can detect later shape changes. It never contains secrets or
absolute paths from the producing machine.
"""

from __future__ import annotations

from datetime import datetime, timezone
import hashlib
import json
from pathlib import Path
from typing import Any, Optional

import pandas as pd

from ._version_scm import __version__

PROVENANCE_FILENAME = "provenance.json"
PROVENANCE_FORMAT_VERSION = 1

# Forcing timestamps label the END of each interval (docs: inputs/forcing-data),
# and output rows inherit that labelling; record it so a reader of the sidecar
# does not have to know the convention beforehand.
TIMESTAMP_CONVENTION = "interval_end"


def now_utc_iso() -> str:
    """Return the current UTC time as ISO 8601 with a trailing ``Z``."""
    return (
        datetime
        .now(timezone.utc)
        .replace(microsecond=0)
        .isoformat()
        .replace("+00:00", "Z")
    )


def file_identity(path: Path) -> dict[str, Any]:
    """Describe a file by name, size and SHA-256 of its content.

    Only the file name is recorded, never its directory, so the sidecar can be
    shared without exposing the producing machine's directory layout.
    """
    path = Path(path)
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for chunk in iter(lambda: handle.read(1 << 20), b""):
            digest.update(chunk)
    return {
        "name": path.name,
        "size_bytes": path.stat().st_size,
        "sha256": digest.hexdigest(),
    }


def dataframe_sha256(df: pd.DataFrame) -> str:
    """Content hash of a model-ready frame: column names, index and values.

    This is the identity of what was actually simulated, which differs from
    the source file whenever the loader resampled, renamed or converted it,
    or the user edited the frame in memory.
    """
    digest = hashlib.sha256()
    digest.update("|".join(map(str, df.columns)).encode("utf-8"))
    digest.update(pd.util.hash_pandas_object(df, index=True).values.tobytes())
    return digest.hexdigest()


def json_sha256(obj: Any) -> str:
    """Content hash of a JSON-serialisable object with sorted keys."""
    text = json.dumps(obj, sort_keys=True, ensure_ascii=True, default=str)
    return hashlib.sha256(text.encode("utf-8")).hexdigest()


def timestamp_to_iso(value: Any) -> Optional[str]:
    """Render a pandas/datetime timestamp as ISO 8601, or ``None``."""
    if value is None:
        return None
    try:
        if pd.isna(value):
            return None
        return pd.Timestamp(value).isoformat()
    except (TypeError, ValueError):
        return str(value)


def requested_bound_to_str(value: Any) -> Optional[str]:
    """Render a user- or config-supplied period bound as a string.

    ``run()`` accepts strings, timestamps or ``RefValue``-wrapped config
    values; the sidecar records what was asked for without reinterpreting it.
    """
    if value is None:
        return None
    if isinstance(value, (pd.Timestamp, datetime)):
        # ``pd.Timestamp.value`` is the nanosecond integer; never unwrap it.
        return timestamp_to_iso(value)
    value = getattr(value, "value", value)
    if value is None:
        return None
    if isinstance(value, (pd.Timestamp, datetime)):
        return timestamp_to_iso(value)
    return str(value)


def supy_build_identity() -> dict[str, Any]:
    """Return the SuPy version and git commit of the running build."""
    try:
        # Lazy: ``cmd`` pulls in click and the CLI modules, which the
        # simulation core does not otherwise need at import time.
        from .cmd.json_envelope import _git_commit

        git_commit = _git_commit()
    except Exception:  # pragma: no cover - defensive; provenance must not fail a save
        git_commit = None
    return {"supy_version": __version__, "git_commit": git_commit}


def write_provenance(path_dir: Path, payload: dict[str, Any]) -> Path:
    """Write ``payload`` as ``<path_dir>/provenance.json`` and return its path."""
    path_dir = Path(path_dir)
    path_dir.mkdir(parents=True, exist_ok=True)
    path_provenance = path_dir / PROVENANCE_FILENAME
    text = json.dumps(payload, indent=2, sort_keys=True, ensure_ascii=True)
    path_provenance.write_text(text + "\n", encoding="utf-8")
    return path_provenance


def read_provenance(path_dir: Path) -> dict[str, Any]:
    """Read ``<path_dir>/provenance.json``; raise ``FileNotFoundError`` if absent."""
    path_provenance = Path(path_dir) / PROVENANCE_FILENAME
    if not path_provenance.is_file():
        raise FileNotFoundError(f"{PROVENANCE_FILENAME} not found in {path_dir}")
    return json.loads(path_provenance.read_text(encoding="utf-8"))
