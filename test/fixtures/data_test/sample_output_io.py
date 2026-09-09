"""Shared read/write for the sharded sample-output reference fixture.

The one-year reference output (105,408 rows x 113 columns) used to live in a
single ``sample_output.csv.gz`` blob. Two problems with that shape:

- gzip is opaque to git, so every genuine physics change stored a fresh ~55 MB
  blob with no delta against the previous one, and ``git diff`` only ever said
  "binary files differ".
- a full-year *plain* CSV at full float repr is ~156 MB, over the repository's
  pre-commit size limit.

The fixture is therefore stored as twelve plain-CSV monthly shards
(``sample_output_2012-MM.csv``), written at seven significant figures. Each
shard is well under the size limit and diff-able: a physics change is visible
line by line rather than as "binary files differ". Plain CSV also has no
timestamp header, so the fixture is byte-reproducible by construction (no gzip
``mtime`` to pin).

What plain CSV does NOT buy is cheap history. Both refreshes before the
precision change stored twelve fresh blobs with no delta base, because a
physics change moves nearly every float in its last digits and git's delta
heuristic finds nothing to reuse. Truncating the write precision is what bounds
that cost: it removes the noise digits that made every line differ, and roughly
halves the committed reference.

Alongside the shards sits ``provenance.json``, written by the generator and
asserted by the sample-output tests: the SuPy build, compiler, platform and
per-shard content hashes of the run the reference came from. See
:func:`write_reference_provenance`.

Both the generator (``scripts/suews/gen_sample_output.py``) and the tests import
this module so the split/combine convention has a single source of truth.
"""

from __future__ import annotations

import hashlib
import json
from pathlib import Path
import platform
import subprocess
import sys
from typing import Any

import pandas as pd

# The reference is one leap-year at a 5-minute timestep. The final timestep is
# stamped at the start of the next interval (2013-01-01 00:00), so a naive
# month grouping would spill a single row into a 13th shard; that row is folded
# into the December shard to keep exactly twelve files.
SHARD_PREFIX = "sample_output_2012-"
SHARD_GLOB = "sample_output_2012-*.csv"
_LAST_MONTH = pd.Period("2012-12", "M")
EXPECTED_ROWS = 105408

# Write precision for the reference floats.
#
# ``%.7g`` keeps seven significant figures, so a written value carries at most
# 5e-7 relative error against the double it came from (half a unit in the
# seventh digit, worst case at a leading mantissa digit of 1).
#
# The tightest tolerance any test applies to this reference is T2's
# ``rtol=0.002`` in ``test/core/test_sample_output.py::TOLERANCE_CONFIG``. That
# is 4000 times the worst-case write error, so the write precision is not what
# any comparison in this repository is sensitive to. Six significant figures
# would already have left a factor of 400 (measured at most 5e-6 relative error
# on this very reference); seven is chosen to keep the margin unquestionable
# while still removing the noise digits that made every line of every shard
# differ on every refresh.
#
# Raising the precision is a size and history cost, not a correctness gain.
# Lowering it below six significant figures would start to encroach on T2 and
# must not be done without redoing that measurement.
REFERENCE_FLOAT_FORMAT = "%.7g"

PROVENANCE_FILENAME = "provenance.json"
PROVENANCE_FORMAT_VERSION = 1


def write_sample_output_shards(df: pd.DataFrame, data_dir: Path) -> list[Path]:
    """Write ``df`` as twelve monthly plain-CSV shards under ``data_dir``.

    ``df`` must carry the ``(grid, datetime)`` MultiIndex produced by
    ``SUEWSSimulation.run``. Row order within each month is preserved. Floats
    are written at :data:`REFERENCE_FLOAT_FORMAT`.
    """
    data_dir = Path(data_dir)
    datetimes = df.index.get_level_values("datetime")
    months = datetimes.to_period("M")
    # Fold the boundary timestep (2013-01) into the last real month.
    months = months.where(months <= _LAST_MONTH, _LAST_MONTH)

    written: list[Path] = []
    for period, shard in df.groupby(months, sort=True):
        path = data_dir / f"sample_output_{period}.csv"
        shard.to_csv(path, float_format=REFERENCE_FLOAT_FORMAT)
        written.append(path)
    return written


def load_sample_output(data_dir: Path) -> pd.DataFrame:
    """Reconstruct the full reference frame from the monthly shards.

    Globs the shards, sorts by name (``YYYY-MM`` sorts chronologically),
    concatenates, and checks the result is the expected length with a unique
    index -- so a dropped or overlapping shard fails loudly rather than
    silently shortening the reference.
    """
    data_dir = Path(data_dir)
    shard_paths = sorted(data_dir.glob(SHARD_GLOB))
    if not shard_paths:
        raise FileNotFoundError(
            f"[X] no sample-output shards found under {data_dir} "
            f"(expected {SHARD_GLOB})"
        )

    frame = pd.concat(
        pd.read_csv(
            p,
            index_col=[0, 1],
            parse_dates=[1],
            float_precision="round_trip",
        )
        for p in shard_paths
    )

    if not frame.index.is_unique:
        raise ValueError(
            f"[X] reconstructed reference has duplicate index entries across "
            f"{len(shard_paths)} shards -- shards overlap"
        )
    if len(frame) != EXPECTED_ROWS:
        raise ValueError(
            f"[X] reconstructed reference has {len(frame)} rows, "
            f"expected {EXPECTED_ROWS} -- a shard is missing or truncated"
        )
    return frame


# ---------------------------------------------------------------------------
# Provenance sidecar
# ---------------------------------------------------------------------------
#
# The sidecar records the identity of the run the shards came from. It is
# deliberately hashed over the shard BYTES rather than over an in-memory frame:
# file bytes are bit-identical on every platform and every pandas version, so
# the assertion in the tests can only fail because the reference or the sidecar
# actually moved -- never because a hashing implementation changed underneath
# it.


def _file_digest(path: Path) -> str:
    """SHA-256 of a file's bytes."""
    digest = hashlib.sha256()
    with Path(path).open("rb") as handle:
        for chunk in iter(lambda: handle.read(1 << 20), b""):
            digest.update(chunk)
    return digest.hexdigest()


def shard_identities(data_dir: Path) -> list[dict[str, Any]]:
    """Describe every shard under ``data_dir`` by name, size and SHA-256.

    Only file names are recorded, never directories, so the sidecar can be read
    on any checkout without carrying the producing machine's layout.
    """
    data_dir = Path(data_dir)
    return [
        {
            "name": path.name,
            "size_bytes": path.stat().st_size,
            "sha256": _file_digest(path),
        }
        for path in sorted(data_dir.glob(SHARD_GLOB))
    ]


def column_names_sha256(df: pd.DataFrame) -> str:
    """SHA-256 over the reference's column names, in order.

    Cheap identity for the output contract: a column added, dropped or
    reordered changes this without hashing 105,408 rows of values.
    """
    payload = "\n".join(map(str, df.columns))
    return hashlib.sha256(payload.encode("utf-8")).hexdigest()


def _meson_compilers(repo_root: Path) -> dict[str, Any] | None:
    """Return the compiler identities meson recorded for the local build.

    ``None`` when there is no meson build directory to introspect (a wheel
    install, or a fresh clone), which is not an error: the sidecar records what
    the producing machine could see.

    Absolute compiler paths from ``exelist`` are deliberately dropped; only the
    identity and version are kept, so the sidecar carries no machine layout.
    """
    build_dirs = sorted((repo_root / "build").glob("*/meson-info"))
    if not build_dirs:
        return None
    try:
        result = subprocess.run(
            ["meson", "introspect", "--compilers", str(build_dirs[0].parent)],
            capture_output=True,
            text=True,
            check=True,
        )
        host = json.loads(result.stdout).get("host", {})
    except (OSError, subprocess.CalledProcessError, json.JSONDecodeError):
        return None
    return {
        language: {
            "id": info.get("id"),
            "version": info.get("version"),
            "full_version": info.get("full_version"),
        }
        for language, info in host.items()
    }


def _fortran_build_profile(repo_root: Path) -> str | None:
    """Return the Fortran build profile, resolved by the build's own code.

    ``src/supy/run_make.py`` is a Meson helper script rather than part of the
    installed package, so it is loaded from the checkout by path instead of
    imported. Asking it, rather than re-reading ``SUEWS_BUILD_PROFILE`` here,
    keeps one implementation of the default and of the validation.

    ``None`` when the checkout is not available, so there is nothing to ask.
    """
    import importlib.util

    path = repo_root / "src" / "supy" / "run_make.py"
    if not path.is_file():
        return None
    spec = importlib.util.spec_from_file_location("_suews_run_make", path)
    if spec is None or spec.loader is None:
        return None
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module.build_profile_from_env()


def build_identity(repo_root: Path) -> dict[str, Any]:
    """Describe the build that produced the reference: SuPy, compiler, flags."""
    from supy._provenance import supy_build_identity

    identity = dict(supy_build_identity())
    # The profile names the Fortran flag set (release: -O3, no runtime checks;
    # checked: -O0 -fcheck=all). The flags themselves live in
    # src/suews/Makefile.gfortran, keyed on this profile, and are deliberately
    # not copied here: a copy goes stale the first time they are tuned.
    identity["fortran_build_profile"] = _fortran_build_profile(repo_root)
    identity["compilers"] = _meson_compilers(repo_root)
    return identity


def platform_identity() -> dict[str, Any]:
    """Describe the machine the reference was generated on."""
    return {
        "system": platform.system(),
        "machine": platform.machine(),
        "python": ".".join(map(str, sys.version_info[:3])),
        "pandas": pd.__version__,
    }


def write_reference_provenance(
    df: pd.DataFrame, data_dir: Path, repo_root: Path
) -> Path:
    """Write ``provenance.json`` beside the shards; return its path.

    Call after :func:`write_sample_output_shards`: the sidecar hashes the shard
    files as they now stand on disk.
    """
    from supy._provenance import now_utc_iso

    data_dir = Path(data_dir)
    payload = {
        "format_version": PROVENANCE_FORMAT_VERSION,
        "generated_utc": now_utc_iso(),
        "float_format": REFERENCE_FLOAT_FORMAT,
        "rows": int(len(df)),
        "columns": int(df.shape[1]),
        "column_names_sha256": column_names_sha256(df),
        "shards": shard_identities(data_dir),
        "build": build_identity(repo_root),
        "platform": platform_identity(),
    }
    path = data_dir / PROVENANCE_FILENAME
    path.write_text(
        json.dumps(payload, indent=2, sort_keys=True, ensure_ascii=True) + "\n",
        encoding="utf-8",
    )
    return path


def read_reference_provenance(data_dir: Path) -> dict[str, Any]:
    """Read the sidecar beside the shards; raise if it is absent."""
    path = Path(data_dir) / PROVENANCE_FILENAME
    if not path.is_file():
        raise FileNotFoundError(
            f"[X] {PROVENANCE_FILENAME} not found in {data_dir} -- regenerate "
            f"the reference with scripts/suews/gen_sample_output.py"
        )
    return json.loads(path.read_text(encoding="utf-8"))
