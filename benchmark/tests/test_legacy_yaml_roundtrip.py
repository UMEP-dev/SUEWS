"""Validate the stored legacy-benchmark YAML configs (Phase B).

Each ``benchmark/inputs/legacy/<ver>.yml`` is the modern-YAML view of a
historical canonical-KCL benchmark input, produced by the forward
``suews-convert`` path (legacy tables -> current schema). Phase B's acceptance
gate is two-fold and BOTH halves must hold for every stored file:

  1. The YAML loads under the *current* validator
     (``SUEWSConfig(**yaml.safe_load(...))``); and
  2. its ``df_state`` round-trips stably:
     ``df = cfg.to_df_state(); cfg2 = from_df_state(df); df2 = cfg2.to_df_state()``
     with ``df.equals(df2)``.

Stability proves the YAML is a faithful *modern* representation of the legacy
input (it is not yet table-regenerative -- that is Phase C). Only versions that
pass both halves are stored here; versions whose legacy data the current
validator rejects (e.g. an out-of-bounds initial albedo carried forward
faithfully from the source tables) are Phase-C candidates and are documented in
``benchmark/inputs/legacy/README.md`` rather than committed as fixtures.

This test lives under ``benchmark/tests/`` (outside the repo ``test/`` tree),
so it needs no ``pytest.mark`` registration.
"""
import sys
from pathlib import Path

import pytest
import yaml

# supy is the heavy dependency; skip cleanly if the editable build is absent so
# the pure-Python benchmark units still run in a minimal environment.
pytest.importorskip("supy")
from supy.data_model.core.config import SUEWSConfig  # noqa: E402

LEGACY_DIR = Path(__file__).resolve().parents[1] / "inputs" / "legacy"


def _legacy_yaml_files():
    """All stored legacy benchmark YAMLs, sorted by version tag."""
    return sorted(LEGACY_DIR.glob("[0-9][0-9][0-9][0-9]*.yml"))


def test_legacy_yaml_dir_is_populated():
    """At least one stored legacy YAML exists (guards an empty glob)."""
    files = _legacy_yaml_files()
    assert files, f"no legacy benchmark YAMLs found under {LEGACY_DIR}"


@pytest.mark.parametrize(
    "yaml_path",
    _legacy_yaml_files(),
    ids=lambda p: p.stem,
)
def test_legacy_yaml_loads_and_roundtrips(yaml_path):
    """Stored legacy YAML loads under the current schema and round-trips stably.

    Both Phase-B gate halves are asserted: the config validates, and its
    ``df_state`` is fixed under one ``to_df_state -> from_df_state ->
    to_df_state`` cycle.
    """
    data = yaml.safe_load(yaml_path.read_text(encoding="utf-8"))

    # Gate half 1: loads under the current validator.
    cfg = SUEWSConfig(**data)

    # Gate half 2: df_state round-trips stably.
    df = cfg.to_df_state()
    cfg2 = type(cfg).from_df_state(df)
    df2 = cfg2.to_df_state()

    if not df.equals(df2):
        diff_cols = [c for c in df.columns if not df[c].equals(df2[c])]
        pytest.fail(
            f"{yaml_path.name}: df_state not stable under round-trip; "
            f"{len(diff_cols)} differing column(s): {diff_cols[:20]}"
        )


def test_legacy_yaml_is_clean_of_local_paths():
    """No absolute/temp path artefacts leak into the stored YAMLs.

    The forward converter can leave an absolute machine-specific temp path as
    the ``output.dir`` and an absolute forcing path; Phase B cleans both to
    neutral placeholders. Guard against a regression that re-commits a
    machine-specific path.
    """
    bad_markers = ("/private/", "/var/folders", "/Users/")
    offenders = []
    for path in _legacy_yaml_files():
        text = path.read_text(encoding="utf-8")
        for marker in bad_markers:
            if marker in text:
                offenders.append(f"{path.name}: contains {marker!r}")
    assert not offenders, "local path artefacts in stored YAML:\n" + "\n".join(offenders)
