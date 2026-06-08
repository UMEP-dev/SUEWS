"""Faithful round-trip: legacy SUEWS tables -> 2025a tables -> legacy tables.

Exercises the reverse table converter
(:mod:`supy.util.converter.table.reverse`): a legacy table set is converted
forward to the current ``2025a`` schema, captured, and regenerated back to its
original version, then compared against the (cleaned) source.

Faithfulness is asserted at the *data* level -- identical column set, identical
column order, identical per-``Code`` values (numeric tolerance for the forward
converter's float reformatting, e.g. ``4.11e6`` <-> ``4110000.0``). This is the
guarantee that matters for re-running the historical Fortran, which reads these
tables positionally with a list-directed (format-free) parse.

Fixtures are the canonical KCL ``Release/InputTables/<ver>`` sets vendored in the
``2020a`` git tag; the test extracts them with ``git archive`` and skips cleanly
where the tag (or git) is unavailable. ``2018b`` is the narrowest delta on the
chain (one captured delete) and ``2016a`` the widest (173 adds, root layout).
"""
from pathlib import Path
import shutil
import subprocess

import pytest
import yaml

pytest.importorskip("supy")
from supy._load import load_InitialCond_grid_df  # noqa: PLC2701 (internal loader; no public equivalent)
from supy.data_model.core.config import SUEWSConfig
from supy.util.converter.table import reverse as R
from supy.util.converter.table.table import (
    clean_legacy_table,
    convert_table,
)
from supy.util.converter.table.table_writer import df_state_to_tables

_REPO_ROOT = Path(__file__).resolve().parents[2]
_TAG = "2020a"


def _extract_legacy_tables(ver: str, dest: Path) -> Path:
    """Extract ``Release/InputTables/<ver>`` from the 2020a tag, or skip."""
    rel = f"Release/InputTables/{ver}"
    dest.mkdir(parents=True, exist_ok=True)
    try:
        archive = subprocess.run(
            ["git", "-C", str(_REPO_ROOT), "archive", _TAG, rel],
            capture_output=True,
            check=True,
        ).stdout
    except (subprocess.CalledProcessError, FileNotFoundError) as exc:
        pytest.skip(f"legacy fixture {ver} unavailable from tag {_TAG}: {exc}")
    tar_in = dest / "archive.tar"
    tar_in.write_bytes(archive)
    shutil.unpack_archive(str(tar_in), str(dest))
    src = dest / rel
    if not (src / "RunControl.nml").exists():
        pytest.skip(f"legacy fixture {ver} missing RunControl.nml")
    return src


def _num(token: str):
    try:
        return float(token)
    except (TypeError, ValueError):
        return None


def _values_equal(a: str, b: str) -> bool:
    na, nb = _num(a), _num(b)
    if na is not None and nb is not None:
        return abs(na - nb) <= 1e-6 * max(1.0, abs(na), abs(nb))
    return a == b


def _compare_table(path_expected: Path, path_actual: Path) -> list[str]:
    """Return a list of faithfulness violations (empty == faithful)."""
    he, re_ = R._read_table(path_expected)
    ha, ra = R._read_table(path_actual)
    issues: list[str] = []
    if he != ha:
        issues.append(
            f"header mismatch (order_ok={he == ha}): "
            f"expected_only={[c for c in he if c not in ha]} "
            f"actual_only={[c for c in ha if c not in he]}"
        )
    common = [c for c in he if c in ha]
    de = {r[0]: r for r in re_}
    da = {r[0]: r for r in ra}
    if set(de) != set(da):
        issues.append(
            f"Code set mismatch: expected_only={set(de) - set(da)} "
            f"actual_only={set(da) - set(de)}"
        )
    for code in set(de) & set(da):
        for col in common:
            ie, ia = he.index(col), ha.index(col)
            ve = de[code][ie] if len(de[code]) > ie else None
            va = da[code][ia] if len(da[code]) > ia else None
            if not _values_equal(ve, va):
                issues.append(f"value Code {code} {col}: expected={ve} actual={va}")
    return issues


@pytest.mark.parametrize("ver", ["2018b", "2016a"])
def test_legacy_table_roundtrip_is_faithful(ver, tmp_path):
    """``legacy -> 2025a -> legacy`` regenerates the source tables faithfully."""
    src = _extract_legacy_tables(ver, tmp_path / "src")
    fwd = tmp_path / "fwd"
    flat = tmp_path / "flat"
    rev = tmp_path / "rev"
    cleaned = tmp_path / "cleaned"
    for d in (fwd, flat, rev, cleaned):
        d.mkdir(parents=True, exist_ok=True)

    # Forward to the current schema, then flatten the produced table set.
    convert_table(str(src), str(fwd), ver, "2025a", validate_profiles=False)
    for produced in list(fwd.rglob("SUEWS_*.txt")) + list(fwd.rglob("*.nml")):
        shutil.copyfile(produced, flat / produced.name)

    # Capture legacy extras and regenerate the original tables.
    extras = R.capture_legacy_extras(src, ver)
    R.reverse_convert_table(flat, rev, ver, extras)

    # Cleaned baseline: source tables through the forward step-1 cleaning.
    for tbl in src.glob("SUEWS_*.txt"):
        dst = cleaned / tbl.name
        shutil.copyfile(tbl, dst)
        clean_legacy_table(str(dst))

    failures = {}
    for tbl in sorted(src.glob("SUEWS_*.txt")):
        regenerated = rev / tbl.name
        assert regenerated.exists(), f"{tbl.name} not regenerated for {ver}"
        issues = _compare_table(cleaned / tbl.name, regenerated)
        if issues:
            failures[tbl.name] = issues[:5]

    assert not failures, f"{ver} round-trip not faithful:\n" + "\n".join(
        f"  {f}: {iss}" for f, iss in failures.items()
    )


def test_runcontrol_roundtrip_preserves_keys(tmp_path):
    """RunControl.nml regenerates with identical keys and values (2018b)."""
    f90nml = pytest.importorskip("f90nml")
    ver = "2018b"
    src = _extract_legacy_tables(ver, tmp_path / "src")
    fwd = tmp_path / "fwd"
    flat = tmp_path / "flat"
    rev = tmp_path / "rev"
    for d in (fwd, flat, rev):
        d.mkdir(parents=True, exist_ok=True)

    convert_table(str(src), str(fwd), ver, "2025a", validate_profiles=False)
    for produced in list(fwd.rglob("SUEWS_*.txt")) + list(fwd.rglob("*.nml")):
        shutil.copyfile(produced, flat / produced.name)
    extras = R.capture_legacy_extras(src, ver)
    R.reverse_convert_table(flat, rev, ver, extras)

    src_rc = f90nml.read(str(src / "RunControl.nml"))["runcontrol"]
    rev_rc = f90nml.read(str(rev / "RunControl.nml"))["runcontrol"]
    assert set(src_rc) == set(rev_rc), (
        f"RunControl key set differs: "
        f"src_only={set(src_rc) - set(rev_rc)} rev_only={set(rev_rc) - set(src_rc)}"
    )
    diffs = {k: (src_rc[k], rev_rc[k]) for k in src_rc if str(src_rc[k]) != str(rev_rc[k])}
    assert not diffs, f"RunControl value diffs: {diffs}"


# --------------------------------------------------------------------------- #
# Full round-trip: legacy tables <-> modern YAML
# --------------------------------------------------------------------------- #
def _forward_to_yaml(ver: str, src: Path, tmp: Path):
    """``legacy tables -> 2025a template -> df_state -> YAML -> df_state``.

    Returns ``(template_dir, df_state)`` where ``df_state`` has been through the
    YAML leg (the modern config round-trip), so the reverse drives off it.
    """
    template = tmp / "T0"
    convert_table(str(src), str(template), ver, "2025a", validate_profiles=False)
    df0 = load_InitialCond_grid_df(next(template.rglob("RunControl.nml")))
    cfg = SUEWSConfig.from_df_state(df0)
    cfg.strict_initial_state_bounds = False  # legacy provenance (C3), as suews-convert does
    cfg.to_yaml(tmp / "config.yml")
    df = SUEWSConfig(
        **yaml.safe_load((tmp / "config.yml").read_text(encoding="utf-8"))
    ).to_df_state()
    return template, df


def _reverse_via_writer(df, template: Path, ver: str, src: Path, tmp: Path) -> Path:
    """``df_state -> 2025a tables (writer) -> legacy tables (reverse)``."""
    written = df_state_to_tables(df, template, tmp / "Tprime")
    flat = tmp / "flat"
    flat.mkdir()
    for produced in list(written.rglob("SUEWS_*.txt")) + list(
        written.parent.rglob("*.nml")
    ):
        shutil.copyfile(produced, flat / produced.name)
    rev = tmp / "legacy_rt"
    R.reverse_convert_table(flat, rev, ver, R.capture_legacy_extras(src, ver))
    return rev


@pytest.mark.parametrize("ver", ["2016a", "2018b"])
def test_full_roundtrip_legacy_yaml_legacy_is_faithful(ver, tmp_path):
    """``legacy tables -> modern YAML -> legacy tables`` regenerates the source.

    Exercises the whole loop, including the ``df_state -> 2025a tables`` writer
    that closes it. Faithfulness is asserted at the same data level as the
    table-only round-trip. ``2018b`` also covers the C3 legacy-bounds path.
    """
    src = _extract_legacy_tables(ver, tmp_path / "src")
    template, df = _forward_to_yaml(ver, src, tmp_path)
    rev = _reverse_via_writer(df, template, ver, src, tmp_path)

    cleaned = tmp_path / "cleaned"
    cleaned.mkdir()
    for tbl in src.glob("SUEWS_*.txt"):
        dst = cleaned / tbl.name
        shutil.copyfile(tbl, dst)
        clean_legacy_table(str(dst))

    failures = {}
    for tbl in sorted(src.glob("SUEWS_*.txt")):
        regenerated = rev / tbl.name
        assert regenerated.exists(), f"{tbl.name} not regenerated for {ver}"
        issues = _compare_table(cleaned / tbl.name, regenerated)
        if issues:
            failures[tbl.name] = issues[:5]

    assert not failures, f"{ver} full round-trip not faithful:\n" + "\n".join(
        f"  {f}: {iss}" for f, iss in failures.items()
    )


def test_writer_reload_equivalence(tmp_path):
    """``load(df_state_to_tables(df_state)) == df_state`` (the writer contract)."""
    ver = "2016a"
    src = _extract_legacy_tables(ver, tmp_path / "src")
    template = tmp_path / "T0"
    convert_table(str(src), str(template), ver, "2025a", validate_profiles=False)
    df0 = load_InitialCond_grid_df(next(template.rglob("RunControl.nml")))

    written = df_state_to_tables(df0, template, tmp_path / "Tprime")
    df1 = load_InitialCond_grid_df(next(written.parent.rglob("RunControl.nml")))

    grid = df0.index[0]
    diffs = [
        c
        for c in df0.columns
        if c in df1.columns
        and not _values_equal(str(df0.loc[grid, c]), str(df1.loc[grid, c]))
    ]
    assert not diffs, f"reload not equivalent: {len(diffs)} cols, e.g. {diffs[:6]}"


def test_edit_propagates_through_full_roundtrip(tmp_path):
    """An edit to df_state flows through the writer + reverse into the legacy table.

    Proves the round-trip is edit-propagating, not a snapshot: change the paved
    initial albedo and confirm the regenerated legacy ``SUEWS_NonVeg`` row for
    ``Code_Paved`` carries the new value.
    """
    ver = "2016a"
    src = _extract_legacy_tables(ver, tmp_path / "src")
    template = tmp_path / "T0"
    convert_table(str(src), str(template), ver, "2025a", validate_profiles=False)
    df = load_InitialCond_grid_df(next(template.rglob("RunControl.nml")))
    grid = df.index[0]
    df.loc[grid, ("alb", "(0,)")] = 0.42  # paved albedo

    rev = _reverse_via_writer(df, template, ver, src, tmp_path)

    ss_header, ss_row = R._read_table(rev / "SUEWS_SiteSelect.txt")
    paved_code = ss_row[0][ss_header.index("Code_Paved")]
    nv_header, nv_rows = R._read_table(rev / "SUEWS_NonVeg.txt")
    paved_row = next(r for r in nv_rows if r[0] == paved_code)
    assert float(paved_row[nv_header.index("AlbedoMax")]) == pytest.approx(0.42)
