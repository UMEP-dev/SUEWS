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

Fixtures are the canonical KCL parameter tables (no forcing) vendored under
``test/fixtures/legacy_tables/<ver>/`` (see that README for provenance), so the
test is self-contained under the standard CI checkout. ``2018b`` is the
narrowest delta on the chain (one captured delete) and ``2016a`` the widest
(173 adds, root layout).
"""
from pathlib import Path
import shutil

import pytest
import yaml

pytest.importorskip("supy")
from supy._load import load_InitialCond_grid_df  # noqa: PLC2701
from supy.data_model.core.config import SUEWSConfig
import supy.util.converter.table.reverse as R
from supy.util.converter.table.table import (
    clean_legacy_table,
    convert_table,
)
from supy.util.converter.table.table_writer import df_state_to_tables

# Nature axis (gh#1300): exercises the converter / pandas / pydantic surface,
# not compiled physics.
pytestmark = pytest.mark.api

# Vendored parameter tables (no forcing) -- see the fixture README.
# Self-contained so the test runs under the standard (shallow) CI checkout,
# rather than extracting from a git tag.
_FIXTURE_ROOT = Path(__file__).resolve().parents[1] / "fixtures" / "legacy_tables"

# Every version with vendored fixtures. The two anchors run in the normal CI
# tier; the rest carry the ``slow`` marker (full matrix runs in the ``all``
# tier and locally).
_ANCHOR_VERSIONS = ["2016a", "2018b"]
_ALL_VERSIONS = [
    "2016a",
    "2017a",
    "2017b",
    "2018a",
    "2018b",
    "2018c",
    "2019a",
    "2019b",
    "2020a",
]
_MATRIX = [
    ver if ver in _ANCHOR_VERSIONS else pytest.param(ver, marks=pytest.mark.slow)
    for ver in _ALL_VERSIONS
]


def _extract_legacy_tables(ver: str, dest: Path) -> Path:
    """Copy the vendored ``<ver>`` parameter-table fixture into ``dest``."""
    src = _FIXTURE_ROOT / ver
    if not (src / "RunControl.nml").exists():
        pytest.skip(f"legacy table fixture {ver} not vendored under {_FIXTURE_ROOT}")
    dest.mkdir(parents=True, exist_ok=True)
    for f in src.iterdir():
        if f.is_file():
            shutil.copyfile(f, dest / f.name)
    return dest


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


# Columns where the modern data model canonicalises the legacy ``-999``
# "unset / derive internally" sentinel to an explicit default on the YAML leg
# (e.g. ``faibldg: None -> 0.3`` in ``to_df_state``). Exempted only in the
# through-YAML round-trip; the table-only round-trip stays strict.
_SENTINEL_CANONICALISED_COLS = {"FAI_Bldgs", "FAI_DecTr", "FAI_EveTr"}


def _compare_table(
    path_expected: Path, path_actual: Path, sentinel_defaults_ok: bool = False
) -> list[str]:
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
                if (
                    sentinel_defaults_ok
                    and col in _SENTINEL_CANONICALISED_COLS
                    and _num(ve) == -999
                ):
                    continue
                issues.append(f"value Code {code} {col}: expected={ve} actual={va}")
    return issues


@pytest.mark.parametrize("ver", _MATRIX)
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


@pytest.mark.parametrize("ver", _MATRIX)
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
        issues = _compare_table(
            cleaned / tbl.name, regenerated, sentinel_defaults_ok=True
        )
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


# --------------------------------------------------------------------------- #
# Native modern -> legacy -> modern (the cross-era benchmark direction)
# --------------------------------------------------------------------------- #
# After ``modern -> legacy -> modern`` the only df_state variables allowed to
# differ are:
# - derived/transient initial state the model recomputes on start-up;
# - the veg albedo canonicalisation (the driver re-derives ``alb(veg)`` from
#   ``albXXX_id`` each step, so the pair is one logical value);
# - era-representability: parameters the target version cannot express.
_MLM_DERIVED = {"hdd_id", "tair_av", "tmax_id", "tmin_id", "tstep_prev"}
_MLM_COMMON = _MLM_DERIVED | {
    "alb",  # veg albedo canonicalisation
    "rslmethod",
    "rsllevel",  # RunControl flags introduced after 2018b
    "soilstore_surf",  # water-surface store: not in the era nml declaration
    "use_sw_direct_albedo",  # SPARTACUS (2024a+): no legacy representation
}
# Pre-2018a eras: single-column WD/WE families (TrafficRate, BuildEnergyUse,
# TCritic) and no per-purpose population/traffic profiles -- heterogeneous
# modern values collapse with a warning.
_MLM_PRE2018A = {
    "popprof_24hr",
    "traffprof_24hr",
    "baset_cooling",
    "baset_heating",
    "qf0_beu",
    "trafficrate",
}
_MLM_ALLOWED = {
    "2016a": _MLM_COMMON | _MLM_PRE2018A | {"pormin_dec"},
    "2017a": _MLM_COMMON | _MLM_PRE2018A,
    "2017b": _MLM_COMMON | _MLM_PRE2018A,
    "2018a": _MLM_COMMON,
    "2018b": _MLM_COMMON,
    "2018c": _MLM_COMMON,
    "2019a": _MLM_COMMON,
    "2019b": _MLM_COMMON,
    "2020a": _MLM_COMMON,
}

# Perturbations for the stress variant: values absent from any fixture,
# exercising every writer/carry path (direct WD/WE pairs, two-level profile
# codes with WD != WE forcing clone-on-write, OHM de-aliasing, parameter
# vectors, nml-backed initial state, cross-file moves and splits).
_MLM_PERTURB = {
    ("qf0_beu", "(0,)"): 0.7,
    ("qf0_beu", "(1,)"): 0.9,
    ("trafficrate", "(0,)"): 0.02,
    ("trafficrate", "(1,)"): 0.05,
    ("ahprof_24hr", "(3, 0)"): 0.31,
    ("ahprof_24hr", "(3, 1)"): 0.77,
    ("popprof_24hr", "(5, 0)"): 0.41,
    ("popprof_24hr", "(5, 1)"): 0.62,
    ("ohm_coef", "(0, 0, 0)"): 0.31,
    ("laipower", "(2, 1)"): 0.04,
    ("storedrainprm", "(1, 2)"): 0.6,
    ("soilstore_surf", "(3,)"): 120.0,
    ("baset_heating", "(0,)"): 13.0,
    ("baset_heating", "(1,)"): 15.0,
    ("z", "0"): 25.0,
    ("stabilitymethod", "0"): 1,
}


def _sample_df_state():
    from supy._env import trv_supy_module  # noqa: PLC0415, PLC2701

    cfg = SUEWSConfig(
        **yaml.safe_load(
            (trv_supy_module / "sample_data" / "sample_config.yml").read_text(
                encoding="utf-8"
            )
        )
    )
    return cfg.to_df_state()


def _df_state_diff_vars(df, df2):
    """Variables whose values differ between two df_state frames (first grid)."""
    grid = df.index[0]
    common = [c for c in df.columns if c in df2.columns]
    return {
        c[0]
        for c in common
        if not _values_equal(str(df.loc[grid, c]), str(df2.loc[grid, c]))
    }


def _mlm_chain(df, ver: str, tmp_path: Path):
    """``df_state -> <ver> legacy tables -> 2025a -> df_state``."""
    src = _extract_legacy_tables(ver, tmp_path / "src")
    template = tmp_path / "T0"
    convert_table(str(src), str(template), ver, "2025a", validate_profiles=False)
    rev = _reverse_via_writer(df, template, ver, src, tmp_path)

    back = tmp_path / "T2"
    convert_table(str(rev), str(back), ver, "2025a", validate_profiles=False)
    return load_InitialCond_grid_df(next(back.rglob("RunControl.nml")))


@pytest.mark.parametrize("ver", _MATRIX)
def test_modern_to_legacy_to_modern_roundtrip(ver, tmp_path):
    """A native modern config survives ``modern -> legacy -> modern``.

    Drives the writer + reverse converter from the shipped sample config (no
    legacy original), regenerates ``ver`` tables, forward-converts them back,
    and reloads. Everything the target version can represent must round-trip;
    the allowed residual is pinned per era (derived state, the veg-albedo
    canonicalisation, era-representability losses).
    """
    df = _sample_df_state()
    df2 = _mlm_chain(df, ver, tmp_path)

    unexpected = _df_state_diff_vars(df, df2) - _MLM_ALLOWED[ver]
    assert not unexpected, (
        f"{ver} modern->legacy->modern lost unexpected variables: "
        f"{sorted(unexpected)}"
    )


@pytest.mark.parametrize("ver", _MATRIX)
def test_modern_perturbed_to_legacy_roundtrip(ver, tmp_path):
    """Perturbed values survive ``modern -> legacy -> modern`` cell-for-cell.

    Stresses every writer/carry path with values present in no fixture
    (``_MLM_PERTURB``): WD/WE asymmetry forcing clone-on-write profile
    splits, distinct per-surface OHM coefficients, parameter vectors,
    nml-backed state, and the cross-file move/split carries. Each perturbed
    cell the era can represent must come back exactly.
    """
    df = _sample_df_state()
    grid = df.index[0]
    for key, value in _MLM_PERTURB.items():
        assert key in df.columns, f"perturbation key missing from df_state: {key}"
        df.loc[grid, key] = value

    df2 = _mlm_chain(df, ver, tmp_path)

    unexpected = _df_state_diff_vars(df, df2) - _MLM_ALLOWED[ver]
    assert not unexpected, (
        f"{ver} perturbed modern->legacy->modern lost unexpected variables: "
        f"{sorted(unexpected)}"
    )
    lost = [
        (key, value, df2.loc[grid, key])
        for key, value in _MLM_PERTURB.items()
        if key[0] not in _MLM_ALLOWED[ver]  # representability-lost vars excluded
        and key in df2.columns
        and not _values_equal(str(value), str(df2.loc[grid, key]))
    ]
    assert not lost, f"{ver} perturbations lost through the round-trip: {lost}"


def test_cross_version_chain_2016a_modern_2018b(tmp_path):
    """Arbitrary-direction chain: ``2016a tables -> modern -> 2018b tables -> modern``.

    Loads the 2016a fixture into the modern representation, regenerates a
    *different* era's tables (2018b) from it, forward-converts those back, and
    confirms the two modern states agree on everything 2018b can represent.
    """
    src_a = _extract_legacy_tables("2016a", tmp_path / "srcA")
    _, df = _forward_to_yaml("2016a", src_a, tmp_path)

    src_b = _extract_legacy_tables("2018b", tmp_path / "srcB")
    template_b = tmp_path / "TB"
    convert_table(str(src_b), str(template_b), "2018b", "2025a", validate_profiles=False)
    tmp_b = tmp_path / "legB"
    tmp_b.mkdir()
    rev_b = _reverse_via_writer(df, template_b, "2018b", src_b, tmp_b)

    back = tmp_path / "T2B"
    convert_table(str(rev_b), str(back), "2018b", "2025a", validate_profiles=False)
    df2 = load_InitialCond_grid_df(next(back.rglob("RunControl.nml")))

    unexpected = _df_state_diff_vars(df, df2) - _MLM_ALLOWED["2018b"]
    assert not unexpected, (
        "2016a->modern->2018b->modern chain lost unexpected variables: "
        f"{sorted(unexpected)}"
    )


def test_yaml_upgrade_chain_old_schema_to_legacy(tmp_path):
    """Full arbitrary chain: old-schema YAML -> current YAML -> legacy -> modern.

    Starts from the oldest vendored release config (pre-``schema_version``),
    upgrades it through the schema-migration handlers, regenerates 2016a
    tables from it, forward-converts them back, and confirms nothing the era
    can represent is lost. This is the user-facing journey of running a
    years-old YAML against a historical table format.
    """
    from supy.util.converter.yaml_upgrade import upgrade_yaml  # noqa: PLC0415

    release_configs = _FIXTURE_ROOT.parent / "release_configs"
    old_yaml = release_configs / "2025.10.15.yml"
    if not old_yaml.exists():
        pytest.skip("release config fixture 2025.10.15.yml not vendored")

    upgraded = tmp_path / "current.yml"
    upgrade_yaml(old_yaml, upgraded, from_ver="2025.10.15")
    cfg = SUEWSConfig(**yaml.safe_load(upgraded.read_text(encoding="utf-8")))
    df = cfg.to_df_state()

    df2 = _mlm_chain(df, "2016a", tmp_path)

    unexpected = _df_state_diff_vars(df, df2) - _MLM_ALLOWED["2016a"]
    assert not unexpected, (
        "old-schema YAML -> current -> 2016a -> modern chain lost unexpected "
        f"variables: {sorted(unexpected)}"
    )
