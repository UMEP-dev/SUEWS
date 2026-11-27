"""Native resolved-YAML water-distribution guards across CLI, buffer, and checkpoint routes (#2086, #2002, #2349)."""

from __future__ import annotations

from copy import deepcopy
from importlib import import_module
from importlib.resources import files
import json
import math
from pathlib import Path
import struct
import subprocess

import numpy as np
from pyarrow import ipc
import pytest
import yaml

from supy.data_model import SUEWSConfig
from supy.data_model.core.hydro import WATER_DISTRIBUTION_REQUIRED, WaterDistribution
from supy.data_model.core.type import SurfaceType

_run_rust = import_module("supy._run_rust")
_YAML_LOADER = getattr(yaml, "CSafeLoader", yaml.SafeLoader)
_YAML_DUMPER = getattr(yaml, "CSafeDumper", yaml.SafeDumper)

pytestmark = [
    pytest.mark.physics,
    pytest.mark.contract("C-STRICT", "C-BIND", "C-ERROR", "C-PARITY", "C-RESTART"),
]


def _dump_yaml(doc: dict) -> str:
    return yaml.dump(doc, Dumper=_YAML_DUMPER, sort_keys=False)


def _load_yaml(text: str) -> dict:
    return yaml.load(text, Loader=_YAML_LOADER)


@pytest.fixture(scope="module")
def _waterdist_base(sample_config_loaded, sample_forcing_parsed):
    """Cache resolved sample config, forcing lines, and 1-step / 24-step forcing blocks."""
    resolved_doc = _load_yaml(_run_rust.config_to_engine_yaml(sample_config_loaded))
    # Establish positive baseline that the unmutated resolved dict passes SUEWSConfig.
    SUEWSConfig.from_dict(deepcopy(resolved_doc))
    forcing_entry = resolved_doc["model"]["control"]["forcing"]["file"]
    forcing_name = (
        forcing_entry["value"] if isinstance(forcing_entry, dict) else forcing_entry
    )
    assert isinstance(forcing_name, str) and forcing_name
    forcing_raw = (files("supy") / "sample_data" / forcing_name).read_text(
        encoding="utf-8"
    )
    forcing_lines = [line for line in forcing_raw.splitlines() if line.strip()]

    _, df_forcing_24 = sample_forcing_parsed(24)
    forcing_block_24 = np.ascontiguousarray(
        _run_rust._prepare_forcing_block(df_forcing_24), dtype=np.float64
    )
    forcing_block_1 = np.ascontiguousarray(forcing_block_24[:1], dtype=np.float64)
    return {
        "resolved_doc": resolved_doc,
        "forcing_name": forcing_name,
        "forcing_lines": forcing_lines,
        "forcing_block_24": forcing_block_24,
        "forcing_block_1": forcing_block_1,
    }


def _sample_resolved_doc(base_cache: dict, run_dir: Path | None = None) -> dict:
    """Return a defensive copy of the resolved native config dict."""
    doc = deepcopy(base_cache["resolved_doc"])
    if run_dir is not None:
        doc["model"]["control"]["output"]["dir"] = str(run_dir)
    return doc


def _write_wet_day_cli_inputs(
    base_cache: dict, run_dir: Path, doc: dict, forcing_hours: int = 25
) -> Path:
    """Write resolved YAML and wet-day hourly forcing prefix (2012-01-01) to ``run_dir``."""
    forcing_name = base_cache["forcing_name"]
    lines = base_cache["forcing_lines"]
    prefix = "\n".join(lines[: forcing_hours + 1]) + "\n"
    (run_dir / forcing_name).write_text(prefix, encoding="utf-8")
    doc = deepcopy(doc)
    doc["model"]["control"].pop("start_time", None)
    doc["model"]["control"].pop("end_time", None)
    config_path = run_dir / "resolved_config.yml"
    config_path.write_text(_dump_yaml(doc), encoding="utf-8")
    return config_path


@pytest.mark.parametrize(
    ("case", "mutator", "expected_substrings", "public_needles"),
    [
        (
            "paved_delete_to_runoff",
            lambda lc: lc["paved"]["waterdist"].pop("to_runoff"),
            ("sites[gridiv=7]", "paved.waterdist.to_runoff"),
            ("paved", "to_runoff"),
        ),
        (
            "paved_null_to_grass",
            lambda lc: lc["paved"]["waterdist"].__setitem__(
                "to_grass", {"value": None}
            ),
            ("sites[gridiv=7]", "paved.waterdist.to_grass"),
            ("paved", "to_grass"),
        ),
        (
            "paved_bad_sum",
            lambda lc: lc["paved"]["waterdist"].__setitem__("to_grass", {"value": 0.5}),
            ("sites[gridiv=7]", "paved.waterdist", "sum must be 1.0"),
            ("paved", "sum must be 1.0"),
        ),
    ],
)
@pytest.mark.e2e
def test_waterdist_br02_failure_witnesses(
    tmp_path: Path,
    suews_engine_bin: Path,
    _waterdist_base: dict,
    case: str,
    mutator,
    expected_substrings: tuple[str, ...],
    public_needles: tuple[str, ...],
) -> None:
    """BR-02 witnesses for waterdist (missing, null, malformed) across CLI and Python."""
    run_dir = tmp_path / case
    run_dir.mkdir()
    doc = _sample_resolved_doc(_waterdist_base, run_dir)
    doc["sites"][0]["gridiv"] = 7
    mutator(doc["sites"][0]["properties"]["land_cover"])
    config_path = _write_wet_day_cli_inputs(
        _waterdist_base, run_dir, doc, forcing_hours=2
    )

    # 1. Standalone suews-engine CLI witness
    result = subprocess.run(
        [str(suews_engine_bin), "run", str(config_path)],
        capture_output=True,
        text=True,
        timeout=60,
        check=False,
    )
    assert result.returncode != 0, (
        f"expected non-zero exit for {case}; stdout={result.stdout} stderr={result.stderr}"
    )
    combined = f"{result.stdout}\n{result.stderr}"
    for needle in expected_substrings:
        assert needle in combined, f"{case}: expected {needle!r} in {combined!r}"
    assert not (run_dir / "suews_output.arrow").exists()

    # 2. Public Python SUEWSConfig entry point witness
    with pytest.raises((ValueError, TypeError)) as cfg_exc:
        SUEWSConfig.from_dict(doc)
    msg = str(cfg_exc.value)
    for needle in public_needles:
        assert needle in msg, f"{case}: expected {needle!r} in {msg!r}"


@pytest.mark.e2e
def test_cli_accepts_valid_wet_day_and_inactive_omission(
    tmp_path: Path, suews_engine_bin: Path, _waterdist_base: dict
) -> None:
    """Valid resolved wet-day config and inactive-surface omission produce identical runoff."""
    runoff_by_case = {}
    for case, omit_inactive in [("control", False), ("inactive_omitted", True)]:
        run_dir = tmp_path / case
        run_dir.mkdir()
        doc = _sample_resolved_doc(_waterdist_base, run_dir)
        if omit_inactive:
            lc = doc["sites"][0]["properties"]["land_cover"]
            assert lc["evetr"]["sfr"]["value"] == pytest.approx(0.0)
            assert lc["bsoil"]["sfr"]["value"] == pytest.approx(0.0)
            lc["evetr"].pop("waterdist", None)
            lc["bsoil"]["waterdist"] = {}
        config_path = _write_wet_day_cli_inputs(_waterdist_base, run_dir, doc)
        result = subprocess.run(
            [str(suews_engine_bin), "run", str(config_path)],
            capture_output=True,
            text=True,
            timeout=60,
            check=False,
        )
        assert result.returncode == 0, f"{case} failed: {result.stderr}"
        with ipc.open_file(str(run_dir / "suews_output.arrow")) as reader:
            table = reader.read_all().select(["Rain", "RO"])
            rain = table.column("Rain").to_numpy(zero_copy_only=False).copy()
            ro = table.column("RO").to_numpy(zero_copy_only=False).copy()
        assert float(rain.sum()) > 10.0
        assert float(ro.sum()) > 1.0
        runoff_by_case[case] = ro

    np.testing.assert_array_equal(
        runoff_by_case["inactive_omitted"], runoff_by_case["control"]
    )


def test_buffer_multisite_and_checkpoint_routes_reject_corrupted_waterdist(
    _waterdist_base: dict,
) -> None:
    """Direct Rust buffer, multi-site, and checkpoint routes enforce waterdist guards."""
    rust = _run_rust._check_rust_available()
    forcing_block = _waterdist_base["forcing_block_24"]
    len_sim = len(forcing_block)

    valid_doc = _sample_resolved_doc(_waterdist_base)
    valid_yaml = _dump_yaml(valid_doc)

    # Fresh valid buffer run yields a valid checkpoint JSON state.
    out_data = np.zeros(
        (len_sim, _run_rust.OUTPUT_DATA_COLS), dtype=np.float64, order="F"
    )
    out_dt = np.zeros(
        (len_sim, _run_rust.OUTPUT_TIME_COLS), dtype=np.float64, order="F"
    )
    state_json, n_steps, _warnings = rust.run_suews_into(
        valid_yaml, forcing_block, len_sim, out_data, out_dt, None
    )
    assert n_steps == len_sim

    # Corrupted site with non-1 gridiv=7 fails on fresh buffer, checkpoint, and multi-site
    # across missing, null, and malformed waterdist inputs.
    cases = [
        (
            "missing",
            lambda wd: wd.pop("to_runoff"),
            r"sites\[gridiv=7\]\.properties\.land_cover\.paved\.waterdist\.to_runoff",
        ),
        (
            "null",
            lambda wd: wd.__setitem__("to_grass", {"value": None}),
            r"sites\[gridiv=7\]\.properties\.land_cover\.paved\.waterdist\.to_grass",
        ),
        (
            "malformed",
            lambda wd: wd.__setitem__("to_paved", {"value": 0.5}),
            r"sites\[gridiv=7\]\.properties\.land_cover\.paved\.waterdist\.to_paved",
        ),
    ]
    for label, mutator, pattern in cases:
        corrupt_doc = deepcopy(valid_doc)
        corrupt_doc["sites"][0]["gridiv"] = 7
        mutator(
            corrupt_doc["sites"][0]["properties"]["land_cover"]["paved"]["waterdist"]
        )
        corrupt_yaml = _dump_yaml(corrupt_doc)

        for state_arg in (None, state_json):
            with pytest.raises(RuntimeError, match=pattern):
                rust.run_suews_into(
                    corrupt_yaml, forcing_block, len_sim, out_data, out_dt, state_arg
                )

        if label == "missing":
            with pytest.raises(RuntimeError, match=pattern):
                rust.run_suews_multi(
                    [valid_yaml, corrupt_yaml],
                    forcing_block.ravel(order="C").tolist(),
                    len_sim,
                    2,
                )


_PAVED_KEYS = (
    "to_bldgs",
    "to_dectr",
    "to_evetr",
    "to_grass",
    "to_bsoil",
    "to_water",
    "to_runoff",
)
_BSOIL_KEYS = (
    "to_paved",
    "to_bldgs",
    "to_dectr",
    "to_evetr",
    "to_grass",
    "to_water",
    "to_soilstore",
)
# Boundary vectors where naive sequential float addition disagrees with math.fsum.
_SUM_V1 = (0.034, 0.011, 0.015, 0.050, 0.040, 0.021, 0.82899)
_SUM_V2 = (0.030, 0.041, 0.040, 0.044, 0.011, 0.034, 0.79999)
_SUM_PERMUTATIONS = (
    (0, 1, 2, 3, 4, 5, 6),
    (6, 5, 4, 3, 2, 1, 0),
    (3, 4, 5, 6, 0, 1, 2),
)
_SUM_BOUNDARY_CASES = (
    ("lower_outside_v2", _SUM_V2, False, True),
    ("lower_at_v1", _SUM_V1, True, True),
    (
        "lower_inside",
        (0.034, 0.011, 0.015, 0.050, 0.040, 0.021, 0.828991),
        True,
        True,
    ),
    (
        "upper_at_1p1e5",
        (0.034, 0.011, 0.015, 0.050, 0.040, 0.021, 0.82901),
        True,
        True,
    ),
    (
        "upper_between_active_and_inactive",
        (0.034, 0.011, 0.015, 0.050, 0.040, 0.021, 0.82901000005),
        True,
        False,
    ),
    (
        "upper_outside",
        (0.034, 0.011, 0.015, 0.050, 0.040, 0.021, 0.8290100002),
        False,
        False,
    ),
)


def _python_waterdist_valid(
    surface_type: SurfaceType,
    keys: tuple[str, ...],
    vals: tuple[float, ...],
    *,
    require_complete: bool,
) -> bool:
    """Return whether ``WaterDistribution.validate_distribution`` accepts ``vals``."""
    wd = WaterDistribution(**dict(zip(keys, vals, strict=True)))
    try:
        wd.validate_distribution(surface_type, require_complete=require_complete)
    except ValueError:
        return False
    return True


def _rust_waterdist_valid(
    base_doc: dict,
    forcing_block: np.ndarray,
    surface: str,
    keys: tuple[str, ...],
    vals: tuple[float, ...],
) -> bool:
    """Return whether native ``run_suews_into`` accepts ``vals`` on ``surface``."""
    rust = _run_rust._check_rust_available()
    doc = deepcopy(base_doc)
    doc["sites"][0]["gridiv"] = 73
    doc["sites"][0]["properties"]["land_cover"][surface]["waterdist"] = {
        k: {"value": v} for k, v in zip(keys, vals, strict=True)
    }
    yaml_str = _dump_yaml(doc)
    out_data = np.zeros((1, _run_rust.OUTPUT_DATA_COLS), dtype=np.float64, order="F")
    out_dt = np.zeros((1, _run_rust.OUTPUT_TIME_COLS), dtype=np.float64, order="F")
    try:
        _, n_steps, _ = rust.run_suews_into(
            yaml_str, forcing_block, 1, out_data, out_dt, None
        )
        assert n_steps == 1
        assert np.isfinite(out_dt).all()
    except RuntimeError as exc:
        assert f"sites[gridiv=73].properties.land_cover.{surface}.waterdist" in str(exc)
        assert "sum must" in str(exc)
        return False
    return True


def test_python_and_native_waterdist_sum_boundary_parity(_waterdist_base: dict) -> None:
    """Python ``WaterDistribution`` and native Rust agree on acceptance at sum boundaries."""
    forcing_block = _waterdist_base["forcing_block_1"]
    base_doc = _sample_resolved_doc(_waterdist_base)

    # These particular weight reassignments preserve acceptance; they are different inputs.
    for label, vec, expected in (("v1", _SUM_V1, True), ("v2", _SUM_V2, False)):
        for perm in _SUM_PERMUTATIONS:
            vals = tuple(vec[i] for i in perm)
            assert (
                _python_waterdist_valid(
                    SurfaceType.PAVED, _PAVED_KEYS, vals, require_complete=True
                )
                is expected
            ), f"Python mismatch for {label} perm={perm}"
            assert (
                _rust_waterdist_valid(
                    base_doc, forcing_block, "paved", _PAVED_KEYS, vals
                )
                is expected
            ), f"Rust mismatch for {label} perm={perm}"

    # Lower (~0.99999) and upper (~1.00001) boundaries across active and inactive surfaces.
    for label, vals, active_ok, inactive_ok in _SUM_BOUNDARY_CASES:
        assert (
            _python_waterdist_valid(
                SurfaceType.PAVED, _PAVED_KEYS, vals, require_complete=True
            )
            is active_ok
        ), f"Python active mismatch: {label}"
        assert (
            _rust_waterdist_valid(base_doc, forcing_block, "paved", _PAVED_KEYS, vals)
            is active_ok
        ), f"Rust active mismatch: {label}"
        assert (
            _python_waterdist_valid(
                SurfaceType.BSOIL, _BSOIL_KEYS, vals, require_complete=False
            )
            is inactive_ok
        ), f"Python inactive mismatch: {label}"
        assert (
            _rust_waterdist_valid(base_doc, forcing_block, "bsoil", _BSOIL_KEYS, vals)
            is inactive_ok
        ), f"Rust inactive mismatch: {label}"


@pytest.mark.e2e
def test_cli_waterdist_sum_v1_v2_parity(
    tmp_path: Path, suews_engine_bin: Path, _waterdist_base: dict
) -> None:
    """Standalone ``suews-engine`` accepts v1 and rejects v2 identically to public Python ``sum``."""
    for case, vals, expect_zero in (
        ("cli_v1_accept", _SUM_V1, True),
        ("cli_v2_reject", _SUM_V2, False),
    ):
        run_dir = tmp_path / case
        run_dir.mkdir()
        doc = _sample_resolved_doc(_waterdist_base, run_dir)
        doc["sites"][0]["properties"]["land_cover"]["paved"]["waterdist"] = {
            k: {"value": v} for k, v in zip(_PAVED_KEYS, vals, strict=True)
        }
        config_path = _write_wet_day_cli_inputs(
            _waterdist_base, run_dir, doc, forcing_hours=2
        )
        result = subprocess.run(
            [str(suews_engine_bin), "run", str(config_path)],
            capture_output=True,
            text=True,
            timeout=60,
            check=False,
        )
        assert (result.returncode == 0) is expect_zero, (
            f"{case}: rc={result.returncode} stdout={result.stdout} stderr={result.stderr}"
        )


# Public-file witnesses: a compensated sum can differ from the exact sum by
# one ULP at the original tolerance. Weight order below is destination order.
_SOURCE_ROUNDING_CASES = (
    (0.5, 0.49998999999999993, 2**-54, 2**-111, 0.0, 0.0, 0.0),
    (0.5, 0.500010000100001, 2**-53, 2**-110, 0.0, 0.0, 0.0),
    (0.5, 0.5000100000000001, 2**-53, 2**-110, 0.0, 0.0, 0.0),
)


@pytest.mark.parametrize("surface", tuple(WATER_DISTRIBUTION_REQUIRED))
def test_native_preserves_public_python_rounding(_waterdist_base: dict, surface):
    """All surfaces use public sum; reverse key order preserves the same input."""
    forcing = _waterdist_base["forcing_block_1"]
    keys = WATER_DISTRIBUTION_REQUIRED[surface]
    kind = SurfaceType(surface)
    for active in (True, False):
        base = _sample_resolved_doc(_waterdist_base)
        lc = base["sites"][0]["properties"]["land_cover"]
        old_fraction = lc[surface]["sfr"]["value"]
        fraction = 0.01 if active else 0.0
        lc[surface]["sfr"]["value"] = fraction
        lc["water"]["sfr"]["value"] += old_fraction - fraction
        for values in _SOURCE_ROUNDING_CASES:
            total = sum(values)
            expected = (
                math.isclose(total, 1.0, rel_tol=1e-5)
                if active
                else total <= 1.0 + 1e-5
            )
            assert (
                _python_waterdist_valid(kind, keys, values, require_complete=active)
                is expected
            )
            assert (
                _rust_waterdist_valid(base, forcing, surface, keys, values) is expected
            )
            # Reverse pairs together, without reassigning destination weights.
            assert (
                _rust_waterdist_valid(
                    base,
                    forcing,
                    surface,
                    tuple(reversed(keys)),
                    tuple(reversed(values)),
                )
                is expected
            )


def test_water_fraction_oracle_matches_running_python():
    """The recorded native oracle must match supported Python's public sum."""
    oracle = Path(__file__).resolve().parents[2] / (
        "src/suews_bridge/tests/fixtures/water_fraction_sum.json"
    )
    for values, expected_bits in json.loads(oracle.read_text(encoding="utf-8")):
        observed = struct.unpack(">Q", struct.pack(">d", float(sum(values))))[0]
        assert observed == expected_bits, values
