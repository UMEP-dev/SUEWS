"""End-to-end engine and data-model guards for BioCO2, OHM/AnOHM, and LAI (#2002)."""

from importlib import import_module
from importlib.resources import as_file, files
import os
from pathlib import Path
import subprocess

import pytest
import yaml

from supy.data_model import SUEWSConfig
from supy.data_model.core.site import (
    DectrProperties,
    EvetrProperties,
    GrassProperties,
    LAIParams,
)
from supy.data_model.core.surface import (
    BldgsProperties,
    BsoilProperties,
    PavedProperties,
    WaterProperties,
)

_run_rust = import_module("supy._run_rust")

_SURFACE_MODELS = {
    "paved": PavedProperties,
    "bldgs": BldgsProperties,
    "evetr": EvetrProperties,
    "dectr": DectrProperties,
    "grass": GrassProperties,
    "bsoil": BsoilProperties,
    "water": WaterProperties,
}
_OHM_DEFAULT_KEYS = (
    "ch_anohm",
    "rho_cp_anohm",
    "k_anohm",
    "ohm_threshold_summer_winter",
    "ohm_threshold_wet_dry",
)
_BIOCO2_DEFAULT_KEYS = ("beta_enh_bioco2", "alpha_enh_bioco2", "min_res_bioco2")

pytestmark = [
    pytest.mark.api,
    pytest.mark.core,
    pytest.mark.contract("C-STRICT", "C-EMIT", "C-NATIVE-ERROR"),
]


def _locate_suews_engine() -> Path:
    """Return the bundled or locally built ``suews-engine`` binary."""
    binary_name = "suews-engine.exe" if os.name == "nt" else "suews-engine"
    bundled = files("supy").joinpath("bin").joinpath(binary_name)
    with as_file(bundled) as bundled_path:
        if bundled_path.exists():
            return Path(bundled_path)

    repo_root = Path(__file__).resolve().parents[2]
    dev_binary = repo_root / "src" / "suews_bridge" / "target" / "release" / binary_name
    if dev_binary.exists():
        return dev_binary
    pytest.skip("Rust CLI binary not found; build with: make bridge")


def _unwrap(value):
    return value["value"] if isinstance(value, dict) else value


def _prepare_resolved_run(
    run_dir: Path,
    *,
    emissions: int = 2,
    storage_heat: int = 1,
    lai_method: int | None = None,
    n_forcing_rows: int = 2,
    forcing_lai: float | None = None,
    forcing_qs: float | None = None,
    mutator=None,
) -> Path:
    """Write a resolved YAML config and forcing slice into ``run_dir``."""
    sample_dir = files("supy").joinpath("sample_data")
    with sample_dir.joinpath("sample_config.yml").open(encoding="utf-8") as handle:
        cfg = yaml.safe_load(handle)

    control = cfg["model"]["control"]
    control["output"]["dir"] = str(run_dir)
    forcing_entry = control.get("forcing", {}).get("file")
    forcing_name = _unwrap(forcing_entry)
    assert isinstance(forcing_name, str) and forcing_name

    cfg["model"]["physics"]["emissions"] = {"value": emissions}
    cfg["model"]["physics"]["storage_heat"] = {"value": storage_heat}
    if lai_method is not None:
        cfg["model"]["physics"]["leaf_area_index"] = (
            "observed" if lai_method == 0 else "modelled"
        )
    resolved_doc = yaml.safe_load(
        _run_rust.config_to_engine_yaml(SUEWSConfig.from_dict(cfg))
    )
    if mutator is not None:
        mutator(resolved_doc)

    forcing_raw = sample_dir.joinpath(forcing_name).read_text(encoding="utf-8")
    forcing_lines = [line for line in forcing_raw.splitlines() if line.strip()]
    header_cols = forcing_lines[0].split()
    lai_idx = header_cols.index("lai")
    qs_idx = header_cols.index("qs")
    out_lines = [forcing_lines[0]]
    for line in forcing_lines[1 : 1 + n_forcing_rows]:
        parts = line.split()
        if forcing_lai is not None:
            parts[lai_idx] = f"{forcing_lai:.3f}"
        if forcing_qs is not None:
            parts[qs_idx] = f"{forcing_qs:.3f}"
        out_lines.append(" ".join(parts))
    (run_dir / forcing_name).write_text("\n".join(out_lines) + "\n", encoding="utf-8")

    config_path = run_dir / "resolved_config.yml"
    config_path.write_text(
        yaml.safe_dump(resolved_doc, sort_keys=False), encoding="utf-8"
    )
    return config_path


def test_data_model_emits_required_bioco2_ohm_lai_defaults(tmp_path: Path):
    """Data model emits authoritative OHM, LAI, and BioCO2 defaults when omitted (#2001)."""
    sample_dir = files("supy").joinpath("sample_data")
    sample_raw = yaml.safe_load(
        sample_dir.joinpath("sample_config.yml").read_text(encoding="utf-8")
    )
    sample_raw["model"]["control"]["output"]["dir"] = str(tmp_path)
    sample_raw["model"]["physics"]["storage_heat"] = {"value": 3}
    sample_raw["model"]["physics"]["emissions"] = {"value": 32}

    # Strip all defaulted OHM, AnOHM, BioCO2, and LAI keys from the source YAML
    # before constructing SUEWSConfig so this exercises actual default emission.
    src_lc = sample_raw["sites"][0]["properties"]["land_cover"]
    for surface in _SURFACE_MODELS:
        for key in _OHM_DEFAULT_KEYS:
            src_lc[surface].pop(key, None)
    for surface in ("evetr", "dectr", "grass"):
        for key in _BIOCO2_DEFAULT_KEYS:
            src_lc[surface].pop(key, None)
        src_lc[surface]["lai"].pop("lai_min", None)
        src_lc[surface]["lai"].pop("lai_type", None)

    resolved_text = _run_rust.config_to_engine_yaml(SUEWSConfig.from_dict(sample_raw))
    land_cover = yaml.safe_load(resolved_text)["sites"][0]["properties"]["land_cover"]

    for surface, model_cls in _SURFACE_MODELS.items():
        defaults = model_cls()
        for key in _OHM_DEFAULT_KEYS:
            assert key in land_cover[surface], f"{surface}.{key} missing from YAML"
            assert _unwrap(land_cover[surface][key]) == pytest.approx(
                _unwrap(getattr(defaults, key))
            )

    lai_defaults = LAIParams()
    for surface in ("evetr", "dectr", "grass"):
        defaults = _SURFACE_MODELS[surface]()
        for key in _BIOCO2_DEFAULT_KEYS:
            assert key in land_cover[surface], f"{surface}.{key} missing from YAML"
            assert _unwrap(land_cover[surface][key]) == pytest.approx(
                _unwrap(getattr(defaults, key))
            )
        lai = land_cover[surface]["lai"]
        assert _unwrap(lai["lai_min"]) == pytest.approx(_unwrap(lai_defaults.lai_min))
        assert type(_unwrap(lai["lai_type"])) is int
        assert _unwrap(lai["lai_type"]) == _unwrap(lai_defaults.lai_type)

    # Confirm the default-emitted resolved YAML loads and runs cleanly in
    # suews-engine under active AnOHM (storage_heat=3) and general BioCO2 (32).
    forcing_name = _unwrap(sample_raw["model"]["control"]["forcing"]["file"])
    forcing_lines = [
        line
        for line in sample_dir.joinpath(forcing_name)
        .read_text(encoding="utf-8")
        .splitlines()
        if line.strip()
    ]
    (tmp_path / forcing_name).write_text(
        "\n".join(forcing_lines[:3]) + "\n", encoding="utf-8"
    )
    cfg_path = tmp_path / "default_emitted.yml"
    cfg_path.write_text(resolved_text, encoding="utf-8")
    result = subprocess.run(
        [str(_locate_suews_engine()), "run", str(cfg_path)],
        capture_output=True,
        text=True,
        timeout=60,
        check=False,
    )
    assert result.returncode == 0, (
        f"default-emitted config failed ({result.returncode}): {result.stderr}"
    )
    assert (tmp_path / "suews_output.arrow").is_file()


@pytest.mark.parametrize(
    ("emissions", "storage_heat", "lai_method", "path_to_drop", "expected_path"),
    [
        (
            2,
            1,
            None,
            ("paved", "ohm_coef", "summer_dry", "a1"),
            "properties.land_cover.paved.ohm_coef.summer_dry.a1",
        ),
        (
            2,
            1,
            None,
            ("evetr", "ohm_coef", "summer_dry", "a1"),
            "properties.land_cover.evetr.ohm_coef.summer_dry.a1",
        ),
        (
            2,
            1,
            None,
            ("grass", "ohm_threshold_wet_dry"),
            "properties.land_cover.grass.ohm_threshold_wet_dry",
        ),
        (
            2,
            6,
            None,
            ("evetr", "ohm_threshold_summer_winter"),
            "properties.land_cover.evetr.ohm_threshold_summer_winter",
        ),
        (
            2,
            3,
            None,
            ("evetr", "ch_anohm"),
            "properties.land_cover.evetr.ch_anohm",
        ),
        (
            2,
            1,
            0,
            ("evetr", "lai", "base_temperature"),
            "properties.land_cover.evetr.lai.base_temperature",
        ),
        (
            2,
            1,
            None,
            ("grass", "lai", "lai_power", "growth_lai"),
            "properties.land_cover.grass.lai.lai_power.growth_lai",
        ),
        (
            2,
            1,
            None,
            ("evetr", "lai", "lai_type"),
            "properties.land_cover.evetr.lai.lai_type",
        ),
        (
            12,
            1,
            None,
            ("grass", "beta_bio_co2"),
            "properties.land_cover.grass.beta_bio_co2",
        ),
        (
            22,
            1,
            None,
            ("grass", "theta_bio_co2"),
            "properties.land_cover.grass.theta_bio_co2",
        ),
        (
            32,
            1,
            None,
            ("evetr", "beta_enh_bioco2"),
            "properties.land_cover.evetr.beta_enh_bioco2",
        ),
        (
            42,
            1,
            None,
            ("evetr", "beta_bio_co2"),
            "properties.land_cover.evetr.beta_bio_co2",
        ),
        (
            12,
            1,
            None,
            ("grass", "resp_a"),
            "properties.land_cover.grass.resp_a",
        ),
    ],
)
def test_engine_rejects_missing_active_ohm_lai_bioco2_fields(
    tmp_path: Path,
    emissions: int,
    storage_heat: int,
    lai_method: int | None,
    path_to_drop: tuple[str, ...],
    expected_path: str,
):
    """Standalone ``suews-engine`` rejects missing active OHM, LAI, or BioCO2 keys."""
    engine_bin = _locate_suews_engine()

    def mutate(doc: dict):
        node = doc["sites"][0]["properties"]["land_cover"]
        for key in path_to_drop[:-1]:
            node = node[key]
        node.pop(path_to_drop[-1], None)

    config_path = _prepare_resolved_run(
        tmp_path,
        emissions=emissions,
        storage_heat=storage_heat,
        lai_method=lai_method,
        mutator=mutate,
    )
    result = subprocess.run(
        [str(engine_bin), "run", str(config_path)],
        capture_output=True,
        text=True,
        timeout=60,
        check=False,
    )
    assert result.returncode != 0, (
        f"missing {expected_path} must not exit 0 (stdout={result.stdout})"
    )
    assert expected_path in result.stderr, result.stderr
    assert not (tmp_path / "suews_output.arrow").exists()


@pytest.mark.parametrize("resp_key", ["resp_a", "resp_b", "min_res_bioco2"])
def test_engine_rejects_missing_respiration_at_fortran_f32_boundary(
    tmp_path: Path,
    resp_key: str,
):
    """Exact ``veg_frac_sum=0.01, sfr=0.005`` enters Fortran respiration and must fail if omitted."""
    engine_bin = _locate_suews_engine()

    def mutate(doc: dict):
        lc = doc["sites"][0]["properties"]["land_cover"]
        lc["paved"]["sfr"] = {"value": 0.99}
        lc["bldgs"]["sfr"] = {"value": 0.0}
        lc["evetr"]["sfr"] = {"value": 0.0}
        lc["dectr"]["sfr"] = {"value": 0.005}
        lc["grass"]["sfr"] = {"value": 0.005}
        lc["bsoil"]["sfr"] = {"value": 0.0}
        lc["water"]["sfr"] = {"value": 0.0}
        lc["grass"].pop(resp_key, None)

    config_path = _prepare_resolved_run(
        tmp_path,
        emissions=12,
        storage_heat=1,
        mutator=mutate,
    )
    result = subprocess.run(
        [str(engine_bin), "run", str(config_path)],
        capture_output=True,
        text=True,
        timeout=60,
        check=False,
    )
    assert result.returncode != 0, (
        f"missing {resp_key} at 0.005/0.01 boundary must not exit 0"
    )
    assert f"properties.land_cover.grass.{resp_key}" in result.stderr, result.stderr
    assert not (tmp_path / "suews_output.arrow").exists()


def test_engine_accepts_omitted_inactive_fields_across_daily_update(tmp_path: Path):
    """Standalone ``suews-engine`` runs cleanly across a 2-day daily update with inactive fields omitted."""
    engine_bin = _locate_suews_engine()

    def mutate(doc: dict):
        lc = doc["sites"][0]["properties"]["land_cover"]
        # EmissionsMethod=2: CO2_biogen is never called.
        for surface in ("evetr", "dectr", "grass"):
            for key in (
                "beta_bio_co2",
                "beta_enh_bioco2",
                "alpha_bio_co2",
                "alpha_enh_bioco2",
                "resp_a",
                "resp_b",
                "theta_bio_co2",
                "min_res_bioco2",
            ):
                lc[surface].pop(key, None)
        # StorageHeatMethod=1: AnOHM fields are unused, and paved/bldgs/water
        # skip ohm_threshold_wet_dry in OHM_coef_surface.
        for surface in ("paved", "bldgs", "evetr", "dectr", "grass", "bsoil", "water"):
            for key in ("ch_anohm", "rho_cp_anohm", "k_anohm"):
                lc[surface].pop(key, None)
        for surface in ("paved", "bldgs", "water"):
            lc[surface].pop("ohm_threshold_wet_dry", None)
        # laimethod=0: calculate_lai is skipped, so lai_power and lai_type are unused.
        for surface in ("evetr", "dectr", "grass"):
            lc[surface]["lai"].pop("lai_power", None)
            lc[surface]["lai"].pop("lai_type", None)

    config_path = _prepare_resolved_run(
        tmp_path,
        emissions=2,
        storage_heat=1,
        lai_method=0,
        n_forcing_rows=48,
        forcing_lai=1.5,
        mutator=mutate,
    )
    result = subprocess.run(
        [str(engine_bin), "run", str(config_path)],
        capture_output=True,
        text=True,
        timeout=60,
        check=False,
    )
    assert result.returncode == 0, (
        f"omitted inactive fields failed ({result.returncode}): {result.stderr}"
    )
    assert (tmp_path / "suews_output.arrow").is_file()


def test_engine_accepts_omitted_ohm_under_observed_qs(tmp_path: Path):
    """Standalone ``suews-engine`` accepts omitted OHM and AnOHM fields under ``storage_heat=0``."""
    engine_bin = _locate_suews_engine()

    def mutate(doc: dict):
        lc = doc["sites"][0]["properties"]["land_cover"]
        for surface in ("paved", "bldgs", "evetr", "dectr", "grass", "bsoil", "water"):
            for key in (
                "ch_anohm",
                "rho_cp_anohm",
                "k_anohm",
                "ohm_threshold_summer_winter",
                "ohm_threshold_wet_dry",
                "ohm_coef",
            ):
                lc[surface].pop(key, None)

    config_path = _prepare_resolved_run(
        tmp_path,
        emissions=2,
        storage_heat=0,
        n_forcing_rows=24,
        forcing_qs=0.0,
        mutator=mutate,
    )
    result = subprocess.run(
        [str(engine_bin), "run", str(config_path)],
        capture_output=True,
        text=True,
        timeout=60,
        check=False,
    )
    assert result.returncode == 0, (
        f"storage_heat=0 with omitted OHM failed ({result.returncode}): {result.stderr}"
    )
    assert (tmp_path / "suews_output.arrow").is_file()


def test_engine_rejects_deleted_evetr_surface_block(tmp_path: Path):
    """Deleting an entire zero-fraction vegetation block must not bypass OHM/sfr/LAI guards."""
    engine_bin = _locate_suews_engine()

    def mutate(doc: dict):
        doc["sites"][0]["properties"]["land_cover"].pop("evetr", None)

    config_path = _prepare_resolved_run(
        tmp_path,
        emissions=32,
        storage_heat=0,
        mutator=mutate,
    )
    result = subprocess.run(
        [str(engine_bin), "run", str(config_path)],
        capture_output=True,
        text=True,
        timeout=60,
        check=False,
    )
    assert result.returncode != 0, (
        f"deleted evetr block must not exit 0 (stdout={result.stdout})"
    )
    assert "properties.land_cover.evetr`" in result.stderr, result.stderr
    assert "is missing" in result.stderr, result.stderr
    assert not (tmp_path / "suews_output.arrow").exists()


@pytest.mark.parametrize(
    "sfr_mutation",
    ["omit", "bare_null", "wrapped_null"],
)
def test_engine_rejects_unresolved_veg_sfr_with_omitted_bioco2(
    tmp_path: Path,
    sfr_mutation: str,
):
    """Omitted or null vegetation ``sfr`` cannot bypass BioCO2 guards in ``suews-engine``."""
    engine_bin = _locate_suews_engine()

    def mutate(doc: dict):
        grass = doc["sites"][0]["properties"]["land_cover"]["grass"]
        if sfr_mutation == "omit":
            grass.pop("sfr", None)
        elif sfr_mutation == "bare_null":
            grass["sfr"] = None
        else:
            grass["sfr"] = {"value": None}
        for key in ("beta_bio_co2", "alpha_bio_co2", "resp_a", "resp_b"):
            grass.pop(key, None)

    config_path = _prepare_resolved_run(
        tmp_path,
        emissions=12,
        storage_heat=1,
        mutator=mutate,
    )
    result = subprocess.run(
        [str(engine_bin), "run", str(config_path)],
        capture_output=True,
        text=True,
        timeout=60,
        check=False,
    )
    assert result.returncode != 0, (
        f"unresolved grass.sfr ({sfr_mutation}) must not exit 0"
    )
    assert "properties.land_cover.grass.sfr" in result.stderr, result.stderr
    assert not (tmp_path / "suews_output.arrow").exists()


def test_engine_rejects_malformed_inactive_parent_blocks(tmp_path: Path):
    """Malformed non-mapping parent blocks fail cleanly even on inactive option branches."""
    engine_bin = _locate_suews_engine()

    def mutate(doc: dict):
        lc = doc["sites"][0]["properties"]["land_cover"]
        lc["paved"]["ohm_coef"] = "not-a-mapping"

    config_path = _prepare_resolved_run(
        tmp_path,
        emissions=2,
        storage_heat=0,
        mutator=mutate,
    )
    result = subprocess.run(
        [str(engine_bin), "run", str(config_path)],
        capture_output=True,
        text=True,
        timeout=60,
        check=False,
    )
    assert result.returncode != 0, (
        f"malformed inactive ohm_coef must not exit 0 (stdout={result.stdout})"
    )
    assert "properties.land_cover.paved.ohm_coef" in result.stderr, result.stderr
    assert "must be a mapping" in result.stderr, result.stderr
    assert not (tmp_path / "suews_output.arrow").exists()
