"""Public Phase A publication and five-route engine-boundary parity (gh#2099).

This complements the full DataFrame/default/receiver checks in
``test_entrypoint_parity.py``; it does not replace or narrow their selection.
"""

import copy
from importlib.resources import files
import json
from pathlib import Path

from click.testing import CliRunner
import pytest
import yaml

# Exercise the real export boundary in its owning module.
from supy._run_rust import config_to_engine_yaml  # ruff: ignore[import-private-name]
from supy.cmd.suews_cli import cli as suews_cli
from supy.data_model.core import SUEWSConfig
from supy.suews_sim import SUEWSSimulation

pytestmark = [
    pytest.mark.native,
    pytest.mark.cfg,
    pytest.mark.contract("C-PARITY", "C-VALID", "C-ERROR"),
]

# Only loader provenance differs between input routes; retain every engine value.
_LOADER_KEYS = {"_yaml_path", "_auto_generate_annotated", "_yaml_raw"}
_SAFE_LOADER = getattr(yaml, "CSafeLoader", yaml.SafeLoader)
_SAFE_DUMPER = getattr(yaml, "CSafeDumper", yaml.SafeDumper)


@pytest.fixture(scope="module")
def sample_yaml_bytes():
    """Share immutable input bytes, never decoded product configurations."""
    return (files("supy") / "sample_data" / "sample_config.yml").read_bytes()


def _canonical_engine(config):
    resolved = yaml.load(config_to_engine_yaml(config), Loader=_SAFE_LOADER)
    return {key: value for key, value in resolved.items() if key not in _LOADER_KEYS}


@pytest.mark.medium
@pytest.mark.parametrize(
    ("grid_id", "height", "emissivity"),
    [(None, None, None), (17, 18.5, 0.88), (29, 23.5, 0.91)],
    ids=["unchanged", "custom_17", "custom_29"],
)
def test_public_phase_a_preserves_five_route_engine_values(
    sample_yaml_bytes, tmp_path, monkeypatch, grid_id, height, emissivity
):
    """Reload the actual CLI-published file and compare complete resolved values."""
    payload = yaml.load(sample_yaml_bytes, Loader=_SAFE_LOADER)
    if grid_id is not None:
        payload["sites"][0]["gridiv"] = grid_id
        cover = payload["sites"][0]["properties"]["land_cover"]
        cover["bldgs"]["bldgh"] = {"value": height}
        cover["paved"]["emis"] = {"value": emissivity}
    expected_ids = [site["gridiv"] for site in payload["sites"]]

    runner = CliRunner()
    monkeypatch.chdir(tmp_path)
    source = Path("input.yml").resolve()
    source.write_text(
        yaml.dump(payload, Dumper=_SAFE_DUMPER, sort_keys=False), encoding="utf-8"
    )
    result = runner.invoke(
        suews_cli,
        ["validate", "--pipeline", "A", "--forcing", "off", "--format", "json", str(source)],
    )
    assert result.exit_code == 0, result.output
    envelope = json.loads(result.stdout)
    assert envelope["status"] in {"success", "warning"}, envelope
    assert envelope["data"]["phases_run"] == ["A"]
    published = Path(envelope["data"]["updated_yaml"]).resolve()
    assert published != source
    assert published.is_relative_to(source.parent)
    assert published.is_file(), envelope

    # Only update_config inherently materialises a DataFrame. Do not request
    # another frame here: the original full-parity suite covers that contract.
    configurations = {
        "from_yaml": SUEWSConfig.from_yaml(str(source)),
        "from_dict": SUEWSConfig.from_dict(copy.deepcopy(payload)),
        "programmatic": SUEWSConfig(**copy.deepcopy(payload)),
        "update_config": SUEWSSimulation().update_config(
            copy.deepcopy(payload), auto_load_forcing=False
        ).config,
        "public_phase_a": SUEWSConfig.from_yaml(str(published)),
    }
    reference = _canonical_engine(configurations["from_yaml"])
    for route, config in configurations.items():
        resolved = _canonical_engine(config) if route != "from_yaml" else reference
        assert resolved == reference, route
        ids = [site.gridiv for site in config.sites]
        assert ids == expected_ids, route
        assert [site["gridiv"] for site in resolved["sites"]] == expected_ids, route
        if grid_id is not None:
            cover = resolved["sites"][0]["properties"]["land_cover"]
            assert cover["bldgs"]["bldgh"]["value"] == height, route
            assert cover["paved"]["emis"]["value"] == emissivity, route


def test_model_load_allows_land_cover_sum_drift_exceeding_tolerance(
    sample_yaml_bytes, tmp_path
) -> None:
    """Model load keeps accepting configs with land-cover sums off by > 1e-4."""
    payload = yaml.load(sample_yaml_bytes, Loader=_SAFE_LOADER)
    payload["sites"][0]["properties"]["land_cover"]["paved"]["sfr"]["value"] = 0.6
    yaml_path = tmp_path / "drifted_load.yml"
    yaml_path.write_text(
        yaml.dump(payload, Dumper=_SAFE_DUMPER, sort_keys=False), encoding="utf-8"
    )

    assert SUEWSConfig.from_yaml(str(yaml_path)).sites[0].properties.land_cover.paved.sfr.value == 0.6
    assert SUEWSConfig.from_dict(copy.deepcopy(payload)).sites[0].properties.land_cover.paved.sfr.value == 0.6
    assert SUEWSConfig(**copy.deepcopy(payload)).sites[0].properties.land_cover.paved.sfr.value == 0.6


def test_validate_for_engine_and_sim_run_land_cover_sum_policy(short_sample_sim) -> None:
    """Engine handoff and ``sim.run()`` allow <= 1e-4 drift and reject > 1e-4."""
    # Small drift within 1e-4 warns without silently mutating the config, and still runs.
    sim_small = short_sample_sim()
    original_paved = sim_small.config.sites[0].properties.land_cover.paved.sfr.value
    sim_small.config.sites[0].properties.land_cover.paved.sfr.value = original_paved + 5e-5
    with pytest.warns(UserWarning, match="Surface fractions sum to"):
        sim_small.config.validate_for_engine()
    assert sim_small.config.sites[0].properties.land_cover.paved.sfr.value == pytest.approx(
        original_paved + 5e-5
    )
    with pytest.warns(UserWarning, match="Surface fractions sum to"):
        assert config_to_engine_yaml(sim_small.config)
    with pytest.warns(UserWarning, match="Surface fractions sum to"):
        SUEWSSimulation().update_config(copy.deepcopy(sim_small.config), auto_load_forcing=False)
    with pytest.warns(UserWarning, match="Surface fractions sum to"):
        df_out = sim_small.run(end_date=sim_small._df_forcing.index[11])
    assert len(df_out) == 12
    assert sim_small.config.sites[0].properties.land_cover.paved.sfr.value == pytest.approx(
        original_paved + 5e-5
    )

    # Drift > 1e-4 (both positive and negative) and all-zero fail at all boundaries.
    for delta in (2e-4, -2e-4):
        sim_bad = short_sample_sim()
        sim_bad.config.sites[0].properties.land_cover.paved.sfr.value += delta
        with pytest.raises(ValueError, match="Surface fractions sum to"):
            sim_bad.config.validate_for_engine()
        with pytest.raises(ValueError, match="Surface fractions sum to"):
            config_to_engine_yaml(sim_bad.config)
        with pytest.raises(ValueError, match="Surface fractions sum to"):
            SUEWSSimulation().update_config(sim_bad.config, auto_load_forcing=False)
        with pytest.raises(ValueError, match="Surface fractions sum to"):
            sim_bad.run(end_date=sim_bad._df_forcing.index[11])
        assert sim_bad._df_output is None
        assert sim_bad._run_completed is False

    sim_zero = short_sample_sim()
    lc = sim_zero.config.sites[0].properties.land_cover
    for surf in ("paved", "bldgs", "evetr", "dectr", "grass", "bsoil", "water"):
        getattr(lc, surf).sfr.value = 0.0
    with pytest.raises(ValueError, match="All surface fractions are zero"):
        sim_zero.run(end_date=sim_zero._df_forcing.index[11])
    assert sim_zero._df_output is None
    assert sim_zero._run_completed is False


def test_update_config_rejected_land_cover_drift_leaves_simulation_unchanged(
    short_sample_sim, sample_yaml_bytes, tmp_path
) -> None:
    """Rejected dict and YAML updates leave an existing SUEWSSimulation completely intact."""
    sim = short_sample_sim()
    orig_config = sim.config
    orig_paved = orig_config.sites[0].properties.land_cover.paved.sfr.value
    orig_path = sim._config_path
    orig_identity = sim._config_identity
    orig_state = sim.state_init.copy(deep=True)

    # 1. Partial dict update pushing sum(sfr) off by +2e-4 fails atomically.
    with pytest.raises(ValueError, match="Surface fractions sum to"):
        sim.update_config(
            {
                "sites": {
                    0: {
                        "properties": {
                            "land_cover": {
                                "paved": {"sfr": {"value": orig_paved + 2e-4}}
                            }
                        }
                    }
                }
            },
            auto_load_forcing=False,
        )
    assert sim.config is orig_config
    assert sim.config.sites[0].properties.land_cover.paved.sfr.value == pytest.approx(
        orig_paved
    )
    assert sim._config_path == orig_path
    assert sim._config_identity == orig_identity
    assert sim.state_init.equals(orig_state)

    # 2. YAML path update pushing sum(sfr) off by +2e-4 fails atomically.
    payload = yaml.load(sample_yaml_bytes, Loader=_SAFE_LOADER)
    payload["sites"][0]["properties"]["land_cover"]["paved"]["sfr"]["value"] += 2e-4
    bad_yaml = tmp_path / "rejected_update.yml"
    bad_yaml.write_text(
        yaml.dump(payload, Dumper=_SAFE_DUMPER, sort_keys=False), encoding="utf-8"
    )
    with pytest.raises(ValueError, match="Surface fractions sum to"):
        sim.update_config(bad_yaml, auto_load_forcing=False)
    assert sim.config is orig_config
    assert sim.config.sites[0].properties.land_cover.paved.sfr.value == pytest.approx(
        orig_paved
    )
    assert sim._config_path == orig_path
    assert sim._config_identity == orig_identity
    assert sim.state_init.equals(orig_state)


def test_public_phase_c_and_dry_run_reject_land_cover_sum_drift(
    sample_yaml_bytes, tmp_path
) -> None:
    """Public Phase C API, CLI, and --dry-run reject sum(sfr) off by > 1e-4 while raw load stays permissive."""
    import supy

    payload = yaml.load(sample_yaml_bytes, Loader=_SAFE_LOADER)
    payload["sites"][0]["properties"]["land_cover"]["paved"]["sfr"]["value"] += 2e-4
    bad_path = tmp_path / "phase_c_drift.yml"
    bad_path.write_text(
        yaml.dump(payload, Dumper=_SAFE_DUMPER, sort_keys=False), encoding="utf-8"
    )

    # Raw model load still succeeds so callers can inspect or repair.
    assert SUEWSConfig.from_yaml(str(bad_path)) is not None

    # Public Python API Phase C rejects it with structured error.
    c_report = supy.validate_config(bad_path, pipeline="C", forcing="off")
    assert not c_report.is_valid
    assert any(
        i.code == "C.LAND_COVER.SURFACE_FRACTIONS.SUM_ERROR"
        for i in c_report.errors
    ), c_report.errors

    # Public CLI Phase C dry-run and ABC dry-run both reject with zero writes and no duplicate errors.
    runner = CliRunner()
    before_files = set(tmp_path.rglob("*"))
    for pipe in ("C", "ABC"):
        res = runner.invoke(
            suews_cli,
            ["validate", "-p", pipe, "--dry-run", "--format", "json", str(bad_path)],
        )
        assert res.exit_code == 1, res.output
        assert set(tmp_path.rglob("*")) == before_files
        env = json.loads(res.stdout)
        assert env["status"] == "error"
        assert env["data"]["is_valid"] is False
        sfr_errs = [
            err for err in env["errors"] if "Surface fractions sum to" in err["message"]
        ]
        assert len(sfr_errs) == 1, (pipe, env["errors"])

    # Public CLI staged Phase C rejects with structured report and non-zero exit.
    staged_res = runner.invoke(
        suews_cli,
        ["validate", "-p", "C", "--forcing", "off", "--format", "json", str(bad_path)],
    )
    assert staged_res.exit_code == 1, staged_res.output
    staged_env = json.loads(staged_res.stdout)
    assert staged_env["status"] == "error"
    staged_issues = staged_env["data"]["validation_report"]["phases"][0]["issues"]
    assert any(
        issue["code"] == "C.LAND_COVER.SURFACE_FRACTIONS.SUM_ERROR"
        for issue in staged_issues
    ), staged_issues
