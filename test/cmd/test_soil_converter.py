"""Coordinated conversion: independent algebra, immutable inputs and refusals."""

from copy import deepcopy
from hashlib import sha256
import json
from pathlib import Path

from click.testing import CliRunner
import numpy as np
import pandas as pd
import pytest
import yaml

from supy import SUEWSSimulation
from supy.cmd.schema_cli import cli
from supy.cmd.table_converter import convert_table_cmd
from supy.data_model.core.config import SUEWSConfig
from supy.util.converter.yaml_upgrade import YamlUpgradeError, upgrade_soil_observation

pytestmark = pytest.mark.api
FIXTURES = Path(__file__).parents[1] / "fixtures" / "soil_observation"


def convert(tmp_path, method=1, **kwargs):
    paths = [
        FIXTURES / f"legacy_method{method}.yml",
        FIXTURES / f"legacy_method{method}.txt",
        tmp_path / "new.yml",
        tmp_path / "new.txt",
        tmp_path / "manifest.json",
    ]
    options = {"layer_top": 0, "layer_depth": 0.2}
    options.update(kwargs)
    return upgrade_soil_observation(*paths, **options), paths


@pytest.mark.parametrize("method", [1, 2])
def test_convert_content_and_provenance(tmp_path, method):
    inputs = [FIXTURES / f"legacy_method{method}.{ext}" for ext in ("yml", "txt")]
    before = [(p.read_bytes(), p.stat().st_mtime_ns) for p in inputs]
    report, paths = convert(tmp_path, method)
    old = pd.read_csv(inputs[1], sep=r"\s+")
    new = pd.read_csv(paths[3], sep=r"\s+")
    # 1000 * theta * 0.2 * 0.8, or 1250 * w * 0.2 * 0.8.
    np.testing.assert_allclose(new.sm, [0, 50, 120, 160], rtol=0, atol=1e-12)
    pd.testing.assert_frame_equal(new.drop(columns="sm"), old.drop(columns="xsmd"))
    assert [(p.read_bytes(), p.stat().st_mtime_ns) for p in inputs] == before
    assert report["resolved_method"] == method
    assert report["original_method"] == method
    for path in inputs:
        assert (
            report["input_hashes"][path.name] == sha256(path.read_bytes()).hexdigest()
        )
    for path in paths[2:4]:
        assert (
            report["output_hashes"][path.name] == sha256(path.read_bytes()).hexdigest()
        )
    assert json.loads(paths[4].read_text(encoding="utf-8")) == report
    cfg = yaml.safe_load(paths[2].read_text(encoding="utf-8"))
    assert cfg["schema_version"] == "2026.6.dev6"
    obs = cfg["sites"][0]["properties"]["soil_observation"]
    assert obs["layer_depth"]["value"] == pytest.approx(0.2)
    assert obs["bulk_density"]["value"] == 1250
    assert obs["bulk_density"]["ref"]["desc"] == "Synthetic dry fine-earth density"
    assert "smcap" not in obs
    assert any("Sensor saturation capacity is retired" in x for x in report["changes"])
    assert any("Count=1" in x and "40" in x for x in report["warnings"])
    assert any("wilting" in x for x in report["warnings"])
    SUEWSConfig.model_validate(cfg)
    oracle = yaml.safe_load((FIXTURES / "canonical.yml").read_text(encoding="utf-8"))
    expected_config = SUEWSConfig.model_validate(oracle)
    assert (
        expected_config.sites[0].properties.soil_observation
        == SUEWSConfig.model_validate(cfg).sites[0].properties.soil_observation
    )
    # Exercise the actual emitted runnable YAML and the kernel deficit boundary.
    simulation = SUEWSSimulation(paths[2])
    forcing = simulation.forcing.df
    output = simulation.run(start_date=forcing.index[0], end_date=forcing.index[-1])
    np.testing.assert_allclose(output.df.SUEWS.SMD, [80, 70, 0, 0], rtol=0, atol=1e-12)
    assert output.df.SUEWS.QE.max() > 0
    # A second coordinated operation is content-idempotent and needs no geometry.
    again = tmp_path / "again"
    repeat = upgrade_soil_observation(
        paths[2],
        paths[3],
        again / "new.yml",
        again / "new.txt",
        again / "manifest.json",
    )
    assert repeat["completed"]
    assert (again / "new.txt").read_bytes() == paths[3].read_bytes()


def test_dry_run_never_stamps_or_writes(tmp_path):
    report, paths = convert(tmp_path, dry_run=True, layer_depth=None, layer_top=None)
    assert "requires confirmed layer bounds" in report["errors"][0]
    assert not report["completed"]
    assert all(not p.exists() for p in paths[2:])
    report, _ = convert(tmp_path, dry_run=True)
    assert not report["errors"]
    assert report["output_hashes"]
    assert all(not p.exists() for p in paths[2:])


@pytest.mark.parametrize(
    "case,match",
    [
        ("ambiguous", "ambiguous observed selector"),
        ("conflict", "Both legacy and canonical"),
        ("depth", "full configured soil store|layer_depth"),
        ("missing_fc", "soil_field_capacity"),
        ("density", "Gravimetric conversion requires"),
        ("multi_grid", "Shared forcing requires matching"),
        ("gap", "contains missing values"),
    ],
)
def test_refusals_are_transactional(tmp_path, case, match):
    method = 2 if case == "density" else 1
    raw = yaml.safe_load(
        (FIXTURES / f"legacy_method{method}.yml").read_text(encoding="utf-8")
    )
    physics = raw["model"]["physics"]
    props = raw["sites"][0]["properties"]
    if case == "ambiguous":
        physics["soil_moisture_deficit"] = "observed"
    elif case == "conflict":
        physics["soil_moisture"] = "observed"
    elif case == "depth":
        props["land_cover"]["paved"]["soil_depth"] = 100
    elif case == "missing_fc":
        del props["land_cover"]["paved"]["soil_field_capacity"]
    elif case == "density":
        del props["soil_observation"]["bulk_density"]
    elif case == "multi_grid":
        second = deepcopy(raw["sites"][0])
        second["gridiv"] = 99
        second["properties"]["land_cover"]["paved"]["soil_store_capacity"] = 60
        raw["sites"].append(second)
    source = tmp_path / "old.yml"
    source.write_text(yaml.safe_dump(raw), encoding="utf-8")
    forcing = FIXTURES / (
        "legacy_gap.txt" if case == "gap" else f"legacy_method{method}.txt"
    )
    outputs = [tmp_path / n for n in ("new.yml", "new.txt", "manifest.json")]
    before = source.read_bytes()
    with pytest.raises((ValueError, YamlUpgradeError), match=match):
        upgrade_soil_observation(
            source, forcing, *outputs, layer_top=0, layer_depth=0.2
        )
    assert source.read_bytes() == before
    assert all(not p.exists() for p in outputs)


@pytest.mark.parametrize(
    "command,prefix", [(convert_table_cmd, []), (cli, ["convert-soil"])]
)
def test_both_cli_routes(tmp_path, command, prefix):
    args = [
        *prefix,
        "-i",
        str(FIXTURES / "legacy_method2.yml"),
        "-o",
        str(tmp_path / "new.yml"),
        "--forcing",
        str(FIXTURES / "legacy_method2.txt"),
        "--forcing-output",
        str(tmp_path / "new.txt"),
        "--manifest",
        str(tmp_path / "manifest.json"),
        "--layer-top",
        "0",
        "--layer-depth",
        "0.2",
        "--format",
        "json",
    ]
    result = CliRunner().invoke(command, args)
    assert result.exit_code == 0, result.output
    assert json.loads(result.output)["data"]["resolved_method"] == 2


def test_modelled_missing_tokens_and_overwrite_guard(tmp_path):
    cfg = yaml.safe_load((FIXTURES / "legacy_method1.yml").read_text(encoding="utf-8"))
    cfg["model"]["physics"]["soil_moisture_deficit"] = 0
    source = tmp_path / "old.yml"
    source.write_text(yaml.safe_dump(cfg), encoding="utf-8")
    forcing = tmp_path / "old.txt"
    forcing.write_text(
        (FIXTURES / "legacy_gap.txt")
        .read_text(encoding="utf-8")
        .replace("0.25", "-999.00")
        .replace("0.5", "nan"),
        encoding="utf-8",
    )
    outputs = [tmp_path / n for n in ("new.yml", "new.txt", "manifest.json")]
    upgrade_soil_observation(source, forcing, *outputs)
    assert outputs[1].read_bytes() == forcing.read_bytes().replace(b"xsmd", b"sm")
    with pytest.raises(YamlUpgradeError, match="new, distinct paths"):
        upgrade_soil_observation(source, forcing, *outputs)


def test_explicit_ambiguous_unit_and_sensor_depth(tmp_path):
    cfg = yaml.safe_load((FIXTURES / "legacy_method2.yml").read_text(encoding="utf-8"))
    cfg["model"]["physics"]["soil_moisture_deficit"] = "observed"
    cfg["sites"][0]["properties"]["soil_observation"]["depth"]["value"] = 50
    source = tmp_path / "old.yml"
    source.write_text(yaml.safe_dump(cfg), encoding="utf-8")
    paths = [source, FIXTURES / "legacy_method2.txt"] + [
        tmp_path / n for n in ("new.yml", "new.txt", "manifest.json")
    ]
    report = upgrade_soil_observation(
        *paths, layer_top=0, layer_depth=0.2, source_unit="kg kg-1"
    )
    assert report["original_method"] == "observed"
    assert report["resolved_method"] == 2
    assert yaml.safe_load(paths[2].read_text(encoding="utf-8"))["sites"][0][
        "properties"
    ]["soil_observation"]["layer_depth"]["value"] == pytest.approx(0.2)


def test_positional_contract_and_header_collision(tmp_path):
    original = (FIXTURES / "legacy_method1.txt").read_text(encoding="utf-8")
    forcing = tmp_path / "positional.txt"
    forcing.write_text(original.split("\n", 1)[1], encoding="utf-8")
    outputs = [tmp_path / n for n in ("new.yml", "new.txt", "manifest.json")]
    args = [FIXTURES / "legacy_method1.yml", forcing, *outputs]
    with pytest.raises(YamlUpgradeError, match="explicit source_contract"):
        upgrade_soil_observation(*args, layer_top=0, layer_depth=0.2)
    report = upgrade_soil_observation(
        *args, layer_top=0, layer_depth=0.2, source_contract="1.2.0"
    )
    assert report["row_count"] == 4
    np.testing.assert_allclose(
        pd.read_csv(outputs[1], sep=r"\s+").sm, [0, 50, 120, 160]
    )
    forcing.write_text(original.replace("xsmd lai", "xsmd sm"), encoding="utf-8")
    report = upgrade_soil_observation(
        FIXTURES / "legacy_method1.yml",
        forcing,
        tmp_path / "other.yml",
        tmp_path / "other.txt",
        tmp_path / "other.json",
        dry_run=True,
    )
    assert "Both legacy and canonical" in report["errors"][0]


@pytest.mark.parametrize("drop", [False, True])
def test_populated_modelled_input_needs_explicit_decision(tmp_path, drop):
    raw = yaml.safe_load((FIXTURES / "legacy_method1.yml").read_text(encoding="utf-8"))
    raw["model"]["physics"]["soil_moisture_deficit"] = 0
    source = tmp_path / "old.yml"
    source.write_text(yaml.safe_dump(raw), encoding="utf-8")
    args = [source, FIXTURES / "legacy_method1.txt"] + [
        tmp_path / n for n in ("new.yml", "new.txt", "manifest.json")
    ]
    with pytest.raises(YamlUpgradeError, match="modelled xsmd values require"):
        upgrade_soil_observation(*args)
    report = upgrade_soil_observation(
        *args,
        drop_legacy=drop,
        source_unit=None if drop else "m3 m-3",
        layer_top=0,
        layer_depth=0.2,
    )
    actual = pd.read_csv(args[3], sep=r"\s+").sm
    np.testing.assert_allclose(actual, [-999] * 4 if drop else [0, 50, 120, 160])
    if drop:
        assert any("explicit request" in change for change in report["changes"])


def test_mismatched_confirmed_legacy_thickness_refused(tmp_path):
    raw = yaml.safe_load((FIXTURES / "legacy_method1.yml").read_text(encoding="utf-8"))
    raw["sites"][0]["properties"]["soil_observation"]["depth"]["value"] = 50
    source = tmp_path / "old.yml"
    source.write_text(yaml.safe_dump(raw), encoding="utf-8")
    args = [source, FIXTURES / "legacy_method1.txt"] + [
        tmp_path / n for n in ("new.yml", "new.txt", "manifest.json")
    ]
    with pytest.raises(YamlUpgradeError, match="requires confirmed layer bounds"):
        upgrade_soil_observation(
            *args, layer_top=0, layer_depth=0.2, legacy_depth_is_thickness=True
        )


def test_shuffled_columns_text_flags_and_crlf_are_preserved(tmp_path):
    lines = (FIXTURES / "legacy_method1.txt").read_text(encoding="utf-8").splitlines()
    shuffled = []
    for i, line in enumerate(lines):
        tokens = line.split()
        shuffled.append(
            "\t".join([*tokens[4:], *tokens[:4], "quality" if i == 0 else "accepted"])
        )
    original = ("\r\n".join(shuffled) + "\r\n").encode()
    forcing = tmp_path / "shuffled.txt"
    forcing.write_bytes(original)
    paths = [FIXTURES / "legacy_method1.yml", forcing] + [
        tmp_path / n for n in ("new.yml", "new.txt", "manifest.json")
    ]
    upgrade_soil_observation(*paths, layer_top=0, layer_depth=0.2)
    actual = paths[3].read_bytes()
    assert forcing.read_bytes() == original
    assert actual.count(b"\r\n") == 5
    for old, new in zip(original.splitlines(), actual.splitlines()):
        old_tokens, new_tokens = old.split(), new.split()
        # Soil is at original index 19, now 15 after moving the clock columns.
        assert old_tokens[:15] + old_tokens[16:] == new_tokens[:15] + new_tokens[16:]
    np.testing.assert_allclose(pd.read_csv(paths[3], sep=r"\s+").sm, [0, 50, 120, 160])
