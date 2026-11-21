"""Resolved drainage inputs must reach each wet grid without native fallbacks."""

from copy import deepcopy
from importlib import import_module
from importlib.resources import files

import numpy as np
import pandas as pd
import pytest
import yaml

from supy import SUEWSSimulation
from supy.data_model import SUEWSConfig

pytestmark = [
    pytest.mark.api,
    pytest.mark.physics,
    pytest.mark.contract("C-STRICT", "C-BIND", "C-RESPONSE", "C-ERROR"),
]
_run_rust = import_module("supy._run_rust")


@pytest.fixture(scope="module")
def drainage_source():
    sim = SUEWSSimulation(str(files("supy") / "sample_data/sample_config.yml"))
    data = sim.config.model_dump(mode="json", exclude_none=True)
    forcing = sim.forcing.df.iloc[:24].copy()
    # At this revision the in-memory rain column is an interval depth (mm).
    # File output rate metadata does not change the native forcing contract.
    forcing["rain"] = 12.0
    return data, forcing


def drainage_config(source, pairs):
    data = deepcopy(source)
    template = data["sites"][0]
    data["sites"] = []
    for grid, pipe, water in pairs:
        site = deepcopy(template)
        site["gridiv"] = grid
        for field, value in [("pipecapacity", pipe), ("runofftowater", water)]:
            if value is None:
                site["properties"].pop(field, None)
            else:
                site["properties"][field] = {"value": value}
        data["sites"].append(site)
    return SUEWSConfig(**data)


def wet_run(config, forcing, n_jobs=2):
    sim = SUEWSSimulation(config)
    sim.update_forcing(forcing)
    return sim.run(
        start_date=forcing.index[0], end_date=forcing.index[-1], n_jobs=n_jobs
    ).df


def test_source_drainage_defaults_are_exported(drainage_source):
    source, _ = drainage_source
    config = drainage_config(source, [(73, None, None), (91, 0.125, 0.75)])
    emitted = yaml.safe_load(
        _run_rust.config_to_engine_yaml(config, for_file_output=False)
    )
    for site, expected in zip(emitted["sites"], [(100.0, 0.0), (0.125, 0.75)]):
        assert (
            tuple(
                (
                    site["properties"][key]["value"]
                    if isinstance(site["properties"][key], dict)
                    else site["properties"][key]
                )
                for key in ("pipecapacity", "runofftowater")
            )
            == expected
        )


@pytest.mark.parametrize("reverse", [False, True])
def test_wet_overflow_distinguishes_drainage_fields(drainage_source, reverse):
    source, forcing = drainage_source
    # Crossed controls isolate each field; all pairs are valid source inputs.
    pairs = [(73, 0.125, 0.0), (91, 0.125, 0.75)]
    if reverse:
        pairs.reverse()
    config = drainage_config(source, pairs)
    parallel = wet_run(config, forcing)
    serial = wet_run(config, forcing, n_jobs=1)
    pd.testing.assert_frame_equal(
        parallel.sort_index(), serial.sort_index(), check_exact=True
    )
    a = parallel.xs(73, level="grid")["SUEWS"]
    b = parallel.xs(91, level="grid")["SUEWS"]
    assert a["ROImp"].sum() > 0, "scenario must activate pipe overflow"
    np.testing.assert_allclose(b["ROImp"], a["ROImp"] * 0.25, rtol=1e-12, atol=1e-15)
    # The water body has ample storage: pipe overflow changes ROImp, not
    # necessarily ROWater. Its routing fraction must not change pipe capacity.
    np.testing.assert_array_equal(b["ROPipe"], a["ROPipe"])
    capacity_pairs = [(73, 100.0, 0.0), (91, 0.125, 0.0)]
    if reverse:
        capacity_pairs.reverse()
    capacity = wet_run(drainage_config(source, capacity_pairs), forcing)
    high = capacity.xs(73, level="grid")["SUEWS"]
    low = capacity.xs(91, level="grid")["SUEWS"]
    assert high["ROPipe"].sum() > low["ROPipe"].sum()
    assert low["ROImp"].sum() > high["ROImp"].sum()
    defaults = wet_run(drainage_config(source, [(73, None, None)]), forcing)
    pd.testing.assert_frame_equal(defaults, capacity.loc[[73]], check_exact=True)


@pytest.mark.parametrize("field", ["pipecapacity", "runofftowater"])
@pytest.mark.parametrize("bad", ["missing", "null", "text", "nan"])
def test_malformed_later_drainage_preserves_rejected_grid_buffers(
    drainage_source, field, bad
):
    source, forcing = drainage_source
    config = drainage_config(source, [(73, 100.0, 0.0), (91, 0.125, 0.75)])
    root = yaml.safe_load(
        _run_rust.config_to_engine_yaml(config, for_file_output=False)
    )
    props = root["sites"][1]["properties"]
    if bad == "missing":
        props.pop(field)
    else:
        props[field] = {
            "value": {"null": None, "text": "broken", "nan": float("nan")}[bad]
        }
    configs = []
    for site in root["sites"]:
        one = deepcopy(root)
        one["sites"] = [site]
        configs.append(yaml.safe_dump(one))
    block = _run_rust._prepare_forcing_block(forcing)
    buffers = [_run_rust._alloc_output_buffers(len(block)) for _ in configs]
    for data, dates in buffers:
        data.fill(12345.0)
        dates.fill(12345.0)
    native = _run_rust._load_rust_module()
    expected = _run_rust._alloc_output_buffers(len(block))
    native.run_suews_into(configs[0], block, len(block), *expected, None)
    with pytest.raises(RuntimeError, match=rf"sites\[gridiv=91\].properties.{field}"):
        native.run_suews_multi_into(
            configs, block, len(block), [b[0] for b in buffers], [b[1] for b in buffers]
        )
    # Grids run independently: the valid sibling completes normally, while
    # the rejected grid never enters the kernel or writes its caller buffers.
    for actual, serial in zip(buffers[0], expected):
        np.testing.assert_array_equal(actual, serial)
    for rejected in buffers[1]:
        np.testing.assert_array_equal(rejected, 12345.0)


@pytest.mark.parametrize(
    "pipe, water", [(0.0, 0.0), (-1.0, 0.0), (100.0, -0.1), (100.0, 1.1)]
)
def test_source_drainage_range_policy_is_unchanged(drainage_source, pipe, water):
    source, _ = drainage_source
    with pytest.raises(ValueError):
        drainage_config(source, [(73, pipe, water)])
