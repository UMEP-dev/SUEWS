"""Exercise a targeted SPARTACUS configuration change on the existing sample.

This checks validation and execution, not agreement with observations.
The test-local geometry changes are not a new scientific reference dataset.
"""

import numpy as np
import pytest

import supy as sp

pytestmark = [pytest.mark.physics, pytest.mark.rust]


def test_spartacus_config_patch_runs_and_reports_radiation():
    sim = sp.SUEWSSimulation.from_sample_data()
    config = sim.config.model_dump(exclude_none=True, mode="json")
    # The sample has no observed incoming longwave: select the air-derived mode.
    config["model"]["physics"]["net_radiation"] = {"value": 1003}
    layers = config["sites"][0]["properties"]["vertical_layers"]
    # Its 15-22 m layer is above both 13.1 m tree crowns.
    layers["veg_frac"]["value"][2] = 0.0
    layers["veg_scale"]["value"][2] = 0.0
    for key in ("_yaml_path", "_auto_generate_annotated", "_yaml_raw"):
        config.pop(key, None)
    sim.update_config(config, auto_load_forcing=False)

    assert sim.config.model.physics.net_radiation.value.value == 1003
    output = sim.run(end_date=sim._df_forcing.index[23], n_jobs=1)
    df = output.df
    qn = df["SUEWS", "QN"].to_numpy()
    assert qn.shape[0] == 24
    assert np.isfinite(qn).all()
    kup_spc = df["SPARTACUS", "Kup"].to_numpy()
    assert kup_spc.shape[0] == 24
    assert np.isfinite(kup_spc).all()
