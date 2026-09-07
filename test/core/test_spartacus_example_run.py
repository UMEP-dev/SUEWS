"""Short simulation on the packaged SPARTACUS-Surface example (gh#1699).

Confirms that the shipped ``sample_config_spartacus.yml`` runs end to end with
SPARTACUS-Surface net radiation and produces finite radiation output, so users
who scaffold it with ``suews init --template spartacus`` get a working case.
This is a mechanics check, not a scientific validation: nothing here is
compared against observations or against the NARP sample's numbers.
"""

from importlib.resources import as_file, files

import numpy as np
import pytest

import supy as sp

pytestmark = [pytest.mark.physics, pytest.mark.rust]


def test_spartacus_example_runs_and_reports_radiation():
    with as_file(
        files("supy").joinpath("sample_data/sample_config_spartacus.yml")
    ) as path:
        sim = sp.SUEWSSimulation(path)

    assert sim._config.model.physics.net_radiation.value.value == 1003

    # Two hours at the 5-minute sample step; enough to exercise SPARTACUS in
    # the driver without turning this into a slow test.
    output = sim.run(end_date=sim._df_forcing.index[23], n_jobs=1)
    df = output.df

    qn = df["SUEWS", "QN"].to_numpy()
    assert qn.shape[0] == 24
    assert np.isfinite(qn).all()
    assert "SPARTACUS" in df.columns.get_level_values(0)
    kup_spc = df["SPARTACUS", "Kup"].to_numpy()
    assert np.isfinite(kup_spc).all()
