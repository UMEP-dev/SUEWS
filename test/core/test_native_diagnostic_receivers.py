"""Native warning date/grid identity through C, Rust, PyO3 and Python (#2096)."""

from importlib import import_module

import pandas as pd
import pytest

from supy import SUEWSSimulation

run_rust = import_module("supy._run_rust")

pytestmark = [
    pytest.mark.core,
    pytest.mark.physics,
    pytest.mark.rust,
    pytest.mark.contract("C-CAPI", "C-NATIVE-ERROR"),
]


@pytest.mark.parametrize("grid_order", [(37, 11), (11, 37)])
def test_native_warning_dates_and_grid_identity(sample_yaml_path, grid_order):
    """Keep exact native dates associated with the warning grid in either order."""
    sim = SUEWSSimulation(str(sample_yaml_path))
    config = sim.config.model_copy(deep=True)
    config.model.physics.storage_heat = 6  # DyOHM, as in #1737's live guard test
    sites = {}
    for grid_id in grid_order:
        site = config.sites[0].model_copy(deep=True)
        site.gridiv = grid_id
        if grid_id == 37:
            # Only this grid violates the explicit thermal-step stability bound.
            site.properties.land_cover.paved.thermal_layers.rho_cp.value[0] = 2.0e5
        sites[grid_id] = site
    config.sites = list(sites.values())

    # Real sample rows straddling midnight: a first-record day copied into later
    # records cannot pass even though count, message and minute remain correct.
    forcing = sim.forcing.df
    first_day = forcing.index[0].normalize()
    window = forcing.loc[
        (forcing.index >= first_day + pd.Timedelta(hours=23, minutes=45))
        & (forcing.index <= first_day + pd.Timedelta(days=1, minutes=15))
    ]
    assert window.index.normalize().nunique() == 2
    _, _, warnings = run_rust.run_suews_rust_multi(config, window, max_workers=2)
    frame = warnings.to_frame()
    assert frame["grid"].tolist() == [37] * len(window)
    assert frame["datetime"].tolist() == window.index.tolist()
    assert frame["location"].tolist() == ["cal_tsfc_dyohm"] * len(window)
    assert frame["message"].str.contains("time step may be too large").all()
    assert warnings.totals == {11: 0, 37: len(window)}
