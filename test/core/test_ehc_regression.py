import numpy as np
import pytest

import supy as sp


pytestmark = [
    pytest.mark.physics,
    pytest.mark.core,
    pytest.mark.rust,
    pytest.mark.smoke,
]


SURFACE_NAMES = ("paved", "bldgs", "evetr", "dectr", "grass", "bsoil", "water")


def _run_short_ehc_with_surface_cp(rho_cp):
    sim = sp.SUEWSSimulation.from_sample_data()
    layer_values = [rho_cp] * 5
    surface_updates = {
        name: {"thermal_layers": {"rho_cp": {"value": layer_values}}}
        for name in SURFACE_NAMES
    }
    sim.update_config(
        {
            "model": {
                "physics": {
                    "net_radiation": {"value": 3},
                    "storage_heat": {"value": 5},
                }
            },
            "sites": {
                "properties": {
                    "land_cover": surface_updates,
                }
            },
        }
    )

    output = sim.run(end_date=sim._df_forcing.index[11])
    return output.df[("SUEWS", "QS")].to_numpy()


def test_ehc_lumped_storage_is_sensitive_to_surface_rho_cp():
    low_cp_qs = _run_short_ehc_with_surface_cp(1.0e6)
    high_cp_qs = _run_short_ehc_with_surface_cp(4.0e6)

    assert low_cp_qs.shape == high_cp_qs.shape
    assert np.max(np.abs(high_cp_qs - low_cp_qs)) > 1.0
