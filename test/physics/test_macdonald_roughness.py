"""Regression tests for the MacDonald (1998) momentum-roughness option (gh#1615).

`RoughLenMomMethod = 3` must derive `z0m` from the MacDonald displacement height
computed in the same timestep, not from the persistent `zdm` state field (which
carries the previous timestep's plan-area-blended value, and zero on the first
timestep).

The site used here has no trees, so PAI, Zh, FAI and the building plan-area
fraction are all constant over the run and the MacDonald relations can be
asserted in closed form.
"""

from conftest import load_sample_frames, run_simulation
import numpy as np
import pytest
import yaml

from supy._env import trv_supy_module
from supy.data_model import SUEWSConfig

pytestmark = pytest.mark.physics

# Roughness lengths of the non-roughness (zh = 0) surfaces, mirroring the
# PARAMETER values in suews_phys_resist.f95.
Z0M_PAVED = 0.003
Z0M_GRASS = 0.02
Z0M_BSOIL = 0.002
Z0M_WATER = 0.0005

# MacDonald (1998) coefficients, as used in suews_phys_resist.f95.
MACDONALD_A = 4.43
DRAG_COEFFICIENT = 1.2
BETA = 1.0
VON_KARMAN = 0.4

N_STEPS = 288 * 3  # three days at the 5-min sample resolution


def _tree_free_macdonald_config(tmp_path):
    """Sample config with MacDonald roughness and no tree cover.

    Zeroing the deciduous fraction removes the phenology feedback on porosity,
    so Zh, FAI and PAI stay constant and the closed-form check below is exact.
    """
    src = trv_supy_module / "sample_data" / "sample_config.yml"
    raw = yaml.safe_load(src.read_text(encoding="utf-8"))

    raw["model"]["physics"]["roughness_length_momentum"] = "macdonald"
    # Pin the FAI source: the closed form below uses the provided FAIBldg as-is,
    # which only holds for FAImethod = 0. Do not rely on the sample default.
    raw["model"]["physics"]["frontal_area_index"] = "observed"

    land_cover = raw["sites"][0]["properties"]["land_cover"]
    # Move the deciduous fraction into grass, keeping the fractions summing to 1.
    land_cover["grass"]["sfr"]["value"] += land_cover["dectr"]["sfr"]["value"]
    land_cover["dectr"]["sfr"]["value"] = 0.0
    land_cover["evetr"]["sfr"]["value"] = 0.0

    path = tmp_path / "config_macdonald.yml"
    path.write_text(yaml.safe_dump(raw, sort_keys=False), encoding="utf-8")
    return path, land_cover


def _expected_z0m_zdm(land_cover):
    """MacDonald z0m and zdm, plus the surface-blended values SUEWS reports."""
    sfr = {
        name: land_cover[name]["sfr"]["value"] for name in land_cover if name != "ref"
    }
    height_building = land_cover["bldgs"]["bldgh"]["value"]
    # With FAImethod = 0 the provided frontal-area index is used as given; with
    # buildings as the only roughness elements it is the whole of FAI.
    fai = land_cover["bldgs"]["faibldg"]["value"]

    # Buildings are the only roughness elements, so PAI and Zh follow directly.
    plan_area_index = sfr["bldgs"]
    lambda_p = sfr["bldgs"]
    zh = height_building

    # MacDonald displacement height, then roughness length from that same value.
    zdm_over_zh = 1 + MACDONALD_A ** (-lambda_p) * (lambda_p - 1)
    zdm_zh = zdm_over_zh * zh
    exponent = (
        0.5 * BETA * DRAG_COEFFICIENT / VON_KARMAN**2 * (1 - zdm_over_zh) * fai
    ) ** (-0.5)
    z0m_zh = (1 - zdm_over_zh) * np.exp(-exponent) * zh

    # SUEWS blends the roughness elements with the zh = 0 surfaces by plan area.
    z0m_non_rough = (
        Z0M_PAVED * sfr["paved"]
        + Z0M_GRASS * sfr["grass"]
        + Z0M_BSOIL * sfr["bsoil"]
        + Z0M_WATER * sfr["water"]
    )
    z0m = plan_area_index * z0m_zh + z0m_non_rough
    zdm = plan_area_index * zdm_zh  # zdm_zh0 is zero for the non-rough surfaces
    return z0m, zdm


@pytest.fixture(scope="module")
def macdonald_run(tmp_path_factory):
    tmp_path = tmp_path_factory.mktemp("macdonald")
    config_path, land_cover = _tree_free_macdonald_config(tmp_path)

    df_state = SUEWSConfig.from_yaml(str(config_path)).to_df_state()
    _, df_forcing = load_sample_frames()
    df_output, _ = run_simulation(df_forcing.iloc[:N_STEPS], df_state)

    return df_output.loc[:, "SUEWS"], land_cover


@pytest.mark.core
def test_z0m_matches_macdonald_closed_form(macdonald_run):
    """z0m must follow MacDonald (1998) using this timestep's displacement height.

    Before gh#1615 the roughness length was computed from the persistent `zdm`
    state, which is smaller than the MacDonald displacement height (it is blended
    down by the non-roughness plan area), inflating z0m several-fold.
    """
    df_output, land_cover = macdonald_run
    z0m_expected, zdm_expected = _expected_z0m_zdm(land_cover)

    assert df_output["zdm"].to_numpy() == pytest.approx(zdm_expected, rel=1e-6)
    assert df_output["z0m"].to_numpy() == pytest.approx(z0m_expected, rel=1e-6)


@pytest.mark.core
def test_z0m_is_not_contaminated_by_initial_state(macdonald_run):
    """z0m must be constant here: the site morphology never changes.

    With the bug, `zdm` is still zero when the model starts, so (1 - zdm/Zh) = 1
    and z0m takes its maximum possible value before settling once the state fills
    in. That start-up transient survives the output averaging, so a flat z0m
    series is enough to pin it.
    """
    df_output, _ = macdonald_run
    z0m = df_output["z0m"].to_numpy()

    assert z0m[0] == pytest.approx(z0m[1], rel=1e-9)
    assert np.ptp(z0m) == pytest.approx(0.0, abs=1e-9)
