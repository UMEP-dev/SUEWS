"""Shared setup and serialisation for the committed implicit EHC reference."""

from pathlib import Path

import pandas as pd

from supy import SUEWSSimulation
from supy.data_model import SUEWSConfig

ROOT = Path(__file__).resolve().parents[2]
REFERENCE_DIR = Path(__file__).parent / "ehc_reference"
CASES = ("ehc_slab", "ehc_facets")
# Continuous annual evolution is needed to retain the state entering these
# windows: startup, winter envelope losses, the 9 August explicit-oscillation
# night, and the 17-19 August warm-night convergence window.
WINDOWS = (
    ("2012-01-01", "2012-01-03"),
    ("2012-02-01", "2012-02-04"),
    ("2012-08-09", "2012-08-10"),
    ("2012-08-17", "2012-08-20"),
)
COLUMNS = ([('SUEWS', name) for name in ("QN", "QF", "QS", "QH", "QE", "Ts", "RA")]
           + [('EHC', name) for name in ("dUdt_EHC", "QBot_EHC", "QIndoor_EHC", "QF_Air",
                                         "T_Indoor_EHC", "T_Deep_EHC")]
           + [('debug', 'iter')])


def build_sim(case, coupling=None):
    """Build the sample site with omitted coupling/stability defaults."""
    sim = SUEWSSimulation.from_sample_data()
    config = sim.config.model_dump(exclude_none=True, mode="json")
    physics = config["model"]["physics"]
    physics.pop("ehc_surface_coupling", None)
    physics.pop("control_ehc_stability", None)
    if coupling is not None:
        physics["ehc_surface_coupling"] = coupling
    if case != "sample":
        physics["storage_heat"] = {"value": 5}
    if case == "ehc_facets":
        physics["net_radiation"] = {"value": 1003}
        layers = config["sites"][0]["properties"]["vertical_layers"]
        layers["veg_frac"]["value"][2] = 0.0
        layers["veg_scale"]["value"][2] = 0.0
    for key in ("_yaml_path", "_auto_generate_annotated", "_yaml_raw"):
        config.pop(key, None)
    sim.update_config(SUEWSConfig.model_validate(config), auto_load_forcing=False)
    return sim


def select_reference(frame):
    """Select fixed windows without restarting or discarding preceding state."""
    times = frame.index.get_level_values("datetime")
    selected = pd.Series(False, index=frame.index)
    for start, end in WINDOWS:
        selected |= (times >= pd.Timestamp(start)) & (times < pd.Timestamp(end))
    result = frame.loc[selected, COLUMNS].copy()
    result.columns = ["|".join(column) for column in result.columns]
    return result


def reference_csv(frame):
    """Use the same seven-significant-figure precision as the OHM reference."""
    return frame.to_csv(float_format="%.7g", lineterminator="\n")
