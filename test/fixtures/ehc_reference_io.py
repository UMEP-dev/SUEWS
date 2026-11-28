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
STARTUP_WINDOW = WINDOWS[0]
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


def _select_windows(frame, windows):
    """Select fixed half-open datetime windows ``[start, end)``."""
    times = frame.index.get_level_values("datetime")
    selected = pd.Series(False, index=frame.index)
    for start, end in windows:
        selected |= (times >= pd.Timestamp(start)) & (times < pd.Timestamp(end))
    result = frame.loc[selected, COLUMNS].copy()
    result.columns = ["|".join(column) for column in result.columns]
    return result


def select_reference(frame):
    """Select fixed windows without restarting or discarding preceding state."""
    return _select_windows(frame, WINDOWS)


def select_startup_reference(frame):
    """Select the startup window ``STARTUP_WINDOW`` for the fast core witness."""
    return _select_windows(frame, (STARTUP_WINDOW,))


def read_reference_window_csv(case, window=STARTUP_WINDOW):
    """Return the header and exact committed CSV rows inside ``[start, end)``."""
    start, end = pd.Timestamp(window[0]), pd.Timestamp(window[1])
    lines = (REFERENCE_DIR / f"{case}.csv").read_text(encoding="utf-8").splitlines()
    if not lines or lines[0].split(",", 2)[:2] != ["grid", "datetime"]:
        raise ValueError(f"Unexpected header in committed reference {case}.csv")
    header, body = lines[0], lines[1:]
    selected = [
        line for line in body if start <= pd.Timestamp(line.split(",", 2)[1]) < end
    ]
    if not selected:
        raise ValueError(f"No committed rows in {case}.csv for window {window}")
    return "\n".join([header, *selected]) + "\n"


def reference_csv(frame):
    """Use the same seven-significant-figure precision as the OHM reference."""
    return frame.to_csv(float_format="%.7g", lineterminator="\n")
