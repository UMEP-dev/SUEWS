"""Shared coupled physics anchors for contract tests (gh#2098, gh#2099, gh#2100).

Six self-contained configuration builders cover the coupled physics regimes that
the bundled ``sample_config.yml`` leaves inactive (``evetr.sfr = bsoil.sfr = 0``,
default OHM/NARP, disabled snow/STEBBS/CO2/irrigation/DLS):

1. ``all_surfaces_baseline``: all seven land-cover surfaces active (``sfr > 0``,
   summing to 1.0) under baseline NARP + OHM + modelled LAI/soil/water-use.
2. ``co2_methods``: active anthropogenic and biogenic CO2 (``emissions = 12``,
   non-zero ``emission_co2_point_source``) with all seven surfaces active.
3. ``snow_cold``: active snow processes (``snow_use = 1``) with finite cold-site
   snow parameters and initial snow states.
4. ``spartacus_ehc_stebbs``: coupled SPARTACUS radiation (``net_radiation = 1003``),
   explicit heat conduction (``storage_heat = 5``), and STEBBS (``stebbs.enabled``).
5. ``observed_and_dyohm``: DyOHM storage heat (``storage_heat = 6``) coupled with
   observed soil moisture (canonical ``soil_observation`` + ``soil_field_capacity``),
   observed LAI, observed water use, and observed incoming longwave radiation.
6. ``irrigation_and_dls``: active modelled irrigation (``water_use = 0`` with
   ``daywat = 1.0``, Northern Hemisphere window ``ie_start = 100 .. ie_end = 280``
   covering 183 eligible days in the 2012 simulation window, active ``h_maintain``
   ponding depth, non-zero ``faut``, irrigation coefficients, and surface
   fractions) coupled with daylight-saving output timestamps
   (``timestamp_reference = "daylight"``).

Every builder returns a fresh deep copy loaded through ``importlib.resources``
(``importlib.resources.files("supy")``), so callers may mutate freely without
touching shared state or relying on ``<module>.__file__``.
"""

from __future__ import annotations

from collections.abc import Callable
import copy
from dataclasses import dataclass
from functools import lru_cache
from importlib.resources import files
from pathlib import Path
from typing import Any

import yaml

from supy.data_model.core import SUEWSConfig

ALL_SURFACES: tuple[str, ...] = (
    "paved",
    "bldgs",
    "evetr",
    "dectr",
    "grass",
    "bsoil",
    "water",
)

NON_WATER_SURFACES: tuple[str, ...] = (
    "paved",
    "bldgs",
    "evetr",
    "dectr",
    "grass",
    "bsoil",
)

ALL_SURFACES_SFR: dict[str, float] = {
    "paved": 0.40,
    "bldgs": 0.38,
    "evetr": 0.03,
    "dectr": 0.02,
    "grass": 0.03,
    "bsoil": 0.02,
    "water": 0.12,
}

ANCHOR_NAMES: tuple[str, ...] = (
    "all_surfaces_baseline",
    "co2_methods",
    "snow_cold",
    "spartacus_ehc_stebbs",
    "observed_and_dyohm",
    "irrigation_and_dls",
)


@dataclass(frozen=True)
class AnchorSpec:
    """Metadata and builder for one coupled physics anchor configuration."""

    name: str
    description: str
    builder: Callable[[], dict[str, Any]]
    expected_physics: dict[str, int | bool]
    active_subtrees: tuple[str, ...]


@lru_cache(maxsize=1)
def _sample_config_base() -> dict[str, Any]:
    """Read and parse the packaged sample configuration once per process."""
    text = (
        files("supy")
        .joinpath("sample_data", "sample_config.yml")
        .read_text(encoding="utf-8")
    )
    return yaml.safe_load(text)


def _evening_irrigation_profile() -> dict[str, float]:
    """Return a 24-hour irrigation profile concentrated in hours 19..23 (sum=1.0)."""
    return {str(hour): (0.2 if 19 <= hour <= 23 else 0.0) for hour in range(1, 25)}


def _build_all_surfaces_baseline() -> dict[str, Any]:
    """Build the 7-surface active baseline anchor (sum of ``sfr`` == 1.0)."""
    data = copy.deepcopy(_sample_config_base())
    data["name"] = "anchor_all_surfaces_baseline"
    land_cover = data["sites"][0]["properties"]["land_cover"]
    for surf, frac in ALL_SURFACES_SFR.items():
        land_cover[surf]["sfr"] = {"value": frac}
    return data


def _build_co2_methods() -> dict[str, Any]:
    """Build the active anthropogenic + biogenic CO2 anchor (``emissions = 12``)."""
    data = _build_all_surfaces_baseline()
    data["name"] = "anchor_co2_methods"
    data["model"]["physics"]["emissions"] = {"value": 12}
    co2 = data["sites"][0]["properties"]["anthropogenic_emissions"]["co2"]
    # Non-zero active point source verifies active wiring rather than a zero default.
    co2["emission_co2_point_source"] = {"value": 2.75}
    return data


def _build_snow_cold() -> dict[str, Any]:
    """Build the cold-site active snow anchor (``snow_use = 1``)."""
    data = _build_all_surfaces_baseline()
    data["name"] = "anchor_snow_cold"
    data["model"]["physics"]["snow"] = "enabled"

    site = data["sites"][0]
    props = site["properties"]
    snow = props["snow"]
    snow["snow_depth_limit_building"] = {"value": 50.0}
    snow["snow_depth_limit_paved"] = {"value": 50.0}
    zero_24hr = {str(hour): 0.0 for hour in range(1, 25)}
    snow["snow_profile_24hr"] = {
        "working_day": dict(zero_24hr),
        "holiday": dict(zero_24hr),
    }

    land_cover = props["land_cover"]
    init_states = site["initial_states"]
    init_states["snowalb"] = {"value": 0.65}
    for surf in NON_WATER_SURFACES:
        land_cover[surf]["snowpack_limit"] = {"value": 100.0}
        init_states[surf]["snowfrac"] = {"value": 0.5}
        init_states[surf]["snowpack"] = {"value": 20.0}
        init_states[surf]["snowdens"] = {"value": 200.0}
        init_states[surf]["snowwater"] = {"value": 1.0}
        init_states[surf]["icefrac"] = {"value": 0.1}

    return data


def _build_spartacus_ehc_stebbs() -> dict[str, Any]:
    """Build the coupled SPARTACUS + EHC + STEBBS anchor."""
    data = _build_all_surfaces_baseline()
    data["name"] = "anchor_spartacus_ehc_stebbs"
    physics = data["model"]["physics"]
    physics["net_radiation"] = {"value": 1003}
    physics["storage_heat"] = {"value": 5}
    physics["stebbs"]["enabled"] = True
    physics["stebbs"]["parameter_source"] = "default"

    # Match top-layer vegetation geometry to tree heights (13.1 m < 15.0 m layer base).
    vl = data["sites"][0]["properties"]["vertical_layers"]
    vl["veg_frac"]["value"][2] = 0.0
    vl["veg_scale"]["value"][2] = 0.0
    vl["veg_ext"] = {"value": [0.5, 0.5, 0.0]}
    return data


def _build_observed_and_dyohm() -> dict[str, Any]:
    """Build the coupled DyOHM + observed soil/LAI/water-use/Ldown anchor."""
    data = _build_all_surfaces_baseline()
    data["name"] = "anchor_observed_and_dyohm"
    physics = data["model"]["physics"]
    physics["storage_heat"] = {"value": 6}
    physics["soil_moisture"] = "observed"
    physics["leaf_area_index"] = "observed"
    physics["water_use"] = "observed"
    physics["net_radiation"] = {"narp": {"ldown": "observed"}}
    physics["frontal_area_index"] = "observed"

    props = data["sites"][0]["properties"]
    props["soil_observation"] = {
        "layer_top": 0.0,
        "layer_depth": 0.2,
        "soil_not_rocks": 0.9,
    }
    land_cover = props["land_cover"]
    for surf in NON_WATER_SURFACES:
        # 1000 * 0.6 * 0.2 = 120 kg m-2 at field capacity >= 80 kg m-2 capacity.
        land_cover[surf]["soil_depth"] = {"value": 200.0}
        land_cover[surf]["soil_store_capacity"] = {"value": 80.0}
        land_cover[surf]["soil_field_capacity"] = {"value": 0.6}
    return data


def _build_irrigation_and_dls() -> dict[str, Any]:
    """Build the coupled modelled-irrigation + daylight-saving anchor."""
    data = _build_all_surfaces_baseline()
    data["name"] = "anchor_irrigation_and_dls"
    data["model"]["physics"]["water_use"] = "modelled"
    data["model"]["control"]["output"]["timestamp_reference"] = "daylight"

    props = data["sites"][0]["properties"]
    # Non-zero standard-time UTC offset avoids the UTC-0 daylight warning.
    props["timezone"] = {"value": 1}
    props["anthropogenic_emissions"]["startdls"] = {"value": 86.0}
    props["anthropogenic_emissions"]["enddls"] = {"value": 303.0}

    irrigation = props["irrigation"]
    irrigation["h_maintain"] = {"value": 10.0}
    irrigation["faut"] = {"value": 0.2}
    irrigation["ie_start"] = {"value": 100.0}
    irrigation["ie_end"] = {"value": 280.0}
    irrigation["internalwateruse_h"] = {"value": 0.1}
    for day in (
        "monday",
        "tuesday",
        "wednesday",
        "thursday",
        "friday",
        "saturday",
        "sunday",
    ):
        irrigation["daywat"][day] = 1.0
        irrigation["daywatper"][day] = 1.0
    evening = _evening_irrigation_profile()
    for profile_key in ("wuprofa_24hr", "wuprofm_24hr"):
        irrigation[profile_key]["working_day"] = dict(evening)
        irrigation[profile_key]["holiday"] = dict(evening)

    land_cover = props["land_cover"]
    land_cover["evetr"]["ie_a"] = {"value": 3.674}
    land_cover["evetr"]["ie_m"] = {"value": 1.102}
    land_cover["evetr"]["irrigation_fraction"] = {"value": 0.5}
    land_cover["dectr"]["irrigation_fraction"] = {"value": 0.5}
    land_cover["grass"]["irrigation_fraction"] = {"value": 1.0}
    return data


ANCHOR_SPECS: dict[str, AnchorSpec] = {
    "all_surfaces_baseline": AnchorSpec(
        name="all_surfaces_baseline",
        description="All seven land-cover surfaces active (sfr > 0, sum = 1.0) with baseline NARP/OHM physics.",
        builder=_build_all_surfaces_baseline,
        expected_physics={
            "net_radiation": 3,
            "storage_heat": 1,
            "emissions": 2,
            "snow_use": 0,
            "soil_moisture": 0,
            "water_use": 0,
            "laimethod": 1,
            "stebbs.enabled": False,
        },
        active_subtrees=(
            "model.control",
            "properties.site_scalars",
            "properties.lumps",
            "properties.conductance",
            "properties.land_cover",
            "initial_states",
        ),
    ),
    "co2_methods": AnchorSpec(
        name="co2_methods",
        description="Active anthropogenic and biogenic CO2 (emissions=12) with non-zero point source.",
        builder=_build_co2_methods,
        expected_physics={
            "net_radiation": 3,
            "storage_heat": 1,
            "emissions": 12,
            "snow_use": 0,
            "soil_moisture": 0,
            "water_use": 0,
            "laimethod": 1,
            "stebbs.enabled": False,
        },
        active_subtrees=(
            "properties.anthropogenic_emissions.co2",
            "properties.land_cover.bioco2",
        ),
    ),
    "snow_cold": AnchorSpec(
        name="snow_cold",
        description="Active snow module (snow_use=1) with finite cold-site snow parameters and initial snow states.",
        builder=_build_snow_cold,
        expected_physics={
            "net_radiation": 3,
            "storage_heat": 1,
            "emissions": 2,
            "snow_use": 1,
            "soil_moisture": 0,
            "water_use": 0,
            "laimethod": 1,
            "stebbs.enabled": False,
        },
        active_subtrees=(
            "properties.snow",
            "properties.land_cover.snowpack_limit",
            "initial_states.snow",
        ),
    ),
    "spartacus_ehc_stebbs": AnchorSpec(
        name="spartacus_ehc_stebbs",
        description="Coupled SPARTACUS (1003), EHC (5), and STEBBS (enabled, default parameter source).",
        builder=_build_spartacus_ehc_stebbs,
        expected_physics={
            "net_radiation": 1003,
            "storage_heat": 5,
            "emissions": 2,
            "snow_use": 0,
            "soil_moisture": 0,
            "water_use": 0,
            "laimethod": 1,
            "stebbs.enabled": True,
            "stebbs.parameters": 1,
        },
        active_subtrees=(
            "properties.spartacus",
            "properties.vertical_layers",
            "properties.stebbs",
            "properties.building_archetype",
            "properties.land_cover.thermal_layers",
        ),
    ),
    "observed_and_dyohm": AnchorSpec(
        name="observed_and_dyohm",
        description="DyOHM storage heat (6) with observed soil moisture, LAI, water use, and Ldown.",
        builder=_build_observed_and_dyohm,
        expected_physics={
            "net_radiation": 1,
            "storage_heat": 6,
            "emissions": 2,
            "snow_use": 0,
            "soil_moisture": 1,
            "water_use": 1,
            "laimethod": 0,
            "stebbs.enabled": False,
        },
        active_subtrees=(
            "properties.soil_observation",
            "properties.land_cover.soil_field_capacity",
            "properties.land_cover.thermal_layers",
            "properties.land_cover.ohm_coef",
        ),
    ),
    "irrigation_and_dls": AnchorSpec(
        name="irrigation_and_dls",
        description="Active modelled irrigation with daylight-saving output clock and DLS window.",
        builder=_build_irrigation_and_dls,
        expected_physics={
            "net_radiation": 3,
            "storage_heat": 1,
            "emissions": 2,
            "snow_use": 0,
            "soil_moisture": 0,
            "water_use": 0,
            "laimethod": 1,
            "stebbs.enabled": False,
        },
        active_subtrees=(
            "properties.irrigation",
            "properties.anthropogenic_emissions.dls",
            "properties.land_cover.irrigation",
        ),
    ),
}


def build_anchor_dict(name: str) -> dict[str, Any]:
    """Return a fresh mutable raw-YAML mapping for the named physics anchor."""
    if name not in ANCHOR_SPECS:
        valid = ", ".join(ANCHOR_NAMES)
        raise KeyError(f"Unknown physics anchor {name!r}; expected one of: {valid}")
    return ANCHOR_SPECS[name].builder()


def build_all_anchor_dicts() -> dict[str, dict[str, Any]]:
    """Return fresh mutable raw-YAML mappings for all six coupled physics anchors."""
    return {name: build_anchor_dict(name) for name in ANCHOR_NAMES}


def write_anchor_yaml(name: str, path: Path) -> Path:
    """Serialise the named anchor configuration to *path* as UTF-8 YAML."""
    data = build_anchor_dict(name)
    dumper = getattr(yaml, "CSafeDumper", yaml.SafeDumper)
    path.write_text(
        yaml.dump(data, sort_keys=False, Dumper=dumper),
        encoding="utf-8",
    )
    return path


def load_anchor_config(name: str, tmp_path: Path | None = None) -> SUEWSConfig:
    """Load the named anchor via ``SUEWSConfig.from_yaml`` (when *tmp_path* given) or ``from_dict``."""
    if tmp_path is not None:
        yaml_path = write_anchor_yaml(name, tmp_path / f"{name}.yml")
        return SUEWSConfig.from_yaml(str(yaml_path))
    return SUEWSConfig.from_dict(build_anchor_dict(name))
