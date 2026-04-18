"""Central registry of deprecated field name mappings.

Each dict maps old (fused) field names to new (snake_case) names.
Used by ``@model_validator(mode='before')`` on affected Pydantic models
and by the Phase A validation pipeline.
"""

import warnings
from typing import Dict


# -- ModelPhysics (model.py) -------------------------------------------------

MODELPHYSICS_RENAMES: Dict[str, str] = {
    "netradiationmethod": "net_radiation_method",
    "emissionsmethod": "emissions_method",
    "storageheatmethod": "storage_heat_method",
    "ohmincqf": "ohm_inc_qf",
    "roughlenmommethod": "roughness_length_momentum_method",
    "roughlenheatmethod": "roughness_length_heat_method",
    "stabilitymethod": "stability_method",
    "smdmethod": "smd_method",
    "waterusemethod": "water_use_method",
    "rslmethod": "rsl_method",
    "faimethod": "fai_method",
    "rsllevel": "rsl_level",
    "gsmodel": "gs_model",
    "snowuse": "snow_use",
    "stebbsmethod": "stebbs_method",
    "rcmethod": "rc_method",
}

# -- SurfaceProperties (surface.py) ------------------------------------------

SURFACEPROPERTIES_RENAMES: Dict[str, str] = {
    "soildepth": "soil_depth",
    "soilstorecap": "soil_store_capacity",
    "statelimit": "state_limit",
    "wetthresh": "wet_threshold",
    "sathydraulicconduct": "saturated_hydraulic_conductivity",
    "soildensity": "soil_density",
    "storedrainprm": "storage_drain_params",
    "snowpacklimit": "snowpack_limit",
    "irrfrac": "irrigation_fraction",
    "ohm_threshsw": "ohm_threshold_summer_winter",
    "ohm_threshwd": "ohm_threshold_wet_dry",
}

# -- LAIParams (site.py) -----------------------------------------------------

LAIPARAMS_RENAMES: Dict[str, str] = {
    "baset": "base_temperature",
    "gddfull": "gdd_full",
    "basete": "base_temperature_senescence",
    "sddfull": "sdd_full",
    "laimin": "lai_min",
    "laimax": "lai_max",
    "laipower": "lai_power",
    "laitype": "lai_type",
}

# -- VegetatedSurfaceProperties (site.py) ------------------------------------

VEGETATEDSURFACEPROPERTIES_RENAMES: Dict[str, str] = {
    "maxconductance": "max_conductance",
    "beta_bioco2": "beta_bio_co2",
    "alpha_bioco2": "alpha_bio_co2",
    "theta_bioco2": "theta_bio_co2",
}

# -- EvetrProperties (site.py) -----------------------------------------------

EVETRPROPERTIES_RENAMES: Dict[str, str] = {
    "faievetree": "fai_evergreen_tree",
    "evetreeh": "evergreen_tree_height",
}

# -- DectrProperties (site.py) -----------------------------------------------

DECTRPROPERTIES_RENAMES: Dict[str, str] = {
    "faidectree": "fai_deciduous_tree",
    "dectreeh": "deciduous_tree_height",
    "pormin_dec": "porosity_min_deciduous",
    "pormax_dec": "porosity_max_deciduous",
    "capmax_dec": "capacity_max_deciduous",
    "capmin_dec": "capacity_min_deciduous",
}

# -- SnowParams (site.py) ----------------------------------------------------

SNOWPARAMS_RENAMES: Dict[str, str] = {
    "crwmax": "water_holding_capacity_max",
    "crwmin": "water_holding_capacity_min",
    "preciplimit": "precip_limit",
    "preciplimitalb": "precip_limit_albedo",
    "snowalbmax": "snow_albedo_max",
    "snowalbmin": "snow_albedo_min",
    "snowdensmin": "snow_density_min",
    "snowdensmax": "snow_density_max",
    "snowlimbldg": "snow_limit_building",
    "snowlimpaved": "snow_limit_paved",
    "tempmeltfact": "temp_melt_factor",
    "radmeltfact": "rad_melt_factor",
}

# -- Combined -----------------------------------------------------------------

ALL_FIELD_RENAMES: Dict[str, str] = {
    **MODELPHYSICS_RENAMES,
    **SURFACEPROPERTIES_RENAMES,
    **LAIPARAMS_RENAMES,
    **VEGETATEDSURFACEPROPERTIES_RENAMES,
    **EVETRPROPERTIES_RENAMES,
    **DECTRPROPERTIES_RENAMES,
    **SNOWPARAMS_RENAMES,
}

# Reverse mapping: new_name -> old_name (for serialisation to Fortran bridge)
_REVERSE_RENAMES: Dict[str, str] = {v: k for k, v in ALL_FIELD_RENAMES.items()}


def apply_field_renames(
    values: dict, renames: Dict[str, str], class_name: str
) -> dict:
    """Replace deprecated field names in *values* with their new equivalents.

    Called from ``@model_validator(mode='before')`` on each affected model.

    Parameters
    ----------
    values : dict
        Raw input dict (YAML or kwargs).
    renames : dict
        ``{old_name: new_name}`` mapping for this model class.
    class_name : str
        Model class name, used in warning messages.

    Returns
    -------
    dict
        Updated dict with old keys replaced by new keys.

    Raises
    ------
    ValueError
        If both old and new names are present for the same field.
    """
    for old_name, new_name in renames.items():
        if old_name in values:
            if new_name in values:
                raise ValueError(
                    f"{class_name}: both '{old_name}' (deprecated) and "
                    f"'{new_name}' are present. Use only '{new_name}'."
                )
            values[new_name] = values.pop(old_name)
            warnings.warn(
                f"{class_name}: field '{old_name}' is deprecated, "
                f"use '{new_name}' instead.",
                DeprecationWarning,
                stacklevel=4,
            )
    return values


def reverse_field_renames(data: dict) -> dict:
    """Recursively replace new field names with old ones for serialisation.

    Used before passing ``model_dump()`` output to the Rust/Fortran bridge,
    which expects the legacy (fused) field names.
    """
    result = {}
    for key, value in data.items():
        out_key = _REVERSE_RENAMES.get(key, key)
        if isinstance(value, dict):
            result[out_key] = reverse_field_renames(value)
        elif isinstance(value, list):
            result[out_key] = [
                reverse_field_renames(item) if isinstance(item, dict) else item
                for item in value
            ]
        else:
            result[out_key] = value
    return result
