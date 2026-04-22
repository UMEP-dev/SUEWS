"""Central registry of deprecated field name mappings.

Each dict maps old (fused) field names to new (snake_case) names.
Used by ``@model_validator(mode='before')`` on affected Pydantic models,
the Phase A validation pipeline, and raw-dict compatibility helpers.
"""

from __future__ import annotations

import warnings
from collections.abc import Mapping
from typing import Any, Dict


# -- ModelPhysics (model.py) -------------------------------------------------
#
# MODELPHYSICS_RENAMES maps legacy fused spellings DIRECTLY to the current
# final names (Category 1 fused -> Category 2+3 bare where applicable).
# This consolidation keeps ALL_FIELD_RENAMES a one-to-one map between
# legacy-fused keys and final values — critical for the Rust bridge's
# reverse lookup (Fortran state is still keyed by the fused spelling).
#
# MODELPHYSICS_SUFFIX_RENAMES maps the Category 1 intermediate (shipped
# in Schema 2026.5 as `net_radiation_method` etc.) to the final bare
# names. Consumed only by the Pydantic backward-compat shim on
# ``ModelPhysics`` so users who hand-wrote their YAMLs to the 2026.5
# snake_case-with-suffix shape still load with a DeprecationWarning.

MODELPHYSICS_RENAMES: Dict[str, str] = {
    # Fused -> Category 2+3 final (shadows both Cat 1 and Cat 2+3 for
    # users arriving from any pre-2026.5.dev2 schema in a single step).
    "netradiationmethod": "net_radiation",
    "emissionsmethod": "emissions",
    "storageheatmethod": "storage_heat",
    "roughlenmommethod": "roughness_length_momentum",
    "roughlenheatmethod": "roughness_length_heat",
    "stabilitymethod": "stability",
    "smdmethod": "soil_moisture_deficit",
    "waterusemethod": "water_use",
    "rslmethod": "roughness_sublayer",
    "faimethod": "frontal_area_index",
    "rsllevel": "roughness_sublayer_level",
    "gsmodel": "surface_conductance",
    "stebbsmethod": "stebbs",
    "rcmethod": "outer_cap_fraction",
    "setpointmethod": "setpoint",  # fused identifier missed by Category 1
    # Flags/enum choices not touched by Category 2+3 keep their Cat 1 target.
    "ohmincqf": "ohm_inc_qf",
    "snowuse": "snow_use",
}

# Category 1 intermediate (snake_case with redundant suffix) -> final bare.
# Applied in ``ModelPhysics._rename_physics_fields`` after
# ``MODELPHYSICS_RENAMES`` so YAMLs authored against Schema 2026.5 (15
# days of release window) keep loading with a deprecation warning.
# NOT spread into ``ALL_FIELD_RENAMES`` — doing so would introduce a
# second alias for the same final name and break the one-to-one
# reverse-mapping the Rust bridge depends on. Users running the Rust
# CLI (``suews run``) on a Schema 2026.5 YAML should migrate via
# ``suews-schema migrate --target-version 2026.5.dev2`` first.

MODELPHYSICS_SUFFIX_RENAMES: Dict[str, str] = {
    # Suffix drop (8) -- enum type (e.g. NetRadiationMethod) carries "method"
    "net_radiation_method": "net_radiation",
    "emissions_method": "emissions",
    "storage_heat_method": "storage_heat",
    "roughness_length_momentum_method": "roughness_length_momentum",
    "roughness_length_heat_method": "roughness_length_heat",
    "stability_method": "stability",
    "water_use_method": "water_use",
    "stebbs_method": "stebbs",
    # Expand opaque domain abbreviations (4)
    "smd_method": "soil_moisture_deficit",
    "rsl_method": "roughness_sublayer",
    "rsl_level": "roughness_sublayer_level",
    "fai_method": "frontal_area_index",
    # Expand + semantic rename (2)
    "rc_method": "outer_cap_fraction",
    "gs_model": "surface_conductance",
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
    "evetreeh": "height_evergreen_tree",
}

# -- DectrProperties (site.py) -----------------------------------------------

DECTRPROPERTIES_RENAMES: Dict[str, str] = {
    "faidectree": "fai_deciduous_tree",
    "dectreeh": "height_deciduous_tree",
    "pormin_dec": "porosity_min_deciduous",
    "pormax_dec": "porosity_max_deciduous",
    "capmax_dec": "capacity_max_deciduous",
    "capmin_dec": "capacity_min_deciduous",
}

# -- ArchetypeProperties (site.py) -------------------------------------------
#
# STEBBS keeps PascalCase (to match the Fortran-side STEBBS interface), but
# the exemplar in `.claude/rules/00-project-essentials.md` spells "External"
# out in full (`RoofExternalEmissivity`). These pairs bring the fused
# `ext` cluster into line with that exemplar (gh#1327).

ARCHETYPEPROPERTIES_RENAMES: Dict[str, str] = {
    "WallextThickness": "WallExternalThickness",
    "WallextEffectiveConductivity": "WallExternalEffectiveConductivity",
    "WallextDensity": "WallExternalDensity",
    "WallextCp": "WallExternalCp",
    "RoofextThickness": "RoofExternalThickness",
    "RoofextEffectiveConductivity": "RoofExternalEffectiveConductivity",
    "RoofextDensity": "RoofExternalDensity",
    "RoofextCp": "RoofExternalCp",
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
#
# ``ALL_FIELD_RENAMES`` is a one-to-one map from every legacy fused key to
# its current final name. ``MODELPHYSICS_SUFFIX_RENAMES`` is deliberately
# NOT spread here — it carries Cat 1 intermediate aliases that would
# introduce a second alias per final name, which the Rust bridge's
# reverse lookup (``ALL_FIELD_RENAMES`` inverted) cannot represent.

ALL_FIELD_RENAMES: Dict[str, str] = {
    **MODELPHYSICS_RENAMES,
    **SURFACEPROPERTIES_RENAMES,
    **LAIPARAMS_RENAMES,
    **VEGETATEDSURFACEPROPERTIES_RENAMES,
    **EVETRPROPERTIES_RENAMES,
    **DECTRPROPERTIES_RENAMES,
    **ARCHETYPEPROPERTIES_RENAMES,
    **SNOWPARAMS_RENAMES,
}

# Reverse mapping: new_name -> old_name (for serialisation to Fortran bridge).
# The Fortran bridge still indexes state by fused spellings, so `_REVERSE_*`
# maps each final public name back to its original fused form.
_REVERSE_RENAMES: Dict[str, str] = {v: k for k, v in ALL_FIELD_RENAMES.items()}

# For raw-YAML preflight checks that bypass Pydantic. Both the fused form
# (`netradiationmethod`) and the Category 1 intermediate form
# (`net_radiation_method`) must resolve to the final bare name, so the
# combined mapping unions MODELPHYSICS_RENAMES (fused -> final) with
# MODELPHYSICS_SUFFIX_RENAMES (intermediate -> final). The Cat 1
# intermediate keys are dict-disjoint from the fused keys, so the
# ``|`` union is safe and order-independent.
_MODELPHYSICS_ALL_RENAMES: Dict[str, str] = {
    # Order matters for the reverse map below: spreading SUFFIX first and
    # RENAMES last means inverting the dict prefers the fused legacy
    # (``netradiationmethod``) over the Cat 1 intermediate
    # (``net_radiation_method``) when both claim the same final target.
    # The Fortran bridge's state is keyed by the fused form, so the
    # reverse map must point at the fused legacy.
    **MODELPHYSICS_SUFFIX_RENAMES,
    **MODELPHYSICS_RENAMES,
}
_REVERSE_MODELPHYSICS_RENAMES: Dict[str, str] = {
    v: k for k, v in _MODELPHYSICS_ALL_RENAMES.items()
}
_MISSING = object()


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


def read_physics_key(physics: dict, new_name: str, default: Any = None):
    """Read a physics key from raw YAML, accepting either the new name or its legacy alias.

    Public-mode gates and other preflight checks inspect the raw user YAML
    before Phase A has renamed keys. The Pydantic backward-compat shim
    accepts both spellings, so these gates must as well, or users on either
    spelling can silently bypass them.

    Unwraps RefValue-style ``{"value": X}`` wrappers. Returns ``default``
    when neither spelling is present.
    """
    entry = read_renamed_key(
        physics,
        new_name,
        renames=_MODELPHYSICS_ALL_RENAMES,
        reverse_renames=_REVERSE_MODELPHYSICS_RENAMES,
        default=default,
    )
    if isinstance(entry, dict) and "value" in entry:
        return entry["value"]
    return entry


def read_renamed_key(
    data: dict,
    name: str,
    *,
    renames: Dict[str, str] = ALL_FIELD_RENAMES,
    reverse_renames: Dict[str, str] | None = None,
    default: Any = None,
):
    """Read a renamed key from a raw dict, accepting both spellings.

    Parameters
    ----------
    data : dict
        Raw mapping to inspect.
    name : str
        Preferred key name. Can be either the new name or the legacy name.
    renames : dict, optional
        ``{old_name: new_name}`` rename mapping.
    reverse_renames : dict, optional
        Precomputed ``{new_name: old_name}`` mapping. If omitted, it is built
        from ``renames``.
    default : Any, optional
        Value returned when neither spelling is present.
    """
    if not isinstance(data, Mapping):
        return default

    reverse = reverse_renames or (
        _REVERSE_RENAMES if renames is ALL_FIELD_RENAMES else {v: k for k, v in renames.items()}
    )

    entry = data.get(name, _MISSING)
    if entry is not _MISSING:
        return entry

    legacy_name = reverse.get(name)
    if legacy_name is not None and legacy_name in data:
        return data[legacy_name]

    renamed_name = renames.get(name)
    if renamed_name is not None and renamed_name in data:
        return data[renamed_name]

    # A single ``name`` may be the target of more than one legacy alias
    # (e.g. ``net_radiation`` is reached from both fused ``netradiationmethod``
    # and Cat 1 intermediate ``net_radiation_method``). The one-to-one
    # reverse map only captures one of those; walk the full rename dict
    # to catch any other legacy alias still sitting in ``data``.
    for old_key, new_key in renames.items():
        if new_key == name and old_key in data:
            return data[old_key]

    return default


def has_renamed_key(
    data: dict,
    name: str,
    *,
    renames: Dict[str, str] = ALL_FIELD_RENAMES,
    reverse_renames: Dict[str, str] | None = None,
) -> bool:
    """Return True if either the preferred or legacy spelling is present."""
    return read_renamed_key(
        data,
        name,
        renames=renames,
        reverse_renames=reverse_renames,
        default=_MISSING,
    ) is not _MISSING


def rename_keys_recursive(
    data: Any,
    renames: Dict[str, str] = ALL_FIELD_RENAMES,
    *,
    reverse_renames: Dict[str, str] | None = None,
    path: str = "",
) -> Any:
    """Recursively rewrite legacy keys to their preferred names.

    Raises
    ------
    ValueError
        If a dict contains both the legacy and preferred spellings for the
        same logical field.
    """
    reverse = reverse_renames or (
        _REVERSE_RENAMES if renames is ALL_FIELD_RENAMES else {v: k for k, v in renames.items()}
    )

    if isinstance(data, dict):
        result = {}
        source_keys: Dict[str, str] = {}
        for key, value in data.items():
            out_key = renames.get(key, key)
            if out_key in source_keys and source_keys[out_key] != key:
                prev_key = source_keys[out_key]
                location = path or "<root>"
                legacy_key = None
                if renames.get(prev_key) == out_key and key == out_key:
                    legacy_key = prev_key
                elif renames.get(key) == out_key and prev_key == out_key:
                    legacy_key = key

                if legacy_key is not None:
                    raise ValueError(
                        f"Both '{legacy_key}' (deprecated) and '{out_key}' are present at {location}. "
                        f"Use only '{out_key}'."
                    )

                raise ValueError(
                    f"Conflicting keys '{prev_key}' and '{key}' both resolve to '{out_key}' at {location}."
                )

            child_path = f"{path}.{out_key}" if path else out_key
            result[out_key] = rename_keys_recursive(
                value,
                renames,
                reverse_renames=reverse,
                path=child_path,
            )
            source_keys[out_key] = key
        return result

    if isinstance(data, list):
        return [
            rename_keys_recursive(
                item,
                renames,
                reverse_renames=reverse,
                path=f"{path}[{idx}]",
            )
            for idx, item in enumerate(data)
        ]

    return data


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
