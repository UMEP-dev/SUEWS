from typing import (
    Dict,
    List,
    Optional,
    Union,
    Literal,
    Tuple,
    Type,
    Generic,
    TypeVar,
    Any,
)
from pydantic import (
    ConfigDict,
    BaseModel,
    Field,
    model_validator,
    field_validator,
    PrivateAttr,
    conlist,
    ValidationError,
)
import numpy as np
import pandas as pd
import yaml
import ast

from .model import Model
from .site import Site, SiteProperties, InitialStates, LandCover
from .type import SurfaceType

from datetime import datetime
from timezonefinder import TimezoneFinder
import pytz

from .._env import logger_supy

try:
    from ..validation import (
        enhanced_from_yaml_validation,
        enhanced_to_df_state_validation,
    )

    _validation_available = True
except ImportError:
    try:
        from .validation_controller import validate_suews_config_conditional

        def enhanced_from_yaml_validation(config_data, strict=True):
            result = validate_suews_config_conditional(
                config_data, strict=False, verbose=True
            )
            if result["errors"] and strict:
                error_msg = f"SUEWS Configuration Validation Failed: {len(result['errors'])} errors\n"
                error_msg += "\n".join(f"  - {err}" for err in result["errors"])
                raise ValueError(error_msg)
            return result

        def enhanced_to_df_state_validation(config_data, strict=False):
            result = validate_suews_config_conditional(
                config_data, strict=False, verbose=False
            )
            if result["errors"] and strict:
                error_msg = (
                    f"Configuration validation found {len(result['errors'])} issues\n"
                )
                error_msg += "\n".join(f"  - {err}" for err in result["errors"])
                raise ValueError(error_msg)
            return result

        _validation_available = True
    except ImportError:
        _validation_available = False
        enhanced_from_yaml_validation = None
        enhanced_to_df_state_validation = None
import os
import warnings


class SeasonCheck(BaseModel):
    start_date: str  # Expected format: YYYY-MM-DD
    lat: float

    def get_season(self) -> str:
        try:
            start = datetime.strptime(self.start_date, "%Y-%m-%d").timetuple().tm_yday
        except ValueError:
            raise ValueError("start_date must be in YYYY-MM-DD format")

        abs_lat = abs(self.lat)

        if abs_lat <= 10:
            return "equatorial"
        if 10 < abs_lat < 23.5:
            return "tropical"

        if self.lat >= 0:  # Northern Hemisphere
            if 150 < start < 250:
                return "summer"
            elif 60 < start <= 150:
                return "spring"
            elif 250 <= start < 335:
                return "fall"
            else:
                return "winter"
        else:  # Southern Hemisphere
            if 150 < start < 250:
                return "winter"
            elif 60 < start <= 150:
                return "fall"
            elif 250 <= start < 335:
                return "spring"
            else:
                return "summer"


class DLSCheck(BaseModel):
    lat: float
    lng: float
    year: int
    startdls: Optional[int] = None
    enddls: Optional[int] = None

    def compute_dst_transitions(self):
        tf = TimezoneFinder()
        tz_name = tf.timezone_at(lat=self.lat, lng=self.lng)

        if not tz_name:
            logger_supy.debug(
                f"[DLS] Cannot determine timezone for lat={self.lat}, lng={self.lng}"
            )
            return None, None, None

        logger_supy.debug(f"[DLS] Timezone identified as '{tz_name}'")
        tz = pytz.timezone(tz_name)

        def find_transition(month: int) -> Optional[int]:
            try:
                prev_dt = tz.localize(datetime(self.year, month, 1, 12), is_dst=None)
                prev_offset = prev_dt.utcoffset()
                for day in range(2, 32):
                    try:
                        curr_dt = tz.localize(
                            datetime(self.year, month, day, 12), is_dst=None
                        )
                        curr_offset = curr_dt.utcoffset()
                        if curr_offset != prev_offset:
                            return curr_dt.timetuple().tm_yday
                        prev_offset = curr_offset
                    except Exception:
                        continue
                return None
            except Exception:
                return None

        # Get standard UTC offset (in winter)
        try:
            std_dt = tz.localize(datetime(self.year, 1, 15), is_dst=False)
            utc_offset_hours = int(std_dt.utcoffset().total_seconds() / 3600)
            logger_supy.debug(f"[DLS] UTC offset in standard time: {utc_offset_hours}")
        except Exception as e:
            logger_supy.debug(f"[DLS] Failed to compute UTC offset: {e}")
            utc_offset_hours = None

        # Determine DST start and end days
        if self.lat >= 0:  # Northern Hemisphere
            start = find_transition(3) or find_transition(4)
            end = find_transition(10) or find_transition(11)
        else:  # Southern Hemisphere
            start = find_transition(9) or find_transition(10)
            end = find_transition(3) or find_transition(4)

        return start, end, utc_offset_hours


def precheck_printing(data: dict) -> dict:
    logger_supy.info("Running basic precheck...")
    return data


def precheck_start_end_date(data: dict) -> Tuple[dict, int, str, str]:
    control = data.get("model", {}).get("control", {})

    start_date = control.get("start_time")
    end_date = control.get("end_time")

    if not isinstance(start_date, str) or "-" not in start_date:
        raise ValueError(
            "Missing or invalid 'start_time' in model.control — must be in 'YYYY-MM-DD' format."
        )

    if not isinstance(end_date, str) or "-" not in end_date:
        raise ValueError(
            "Missing or invalid 'end_time' in model.control — must be in 'YYYY-MM-DD' format."
        )

    try:
        model_year = int(start_date.split("-")[0])
    except Exception:
        raise ValueError(
            "Could not extract model year from 'start_time'. Ensure it is in 'YYYY-MM-DD' format."
        )

    return data, model_year, start_date, end_date


def precheck_model_physics_params(data: dict) -> dict:
    physics = data.get("model", {}).get("physics", {})

    if not physics:
        logger_supy.debug("Skipping physics param check — physics is empty.")
        return data

    required = [
        "netradiationmethod",
        "emissionsmethod",
        "storageheatmethod",
        "ohmincqf",
        "roughlenmommethod",
        "roughlenheatmethod",
        "stabilitymethod",
        "smdmethod",
        "waterusemethod",
        "rslmethod",
        "faimethod",
        "rsllevel",
        "snowuse",
        "stebbsmethod",
    ]

    missing = [k for k in required if k not in physics]
    if missing:
        raise ValueError(f"[model.physics] Missing required params: {missing}")

    empty = [k for k in required if physics.get(k, {}).get("value") in ("", None)]
    if empty:
        raise ValueError(f"[model.physics] Empty or null values for: {empty}")

    logger_supy.debug("All model.physics required params present and non-empty.")
    return data


def precheck_model_options_constraints(data: dict) -> dict:
    physics = data.get("model", {}).get("physics", {})

    diag = physics.get("rslmethod", {}).get("value")
    stability = physics.get("stabilitymethod", {}).get("value")

    if diag == 2 and stability != 3:
        raise ValueError(
            "[model.physics] If rslmethod == 2, stabilitymethod must be 3."
        )

    logger_supy.debug("rslmethod-stabilitymethod constraint passed.")
    return data


def precheck_replace_empty_strings_with_none(data: dict) -> dict:
    ignore_keys = {"control", "physics"}

    def recurse(obj: Any, path=()):
        if isinstance(obj, dict):
            new = {}
            for k, v in obj.items():
                sub_path = path + (k,)
                if v == "" and not (
                    len(sub_path) >= 2
                    and sub_path[0] == "model"
                    and sub_path[1] in ignore_keys
                ):
                    new[k] = None
                else:
                    new[k] = recurse(v, sub_path)
            return new
        elif isinstance(obj, list):
            return [None if item == "" else recurse(item, path) for item in obj]
        else:
            return obj

    cleaned = recurse(data)
    logger_supy.info(
        "Empty strings replaced with None (except model.control and model.physics)."
    )
    return cleaned


def precheck_site_season_adjustments(
    data: dict, start_date: str, model_year: int
) -> dict:
    cleaned_sites = []

    for i, site in enumerate(data.get("sites", [])):
        if isinstance(site, BaseModel):
            site = site.model_dump(mode="python")

        props = site.get("properties", {})
        initial_states = site.get("initial_states", {})

        # --------------------
        # 1. Determine season
        # --------------------
        lat_entry = props.get("lat", {})
        lat = lat_entry.get("value") if isinstance(lat_entry, dict) else lat_entry
        lng = props.get("lng", {}).get("value")
        season = None

        try:
            if lat is not None:  # <- Placeholder: consider cases where lat is None
                season = SeasonCheck(start_date=start_date, lat=lat).get_season()
                logger_supy.debug(f"[site #{i}] Season detected: {season}")

                # If equatorial / tropical / summer → nullify snowalb
                if (
                    season in ("summer", "tropical", "equatorial")
                    and "snowalb" in initial_states
                ):
                    if isinstance(initial_states["snowalb"], dict):
                        initial_states["snowalb"]["value"] = None
                        logger_supy.info(f"[site #{i}] Set snowalb to None")
        except Exception as e:
            raise ValueError(f"[site #{i}] SeasonCheck failed: {e}")

        # --------------------------------------
        # 2. Seasonal adjustment for DecTrees LAI
        # --------------------------------------
        dectr = props.get("land_cover", {}).get("dectr", {})
        sfr = dectr.get("sfr", {}).get("value", 0)

        if sfr > 0:
            lai = dectr.get("lai", {})
            laimin = lai.get("laimin", {}).get("value")
            laimax = lai.get("laimax", {}).get("value")
            lai_val = None

            if laimin is not None and laimax is not None:
                if season == "summer":
                    lai_val = laimax
                elif season == "winter":
                    lai_val = laimin
                elif season in ("spring", "fall"):
                    lai_val = (laimax + laimin) / 2

                if "dectr" in initial_states:
                    initial_states["dectr"]["lai_id"] = {"value": lai_val}
                    logger_supy.debug(
                        f"[site #{i}] Set lai_id to {lai_val} for season {season}"
                    )
        else:
            if "dectr" in initial_states:
                initial_states["dectr"]["lai_id"] = {"value": None}
                logger_supy.debug(f"[site #{i}] Nullified lai_id (no dectr surface)")

        # --------------------------------------
        # 3. DLS Check (timezone and DST start/end days)
        # --------------------------------------
        if (
            lat is not None and lng is not None
        ):  # <- Placeholder: consider cases where lat is None
            try:
                dls = DLSCheck(lat=lat, lng=lng, year=model_year)
                start_dls, end_dls, tz_offset = dls.compute_dst_transitions()

                if start_dls and end_dls:
                    props["anthropogenic_emissions"]["startdls"] = {"value": start_dls}
                    props["anthropogenic_emissions"]["enddls"] = {"value": end_dls}
                    logger_supy.debug(
                        f"[site #{i}] DLS: start={start_dls}, end={end_dls}"
                    )

                if tz_offset is not None:
                    props["timezone"] = {"value": tz_offset}
                    logger_supy.debug(f"[site #{i}] Timezone set to {tz_offset}")

                logger_supy.info(
                    f"[site #{i}] Overwriting pre-existing startdls and enddls with computed values."
                )
            except Exception as e:
                raise ValueError(f"[site #{i}] DLSCheck failed: {e}")

        # Final update
        site["properties"] = props
        site["initial_states"] = initial_states
        cleaned_sites.append(site)

    data["sites"] = cleaned_sites
    return data


def precheck_land_cover_fractions(data: dict) -> dict:
    for i, site in enumerate(data.get("sites", [])):
        props = site.get("properties", {})

        land_cover = props.get("land_cover")
        if not land_cover:
            raise ValueError(f"[site #{i}] Missing land_cover block.")

        # Calculate sum of all non-null surface fractions
        sfr_sum = sum(
            v.get("sfr", {}).get("value", 0)
            for v in land_cover.values()
            if isinstance(v, dict) and v.get("sfr", {}).get("value") is not None
        )

        logger_supy.debug(f"[site #{i}] Total land_cover sfr sum: {sfr_sum:.6f}")

        # Auto-fix tiny floating point errors (epsilon ~0.0001)
        if 0.9999 <= sfr_sum < 1.0:
            max_key = max(
                (
                    k
                    for k, v in land_cover.items()
                    if v.get("sfr", {}).get("value") is not None
                ),
                key=lambda k: land_cover[k]["sfr"]["value"],
            )
            correction = 1.0 - sfr_sum
            land_cover[max_key]["sfr"]["value"] += correction
            logger_supy.info(
                f"[site #{i}] Adjusted {max_key}.sfr up by {correction:.6f} to reach 1.0"
            )

        elif 1.0 < sfr_sum <= 1.0001:
            max_key = max(
                (
                    k
                    for k, v in land_cover.items()
                    if v.get("sfr", {}).get("value") is not None
                ),
                key=lambda k: land_cover[k]["sfr"]["value"],
            )
            correction = sfr_sum - 1.0
            land_cover[max_key]["sfr"]["value"] -= correction
            logger_supy.info(
                f"[site #{i}] Adjusted {max_key}.sfr down by {correction:.6f} to reach 1.0"
            )

        elif abs(sfr_sum - 1.0) > 0.0001:
            raise ValueError(f"[site #{i}] Invalid land_cover sfr sum: {sfr_sum:.6f}")

        site["properties"] = props

    return data


def precheck_nullify_zero_sfr_params(data: dict) -> dict:
    """For each site, nullify all land_cover parameters for surfaces with sfr == 0."""
    for site_idx, site in enumerate(data.get("sites", [])):
        land_cover = site.get("properties", {}).get("land_cover", {})
        for surf_type, props in land_cover.items():
            sfr = props.get("sfr", {}).get("value", 0)
            if sfr == 0:
                logger_supy.info(
                    f"[site #{site_idx}] Nullifying params for surface '{surf_type}' with sfr == 0"
                )
                for param_key, param_val in props.items():
                    if param_key == "sfr":
                        continue
                    # Nullify simple params
                    if isinstance(param_val, dict) and "value" in param_val:
                        param_val["value"] = None
                    # Nullify nested blocks (like ohm_coef, thermal_layers etc)
                    elif isinstance(param_val, dict):

                        def recursive_nullify(d):
                            for k, v in d.items():
                                if isinstance(v, dict):
                                    if "value" in v:
                                        if isinstance(v["value"], list):
                                            v["value"] = [None] * len(v["value"])
                                        else:
                                            v["value"] = None
                                    else:
                                        recursive_nullify(v)

                        recursive_nullify(param_val)
    return data


def precheck_nonzero_sfr_requires_nonnull_params(data: dict) -> dict:
    """
    For each site, check that for all land_cover surface types with sfr > 0,
    all related parameters (except 'sfr') are set (not None and not empty string),
    recursively for nested structures.
    """

    def check_recursively(d: dict, path: list, site_idx: int):
        if isinstance(d, dict):
            if "value" in d:
                val = d["value"]
                if val in (None, "") or (
                    isinstance(val, list) and any(v in (None, "") for v in val)
                ):
                    full_path = ".".join(path)
                    raise ValueError(
                        f"[site #{site_idx}] land_cover.{full_path} must be set (not None or empty) "
                        f"because {path[0]}.sfr > 0"
                    )
            else:
                for k, v in d.items():
                    check_recursively(v, path + [k], site_idx)

        elif isinstance(d, list):
            for idx, item in enumerate(d):
                check_recursively(item, path + [f"[{idx}]"], site_idx)

    for site_idx, site in enumerate(data.get("sites", [])):
        land_cover = site.get("properties", {}).get("land_cover", {})
        for surf_type, props in land_cover.items():
            sfr = props.get("sfr", {}).get("value", 0)
            if sfr > 0:
                for param_key, param_val in props.items():
                    if param_key == "sfr":
                        continue
                    check_recursively(
                        param_val, path=[surf_type, param_key], site_idx=site_idx
                    )

    logger_supy.info(
        "[precheck] Nonzero sfr parameters validated (all required fields are set)."
    )
    return data


def precheck_diagmethod(data: dict) -> dict:
    physics = data.get("model", {}).get("physics", {})
    rslmethod = physics.get("rslmethod", {}).get("value")

    if rslmethod != 2:
        logger_supy.debug("[precheck] rslmethod != 2, skipping faibldg check.")
        return data

    for i, site in enumerate(data.get("sites", [])):
        props = site.get("properties", {})
        land_cover = props.get("land_cover", {})
        bldgs = land_cover.get("bldgs", {})
        sfr = bldgs.get("sfr", {}).get("value", 0)

        if sfr > 0:
            faibldg = bldgs.get("faibldg", {})
            faibldg_value = faibldg.get("value")
            if faibldg_value in (None, "", []):
                raise ValueError(
                    f"[site #{i}] For rslmethod==2 and bldgs.sfr > 0, faibldg must be set and non-null."
                )

    logger_supy.info("[precheck] faibldg check for rslmethod==2 passed.")
    return data


def precheck_storageheatmethod(data: dict) -> dict:
    """If storageheatmethod == 6, check required wall thermal properties and lambda_c."""  # <--- Placeholder: this case refers to DyOHM
    storage_method = (
        data.get("model", {})
        .get("physics", {})
        .get("storageheatmethod", {})
        .get("value")
    )

    if storage_method != 6:
        logger_supy.debug(
            "[precheck] storageheatmethod != 6, skipping wall thermal layer checks."
        )
        return data

    for site_idx, site in enumerate(data.get("sites", [])):
        props = site.get("properties", {})
        vertical_layers = props.get("vertical_layers", {})
        walls = vertical_layers.get("walls", [])

        if not walls or not isinstance(walls, list) or len(walls) == 0:
            raise ValueError(
                f"[site #{site_idx}] Missing vertical_layers.walls for storageheatmethod == 6."
            )

        wall0 = walls[0]
        thermal = wall0.get("thermal_layers", {})

        for param in ["dz", "k", "cp"]:
            param_list = thermal.get(param, {}).get("value")
            if not isinstance(param_list, list) or len(param_list) == 0:
                raise ValueError(
                    f"[site #{site_idx}] Missing wall thermal_layers.{param} for storageheatmethod == 6."
                )
            if param_list[0] in (None, ""):
                raise ValueError(
                    f"[site #{site_idx}] wall thermal_layers.{param}[0] must be set for storageheatmethod == 6."
                )

        lambda_c = props.get("lambda_c", {}).get("value")
        if lambda_c in (None, ""):
            raise ValueError(
                f"[site #{site_idx}] properties.lambda_c must be set for storageheatmethod == 6."
            )

    logger_supy.info(
        "[precheck] storageheatmethod == 6 → wall thermal layers and lambda_c check passed."
    )
    return data


def precheck_stebbsmethod(data: dict) -> dict:
    """
    If stebbsmethod == 0, nullify all parameters under each site's properties.stebbs block.
    """
    physics = data.get("model", {}).get("physics", {})
    stebbs_method = physics.get("stebbsmethod", {}).get("value")

    if stebbs_method != 0:
        logger_supy.info("[precheck] stebbsmethod != 0, skipping stebbs nullification.")
        return data

    logger_supy.info("[precheck] stebbsmethod == 0, nullifying stebbs parameters...")

    for site_idx, site in enumerate(data.get("sites", [])):
        props = site.get("properties", {})
        stebbs_block = props.get("stebbs", {})

        def recursive_nullify(d):
            for k, v in d.items():
                if isinstance(v, dict):
                    if "value" in v:
                        v["value"] = None
                    else:
                        recursive_nullify(v)

        recursive_nullify(stebbs_block)
        site["properties"]["stebbs"] = stebbs_block

    return data


def run_precheck(path: str) -> dict:
    # ---- Step 0: Load yaml from path into a dict ----
    with open(path, "r") as file:
        data = yaml.load(file, Loader=yaml.FullLoader)

    # ---- Step 1: Print start message ----
    data = precheck_printing(data)

    # ---- Step 2: Extract start_date, end_date, model_year ----
    data, model_year, start_date, end_date = precheck_start_end_date(data)
    logger_supy.debug(
        f"Start date: {start_date}, end date: {end_date}, year: {model_year}"
    )

    # ---- Step 3: Check model.physics parameters ----
    data = precheck_model_physics_params(data)

    # ---- Step 4: Enforce model option constraints ----
    data = precheck_model_options_constraints(data)

    # ---- Step 5: Clean empty strings (except model.control and model.physics) ----
    data = precheck_replace_empty_strings_with_none(data)

    # ---- Step 6: Season + LAI + DLS adjustments per site ----
    data = precheck_site_season_adjustments(
        data, start_date=start_date, model_year=model_year
    )

    # ---- Step 7: Nullify params for surfaces with sfr == 0 ----
    data = precheck_nullify_zero_sfr_params(data)

    # ---- Step 8: Check existence of params for surfaces with sfr > 0 ----
    data = precheck_nonzero_sfr_requires_nonnull_params(data)

    # ---- Step 9: Land Cover Fractions checks & adjustments ----
    data = precheck_land_cover_fractions(data)

    # ---- Step 10: Rules associated to selected model options ----
    data = precheck_diagmethod(data)
    data = precheck_storageheatmethod(data)
    data = precheck_stebbsmethod(data)

    # ---- Step 11: Save output YAML ----
    output_filename = f"py0_{os.path.basename(path)}"
    output_path = os.path.join(os.path.dirname(path), output_filename)

    with open(output_path, "w") as f:
        yaml.dump(data, f, sort_keys=False, allow_unicode=True)

    logger_supy.info(f"Saved updated YAML file to: {output_path}")

    # ---- Step 12: Print completion ----
    logger_supy.info("Precheck complete.\n")
    return data


class SUEWSConfig(BaseModel):
    name: str = Field(
        default="sample config",
        description="Name of the SUEWS configuration",
        json_schema_extra={"display_name": "Configuration Name"},
    )
    description: str = Field(
        default="this is a sample config for testing purposes ONLY - values are not realistic",
        description="Description of this SUEWS configuration",
        json_schema_extra={"display_name": "Configuration Description"},
    )
    model: Model = Field(
        default_factory=Model,
        description="Model control and physics parameters",
        json_schema_extra={"display_name": "Model Parameters"},
    )
    sites: List[Site] = Field(
        default=[Site()],
        description="List of sites to simulate",
        min_length=1,
        json_schema_extra={"display_name": "Sites"},
    )

    model_config = ConfigDict(extra="allow")

    # Sort the filtered columns numerically
    @staticmethod
    def sort_key(col):
        try:
            return (col[0], ast.literal_eval(col[1]))
        except ValueError:
            return (col[0], col[1])

    # @model_validator(mode="after")
    # def check_forcing(self):
    #     from .._load import load_SUEWS_Forcing_met_df_yaml
    #     forcing = load_SUEWS_Forcing_met_df_yaml(self.model.control.forcing_file.value)
    #
    #     # Cut the forcing data to model period
    #     cut_forcing = forcing.loc[self.model.control.start_time: self.model.control.end_time]
    #
    #     # Check for missing forcing data
    #     missing_data = any(cut_forcing.isna().any())
    #     if missing_data:
    #         raise ValueError("Forcing data contains missing values.")

    #     # Check initial meteorology (for initial_states)
    #     first_day_forcing = cut_forcing.loc[self.model.control.start_time]
    #     first_day_min_temp = first_day_forcing.iloc[0]["Tair"]
    #     first_day_precip = first_day_forcing.iloc[0]["rain"] # Could check previous day if available

    #     # Use min temp for surface temperature states
    #     for site in self.site:
    #         for surf_type in SurfaceType:
    #             surface = getattr(site.initial_states, surf_type)
    #             surface.temperature.value = [first_day_min_temp]*5
    #             surface.tsfc.value = first_day_min_temp
    #             surface.tin.value = first_day_min_temp

    #     # Use precip to determine wetness state
    #     for site in self.site:
    #         for surf_type in SurfaceType:
    #             surface_is = getattr(site.initial_states, surf_type)
    #             surface_props =getattr(site.properties.land_cover, surf_type)
    #             if first_day_precip:
    #                 surface_is.state.value = surface_props.statelimit
    #                 surface_is.soilstore.value = surface_props.soilstorecap
    #                 if first_day_min_temp < 4:
    #                     surface_is.snowpack.value = surface_props.snowpacklimit
    #                     surface_is.snowfrac.value = 0.5 # Can these sum to greater than 1?
    #                     surface_is.icefrac.value = 0.5 # Can these sum to greater than 1?
    #                     surface_is.snowwater.value = 1 # TODO: What is the limit to this?
    #                     surface_is.snowdens.value = surface_props.snowdensmax
    #             else:
    #                 surface_is.state.value = 0
    #     return self

    @classmethod
    def from_yaml(
        cls, path: str, use_conditional_validation: bool = True, strict: bool = True
    ) -> "SUEWSConfig":
        """Initialize SUEWSConfig from YAML file with conditional validation.

        Args:
            path (str): Path to YAML configuration file
            use_conditional_validation (bool): Whether to use conditional validation
            strict (bool): If True, raise errors on validation failure

        Returns:
            SUEWSConfig: Instance of SUEWSConfig initialized from YAML
        """
        with open(path, "r") as file:
            config_data = yaml.load(file, Loader=yaml.FullLoader)

        if (
            use_conditional_validation and _validation_available
        ):  # _validation_available is always FALSE -- need to fix this
            # Step 1: Pre-validation with enhanced validation
            try:
                enhanced_from_yaml_validation(config_data, strict=strict)
            except ValueError:
                if strict:
                    raise
                # Continue with warnings already issued

            # Step 2: Create config with conditional validation applied
            try:
                return cls(**config_data)
            except Exception as e:
                if strict:
                    raise ValueError(
                        f"Failed to create SUEWSConfig after conditional validation: {e}"
                    )
                else:
                    warnings.warn(f"Config creation warning: {e}")
                    # Try with model_construct to bypass strict validation
                    return cls.model_construct(**config_data)
        elif use_conditional_validation and not _validation_available:
            warnings.warn(
                "Conditional validation requested but not available. Using standard validation."
            )
            # Fall back to original behavior
            return cls(**config_data)
        else:
            # Original behavior - validate everything
            logger_supy.info("Entering SUEWSConfig pydantic validator...")
            return cls(**config_data)

    def create_multi_index_columns(self, columns_file: str) -> pd.MultiIndex:
        """Create MultiIndex from df_state_columns.txt"""
        with open(columns_file, "r") as f:
            lines = f.readlines()

        tuples = []
        for line in lines:
            col_name, indices = line.strip().split(",", 1)
            str_indices = f"{indices}" if indices != "0" else "0"
            tuples.append((col_name, str_indices))

        return pd.MultiIndex.from_tuples(tuples)

    def to_df_state(
        self, use_conditional_validation: bool = True, strict: bool = False
    ) -> pd.DataFrame:
        """Convert config to DataFrame state format with optional conditional validation.

        Args:
            use_conditional_validation (bool): Whether to run conditional validation before conversion
            strict (bool): If True, fail on validation errors; if False, warn and continue

        Returns:
            pd.DataFrame: DataFrame containing SUEWS configuration state
        """
        if use_conditional_validation and _validation_available:
            # Pre-validate configuration before conversion
            config_data = self.model_dump()
            try:
                enhanced_to_df_state_validation(config_data, strict=strict)
            except ValueError:
                if strict:
                    raise
                # Continue with warnings already issued
        elif use_conditional_validation and not _validation_available:
            warnings.warn("Conditional validation requested but not available.")

        # Proceed with DataFrame conversion
        try:
            list_df_site = []
            for i in range(len(self.sites)):
                grid_id = self.sites[i].gridiv
                df_site = self.sites[i].to_df_state(grid_id)
                df_model = self.model.to_df_state(grid_id)
                df_site = pd.concat([df_site, df_model], axis=1)
                list_df_site.append(df_site)

            df = pd.concat(list_df_site, axis=0)
            # remove duplicate columns
            df = df.loc[:, ~df.columns.duplicated()]
        except Exception as e:
            if use_conditional_validation and not strict:
                warnings.warn(
                    f"Error during to_df_state conversion: {e}. This may be due to invalid parameters for disabled methods."
                )
                raise
            else:
                raise

        # # Fix level=1 columns sorted alphabetically not numerically (i.e. 10 < 2)
        # # Filter columns based on level=0 criteria
        # level_0_counts = df.columns.get_level_values(0).value_counts()
        # columns_to_sort = [col for col in df.columns if level_0_counts[col[0]] >= 10]

        # # Sort the filtered columns numericallyí
        # def sort_key(col):
        #     try:
        #         return (col[0], ast.literal_eval(col[1]))
        #     except ValueError:
        #         return (col[0], col[1])

        # sorted_columns = sorted(columns_to_sort, key=sort_key)

        # # Combine the sorted columns with the remaining columns
        # remaining_columns = [col for col in df.columns if col not in columns_to_sort]
        # final_columns = remaining_columns + sorted_columns

        # # Reindex the DataFrame using the final column order
        # df = df.reindex(columns=pd.MultiIndex.from_tuples(final_columns))

        # # set index name
        # df.index.set_names("grid", inplace=True)

        # Custom sorting function for level=1 columns
        def parse_level_1(value):
            """Parse level=1 column values into sortable tuples."""
            if value.startswith("(") and value.endswith(")"):
                # Remove parentheses and split by comma
                parts = value[1:-1].split(",")
                # Convert to integers, ignoring empty strings
                return tuple(int(part) for part in parts if part)
            try:
                # Try converting to an integer for single values like "x"
                return (int(value),)
            except ValueError:
                # Fallback for non-numeric values
                return (value,)

        # Extract MultiIndex levels as a list of tuples
        columns = list(df.columns)

        # Sort the columns using the custom function
        sorted_columns = sorted(
            columns, key=lambda col: (col[0], parse_level_1(col[1]))
        )

        # Re-create the MultiIndex with the sorted columns
        sorted_multi_index = pd.MultiIndex.from_tuples(sorted_columns)

        # Reindex the DataFrame with the sorted MultiIndex to preserve values
        df = df.reindex(columns=sorted_multi_index)

        # set column names
        df.columns.set_names(["var", "ind_dim"], inplace=True)
        df.index.name = "grid"

        return df

    @classmethod
    def from_df_state(cls, df: pd.DataFrame) -> "SUEWSConfig":
        """Create config from DataFrame state format.

        Args:
            df (pd.DataFrame): DataFrame containing SUEWS configuration state.

        Returns:
            SUEWSConfig: Instance of SUEWSConfig reconstructed from DataFrame.
        """
        # Initialize with default values
        config = cls()

        # Get grid IDs from DataFrame index
        grid_ids = df.index.tolist()

        # Create list of sites
        sites = []
        for grid_id in grid_ids:
            # Create site instance
            site = Site(gridiv=grid_id)

            # Set site properties
            site_properties = SiteProperties.from_df_state(df, grid_id)
            site.properties = site_properties

            # Set initial states
            initial_states = InitialStates.from_df_state(df, grid_id)
            site.initial_states = initial_states

            sites.append(site)

        # Update config with reconstructed data
        config.sites = sites

        # Reconstruct model
        config.model = Model.from_df_state(df, grid_ids[0])

        return config

    def to_yaml(self, path: str = "./config-suews.yml"):
        """Convert config to YAML format"""
        with open(path, "w") as file:
            yaml.dump(
                self.model_dump(exclude_none=True),
                file,
                sort_keys=False,
                allow_unicode=True,
            )


def init_config_from_yaml(path: str = "./config-suews.yml") -> SUEWSConfig:
    """Initialize SUEWSConfig from YAML file"""
    with open(path, "r") as file:
        config = yaml.load(file, Loader=yaml.FullLoader)
    return SUEWSConfig(**config)
