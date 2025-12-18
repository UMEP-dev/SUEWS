import yaml
from typing import Optional, Union, List, Any
import numpy as np
from pydantic import ConfigDict, BaseModel, Field, field_validator, model_validator
import pandas as pd
from enum import Enum
import inspect

from .type import RefValue, Reference, FlexibleRefValue
from .type import init_df_state
from .physics_options import (
    NetRadiationMethodConfig,
    RadiationPhysics,
    LongwaveSource,
    NETRAD_MAPPER,
    EmissionsMethodConfig,
    BiogenicModel,
    QFMethod,
    EMISSIONS_MAPPER,
)


def _enum_description(enum_class: type[Enum]) -> str:
    """
    Extract and format enum docstring for Field description.

    This makes enum docstrings the single source of truth for method documentation.
    The docstring is formatted to work with both RTD and JSON Schema generation.

    Args:
        enum_class: The Enum class to extract documentation from

    Returns:
        Formatted description string suitable for Field(description=...)
    """
    if not enum_class.__doc__:
        return ""

    # Clean and extract docstring
    doc = inspect.cleandoc(enum_class.__doc__)

    # Split into summary and options
    lines = doc.split("\n")

    # Find the summary (first paragraph before blank line or options)
    summary_lines = []
    option_lines = []
    in_options = False

    for line in lines:
        if not line.strip():
            in_options = True
            continue
        if in_options:
            option_lines.append(line.strip())
        else:
            summary_lines.append(line.strip())

    summary = " ".join(summary_lines)

    # Format options for Field description
    # Expected format for doc_utils: "0 (NAME) = Description; 1 (NAME2) = Description2"
    if option_lines:
        import re

        options_formatted = []

        for opt_line in option_lines:
            # Handle patterns like "0: NAME - Description" or "1-3: Description"
            # Extract: number(s), name, and description
            match = re.match(r"^(\d+(?:-\d+)?)\s*:\s*(\w+)\s*-\s*(.+)$", opt_line)
            if match:
                num, name, desc = match.groups()
                # Format as: "NUMBER (NAME) = Description"
                options_formatted.append(f"{num} ({name}) = {desc}")

        if options_formatted:
            options_text = "; ".join(options_formatted)
            return f"{summary} Options: {options_text}"

    return summary


def _coerce_enum_value(
    v: Any, enum_class: type[Enum], aliases: Optional[dict[str, str]] = None
) -> Any:
    """Coerce string or dict input to enum value (case-insensitive).

    Supports:
    - Enum member: returns as-is
    - Integer: returns as-is (Pydantic handles)
    - String: case-insensitive lookup by member name or alias
    - Dict with 'value' key: extracts and processes the value

    Parameters
    ----------
    v : Any
        Input value to coerce
    enum_class : type[Enum]
        Target enum class for lookup
    aliases : dict[str, str], optional
        Short alias → member name mapping (case-insensitive)

    Returns
    -------
    Any
        Coerced value suitable for Pydantic validation
    """
    if v is None or isinstance(v, enum_class):
        return v

    # Handle dict with 'value' key
    if isinstance(v, dict) and "value" in v:
        v = v["value"]

    # Handle string (case-insensitive lookup by alias, then member name)
    if isinstance(v, str):
        v_upper = v.upper()

        # Check aliases first
        if aliases:
            for alias, member_name in aliases.items():
                if alias.upper() == v_upper:
                    return enum_class[member_name]

        # Then check member names
        for member in enum_class:
            if member.name.upper() == v_upper:
                return member
        # If no match, let Pydantic handle (will raise appropriate error)

    return v


# =============================================================================
# Physics Option Aliases
# =============================================================================
#
# Naming Convention for Physics Options
# -------------------------------------
# Physics options support multiple input formats for flexibility:
#
# 1. NUMERIC CODES (legacy, still supported)
#    - Integer values matching Fortran interface
#    - Example: storageheatmethod: 1
#
# 2. EXPLICIT MODEL NAMES (preferred for named models)
#    - Use the established model acronym in lowercase
#    - Examples:
#      - ohm: Objective Hysteresis Model
#      - anohm: Analytical OHM
#      - estm: Element Surface Temperature Method
#      - ehc: Explicit Heat Conduction
#      - dyohm: Dynamic OHM
#      - narp: Net All-wave Radiation Parameterization
#      - most: Monin-Obukhov Similarity Theory
#      - rst: Roughness Sublayer Theory
#
# 3. AUTHOR-YEAR FORMAT (Xyy) for methods without model names
#    - Format: First letter of first author surname + two-digit year
#    - Case-insensitive (K09, k09, K09 all work)
#    - Examples:
#      - K09: Kawai et al. 2009 (thermal roughness)
#      - CN98: Campbell & Norman 1998 (stability functions)
#      - W16: Ward et al. 2016 (stomatal conductance)
#      - J11: Järvi et al. 2011 (stomatal conductance, QF)
#      - B82: Brutsaert 1982 (thermal roughness)
#      - M98: MacDonald et al. 1998 (momentum roughness)
#      - GO99: Grimmond & Oke 1999 (roughness length)
#
# 4. GENERIC DESCRIPTIVE TERMS (for simple choices)
#    - fixed, variable, auto: method selection
#    - model, obs: modelled vs observed
#    - provided, calc: use input vs calculate
#
# 5. BINARY OPTIONS (yes/no)
#    - ohmincqf: yes/no (include QF in OHM)
#    - snowuse: yes/no (enable snow module)
#
# All string inputs are case-insensitive.
# =============================================================================

# Maps short alias → enum member name
STORAGE_HEAT_ALIASES = {
    "obs": "OBSERVED",
    "ohm": "OHM_WITHOUT_QF",
    "anohm": "ANOHM",  # Analytical OHM
    "estm": "ESTM",  # Element Surface Temperature Method
    "ehc": "EHC",  # Explicit Heat Conduction
    "dyohm": "DyOHM",  # Dynamic OHM
    "stebbs": "STEBBS",
}

OHM_INC_QF_ALIASES = {
    "no": "EXCLUDE",
    "yes": "INCLUDE",
}

ROUGHLEN_MOM_ALIASES = {
    "fixed": "FIXED",
    "variable": "VARIABLE",
    "M98": "MACDONALD",  # MacDonald et al. 1998
    "GO99": "LAMBDAP_DEPENDENT",  # Grimmond & Oke 1999
}

ROUGHLEN_HEAT_ALIASES = {
    "B82": "BRUTSAERT",  # Brutsaert 1982
    "K09": "KAWAI",  # Kawai et al. 2009
    "K07": "KANDA",  # Kanda et al. 2007
    "auto": "ADAPTIVE",
}

STABILITY_ALIASES = {
    "H88": "HOEGSTROM",  # Högström 1988
    "CN98": "CAMPBELL_NORMAN",  # Campbell & Norman 1998
    "B71": "BUSINGER_HOEGSTROM",  # Businger et al. 1971
}

SMD_ALIASES = {
    "model": "MODELLED",
    "obs_vol": "OBSERVED_VOLUMETRIC",
    "obs_grav": "OBSERVED_GRAVIMETRIC",
}

WATER_USE_ALIASES = {
    "model": "MODELLED",
    "obs": "OBSERVED",
}

RSL_ALIASES = {
    "most": "MOST",
    "rst": "RST",
    "auto": "VARIABLE",
}

FAI_ALIASES = {
    "provided": "USE_PROVIDED",
    "calc": "SIMPLE_SCHEME",
}

RSL_LEVEL_ALIASES = {
    "off": "NONE",
    "basic": "BASIC",
    "full": "DETAILED",
}

GS_MODEL_ALIASES = {
    "J11": "JARVI",  # Järvi et al. 2011
    "W16": "WARD",  # Ward et al. 2016
}

SNOW_USE_ALIASES = {
    "no": "DISABLED",
    "yes": "ENABLED",
}

STEBBS_ALIASES = {
    "off": "NONE",
    "default": "DEFAULT",
    "custom": "PROVIDED",
}

RC_ALIASES = {
    "off": "NONE",
    "basic": "BASIC",
    "full": "DETAILED",
}


class EmissionsMethod(Enum):
    """
    Method for calculating anthropogenic heat flux (QF) and CO2 emissions.

    0: OBSERVED - Uses observed QF values from forcing file (set to zero to exclude QF from energy balance)
    1: L11 - Loridan et al. (2011) SAHP method with air temperature and population density
    2: J11 - Järvi et al. (2011) SAHP_2 method with heating/cooling degree days
    3: L11_UPDATED - Modified Loridan method using daily mean air temperature
    4: J19 - Järvi et al. (2019) method with building energy, metabolism, and traffic
    5: J19_UPDATED - As method 4 but also calculates CO2 emissions
    11: BIOGEN_RECT_L11 - Rectangular hyperbola photosynthesis + L11 QF (experimental)
    12: BIOGEN_RECT_J11 - Rectangular hyperbola photosynthesis + J11 QF (experimental)
    13: BIOGEN_RECT_L11U - Rectangular hyperbola photosynthesis + L11_UPDATED QF (experimental)
    14: BIOGEN_RECT_J19 - Rectangular hyperbola photosynthesis + J19 QF (experimental)
    15: BIOGEN_RECT_J19U - Rectangular hyperbola photosynthesis + J19_UPDATED QF (experimental)
    21: BIOGEN_NRECT_L11 - Non-rectangular hyperbola (Bellucco 2017) + L11 QF (experimental)
    22: BIOGEN_NRECT_J11 - Non-rectangular hyperbola (Bellucco 2017) + J11 QF (experimental)
    23: BIOGEN_NRECT_L11U - Non-rectangular hyperbola (Bellucco 2017) + L11_UPDATED QF (experimental)
    24: BIOGEN_NRECT_J19 - Non-rectangular hyperbola (Bellucco 2017) + J19 QF (experimental)
    25: BIOGEN_NRECT_J19U - Non-rectangular hyperbola (Bellucco 2017) + J19_UPDATED QF (experimental)
    41: BIOGEN_COND_L11 - Conductance-based photosynthesis (Järvi 2019) + L11 QF (experimental)
    42: BIOGEN_COND_J11 - Conductance-based photosynthesis (Järvi 2019) + J11 QF (experimental)
    43: BIOGEN_COND_L11U - Conductance-based photosynthesis (Järvi 2019) + L11_UPDATED QF (experimental)
    44: BIOGEN_COND_J19 - Conductance-based photosynthesis (Järvi 2019) + J19 QF (experimental)
    45: BIOGEN_COND_J19U - Conductance-based photosynthesis (Järvi 2019) + J19_UPDATED QF (experimental)
    """

    # just a demo to show how to use Enum for emissionsmethod
    OBSERVED = 0
    L11 = 1
    J11 = 2
    L11_UPDATED = 3
    J19 = 4
    J19_UPDATED = 5

    def __new__(cls, value):
        obj = object.__new__(cls)
        obj._value_ = value
        # Mark internal options
        if value in [3, 5]:  # L11_UPDATED and J19_UPDATED
            obj._internal = True
        else:
            obj._internal = False
        return obj

    def __int__(self):
        """Representation showing just the value"""
        return self.value

    def __repr__(self):
        """Representation showing the name and value"""
        return str(self.value)


class NetRadiationMethod(Enum):
    """
    Method for calculating net all-wave radiation (Q*).

    0: OBSERVED - Uses observed Q* values from forcing file
    1: LDOWN_OBSERVED - Models Q* using NARP (Net All-wave Radiation Parameterization; Offerle et al. 2003, Loridan et al. 2011) with observed longwave down radiation (L↓) from forcing file
    2: LDOWN_CLOUD - Models Q* using NARP with L↓ estimated from cloud cover fraction
    3: LDOWN_AIR - Models Q* using NARP with L↓ estimated from air temperature and relative humidity
    11: LDOWN_SURFACE - Surface temperature variant of method 1 (not recommended)
    12: LDOWN_CLOUD_SURFACE - Surface temperature variant of method 2 (not recommended)
    13: LDOWN_AIR_SURFACE - Surface temperature variant of method 3 (not recommended)
    100: LDOWN_ZENITH - Zenith angle correction variant of method 1 (not recommended)
    200: LDOWN_CLOUD_ZENITH - Zenith angle correction variant of method 2 (not recommended)
    300: LDOWN_AIR_ZENITH - Zenith angle correction variant of method 3 (not recommended)
    1001: LDOWN_SS_OBSERVED - SPARTACUS-Surface integration with observed L↓ (experimental)
    1002: LDOWN_SS_CLOUD - SPARTACUS-Surface integration with L↓ from cloud fraction (experimental)
    1003: LDOWN_SS_AIR - SPARTACUS-Surface integration with L↓ from air temperature/humidity (experimental)
    """

    OBSERVED = 0
    LDOWN_OBSERVED = 1
    LDOWN_CLOUD = 2
    LDOWN_AIR = 3
    LDOWN_SURFACE = 11
    LDOWN_CLOUD_SURFACE = 12
    LDOWN_AIR_SURFACE = 13
    LDOWN_ZENITH = 100
    LDOWN_CLOUD_ZENITH = 200
    LDOWN_AIR_ZENITH = 300
    LDOWN_SS_OBSERVED = 1001
    LDOWN_SS_CLOUD = 1002
    LDOWN_SS_AIR = 1003

    def __new__(cls, value):
        obj = object.__new__(cls)
        obj._value_ = value
        # Mark internal options (not recommended/experimental)
        if value in [11, 12, 13, 100, 200, 300, 1001, 1002, 1003]:
            obj._internal = True
        else:
            obj._internal = False
        return obj

    def __int__(self):
        return self.value

    def __repr__(self):
        return str(self.value)

    @property
    def model(self) -> str:
        """Return model dimension: obs, narp, or spartacus.

        Returns
        -------
        str
            'obs' for observed, 'narp' for NARP, 'spartacus' for SPARTACUS
        """
        if self.value == 0:
            return "obs"
        elif self.value >= 1000:
            return "spartacus"
        else:
            return "narp"

    @property
    def ldown(self) -> Optional[str]:
        """Return ldown dimension: obs, cloud, air, or None.

        Returns
        -------
        str or None
            L↓ source, or None for observed model
        """
        if self.value == 0:
            return None
        # Extract last digit for ldown code
        lw_code = self.value % 10
        return {1: "obs", 2: "cloud", 3: "air"}.get(lw_code)


class StorageHeatMethod(Enum):
    """
    Method for calculating storage heat flux (ΔQS).

    0: OBSERVED - Uses observed ΔQS values from forcing file
    1: OHM_WITHOUT_QF - Objective Hysteresis Model using Q* only (use with OhmIncQf=0)
    3: ANOHM - Analytical OHM (Sun et al., 2017) - not recommended
    4: ESTM - Element Surface Temperature Method (Offerle et al., 2005) - not recommended
    5: EHC - Explicit Heat Conduction model with separate roof/wall/ground temperatures
    6: DyOHM - Dynamic Objective Hysteresis Model (Liu et al., 2025) with dynamic coefficients
    7: STEBBS - use STEBBS storage heat flux for building, others use OHM
    """

    # Note: EHC (option 5) implements explicit heat conduction
    # Note: DyOHM (option 6) calculates OHM coefficients dynamically based on material properties and meteorology
    # Put STEBBSoption here to turn on STEBBS storage heat flux, internal temperature, etc.

    OBSERVED = 0
    OHM_WITHOUT_QF = 1
    ANOHM = 3
    ESTM = 4
    EHC = 5
    DyOHM = 6
    STEBBS = 7

    def __new__(cls, value):
        obj = object.__new__(cls)
        obj._value_ = value
        # Mark internal options (not recommended)
        if value in [3, 4]:  # ANOHM and ESTM
            obj._internal = True
        else:
            obj._internal = False
        return obj

    def __int__(self):
        return self.value

    def __repr__(self):
        return str(self.value)


class OhmIncQf(Enum):
    """
    Controls inclusion of anthropogenic heat flux in OHM storage heat calculations.

    0: EXCLUDE - Use Q* only (required when StorageHeatMethod=1)
    1: INCLUDE - Use Q*+QF (for other OHM-based storage heat methods)
    """

    EXCLUDE = 0
    INCLUDE = 1

    def __int__(self):
        return self.value

    def __repr__(self):
        return str(self.value)


class MomentumRoughnessMethod(Enum):
    """
    Method for calculating momentum roughness length (z0m).

    1: FIXED - Fixed roughness length from site parameters
    2: VARIABLE - Variable based on vegetation LAI using rule of thumb (Grimmond & Oke 1999)
    3: MACDONALD - MacDonald et al. (1998) morphometric method based on building geometry
    4: LAMBDAP_DEPENDENT - Varies with plan area fraction λp (Grimmond & Oke 1999)
    5: ALTERNATIVE - Alternative variable method
    """

    FIXED = 1  # Fixed roughness length
    VARIABLE = 2  # Variable roughness length based on vegetation state
    MACDONALD = 3  # MacDonald 1998 method
    LAMBDAP_DEPENDENT = 4  # lambdaP dependent method
    ALTERNATIVE = 5  # Alternative method

    def __new__(cls, value):
        obj = object.__new__(cls)
        obj._value_ = value
        # Mark internal options
        if value == 5:  # ALTERNATIVE (vague method)
            obj._internal = True
        else:
            obj._internal = False
        return obj

    def __int__(self):
        return self.value

    def __repr__(self):
        return str(self.value)


class HeatRoughnessMethod(Enum):
    """
    Method for calculating thermal roughness length (z0h).

    1: BRUTSAERT - Brutsaert (1982) z0h = z0m/10 (see Grimmond & Oke 1986)
    2: KAWAI - Kawai et al. (2009) formulation
    3: VOOGT_GRIMMOND - Voogt and Grimmond (2000) formulation
    4: KANDA - Kanda et al. (2007) formulation
    5: ADAPTIVE - Adaptively using z0m based on pervious coverage: if fully pervious, use method 1; otherwise, use method 2
    """

    BRUTSAERT = 1  # z0h = z0m/10
    KAWAI = 2  # Kawai et al. (2009)
    VOOGT_GRIMMOND = 3  # Voogt and Grimmond (2000)
    KANDA = 4  # Kanda et al. (2007)
    ADAPTIVE = 5  # Adaptive method based on surface coverage

    def __int__(self):
        return self.value

    def __repr__(self):
        return str(self.value)


class StabilityMethod(Enum):
    """
    Atmospheric stability correction functions for momentum and heat fluxes.

    0: NOT_USED - Reserved
    1: NOT_USED2 - Reserved
    2: HOEGSTROM - Dyer (1974)/Högström (1988) for momentum, Van Ulden & Holtslag (1985) for stable conditions (not recommended)
    3: CAMPBELL_NORMAN - Campbell & Norman (1998) formulations for both momentum and heat
    4: BUSINGER_HOEGSTROM - Businger et al. (1971)/Högström (1988) formulations (not recommended)
    """

    NOT_USED = 0
    NOT_USED2 = 1
    HOEGSTROM = 2
    CAMPBELL_NORMAN = 3
    BUSINGER_HOEGSTROM = 4

    def __new__(cls, value):
        obj = object.__new__(cls)
        obj._value_ = value
        # Mark internal options (reserved/not recommended)
        if value in [0, 1, 2, 4]:  # NOT_USED, NOT_USED2, HOEGSTROM, BUSINGER_HOEGSTROM
            obj._internal = True
        else:
            obj._internal = False
        return obj

    def __int__(self):
        return self.value

    def __repr__(self):
        return str(self.value)


class SMDMethod(Enum):
    """
    Method for determining soil moisture deficit (SMD).

    0: MODELLED - SMD calculated from water balance using soil parameters
    1: OBSERVED_VOLUMETRIC - Uses observed volumetric soil moisture content (m³/m³) from forcing file
    2: OBSERVED_GRAVIMETRIC - Uses observed gravimetric soil moisture content (kg/kg) from forcing file
    """

    MODELLED = 0
    OBSERVED_VOLUMETRIC = 1
    OBSERVED_GRAVIMETRIC = 2

    def __int__(self):
        return self.value

    def __repr__(self):
        return str(self.value)


class WaterUseMethod(Enum):
    """
    Method for determining external water use (irrigation).

    0: MODELLED - Water use calculated based on soil moisture deficit and irrigation parameters
    1: OBSERVED - Uses observed water use values from forcing file
    """

    MODELLED = 0
    OBSERVED = 1

    def __int__(self):
        return self.value

    def __repr__(self):
        return str(self.value)


class RSLMethod(Enum):
    """
    Roughness Sublayer (RSL) method for calculating near-surface meteorological diagnostics (2m temperature, 2m humidity, 10m wind speed).

    0: MOST (Monin-Obukhov Similarity Theory) - Appropriate for relatively homogeneous, flat surfaces
    1: RST (Roughness Sublayer Theory; Theeuwes et al. 2019) - Appropriate for heterogeneous urban surfaces with tall roughness elements
    2: VARIABLE - Automatically selects between MOST and RST based on surface morphology (plan area index, frontal area index, and roughness element heights)
    """

    MOST = 0
    RST = 1
    VARIABLE = 2

    def __int__(self):
        return self.value

    def __repr__(self):
        return str(self.value)


class FAIMethod(Enum):
    """
    Method for calculating frontal area index (FAI) - the ratio of frontal area to plan area.

    0: USE_PROVIDED - Use FAI values provided in site parameters (FAIBldg, FAIEveTree, FAIDecTree)
    1: SIMPLE_SCHEME - Calculate FAI using simple scheme based on surface fractions and heights (see issue #192)
    """

    USE_PROVIDED = 0  # Use FAI values from site parameters
    SIMPLE_SCHEME = 1  # Calculate FAI using simple scheme (sqrt(fr)*h for buildings, empirical for trees)

    def __new__(cls, value):
        obj = object.__new__(cls)
        obj._value_ = value
        # Mark internal options
        if value == 0:  # ZERO (not documented)
            obj._internal = True
        else:
            obj._internal = False
        return obj

    def __int__(self):
        return self.value

    def __repr__(self):
        return str(self.value)


class RSLLevel(Enum):
    """
    Method for incorporating local environmental feedbacks on surface processes, particularly vegetation phenology and evapotranspiration responses to urban heat island effects.

    0: NONE - No local climate adjustments; use forcing file meteorology directly
    1: BASIC - Simple adjustments for urban temperature effects on leaf area index (LAI) and growing degree days
    2: DETAILED - Comprehensive feedbacks including moisture stress, urban CO2 dome effects, and modified phenology cycles
    """

    NONE = 0
    BASIC = 1
    DETAILED = 2

    def __int__(self):
        return self.value

    def __repr__(self):
        return str(self.value)


class GSModel(Enum):
    """
    Stomatal conductance parameterisation method for vegetation surfaces.

    1: JARVI - Original parameterisation (Järvi et al. 2011) based on environmental controls
    2: WARD - Updated parameterisation (Ward et al. 2016) with improved temperature and VPD responses
    """

    JARVI = 1
    WARD = 2
    # MP: Removed as dependent on rsllevel - legacy options for CO2 with 2 m temperature
    # JARVI_2M = 3
    # WARD_2M = 4

    def __int__(self):
        return self.value

    def __repr__(self):
        return str(self.value)


class StebbsMethod(Enum):
    """
    Surface Temperature Energy Balance Based Scheme (STEBBS) for facet temperatures.

    0: NONE - STEBBS calculations disabled
    1: DEFAULT - STEBBS enabled with default parameters
    2: PROVIDED - STEBBS enabled with user-specified parameters
    """

    NONE = 0
    DEFAULT = 1
    PROVIDED = 2

    def __int__(self):
        return self.value

    def __repr__(self):
        return str(self.value)


class RCMethod(Enum):
    """
    Method to split building envelope heat capacity in STEBBS.

    0: NONE - No heat capacity splitting applied
    1: PROVIDED - Use user defined value (fractional x1) between 0 and 1
    2: PARAMETERISE - Use building material thermal property to parameterise the weighting factor x1
    """

    NONE = 0
    PROVIDED = 1
    PARAMETERISE = 2

    def __int__(self):
        return self.value

    def __repr__(self):
        return str(self.value)


class SnowUse(Enum):
    """
    Controls snow process calculations (Järvi et al. 2014).

    0: DISABLED - Snow processes not included
    1: ENABLED - Snow accumulation, melt, and albedo effects included
    """

    DISABLED = 0
    ENABLED = 1

    def __int__(self):
        return self.value

    def __repr__(self):
        return str(self.value)


def yaml_equivalent_of_default(dumper, data):
    """Convert enum values to YAML scalar integers.

    This function is used as a YAML representer for enum classes. It converts the enum value
    to a string and represents it as a YAML integer scalar.

    Args:
        dumper: The YAML dumper instance
        data: The enum value to be converted

    Returns:
        A YAML scalar node containing the integer value of the enum
    """
    return dumper.represent_scalar("tag:yaml.org,2002:int", str(data.value))


# Register YAML representers for all enums
for enum_class in [
    NetRadiationMethod,
    EmissionsMethod,
    StorageHeatMethod,
    MomentumRoughnessMethod,
    HeatRoughnessMethod,
    StabilityMethod,
    SMDMethod,
    WaterUseMethod,
    RSLMethod,
    FAIMethod,
    RSLLevel,
    GSModel,
    StebbsMethod,
    RCMethod,
    SnowUse,
    OhmIncQf,
]:
    yaml.add_representer(enum_class, yaml_equivalent_of_default)


class ModelPhysics(BaseModel):
    """
    Model physics configuration options.
    """

    model_config = ConfigDict(title="Physics Methods")

    netradiationmethod: NetRadiationMethodConfig = Field(
        default_factory=lambda: NetRadiationMethodConfig(
            scheme=RadiationPhysics.NARP, ldown=LongwaveSource.AIR
        ),
        description=(
            "Method for calculating net all-wave radiation (Q*). "
            "Uses orthogonal dimensions: scheme (obs/narp/spartacus) and "
            "ldown source (obs/cloud/air). See nested structure for details."
        ),
        json_schema_extra={"unit": "dimensionless"},
    )

    @field_validator("netradiationmethod", mode="before")
    @classmethod
    def coerce_netradiationmethod(cls, v: Any) -> Any:
        """Accept legacy and dimension-based forms for netradiationmethod.

        Accepted forms:
        - Dimension-based: {"scheme": "narp", "ldown": "air"}
        - Legacy RefValue: {"value": 3}
        - Plain integer: 3
        - Enum member: NetRadiationMethod.LDOWN_AIR
        - Already a NetRadiationMethodConfig instance
        """
        if v is None or isinstance(v, NetRadiationMethodConfig):
            return v

        # Dimension-based form: {scheme: narp, ldown: air}
        if isinstance(v, dict):
            if "scheme" in v:
                return v  # Pass to Pydantic for validation

            # Legacy form: {value: N}
            if "value" in v:
                code = int(v["value"])
                scheme_val, ldown_val = NETRAD_MAPPER.from_code(code)
                result: dict[str, Any] = {"scheme": scheme_val}
                if ldown_val:
                    result["ldown"] = ldown_val
                return result

        # Plain int or enum
        code = v.value if isinstance(v, Enum) else int(v)
        scheme_val, ldown_val = NETRAD_MAPPER.from_code(code)
        result = {"scheme": scheme_val}
        if ldown_val:
            result["ldown"] = ldown_val
        return result

    emissionsmethod: EmissionsMethodConfig = Field(
        default_factory=lambda: EmissionsMethodConfig(
            heat=QFMethod.J11, co2=BiogenicModel.NONE
        ),
        description=(
            "Method for calculating anthropogenic heat flux and CO2 emissions. "
            "Uses orthogonal dimensions: heat (obs/L11/J11/L11_updated/J19/J19_updated) "
            "and co2 (none/rectangular/non_rectangular/conductance). See nested structure for details."
        ),
        json_schema_extra={"unit": "dimensionless"},
    )

    @field_validator("emissionsmethod", mode="before")
    @classmethod
    def coerce_emissionsmethod(cls, v: Any) -> Any:
        """Accept legacy and dimension-based forms for emissionsmethod.

        Accepted forms:
        - Dimension-based: {"heat": "L11", "co2": "none"}
        - Legacy RefValue: {"value": 1}
        - Plain integer: 1
        - Already an EmissionsMethodConfig instance
        """
        if v is None or isinstance(v, EmissionsMethodConfig):
            return v

        # Dimension-based form: {heat: L11, co2: none}
        if isinstance(v, dict):
            if "heat" in v or "co2" in v:
                return v  # Pass to Pydantic for validation

            # Legacy form: {value: N}
            if "value" in v:
                code = int(v["value"])
                co2_val, heat_val = EMISSIONS_MAPPER.from_code(code)
                return {"heat": heat_val, "co2": co2_val}

        # Plain int
        code = int(v)
        co2_val, heat_val = EMISSIONS_MAPPER.from_code(code)
        return {"heat": heat_val, "co2": co2_val}

    storageheatmethod: FlexibleRefValue(StorageHeatMethod) = Field(
        default=StorageHeatMethod.OHM_WITHOUT_QF,
        description=_enum_description(StorageHeatMethod),
        json_schema_extra={"unit": "dimensionless"},
    )
    ohmincqf: FlexibleRefValue(OhmIncQf) = Field(
        default=OhmIncQf.EXCLUDE,
        description=_enum_description(OhmIncQf),
        json_schema_extra={"unit": "dimensionless"},
    )
    roughlenmommethod: FlexibleRefValue(MomentumRoughnessMethod) = Field(
        default=MomentumRoughnessMethod.VARIABLE,
        description=_enum_description(MomentumRoughnessMethod),
        json_schema_extra={"unit": "dimensionless"},
    )
    roughlenheatmethod: FlexibleRefValue(HeatRoughnessMethod) = Field(
        default=HeatRoughnessMethod.KAWAI,
        description=_enum_description(HeatRoughnessMethod),
        json_schema_extra={"unit": "dimensionless"},
    )
    stabilitymethod: FlexibleRefValue(StabilityMethod) = Field(
        default=StabilityMethod.CAMPBELL_NORMAN,
        description=_enum_description(StabilityMethod),
        json_schema_extra={
            "unit": "dimensionless",
            "provides_to": ["rslmethod"],
            "note": "Provides stability correction functions used by rslmethod calculations",
        },
    )
    smdmethod: FlexibleRefValue(SMDMethod) = Field(
        default=SMDMethod.MODELLED,
        description=_enum_description(SMDMethod),
        json_schema_extra={"unit": "dimensionless"},
    )
    waterusemethod: FlexibleRefValue(WaterUseMethod) = Field(
        default=WaterUseMethod.MODELLED,
        description=_enum_description(WaterUseMethod),
        json_schema_extra={"unit": "dimensionless"},
    )
    rslmethod: FlexibleRefValue(RSLMethod) = Field(
        default=RSLMethod.VARIABLE,
        description=_enum_description(RSLMethod),
        json_schema_extra={
            "unit": "dimensionless",
            "depends_on": ["stabilitymethod"],
            "provides_to": ["rsllevel"],
            "note": "Determines how near-surface values (2m temp, 10m wind) are calculated from forcing data",
        },
    )
    faimethod: FlexibleRefValue(FAIMethod) = Field(
        default=FAIMethod.USE_PROVIDED,
        description=_enum_description(FAIMethod),
        json_schema_extra={"unit": "dimensionless"},
    )
    rsllevel: FlexibleRefValue(RSLLevel) = Field(
        default=RSLLevel.NONE,
        description=_enum_description(RSLLevel),
        json_schema_extra={
            "unit": "dimensionless",
            "depends_on": ["rslmethod"],
            "provides_to": ["gsmodel"],
            "note": "Uses near-surface values from rslmethod to modify vegetation processes",
        },
    )
    gsmodel: FlexibleRefValue(GSModel) = Field(
        default=GSModel.WARD,
        description=_enum_description(GSModel),
        json_schema_extra={
            "unit": "dimensionless",
            "depends_on": ["rsllevel"],
            "note": "Stomatal conductance model influenced by rsllevel adjustments",
        },
    )
    snowuse: FlexibleRefValue(SnowUse) = Field(
        default=SnowUse.DISABLED,
        description=_enum_description(SnowUse),
        json_schema_extra={"unit": "dimensionless"},
    )
    stebbsmethod: FlexibleRefValue(StebbsMethod) = Field(
        default=StebbsMethod.NONE,
        description=_enum_description(StebbsMethod),
        json_schema_extra={"unit": "dimensionless"},
    )
    rcmethod: FlexibleRefValue(RCMethod) = Field(
        default=RCMethod.NONE,
        description=_enum_description(RCMethod),
        json_schema_extra={"unit": "dimensionless"},
    )
    ref: Optional[Reference] = None

    # Validators for case-insensitive string name support (with short aliases)
    @field_validator("storageheatmethod", mode="before")
    @classmethod
    def coerce_storageheatmethod(cls, v: Any) -> Any:
        """Accept string names and short aliases for storageheatmethod."""
        return _coerce_enum_value(v, StorageHeatMethod, STORAGE_HEAT_ALIASES)

    @field_validator("ohmincqf", mode="before")
    @classmethod
    def coerce_ohmincqf(cls, v: Any) -> Any:
        """Accept string names and short aliases (yes/no) for ohmincqf."""
        return _coerce_enum_value(v, OhmIncQf, OHM_INC_QF_ALIASES)

    @field_validator("roughlenmommethod", mode="before")
    @classmethod
    def coerce_roughlenmommethod(cls, v: Any) -> Any:
        """Accept string names and short aliases for roughlenmommethod."""
        return _coerce_enum_value(v, MomentumRoughnessMethod, ROUGHLEN_MOM_ALIASES)

    @field_validator("roughlenheatmethod", mode="before")
    @classmethod
    def coerce_roughlenheatmethod(cls, v: Any) -> Any:
        """Accept string names and short aliases for roughlenheatmethod."""
        return _coerce_enum_value(v, HeatRoughnessMethod, ROUGHLEN_HEAT_ALIASES)

    @field_validator("stabilitymethod", mode="before")
    @classmethod
    def coerce_stabilitymethod(cls, v: Any) -> Any:
        """Accept string names and short aliases for stabilitymethod."""
        return _coerce_enum_value(v, StabilityMethod, STABILITY_ALIASES)

    @field_validator("smdmethod", mode="before")
    @classmethod
    def coerce_smdmethod(cls, v: Any) -> Any:
        """Accept string names and short aliases for smdmethod."""
        return _coerce_enum_value(v, SMDMethod, SMD_ALIASES)

    @field_validator("waterusemethod", mode="before")
    @classmethod
    def coerce_waterusemethod(cls, v: Any) -> Any:
        """Accept string names and short aliases for waterusemethod."""
        return _coerce_enum_value(v, WaterUseMethod, WATER_USE_ALIASES)

    @field_validator("rslmethod", mode="before")
    @classmethod
    def coerce_rslmethod(cls, v: Any) -> Any:
        """Accept string names and short aliases for rslmethod."""
        return _coerce_enum_value(v, RSLMethod, RSL_ALIASES)

    @field_validator("faimethod", mode="before")
    @classmethod
    def coerce_faimethod(cls, v: Any) -> Any:
        """Accept string names and short aliases for faimethod."""
        return _coerce_enum_value(v, FAIMethod, FAI_ALIASES)

    @field_validator("rsllevel", mode="before")
    @classmethod
    def coerce_rsllevel(cls, v: Any) -> Any:
        """Accept string names and short aliases for rsllevel."""
        return _coerce_enum_value(v, RSLLevel, RSL_LEVEL_ALIASES)

    @field_validator("gsmodel", mode="before")
    @classmethod
    def coerce_gsmodel(cls, v: Any) -> Any:
        """Accept string names and short aliases for gsmodel."""
        return _coerce_enum_value(v, GSModel, GS_MODEL_ALIASES)

    @field_validator("snowuse", mode="before")
    @classmethod
    def coerce_snowuse(cls, v: Any) -> Any:
        """Accept string names and short aliases (yes/no) for snowuse."""
        return _coerce_enum_value(v, SnowUse, SNOW_USE_ALIASES)

    @field_validator("stebbsmethod", mode="before")
    @classmethod
    def coerce_stebbsmethod(cls, v: Any) -> Any:
        """Accept string names and short aliases for stebbsmethod."""
        return _coerce_enum_value(v, StebbsMethod, STEBBS_ALIASES)

    @field_validator("rcmethod", mode="before")
    @classmethod
    def coerce_rcmethod(cls, v: Any) -> Any:
        """Accept string names and short aliases for rcmethod."""
        return _coerce_enum_value(v, RCMethod, RC_ALIASES)

    # We then need to set to 0 (or None) all the CO2-related parameters or rules
    # in the code and return them accordingly in the yml file.

    def to_df_state(self, grid_id: int) -> pd.DataFrame:
        """Convert model physics properties to DataFrame state format."""
        df_state = init_df_state(grid_id)

        # Helper function to set values in DataFrame
        def set_df_value(col_name: str, value: Any):
            idx_str = "0"
            if (col_name, idx_str) not in df_state.columns:
                df_state[(col_name, idx_str)] = None
            # Handle different value types
            if isinstance(value, NetRadiationMethodConfig):
                val = value.int_value
            elif isinstance(value, RefValue):
                val = value.value
            else:
                val = value
            df_state.at[grid_id, (col_name, idx_str)] = int(val)

        list_attr = [
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
            "gsmodel",
            "snowuse",
            "stebbsmethod",
            "rcmethod",
        ]
        for attr in list_attr:
            set_df_value(attr, getattr(self, attr))
        return df_state

    @classmethod
    def from_df_state(cls, df: pd.DataFrame, grid_id: int) -> "ModelPhysics":
        """
        Reconstruct ModelPhysics from a DataFrame state format.

        Args:
            df: DataFrame containing model physics properties
            grid_id: Grid ID for the DataFrame index

        Returns:
            ModelPhysics: Instance of ModelPhysics
        """

        properties = {}

        list_attr = [
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
            "gsmodel",
            "snowuse",
            "stebbsmethod",
            "rcmethod",
        ]

        for attr in list_attr:
            try:
                int_val = int(df.loc[grid_id, (attr, "0")])
                # Handle dimensional config methods specially
                if attr == "netradiationmethod":
                    properties[attr] = NetRadiationMethodConfig.from_code(int_val)
                elif attr == "emissionsmethod":
                    properties[attr] = EmissionsMethodConfig.from_code(int_val)
                else:
                    properties[attr] = RefValue(int_val)
            except KeyError:
                raise ValueError(f"Missing attribute '{attr}' in the DataFrame")

        return cls(**properties)


class OutputFormat(Enum):
    """
    Output file format options.

    TXT: Traditional text files (one per year/grid/group)
    PARQUET: Single Parquet file containing all output data (efficient columnar format)
    """

    TXT = "txt"
    PARQUET = "parquet"

    def __str__(self):
        return self.value


class OutputConfig(BaseModel):
    """Configuration for model output files."""

    model_config = ConfigDict(title="Output Configuration")

    format: OutputFormat = Field(
        default=OutputFormat.TXT,
        description="Output file format. Options: 'txt' for traditional text files (one per year/grid/group), 'parquet' for single Parquet file containing all data",
    )
    freq: Optional[int] = Field(
        default=None,
        description="Output frequency in seconds. Must be a multiple of the model timestep (tstep). If not specified, defaults to 3600 (hourly)",
    )
    groups: Optional[List[str]] = Field(
        default=None,
        description="List of output groups to save (only applies to txt format). Available groups: 'SUEWS', 'DailyState', 'snow', 'ESTM', 'RSL', 'BL', 'debug'. If not specified, defaults to ['SUEWS', 'DailyState']",
    )
    path: Optional[str] = Field(
        default=None,
        description="Output directory path where result files will be saved. If not specified, defaults to current working directory.",
    )

    @field_validator("groups")
    def validate_groups(cls, v):
        if v is not None:
            valid_groups = {"SUEWS", "DailyState", "snow", "ESTM", "RSL", "BL", "debug"}
            dev_groups = {"SPARTACUS", "EHC", "STEBBS"}
            invalid = set(v) - valid_groups - dev_groups
            if invalid:
                raise ValueError(
                    f"Invalid output groups: {invalid}. Valid groups are: {valid_groups}"
                )
        return v


class ModelControl(BaseModel):
    model_config = ConfigDict(title="Model Control")

    tstep: FlexibleRefValue(int) = Field(
        default=300, description="Time step in seconds for model calculations"
    )
    forcing_file: Union[FlexibleRefValue(str), FlexibleRefValue(List[str])] = Field(
        default="forcing.txt",
        description="Path(s) to meteorological forcing data file(s). This can be either: (1) A single file path as a string (e.g., 'forcing.txt'), or (2) A list of file paths (e.g., ['forcing_2020.txt', 'forcing_2021.txt', 'forcing_2022.txt']). When multiple files are provided, they will be automatically concatenated in chronological order. The forcing data contains time-series meteorological measurements that drive SUEWS simulations. For detailed information about required variables, file format, and data preparation guidelines, see :ref:`met_input`.",
    )
    kdownzen: Optional[FlexibleRefValue(int)] = Field(
        default=None,
        description="Use zenithal correction for downward shortwave radiation",
        json_schema_extra={"internal_only": True},
    )
    output_file: Union[str, OutputConfig] = Field(
        default="output.txt",
        description="Output file configuration. DEPRECATED: String values are ignored and will issue a warning. Please use an OutputConfig object specifying format ('txt' or 'parquet'), frequency (seconds, must be multiple of tstep), and groups to save (for txt format only). Example: {'format': 'parquet', 'freq': 3600} or {'format': 'txt', 'freq': 1800, 'groups': ['SUEWS', 'DailyState', 'ESTM']}. For detailed information about output variables and file structure, see :ref:`output_files`.",
    )
    # daylightsaving_method: int
    diagnose: FlexibleRefValue(int) = Field(
        default=0,
        description="Level of diagnostic output (0=none, 1=basic, 2=detailed)",
        json_schema_extra={"internal_only": True},
    )
    start_time: Optional[str] = Field(
        default=None,
        description="Start time of model run. If None use forcing data bounds.",
    )
    end_time: Optional[str] = Field(
        default=None,
        description="End time of model run. If None use forcing data bounds.",
    )

    ref: Optional[Reference] = None

    @field_validator("tstep", "diagnose", mode="after")
    def validate_int_float(cls, v):
        if isinstance(v, (np.float64, np.float32)):
            return float(v)
        elif isinstance(v, (np.int64, np.int32)):
            return int(v)
        return v

    def to_df_state(self, grid_id: int) -> pd.DataFrame:
        """Convert model control properties to DataFrame state format."""
        df_state = init_df_state(grid_id)

        # Helper function to set values in DataFrame
        def set_df_value(col_name: str, value: float):
            idx_str = "0"
            if (col_name, idx_str) not in df_state.columns:
                # df_state[(col_name, idx_str)] = np.nan
                df_state[(col_name, idx_str)] = None
            df_state.at[grid_id, (col_name, idx_str)] = value

        list_attr = ["tstep", "diagnose"]
        for attr in list_attr:
            value = getattr(self, attr)
            # Extract value from RefValue if needed
            val = value.value if isinstance(value, RefValue) else value
            set_df_value(attr, val)
        return df_state

    @classmethod
    def from_df_state(cls, df: pd.DataFrame, grid_id: int) -> "ModelControl":
        """Reconstruct model control properties from DataFrame state format."""
        instance = cls()
        for attr in ["tstep", "diagnose"]:
            value = df.loc[grid_id, (attr, "0")]
            setattr(
                instance,
                attr,
                int(value) if isinstance(value, (np.int64, np.int32)) else value,
            )
        return instance


class Model(BaseModel):
    model_config = ConfigDict(title="Model Configuration")

    control: ModelControl = Field(
        default_factory=ModelControl,
        description="Model control parameters including timestep, output options, etc.",
    )
    physics: ModelPhysics = Field(
        default_factory=ModelPhysics,
        description="Model physics parameters including surface properties, coefficients, etc.",
    )

    def to_df_state(self, grid_id: int) -> pd.DataFrame:
        """Convert model to DataFrame state format"""
        df_state = init_df_state(grid_id)
        df_control = self.control.to_df_state(grid_id)
        df_physics = self.physics.to_df_state(grid_id)
        df_state = pd.concat([df_state, df_control, df_physics], axis=1)
        return df_state

    @classmethod
    def from_df_state(cls, df: pd.DataFrame, grid_id: int) -> "Model":
        """Reconstruct Model from DataFrame state format."""
        # Extract control and physics parameters
        control = ModelControl.from_df_state(df, grid_id)
        physics = ModelPhysics.from_df_state(df, grid_id)

        # Create an instance using the extracted parameters
        return cls(control=control, physics=physics)
