import yaml
from typing import Optional, Union, List
import numpy as np
from pydantic import ConfigDict, BaseModel, Field, field_validator, model_validator
import pandas as pd
from enum import Enum
import inspect

from .type import RefValue, Reference, FlexibleRefValue, df_from_cols


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

    netradiationmethod: FlexibleRefValue(NetRadiationMethod) = Field(
        default=NetRadiationMethod.LDOWN_AIR,
        description=_enum_description(NetRadiationMethod),
        json_schema_extra={"unit": "dimensionless"},
    )
    emissionsmethod: FlexibleRefValue(EmissionsMethod) = Field(
        default=EmissionsMethod.J11,
        description=_enum_description(EmissionsMethod),
        json_schema_extra={"unit": "dimensionless"},
    )
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

    # We then need to set to 0 (or None) all the CO2-related parameters or rules
    # in the code and return them accordingly in the yml file.

    def to_df_state(self, grid_id: int) -> pd.DataFrame:
        """Convert model physics properties to DataFrame state format."""
        cols = {("gridiv", "0"): grid_id}
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
            value = getattr(self, attr)
            val = value.value if isinstance(value, RefValue) else value
            cols[(attr, "0")] = int(val)
        return df_from_cols(cols, index=pd.Index([grid_id], name="grid"))

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
                properties[attr] = RefValue(int(df.loc[grid_id, (attr, "0")]))
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
        cols = {("gridiv", "0"): grid_id}
        list_attr = ["tstep", "diagnose"]
        for attr in list_attr:
            value = getattr(self, attr)
            # Extract value from RefValue if needed
            val = value.value if isinstance(value, RefValue) else value
            cols[(attr, "0")] = val
        return df_from_cols(cols, index=pd.Index([grid_id], name="grid"))

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
        df_state = df_from_cols(
            {("gridiv", "0"): grid_id}, index=pd.Index([grid_id], name="grid")
        )
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
