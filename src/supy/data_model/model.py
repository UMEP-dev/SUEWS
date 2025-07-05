import yaml
from typing import Optional, Union, List
import numpy as np
from pydantic import ConfigDict, BaseModel, Field, field_validator, model_validator
import pandas as pd
from enum import Enum

from .type import RefValue, Reference, FlexibleRefValue
from .type import init_df_state


class EmissionsMethod(Enum):
    """
    Method for calculating anthropogenic heat flux (QF) and CO2 emissions.

    0: NO_EMISSIONS - Uses observed QF values from forcing file. Set to zero in forcing file to exclude QF from energy balance
    1: L11 - Loridan et al. (2011) SAHP method. Linear relation with air temperature, weekday/weekend profiles, scales with population density
    2: J11 - Järvi et al. (2011) SAHP_2 method. Uses heating/cooling degree days, weekday/weekend differences via profiles and coefficients
    3: L11_UPDATED - Modified Loridan method using daily mean air temperature instead of instantaneous values
    4: J19 - Järvi et al. (2019) method. Includes building energy use, human metabolism, and traffic contributions
    5: J19_UPDATED - As method 4 but also calculates CO2 emissions (biogenic and anthropogenic components)
    """

    # just a demo to show how to use Enum for emissionsmethod
    NO_EMISSIONS = 0
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
    1: LDOWN_OBSERVED - Models Q* using observed longwave down radiation (L↓) from forcing file
    2: LDOWN_CLOUD - Models Q* with L↓ estimated from cloud cover fraction
    3: LDOWN_AIR - Models Q* with L↓ estimated from air temperature and relative humidity (recommended for basic runs)
    11-13: Surface temperature variants of methods 1-3 (not recommended)
    100-300: Zenith angle correction variants with NARP output (not recommended)
    1001-1003: SPARTACUS-Surface integration variants (experimental)
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
    5: ESTM_EXTENDED - Extended ESTM with separate roof/wall/ground temperatures
    6: OHM_ENHANCED - OHM with enhanced parameterisation
    """

    OBSERVED = 0
    OHM_WITHOUT_QF = 1
    ANOHM = 3
    ESTM = 4
    ESTM_EXTENDED = 5
    OHM_ENHANCED = 6

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
    1: INCLUDE - Use Q*+QF (required when StorageHeatMethod=2)
    """

    EXCLUDE = 0
    INCLUDE = 1

    def __int__(self):
        return self.value

    def __repr__(self):
        return str(self.value)


class RoughnessMethod(Enum):
    """
    Method for calculating aerodynamic roughness length (z0m) and thermal roughness length (z0h).

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


class StabilityMethod(Enum):
    """
    Atmospheric stability correction functions for momentum and heat fluxes.

    0: NOT_USED - Reserved
    1: NOT_USED2 - Reserved
    2: HOEGSTROM - Dyer (1974)/Högström (1988) for momentum, Van Ulden & Holtslag (1985) for stable conditions (not recommended)
    3: CAMPBELL_NORMAN - Campbell & Norman (1998) formulations for both momentum and heat (recommended)
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
    Method for calculating near-surface meteorological diagnostics (2m temperature, 2m humidity, 10m wind speed).

    0: MOST (Monin-Obukhov Similarity Theory) - Appropriate for relatively homogeneous, flat surfaces
    1: RST (Roughness Sublayer Theory) - Appropriate for heterogeneous urban surfaces with tall roughness elements
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

    0: ZERO - FAI set to zero (typically for non-urban areas)
    1: FIXED - Fixed FAI from site parameters
    2: VARIABLE - Variable FAI based on vegetation LAI changes
    """

    ZERO = 0  # Not documented
    FIXED = 1  # Fixed frontal area index
    VARIABLE = 2  # Variable frontal area index based on vegetation state

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
    Method for incorporating local environmental feedbacks on surface processes, particularly vegetation phenology
    and evapotranspiration responses to urban heat island effects.

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


class SnowUse(Enum):
    """
    Controls snow process calculations.

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
    RoughnessMethod,
    StabilityMethod,
    SMDMethod,
    WaterUseMethod,
    RSLMethod,
    FAIMethod,
    RSLLevel,
    GSModel,
    StebbsMethod,
    SnowUse,
    OhmIncQf,
]:
    yaml.add_representer(enum_class, yaml_equivalent_of_default)


class ModelPhysics(BaseModel):
    """
    Model physics configuration options.

    Key method interactions:

    - diagmethod: Determines HOW near-surface values (2m temp, 10m wind) are calculated from forcing data
    - stabilitymethod: Provides stability correction functions used BY diagmethod calculations
    - localclimatemethod: Uses the near-surface values FROM diagmethod to modify vegetation processes
    - gsmodel: Stomatal conductance model that may be influenced by localclimatemethod adjustments
    """

    netradiationmethod: FlexibleRefValue(NetRadiationMethod) = Field(
        default=NetRadiationMethod.LDOWN_AIR,
        description="Method for calculating net all-wave radiation (Q*). Options: 0 (OBSERVED) = Uses observed Q* from forcing file; 1 (LDOWN_OBSERVED) = Models Q* using observed L↓; 2 (LDOWN_CLOUD) = Models Q* with L↓ from cloud cover; 3 (LDOWN_AIR) = Models Q* with L↓ from air temp and RH (recommended); 11 (LDOWN_SURFACE) = Surface temp variant of method 1 (not recommended); 12 (LDOWN_CLOUD_SURFACE) = Surface temp variant of method 2 (not recommended); 13 (LDOWN_AIR_SURFACE) = Surface temp variant of method 3 (not recommended); 100 (LDOWN_ZENITH) = Zenith angle variant of method 1; 200 (LDOWN_CLOUD_ZENITH) = Zenith angle variant of method 2; 300 (LDOWN_AIR_ZENITH) = Zenith angle variant of method 3; 1001 (LDOWN_SS_OBSERVED) = SPARTACUS-Surface variant of method 1 (experimental); 1002 (LDOWN_SS_CLOUD) = SPARTACUS-Surface variant of method 2 (experimental); 1003 (LDOWN_SS_AIR) = SPARTACUS-Surface variant of method 3 (experimental)",
        json_schema_extra={"unit": "dimensionless"},
    )
    emissionsmethod: FlexibleRefValue(EmissionsMethod) = Field(
        default=EmissionsMethod.J11,
        description="Method for calculating anthropogenic heat flux (QF) and CO2 emissions. Options: 0 (NO_EMISSIONS) = Observed QF from forcing file; 1 (L11) = Loridan et al. 2011 linear temp relation; 2 (J11) = Järvi et al. 2011 with HDD/CDD; 3 (L11_UPDATED) = Loridan with daily mean temp; 4 (J19) = Järvi et al. 2019 including metabolism and traffic; 5 (J19_UPDATED) = As 4 with CO2 emissions",
        json_schema_extra={"unit": "dimensionless"},
    )
    storageheatmethod: FlexibleRefValue(StorageHeatMethod) = Field(
        default=StorageHeatMethod.OHM_WITHOUT_QF,
        description="Method for calculating storage heat flux (ΔQS). Options: 0 (OBSERVED) = Uses observed ΔQS from forcing file; 1 (OHM_WITHOUT_QF) = Objective Hysteresis Model using Q* only; 3 (ANOHM) = Analytical OHM (not recommended); 4 (ESTM) = Element Surface Temperature Method (not recommended); 5 (ESTM_EXTENDED) = Extended ESTM with separate facet temps; 6 (OHM_ENHANCED) = Enhanced OHM parameterisation",
        json_schema_extra={"unit": "dimensionless"},
    )
    ohmincqf: FlexibleRefValue(OhmIncQf) = Field(
        default=OhmIncQf.EXCLUDE,
        description="Controls inclusion of anthropogenic heat flux in OHM storage heat calculations. Options: 0 (EXCLUDE) = Use Q* only (required when StorageHeatMethod=1); 1 (INCLUDE) = Use Q*+QF (required when StorageHeatMethod=2)",
        json_schema_extra={"unit": "dimensionless"},
    )
    roughlenmommethod: FlexibleRefValue(RoughnessMethod) = Field(
        default=RoughnessMethod.VARIABLE,
        description="Method for calculating momentum roughness length (z0m). Options: 1 (FIXED) = Fixed from site parameters; 2 (VARIABLE) = Varies with vegetation LAI; 3 (MACDONALD) = MacDonald et al. 1998 morphometric method; 4 (LAMBDAP_DEPENDENT) = Varies with plan area fraction; 5 (ALTERNATIVE) = Alternative variable method",
        json_schema_extra={"unit": "dimensionless"},
    )
    roughlenheatmethod: FlexibleRefValue(RoughnessMethod) = Field(
        default=RoughnessMethod.VARIABLE,
        description="Method for calculating thermal roughness length (z0h). Options: 1 (FIXED) = Fixed from site parameters; 2 (VARIABLE) = Varies with vegetation LAI; 3 (MACDONALD) = MacDonald et al. 1998 morphometric method; 4 (LAMBDAP_DEPENDENT) = Varies with plan area fraction; 5 (ALTERNATIVE) = Alternative variable method",
        json_schema_extra={"unit": "dimensionless"},
    )
    stabilitymethod: FlexibleRefValue(StabilityMethod) = Field(
        default=StabilityMethod.CAMPBELL_NORMAN,
        description="Atmospheric stability correction functions for momentum and heat fluxes. Options: 0 = Reserved; 1 = Reserved; 2 (HOEGSTROM) = Dyer/Högström formulations (not recommended); 3 (CAMPBELL_NORMAN) = Campbell & Norman 1998 formulations (recommended); 4 (BUSINGER_HOEGSTROM) = Businger/Högström formulations (not recommended)",
        json_schema_extra={"unit": "dimensionless"},
    )
    smdmethod: FlexibleRefValue(SMDMethod) = Field(
        default=SMDMethod.MODELLED,
        description="Method for determining soil moisture deficit (SMD). Options: 0 (MODELLED) = Calculated from water balance using soil parameters; 1 (OBSERVED_VOLUMETRIC) = Uses observed volumetric soil moisture (m³/m³) from forcing file; 2 (OBSERVED_GRAVIMETRIC) = Uses observed gravimetric soil moisture (kg/kg) from forcing file",
        json_schema_extra={"unit": "dimensionless"},
    )
    waterusemethod: FlexibleRefValue(WaterUseMethod) = Field(
        default=WaterUseMethod.MODELLED,
        description="Method for determining external water use (irrigation). Options: 0 (MODELLED) = Calculated based on soil moisture deficit and irrigation parameters; 1 (OBSERVED) = Uses observed water use values from forcing file",
        json_schema_extra={"unit": "dimensionless"},
    )
    rslmethod: FlexibleRefValue(RSLMethod) = Field(
        default=RSLMethod.VARIABLE,
        description="Method for calculating near-surface meteorological diagnostics (2m temperature, 2m humidity, 10m wind speed). Options: 0 (MOST) = Monin-Obukhov Similarity Theory for homogeneous surfaces; 1 (RST) = Roughness Sublayer Theory for heterogeneous urban surfaces; 2 (VARIABLE) = Automatic selection based on surface morphology (plan area index, frontal area index, and roughness element heights)",
        json_schema_extra={"unit": "dimensionless"},
    )
    faimethod: FlexibleRefValue(FAIMethod) = Field(
        default=FAIMethod.FIXED,
        description="Method for calculating frontal area index (FAI) - the ratio of frontal area to plan area. Options: 0 (ZERO) = FAI set to zero (non-urban areas); 1 (FIXED) = Fixed FAI from site parameters; 2 (VARIABLE) = Variable FAI based on vegetation LAI changes",
        json_schema_extra={"unit": "dimensionless"},
    )
    rsllevel: FlexibleRefValue(RSLLevel) = Field(
        default=RSLLevel.NONE,
        description="Method for incorporating urban microclimate feedbacks on vegetation and evapotranspiration. Options: 0 (NONE) = No local climate adjustments, use forcing file meteorology directly; 1 (BASIC) = Simple adjustments for urban temperature effects on leaf area index and growing degree days; 2 (DETAILED) = Comprehensive feedbacks including moisture stress, urban CO2 dome effects, and modified phenology cycles",
        json_schema_extra={"unit": "dimensionless"},
    )
    gsmodel: FlexibleRefValue(GSModel) = Field(
        default=GSModel.WARD,
        description="Stomatal conductance parameterisation method for vegetation surfaces. Options: 1 (JARVI) = Original parameterisation (Järvi et al. 2011) based on environmental controls; 2 (WARD) = Updated parameterisation (Ward et al. 2016) with improved temperature and VPD responses",
        json_schema_extra={"unit": "dimensionless"},
    )
    snowuse: FlexibleRefValue(SnowUse) = Field(
        default=SnowUse.DISABLED,
        description="Controls snow process calculations. Options: 0 (DISABLED) = Snow processes not included; 1 (ENABLED) = Snow accumulation, melt, and albedo effects included",
        json_schema_extra={"unit": "dimensionless"},
    )
    stebbsmethod: FlexibleRefValue(StebbsMethod) = Field(
        default=StebbsMethod.NONE,
        description="Surface Temperature Energy Balance Based Scheme (STEBBS) for facet temperatures. Options: 0 (NONE) = STEBBS disabled; 1 (DEFAULT) = STEBBS with default parameters; 2 (PROVIDED) = STEBBS with user-specified parameters",
        json_schema_extra={"unit": "dimensionless"},
    )

    ref: Optional[Reference] = None

    @model_validator(mode="after")
    def check_all(self) -> "ModelPhysics":
        """
        Collect and aggregate all validation checks for ModelPhysics,
        so multiple errors (if any) can be raised together
        """
        errors = []
        # Get values for comparison
        storageheatmethod_val = (
            self.storageheatmethod.value
            if isinstance(self.storageheatmethod, RefValue)
            else self.storageheatmethod
        )
        ohmincqf_val = (
            self.ohmincqf.value
            if isinstance(self.ohmincqf, RefValue)
            else self.ohmincqf
        )
        snowuse_val = (
            self.snowuse.value if isinstance(self.snowuse, RefValue) else self.snowuse
        )

        # storageheatmethod check
        if storageheatmethod_val == 1 and ohmincqf_val != 0:
            errors.append(
                f"\nStorageHeatMethod is set to {storageheatmethod_val} and OhmIncQf is set to {ohmincqf_val}.\n"
                f"You should switch to OhmIncQf=0.\n"
            )
        elif storageheatmethod_val == 2 and ohmincqf_val != 1:
            errors.append(
                f"\nStorageHeatMethod is set to {storageheatmethod_val} and OhmIncQf is set to {ohmincqf_val}.\n"
                f"You should switch to OhmIncQf=1.\n"
            )

        # snowusemethod check
        if snowuse_val == 1:
            errors.append(
                f"\nSnowUse is set to {snowuse_val}.\n"
                f"There are no checks implemented for this case (snow calculations included in the run).\n"
                f"You should switch to SnowUse=0.\n"
            )

        # emissionsmethod check
        # if self.emissionsmethod == 45:
        #     errors.append(
        #         f"\nEmissionsMethod is set to {self.emissionsmethod}.\n"
        #         f"There are no checks implemented for this case (CO2 calculations included in the run).\n"
        #         f"You should switch to EmissionsMethod=0, 1, 2, 3, or 4.\n"
        #     )

        if errors:
            raise ValueError("\n".join(errors))

        return self

    # We then need to set to 0 (or None) all the CO2-related parameters or rules
    # in the code and return them accordingly in the yml file.

    def to_df_state(self, grid_id: int) -> pd.DataFrame:
        """Convert model physics properties to DataFrame state format."""
        df_state = init_df_state(grid_id)

        # Helper function to set values in DataFrame
        def set_df_value(col_name: str, value: float):
            idx_str = "0"
            if (col_name, idx_str) not in df_state.columns:
                # df_state[(col_name, idx_str)] = np.nan
                df_state[(col_name, idx_str)] = None
            val = value.value if isinstance(value, RefValue) else value
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
        ]
        for attr in list_attr:
            if attr == "rslmethod":
                set_df_value("diagmethod", getattr(self, attr))
                continue
            if attr == "rsllevel":
                set_df_value("localclimatemethod", getattr(self, attr))
                continue
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
        ]

        for attr in list_attr:
            try:
                if attr == "rslmethod":
                    properties[attr] = RefValue(
                        int(df.loc[grid_id, ("diagmethod", "0")])
                    )
                    continue
                if attr == "rsllevel":
                    properties[attr] = RefValue(
                        int(df.loc[grid_id, ("localclimatemethod", "0")])
                    )
                    continue
                properties[attr] = RefValue(int(df.loc[grid_id, (attr, "0")]))
            except KeyError:
                raise ValueError(f"Missing attribute '{attr}' in the DataFrame")

        return cls(**properties)


class OutputFormat(Enum):
    '''
    Output file format options.
    
    TXT: Traditional text files (one per year/grid/group)
    PARQUET: Single Parquet file containing all output data (efficient columnar format)
    '''
    TXT = "txt"
    PARQUET = "parquet"
    
    def __str__(self):
        return self.value


class OutputConfig(BaseModel):
    '''Configuration for model output files.'''
    
    format: OutputFormat = Field(
        default=OutputFormat.TXT,
        description="Output file format. Options: 'txt' for traditional text files (one per year/grid/group), 'parquet' for single Parquet file containing all data"
    )
    freq: Optional[int] = Field(
        default=None,
        description="Output frequency in seconds. Must be a multiple of the model timestep (tstep). If not specified, defaults to 3600 (hourly)"
    )
    groups: Optional[List[str]] = Field(
        default=None,
        description="List of output groups to save (only applies to txt format). Available groups: 'SUEWS', 'DailyState', 'snow', 'ESTM', 'RSL', 'BL', 'debug'. If not specified, defaults to ['SUEWS', 'DailyState']"
    )
    
    @field_validator('groups')
    def validate_groups(cls, v):
        if v is not None:
            valid_groups = {'SUEWS', 'DailyState', 'snow', 'ESTM', 'RSL', 'BL', 'debug'}
            invalid = set(v) - valid_groups
            if invalid:
                raise ValueError(f"Invalid output groups: {invalid}. Valid groups are: {valid_groups}")
        return v


class ModelControl(BaseModel):
    tstep: int = Field(
        default=300, description="Time step in seconds for model calculations"
    )
    forcing_file: Union[FlexibleRefValue(str), List[str]] = Field(
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
        description="Output file configuration. DEPRECATED: String values are ignored and will issue a warning. Please use an OutputConfig object specifying format ('txt' or 'parquet'), frequency (seconds, must be multiple of tstep), and groups to save (for txt format only). Example: {'format': 'parquet', 'freq': 3600} or {'format': 'txt', 'freq': 1800, 'groups': ['SUEWS', 'DailyState', 'ESTM']}. For detailed information about output variables and file structure, see :ref:`output_files`."
    )
    # daylightsaving_method: int
    diagnose: int = Field(
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
            set_df_value(attr, getattr(self, attr))
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
    control: ModelControl = Field(
        default_factory=ModelControl,
        description="Model control parameters including timestep, output options, etc.",
    )
    physics: ModelPhysics = Field(
        default_factory=ModelPhysics,
        description="Model physics parameters including surface properties, coefficients, etc.",
    )

    @model_validator(mode="after")
    def validate_radiation_method(self) -> "Model":
        netradiationmethod_val = (
            self.physics.netradiationmethod.value
            if isinstance(self.physics.netradiationmethod, RefValue)
            else self.physics.netradiationmethod
        )
        forcing_file_val = (
            self.control.forcing_file.value
            if isinstance(self.control.forcing_file, RefValue)
            else self.control.forcing_file
        )

        if netradiationmethod_val == 1 and forcing_file_val == "forcing.txt":
            raise ValueError(
                "NetRadiationMethod is set to 1 (using observed Ldown). "
                "The sample forcing file lacks observed Ldown. Use netradiation = 3 for sample forcing. "
                "If not using sample forcing, ensure that the forcing file contains Ldown and rename from forcing.txt."
                # TODO: This is a temporary solution. We need to provide a better way to catch this.
            )
        return self
    
    @model_validator(mode="after")
    def validate_output_config(self) -> "Model":
        """Validate output configuration, especially frequency vs timestep."""
        if isinstance(self.control.output_file, OutputConfig):
            output_config = self.control.output_file
            if output_config.freq is not None:
                tstep = self.control.tstep
                if output_config.freq % tstep != 0:
                    raise ValueError(
                        f"Output frequency ({output_config.freq}s) must be a multiple of timestep ({tstep}s)"
                    )
        elif isinstance(self.control.output_file, str) and self.control.output_file != "output.txt":
            # Issue warning for non-default string values
            import warnings
            warnings.warn(
                f"The 'output_file' parameter with value '{self.control.output_file}' is deprecated and was never used. "
                "Please use the new OutputConfig format or remove this parameter. "
                "Example: output_file: {format: 'parquet', freq: 3600}",
                DeprecationWarning,
                stacklevel=3
            )
        return self

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
