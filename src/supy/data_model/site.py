import math
from typing import Optional
from pydantic import BaseModel, Field, PrivateAttr, model_validator
from .type import ValueWithDOI, Reference
from .profile import HourlyProfile, DayProfile, WeeklyProfile
from .type import init_df_state
from .surface import (
    SurfaceType,
    SurfaceProperties,
    PavedProperties,
    BldgsProperties,
    BsoilProperties,
    WaterProperties,
    VerticalLayers,
)
from .human_activity import AnthropogenicEmissions, IrrigationParams
from .hydro import (
    WaterDistribution,
)
from .state import InitialStates

import pandas as pd
from typing import List, Literal, Union, Dict, Tuple


class VegetationParams(BaseModel):
    porosity_id: ValueWithDOI[int]
    gdd_id: ValueWithDOI[int] = Field(description="Growing degree days ID")
    sdd_id: ValueWithDOI[int] = Field(description="Senescence degree days ID")
    lai: Dict[str, Union[ValueWithDOI[float], List[ValueWithDOI[float]]]] = Field(
        description="Leaf area index parameters"
    )
    ie_a: ValueWithDOI[float] = Field(description="Irrigation efficiency coefficient a")
    ie_m: ValueWithDOI[float] = Field(description="Irrigation efficiency coefficient m")

    ref: Optional[Reference] = None


class Conductance(BaseModel):
    g_max: ValueWithDOI[float] = Field(
        default=ValueWithDOI(40.0), description="Maximum conductance"
    )
    g_k: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.6),
        description="Conductance parameter related to incoming solar radiation",
    )
    g_q_base: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.03),
        description="Base value for conductance parameter related to vapor pressure deficit",
    )
    g_q_shape: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.9),
        description="Shape parameter for conductance related to vapor pressure deficit",
    )
    g_t: ValueWithDOI[float] = Field(
        default=ValueWithDOI(30.0),
        description="Conductance parameter related to air temperature",
    )
    g_sm: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.5),
        description="Conductance parameter related to soil moisture",
    )
    kmax: ValueWithDOI[float] = Field(
        default=ValueWithDOI(1200.0), description="Maximum incoming shortwave radiation"
    )
    gsmodel: ValueWithDOI[int] = Field(
        default=ValueWithDOI(1), description="Stomatal conductance model selection"
    )
    s1: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.2), description="Soil moisture threshold parameter"
    )
    s2: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.5), description="Soil moisture threshold parameter"
    )
    tl: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.0), description="Air temperature threshold parameter"
    )
    th: ValueWithDOI[float] = Field(
        default=ValueWithDOI(50.0), description="Air temperature threshold parameter"
    )

    ref: Optional[Reference] = Reference(ref="Test ref", DOI="test doi", ID="test id")

    def to_df_state(self, grid_id: int) -> pd.DataFrame:
        """
        Convert conductance parameters to DataFrame state format.

        Args:
            grid_id (int): Grid ID for the DataFrame index.

        Returns:
            pd.DataFrame: DataFrame containing conductance parameters.
        """

        df_state = init_df_state(grid_id)

        scalar_params = {
            "g_max": self.g_max,
            "g_k": self.g_k,
            "g_q_base": self.g_q_base,
            "g_q_shape": self.g_q_shape,
            "g_t": self.g_t,
            "g_sm": self.g_sm,
            "kmax": self.kmax,
            "gsmodel": self.gsmodel,
            "s1": self.s1,
            "s2": self.s2,
            "tl": self.tl,
            "th": self.th,
        }

        for param_name, value in scalar_params.items():
            df_state.loc[grid_id, (param_name, "0")] = value.value

        return df_state

    @classmethod
    def from_df_state(cls, df: pd.DataFrame, grid_id: int) -> "Conductance":
        """
        Reconstruct Conductance from a DataFrame state format.

        Args:
            df: DataFrame containing conductance parameters
            grid_id: Grid ID for the DataFrame index

        Returns:
            Conductance: Instance of Conductance
        """
        scalar_params = {
            "g_max": df.loc[grid_id, ("g_max", "0")],
            "g_k": df.loc[grid_id, ("g_k", "0")],
            "g_q_base": df.loc[grid_id, ("g_q_base", "0")],
            "g_q_shape": df.loc[grid_id, ("g_q_shape", "0")],
            "g_t": df.loc[grid_id, ("g_t", "0")],
            "g_sm": df.loc[grid_id, ("g_sm", "0")],
            "kmax": df.loc[grid_id, ("kmax", "0")],
            "gsmodel": int(df.loc[grid_id, ("gsmodel", "0")]),
            "s1": df.loc[grid_id, ("s1", "0")],
            "s2": df.loc[grid_id, ("s2", "0")],
            "tl": df.loc[grid_id, ("tl", "0")],
            "th": df.loc[grid_id, ("th", "0")],
        }

        # Convert scalar parameters to ValueWithDOI
        scalar_params = {
            key: ValueWithDOI(value) for key, value in scalar_params.items()
        }

        return cls(**scalar_params)


class LAIPowerCoefficients(BaseModel):
    growth_lai: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.1),
        description="Power coefficient for LAI in growth equation (LAIPower[1])",
    )
    growth_gdd: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.1),
        description="Power coefficient for GDD in growth equation (LAIPower[2])",
    )
    senescence_lai: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.1),
        description="Power coefficient for LAI in senescence equation (LAIPower[3])",
    )
    senescence_sdd: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.1),
        description="Power coefficient for SDD in senescence equation (LAIPower[4])",
    )

    ref: Optional[Reference] = None

    def to_list(self) -> List[float]:
        """Convert to list format for Fortran interface"""
        return [
            self.growth_lai,
            self.growth_gdd,
            self.senescence_lai,
            self.senescence_sdd,
        ]

    def to_df_state(self, grid_id: int, veg_idx: int) -> pd.DataFrame:
        """Convert LAI power coefficients to DataFrame state format.

        Args:
            grid_id: Grid ID for the DataFrame index
            veg_idx: Vegetation index (0: EVETR, 1: DECTR, 2: GRASS)

        Returns:
            pd.DataFrame: DataFrame containing LAI power coefficients
        """
        df_state = init_df_state(grid_id)

        # Helper function to set values in DataFrame
        def set_df_value(col_name: str, indices: Tuple, value: float):
            idx_str = str(indices)
            if (col_name, idx_str) not in df_state.columns:
                # df_state[(col_name, idx_str)] = np.nan
                df_state[(col_name, idx_str)] = None
            df_state.at[grid_id, (col_name, idx_str)] = value

        # Set power coefficients in order
        for i, value in enumerate(self.to_list()):
            set_df_value("laipower", (i, veg_idx), value.value)

        return df_state

    @classmethod
    def from_df_state(
        cls, df: pd.DataFrame, grid_id: int, veg_idx: int
    ) -> "LAIPowerCoefficients":
        """
        Reconstruct LAIPowerCoefficients from DataFrame state format.

        Args:
            df: DataFrame containing LAI power coefficients
            grid_id: Grid ID for the DataFrame index
            veg_idx: Vegetation index (0: EVETR, 1: DECTR, 2: GRASS)

        Returns:
            LAIPowerCoefficients: Instance of LAIPowerCoefficients
        """
        # Map each coefficient to its corresponding index
        coefficients = [
            ValueWithDOI(df.loc[grid_id, ("laipower", f"(0, {veg_idx})")]),
            ValueWithDOI(df.loc[grid_id, ("laipower", f"(1, {veg_idx})")]),
            ValueWithDOI(df.loc[grid_id, ("laipower", f"(2, {veg_idx})")]),
            ValueWithDOI(df.loc[grid_id, ("laipower", f"(3, {veg_idx})")]),
        ]

        # Return the instance with coefficients
        return cls(
            growth_lai=coefficients[0],
            growth_gdd=coefficients[1],
            senescence_lai=coefficients[2],
            senescence_sdd=coefficients[3],
        )


class LAIParams(BaseModel):
    baset: ValueWithDOI[float] = Field(
        default=ValueWithDOI(10.0),
        description="Base Temperature for initiating growing degree days (GDD) for leaf growth [degC]",
    )
    gddfull: ValueWithDOI[float] = Field(
        default=ValueWithDOI(100.0),
        description="Growing degree days (GDD) needed for full capacity of LAI [degC]",
    )
    basete: ValueWithDOI[float] = Field(
        default=ValueWithDOI(10.0),
        description="Base temperature for initiating senescence degree days (SDD) for leaf off [degC]",
    )
    sddfull: ValueWithDOI[float] = Field(
        default=ValueWithDOI(100.0),
        description="Senescence degree days (SDD) needed to initiate leaf off [degC]",
    )
    laimin: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.1), description="Leaf-off wintertime value [m2 m-2]"
    )
    laimax: ValueWithDOI[float] = Field(
        default=ValueWithDOI(10.0), description="Full leaf-on summertime value [m2 m-2]"
    )
    laipower: LAIPowerCoefficients = Field(
        default_factory=LAIPowerCoefficients,
        description="LAI calculation power parameters for growth and senescence",
    )
    laitype: ValueWithDOI[int] = Field(
        default=ValueWithDOI(0),
        description="LAI calculation choice (0: original, 1: new high latitude)",
    )

    ref: Optional[Reference] = None

    @model_validator(mode="after")
    def validate_lai_ranges(self) -> "LAIParams":
        if self.laimin > self.laimax:
            raise ValueError(
                f"laimin ({self.laimin})must be less than or equal to laimax ({self.laimax})."
            )
        if self.baset > self.gddfull:
            raise ValueError(
                f"baset {self.baset} must be less than gddfull ({self.gddfull})."
            )
        return self

    def to_df_state(self, grid_id: int, surf_idx: int) -> pd.DataFrame:
        """Convert LAI parameters to DataFrame state format.

        Args:
            grid_id: Grid ID for the DataFrame index
            surf_idx: Surface index for vegetation (2: EVETR, 3: DECTR, 4: GRASS)

        Returns:
            pd.DataFrame: DataFrame containing LAI parameters
        """
        df_state = init_df_state(grid_id)

        # Adjust index for vegetation surfaces (surface index - 2)
        veg_idx = surf_idx - 2

        # Helper function to set values in DataFrame
        def set_df_value(col_name: str, indices: Union[Tuple, int], value: float):
            idx_str = str(indices) if isinstance(indices, int) else str(indices)
            if (col_name, idx_str) not in df_state.columns:
                # df_state[(col_name, idx_str)] = np.nan
                df_state[(col_name, idx_str)] = None
            df_state.at[grid_id, (col_name, idx_str)] = value

        # Set basic LAI parameters
        lai_params = {
            "baset": self.baset,
            "gddfull": self.gddfull,
            "basete": self.basete,
            "sddfull": self.sddfull,
            "laimin": self.laimin,
            "laimax": self.laimax,
            "laitype": self.laitype,
        }

        for param, value in lai_params.items():
            set_df_value(param, (veg_idx,), value.value)

        # Add LAI power coefficients using the LAIPowerCoefficients to_df_state method
        if self.laipower:
            df_power = self.laipower.to_df_state(grid_id, veg_idx)
            # Merge power coefficients into main DataFrame
            for col in df_power.columns:
                if col[0] != "grid_iv":  # Skip the grid_iv column
                    df_state[col] = df_power[col]

        return df_state

    @classmethod
    def from_df_state(
        cls, df: pd.DataFrame, grid_id: int, surf_idx: int
    ) -> "LAIParams":
        """
        Reconstruct LAIParams from DataFrame state format.

        Args:
            df (pd.DataFrame): DataFrame containing LAI parameters.
            grid_id (int): Grid ID for the DataFrame index.
            surf_idx (int): Surface index for vegetation (2: EVETR, 3: DECTR, 4: GRASS).

        Returns:
            LAIParams: Instance of LAIParams.
        """
        # Adjust index for vegetation surfaces (surface index - 2)
        veg_idx = surf_idx - 2

        # Helper function to extract values from DataFrame
        def get_df_value(col_name: str, indices: Union[Tuple, int]) -> float:
            idx_str = str(indices) if isinstance(indices, int) else str(indices)
            return df.loc[grid_id, (col_name, idx_str)]

        # Extract basic LAI parameters
        lai_params = {
            "baset": get_df_value("baset", (veg_idx,)),
            "gddfull": get_df_value("gddfull", (veg_idx,)),
            "basete": get_df_value("basete", (veg_idx,)),
            "sddfull": get_df_value("sddfull", (veg_idx,)),
            "laimin": get_df_value("laimin", (veg_idx,)),
            "laimax": get_df_value("laimax", (veg_idx,)),
            "laitype": int(get_df_value("laitype", (veg_idx,))),
        }

        # Convert scalar parameters to ValueWithDOI
        lai_params = {key: ValueWithDOI(value) for key, value in lai_params.items()}

        # Extract LAI power coefficients
        laipower = LAIPowerCoefficients.from_df_state(df, grid_id, veg_idx)

        return cls(**lai_params, laipower=laipower)


class VegetatedSurfaceProperties(SurfaceProperties):
    alb: ValueWithDOI[float] = Field(
        ge=0, le=1, description="Albedo", default=ValueWithDOI(0.2)
    )
    alb_min: ValueWithDOI[float] = Field(
        ge=0, le=1, description="Minimum albedo", default=ValueWithDOI(0.2)
    )
    alb_max: ValueWithDOI[float] = Field(
        ge=0, le=1, description="Maximum albedo", default=ValueWithDOI(0.3)
    )
    beta_bioco2: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.6), description="Biogenic CO2 exchange coefficient"
    )
    beta_enh_bioco2: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.7),
        description="Enhanced biogenic CO2 exchange coefficient",
    )
    alpha_bioco2: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.8), description="Biogenic CO2 exchange coefficient"
    )
    alpha_enh_bioco2: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.9),
        description="Enhanced biogenic CO2 exchange coefficient",
    )
    resp_a: ValueWithDOI[float] = Field(
        default=ValueWithDOI(1.0), description="Respiration coefficient"
    )
    resp_b: ValueWithDOI[float] = Field(
        default=ValueWithDOI(1.1), description="Respiration coefficient"
    )
    theta_bioco2: ValueWithDOI[float] = Field(
        default=ValueWithDOI(1.2), description="Biogenic CO2 exchange coefficient"
    )
    maxconductance: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.5), description="Maximum surface conductance"
    )
    min_res_bioco2: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.1), description="Minimum respiratory biogenic CO2"
    )
    lai: LAIParams = Field(
        default_factory=LAIParams, description="Leaf area index parameters"
    )
    ie_a: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.5),
        description="Irrigation efficiency coefficient-automatic",
    )
    ie_m: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.6),
        description="Irrigation efficiency coefficient-manual",
    )

    ref: Optional[Reference] = None

    @model_validator(mode="after")
    def validate_albedo_range(self) -> "VegetatedSurfaceProperties":
        if self.alb_min > self.alb_max:
            raise ValueError(
                f"alb_min (input {self.alb_min}) must be less than or equal to alb_max (entered {self.alb_max})."
            )
        return self

    def to_df_state(self, grid_id: int) -> pd.DataFrame:
        """Convert vegetated surface properties to DataFrame state format."""
        # Get base properties
        df_state = super().to_df_state(grid_id)

        # Add vegetation-specific properties
        surf_idx = self.get_surface_index()

        # Helper function to set values in DataFrame
        def set_df_value(col_name: str, idx_str: str, value: float):
            if (col_name, idx_str) not in df_state.columns:
                # df_state[(col_name, idx_str)] = np.nan
                df_state[(col_name, idx_str)] = None
            df_state.loc[grid_id, (col_name, idx_str)] = value

        # add ordinary float properties
        for attr in [
            "alb",
            # "alb_min",
            # "alb_max",
            "beta_bioco2",
            "beta_enh_bioco2",
            "alpha_bioco2",
            "alpha_enh_bioco2",
            "resp_a",
            "resp_b",
            "theta_bioco2",
            "maxconductance",
            "min_res_bioco2",
            "ie_a",
            "ie_m",
        ]:
            set_df_value(attr, f"({surf_idx-2},)", getattr(self, attr).value)

        df_lai = self.lai.to_df_state(grid_id, surf_idx)
        df_state = pd.concat([df_state, df_lai], axis=1).sort_index(axis=1)

        return df_state

    @classmethod
    def from_df_state(cls, df: pd.DataFrame, grid_id: int, surf_idx: int) -> "VegetatedSurfaceProperties":
        """Reconstruct vegetated surface properties from DataFrame state format."""
        instance = super().from_df_state(df, grid_id, surf_idx)
        # add ordinary float properties
        for attr in [
            "alb",
            # "alb_min",
            # "alb_max",
            "beta_bioco2",
            "beta_enh_bioco2",
            "alpha_bioco2",
            "alpha_enh_bioco2",
            "resp_a",
            "resp_b",
            "theta_bioco2",
            "maxconductance",
            "min_res_bioco2",
            "ie_a",
            "ie_m",
        ]:
            setattr(instance, attr, ValueWithDOI(df.loc[grid_id, (attr, f"({surf_idx-2},)")]))

        instance.lai = LAIParams.from_df_state(df, grid_id, surf_idx)

        return instance


class EvetrProperties(VegetatedSurfaceProperties):  # TODO: Move waterdist VWD here?
    alb: ValueWithDOI[float] = Field(
        ge=0, le=1, default=ValueWithDOI(0.2), description="Albedo"
    )
    faievetree: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.1), description="Frontal area index of evergreen trees"
    )
    evetreeh: ValueWithDOI[float] = Field(
        default=ValueWithDOI(15.0), description="Evergreen tree height"
    )
    _surface_type: Literal[SurfaceType.EVETR] = SurfaceType.EVETR
    waterdist: WaterDistribution = Field(
        default_factory=lambda: WaterDistribution(SurfaceType.EVETR),
        description="Water distribution for evergreen trees",
    )

    ref: Optional[Reference] = None

    def to_df_state(self, grid_id: int) -> pd.DataFrame:
        """Convert evergreen tree properties to DataFrame state format."""
        # Get base properties from parent
        df_state = super().to_df_state(grid_id)
        surf_idx = self.get_surface_index()

        # Helper function to set values in DataFrame
        def set_df_value(col_name: str, value: float):
            idx_str = f"({surf_idx},)"
            if (col_name, idx_str) not in df_state.columns:
                # df_state[(col_name, idx_str)] = np.nan
                df_state[(col_name, idx_str)] = None
            df_state.loc[grid_id, (col_name, idx_str)] = value

        # Add all non-inherited properties
        list_properties = ["faievetree", "evetreeh"]
        for attr in list_properties:
            df_state.loc[grid_id, (attr, "0")] = getattr(self, attr).value

        # specific properties
        df_state.loc[grid_id, ("alb", "(2,)")] = self.alb.value
        df_state.loc[grid_id, ("albmin_evetr", "0")] = self.alb_min.value
        df_state.loc[grid_id, ("albmax_evetr", "0")] = self.alb_max.value

        return df_state

    @classmethod
    def from_df_state(cls, df: pd.DataFrame, grid_id: int) -> "EvetrProperties":
        """Reconstruct evergreen tree properties from DataFrame state format."""
        surf_idx = 2
        instance = super().from_df_state(df, grid_id, surf_idx)

        instance.alb = ValueWithDOI(df.loc[grid_id, ("alb", "(2,)")])
        instance.faievetree = ValueWithDOI(df.loc[grid_id, ("faievetree", "0")])
        instance.evetreeh = ValueWithDOI(df.loc[grid_id, ("evetreeh", "0")])

        instance.alb_min = ValueWithDOI(df.loc[grid_id, ("albmin_evetr", "0")])
        instance.alb_max = ValueWithDOI(df.loc[grid_id, ("albmax_evetr", "0")])

        return instance


class DectrProperties(VegetatedSurfaceProperties):
    alb: ValueWithDOI[float] = Field(
        ge=0, le=1, default=ValueWithDOI(0.2), description="Albedo"
    )
    faidectree: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.1), description="Frontal area index of deciduous trees"
    )
    dectreeh: ValueWithDOI[float] = Field(
        default=ValueWithDOI(15.0), description="Deciduous tree height"
    )
    pormin_dec: ValueWithDOI[float] = Field(
        ge=0.1, le=0.9, default=ValueWithDOI(0.2), description="Minimum porosity"
    )  # pormin_dec cannot be less than 0.1 and greater than 0.9
    pormax_dec: ValueWithDOI[float] = Field(
        ge=0.1, le=0.9, default=ValueWithDOI(0.6), description="Maximum porosity"
    )  # pormax_dec cannot be less than 0.1 and greater than 0.9
    capmax_dec: ValueWithDOI[float] = Field(
        default=ValueWithDOI(100.0), description="Maximum capacity"
    )
    capmin_dec: ValueWithDOI[float] = Field(
        default=ValueWithDOI(10.0), description="Minimum capacity"
    )
    _surface_type: Literal[SurfaceType.DECTR] = SurfaceType.DECTR
    waterdist: WaterDistribution = Field(
        default_factory=lambda: WaterDistribution(SurfaceType.DECTR),
        description="Water distribution for deciduous trees",
    )

    ref: Optional[Reference] = None

    @model_validator(mode="after")
    def validate_porosity_range(self) -> "DectrProperties":
        if self.pormin_dec >= self.pormax_dec:
            raise ValueError(
                f"pormin_dec ({self.pormin_dec}) must be less than pormax_dec ({self.pormax_dec})."
            )
        return self

    def to_df_state(self, grid_id: int) -> pd.DataFrame:
        """Convert deciduous tree properties to DataFrame state format."""
        # Get base properties from parent
        df_state = super().to_df_state(grid_id)

        list_properties = [
            "faidectree",
            "dectreeh",
            "pormin_dec",
            "pormax_dec",
            "capmax_dec",
            "capmin_dec",
        ]
        # Add all non-inherited properties
        for attr in list_properties:
            df_state.loc[grid_id, (attr, "0")] = getattr(self, attr).value

        # specific properties
        df_state.loc[grid_id, ("alb", "(3,)")] = self.alb.value
        df_state.loc[grid_id, ("albmin_dectr", "0")] = self.alb_min.value
        df_state.loc[grid_id, ("albmax_dectr", "0")] = self.alb_max.value

        return df_state

    @classmethod
    def from_df_state(cls, df: pd.DataFrame, grid_id: int) -> "DectrProperties":
        """Reconstruct deciduous tree properties from DataFrame state format."""
        surf_idx = 3
        instance = super().from_df_state(df, grid_id, surf_idx)

        instance.alb = ValueWithDOI(df.loc[grid_id, ("alb", "(3,)")])
        instance.faidectree = ValueWithDOI(df.loc[grid_id, ("faidectree", "0")])
        instance.dectreeh = ValueWithDOI(df.loc[grid_id, ("dectreeh", "0")])
        instance.pormin_dec = ValueWithDOI(df.loc[grid_id, ("pormin_dec", "0")])
        instance.pormax_dec = ValueWithDOI(df.loc[grid_id, ("pormax_dec", "0")])
        instance.capmax_dec = ValueWithDOI(df.loc[grid_id, ("capmax_dec", "0")])
        instance.capmin_dec = ValueWithDOI(df.loc[grid_id, ("capmin_dec", "0")])

        instance.alb_min = ValueWithDOI(df.loc[grid_id, ("albmin_dectr", "0")])
        instance.alb_max = ValueWithDOI(df.loc[grid_id, ("albmax_dectr", "0")])

        return instance


class GrassProperties(VegetatedSurfaceProperties):
    alb: ValueWithDOI[float] = Field(
        ge=0, le=1, default=ValueWithDOI(0.2), description="Minimum albedo"
    )
    _surface_type: Literal[SurfaceType.GRASS] = SurfaceType.GRASS
    waterdist: WaterDistribution = Field(
        default_factory=lambda: WaterDistribution(SurfaceType.GRASS),
        description="Water distribution for grass",
    )

    def to_df_state(self, grid_id: int) -> pd.DataFrame:
        """Convert grass properties to DataFrame state format."""
        # Get base properties from parent
        df_state = super().to_df_state(grid_id)

        # add specific properties
        df_state.loc[grid_id, ("alb", "(4,)")] = self.alb.value
        df_state[("albmin_grass", "0")] = self.alb_min.value
        df_state[("albmax_grass", "0")] = self.alb_max.value

        return df_state

    @classmethod
    def from_df_state(cls, df: pd.DataFrame, grid_id: int) -> "GrassProperties":
        """Reconstruct grass properties from DataFrame state format."""
        surf_idx = 4
        instance = super().from_df_state(df, grid_id, surf_idx)

        instance.alb = ValueWithDOI(df.loc[grid_id, ("alb", "(4,)")])
        instance.alb_min = ValueWithDOI(df.loc[grid_id, ("albmin_grass", "0")])
        instance.alb_max = ValueWithDOI(df.loc[grid_id, ("albmax_grass", "0")])

        return instance


class SnowParams(BaseModel):
    crwmax: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.1), description="Maximum water capacity of snow"
    )
    crwmin: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.05), description="Minimum water capacity of snow"
    )
    narp_emis_snow: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.99), description="Snow surface emissivity"
    )
    preciplimit: ValueWithDOI[float] = Field(
        default=ValueWithDOI(2.2), description="Limit for snow vs rain precipitation"
    )
    preciplimitalb: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.1), description="Precipitation limit for albedo aging"
    )
    snowalbmax: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.85), description="Maximum snow albedo"
    )
    snowalbmin: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.4), description="Minimum snow albedo"
    )
    snowdensmin: ValueWithDOI[float] = Field(
        default=ValueWithDOI(100.0), description="Minimum snow density (kg m-3)"
    )
    snowdensmax: ValueWithDOI[float] = Field(
        default=ValueWithDOI(400.0), description="Maximum snow density (kg m-3)"
    )
    snowlimbldg: ValueWithDOI = Field(
        default=ValueWithDOI(0.1), description="Snow limit on buildings"
    )
    snowlimpaved: ValueWithDOI = Field(
        default=ValueWithDOI(0.1), description="Snow limit on paved surfaces"
    )
    snowprof_24hr: HourlyProfile = Field(
        default_factory=HourlyProfile, description="24-hour snow profile"
    )
    tau_a: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.018), description="Aging constant for cold snow"
    )
    tau_f: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.11), description="Aging constant for melting snow"
    )
    tau_r: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.05), description="Aging constant for refreezing snow"
    )
    tempmeltfact: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.12), description="Temperature melt factor"
    )
    radmeltfact: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.0016), description="Radiation melt factor"
    )

    ref: Optional[Reference] = None

    @model_validator(mode="after")
    def validate_all(self) -> "SnowParams":
        """
        Aggregate all validation checks for SnowParams,
        so multiple errors (if any) can be raised together
        """
        errors = []
        if self.crwmin >= self.crwmax:
            errors.append(
                f"crwmin ({self.crwmin}) must be less than crwmax ({self.crwmax})."
            )
        if self.snowalbmin >= self.snowalbmax:
            errors.append(
                f"snowalbmin ({self.snowalbmin}) must be less than snowalbmax ({self.snowalbmax})."
            )
        if errors:
            raise ValueError("\n".join(errors))

        return self

    def to_df_state(self, grid_id: int) -> pd.DataFrame:
        """
        Convert snow parameters to DataFrame state format.

        Args:
            grid_id (int): Grid ID for the DataFrame index.

        Returns:
            pd.DataFrame: DataFrame containing snow parameters.
        """

        df_state = init_df_state(grid_id)

        scalar_params = {
            "crwmax": self.crwmax,
            "crwmin": self.crwmin,
            "narp_emis_snow": self.narp_emis_snow,
            "preciplimit": self.preciplimit,
            "preciplimitalb": self.preciplimitalb,
            "snowalbmax": self.snowalbmax,
            "snowalbmin": self.snowalbmin,
            "snowdensmin": self.snowdensmin,
            "snowdensmax": self.snowdensmax,
            "snowlimbldg": self.snowlimbldg,
            "snowlimpaved": self.snowlimpaved,
            "tau_a": self.tau_a,
            "tau_f": self.tau_f,
            "tau_r": self.tau_r,
            "tempmeltfact": self.tempmeltfact,
            "radmeltfact": self.radmeltfact,
        }
        for param_name, value in scalar_params.items():
            df_state.loc[grid_id, (param_name, "0")] = value.value

        df_hourly_profile = self.snowprof_24hr.to_df_state(grid_id, "snowprof_24hr")
        df_state = df_state.combine_first(df_hourly_profile)

        return df_state

    @classmethod
    def from_df_state(cls, df: pd.DataFrame, grid_id: int) -> "SnowParams":
        """
        Reconstruct SnowParams from a DataFrame state format.

        Args:
            df: DataFrame containing snow parameters.
            grid_id: Grid ID for the DataFrame index.

        Returns:
            SnowParams: Instance of SnowParams.
        """
        # Extract scalar attributes
        scalar_params = {
            "crwmax": df.loc[grid_id, ("crwmax", "0")],
            "crwmin": df.loc[grid_id, ("crwmin", "0")],
            "narp_emis_snow": df.loc[grid_id, ("narp_emis_snow", "0")],
            "preciplimit": df.loc[grid_id, ("preciplimit", "0")],
            "preciplimitalb": df.loc[grid_id, ("preciplimitalb", "0")],
            "snowalbmax": df.loc[grid_id, ("snowalbmax", "0")],
            "snowalbmin": df.loc[grid_id, ("snowalbmin", "0")],
            "snowdensmin": df.loc[grid_id, ("snowdensmin", "0")],
            "snowdensmax": df.loc[grid_id, ("snowdensmax", "0")],
            "snowlimbldg": df.loc[grid_id, ("snowlimbldg", "0")],
            "snowlimpaved": df.loc[grid_id, ("snowlimpaved", "0")],
            "tau_a": df.loc[grid_id, ("tau_a", "0")],
            "tau_f": df.loc[grid_id, ("tau_f", "0")],
            "tau_r": df.loc[grid_id, ("tau_r", "0")],
            "tempmeltfact": df.loc[grid_id, ("tempmeltfact", "0")],
            "radmeltfact": df.loc[grid_id, ("radmeltfact", "0")],
        }

        # Convert scalar parameters to ValueWithDOI
        scalar_params = {
            key: ValueWithDOI(value) for key, value in scalar_params.items()
        }

        # Extract HourlyProfile
        snowprof_24hr = HourlyProfile.from_df_state(df, grid_id, "snowprof_24hr")

        # Construct and return the SnowParams instance
        return cls(snowprof_24hr=snowprof_24hr, **scalar_params)


class LandCover(BaseModel):
    paved: PavedProperties = Field(
        default_factory=PavedProperties,
        description="Properties for paved surfaces like roads and pavements",
    )
    bldgs: BldgsProperties = Field(
        default_factory=BldgsProperties,
        description="Properties for building surfaces including roofs and walls",
    )
    evetr: EvetrProperties = Field(
        default_factory=EvetrProperties,
        description="Properties for evergreen trees and vegetation",
    )
    dectr: DectrProperties = Field(
        default_factory=DectrProperties,
        description="Properties for deciduous trees and vegetation",
    )
    grass: GrassProperties = Field(
        default_factory=GrassProperties, description="Properties for grass surfaces"
    )
    bsoil: BsoilProperties = Field(
        default_factory=BsoilProperties, description="Properties for bare soil surfaces"
    )
    water: WaterProperties = Field(
        default_factory=WaterProperties,
        description="Properties for water surfaces like lakes and ponds",
    )

    ref: Optional[Reference] = None

    @model_validator(mode="after")
    def set_surface_types(self) -> "LandCover":
        # Set surface types and validate
        surface_map = {
            "paved": (self.paved, SurfaceType.PAVED),
            "bldgs": (self.bldgs, SurfaceType.BLDGS),
            "dectr": (self.dectr, SurfaceType.DECTR),
            "evetr": (self.evetr, SurfaceType.EVETR),
            "grass": (self.grass, SurfaceType.GRASS),
            "bsoil": (self.bsoil, SurfaceType.BSOIL),
            "water": (self.water, SurfaceType.WATER),
        }

        for prop, surface_type in surface_map.values():
            prop.set_surface_type(surface_type)

        return self

    def to_df_state(self, grid_id: int) -> pd.DataFrame:
        """Convert land cover to DataFrame state format"""
        # df_state = init_df_state(grid_id)

        list_df_state = []
        for lc in ["paved", "bldgs", "dectr", "evetr", "grass", "bsoil", "water"]:
            df_state = getattr(self, lc).to_df_state(grid_id)
            list_df_state.append(df_state)
        df_state = pd.concat(list_df_state, axis=1)
        return df_state

    @classmethod
    def from_df_state(cls, df: pd.DataFrame, grid_id: int) -> "LandCover":
        """Reconstruct LandCover instance from DataFrame state.

        Args:
            df: DataFrame containing land cover parameters
            grid_id: Grid ID for the DataFrame index

        Returns:
            LandCover: Reconstructed LandCover instance
        """
        # Reconstruct each surface type from the DataFrame
        params = {
            "paved": PavedProperties.from_df_state(df, grid_id),
            "bldgs": BldgsProperties.from_df_state(df, grid_id),
            "evetr": EvetrProperties.from_df_state(df, grid_id),
            "dectr": DectrProperties.from_df_state(df, grid_id),
            "grass": GrassProperties.from_df_state(df, grid_id),
            "bsoil": BsoilProperties.from_df_state(df, grid_id),
            "water": WaterProperties.from_df_state(df, grid_id),
        }

        # Return reconstructed instance
        return cls(**params)


class ArchetypeProperties(BaseModel):
    # Not used in STEBBS - DAVE only
    # BuildingCode='1'
    # BuildingClass='SampleClass'

    BuildingType: str = "SampleType"
    BuildingName: str = "SampleBuilding"
    BuildingCount: ValueWithDOI[int] = Field(
        default=ValueWithDOI(1), description="Number of buildings of this archetype [-]"
    )
    Occupants: ValueWithDOI[int] = Field(
        default=ValueWithDOI(1),
        description="Number of occupants present in building [-]",
    )

    # Not used in STEBBS - DAVE only
    # hhs0: int = Field(default=0, description="")
    # hhs1: int = Field(default=0, description="")
    # hhs2: int = Field(default=0, description="")
    # hhs3: int = Field(default=0, description="")
    # hhs4: int = Field(default=0, description="")
    # hhs5: int = Field(default=0, description="")
    # hhs6: int = Field(default=0, description="")
    # hhs7: int = Field(default=0, description="")
    # hhs8: int = Field(default=0, description="")
    # age_0_4: int = Field(default=0, description="")
    # age_5_11: int = Field(default=0, description="")
    # age_12_18: int = Field(default=0, description="")
    # age_19_64: int = Field(default=0, description="")
    # age_65plus: int = Field(default=0, description="")

    stebbs_Height: ValueWithDOI[float] = Field(
        default=ValueWithDOI(10.0),
        description="Building height [m]",
        gt=0.0,
    )
    FootprintArea: ValueWithDOI[float] = Field(
        default=ValueWithDOI(64.0),
        description="Building footprint area [m2]",
        gt=0.0,
    )
    WallExternalArea: ValueWithDOI[float] = Field(
        default=ValueWithDOI(80.0),
        description="External wall area (including window area) [m2]",
        gt=0.0,
    )
    RatioInternalVolume: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.01),
        description="Ratio of internal mass volume to total building volume [-]",
        ge=0.0,
        le=1.0,
    )
    WWR: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.20),
        description="window to wall ratio [-]",
        ge=0.0,
        le=1.0,
    )
    WallThickness: ValueWithDOI[float] = Field(
        default=ValueWithDOI(20.0),
        description="Thickness of external wall [m]",
        gt=0.0,
    )
    WallEffectiveConductivity: ValueWithDOI[float] = Field(
        default=ValueWithDOI(60.0),
        description="Effective thermal conductivity of walls [W m-1 K-1]",
        gt=0.0,
    )
    WallDensity: ValueWithDOI[float] = Field(
        default=ValueWithDOI(1600.0),
        description="Effective density of the walls [kg m-3]",
        gt=0.0,
    )
    WallCp: ValueWithDOI[float] = Field(
        default=ValueWithDOI(850.0),
        description="Effective specific heat capacity of walls [J kg-1 K-1]",
        gt=0.0,
    )
    WallextThickness: ValueWithDOI[float] = Field(
        default=ValueWithDOI(20.0),
        description="Thickness of layers external to insulation in external wall [m]",
        gt=0.0,
    )
    WallextEffectiveConductivity: ValueWithDOI[float] = Field(
        default=ValueWithDOI(60.0),
        description="Effective thermal conductivity of layers external to insulation in walls [W m-1 K-1]",
        gt=0.0,
    )
    WallextDensity: ValueWithDOI[float] = Field(
        default=ValueWithDOI(1600.0),
        description="Effective density of layers external to insulation in the walls [kg m-3]",
        gt=0.0,
    )
    WallextCp: ValueWithDOI[float] = Field(
        default=ValueWithDOI(850.0),
        description="Effective specific heat capacity of layers external to insulation in walls [J kg-1 K-1]",
        gt=0.0,
    )    
    Wallx1: ValueWithDOI[float] = Field(
        default=ValueWithDOI(1.0),
        description="Weighting factor for heat capacity of walls [-]",
        ge=0.0,
        le=1.0,
    )
    WallExternalEmissivity: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.9),
        description="Emissivity of the external surface of walls [-]",
        ge=0.0,
        le=1.0,
    )
    WallInternalEmissivity: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.9),
        description="Emissivity of the internal surface of walls [-]",
        ge=0.0,
        le=1.0,
    )
    WallTransmissivity: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.0),
        description="Transmissivity of walls [-]",
        ge=0.0,
        le=1.0,
    )
    WallAbsorbtivity: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.8),
        description="Absorbtivity of walls [-]",
        ge=0.0,
        le=1.0,
    )
    WallReflectivity: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.2),
        description="Reflectivity of the external surface of walls [-]",
        ge=0.0,
        le=1.0,
    )
    RoofThickness: ValueWithDOI[float] = Field(
        default=ValueWithDOI(20.0),
        description="Thickness of external roof [m]",
        gt=0.0,
    )
    RoofEffectiveConductivity: ValueWithDOI[float] = Field(
        default=ValueWithDOI(60.0),
        description="Effective thermal conductivity of roof [W m-1 K-1]",
        gt=0.0,
    )
    RoofDensity: ValueWithDOI[float] = Field(
        default=ValueWithDOI(1600.0),
        description="Effective density of the roof [kg m-3]",
        gt=0.0,
    )
    RoofCp: ValueWithDOI[float] = Field(
        default=ValueWithDOI(850.0),
        description="Effective specific heat capacity of roof [J kg-1 K-1]",
        gt=0.0,
    )
    RoofextThickness: ValueWithDOI[float] = Field(
        default=ValueWithDOI(20.0),
        description="Thickness of layers external to insulation in external roof [m]",
        gt=0.0,
    )
    RoofextEffectiveConductivity: ValueWithDOI[float] = Field(
        default=ValueWithDOI(60.0),
        description="Effective thermal conductivity of layers external to insulation in roof [W m-1 K-1]",
        gt=0.0,
    )
    RoofextDensity: ValueWithDOI[float] = Field(
        default=ValueWithDOI(1600.0),
        description="Effective density of layers external to insulation in the roof [kg m-3]",
        gt=0.0,
    )
    RoofextCp: ValueWithDOI[float] = Field(
        default=ValueWithDOI(850.0),
        description="Effective specific heat capacity of layers external to insulation in roof [J kg-1 K-1]",
        gt=0.0,
    )
    Roofx1: ValueWithDOI[float] = Field(
        default=ValueWithDOI(1.0),
        description="Weighting factor for heat capacity of roof [-]",
        ge=0.0,
        le=1.0,
    )
    RoofExternalEmissivity: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.9),
        description="Emissivity of the external surface of roof [-]",
        ge=0.0,
        le=1.0,
    )
    RoofInternalEmissivity: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.9),
        description="Emissivity of the internal surface of roof [-]",
        ge=0.0,
        le=1.0,
    )
    RoofTransmissivity: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.0),
        description="Transmissivity of roof [-]",
        ge=0.0,
        le=1.0,
    )
    RoofAbsorbtivity: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.8),
        description="Absorbtivity of roof [-]",
        ge=0.0,
        le=1.0,
    )
    RoofReflectivity: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.2),
        description="Reflectivity of the external surface of roof [-]",
        ge=0.0,
        le=1.0,
    )
    FloorThickness: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.2),
        description="Thickness of ground floor [m]",
        gt=0.0,
    )
    GroundFloorEffectiveConductivity: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.15),
        description="Effective thermal conductivity of ground floor [W m-1 K-1]",
        gt=0.0,
    )
    GroundFloorDensity: ValueWithDOI[float] = Field(
        default=ValueWithDOI(500.0),
        description="Density of the ground floor [kg m-3]",
        gt=0.0,
    )
    GroundFloorCp: ValueWithDOI[float] = Field(
        default=ValueWithDOI(1500.0),
        description="Effective specific heat capacity of the ground floor [J kg-1 K-1]",
        gt=0.0,
    )
    WindowThickness: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.015),
        description="Window thickness [m]",
        gt=0.0,
    )
    WindowEffectiveConductivity: ValueWithDOI[float] = Field(
        default=ValueWithDOI(1.0),
        description="Effective thermal conductivity of windows [W m-1 K-1]",
        gt=0.0,
    )
    WindowDensity: ValueWithDOI[float] = Field(
        default=ValueWithDOI(2500.0),
        description="Effective density of the windows [kg m-3]",
        gt=0.0,
    )
    WindowCp: ValueWithDOI[float] = Field(
        default=ValueWithDOI(840.0),
        description="Effective specific heat capacity of windows [J kg-1 K-1]",
        gt=0.0,
    )
    WindowExternalEmissivity: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.90),
        description="Emissivity of the external surface of windows [-]",
        ge=0.0,
        le=1.0,
    )
    WindowInternalEmissivity: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.90),
        description="Emissivity of the internal surface of windows [-]",
        ge=0.0,
        le=1.0,
    )
    WindowTransmissivity: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.90),
        description="Transmissivity of windows [-]",
        ge=0.0,
        le=1.0,
    )
    WindowAbsorbtivity: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.01),
        description="Absorbtivity of windows [-]",
        ge=0.0,
        le=1.0,
    )
    WindowReflectivity: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.09),
        description="Reflectivity of the external surface of windows [-]",
        ge=0.0,
        le=1.0,
    )
    # TODO: Add defaults below here
    InternalMassDensity: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.0),
        description="Effective density of the internal mass [kg m-3]",
    )
    InternalMassCp: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.0),
        description="Specific heat capacity of internal mass [J kg-1 K-1]",
    )
    InternalMassEmissivity: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.0), description="Emissivity of internal mass [-]"
    )
    MaxHeatingPower: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.0),
        description="Maximum power demand of heating system [W]",
    )
    WaterTankWaterVolume: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.0), description="Volume of water in hot water tank [m3]"
    )
    MaximumHotWaterHeatingPower: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.0),
        description="Maximum power demand of water heating system [W]",
    )
    HeatingSetpointTemperature: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.0), description="Heating setpoint temperature [degC]"
    )
    CoolingSetpointTemperature: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.0), description="Cooling setpoint temperature [degC]"
    )

    ref: Optional[Reference] = None

    def to_df_state(self, grid_id: int) -> pd.DataFrame:
        """Convert ArchetypeProperties to DataFrame state format."""

        df_state = init_df_state(grid_id)

        # Create an empty DataFrame with MultiIndex columns
        columns = [
            (field.lower(), "0") for field in self.model_fields.keys() if field != "ref"
        ]
        df_state = pd.DataFrame(
            index=[grid_id], columns=pd.MultiIndex.from_tuples(columns)
        )

        # Set the values in the DataFrame
        for field_name, field_info in self.model_fields.items():
            if field_name == "ref":
                continue
            attribute = getattr(self, field_name)
            if isinstance(attribute, ValueWithDOI):
                value = attribute.value
            else:
                value = attribute
            df_state.loc[grid_id, (field_name.lower(), "0")] = value

        return df_state

    @classmethod
    def from_df_state(cls, df: pd.DataFrame, grid_id: int) -> "ArchetypeProperties":
        """Reconstruct ArchetypeProperties from DataFrame state format."""
        # Extract the values from the DataFrame
        params = {
            field_name: df.loc[grid_id, (field_name.lower(), "0")]
            for field_name in cls.model_fields.keys()
            if field_name != "ref"
        }

        # Convert params to ValueWithDOI
        non_value_with_doi = ["BuildingType", "BuildingName"]
        params = {
            key: (ValueWithDOI(value) if key not in non_value_with_doi else value)
            for key, value in params.items()
        }

        # Create an instance using the extracted parameters
        return cls(**params)


class StebbsProperties(BaseModel):
    WallInternalConvectionCoefficient: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.0),
        description="Internal convection coefficient of walls [W m-2 K-1]",
    )
    RoofInternalConvectionCoefficient: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.0),
        description="Internal convection coefficient of roof [W m-2 K-1]",
    )    
    InternalMassConvectionCoefficient: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.0),
        description="Convection coefficient of internal mass [W m-2 K-1]",
    )
    FloorInternalConvectionCoefficient: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.0),
        description="Internal convection coefficient of ground floor [W m-2 K-1]",
    )
    WindowInternalConvectionCoefficient: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.0),
        description="Internal convection coefficient of windows [W m-2 K-1]",
    )
    WallExternalConvectionCoefficient: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.0),
        description="Initial external convection coefficient of walls [W m-2 K-1]",
    )
    RoofExternalConvectionCoefficient: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.0),
        description="Initial external convection coefficient of roof [W m-2 K-1]",
    )   
    WindowExternalConvectionCoefficient: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.0),
        description="Initial external convection coefficient of windows [W m-2 K-1]",
    )
    GroundDepth: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.0),
        description="Depth of external ground (deep soil) [m]",
    )
    ExternalGroundConductivity: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.0), description=""
    )
    IndoorAirDensity: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.0), description="Density of indoor air [kg m-3]"
    )
    IndoorAirCp: ValueWithDOI[float] = Field( 
        default=ValueWithDOI(0.0),
        description="Specific heat capacity of indoor air [J kg-1 K-1]",
    )
    WallBuildingViewFactor: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.0),
        description="Building view factor of external walls [-]",
    )
    WallGroundViewFactor: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.0),
        description="Ground view factor of external walls [-]",
    )
    WallSkyViewFactor: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.0), description="Sky view factor of external walls [-]"
    )
    RoofBuildingViewFactor: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.0),
        description="Building view factor of external roof [-]",
    )
    RoofGroundViewFactor: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.0),
        description="Ground view factor of external roof [-]",
    )
    RoofSkyViewFactor: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.0), description="Sky view factor of external roof [-]"
    )    
    MetabolicRate: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.0),
        description="Metabolic rate of building occupants [W]",
    )
    LatentSensibleRatio: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.0),
        description="Latent-to-sensible ratio of metabolic energy release of occupants [-]",
    )
    ApplianceRating: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.0), description="Power demand of single appliance [W]"
    )
    TotalNumberofAppliances: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.0),
        description="Number of appliances present in building [-]",
    )
    ApplianceUsageFactor: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.0), description="Number of appliances in use [-]"
    )
    HeatingSystemEfficiency: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.0), description="Efficiency of space heating system [-]"
    )
    MaxCoolingPower: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.0),
        description="Maximum power demand of cooling system [W]",
    )
    CoolingSystemCOP: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.0),
        description="Coefficient of performance of cooling system [-]",
    )
    VentilationRate: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.0),
        description="Ventilation rate (air changes per hour, ACH) [h-1]",
    )
    IndoorAirStartTemperature: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.0), description="Initial indoor air temperature [degC]"
    )
    IndoorMassStartTemperature: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.0), description="Initial indoor mass temperature [degC]"
    )
    WallIndoorSurfaceTemperature: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.0),
        description="Initial wall indoor surface temperature [degC]",
    )
    WallOutdoorSurfaceTemperature: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.0),
        description="Initial wall outdoor surface temperature [degC]",
    )
    RoofIndoorSurfaceTemperature: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.0),
        description="Initial roof indoor surface temperature [degC]",
    )
    RoofOutdoorSurfaceTemperature: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.0),
        description="Initial roof outdoor surface temperature [degC]",
    )   
    WindowIndoorSurfaceTemperature: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.0),
        description="Initial window indoor surface temperature [degC]",
    )
    WindowOutdoorSurfaceTemperature: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.0),
        description="Initial window outdoor surface temperature [degC]",
    )
    GroundFloorIndoorSurfaceTemperature: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.0),
        description="Initial ground floor indoor surface temperature [degC]",
    )
    GroundFloorOutdoorSurfaceTemperature: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.0),
        description="Initial ground floor outdoor surface temperature [degC]",
    )
    WaterTankTemperature: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.0),
        description="Initial water temperature in hot water tank [degC]",
    )
    InternalWallWaterTankTemperature: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.0),
        description="Initial hot water tank internal wall temperature [degC]",
    )
    ExternalWallWaterTankTemperature: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.0),
        description="Initial hot water tank external wall temperature [degC]",
    )
    WaterTankWallThickness: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.0), description="Hot water tank wall thickness [m]"
    )
    MainsWaterTemperature: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.0),
        description="Temperature of water coming into the water tank [degC]",
    )
    WaterTankSurfaceArea: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.0),
        description="Surface area of hot water tank cylinder [m2]",
    )
    HotWaterHeatingSetpointTemperature: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.0), description="Water tank setpoint temperature [degC]"
    )
    HotWaterTankWallEmissivity: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.0),
        description="Effective external wall emissivity of the hot water tank [-]",
    )
    DomesticHotWaterTemperatureInUseInBuilding: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.0),
        description="Initial water temperature of water held in use in building [degC]",
    )
    InternalWallDHWVesselTemperature: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.0),
        description="Initial hot water vessel internal wall temperature [degC]",
    )
    ExternalWallDHWVesselTemperature: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.0),
        description="Initial hot water vessel external wall temperature [degC]",
    )
    DHWVesselWallThickness: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.0), description="Hot water vessel wall thickness [m]"
    )
    DHWWaterVolume: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.0),
        description="Volume of water held in use in building [m3]",
    )
    DHWSurfaceArea: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.0),
        description="Surface area of hot water in vessels in building [m2]",
    )
    DHWVesselEmissivity: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.0),
        description="NEEDS CHECKED! NOT USED (assumed same as DHWVesselWallEmissivity) [-]",
    )
    HotWaterFlowRate: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.0),
        description="Hot water flow rate from tank to vessel [m3 s-1]",
    )
    DHWDrainFlowRate: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.0),
        description="Flow rate of hot water held in building to drain [m3 s-1]",
    )
    DHWSpecificHeatCapacity: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.0),
        description="Specific heat capacity of hot water [J kg-1 K-1]",
    )
    HotWaterTankSpecificHeatCapacity: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.0),
        description="Specific heat capacity of hot water tank wal [J kg-1 K-1]",
    )
    DHWVesselSpecificHeatCapacity: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.0),
        description="Specific heat capacity of vessels containing hot water in use in buildings [J kg-1 K-1]",
    )
    DHWDensity: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.0), description="Density of hot water in use [kg m-3]"
    )
    HotWaterTankWallDensity: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.0), description="Density of hot water tank wall [kg m-3]"
    )
    DHWVesselDensity: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.0),
        description="Density of vessels containing hot water in use [kg m-3]",
    )
    HotWaterTankBuildingWallViewFactor: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.0),
        description="Water tank/vessel internal building wall/roof view factor [-]",
    )
    HotWaterTankInternalMassViewFactor: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.0),
        description="Water tank/vessel building internal mass view factor [-]",
    )
    HotWaterTankWallConductivity: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.0),
        description="Effective wall conductivity of the hot water tank [W m-1 K-1]",
    )
    HotWaterTankInternalWallConvectionCoefficient: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.0),
        description="Effective internal wall convection coefficient of the hot water tank [W m-2 K-1]",
    )
    HotWaterTankExternalWallConvectionCoefficient: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.0),
        description="Effective external wall convection coefficient of the hot water tank [W m-2 K-1]",
    )
    DHWVesselWallConductivity: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.0),
        description="Effective wall conductivity of the hot water tank [W m-1 K-1]",
    )
    DHWVesselInternalWallConvectionCoefficient: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.0),
        description="Effective internal wall convection coefficient of the vessels holding hot water in use in building [W m-2 K-1]",
    )
    DHWVesselExternalWallConvectionCoefficient: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.0),
        description="Effective external wall convection coefficient of the vessels holding hot water in use in building [W m-2 K-1]",
    )
    DHWVesselWallEmissivity: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.0),
        description="Effective external wall emissivity of hot water being used within building [-]",
    )
    HotWaterHeatingEfficiency: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.0), description="Efficiency of hot water system [-]"
    )
    MinimumVolumeOfDHWinUse: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.0), description="Minimum volume of hot water in use [m3]"
    )

    ref: Optional[Reference] = None

    def to_df_state(self, grid_id: int) -> pd.DataFrame:
        """Convert StebbsProperties to DataFrame state format."""
        df_state = init_df_state(grid_id)

        # Create an empty DataFrame with MultiIndex columns
        columns = [
            (field.lower(), "0") for field in self.model_fields.keys() if field != "ref"
        ]
        df_state = pd.DataFrame(
            index=[grid_id], columns=pd.MultiIndex.from_tuples(columns)
        )

        # Set the values in the DataFrame
        for field_name, field_info in self.model_fields.items():
            if field_name == "ref":
                continue
            df_state.loc[grid_id, (field_name.lower(), "0")] = getattr(
                self, field_name
            ).value

        return df_state

    @classmethod
    def from_df_state(cls, df: pd.DataFrame, grid_id: int) -> "StebbsProperties":
        """Reconstruct StebbsProperties from DataFrame state format."""
        # Extract the values from the DataFrame
        params = {
            field_name: df.loc[grid_id, (field_name.lower(), "0")]
            for field_name in cls.model_fields.keys()
            if field_name != "ref"
        }

        # Convert params to ValueWithDOI
        params = {key: ValueWithDOI(value) for key, value in params.items()}

        # Create an instance using the extracted parameters
        return cls(**params)


class SPARTACUSParams(BaseModel):
    air_ext_lw: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.0),
        description="Air extinction coefficient for longwave radiation",
    )
    air_ext_sw: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.0),
        description="Air extinction coefficient for shortwave radiation",
    )
    air_ssa_lw: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.5),
        description="Air single scattering albedo for longwave radiation",
    )
    air_ssa_sw: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.5),
        description="Air single scattering albedo for shortwave radiation",
    )
    ground_albedo_dir_mult_fact: ValueWithDOI[float] = Field(
        default=ValueWithDOI(1.0),
        description="Multiplication factor for direct ground albedo",
    )
    n_stream_lw_urban: ValueWithDOI[int] = Field(
        default=ValueWithDOI(2),
        description="Number of streams for longwave radiation in urban areas",
    )
    n_stream_sw_urban: ValueWithDOI[int] = Field(
        default=ValueWithDOI(2),
        description="Number of streams for shortwave radiation in urban areas",
    )
    n_vegetation_region_urban: ValueWithDOI[int] = Field(
        default=ValueWithDOI(1),
        description="Number of vegetation regions in urban areas",
    )
    sw_dn_direct_frac: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.5),
        description="Fraction of downward shortwave radiation that is direct",
    )
    use_sw_direct_albedo: ValueWithDOI[float] = Field(
        default=ValueWithDOI(1.0),
        description="Flag to use direct albedo for shortwave radiation",
    )
    veg_contact_fraction_const: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.5), description="Constant vegetation contact fraction"
    )
    veg_fsd_const: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.5),
        description="Constant vegetation fractional standard deviation",
    )
    veg_ssa_lw: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.5),
        description="Vegetation single scattering albedo for longwave radiation",
    )
    veg_ssa_sw: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.5),
        description="Vegetation single scattering albedo for shortwave radiation",
    )

    ref: Optional[Reference] = None

    def to_df_state(self, grid_id: int) -> pd.DataFrame:
        """
        Convert SPARTACUS parameters to DataFrame state format.

        Args:
            grid_id: Grid ID for the DataFrame index

        Returns:
            pd.DataFrame: DataFrame containing SPARTACUS parameters
        """
        # Initialize DataFrame with grid index
        df_state = init_df_state(grid_id)

        # Map SPARTACUS parameters to DataFrame columns
        spartacus_params = {
            "air_ext_lw": self.air_ext_lw,
            "air_ext_sw": self.air_ext_sw,
            "air_ssa_lw": self.air_ssa_lw,
            "air_ssa_sw": self.air_ssa_sw,
            "ground_albedo_dir_mult_fact": self.ground_albedo_dir_mult_fact,
            "n_stream_lw_urban": self.n_stream_lw_urban,
            "n_stream_sw_urban": self.n_stream_sw_urban,
            "n_vegetation_region_urban": self.n_vegetation_region_urban,
            "sw_dn_direct_frac": self.sw_dn_direct_frac,
            "use_sw_direct_albedo": self.use_sw_direct_albedo,
            "veg_contact_fraction_const": self.veg_contact_fraction_const,
            "veg_fsd_const": self.veg_fsd_const,
            "veg_ssa_lw": self.veg_ssa_lw,
            "veg_ssa_sw": self.veg_ssa_sw,
        }

        # Assign each parameter to its corresponding column in the DataFrame
        for param_name, value in spartacus_params.items():
            df_state[(param_name, "0")] = value.value

        return df_state

    @classmethod
    def from_df_state(cls, df: pd.DataFrame, grid_id: int) -> "SPARTACUSParams":
        """
        Reconstruct SPARTACUSParams from DataFrame state format.

        Args:
            df: DataFrame containing SPARTACUS parameters
            grid_id: Grid ID for the DataFrame index

        Returns:
            SPARTACUSParams: An instance of SPARTACUSParams
        """

        spartacus_params = {
            "air_ext_lw",
            "air_ext_sw",
            "air_ssa_lw",
            "air_ssa_sw",
            "ground_albedo_dir_mult_fact",
            "n_stream_lw_urban",
            "n_stream_sw_urban",
            "n_vegetation_region_urban",
            "sw_dn_direct_frac",
            "use_sw_direct_albedo",
            "veg_contact_fraction_const",
            "veg_fsd_const",
            "veg_ssa_lw",
            "veg_ssa_sw",
        }

        params = {
            param: ValueWithDOI(df.loc[grid_id, (param, "0")])
            for param in spartacus_params
        }

        return cls(**params)


class LUMPSParams(BaseModel):
    raincover: ValueWithDOI[float] = Field(ge=0, le=1, default=ValueWithDOI(0.25))
    rainmaxres: ValueWithDOI[float] = Field(ge=0, le=20, default=ValueWithDOI(0.25))
    drainrt: ValueWithDOI[float] = Field(ge=0, le=1, default=ValueWithDOI(0.25))
    veg_type: ValueWithDOI[int] = Field(default=ValueWithDOI(1))

    ref: Optional[Reference] = None

    def to_df_state(self, grid_id: int) -> pd.DataFrame:
        """Convert LUMPS parameters to DataFrame state format.

        Args:
            grid_id: Grid ID for the DataFrame index

        Returns:
            pd.DataFrame: DataFrame containing LUMPS parameters
        """
        df_state = init_df_state(grid_id)

        # Add all attributes
        for attr in ["raincover", "rainmaxres", "drainrt", "veg_type"]:
            df_state[(attr, "0")] = getattr(self, attr).value

        return df_state

    @classmethod
    def from_df_state(cls, df: pd.DataFrame, grid_id: int) -> "LUMPSParams":
        """Create LUMPSParams from DataFrame state format.

        Args:
            df: DataFrame containing LUMPS parameters
            grid_id: Grid ID for the DataFrame index

        Returns:
            LUMPSParams: Instance of LUMPSParams
        """
        # Extract attributes from DataFrame
        params = {}
        for attr in ["raincover", "rainmaxres", "drainrt", "veg_type"]:
            params[attr] = df.loc[grid_id, (attr, "0")]

        # Convert attributes to ValueWithDOI
        params = {key: ValueWithDOI(value) for key, value in params.items()}

        return cls(**params)


class SiteProperties(BaseModel):
    lat: ValueWithDOI[float] = Field(
        ge=-90,
        le=90,
        description="Latitude of the site in degrees",
        default=ValueWithDOI(51.5),
    )
    lng: ValueWithDOI[float] = Field(
        ge=-180,
        le=180,
        description="Longitude of the site in degrees",
        default=ValueWithDOI(-0.13),
    )
    alt: ValueWithDOI[float] = Field(
        gt=0,
        description="Altitude of the site in metres above sea level",
        default=ValueWithDOI(40.0),
    )
    timezone: ValueWithDOI[int] = Field(
        ge=-12,
        le=12,
        description="Time zone offset from UTC in hours",
        default=ValueWithDOI(0),
    )
    surfacearea: ValueWithDOI[float] = Field(
        gt=0,
        description="Total surface area of the site in square metres",
        default=ValueWithDOI(10000.0),
    )
    z: ValueWithDOI[float] = Field(
        gt=0, description="Measurement height in metres", default=ValueWithDOI(10.0)
    )
    z0m_in: ValueWithDOI[float] = Field(
        gt=0,
        description="Momentum roughness length in metres",
        default=ValueWithDOI(1.0),
    )
    zdm_in: ValueWithDOI[float] = Field(
        gt=0,
        description="Zero-plane displacement height in metres",
        default=ValueWithDOI(5.0),
    )
    pipecapacity: ValueWithDOI[float] = Field(
        gt=0,
        description="Maximum capacity of drainage pipes in mm/hr",
        default=ValueWithDOI(100.0),
    )
    runofftowater: ValueWithDOI[float] = Field(
        ge=0,
        le=1,
        description="Fraction of excess water going to water bodies",
        default=ValueWithDOI(0.0),
    )
    narp_trans_site: ValueWithDOI[float] = Field(
        description="Site-specific NARP transmission coefficient",
        default=ValueWithDOI(0.2),
    )
    lumps: LUMPSParams = Field(
        default_factory=LUMPSParams,
        description="Parameters for Local-scale Urban Meteorological Parameterization Scheme",
    )
    spartacus: SPARTACUSParams = Field(
        default_factory=SPARTACUSParams,
        description="Parameters for Solar Parametrizations for Radiative Transfer through Urban Canopy Scheme",
    )
    stebbs: StebbsProperties = Field(
        default_factory=StebbsProperties,
        description="Parameters for the STEBBS building energy model",
    )
    building_archetype: ArchetypeProperties = Field(
        default_factory=ArchetypeProperties,
        description="Parameters for building archetypes",
    )
    conductance: Conductance = Field(
        default_factory=Conductance,
        description="Parameters for surface conductance calculations",
    )
    irrigation: IrrigationParams = Field(
        default_factory=IrrigationParams,
        description="Parameters for irrigation modelling",
    )
    anthropogenic_emissions: AnthropogenicEmissions = Field(
        default_factory=AnthropogenicEmissions,
        description="Parameters for anthropogenic heat and water emissions",
    )
    snow: SnowParams = Field(
        default_factory=SnowParams, description="Parameters for snow modelling"
    )
    land_cover: LandCover = Field(
        default_factory=LandCover,
        description="Parameters for land cover characteristics",
    )
    vertical_layers: VerticalLayers = Field(
        default_factory=VerticalLayers,
        description="Parameters for vertical layer structure",
    )

    n_buildings: ValueWithDOI[int] = Field(
        default=ValueWithDOI(1),
        description="Number of buildings in the site",
    )

    h_std: ValueWithDOI[float] = Field(
        default=ValueWithDOI(10.0),
        description="Standard deviation of building heights in the site",
    )

    lambda_c: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0),
        description="Building surface to plan area ratio [-]",
        ge=0
    )

    ref: Optional[Reference] = None

    def to_df_state(self, grid_id: int) -> pd.DataFrame:
        """Convert site properties to DataFrame state format"""
        df_state = init_df_state(grid_id)

        # simple attributes
        for var in [
            "lat",
            "lng",
            "alt",
            "timezone",
            "surfacearea",
            "z",
            "z0m_in",
            "zdm_in",
            "pipecapacity",
            "runofftowater",
            "narp_trans_site",
            "n_buildings",
            "h_std",
            "lambda_c"
        ]:
            df_state.loc[grid_id, (f"{var}", "0")] = getattr(self, var).value

        # complex attributes
        df_lumps = self.lumps.to_df_state(grid_id)
        df_spartacus = self.spartacus.to_df_state(grid_id)
        df_conductance = self.conductance.to_df_state(grid_id)
        df_irrigation = self.irrigation.to_df_state(grid_id)
        df_anthropogenic_emissions = self.anthropogenic_emissions.to_df_state(grid_id)
        df_snow = self.snow.to_df_state(grid_id)
        df_land_cover = self.land_cover.to_df_state(grid_id)
        df_vertical_layers = self.vertical_layers.to_df_state(grid_id)
        df_stebbs = self.stebbs.to_df_state(grid_id)
        df_building_archetype = self.building_archetype.to_df_state(grid_id)

        df_state = pd.concat(
            [
                df_state,
                df_lumps,
                df_spartacus,
                df_conductance,
                df_irrigation,
                df_anthropogenic_emissions,
                df_snow,
                df_land_cover,
                df_vertical_layers,
                df_stebbs,
                df_building_archetype,
            ],
            axis=1,
        )
        return df_state

    @classmethod
    def from_df_state(cls, df: pd.DataFrame, grid_id: int) -> "SiteProperties":
        """Reconstruct SiteProperties from DataFrame state format.

        Args:
            df: DataFrame containing site properties
            grid_id: Grid ID for the DataFrame index

        Returns:
            SiteProperties: Reconstructed instance
        """
        # Extract simple attributes
        params = {}
        for var in [
            "lat",
            "lng",
            "alt",
            "timezone",
            "surfacearea",
            "z",
            "z0m_in",
            "zdm_in",
            "pipecapacity",
            "runofftowater",
            "narp_trans_site",
            "n_buildings",
            "h_std",
            "lambda_c"
        ]:
            params[var] = ValueWithDOI(df.loc[grid_id, (var, "0")])

        # Extract complex attributes
        params["lumps"] = LUMPSParams.from_df_state(df, grid_id)
        params["spartacus"] = SPARTACUSParams.from_df_state(df, grid_id)
        params["conductance"] = Conductance.from_df_state(df, grid_id)
        params["irrigation"] = IrrigationParams.from_df_state(df, grid_id)
        params["anthropogenic_emissions"] = AnthropogenicEmissions.from_df_state(
            df, grid_id
        )
        params["snow"] = SnowParams.from_df_state(df, grid_id)
        params["land_cover"] = LandCover.from_df_state(df, grid_id)
        params["vertical_layers"] = VerticalLayers.from_df_state(df, grid_id)

        params["stebbs"] = StebbsProperties.from_df_state(df, grid_id)
        params["building_archetype"] = ArchetypeProperties.from_df_state(df, grid_id)

        return cls(**params)


class Site(BaseModel):
    name: str = Field(description="Name of the site", default="test site")
    gridiv: int = Field(
        description="Grid ID for identifying this site in multi-site simulations",
        default=1,
    )
    properties: SiteProperties = Field(
        default_factory=SiteProperties,
        description="Physical and morphological properties of the site",
    )
    initial_states: InitialStates = Field(
        default_factory=InitialStates,
        description="Initial conditions for model state variables",
    )

    def to_df_state(self, grid_id: int) -> pd.DataFrame:
        """Convert site to DataFrame state format"""
        df_state = init_df_state(grid_id)
        df_site_properties = self.properties.to_df_state(grid_id)
        df_initial_states = self.initial_states.to_df_state(grid_id)
        df_state = pd.concat([df_state, df_site_properties, df_initial_states], axis=1)
        return df_state


class SnowAlb(BaseModel):
    snowalb: ValueWithDOI[float] = Field(
        description="Snow albedo",
        default=ValueWithDOI(0.7),
        ge=0,
        le=1,
    )

    def to_df_state(self, grid_id: int) -> pd.DataFrame:
        """Convert snow albedo to DataFrame state format.

        Args:
            grid_id: Grid ID for the DataFrame index

        Returns:
            pd.DataFrame: DataFrame containing snow albedo parameters
        """
        df_state = init_df_state(grid_id)
        df_state[("snowalb", "0")] = self.snowalb.value
        return df_state

    @classmethod
    def from_df_state(cls, df: pd.DataFrame, grid_id: int) -> "SnowAlb":
        """
        Reconstruct SnowAlb from a DataFrame state format.

        Args:
            df (pd.DataFrame): DataFrame containing snow albedo parameters.
            grid_id (int): Grid ID for the DataFrame index.

        Returns:
            SnowAlb: Instance of SnowAlb.
        """
        snowalb = df.loc[grid_id, ("snowalb", "0")]
        return cls(snowalb=ValueWithDOI(snowalb))
