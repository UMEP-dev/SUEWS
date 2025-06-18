from typing import Optional
from pydantic import BaseModel, Field, model_validator
from .type import RefValue, Reference
from .profile import HourlyProfile
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
    porosity_id: RefValue[int] = Field(description="Initial porosity for deciduous trees", unit="dimensionless")
    gdd_id: RefValue[int] = Field(description="Growing degree days ID", unit="degC d")
    sdd_id: RefValue[int] = Field(description="Senescence degree days ID", unit="degC d")
    lai: Dict[str, Union[RefValue[float], List[RefValue[float]]]] = Field(
        description="Leaf area index parameters",
        unit="m^2 m^-2"
    )
    ie_a: RefValue[float] = Field(description="Irrigation efficiency coefficient a", unit="dimensionless")
    ie_m: RefValue[float] = Field(description="Irrigation efficiency coefficient m", unit="dimensionless")

    ref: Optional[Reference] = None


class Conductance(BaseModel):
    g_max: RefValue[float] = Field(
        default=RefValue(40.0),
        description="Maximum surface conductance for photosynthesis",
        unit="mm s^-1"
    )
    g_k: RefValue[float] = Field(
        default=RefValue(0.6),
        description="Conductance parameter related to incoming solar radiation",
        unit="dimensionless"
    )
    g_q_base: RefValue[float] = Field(
        default=RefValue(0.03),
        description="Base value for conductance parameter related to vapor pressure deficit",
        unit="kPa^-1"
    )
    g_q_shape: RefValue[float] = Field(
        default=RefValue(0.9),
        description="Shape parameter for conductance related to vapor pressure deficit",
        unit="dimensionless"
    )
    g_t: RefValue[float] = Field(
        default=RefValue(30.0),
        description="Conductance parameter related to air temperature",
        unit="degC"
    )
    g_sm: RefValue[float] = Field(
        default=RefValue(0.5),
        description="Conductance parameter related to soil moisture",
        unit="dimensionless"
    )
    kmax: RefValue[float] = Field(
        default=RefValue(1200.0),
        description="Maximum incoming shortwave radiation",
        unit="W m^-2"
    )
    gsmodel: RefValue[int] = Field(
        default=RefValue(1),
        description="Stomatal conductance model selection",
        unit="dimensionless"
    )
    s1: RefValue[float] = Field(
        default=RefValue(0.2),
        description="Lower soil moisture threshold for conductance response",
        unit="dimensionless"
    )
    s2: RefValue[float] = Field(
        default=RefValue(0.5),
        description="Parameter related to soil moisture dependence",
        unit="mm"
    )
    tl: RefValue[float] = Field(
        default=RefValue(0.0),
        description="Lower air temperature threshold for conductance response",
        unit="degC"
    )
    th: RefValue[float] = Field(
        default=RefValue(50.0),
        description="Upper air temperature threshold for conductance response",
        unit="degC"
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

        # Convert scalar parameters to RefValue
        scalar_params = {
            key: RefValue(value) for key, value in scalar_params.items()
        }

        return cls(**scalar_params)


class LAIPowerCoefficients(BaseModel):
    growth_lai: RefValue[float] = Field(
        default=RefValue(0.1),
        description="Power coefficient for LAI in growth equation (LAIPower[1])",
        unit="dimensionless",
    )
    growth_gdd: RefValue[float] = Field(
        default=RefValue(0.1),
        description="Power coefficient for GDD in growth equation (LAIPower[2])",
        unit="dimensionless",
    )
    senescence_lai: RefValue[float] = Field(
        default=RefValue(0.1),
        description="Power coefficient for LAI in senescence equation (LAIPower[3])",
        unit="dimensionless",
    )
    senescence_sdd: RefValue[float] = Field(
        default=RefValue(0.1),
        description="Power coefficient for SDD in senescence equation (LAIPower[4])",
        unit="dimensionless",
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
            RefValue(df.loc[grid_id, ("laipower", f"(0, {veg_idx})")]),
            RefValue(df.loc[grid_id, ("laipower", f"(1, {veg_idx})")]),
            RefValue(df.loc[grid_id, ("laipower", f"(2, {veg_idx})")]),
            RefValue(df.loc[grid_id, ("laipower", f"(3, {veg_idx})")]),
        ]

        # Return the instance with coefficients
        return cls(
            growth_lai=coefficients[0],
            growth_gdd=coefficients[1],
            senescence_lai=coefficients[2],
            senescence_sdd=coefficients[3],
        )


class LAIParams(BaseModel):
    baset: RefValue[float] = Field(
        default=RefValue(10.0),
        description="Base temperature for initiating growing degree days (GDD) for leaf growth",
        unit="degC"
    )
    gddfull: RefValue[float] = Field(
        default=RefValue(100.0),
        description="Growing degree days (GDD) needed for full capacity of LAI",
        unit="degC*day"
    )
    basete: RefValue[float] = Field(
        default=RefValue(10.0),
        description="Base temperature for initiating senescence degree days (SDD) for leaf off",
        unit="degC"
    )
    sddfull: RefValue[float] = Field(
        default=RefValue(100.0),
        description="Senescence degree days (SDD) needed to initiate leaf off",
        unit="degC*day"
    )
    laimin: RefValue[float] = Field(
        default=RefValue(0.1),
        description="Leaf-off wintertime LAI value",
        unit="m^2 m^-2"
    )
    laimax: RefValue[float] = Field(
        default=RefValue(10.0),
        description="Full leaf-on summertime LAI value",
        unit="m^2 m^-2"
    )
    laipower: LAIPowerCoefficients = Field(
        default_factory=LAIPowerCoefficients,
        description="LAI calculation power parameters for growth and senescence",
    )
    laitype: RefValue[int] = Field(
        default=RefValue(0),
        description="LAI calculation choice (0: original, 1: new high latitude)",
        unit="dimensionless"
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

        # Convert scalar parameters to RefValue
        lai_params = {key: RefValue(value) for key, value in lai_params.items()}

        # Extract LAI power coefficients
        laipower = LAIPowerCoefficients.from_df_state(df, grid_id, veg_idx)

        return cls(**lai_params, laipower=laipower)


class VegetatedSurfaceProperties(SurfaceProperties):
    alb: RefValue[float] = Field(
        ge=0, le=1,
        description="Albedo",
        unit="dimensionless",
        default=RefValue(0.2)
    )
    alb_min: RefValue[float] = Field(
        ge=0, le=1,
        description="Minimum albedo",
        unit="dimensionless",
        default=RefValue(0.2)
    )
    alb_max: RefValue[float] = Field(
        ge=0, le=1,
        description="Maximum albedo",
        unit="dimensionless",
        default=RefValue(0.3)
    )
    beta_bioco2: RefValue[float] = Field(
        default=RefValue(0.6), description="Biogenic CO2 exchange coefficient", unit="dimensionless"
    )
    beta_enh_bioco2: RefValue[float] = Field(
        default=RefValue(0.7),
        description="Enhanced biogenic CO2 exchange coefficient",
        unit="dimensionless",
    )
    alpha_bioco2: RefValue[float] = Field(
        default=RefValue(0.8), description="Biogenic CO2 exchange coefficient", unit="dimensionless"
    )
    alpha_enh_bioco2: RefValue[float] = Field(
        default=RefValue(0.9),
        description="Enhanced biogenic CO2 exchange coefficient",
        unit="dimensionless",
    )
    resp_a: RefValue[float] = Field(
        default=RefValue(1.0), description="Respiration coefficient", unit="umol m^-2 s^-1"
    )
    resp_b: RefValue[float] = Field(
        default=RefValue(1.1), description="Respiration coefficient", unit="dimensionless"
    )
    theta_bioco2: RefValue[float] = Field(
        default=RefValue(1.2), description="Biogenic CO2 exchange coefficient", unit="dimensionless"
    )
    maxconductance: RefValue[float] = Field(
        default=RefValue(0.5), description="Maximum surface conductance", unit="mm s^-1"
    )
    min_res_bioco2: RefValue[float] = Field(
        default=RefValue(0.1), description="Minimum respiratory biogenic CO2", unit="umol m^-2 s^-1"
    )
    lai: LAIParams = Field(
        default_factory=LAIParams, description="Leaf area index parameters"
    )
    ie_a: RefValue[float] = Field(
        default=RefValue(0.5),
        description="Irrigation efficiency coefficient-automatic",
        unit="dimensionless",
    )
    ie_m: RefValue[float] = Field(
        default=RefValue(0.6),
        description="Irrigation efficiency coefficient-manual",
        unit="dimensionless",
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
            setattr(instance, attr, RefValue(df.loc[grid_id, (attr, f"({surf_idx-2},)")]))

        instance.lai = LAIParams.from_df_state(df, grid_id, surf_idx)

        return instance


class EvetrProperties(VegetatedSurfaceProperties):  # TODO: Move waterdist VWD here?
    alb: RefValue[float] = Field(
        ge=0, le=1,
        default=RefValue(0.2),
        description="Albedo",
        unit="dimensionless"
    )
    faievetree: RefValue[float] = Field(
        default=RefValue(0.1),
        description="Frontal area index of evergreen trees",
        unit="dimensionless"
    )
    evetreeh: RefValue[float] = Field(
        default=RefValue(15.0),
        description="Evergreen tree height",
        unit="m"
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

        instance.alb = RefValue(df.loc[grid_id, ("alb", "(2,)")])
        instance.faievetree = RefValue(df.loc[grid_id, ("faievetree", "0")])
        instance.evetreeh = RefValue(df.loc[grid_id, ("evetreeh", "0")])

        instance.alb_min = RefValue(df.loc[grid_id, ("albmin_evetr", "0")])
        instance.alb_max = RefValue(df.loc[grid_id, ("albmax_evetr", "0")])

        return instance


class DectrProperties(VegetatedSurfaceProperties):
    alb: RefValue[float] = Field(
        ge=0, le=1,
        default=RefValue(0.2),
        description="Albedo",
        unit="dimensionless"
    )
    faidectree: RefValue[float] = Field(
        default=RefValue(0.1),
        description="Frontal area index of deciduous trees",
        unit="dimensionless"
    )
    dectreeh: RefValue[float] = Field(
        default=RefValue(15.0),
        description="Deciduous tree height",
        unit="m"
    )
    pormin_dec: RefValue[float] = Field(
        ge=0.1, le=0.9,
        default=RefValue(0.2),
        description="Minimum porosity",
        unit="dimensionless"
    )  # pormin_dec cannot be less than 0.1 and greater than 0.9
    pormax_dec: RefValue[float] = Field(
        ge=0.1, le=0.9,
        default=RefValue(0.6),
        description="Maximum porosity",
        unit="dimensionless"
    )  # pormax_dec cannot be less than 0.1 and greater than 0.9
    capmax_dec: RefValue[float] = Field(
        default=RefValue(100.0),
        description="Maximum water capacity",
        unit="mm"
    )
    capmin_dec: RefValue[float] = Field(
        default=RefValue(10.0),
        description="Minimum water capacity",
        unit="mm"
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

        instance.alb = RefValue(df.loc[grid_id, ("alb", "(3,)")])
        instance.faidectree = RefValue(df.loc[grid_id, ("faidectree", "0")])
        instance.dectreeh = RefValue(df.loc[grid_id, ("dectreeh", "0")])
        instance.pormin_dec = RefValue(df.loc[grid_id, ("pormin_dec", "0")])
        instance.pormax_dec = RefValue(df.loc[grid_id, ("pormax_dec", "0")])
        instance.capmax_dec = RefValue(df.loc[grid_id, ("capmax_dec", "0")])
        instance.capmin_dec = RefValue(df.loc[grid_id, ("capmin_dec", "0")])

        instance.alb_min = RefValue(df.loc[grid_id, ("albmin_dectr", "0")])
        instance.alb_max = RefValue(df.loc[grid_id, ("albmax_dectr", "0")])

        return instance


class GrassProperties(VegetatedSurfaceProperties):
    alb: RefValue[float] = Field(
        ge=0, le=1, default=RefValue(0.2), description="Minimum albedo", unit="dimensionless"
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

        instance.alb = RefValue(df.loc[grid_id, ("alb", "(4,)")])
        instance.alb_min = RefValue(df.loc[grid_id, ("albmin_grass", "0")])
        instance.alb_max = RefValue(df.loc[grid_id, ("albmax_grass", "0")])

        return instance


class SnowParams(BaseModel):
    crwmax: RefValue[float] = Field(
        default=RefValue(0.1),
        description="Maximum water holding capacity of snow",
        unit="mm"
    )
    crwmin: RefValue[float] = Field(
        default=RefValue(0.05),
        description="Minimum water holding capacity of snow",
        unit="mm"
    )
    narp_emis_snow: RefValue[float] = Field(
        default=RefValue(0.99),
        description="Snow surface emissivity",
        unit="dimensionless"
    )
    preciplimit: RefValue[float] = Field(
        default=RefValue(2.2),
        description="Temperature threshold for snow vs rain precipitation",
        unit="degC"
    )
    preciplimitalb: RefValue[float] = Field(
        default=RefValue(0.1),
        description="Precipitation threshold for snow albedo aging",
        unit="mm"
    )
    snowalbmax: RefValue[float] = Field(
        default=RefValue(0.85),
        description="Maximum snow albedo",
        unit="dimensionless"
    )
    snowalbmin: RefValue[float] = Field(
        default=RefValue(0.4),
        description="Minimum snow albedo",
        unit="dimensionless"
    )
    snowdensmin: RefValue[float] = Field(
        default=RefValue(100.0),
        description="Minimum snow density",
        unit="kg m^-3"
    )
    snowdensmax: RefValue[float] = Field(
        default=RefValue(400.0),
        description="Maximum snow density",
        unit="kg m^-3"
    )
    snowlimbldg: RefValue = Field(
        default=RefValue(0.1),
        description="Maximum snow depth limit on buildings",
        unit="m"
    )
    snowlimpaved: RefValue = Field(
        default=RefValue(0.1),
        description="Maximum snow depth limit on paved surfaces",
        unit="m"
    )
    snowprof_24hr: HourlyProfile = Field(
        default_factory=HourlyProfile, description="24-hour snow profile"
    )
    tau_a: RefValue[float] = Field(
        default=RefValue(0.018),
        description="Time constant for snow albedo aging in cold snow",
        unit="dimensionless"
    )
    tau_f: RefValue[float] = Field(
        default=RefValue(0.11),
        description="Time constant for snow albedo aging in melting snow",
        unit="dimensionless"
    )
    tau_r: RefValue[float] = Field(
        default=RefValue(0.05),
        description="Time constant for snow albedo aging in refreezing snow",
        unit="dimensionless"
    )
    tempmeltfact: RefValue[float] = Field(
        default=RefValue(0.12),
        description="Hourly temperature melt factor of snow",
        unit="mm K^-1 h^-1"
    )
    radmeltfact: RefValue[float] = Field(
        default=RefValue(0.0016),
        description="Hourly radiation melt factor of snow",
        unit="mm W^-1 m^2 h^-1"
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

        # Convert scalar parameters to RefValue
        scalar_params = {
            key: RefValue(value) for key, value in scalar_params.items()
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
    BuildingCount: RefValue[int] = Field(
        default=RefValue(1), description="Number of buildings of this archetype [-]", unit="dimensionless"
    )
    Occupants: RefValue[int] = Field(
        default=RefValue(1),
        description="Number of occupants present in building [-]",
        unit="dimensionless",
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

    stebbs_Height: RefValue[float] = Field(
        default=RefValue(10.0),
        description="Building height [m]",
        unit="m",
        gt=0.0,
    )
    FootprintArea: RefValue[float] = Field(
        default=RefValue(64.0),
        description="Building footprint area [m2]",
        unit="m^2",
        gt=0.0,
    )
    WallExternalArea: RefValue[float] = Field(
        default=RefValue(80.0),
        description="External wall area (including window area) [m2]",
        unit="m^2",
        gt=0.0,
    )
    RatioInternalVolume: RefValue[float] = Field(
        default=RefValue(0.01),
        description="Ratio of internal mass volume to total building volume [-]",
        unit="dimensionless",
        ge=0.0,
        le=1.0,
    )
    WWR: RefValue[float] = Field(
        default=RefValue(0.20),
        description="window to wall ratio [-]",
        unit="dimensionless",
        ge=0.0,
        le=1.0,
    )
    WallThickness: RefValue[float] = Field(
        default=RefValue(20.0),
        description="Thickness of external wall and roof (weighted) [m]",
        unit="m",
        gt=0.0,
    )
    WallEffectiveConductivity: RefValue[float] = Field(
        default=RefValue(60.0),
        description="Effective thermal conductivity of walls and roofs (weighted) [W m-1 K-1]",
        unit="W m^-1 K^-1",
        gt=0.0,
    )
    WallDensity: RefValue[float] = Field(
        default=RefValue(1600.0),
        description="Effective density of the walls and roof (weighted) [kg m-3]",
        unit="kg m^-3",
        gt=0.0,
    )
    WallCp: RefValue[float] = Field(
        default=RefValue(850.0),
        description="Effective specific heat capacity of walls and roof (weighted) [J kg-1 K-1]",
        unit="J kg^-1 K^-1",
        gt=0.0,
    )
    Wallx1: RefValue[float] = Field(
        default=RefValue(1.0),
        description="Weighting factor for heat capacity of walls and roof [-]",
        unit="dimensionless",
        ge=0.0,
        le=1.0,
    )
    WallExternalEmissivity: RefValue[float] = Field(
        default=RefValue(0.9),
        description="Emissivity of the external surface of walls and roof [-]",
        unit="dimensionless",
        ge=0.0,
        le=1.0,
    )
    WallInternalEmissivity: RefValue[float] = Field(
        default=RefValue(0.9),
        description="Emissivity of the internal surface of walls and roof [-]",
        unit="dimensionless",
        ge=0.0,
        le=1.0,
    )
    WallTransmissivity: RefValue[float] = Field(
        default=RefValue(0.0),
        description="Transmissivity of walls and roof [-]",
        unit="dimensionless",
        ge=0.0,
        le=1.0,
    )
    WallAbsorbtivity: RefValue[float] = Field(
        default=RefValue(0.8),
        description="Absorbtivity of walls and roof [-]",
        unit="dimensionless",
        ge=0.0,
        le=1.0,
    )
    WallReflectivity: RefValue[float] = Field(
        default=RefValue(0.2),
        description="Reflectivity of the external surface of walls and roof [-]",
        unit="dimensionless",
        ge=0.0,
        le=1.0,
    )
    FloorThickness: RefValue[float] = Field(
        default=RefValue(0.2),
        description="Thickness of ground floor [m]",
        unit="m",
        gt=0.0,
    )
    GroundFloorEffectiveConductivity: RefValue[float] = Field(
        default=RefValue(0.15),
        description="Effective thermal conductivity of ground floor [W m-1 K-1]",
        unit="W m^-1 K^-1",
        gt=0.0,
    )
    GroundFloorDensity: RefValue[float] = Field(
        default=RefValue(500.0),
        description="Density of the ground floor [kg m-3]",
        unit="kg m^-3",
        gt=0.0,
    )
    GroundFloorCp: RefValue[float] = Field(
        default=RefValue(1500.0),
        description="Effective specific heat capacity of the ground floor [J kg-1 K-1]",
        unit="J kg^-1 K^-1",
        gt=0.0,
    )
    WindowThickness: RefValue[float] = Field(
        default=RefValue(0.015),
        description="Window thickness [m]",
        unit="m",
        gt=0.0,
    )
    WindowEffectiveConductivity: RefValue[float] = Field(
        default=RefValue(1.0),
        description="Effective thermal conductivity of windows [W m-1 K-1]",
        unit="W m^-1 K^-1",
        gt=0.0,
    )
    WindowDensity: RefValue[float] = Field(
        default=RefValue(2500.0),
        description="Effective density of the windows [kg m-3]",
        unit="kg m^-3",
        gt=0.0,
    )
    WindowCp: RefValue[float] = Field(
        default=RefValue(840.0),
        description="Effective specific heat capacity of windows [J kg-1 K-1]",
        unit="J kg^-1 K^-1",
        gt=0.0,
    )
    WindowExternalEmissivity: RefValue[float] = Field(
        default=RefValue(0.90),
        description="Emissivity of the external surface of windows [-]",
        unit="dimensionless",
        ge=0.0,
        le=1.0,
    )
    WindowInternalEmissivity: RefValue[float] = Field(
        default=RefValue(0.90),
        description="Emissivity of the internal surface of windows [-]",
        unit="dimensionless",
        ge=0.0,
        le=1.0,
    )
    WindowTransmissivity: RefValue[float] = Field(
        default=RefValue(0.90),
        description="Transmissivity of windows [-]",
        unit="dimensionless",
        ge=0.0,
        le=1.0,
    )
    WindowAbsorbtivity: RefValue[float] = Field(
        default=RefValue(0.01),
        description="Absorbtivity of windows [-]",
        unit="dimensionless",
        ge=0.0,
        le=1.0,
    )
    WindowReflectivity: RefValue[float] = Field(
        default=RefValue(0.09),
        description="Reflectivity of the external surface of windows [-]",
        unit="dimensionless",
        ge=0.0,
        le=1.0,
    )
    # TODO: Add defaults below here
    InternalMassDensity: RefValue[float] = Field(
        default=RefValue(0.0),
        description="Effective density of the internal mass [kg m-3]",
        unit="kg m^-3",
    )
    InternalMassCp: RefValue[float] = Field(
        default=RefValue(0.0),
        description="Specific heat capacity of internal mass [J kg-1 K-1]",
        unit="J kg^-1 K^-1",
    )
    InternalMassEmissivity: RefValue[float] = Field(
        default=RefValue(0.0), description="Emissivity of internal mass [-]",
        unit="dimensionless",
    )
    MaxHeatingPower: RefValue[float] = Field(
        default=RefValue(0.0),
        description="Maximum power demand of heating system [W]",
        unit="W",
    )
    WaterTankWaterVolume: RefValue[float] = Field(
        default=RefValue(0.0), description="Volume of water in hot water tank [m3]",
        unit="m^3",
    )
    MaximumHotWaterHeatingPower: RefValue[float] = Field(
        default=RefValue(0.0),
        description="Maximum power demand of water heating system [W]",
        unit="W",
    )
    HeatingSetpointTemperature: RefValue[float] = Field(
        default=RefValue(0.0), description="Heating setpoint temperature [degC]",
        unit="degC",
    )
    CoolingSetpointTemperature: RefValue[float] = Field(
        default=RefValue(0.0), description="Cooling setpoint temperature [degC]",
        unit="degC",
    )

    ref: Optional[Reference] = None

    def to_df_state(self, grid_id: int) -> pd.DataFrame:
        """Convert ArchetypeProperties to DataFrame state format."""

        df_state = init_df_state(grid_id)

        # Create an empty DataFrame with MultiIndex columns
        columns = [
            (field.lower(), "0") for field in self.__class__.model_fields.keys() if field != "ref"
        ]
        df_state = pd.DataFrame(
            index=[grid_id], columns=pd.MultiIndex.from_tuples(columns)
        )

        # Set the values in the DataFrame
        for field_name, field_info in self.__class__.model_fields.items():
            if field_name == "ref":
                continue
            attribute = getattr(self, field_name)
            if isinstance(attribute, RefValue):
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

        # Convert params to RefValue
        non_value_with_doi = ["BuildingType", "BuildingName"]
        params = {
            key: (RefValue(value) if key not in non_value_with_doi else value)
            for key, value in params.items()
        }

        # Create an instance using the extracted parameters
        return cls(**params)


class StebbsProperties(BaseModel):
    WallInternalConvectionCoefficient: RefValue[float] = Field(
        default=RefValue(0.0),
        description="Internal convection coefficient of walls and roof [W m-2 K-1]",
        unit="W m^-2 K^-1",
    )
    InternalMassConvectionCoefficient: RefValue[float] = Field(
        default=RefValue(0.0),
        description="Convection coefficient of internal mass [W m-2 K-1]",
        unit="W m^-2 K^-1",
    )
    FloorInternalConvectionCoefficient: RefValue[float] = Field(
        default=RefValue(0.0),
        description="Internal convection coefficient of ground floor [W m-2 K-1]",
        unit="W m^-2 K^-1",
    )
    WindowInternalConvectionCoefficient: RefValue[float] = Field(
        default=RefValue(0.0),
        description="Internal convection coefficient of windows [W m-2 K-1]",
        unit="W m^-2 K^-1",
    )
    WallExternalConvectionCoefficient: RefValue[float] = Field(
        default=RefValue(0.0),
        description="Initial external convection coefficient of walls and roof [W m-2 K-1]",
        unit="W m^-2 K^-1",
    )
    WindowExternalConvectionCoefficient: RefValue[float] = Field(
        default=RefValue(0.0),
        description="Initial external convection coefficient of windows [W m-2 K-1]",
        unit="W m^-2 K^-1",
    )
    GroundDepth: RefValue[float] = Field(
        default=RefValue(0.0),
        description="Depth of external ground (deep soil) [m]",
        unit="m",
    )
    ExternalGroundConductivity: RefValue[float] = Field(
        default=RefValue(0.0), description="External ground thermal conductivity", unit="W m^-1 K^-1"
    )
    IndoorAirDensity: RefValue[float] = Field(
        default=RefValue(0.0), description="Density of indoor air [kg m-3]", unit="kg m^-3"
    )
    IndoorAirCp: RefValue[float] = Field(
        default=RefValue(0.0),
        description="Specific heat capacity of indoor air [J kg-1 K-1]",
        unit="J kg^-1 K^-1",
    )
    WallBuildingViewFactor: RefValue[float] = Field(
        default=RefValue(0.0),
        description="Building view factor of external walls [-]",
        unit="dimensionless",
    )
    WallGroundViewFactor: RefValue[float] = Field(
        default=RefValue(0.0),
        description="Ground view factor of external walls [-]",
        unit="dimensionless",
    )
    WallSkyViewFactor: RefValue[float] = Field(
        default=RefValue(0.0), description="Sky view factor of external walls [-]", unit="dimensionless"
    )
    MetabolicRate: RefValue[float] = Field(
        default=RefValue(0.0),
        description="Metabolic rate of building occupants [W]",
        unit="W",
    )
    LatentSensibleRatio: RefValue[float] = Field(
        default=RefValue(0.0),
        description="Latent-to-sensible ratio of metabolic energy release of occupants [-]",
        unit="dimensionless",
    )
    ApplianceRating: RefValue[float] = Field(
        default=RefValue(0.0), description="Power demand of single appliance [W]", unit="W"
    )
    TotalNumberofAppliances: RefValue[float] = Field(
        default=RefValue(0.0),
        description="Number of appliances present in building [-]",
        unit="dimensionless",
    )
    ApplianceUsageFactor: RefValue[float] = Field(
        default=RefValue(0.0), description="Number of appliances in use [-]", unit="dimensionless"
    )
    HeatingSystemEfficiency: RefValue[float] = Field(
        default=RefValue(0.0), description="Efficiency of space heating system [-]", unit="dimensionless"
    )
    MaxCoolingPower: RefValue[float] = Field(
        default=RefValue(0.0),
        description="Maximum power demand of cooling system [W]",
        unit="W",
    )
    CoolingSystemCOP: RefValue[float] = Field(
        default=RefValue(0.0),
        description="Coefficient of performance of cooling system [-]",
        unit="dimensionless",
    )
    VentilationRate: RefValue[float] = Field(
        default=RefValue(0.0),
        description="Ventilation rate (air changes per hour, ACH) [h-1]",
        unit="h^-1",
    )
    IndoorAirStartTemperature: RefValue[float] = Field(
        default=RefValue(0.0), description="Initial indoor air temperature [degC]",
        unit="degC",
    )
    IndoorMassStartTemperature: RefValue[float] = Field(
        default=RefValue(0.0), description="Initial indoor mass temperature [degC]",
        unit="degC",
    )
    WallIndoorSurfaceTemperature: RefValue[float] = Field(
        default=RefValue(0.0),
        description="Initial wall/roof indoor surface temperature [degC]",
        unit="degC",
    )
    WallOutdoorSurfaceTemperature: RefValue[float] = Field(
        default=RefValue(0.0),
        description="Initial wall/roof outdoor surface temperature [degC]",
        unit="degC",
    )
    WindowIndoorSurfaceTemperature: RefValue[float] = Field(
        default=RefValue(0.0),
        description="Initial window indoor surface temperature [degC]",
        unit="degC",
    )
    WindowOutdoorSurfaceTemperature: RefValue[float] = Field(
        default=RefValue(0.0),
        description="Initial window outdoor surface temperature [degC]",
        unit="degC",
    )
    GroundFloorIndoorSurfaceTemperature: RefValue[float] = Field(
        default=RefValue(0.0),
        description="Initial ground floor indoor surface temperature [degC]",
        unit="degC",
    )
    GroundFloorOutdoorSurfaceTemperature: RefValue[float] = Field(
        default=RefValue(0.0),
        description="Initial ground floor outdoor surface temperature [degC]",
        unit="degC",
    )
    WaterTankTemperature: RefValue[float] = Field(
        default=RefValue(0.0),
        description="Initial water temperature in hot water tank [degC]",
        unit="degC",
    )
    InternalWallWaterTankTemperature: RefValue[float] = Field(
        default=RefValue(0.0),
        description="Initial hot water tank internal wall temperature [degC]",
        unit="degC",
    )
    ExternalWallWaterTankTemperature: RefValue[float] = Field(
        default=RefValue(0.0),
        description="Initial hot water tank external wall temperature [degC]",
        unit="degC",
    )
    WaterTankWallThickness: RefValue[float] = Field(
        default=RefValue(0.0), description="Hot water tank wall thickness [m]",
        unit="m",
    )
    MainsWaterTemperature: RefValue[float] = Field(
        default=RefValue(0.0),
        description="Temperature of water coming into the water tank [degC]",
        unit="degC",
    )
    WaterTankSurfaceArea: RefValue[float] = Field(
        default=RefValue(0.0),
        description="Surface area of hot water tank cylinder [m2]",
        unit="m^2",
    )
    HotWaterHeatingSetpointTemperature: RefValue[float] = Field(
        default=RefValue(0.0), description="Water tank setpoint temperature [degC]",
        unit="degC",
    )
    HotWaterTankWallEmissivity: RefValue[float] = Field(
        default=RefValue(0.0),
        description="Effective external wall emissivity of the hot water tank [-]",
        unit="dimensionless",
    )
    DomesticHotWaterTemperatureInUseInBuilding: RefValue[float] = Field(
        default=RefValue(0.0),
        description="Initial water temperature of water held in use in building [degC]",
        unit="degC",
    )
    InternalWallDHWVesselTemperature: RefValue[float] = Field(
        default=RefValue(0.0),
        description="Initial hot water vessel internal wall temperature [degC]",
        unit="degC",
    )
    ExternalWallDHWVesselTemperature: RefValue[float] = Field(
        default=RefValue(0.0),
        description="Initial hot water vessel external wall temperature [degC]",
        unit="degC",
    )
    DHWVesselWallThickness: RefValue[float] = Field(
        default=RefValue(0.0), description="Hot water vessel wall thickness [m]",
        unit="m",
    )
    DHWWaterVolume: RefValue[float] = Field(
        default=RefValue(0.0),
        description="Volume of water held in use in building [m3]",
        unit="m^3",
    )
    DHWSurfaceArea: RefValue[float] = Field(
        default=RefValue(0.0),
        description="Surface area of hot water in vessels in building [m2]",
        unit="m^2",
    )
    DHWVesselEmissivity: RefValue[float] = Field(
        default=RefValue(0.0),
        description="NEEDS CHECKED! NOT USED (assumed same as DHWVesselWallEmissivity) [-]",
        unit="dimensionless",
    )
    HotWaterFlowRate: RefValue[float] = Field(
        default=RefValue(0.0),
        description="Hot water flow rate from tank to vessel [m3 s-1]",
        unit="m^3 s^-1",
    )
    DHWDrainFlowRate: RefValue[float] = Field(
        default=RefValue(0.0),
        description="Flow rate of hot water held in building to drain [m3 s-1]",
        unit="m^3 s^-1",
    )
    DHWSpecificHeatCapacity: RefValue[float] = Field(
        default=RefValue(0.0),
        description="Specific heat capacity of hot water [J kg-1 K-1]",
        unit="J kg^-1 K^-1",
    )
    HotWaterTankSpecificHeatCapacity: RefValue[float] = Field(
        default=RefValue(0.0),
        description="Specific heat capacity of hot water tank wal [J kg-1 K-1]",
        unit="J kg^-1 K^-1",
    )
    DHWVesselSpecificHeatCapacity: RefValue[float] = Field(
        default=RefValue(0.0),
        description="Specific heat capacity of vessels containing hot water in use in buildings [J kg-1 K-1]",
        unit="J kg^-1 K^-1",
    )
    DHWDensity: RefValue[float] = Field(
        default=RefValue(0.0), description="Density of hot water in use [kg m-3]",
        unit="kg m^-3",
    )
    HotWaterTankWallDensity: RefValue[float] = Field(
        default=RefValue(0.0), description="Density of hot water tank wall [kg m-3]",
        unit="kg m^-3",
    )
    DHWVesselDensity: RefValue[float] = Field(
        default=RefValue(0.0),
        description="Density of vessels containing hot water in use [kg m-3]",
        unit="kg m^-3",
    )
    HotWaterTankBuildingWallViewFactor: RefValue[float] = Field(
        default=RefValue(0.0),
        description="Water tank/vessel internal building wall/roof view factor [-]",
        unit="dimensionless",
    )
    HotWaterTankInternalMassViewFactor: RefValue[float] = Field(
        default=RefValue(0.0),
        description="Water tank/vessel building internal mass view factor [-]",
        unit="dimensionless",
    )
    HotWaterTankWallConductivity: RefValue[float] = Field(
        default=RefValue(0.0),
        description="Effective wall conductivity of the hot water tank [W m-1 K-1]",
        unit="W m^-1 K^-1",
    )
    HotWaterTankInternalWallConvectionCoefficient: RefValue[float] = Field(
        default=RefValue(0.0),
        description="Effective internal wall convection coefficient of the hot water tank [W m-2 K-1]",
        unit="W m^-2 K^-1",
    )
    HotWaterTankExternalWallConvectionCoefficient: RefValue[float] = Field(
        default=RefValue(0.0),
        description="Effective external wall convection coefficient of the hot water tank [W m-2 K-1]",
        unit="W m^-2 K^-1",
    )
    DHWVesselWallConductivity: RefValue[float] = Field(
        default=RefValue(0.0),
        description="Effective wall conductivity of the hot water tank [W m-1 K-1]",
        unit="W m^-1 K^-1",
    )
    DHWVesselInternalWallConvectionCoefficient: RefValue[float] = Field(
        default=RefValue(0.0),
        description="Effective internal wall convection coefficient of the vessels holding hot water in use in building [W m-2 K-1]",
        unit="W m^-2 K^-1",
    )
    DHWVesselExternalWallConvectionCoefficient: RefValue[float] = Field(
        default=RefValue(0.0),
        description="Effective external wall convection coefficient of the vessels holding hot water in use in building [W m-2 K-1]",
        unit="W m^-2 K^-1",
    )
    DHWVesselWallEmissivity: RefValue[float] = Field(
        default=RefValue(0.0),
        description="Effective external wall emissivity of hot water being used within building [-]",
        unit="dimensionless",
    )
    HotWaterHeatingEfficiency: RefValue[float] = Field(
        default=RefValue(0.0), description="Efficiency of hot water system [-]", unit="dimensionless"
    )
    MinimumVolumeOfDHWinUse: RefValue[float] = Field(
        default=RefValue(0.0), description="Minimum volume of hot water in use [m3]", unit="m^3"
    )

    ref: Optional[Reference] = None

    def to_df_state(self, grid_id: int) -> pd.DataFrame:
        """Convert StebbsProperties to DataFrame state format."""
        df_state = init_df_state(grid_id)

        # Create an empty DataFrame with MultiIndex columns
        columns = [
            (field.lower(), "0") for field in self.__class__.model_fields.keys() if field != "ref"
        ]
        df_state = pd.DataFrame(
            index=[grid_id], columns=pd.MultiIndex.from_tuples(columns)
        )

        # Set the values in the DataFrame
        for field_name, field_info in self.__class__.model_fields.items():
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

        # Convert params to RefValue
        params = {key: RefValue(value) for key, value in params.items()}

        # Create an instance using the extracted parameters
        return cls(**params)


class SPARTACUSParams(BaseModel):
    air_ext_lw: RefValue[float] = Field(
        default=RefValue(0.0),
        description="Air extinction coefficient for longwave radiation",
        unit="m^-1",
    )
    air_ext_sw: RefValue[float] = Field(
        default=RefValue(0.0),
        description="Air extinction coefficient for shortwave radiation",
        unit="m^-1",
    )
    air_ssa_lw: RefValue[float] = Field(
        default=RefValue(0.5),
        description="Air single scattering albedo for longwave radiation",
        unit="dimensionless",
    )
    air_ssa_sw: RefValue[float] = Field(
        default=RefValue(0.5),
        description="Air single scattering albedo for shortwave radiation",
        unit="dimensionless",
    )
    ground_albedo_dir_mult_fact: RefValue[float] = Field(
        default=RefValue(1.0),
        description="Multiplication factor for direct ground albedo",
        unit="dimensionless"
    )
    n_stream_lw_urban: RefValue[int] = Field(
        default=RefValue(2),
        description="Number of streams for longwave radiation in urban areas",
        unit="dimensionless",
    )
    n_stream_sw_urban: RefValue[int] = Field(
        default=RefValue(2),
        description="Number of streams for shortwave radiation in urban areas",
        unit="dimensionless",
    )
    n_vegetation_region_urban: RefValue[int] = Field(
        default=RefValue(1),
        description="Number of vegetation regions in urban areas",
        unit="dimensionless",
    )
    sw_dn_direct_frac: RefValue[float] = Field(
        default=RefValue(0.5),
        description="Fraction of downward shortwave radiation that is direct",
        unit="dimensionless",
    )
    use_sw_direct_albedo: RefValue[float] = Field(
        default=RefValue(1.0),
        description="Flag to use direct albedo for shortwave radiation",
        unit="dimensionless"
    )
    veg_contact_fraction_const: RefValue[float] = Field(
        default=RefValue(0.5), description="Constant vegetation contact fraction", unit="dimensionless"
    )
    veg_fsd_const: RefValue[float] = Field(
        default=RefValue(0.5),
        description="Constant vegetation fractional standard deviation",
        unit="dimensionless",
    )
    veg_ssa_lw: RefValue[float] = Field(
        default=RefValue(0.5),
        description="Vegetation single scattering albedo for longwave radiation",
        unit="dimensionless",
    )
    veg_ssa_sw: RefValue[float] = Field(
        default=RefValue(0.5),
        description="Vegetation single scattering albedo for shortwave radiation",
        unit="dimensionless",
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
            param: RefValue(df.loc[grid_id, (param, "0")])
            for param in spartacus_params
        }

        return cls(**params)


class LUMPSParams(BaseModel):
    raincover: RefValue[float] = Field(ge=0, le=1, default=RefValue(0.25), description="Rain water coverage fraction", unit="dimensionless")
    rainmaxres: RefValue[float] = Field(ge=0, le=20, default=RefValue(0.25), description="Maximum rain water storage", unit="mm")
    drainrt: RefValue[float] = Field(ge=0, le=1, default=RefValue(0.25), description="Drainage rate coefficient", unit="dimensionless")
    veg_type: RefValue[int] = Field(default=RefValue(1), description="Vegetation type selection", unit="dimensionless")

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

        # Convert attributes to RefValue
        params = {key: RefValue(value) for key, value in params.items()}

        return cls(**params)


class SiteProperties(BaseModel):
    lat: RefValue[float] = Field(
        ge=-90,
        le=90,
        description="Latitude of the site in degrees",
        unit="degrees",
        default=RefValue(51.5),
    )
    lng: RefValue[float] = Field(
        ge=-180,
        le=180,
        description="Longitude of the site in degrees",
        unit="degrees",
        default=RefValue(-0.13),
    )
    alt: RefValue[float] = Field(
        gt=0,
        description="Altitude of the site above sea level",
        unit="m",
        default=RefValue(40.0),
    )
    timezone: RefValue[int] = Field(
        ge=-12,
        le=12,
        description="Time zone offset from UTC",
        unit="hour",
        default=RefValue(0),
    )
    surfacearea: RefValue[float] = Field(
        gt=0,
        description="Total surface area of the site",
        unit="ha",
        default=RefValue(1.0),
    )
    z: RefValue[float] = Field(
        gt=0,
        description="Measurement height",
        unit="m",
        default=RefValue(10.0)
    )
    z0m_in: RefValue[float] = Field(
        gt=0,
        description="Momentum roughness length",
        unit="m",
        default=RefValue(1.0),
    )
    zdm_in: RefValue[float] = Field(
        gt=0,
        description="Zero-plane displacement height",
        unit="m",
        default=RefValue(5.0),
    )
    pipecapacity: RefValue[float] = Field(
        gt=0,
        description="Maximum capacity of drainage pipes",
        unit="mm h^-1",
        default=RefValue(100.0),
    )
    runofftowater: RefValue[float] = Field(
        ge=0,
        le=1,
        description="Fraction of excess water going to water bodies",
        unit="dimensionless",
        default=RefValue(0.0),
    )
    narp_trans_site: RefValue[float] = Field(
        description="Site-specific NARP transmission coefficient",
        unit="dimensionless",
        default=RefValue(0.2),
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

    n_buildings: RefValue[int] = Field(
        default=RefValue(1),
        description="Number of buildings in the site",
        unit="dimensionless",
    )

    h_std: RefValue[float] = Field(
        default=RefValue(10.0),
        description="Standard deviation of building heights in the site",
        unit="m",
    )

    lambda_c: RefValue[float] = Field(
        default=RefValue(0),
        description="External building surface area to plan area ratio",
        unit="m^2 m^-2",
        ge=0
    )

    ref: Optional[Reference] = None

    class Config:
        extra = "forbid"  # This will prevent extra fields from being accepted
        validate_assignment = True  # This will validate fields on assignment
        validate_default = True  # This will validate default values

    @model_validator(mode="after")
    def validate_required_fields(self) -> "SiteProperties":
        """Validate that all required fields are present and have valid values."""
        errors = []

        # List of required fields that must be present and non-None
        required_fields = [
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
            "lumps",
            "spartacus",
            "conductance",
            "irrigation",
            "anthropogenic_emissions",
            "snow",
            "land_cover",
            "vertical_layers",
        ]

        for field in required_fields:
            value = getattr(self, field, None)
            if value is None:
                errors.append(f"Required field '{field}' is missing")
            elif isinstance(value, RefValue) and value.value is None:
                errors.append(f"Required field '{field}' has no value")

        # Additional validation rules
        if self.z0m_in.value >= self.zdm_in.value:
            errors.append(
                f"z0m_in ({self.z0m_in.value}) must be less than zdm_in ({self.zdm_in.value})"
            )

        if errors:
            raise ValueError("\n".join(errors))

        return self

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
            params[var] = RefValue(df.loc[grid_id, (var, "0")])

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
    snowalb: RefValue[float] = Field(
        description="Snow albedo",
        unit="dimensionless",
        default=RefValue(0.7),
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
        return cls(snowalb=RefValue(snowalb))
