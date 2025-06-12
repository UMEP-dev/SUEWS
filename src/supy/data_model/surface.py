from enum import Enum
from pydantic import BaseModel, Field, PrivateAttr, model_validator
from typing import Optional, Literal, List, Union
import pandas as pd
from .type import RefValue, Reference

from .type import init_df_state

from .ohm import OHM_Coefficient_season_wetness

from .type import SurfaceType

from .hydro import WaterDistribution, StorageDrainParams


class ThermalLayers(BaseModel):
    dz: RefValue[List[float]] = Field(
        default=RefValue([0.1, 0.2, 0.3, 0.4, 0.5]),
        description="Thickness of thermal layers from surface to depth",
        unit="m",
    )
    k: RefValue[List[float]] = Field(
        default=RefValue([1.0, 1.0, 1.0, 1.0, 1.0]),
        description="Thermal conductivity of each thermal layer",
        unit="W m^-1 K^-1",
    )
    cv: RefValue[List[float]] = Field(
        default=RefValue([1000, 1000, 1000, 1000, 1000]),
        description="Volumetric heat capacity of each thermal layer",
        unit="J m^-3 K^-1",
    )

    ref: Optional[Reference] = None

    def to_df_state(
        self,
        grid_id: int,
        idx: int,
        surf_type: Literal[
            "paved",
            "bldgs",
            "evetr",
            "dectr",
            "grass",
            "bsoil",
            "water",
            "roof",
            "wall",
        ],
    ) -> pd.DataFrame:
        """Convert thermal layer parameters to DataFrame state format.

        Args:
            grid_id: Grid ID for the DataFrame index
            surf_type: Surface type or facet type ("roof" or "wall")

        Returns:
            pd.DataFrame: DataFrame containing thermal layer parameters
        """
        df_state = init_df_state(grid_id)

        if surf_type == "roof":
            suffix = "roof"
        elif surf_type == "wall":
            suffix = "wall"
        else:
            suffix = "surf"

        # Add thermal layer parameters
        for i in range(5):
            df_state[(f"dz_{suffix}", f"({idx}, {i})")] = self.dz.value[i]
            df_state[(f"k_{suffix}", f"({idx}, {i})")] = self.k.value[i]
            df_state[(f"cp_{suffix}", f"({idx}, {i})")] = self.cv.value[i] # TODO: Change df_state to use cv instead of cp

        return df_state

    @classmethod
    def from_df_state(
        cls,
        df: pd.DataFrame,
        grid_id: int,
        idx: int,
        surf_type: Union[SurfaceType, Literal["roof", "wall"]],
    ) -> "ThermalLayers":
        """Reconstruct ThermalLayers instance from DataFrame.

        Args:
            df: DataFrame containing thermal layer parameters.
            grid_id: Grid ID for the DataFrame index.
            idx: Surface index for identifying columns.
            surf_type: Surface type or facet type ("roof" or "wall").

        Returns:
            ThermalLayers: Reconstructed ThermalLayers instance.
        """
        dz = []
        k = []
        cv = []

        # Determine suffix based on surf_type
        if surf_type == "roof":
            suffix = "roof"
        elif surf_type == "wall":
            suffix = "wall"
        else:
            suffix = "surf"

        # Extract thermal layer parameters for each of the 5 layers
        for i in range(5):
            dz.append(df.loc[grid_id, (f"dz_{suffix}", f"({idx}, {i})")])
            k.append(df.loc[grid_id, (f"k_{suffix}", f"({idx}, {i})")])
            cv.append(df.loc[grid_id, (f"cp_{suffix}", f"({idx}, {i})")])

        # Convert to RefValue
        dz = RefValue[List[float]](dz)
        k = RefValue[List[float]](k)
        cv = RefValue[List[float]](cv)

        # Return reconstructed instance
        return cls(dz=dz, k=k, cv=cv)


class SurfaceProperties(BaseModel):
    """Base properties for all surface types"""

    sfr: RefValue[float] = Field(
        ge=0,
        le=1,
        description="Surface fraction of grid area covered by this surface type",
        unit="dimensionless",
        default=RefValue(1.0 / 7),
    )
    emis: RefValue[float] = Field(
        ge=0,
        le=1,
        description="Surface emissivity for longwave radiation",
        unit="dimensionless",
        default=RefValue(0.95),
    )
    chanohm: Optional[RefValue[float]] = Field(
        default=RefValue(0.0),
        description="Bulk transfer coefficient for this surface to use in AnOHM",
        unit="J m^-3 K^-1",
    )
    cpanohm: Optional[RefValue[float]] = Field(
        default=RefValue(1200.0),
        description="Volumetric heat capacity for this surface to use in AnOHM",
        unit="J m^-3 K^-1",
    )
    kkanohm: Optional[RefValue[float]] = Field(
        default=RefValue(0.4),
        description="Thermal conductivity for this surface to use in AnOHM",
        unit="W m^-1 K^-1",
    )
    ohm_threshsw: Optional[RefValue[float]] = Field(
        default=RefValue(0.0),
        description="Summer/winter threshold based on temperature for OHM calculation",
        unit="degC",
    )
    ohm_threshwd: Optional[RefValue[float]] = Field(
        default=RefValue(0.0),
        description="Soil moisture threshold determining whether wet/dry OHM coefficients are applied",
        unit="dimensionless",
    )
    ohm_coef: Optional[OHM_Coefficient_season_wetness] = Field(
        default_factory=OHM_Coefficient_season_wetness
    )
    soildepth: RefValue[float] = Field(
        default=RefValue(0.15),
        description="Depth of soil layer for hydrological calculations",
        unit="m",
    )
    soilstorecap: RefValue[float] = Field(
        default=RefValue(150.0),
        description="Maximum water storage capacity of soil",
        unit="mm",
    )
    statelimit: RefValue[float] = Field(
        default=RefValue(10.0),
        description="Minimum water storage capacity for state change",
        unit="mm",
    )
    wetthresh: RefValue[float] = Field(
        default=RefValue(0.5),
        description="Surface wetness threshold for OHM calculations",
        unit="dimensionless",
    )
    sathydraulicconduct: RefValue[float] = Field(
        default=RefValue(0.0001),
        description="Saturated hydraulic conductivity of soil",
        unit="mm s^-1",
    )
    waterdist: Optional[WaterDistribution] = Field(
        default=None,
        description="Water distribution parameters",
    )
    storedrainprm: StorageDrainParams = Field(
        default_factory=StorageDrainParams,
        description="Storage and drain parameters",
    )
    snowpacklimit: Optional[RefValue[float]] = Field(
        default=RefValue(10.0),
        description="Limit of snow that can be held on surface",
        unit="mm",
    )
    thermal_layers: ThermalLayers = Field(
        default_factory=ThermalLayers, description="Thermal layers for the surface"
    )
    irrfrac: Optional[RefValue[float]] = Field(
        default=RefValue(0.0),
        description="Fraction of surface area that can be irrigated",
        unit="dimensionless",
    )
    _surface_type: Optional[SurfaceType] = PrivateAttr(default=None)

    ref: Optional[Reference] = None

    def set_surface_type(self, surface_type: SurfaceType):
        self._surface_type = surface_type
        if self._surface_type == SurfaceType.WATER:
            if self.waterdist is not None:
                raise ValueError("Water surface should not have water distribution")
        else:
            if self.waterdist is None:
                raise ValueError(
                    f"Water distribution required for {self._surface_type.value}"
                )
            self.waterdist.validate_distribution(self._surface_type)

    def get_surface_type(self) -> SurfaceType:
        return self._surface_type

    def get_surface_name(self) -> str:
        return self._surface_type.value

    def get_surface_index(self) -> int:
        dict_surface_type = {
            SurfaceType.PAVED: 0,
            SurfaceType.BLDGS: 1,
            SurfaceType.EVETR: 2,
            SurfaceType.DECTR: 3,
            SurfaceType.GRASS: 4,
            SurfaceType.BSOIL: 5,
            SurfaceType.WATER: 6,
        }
        return dict_surface_type[self._surface_type]

    def to_df_state(self, grid_id: int) -> pd.DataFrame:
        """Convert surface properties to DataFrame state format.
        This is the base implementation that handles common surface properties."""
        df_state = init_df_state(grid_id)

        # Get surface index
        surf_idx = self.get_surface_index()

        # Get surface name
        surf_name = self.get_surface_name()

        # Helper function to set values in DataFrame
        def set_df_value(col_name: str, value: float):
            idx_str = f"({surf_idx},)"
            if (col_name, idx_str) not in df_state.columns:
                # df_state[(col_name, idx_str)] = np.nan
                df_state[(col_name, idx_str)] = None
            df_state.loc[grid_id, (col_name, idx_str)] = value

        # Get all properties of this class using introspection
        properties = [
            "sfr",
            "emis",
            "chanohm",
            "cpanohm",
            "kkanohm",
            "ohm_coef",
            "ohm_threshsw",
            "ohm_threshwd",
            "soildepth",
            "soilstorecap",
            "statelimit",
            "wetthresh",
            "sathydraulicconduct",
            "waterdist",
            "storedrainprm",
            "snowpacklimit",
            "thermal_layers",
            "irrfrac",
        ]
        # drop 'surface_type' and model-specific properties (e.g. model_xx)
        properties = [
            p for p in properties if p != "surface_type" and not p.startswith("model_")
        ]

        # Process each property
        dfs = [df_state]  # List to collect all DataFrames

        for property in properties:
            # Handle nested properties with their own to_df_state methods
            if property in [
                "waterdist",
                "storedrainprm",
                "ohm_coef",
                # "lai",
            ]:
                nested_obj = getattr(self, property)
                if nested_obj is not None and hasattr(nested_obj, "to_df_state"):
                    nested_df = nested_obj.to_df_state(grid_id, surf_idx)
                    dfs.append(nested_df)
            elif property == "thermal_layers":
                nested_df = self.thermal_layers.to_df_state(
                    grid_id, surf_idx, surf_name
                )
                dfs.append(nested_df)
            elif property == "irrfrac":
                value = getattr(self, property)
                value = value.value if isinstance(value, RefValue) else value
                df_state.loc[grid_id, (f"{property}{surf_name}", "0")] = value
            elif property in ["sfr", "soilstorecap", "statelimit", "wetthresh"]:
                value = getattr(self, property)
                value = value.value if isinstance(value, RefValue) else value
                set_df_value(f"{property}_surf", value)
            else:
                value = getattr(self, property)
                value = value.value if isinstance(value, RefValue) else value
                set_df_value(property, value)
            # except Exception as e:
            #     print(f"Warning: Could not set property {property}: {str(e)}")
            #     continue

        # add dummy columns to conform to SUEWS convention
        list_cols = [
            "ohm_threshsw",
            "ohm_threshwd",
        ]
        for col in list_cols:
            df_state[(col, "(7,)")] = 0

        # Merge all DataFrames
        df_final = pd.concat(dfs, axis=1).sort_index(axis=1)
        return df_final

    @classmethod
    def from_df_state(
        cls, df: pd.DataFrame, grid_id: int, surf_idx: int
    ) -> "SurfaceProperties":
        """Reconstruct surface properties from DataFrame state format."""

        # Get surface name
        surf_name = [
            "paved",
            "bldgs",
            "evetr",
            "dectr",
            "grass",
            "bsoil",
            "water",
        ][surf_idx]

        # Get all properties of this class using introspection
        properties = [
            "sfr",
            "emis",
            "chanohm",
            "cpanohm",
            "kkanohm",
            "ohm_coef",
            "ohm_threshsw",
            "ohm_threshwd",
            "soildepth",
            "soilstorecap",
            "statelimit",
            "wetthresh",
            "sathydraulicconduct",
            "waterdist",
            "storedrainprm",
            "snowpacklimit",
            "thermal_layers",
            "irrfrac",
        ]

        # drop 'surface_type' and model-specific properties (e.g. model_xx)
        properties = [
            p for p in properties if p != "surface_type" and not p.startswith("model_")
        ]

        # Create a dictionary to hold the properties and their values
        property_values = {}

        # Process each property
        for property in properties:
            # Handle nested properties with their own from_df_state methods
            if property in [
                "waterdist",
                "storedrainprm",
                # "ohm_coef",
                # "lai",
            ]:
                nested_obj = cls.model_fields[property].annotation
                if nested_obj is not None and hasattr(nested_obj, "from_df_state"):
                    property_values[property] = nested_obj.from_df_state(
                        df, grid_id, surf_idx
                    )
                continue
            elif property == "ohm_coef":  # moved seperately as optional fails hasattr()
                if cls.model_fields[property].annotation is not None:
                    property_values[property] = (
                        OHM_Coefficient_season_wetness.from_df_state(
                            df, grid_id, surf_idx
                        )
                    )
            elif property == "thermal_layers":
                property_values[property] = cls.model_fields[
                    "thermal_layers"
                ].annotation.from_df_state(df, grid_id, surf_idx, surf_name)
            elif property == "irrfrac":
                value = df.loc[grid_id, (f"{property}{surf_name}", "0")]
                property_values[property] = RefValue(value)
            elif property in ["sfr", "soilstorecap", "statelimit", "wetthresh"]:
                value = df.loc[grid_id, (f"{property}_surf", f"({surf_idx},)")]
                property_values[property] = RefValue(value)
            else:
                value = df.loc[grid_id, (property, f"({surf_idx},)")]
                property_values[property] = RefValue(value)

        return cls(**property_values)


class NonVegetatedSurfaceProperties(SurfaceProperties):
    alb: RefValue[float] = Field(
        ge=0,
        le=1,
        description="Surface albedo",
        unit="dimensionless",
        default=RefValue(0.1),
    )

    def to_df_state(self, grid_id: int) -> pd.DataFrame:
        """Convert non-vegetated surface properties to DataFrame state format."""

        # Get base properties from parent
        df_base = super().to_df_state(grid_id)

        surf_idx = self.get_surface_index()

        if self.waterdist is not None:
            df_waterdist = self.waterdist.to_df_state(grid_id, surf_idx)
            df_base = pd.concat([df_base, df_waterdist], axis=1).sort_index(axis=1)

        for attr in ["alb"]:
            df_base.loc[grid_id, (attr, f"({surf_idx},)")] = getattr(self, attr).value
            df_base = df_base.sort_index(axis=1)

        return df_base

    @classmethod
    def from_df_state(
        cls, df: pd.DataFrame, grid_id: int, surf_idx: int
    ) -> "NonVegetatedSurfaceProperties":
        """Reconstruct non-vegetated surface properties from DataFrame state format."""
        instance = super().from_df_state(df, grid_id, surf_idx)
        instance.alb = RefValue(df.loc[grid_id, ("alb", f"({surf_idx},)")])
        return instance


class PavedProperties(
    NonVegetatedSurfaceProperties
):  # May need to move VWD for waterdist to here for referencing
    _surface_type: Literal[SurfaceType.PAVED] = SurfaceType.PAVED
    waterdist: WaterDistribution = Field(
        default_factory=lambda: WaterDistribution(SurfaceType.PAVED),
        description="Water distribution fractions for paved surfaces",
    )

    def to_df_state(self, grid_id: int) -> pd.DataFrame:
        """Convert paved surface properties to DataFrame state format."""
        dfs = []

        # Get base properties from parent
        df_base = super().to_df_state(grid_id)
        dfs.append(df_base)

        surf_idx = self.get_surface_index()

        # Create DataFrame for this class's properties
        param_tuples = []
        values = []

        # Add all non-inherited properties that aren't model-specific or nested objects
        for attr in dir(self):
            if (
                not attr.startswith("_")
                and not callable(getattr(self, attr))
                and not attr.startswith("model_")
                and attr
                not in [
                    "_surface_type",
                    "waterdist",
                    "storedrainprm",
                    "thermal_layers",
                    "ohm_coef",
                ]
                and attr not in dir(super())
            ):
                value = getattr(self, attr)
                if not isinstance(value, (BaseModel, Enum)):
                    param_tuples.append((attr, (surf_idx,)))
                    values.append(value)

        if param_tuples:  # Only create DataFrame if we have properties to add
            columns = pd.MultiIndex.from_tuples(param_tuples, names=["var", "ind_dim"])
            df = pd.DataFrame(
                index=pd.Index([grid_id], name="grid"),
                columns=columns,
                data=[values],
                dtype=float,
            )
            dfs.append(df)

        # Add nested property DataFrames
        for nested_prop in ["waterdist", "storedrainprm", "thermal_layers", "ohm_coef"]:
            nested_obj = getattr(self, nested_prop)
            if nested_obj is not None and hasattr(nested_obj, "to_df_state"):
                if nested_prop == "thermal_layers":
                    surf_name = self.get_surface_name()
                    nested_df = nested_obj.to_df_state(grid_id, surf_idx, surf_name)
                else:
                    nested_df = nested_obj.to_df_state(grid_id, surf_idx)
                dfs.append(nested_df)

        # Merge all DataFrames
        df_final = pd.concat(dfs, axis=1)
        return df_final

    @classmethod
    def from_df_state(cls, df: pd.DataFrame, grid_id: int) -> "PavedProperties":
        """Reconstruct paved surface properties from DataFrame state format."""
        surf_idx = 0
        instance = super().from_df_state(df, grid_id, surf_idx)
        return instance


class BuildingLayer(
    BaseModel
):  # May need to move VWD for thermal layers here for referencing
    alb: RefValue[float] = Field(
        ge=0,
        le=1,
        description="Surface albedo",
        unit="dimensionless",
        default=RefValue(0.1),
    )
    emis: RefValue[float] = Field(
        ge=0,
        le=1,
        description="Surface emissivity",
        unit="dimensionless",
        default=RefValue(0.95),
    )
    thermal_layers: ThermalLayers = Field(
        default_factory=ThermalLayers,
        description="Thermal layers for the surface",
    )
    statelimit: RefValue[float] = Field(
        default=RefValue(10.0),
        description="Minimum water storage capacity for state change",
        unit="mm",
    )
    soilstorecap: RefValue[float] = Field(
        default=RefValue(150.0),
        description="Maximum water storage capacity of soil",
        unit="mm",
    )
    wetthresh: RefValue[float] = Field(
        default=RefValue(0.5),
        description="Surface wetness threshold for OHM calculations",
        unit="dimensionless",
    )
    roof_albedo_dir_mult_fact: Optional[RefValue[float]] = Field(
        default=RefValue(0.1),
        description="Directional albedo multiplication factor for roofs",
        unit="dimensionless",
    )
    wall_specular_frac: Optional[RefValue[float]] = Field(
        default=RefValue(0.1),
        description="Specular reflection fraction for walls",
        unit="dimensionless",
    )
    _facet_type: Literal["roof", "wall"] = PrivateAttr(default="roof")

    ref: Optional[Reference] = None

    def to_df_state(
        self,
        grid_id: int,
        layer_idx: int,
        facet_type: Literal["roof", "wall"],
    ) -> pd.DataFrame:
        """Convert building layer parameters to DataFrame state format.

        Args:
            grid_id: Grid ID for the DataFrame index
            layer_idx: Layer index (0 or 1 for two layers)

        Returns:
            pd.DataFrame: DataFrame containing building layer parameters
        """
        df_state = init_df_state(grid_id)

        # Add basic parameters
        df_state[(f"alb_{facet_type}", f"({layer_idx},)")] = self.alb.value
        df_state[(f"emis_{facet_type}", f"({layer_idx},)")] = self.emis.value
        df_state[(f"statelimit_{facet_type}", f"({layer_idx},)")] = (
            self.statelimit.value
        )
        df_state[(f"soilstorecap_{facet_type}", f"({layer_idx},)")] = (
            self.soilstorecap.value
        )
        df_state[(f"wetthresh_{facet_type}", f"({layer_idx},)")] = self.wetthresh.value

        # Determine prefix based on layer type
        prefix = facet_type

        # Add layer-specific parameters
        if facet_type == "roof" and self.roof_albedo_dir_mult_fact is not None:
            df_state[(f"{prefix}_albedo_dir_mult_fact", f"(0, {layer_idx})")] = (
                self.roof_albedo_dir_mult_fact.value
            )
        elif facet_type == "wall" and self.wall_specular_frac is not None:
            df_state[(f"{prefix}_specular_frac", f"(0, {layer_idx})")] = (
                self.wall_specular_frac.value
            )

        # Add thermal layers
        df_thermal = self.thermal_layers.to_df_state(grid_id, layer_idx, facet_type)
        df_state = pd.concat([df_state, df_thermal], axis=1)

        return df_state

    @classmethod
    def from_df_state(
        cls,
        df: pd.DataFrame,
        grid_id: int,
        layer_idx: int,
        facet_type: Literal["roof", "wall"],
    ) -> "BuildingLayer":
        """Reconstruct BuildingLayer instance from DataFrame.

        Args:
            df: DataFrame containing building layer parameters.
            grid_id: Grid ID for the DataFrame index.
            layer_idx: Layer index (0 or 1 for two layers).
            facet_type: Facet type ("roof" or "wall").

        Returns:
            BuildingLayer: Reconstructed BuildingLayer instance.
        """
        # Prefix for the specific layer type
        prefix = facet_type

        # Extract scalar parameters
        params = {
            "alb": df.loc[grid_id, (f"alb_{prefix}", f"({layer_idx},)")],
            "emis": df.loc[grid_id, (f"emis_{prefix}", f"({layer_idx},)")],
            "statelimit": df.loc[grid_id, (f"statelimit_{prefix}", f"({layer_idx},)")],
            "soilstorecap": df.loc[
                grid_id, (f"soilstorecap_{prefix}", f"({layer_idx},)")
            ],
            "wetthresh": df.loc[grid_id, (f"wetthresh_{prefix}", f"({layer_idx},)")],
        }

        # Extract optional parameters
        if facet_type == "roof":
            params["roof_albedo_dir_mult_fact"] = df.loc[
                grid_id, (f"roof_albedo_dir_mult_fact", f"(0, {layer_idx})")
            ]

        elif facet_type == "wall":
            params["wall_specular_frac"] = df.loc[
                grid_id, (f"wall_specular_frac", f"(0, {layer_idx})")
            ]

        # Extract ThermalLayers
        thermal_layers = ThermalLayers.from_df_state(df, grid_id, layer_idx, facet_type)

        # Convert params to VWD - move below thermal_layers if needed
        params = {key: RefValue(value) for key, value in params.items()}

        # Add thermal_layers to params
        params["thermal_layers"] = thermal_layers

        # Return the reconstructed instance
        return cls(**params)


class BldgsProperties(
    NonVegetatedSurfaceProperties
):  # May need to move VWD for waterdist to here for referencing
    _surface_type: Literal[SurfaceType.BLDGS] = SurfaceType.BLDGS
    faibldg: Optional[RefValue[float]] = Field(
        ge=0,
        default=RefValue(0.3),
        description="Frontal area index of buildings",
        unit="dimensionless",
    )
    bldgh: Optional[RefValue[float]] = Field(
        ge=3, default=RefValue(10.0), description="Building height", unit="m"
    )  # We need to check if there is a building - and then this has to be greather than 0, accordingly.
    waterdist: WaterDistribution = Field(
        default_factory=lambda: WaterDistribution(SurfaceType.BLDGS)
    )

    ref: Optional[Reference] = None

    # @model_validator(mode="after")    # This is no longer appropriate - may be reintroduced and altered
    # def validate_rsl_zd_range(self) -> "BldgsProperties":
    #     sfr_bldg_lower_limit = 0.18
    #     if self.sfr < sfr_bldg_lower_limit:
    #         if self.faibldg.value < 0.25 * (1 - self.sfr.value):
    #             raise ValueError(         # This should be a warning not raising an error
    #                 "Frontal Area Index (FAI) is below a lower limit of: 0.25 * (1 - PAI), which is likely to cause a negative displacement height (zd) in the RSL.\n"
    #                 f"\tYou have entered a building FAI of {self.faibldg} and a building PAI of {self.sfr}.\n"
    #                 "\tFor more details, please refer to: https://github.com/UMEP-dev/SUEWS/issues/302"
    #             )
    #     return self

    def to_df_state(self, grid_id: int) -> pd.DataFrame:
        """Convert building properties to DataFrame state format."""
        df_state = super().to_df_state(grid_id).sort_index(axis=1)

        df_state.loc[grid_id, ("faibldg", "0")] = self.faibldg.value
        df_state = df_state.sort_index(axis=1)
        df_state.loc[grid_id, ("bldgh", "0")] = self.bldgh.value

        return df_state

    @classmethod
    def from_df_state(cls, df: pd.DataFrame, grid_id: int) -> "BldgsProperties":
        """Reconstruct building properties from DataFrame state format."""
        surf_idx = 1
        instance = super().from_df_state(df, grid_id, surf_idx)
        instance.bldgh = RefValue(df.loc[grid_id, ("bldgh", "0")])
        instance.faibldg = RefValue(df.loc[grid_id, ("faibldg", "0")])
        return instance


class BsoilProperties(
    NonVegetatedSurfaceProperties
):  # May need to move VWD for waterdist to here for referencing
    _surface_type: Literal[SurfaceType.BSOIL] = SurfaceType.BSOIL
    waterdist: WaterDistribution = Field(
        default_factory=lambda: WaterDistribution(SurfaceType.BSOIL),
        description="Water distribution for bare soil",
    )

    def to_df_state(self, grid_id: int) -> pd.DataFrame:
        """Convert bare soil properties to DataFrame state format."""
        df_state = super().to_df_state(grid_id)
        # df_state.loc[grid_id, ("waterdist", "0")] = self.waterdist
        return df_state

    @classmethod
    def from_df_state(cls, df: pd.DataFrame, grid_id: int) -> "BsoilProperties":
        """Reconstruct bare soil properties from DataFrame state format."""
        surf_idx = 5
        instance = super().from_df_state(df, grid_id, surf_idx)
        return instance


class WaterProperties(NonVegetatedSurfaceProperties):
    _surface_type: Literal[SurfaceType.WATER] = SurfaceType.WATER
    flowchange: RefValue[float] = Field(
        default=RefValue(0.0),
        description="Change in water flow for water bodies",
        unit="mm h^-1",
    )

    def to_df_state(self, grid_id: int) -> pd.DataFrame:
        """Convert water surface properties to DataFrame state format."""
        df_state = super().to_df_state(grid_id)
        surf_idx = self.get_surface_index()

        # Helper function to set values in DataFrame
        def set_df_value(col_name: str, value: float):
            idx_str = f"({surf_idx},)"
            if (col_name, idx_str) not in df_state.columns:
                # df_state[(col_name, idx_str)] = np.nan
                df_state[(col_name, idx_str)] = None
            df_state.loc[grid_id, (col_name, idx_str)] = value

        list_attr = ["flowchange"]

        # Add all non-inherited properties
        df_state.loc[grid_id, ("flowchange", "0")] = self.flowchange.value

        return df_state

    @classmethod
    def from_df_state(cls, df: pd.DataFrame, grid_id: int) -> "WaterProperties":
        """Reconstruct water properties from DataFrame state format."""
        surf_idx = 6
        instance = super().from_df_state(df, grid_id, surf_idx)
        instance.flowchange = RefValue(df.loc[grid_id, ("flowchange", f"0")])
        return instance


class RoofLayer(BuildingLayer):
    _facet_type: Literal["roof"] = "roof"


class WallLayer(BuildingLayer):
    _facet_type: Literal["wall"] = "wall"


class VerticalLayers(BaseModel):
    nlayer: RefValue[int] = Field(
        default=RefValue(3),
        description="Number of vertical layers in the urban canopy",
        unit="dimensionless",
    )
    height: RefValue[List[float]] = Field(
        default=RefValue([0.0, 10.0, 20.0, 30.0]),
        description="Heights of layer boundaries, length must be nlayer+1",
        unit="m",
    )
    veg_frac: RefValue[List[float]] = Field(
        default=RefValue([0.0, 0.0, 0.0]),
        description="Fraction of vegetation in each layer, length must be nlayer",
        unit="dimensionless",
    )
    veg_scale: RefValue[List[float]] = Field(
        default=RefValue([1.0, 1.0, 1.0]),
        description="Scaling factor for vegetation in each layer, length must be nlayer",
        unit="dimensionless",
    )
    building_frac: RefValue[List[float]] = Field(
        default=RefValue([0.4, 0.3, 0.3]),
        description="Fraction of buildings in each layer, must sum to 1.0, length must be nlayer",
        unit="dimensionless",
    )
    building_scale: RefValue[List[float]] = Field(
        default=RefValue([1.0, 1.0, 1.0]),
        description="Scaling factor for buildings in each layer, length must be nlayer",
        unit="dimensionless",
    )
    roofs: List[RoofLayer] = Field(
        default_factory=lambda: [RoofLayer(), RoofLayer(), RoofLayer()],
        description="Properties for roof surfaces in each layer, length must be nlayer",
    )
    walls: List[WallLayer] = Field(
        default_factory=lambda: [WallLayer(), WallLayer(), WallLayer()],
        description="Properties for wall surfaces in each layer, length must be nlayer",
    )

    ref: Optional[Reference] = None

    @model_validator(mode="after")
    def validate_building(self) -> "VerticalLayers":
        # Validate building heights
        if len(self.height.value) != self.nlayer.value + 1:
            raise ValueError(
                f"Number of building heights ({len(self.height.value)}) must match nlayer+1 = ({self.nlayer.value + 1})"
            )

        # Validate building fractions
        if len(self.building_frac.value) != self.nlayer.value:
            raise ValueError(
                f"Number of building fractions ({len(self.building_frac.value)}) must match nlayer ({self.nlayer.value})"
            )
        # This rule is not correct, we just need building_frac to be in range [0,1]
        # if not math.isclose(sum(self.building_frac), 1.0, rel_tol=1e-9):
        #    raise ValueError(
        #        f"Building fractions must sum to 1.0, got {sum(self.building_frac)}"
        #    )

        # Validate building scales
        if len(self.building_scale.value) != self.nlayer.value:
            raise ValueError(
                f"Number of building scales ({len(self.building_scale.value)}) must match nlayer ({self.nlayer.value})"
            )

        # Validate number of roof layers matches nlayer
        if len(self.roofs) != self.nlayer.value:
            raise ValueError(
                f"Number of roof layers ({len(self.roof)}) must match nlayer ({self.nlayer.value})"
            )

        # Validate number of wall layers matches nlayer
        if len(self.walls) != self.nlayer.value:
            raise ValueError(
                f"Number of wall layers ({len(self.wall)}) must match nlayer ({self.nlayer.value})"
            )

        return self

    def to_df_state(self, grid_id: int) -> pd.DataFrame:
        """Convert vertical layers to DataFrame state format."""
        # Initialize empty DataFrame with grid_id index
        df_state = init_df_state(grid_id)

        # Set number of vertical layers
        df_state[(f"nlayer", "0")] = self.nlayer.value

        # Set heights for each layer boundary (nlayer + 1 heights needed)
        for i in range(self.nlayer.value + 1):
            df_state[("height", f"({i},)")] = self.height.value[i]

        # Set vegetation and building parameters for each layer
        for var in ["veg_frac", "veg_scale", "building_frac", "building_scale"]:
            for i in range(self.nlayer.value):
                df_state[(f"{var}", f"({i},)")] = getattr(self, var).value[i]

        # Convert roof and wall properties to DataFrame format for each layer
        df_roofs = pd.concat(
            [
                self.roofs[i].to_df_state(grid_id, i, "roof")
                for i in range(self.nlayer.value)
            ],
            axis=1,
        )
        df_walls = pd.concat(
            [
                self.walls[i].to_df_state(grid_id, i, "wall")
                for i in range(self.nlayer.value)
            ],
            axis=1,
        )

        # Combine all DataFrames
        df_state = pd.concat([df_state, df_roofs, df_walls], axis=1)

        return df_state

    @classmethod
    def from_df_state(cls, df: pd.DataFrame, grid_id: int) -> "VerticalLayers":
        """Reconstruct VerticalLayers instance from DataFrame."""
        # Extract the number of layers
        nlayer = int(df.loc[grid_id, ("nlayer", "0")])

        # Extract heights for each layer boundary
        height = [df.loc[grid_id, ("height", f"({i},)")] for i in range(nlayer + 1)]

        # Extract vegetation and building parameters for each layer
        veg_frac = [df.loc[grid_id, ("veg_frac", f"({i},)")] for i in range(nlayer)]
        veg_scale = [df.loc[grid_id, ("veg_scale", f"({i},)")] for i in range(nlayer)]
        building_frac = [
            df.loc[grid_id, ("building_frac", f"({i},)")] for i in range(nlayer)
        ]
        building_scale = [
            df.loc[grid_id, ("building_scale", f"({i},)")] for i in range(nlayer)
        ]

        # Reconstruct roof and wall properties for each layer
        roofs = [RoofLayer.from_df_state(df, grid_id, i, "roof") for i in range(nlayer)]
        walls = [WallLayer.from_df_state(df, grid_id, i, "wall") for i in range(nlayer)]

        # Construct and return VerticalLayers instance
        return cls(
            nlayer=RefValue(nlayer),
            height=RefValue(height),
            veg_frac=RefValue(veg_frac),
            veg_scale=RefValue(veg_scale),
            building_frac=RefValue(building_frac),
            building_scale=RefValue(building_scale),
            roofs=roofs,
            walls=walls,
        )
