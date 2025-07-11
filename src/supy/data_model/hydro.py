from pydantic import ConfigDict, BaseModel, Field, PrivateAttr
from typing import Optional
import pandas as pd
from .type import RefValue, Reference, SurfaceType, FlexibleRefValue
import math


class WaterDistribution(BaseModel):
    # Optional fields for all possible distributions
    to_paved: Optional[FlexibleRefValue(float)] = Field(
        None,
        description="Fraction of water redistributed to paved surfaces within the grid",
        json_schema_extra={"unit": "dimensionless", "display_name": "To Paved"},
        ge=0,
        le=1,
    )
    to_bldgs: Optional[FlexibleRefValue(float)] = Field(
        None,
        description="Fraction of water redistributed to building surfaces within the grid",
        json_schema_extra={"unit": "dimensionless", "display_name": "To Buildings"},
        ge=0,
        le=1,
    )
    to_dectr: Optional[FlexibleRefValue(float)] = Field(
        None,
        description="Fraction of water redistributed to deciduous tree surfaces within the grid",
        json_schema_extra={
            "unit": "dimensionless",
            "display_name": "To Deciduous Trees",
        },
        ge=0,
        le=1,
    )
    to_evetr: Optional[FlexibleRefValue(float)] = Field(
        None,
        description="Fraction of water redistributed to evergreen tree surfaces within the grid",
        json_schema_extra={
            "unit": "dimensionless",
            "display_name": "To Evergreen Trees",
        },
        ge=0,
        le=1,
    )
    to_grass: Optional[FlexibleRefValue(float)] = Field(
        None,
        description="Fraction of water redistributed to grass surfaces within the grid",
        json_schema_extra={"unit": "dimensionless", "display_name": "To Grass"},
        ge=0,
        le=1,
    )
    to_bsoil: Optional[FlexibleRefValue(float)] = Field(
        None,
        description="Fraction of water redistributed to bare soil surfaces within the grid",
        json_schema_extra={"unit": "dimensionless", "display_name": "To Bare Soil"},
        ge=0,
        le=1,
    )
    to_water: Optional[FlexibleRefValue(float)] = Field(
        None,
        description="Fraction of water redistributed to water surfaces within the grid",
        json_schema_extra={"unit": "dimensionless", "display_name": "To Water"},
        ge=0,
        le=1,
    )
    to_runoff: Optional[FlexibleRefValue(float)] = Field(
        None,
        description="Fraction of water going to surface runoff (for impervious surfaces: paved and buildings)",
        json_schema_extra={"unit": "dimensionless", "display_name": "To Runoff"},
        ge=0,
        le=1,
    )  # For paved/bldgs
    to_soilstore: Optional[FlexibleRefValue(float)] = Field(
        None,
        description="Fraction of water going to subsurface soil storage (for pervious surfaces: vegetation and bare soil)",
        json_schema_extra={"unit": "dimensionless", "display_name": "To Soil Store"},
        ge=0,
        le=1,
    )  # For vegetated surfaces
    _surface_type: Optional[SurfaceType] = PrivateAttr(None)

    ref: Optional[Reference] = None

    def __init__(self, surface_type: Optional[SurfaceType] = None, **data):
        # Store surface type as private attribute
        super().__init__(**data)
        self._surface_type = surface_type

        # If surface type is provided, set default values
        if surface_type:
            self._set_defaults(surface_type)
            self.validate_distribution(surface_type)

    def _set_defaults(self, surface_type: SurfaceType):
        # Default distributions based on surface type
        default_distributions = {
            SurfaceType.PAVED: {
                "to_bldgs": RefValue(0.2),
                "to_evetr": RefValue(0.1),
                "to_dectr": RefValue(0.1),
                "to_grass": RefValue(0.1),
                "to_bsoil": RefValue(0.1),
                "to_water": RefValue(0.1),
                "to_runoff": RefValue(0.3),
            },
            SurfaceType.BLDGS: {
                "to_paved": RefValue(0.2),
                "to_evetr": RefValue(0.1),
                "to_dectr": RefValue(0.1),
                "to_grass": RefValue(0.1),
                "to_bsoil": RefValue(0.1),
                "to_water": RefValue(0.1),
                "to_runoff": RefValue(0.3),
            },
            SurfaceType.EVETR: {
                "to_paved": RefValue(0.1),
                "to_bldgs": RefValue(0.1),
                "to_dectr": RefValue(0.1),
                "to_grass": RefValue(0.1),
                "to_bsoil": RefValue(0.1),
                "to_water": RefValue(0.1),
                "to_soilstore": RefValue(0.4),
            },
            SurfaceType.DECTR: {
                "to_paved": RefValue(0.1),
                "to_bldgs": RefValue(0.1),
                "to_evetr": RefValue(0.1),
                "to_grass": RefValue(0.1),
                "to_bsoil": RefValue(0.1),
                "to_water": RefValue(0.1),
                "to_soilstore": RefValue(0.4),
            },
            SurfaceType.GRASS: {
                "to_paved": RefValue(0.1),
                "to_bldgs": RefValue(0.1),
                "to_dectr": RefValue(0.1),
                "to_evetr": RefValue(0.1),
                "to_bsoil": RefValue(0.1),
                "to_water": RefValue(0.1),
                "to_soilstore": RefValue(0.4),
            },
            SurfaceType.BSOIL: {
                "to_paved": RefValue(0.1),
                "to_bldgs": RefValue(0.1),
                "to_dectr": RefValue(0.1),
                "to_evetr": RefValue(0.1),
                "to_grass": RefValue(0.1),
                "to_water": RefValue(0.1),
                "to_soilstore": RefValue(0.4),
            },
        }

        if surface_type in default_distributions:
            defaults = default_distributions[surface_type]
            for key, value in defaults.items():
                if getattr(self, key) is None:
                    setattr(self, key, value)

    def validate_distribution(self, surface_type: SurfaceType) -> None:
        """Validate water distribution based on surface type"""
        # Define required fields for each surface type
        required_fields = {
            SurfaceType.PAVED: [
                "to_bldgs",
                "to_dectr",
                "to_evetr",
                "to_grass",
                "to_bsoil",
                "to_water",
                "to_runoff",
            ],
            SurfaceType.BLDGS: [
                "to_paved",
                "to_dectr",
                "to_evetr",
                "to_grass",
                "to_bsoil",
                "to_water",
                "to_runoff",
            ],
            SurfaceType.DECTR: [
                "to_paved",
                "to_bldgs",
                "to_evetr",
                "to_grass",
                "to_bsoil",
                "to_water",
                "to_soilstore",
            ],
            SurfaceType.EVETR: [
                "to_paved",
                "to_bldgs",
                "to_dectr",
                "to_grass",
                "to_bsoil",
                "to_water",
                "to_soilstore",
            ],
            SurfaceType.GRASS: [
                "to_paved",
                "to_bldgs",
                "to_dectr",
                "to_evetr",
                "to_bsoil",
                "to_water",
                "to_soilstore",
            ],
            SurfaceType.BSOIL: [
                "to_paved",
                "to_bldgs",
                "to_dectr",
                "to_evetr",
                "to_grass",
                "to_water",
                "to_soilstore",
            ],
            SurfaceType.WATER: None,  # Water surface doesn't have water distribution
        }

        if surface_type == SurfaceType.WATER:
            raise ValueError("Water surface should not have water distribution")

        fields = required_fields[surface_type]
        values = []

        # Check required fields are present and collect values
        for field in fields:
            value = getattr(self, field)
            if value is None:
                raise ValueError(
                    f"Missing required field {field} for {surface_type.value}"
                )
            values.append(value)

        # Validate sum
        total = sum(
            value.value if isinstance(value, RefValue) else value for value in values
        )
        # if not np.isclose(total, 1.0, rtol=1e-5):
        if not math.isclose(total, 1.0, rel_tol=1e-5):
            raise ValueError(f"Water distribution sum must be 1.0, got {total}")

    def to_df_state(self, grid_id: int, surf_idx: int) -> pd.DataFrame:
        """Convert water distribution parameters to DataFrame state format.

        Args:
            grid_id: Grid ID for the DataFrame index
            surf_idx: Surface index (0=paved, 1=bldgs, 2=dectr, 3=evetr, 4=grass, 5=bsoil, 6=water)

        Returns:
            pd.DataFrame: DataFrame containing water distribution parameters with MultiIndex columns
        """
        # Create tuples for MultiIndex columns
        param_tuples = []
        values = []

        # Add all non-None distribution parameters using a two-step process
        # 1. Collect all non-None values
        list_waterdist_value = []
        for i, attr in enumerate([
            "to_paved",
            "to_bldgs",
            "to_evetr",
            "to_dectr",
            "to_grass",
            "to_bsoil",
            "to_water",
            # "to_soilstore",
            # "to_runoff",
        ]):
            value = getattr(self, attr)
            if value is None:
                list_waterdist_value.append(0.0)
            else:
                list_waterdist_value.append(value)

        # either to_soilstore or to_runoff must be provided - the other must be 0
        to_soilstore_or_runoff = (
            self.to_runoff if self.to_soilstore is None else self.to_soilstore
        )
        list_waterdist_value.append(to_soilstore_or_runoff)

        # 2. Create param_tuples and values - only add non-None values following the order of the list
        for i, value in enumerate(list_waterdist_value):
            if value is not None:
                param_tuples.append(("waterdist", f"({i}, {surf_idx})"))
                values.append(value)

        # Create MultiIndex columns
        columns = pd.MultiIndex.from_tuples(param_tuples, names=["var", "ind_dim"])

        # Convert RefValue to float
        values = [
            value.value if isinstance(value, RefValue) else value for value in values
        ]

        # Create DataFrame with single row
        df = pd.DataFrame(
            index=pd.Index([grid_id], name="grid"),
            columns=columns,
            data=[values],
            dtype=float,
        )

        return df

    @classmethod
    def from_df_state(
        cls, df: pd.DataFrame, grid_id: int, surf_idx: int
    ) -> "WaterDistribution":
        """
        Reconstruct WaterDistribution from a DataFrame state format.

        Args:
            df (pd.DataFrame): DataFrame containing water distribution parameters.
            grid_id (int): Grid ID for the DataFrame index.
            surf_idx (int): Surface index for identifying columns.

        Returns:
            WaterDistribution: Instance of WaterDistribution.
        """
        dict_surface_type = {
            0: SurfaceType.PAVED,
            1: SurfaceType.BLDGS,
            2: SurfaceType.EVETR,
            3: SurfaceType.DECTR,
            4: SurfaceType.GRASS,
            5: SurfaceType.BSOIL,
            6: SurfaceType.WATER,
        }
        surface_type = dict_surface_type[surf_idx]
        # initialize an instance of this class
        instance = cls(surface_type=surface_type)

        # Define the parameter names and their indices
        param_map = {
            "to_paved": 0,
            "to_bldgs": 1,
            "to_evetr": 2,
            "to_dectr": 3,
            "to_grass": 4,
            "to_bsoil": 5,
            "to_water": 6,
            # "to_soilstore": 7,
            # "to_runoff": 8,
        }

        # Extract the values from the DataFrame
        params = {
            param: df.loc[grid_id, ("waterdist", f"({idx}, {surf_idx})")]
            for param, idx in param_map.items()
        }
        for param, value in params.items():
            value = RefValue(value)
            if getattr(instance, param) is not None:
                setattr(instance, param, value)

        # set the last to_soilstore or to_runoff
        waterdist_last = df.loc[grid_id, ("waterdist", f"(7, {surf_idx})")]
        waterdist_last = RefValue(waterdist_last)
        if getattr(instance, "to_soilstore") is None:
            setattr(instance, "to_runoff", waterdist_last)
        else:
            setattr(instance, "to_soilstore", waterdist_last)

        return instance


class StorageDrainParams(BaseModel):
    store_min: FlexibleRefValue(float) = Field(
        ge=0,
        default=0.0,
        description="Minimum water storage capacity",
        json_schema_extra={"unit": "mm", "display_name": "Minimum Storage"},
    )
    store_max: Optional[FlexibleRefValue(float)] = Field(
        ge=0,
        default=None,
        description="Maximum water storage capacity",
        json_schema_extra={"unit": "mm", "display_name": "Maximum Storage"},
    )
    store_cap: Optional[FlexibleRefValue(float)] = Field(
        ge=0,
        default=None,
        description="Current water storage capacity - the actual storage capacity available for surface water retention. This represents the depth of water that can be stored on or in the surface before drainage begins. For paved surfaces, this might represent depression storage; for vegetated surfaces, it includes canopy interception storage.",
        json_schema_extra={"unit": "mm", "display_name": "Storage Capacity"},
    )
    drain_eq: FlexibleRefValue(int) = Field(
        default=0,
        description="Drainage equation selection (0: linear, 1: exponential)",
        json_schema_extra={
            "unit": "dimensionless",
            "display_name": "Drainage Equation",
        },
    )
    drain_coef_1: Optional[FlexibleRefValue(float)] = Field(
        default=None,
        description="Drainage coefficient 1 (rate parameter)",
        json_schema_extra={"unit": "mm h^-1", "display_name": "Drainage Coefficient 1"},
    )
    drain_coef_2: Optional[FlexibleRefValue(float)] = Field(
        default=None,
        description="Drainage coefficient 2 (shape parameter)",
        json_schema_extra={
            "unit": "dimensionless",
            "display_name": "Drainage Coefficient 2",
        },
    )

    ref: Optional[Reference] = None

    def to_df_state(self, grid_id: int, surf_idx: int) -> pd.DataFrame:
        """Convert storage and drain parameters to DataFrame state format.

        Args:
            grid_id: Grid ID for the DataFrame index
            surf_idx: Surface index (0=paved, 1=bldgs, 2=dectr, 3=evetr, 4=grass, 5=bsoil, 6=water)

        Returns:
            pd.DataFrame: DataFrame containing storage and drain parameters with MultiIndex columns
        """
        # Create tuples for MultiIndex columns
        param_tuples = [
            ("storedrainprm", f"({i}, {surf_idx})")
            for i, _ in enumerate([
                "store_min",
                "drain_eq",
                "drain_coef_1",
                "drain_coef_2",
                "store_max",
                "store_cap",
            ])
        ]

        # Create MultiIndex columns
        columns = pd.MultiIndex.from_tuples(param_tuples, names=["var", "ind_dim"])

        # Create DataFrame with single row
        df = pd.DataFrame(
            index=pd.Index([grid_id], name="grid"), columns=columns, dtype=float
        )

        # Fill values
        for i, var in enumerate([
            "store_min",
            "drain_eq",
            "drain_coef_1",
            "drain_coef_2",
            "store_max",
            "store_cap",
        ]):
            field_val = getattr(self, var)
            if field_val is not None:
                val = field_val.value if isinstance(field_val, RefValue) else field_val
            else:
                val = 0.0  # Default to 0.0 for DataFrame compatibility
            df.loc[grid_id, ("storedrainprm", f"({i}, {surf_idx})")] = val

        return df

    @classmethod
    def from_df_state(
        cls, df: pd.DataFrame, grid_id: int, surf_idx: int
    ) -> "StorageDrainParams":
        """
        Reconstruct StorageDrainParams from DataFrame state format.

        Args:
            df: DataFrame containing storage and drain parameters.
            grid_id: Grid ID for the DataFrame index.
            surf_idx: Surface index (0=paved, 1=bldgs, 2=dectr, 3=evetr, 4=grass, 5=bsoil, 6=water).

        Returns:
            StorageDrainParams: Instance of StorageDrainParams.
        """
        # Define the parameter names and their indices
        param_map = {
            "store_min": 0,
            "drain_eq": 1,
            "drain_coef_1": 2,
            "drain_coef_2": 3,
            "store_max": 4,
            "store_cap": 5,
        }

        # Extract the values from the DataFrame
        params = {
            param: df.loc[grid_id, ("storedrainprm", f"({idx}, {surf_idx})")]
            for param, idx in param_map.items()
        }

        # Conver params to RefValue
        params = {key: RefValue(value) for key, value in params.items()}

        # Create an instance using the extracted parameters
        return cls(**params)
