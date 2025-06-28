from typing import Optional, Union, List, Literal, Type
import pandas as pd
from pydantic import (
    ConfigDict,
    BaseModel,
    Field,
    field_validator,
    model_validator,
    PrivateAttr,
)

from .type import RefValue, Reference, FlexibleRefValue, init_df_state, SurfaceType


class SurfaceInitialState(BaseModel):
    """Base initial state parameters for all surface types"""

    state: FlexibleRefValue(float) = Field(
        description="Initial water state of the surface",
        json_schema_extra={"unit": "mm", "display_name": "State"},
        default=0.0,
        ge=0,
    )  # Default set to 0.0 means dry surface.
    soilstore: FlexibleRefValue(float) = Field(
        description="Initial soil store (essential for QE)",
        json_schema_extra={"unit": "mm", "display_name": "Soilstore"},
        default=150.0,
        ge=10,
    )  # Default set to 150.0 (wet soil) and ge=10 (less than 10 would be too dry) are physically reasonable for a model run.
    snowfrac: Optional[Union[FlexibleRefValue(float), None]] = Field(
        description="Snow fraction",
        json_schema_extra={"unit": "dimensionless", "display_name": "Snow Fraction"},
        default=0.0,
        ge=0,
        le=1,
    )  # Default set to 0.0 means no snow on the ground.
    snowpack: Optional[Union[FlexibleRefValue(float), None]] = Field(
        description="Snow pack",
        json_schema_extra={"unit": "mm", "display_name": "Snow Pack"},
        default=0.0,
        ge=0,
    )
    icefrac: Optional[Union[FlexibleRefValue(float), None]] = Field(
        description="Ice fraction",
        json_schema_extra={"unit": "dimensionless", "display_name": "Ice Fraction"},
        default=0.0,
        ge=0,
        le=1,
    )
    snowwater: Optional[Union[FlexibleRefValue(float), None]] = Field(
        description="Snow water",
        json_schema_extra={"unit": "mm", "display_name": "Snow Water"},
        default=0.0,
        ge=0,
    )
    snowdens: Optional[Union[FlexibleRefValue(float), None]] = Field(
        description="Snow density",
        json_schema_extra={"unit": "kg m^-3", "display_name": "Snow Density"},
        default=0.0,
        ge=0,
    )
    temperature: FlexibleRefValue(List[float]) = Field(
        description="Initial temperature for each thermal layer",
        json_schema_extra={"unit": "degC", "display_name": "Temperature"},
        default=[15.0, 15.0, 15.0, 15.0, 15.0],
    )  # We need to check/undestand what model are these temperatures related to. ESTM? What surface type (wall and roof) of building?
    tsfc: Optional[Union[FlexibleRefValue(float), None]] = Field(
        description="Initial exterior surface temperature",
        json_schema_extra={"unit": "degC", "display_name": "Surface Temperature"},
        default=15.0,
    )
    tin: Optional[Union[FlexibleRefValue(float), None]] = Field(
        description="Initial interior surface temperature",
        json_schema_extra={"unit": "degC", "display_name": "Interior Temperature"},
        default=20.0,
    )  # We need to know which model is using this.
    _surface_type: Optional[SurfaceType] = PrivateAttr(default=None)

    ref: Optional[Reference] = None

    @field_validator("temperature", mode="before")
    def validate_temperature(cls, v):
        if isinstance(v, dict):
            value = v["value"]
        else:
            value = v.value if isinstance(v, RefValue) else v
        if len(value) != 5:
            raise ValueError("temperature must have exactly 5 items")
        return v

    def set_surface_type(self, surface_type: SurfaceType):
        """Set surface type"""
        self._surface_type = surface_type

    def get_surface_index(self) -> int:
        """Get surface index"""
        return {
            SurfaceType.PAVED: 0,
            SurfaceType.BLDGS: 1,
            SurfaceType.EVETR: 2,
            SurfaceType.DECTR: 3,
            SurfaceType.GRASS: 4,
            SurfaceType.BSOIL: 5,
            SurfaceType.WATER: 6,
        }[self._surface_type]

    def to_df_state(
        self, grid_id: int, vert_idx: int = None, is_roof: bool = False
    ) -> pd.DataFrame:
        """Convert base surface initial state to DataFrame state format.

        Args:
            grid_id: Grid ID for the DataFrame index

        Returns:
            pd.DataFrame: DataFrame containing initial state parameters
        """
        df_state = init_df_state(grid_id)

        # Get surface index
        if vert_idx is None:
            idx = self.get_surface_index()
            str_type = "surf"
        else:
            idx = vert_idx
            str_type = "roof" if is_roof else "wall"
        # Set basic state parameters
        df_state[(f"state_{str_type}", f"({idx},)")] = (
            self.state.value if isinstance(self.state, RefValue) else self.state
        )
        df_state[(f"soilstore_{str_type}", f"({idx},)")] = (
            self.soilstore.value
            if isinstance(self.soilstore, RefValue)
            else self.soilstore
        )

        # Set snow/ice parameters if present
        if self.snowfrac is not None:
            df_state[(f"snowfrac", f"({idx},)")] = (
                self.snowfrac.value
                if isinstance(self.snowfrac, RefValue)
                else self.snowfrac
            )
        if self.snowpack is not None:
            df_state[(f"snowpack", f"({idx},)")] = (
                self.snowpack.value
                if isinstance(self.snowpack, RefValue)
                else self.snowpack
            )
        if self.icefrac is not None:
            df_state[(f"icefrac", f"({idx},)")] = (
                self.icefrac.value
                if isinstance(self.icefrac, RefValue)
                else self.icefrac
            )
        if self.snowwater is not None:
            df_state[(f"snowwater", f"({idx},)")] = (
                self.snowwater.value
                if isinstance(self.snowwater, RefValue)
                else self.snowwater
            )
        if self.snowdens is not None:
            df_state[(f"snowdens", f"({idx},)")] = (
                self.snowdens.value
                if isinstance(self.snowdens, RefValue)
                else self.snowdens
            )

        # Set temperature parameters
        temp_values = (
            self.temperature.value
            if isinstance(self.temperature, RefValue)
            else self.temperature
        )
        for i, temp in enumerate(temp_values):
            df_state[(f"temp_{str_type}", f"({idx}, {i})")] = temp

        if self.tsfc is not None:
            df_state[(f"tsfc_{str_type}", f"({idx},)")] = (
                self.tsfc.value if isinstance(self.tsfc, RefValue) else self.tsfc
            )
        if self.tin is not None:
            df_state[(f"tin_{str_type}", f"({idx},)")] = (
                self.tin.value if isinstance(self.tin, RefValue) else self.tin
            )

        return df_state

    @classmethod
    def from_df_state(
        cls, df: pd.DataFrame, grid_id: int, surf_idx: int, str_type: str = "surf"
    ) -> "SurfaceInitialState":
        """
        Reconstruct SurfaceInitialState from a DataFrame state format.

        Args:
            df (pd.DataFrame): DataFrame containing surface state parameters.
            grid_id (int): Grid ID for the DataFrame index.
            surf_idx (int): Surface index for identifying columns.
            str_type (str): Surface type prefix ("surf", "roof", or "wall").

        Returns:
            SurfaceInitialState: Instance of SurfaceInitialState.
        """
        # Base surface state parameters
        state = RefValue(df.loc[grid_id, (f"state_{str_type}", f"({surf_idx},)")])
        soilstore = RefValue(
            df.loc[grid_id, (f"soilstore_{str_type}", f"({surf_idx},)")]
        )

        # Snow/ice parameters
        if str_type not in ["roof", "wall"]:
            snowfrac = RefValue(df.loc[grid_id, (f"snowfrac", f"({surf_idx},)")])
            snowpack = RefValue(df.loc[grid_id, (f"snowpack", f"({surf_idx},)")])
            icefrac = RefValue(df.loc[grid_id, (f"icefrac", f"({surf_idx},)")])
            snowwater = RefValue(df.loc[grid_id, (f"snowwater", f"({surf_idx},)")])
            snowdens = RefValue(df.loc[grid_id, (f"snowdens", f"({surf_idx},)")])
        else:
            snowfrac = None
            snowpack = None
            icefrac = None
            snowwater = None
            snowdens = None

        # Temperature parameters
        temperature = RefValue([
            df.loc[grid_id, (f"temp_{str_type}", f"({surf_idx}, {i})")]
            for i in range(5)
        ])

        # Exterior and interior surface temperature
        tsfc = RefValue(df.loc[grid_id, (f"tsfc_{str_type}", f"({surf_idx},)")])
        tin = RefValue(df.loc[grid_id, (f"tin_{str_type}", f"({surf_idx},)")])

        return cls(
            state=state,
            soilstore=soilstore,
            snowfrac=snowfrac,
            snowpack=snowpack,
            icefrac=icefrac,
            snowwater=snowwater,
            snowdens=snowdens,
            temperature=temperature,
            tsfc=tsfc,
            tin=tin,
        )


class WaterUse(BaseModel):
    wu_total: FlexibleRefValue(float) = Field(
        description="Total water use",
        json_schema_extra={"unit": "mm", "display_name": "Wu Total"},
        default=0.0,
        ge=0,
    )  # Default set to 0.0 means no irrigation.
    wu_auto: FlexibleRefValue(float) = Field(
        description="Automatic water use",
        json_schema_extra={"unit": "mm", "display_name": "Wu Auto"},
        default=0.0,
        ge=0,
    )
    wu_manual: FlexibleRefValue(float) = Field(
        description="Manual water use",
        json_schema_extra={"unit": "mm", "display_name": "Wu Manual"},
        default=0.0,
        ge=0,
    )

    ref: Optional[Reference] = None

    def to_df_state(self, veg_idx: int, grid_id: int) -> pd.DataFrame:
        """Convert water use to DataFrame state format."""
        df_state = init_df_state(grid_id)
        df_state.loc[grid_id, ("wuday_id", f"({veg_idx * 3 + 0},)")] = (
            self.wu_total.value
            if isinstance(self.wu_total, RefValue)
            else self.wu_total
        )
        df_state.loc[grid_id, ("wuday_id", f"({veg_idx * 3 + 1},)")] = (
            self.wu_auto.value if isinstance(self.wu_auto, RefValue) else self.wu_auto
        )
        df_state.loc[grid_id, ("wuday_id", f"({veg_idx * 3 + 2},)")] = (
            self.wu_manual.value
            if isinstance(self.wu_manual, RefValue)
            else self.wu_manual
        )
        return df_state

    @classmethod
    def from_df_state(cls, df: pd.DataFrame, veg_idx: int, grid_id: int) -> "WaterUse":
        """
        Reconstruct WaterUse from a DataFrame state format.

        Args:
            df (pd.DataFrame): DataFrame containing water use parameters.
            veg_idx (int): Vegetation index for identifying columns.
            grid_id (int): Grid ID for the DataFrame index.

        Returns:
            WaterUse: Instance of WaterUse.
        """
        wu_total = df.loc[grid_id, ("wuday_id", f"({veg_idx * 3 + 0},)")].item()
        wu_auto = df.loc[grid_id, ("wuday_id", f"({veg_idx * 3 + 1},)")].item()
        wu_manual = df.loc[grid_id, ("wuday_id", f"({veg_idx * 3 + 2},)")].item()

        return cls(
            wu_total=RefValue(wu_total),
            wu_auto=RefValue(wu_auto),
            wu_manual=RefValue(wu_manual),
        )


class InitialStatePaved(SurfaceInitialState):
    _surface_type: Literal[SurfaceType.PAVED] = SurfaceType.PAVED


class InitialStateBldgs(SurfaceInitialState):
    _surface_type: Literal[SurfaceType.BLDGS] = SurfaceType.BLDGS


class InitialStateVeg(SurfaceInitialState):
    """Base initial state parameters for vegetated surfaces"""

    alb_id: FlexibleRefValue(float) = Field(
        description="Albedo at the start of the model run.",
        json_schema_extra={"unit": "dimensionless", "display_name": "Alb Id"},
        default=0.25,
    )
    lai_id: FlexibleRefValue(float) = Field(
        description="Leaf area index at the start of the model run.",
        json_schema_extra={"unit": "m^2 m^-2", "display_name": "Lai Id"},
        default=1.0,
    )
    gdd_id: FlexibleRefValue(float) = Field(
        description="Growing degree days at the start of the model run",
        json_schema_extra={"unit": "degC d", "display_name": "Gdd Id"},
        default=0,
    )  # We need to check this and give info for setting values.
    sdd_id: FlexibleRefValue(float) = Field(
        description="Senescence degree days at the start of the model run",
        json_schema_extra={"unit": "degC d", "display_name": "Sdd Id"},
        default=0,
    )  # This need to be consistent with GDD.
    wu: WaterUse = Field(
        default_factory=WaterUse, json_schema_extra={"display_name": "Water Use"}
    )

    ref: Optional[Reference] = None

    @model_validator(mode="after")
    def validate_surface_state(self) -> "InitialStateVeg":
        """Validate state based on surface type"""
        # Skip validation if surface type not yet set
        if not hasattr(self, "_surface_type") or self._surface_type is None:
            return self

        if self._surface_type not in [
            SurfaceType.DECTR,
            SurfaceType.EVETR,
            SurfaceType.GRASS,
        ]:
            raise ValueError(
                f"Invalid surface type {self._surface_type} for vegetated surface"
            )
        return self

    def to_df_state(self, grid_id: int) -> pd.DataFrame:
        """Convert vegetated surface initial state to DataFrame state format.

        Args:
            grid_id: Grid ID for the DataFrame index

        Returns:
            pd.DataFrame: DataFrame containing initial state parameters
        """
        # Get base surface state parameters
        df_state = super().to_df_state(grid_id)

        # Get surface index
        surf_idx = self.get_surface_index()
        veg_idx = surf_idx - 2

        # Add vegetated surface specific parameters
        # alb is universal so use surf_idx
        df_state[("alb", f"({surf_idx},)")] = (
            self.alb_id.value if isinstance(self.alb_id, RefValue) else self.alb_id
        )
        # others are aligned with veg_idx
        df_state[("lai_id", f"({veg_idx},)")] = (
            self.lai_id.value if isinstance(self.lai_id, RefValue) else self.lai_id
        )
        df_state[("gdd_id", f"({veg_idx},)")] = (
            self.gdd_id.value if isinstance(self.gdd_id, RefValue) else self.gdd_id
        )
        df_state[("sdd_id", f"({veg_idx},)")] = (
            self.sdd_id.value if isinstance(self.sdd_id, RefValue) else self.sdd_id
        )

        # Add water use parameters
        df_wu = self.wu.to_df_state(veg_idx, grid_id)
        df_state = pd.concat([df_state, df_wu], axis=1)

        # Drop any duplicate columns
        df_state = df_state.loc[:, ~df_state.columns.duplicated()]

        return df_state

    @classmethod
    def from_df_state(
        cls, df: pd.DataFrame, grid_id: int, surf_idx: int
    ) -> "InitialStateVeg":
        """
        Reconstruct VegetatedSurfaceInitialState from a DataFrame state format."""
        base_instance = SurfaceInitialState.from_df_state(df, grid_id, surf_idx)

        # Vegetated surface-specific parameters
        alb_key = ("alb", f"({surf_idx},)")
        lai_key = ("lai_id", f"({surf_idx - 2},)")
        gdd_key = ("gdd_id", f"({surf_idx - 2},)")
        sdd_key = ("sdd_id", f"({surf_idx - 2},)")

        alb_id = df.loc[grid_id, alb_key]
        lai_id = df.loc[grid_id, lai_key]
        gdd_id = df.loc[grid_id, gdd_key]
        sdd_id = df.loc[grid_id, sdd_key]

        # Convert to RefValue
        alb_id = RefValue(alb_id)
        lai_id = RefValue(lai_id)
        gdd_id = RefValue(gdd_id)
        sdd_id = RefValue(sdd_id)

        # Reconstruct WaterUse instance
        veg_idx = surf_idx - 2
        wu = WaterUse.from_df_state(df, veg_idx, grid_id)

        return cls(
            **base_instance.model_dump(),
            alb_id=alb_id,
            lai_id=lai_id,
            gdd_id=gdd_id,
            sdd_id=sdd_id,
            wu=wu,
        )


class InitialStateEvetr(InitialStateVeg):
    _surface_type: Literal[SurfaceType.EVETR] = SurfaceType.EVETR

    def to_df_state(self, grid_id: int) -> pd.DataFrame:
        """Convert evergreen tree initial state to DataFrame state format."""
        df_state = super().to_df_state(grid_id)
        df_state[("albevetr_id", "0")] = (
            self.alb_id.value if isinstance(self.alb_id, RefValue) else self.alb_id
        )
        return df_state

    @classmethod
    def from_df_state(
        cls, df: pd.DataFrame, grid_id: int, surf_idx: int
    ) -> "InitialStateEvetr":
        """
        Reconstruct InitialStateEvetr from a DataFrame state format.

        Args:
            df (pd.DataFrame): DataFrame containing EVETR state parameters.
            grid_id (int): Grid ID for the DataFrame index.
            surf_idx (int): Surface index for identifying columns.

        Returns:
            InitialStateEvetr: Instance of InitialStateEvetr.
        """
        # Call the parent class to extract common fields
        base_instance = super().from_df_state(df, grid_id, surf_idx)

        # Extract the EVETR-specific field
        alb_id = df.loc[grid_id, ("albevetr_id", "0")].item()

        # Use `base_instance.dict()` to pass the existing attributes, excluding `alb_id` to avoid duplication
        base_instance_dict = base_instance.model_dump()
        base_instance_dict["alb_id"] = {"value": alb_id}  # Update alb_id explicitly

        # Return a new instance with the updated dictionary
        return cls(**base_instance_dict)


class InitialStateDectr(InitialStateVeg):
    """Initial state parameters for deciduous trees"""

    porosity_id: FlexibleRefValue(float) = Field(
        description="Porosity for deciduous trees at the start of the model run",
        json_schema_extra={"unit": "dimensionless", "display_name": "Porosity Id"},
        default=0.2,
        ge=0,
        le=1,
    )
    decidcap_id: FlexibleRefValue(float) = Field(
        description="Deciduous capacity for deciduous trees at the start of the model run",
        json_schema_extra={"unit": "mm", "display_name": "Decidcap Id"},
        default=0.3,
        ge=0,
    )
    _surface_type: Literal[SurfaceType.DECTR] = SurfaceType.DECTR

    ref: Optional[Reference] = None

    @model_validator(mode="after")
    def validate_surface_state(self) -> "InitialStateDectr":
        """Validate state based on surface type"""
        # Skip validation if surface type not yet set
        if not hasattr(self, "_surface_type") or self._surface_type is None:
            return self

        if self._surface_type != SurfaceType.DECTR:
            raise ValueError("This state is only valid for deciduous trees")
        return self

    def to_df_state(self, grid_id: int) -> pd.DataFrame:
        """Convert deciduous tree initial state to DataFrame state format.

        Args:
            grid_id: Grid ID for the DataFrame index

        Returns:
            pd.DataFrame: DataFrame containing initial state parameters
        """
        # Get base vegetated surface state parameters
        df_state = super().to_df_state(grid_id)

        # Add deciduous tree specific parameters
        df_state[("porosity_id", "0")] = (
            self.porosity_id.value
            if isinstance(self.porosity_id, RefValue)
            else self.porosity_id
        )
        df_state[("decidcap_id", "0")] = (
            self.decidcap_id.value
            if isinstance(self.decidcap_id, RefValue)
            else self.decidcap_id
        )
        df_state[("albdectr_id", "0")] = (
            self.alb_id.value if isinstance(self.alb_id, RefValue) else self.alb_id
        )

        return df_state

    @classmethod
    def from_df_state(
        cls, df: pd.DataFrame, grid_id: int, surf_idx: int
    ) -> "InitialStateDectr":
        """
        Reconstruct DeciduousTreeSurfaceInitialState from a DataFrame state format.

        Args:
            df (pd.DataFrame): DataFrame containing deciduous tree state parameters.
            grid_id (int): Grid ID for the DataFrame index.
            surf_idx (int): Surface index for identifying columns.

        Returns:
            DeciduousTreeSurfaceInitialState: Instance of DeciduousTreeSurfaceInitialState.
        """
        # Base class reconstruction
        base_instance = InitialStateVeg.from_df_state(df, grid_id, surf_idx)

        # Deciduous tree-specific parameters
        porosity_id = df.loc[grid_id, ("porosity_id", "0")]
        decidcap_id = df.loc[grid_id, ("decidcap_id", "0")]
        alb_id = df.loc[grid_id, ("albdectr_id", "0")]

        # Convert to RefValue
        porosity_id = RefValue(porosity_id)
        decidcap_id = RefValue(decidcap_id)
        base_instance_dict = base_instance.model_dump()
        base_instance_dict["alb_id"] = {"value": alb_id}  # Update alb_id explicitly

        return cls(
            **base_instance_dict,
            porosity_id=porosity_id,
            decidcap_id=decidcap_id,
        )


class InitialStateGrass(InitialStateVeg):
    _surface_type: Literal[SurfaceType.GRASS] = SurfaceType.GRASS

    def to_df_state(self, grid_id: int) -> pd.DataFrame:
        """Convert grass initial state to DataFrame state format."""
        df_state = super().to_df_state(grid_id)
        df_state[("albgrass_id", "0")] = (
            self.alb_id.value if isinstance(self.alb_id, RefValue) else self.alb_id
        )
        return df_state

    @classmethod
    def from_df_state(
        cls, df: pd.DataFrame, grid_id: int, surf_idx: int
    ) -> "InitialStateGrass":
        """
        Reconstruct InitialStateGrass from a DataFrame state format.

        Args:
            df (pd.DataFrame): DataFrame containing grass state parameters.
            grid_id (int): Grid ID for the DataFrame index.
            surf_idx (int): Surface index for identifying columns.

        Returns:
            InitialStateGrass: Instance of InitialStateGrass.
        """
        # Call the parent class to extract common fields
        base_instance = super().from_df_state(df, grid_id, surf_idx)

        # Extract the GRASS-specific field
        alb_id = df.loc[grid_id, ("albgrass_id", "0")].item()

        # Use `base_instance.dict()` to pass the existing attributes, excluding `alb_id` to avoid duplication
        base_instance_dict = base_instance.model_dump()
        base_instance_dict["alb_id"] = {"value": alb_id}  # Update alb_id explicitly

        # Return a new instance with the updated dictionary
        return cls(**base_instance_dict)


class InitialStateBsoil(SurfaceInitialState):
    _surface_type: Literal[SurfaceType.BSOIL] = SurfaceType.BSOIL


class InitialStateWater(SurfaceInitialState):
    _surface_type: Literal[SurfaceType.WATER] = SurfaceType.WATER


class HDD_ID(BaseModel):
    """Heating Degree Days and related meteorological tracking parameters.

    This structure maintains both current day accumulations (fields 1-6) and
    previous day values (fields 7-12) for various meteorological parameters
    used in anthropogenic heat and water use calculations.
    """

    # Current day accumulations (updated throughout the day)
    hdd_accum: float = Field(
        default=0.0,
        description="Current day's heating degree days accumulation [degC]",
        json_schema_extra={
            "unit": "degC",
            "display_name": "Hdd Accum",
            "internal_only": True,
        },
    )
    cdd_accum: float = Field(
        default=0.0,
        description="Current day's cooling degree days accumulation [degC]",
        json_schema_extra={
            "unit": "degC",
            "display_name": "Cdd Accum",
            "internal_only": True,
        },
    )
    temp_accum: float = Field(
        default=0.0,
        description="Current day's temperature accumulation for daily mean [degC]",
        json_schema_extra={
            "unit": "degC",
            "display_name": "Temp Accum",
            "internal_only": True,
        },
    )
    temp_5day_accum: float = Field(
        default=0.0,
        description="5-day running mean temperature accumulation [degC]",
        json_schema_extra={
            "unit": "degC",
            "display_name": "Temp 5Day Accum",
            "internal_only": True,
        },
    )
    precip_accum: float = Field(
        default=0.0,
        description="Current day's precipitation total [mm]",
        json_schema_extra={
            "unit": "mm",
            "display_name": "Precip Accum",
            "internal_only": True,
        },
    )
    days_since_rain_accum: float = Field(
        default=0.0,
        description="Days since rain counter (current) [days]",
        json_schema_extra={
            "unit": "days",
            "display_name": "Days Since Rain Accum",
            "internal_only": True,
        },
    )

    # Previous day values (used in calculations)
    hdd_daily: float = Field(
        default=0.0,
        description="Previous day's heating degree days for QF calculations [degC]",
        json_schema_extra={
            "unit": "degC",
            "display_name": "Hdd Daily",
            "internal_only": True,
        },
    )
    cdd_daily: float = Field(
        default=0.0,
        description="Previous day's cooling degree days for QF calculations [degC]",
        json_schema_extra={
            "unit": "degC",
            "display_name": "Cdd Daily",
            "internal_only": True,
        },
    )
    temp_daily_mean: float = Field(
        default=0.0,
        description="Previous day's mean temperature for water use calculations [degC]",
        json_schema_extra={
            "unit": "degC",
            "display_name": "Temp Daily Mean",
            "internal_only": True,
        },
    )
    temp_5day_mean: float = Field(
        default=0.0,
        description="Previous 5-day running mean temperature for QF calculations [degC]",
        json_schema_extra={
            "unit": "degC",
            "display_name": "Temp 5Day Mean",
            "internal_only": True,
        },
    )
    precip_daily_total: float = Field(
        default=0.0,
        description="Previous day's precipitation total [mm]",
        json_schema_extra={
            "unit": "mm",
            "display_name": "Precip Daily Total",
            "internal_only": True,
        },
    )
    days_since_rain: float = Field(
        default=0.0,
        description="Days since rain for irrigation calculations [days]",
        json_schema_extra={
            "unit": "days",
            "display_name": "Days Since Rain",
            "internal_only": True,
        },
    )

    def to_list(self) -> List[float]:
        """Convert to list format for legacy compatibility."""
        return [
            self.hdd_accum,
            self.cdd_accum,
            self.temp_accum,
            self.temp_5day_accum,
            self.precip_accum,
            self.days_since_rain_accum,
            self.hdd_daily,
            self.cdd_daily,
            self.temp_daily_mean,
            self.temp_5day_mean,
            self.precip_daily_total,
            self.days_since_rain,
        ]

    @classmethod
    def from_list(cls, values: List[float]) -> "HDD_ID":
        """Create from list format for legacy compatibility."""
        if len(values) != 12:
            raise ValueError(f"Expected 12 values for HDD_ID, got {len(values)}")
        return cls(
            hdd_accum=values[0],
            cdd_accum=values[1],
            temp_accum=values[2],
            temp_5day_accum=values[3],
            precip_accum=values[4],
            days_since_rain_accum=values[5],
            hdd_daily=values[6],
            cdd_daily=values[7],
            temp_daily_mean=values[8],
            temp_5day_mean=values[9],
            precip_daily_total=values[10],
            days_since_rain=values[11],
        )


class InitialStates(BaseModel):
    """Initial conditions for the SUEWS model"""

    snowalb: FlexibleRefValue(float) = Field(
        description="Snow albedo at the start of the model run",
        json_schema_extra={"unit": "dimensionless", "display_name": "Snow Albedo"},
        default=0.5,
        ge=0,
        le=1,
    )
    paved: InitialStatePaved = Field(
        default_factory=InitialStatePaved, json_schema_extra={"display_name": "Paved"}
    )
    bldgs: InitialStateBldgs = Field(
        default_factory=InitialStateBldgs,
        json_schema_extra={"display_name": "Buildings"},
    )
    evetr: InitialStateEvetr = Field(
        default_factory=InitialStateEvetr,
        json_schema_extra={"display_name": "Evergreen Trees"},
    )
    dectr: InitialStateDectr = Field(
        default_factory=InitialStateDectr,
        json_schema_extra={"display_name": "Deciduous Trees"},
    )
    grass: InitialStateGrass = Field(
        default_factory=InitialStateGrass, json_schema_extra={"display_name": "Grass"}
    )
    bsoil: InitialStateBsoil = Field(
        default_factory=InitialStateBsoil,
        json_schema_extra={"display_name": "Bare Soil"},
    )
    water: InitialStateWater = Field(
        default_factory=InitialStateWater, json_schema_extra={"display_name": "Water"}
    )
    roofs: Optional[List[SurfaceInitialState]] = Field(
        default=[
            SurfaceInitialState(),
            SurfaceInitialState(),
            SurfaceInitialState(),
        ],
        description="Initial states for roof layers",
        json_schema_extra={"display_name": "Roofs"},
    )
    walls: Optional[List[SurfaceInitialState]] = Field(
        default=[
            SurfaceInitialState(),
            SurfaceInitialState(),
            SurfaceInitialState(),
        ],
        description="Initial states for wall layers",
        json_schema_extra={"display_name": "Walls"},
    )

    dqndt: float = Field(
        default=0,
        description="Change in net radiation",
        json_schema_extra={"display_name": "dQn/dt", "internal_only": True},
    )
    dqnsdt: float = Field(
        default=0,
        description="Change in net shortwave radiation",
        json_schema_extra={"display_name": "dQns/dt", "internal_only": True},
    )
    dt_since_start: float = Field(
        default=0,
        description="Time since start",
        json_schema_extra={"display_name": "Time Since Start", "internal_only": True},
    )
    lenday_id: int = Field(
        default=0,
        description="Length of the day ID",
        json_schema_extra={"display_name": "Day Length ID", "internal_only": True},
    )
    qn_av: float = Field(
        default=0,
        description="Average net radiation",
        json_schema_extra={
            "display_name": "Average Net Radiation",
            "internal_only": True,
        },
    )
    qn_s_av: float = Field(
        default=0,
        description="Average net shortwave radiation",
        json_schema_extra={
            "display_name": "Average Net Shortwave Radiation",
            "internal_only": True,
        },
    )
    tair_av: float = Field(
        default=0,
        description="Average air temperature",
        json_schema_extra={"display_name": "Average Air Temperature"},
    )
    tmax_id: float = Field(
        default=0,
        description="Maximum temperature ID",
        json_schema_extra={
            "display_name": "Maximum Temperature ID",
            "internal_only": True,
        },
    )
    tmin_id: float = Field(
        default=0,
        description="Minimum temperature ID",
        json_schema_extra={
            "display_name": "Minimum Temperature ID",
            "internal_only": True,
        },
    )
    tstep_prev: float = Field(
        default=0,
        description="Previous time step",
        json_schema_extra={"display_name": "Previous Time Step", "internal_only": True},
    )
    snowfallcum: float = Field(
        default=0,
        description="Cumulative snowfall",
        json_schema_extra={
            "display_name": "Cumulative Snowfall",
            "internal_only": True,
        },
    )
    hdd_id: HDD_ID = Field(
        default_factory=HDD_ID,
        json_schema_extra={"display_name": "Heating Degree Days ID"},
        description="Heating degree days and meteorological tracking parameters",
    )

    @model_validator(mode="before")
    @classmethod
    def convert_hdd_id_from_list(cls, data):
        """Convert legacy list format for hdd_id to HDD_ID object."""
        if isinstance(data, dict) and "hdd_id" in data:
            hdd_value = data["hdd_id"]
            if isinstance(hdd_value, list):
                # Convert from legacy list format to HDD_ID object
                if len(hdd_value) >= 12:
                    data["hdd_id"] = {
                        "hdd_accum": hdd_value[0],
                        "cdd_accum": hdd_value[1],
                        "temp_accum": hdd_value[2],
                        "temp_5day_accum": hdd_value[3],
                        "precip_accum": hdd_value[4],
                        "days_since_rain_accum": hdd_value[5],
                        "hdd_daily": hdd_value[6],
                        "cdd_daily": hdd_value[7],
                        "temp_daily_mean": hdd_value[8],
                        "temp_5day_mean": hdd_value[9],
                        "precip_daily_total": hdd_value[10],
                        "days_since_rain": hdd_value[11],
                    }
                else:
                    # If list is too short, create default HDD_ID
                    data["hdd_id"] = {}
        return data

    def to_df_state(self, grid_id: int) -> pd.DataFrame:
        """Convert initial states to DataFrame state format."""
        df_state = init_df_state(grid_id)

        # Add snowalb
        df_state[("snowalb", "0")] = (
            self.snowalb.value if isinstance(self.snowalb, RefValue) else self.snowalb
        )

        # Add surface states
        surfaces = {
            "paved": self.paved,
            "bldgs": self.bldgs,
            "evetr": self.evetr,
            "dectr": self.dectr,
            "grass": self.grass,
            "bsoil": self.bsoil,
            "water": self.water,
        }
        # Add surface states
        for surface in surfaces.values():
            df_surface = surface.to_df_state(grid_id)
            df_state = pd.concat([df_state, df_surface], axis=1)

        # Add roof and wall states
        for facet_list, facet_type in [(self.roofs, "roof"), (self.walls, "wall")]:
            if facet_list is not None:  # Check for None explicitly
                for i, facet in enumerate(facet_list):
                    is_roof = facet_type == "roof"
                    df_facet = facet.to_df_state(grid_id, i, is_roof)
                    df_state = pd.concat([df_state, df_facet], axis=1)
                    df_state = df_state.sort_index(axis=1)

        # Add these attributes to the DataFrame
        df_state[("dqndt", "0")] = self.dqndt
        df_state[("dqnsdt", "0")] = self.dqnsdt
        df_state[("dt_since_start", "0")] = self.dt_since_start
        df_state[("lenday_id", "0")] = self.lenday_id
        df_state[("qn_av", "0")] = self.qn_av
        df_state[("qn_s_av", "0")] = self.qn_s_av
        df_state[("tair_av", "0")] = self.tair_av
        df_state[("tmax_id", "0")] = self.tmax_id
        df_state[("tmin_id", "0")] = self.tmin_id
        df_state[("tstep_prev", "0")] = self.tstep_prev
        df_state[("snowfallcum", "0")] = self.snowfallcum

        df_state = df_state.sort_index(axis=1)
        # special treatment for hdd_id - convert to list format for legacy compatibility
        hdd_list = self.hdd_id.to_list()
        for i, hdd in enumerate(hdd_list):
            df_state[(f"hdd_id", f"({i},)")] = hdd
        df_state = df_state.sort_index(axis=1)

        # Drop duplicate columns while preserving first occurrence
        df_state = df_state.loc[:, ~df_state.columns.duplicated(keep="first")]

        return df_state

    @classmethod
    def from_df_state(cls, df: pd.DataFrame, grid_id: int) -> "InitialStates":
        snowalb = df.loc[grid_id, ("snowalb", "0")]
        snowalb = RefValue(snowalb)

        surface_types = {
            "paved": InitialStatePaved,
            "bldgs": InitialStateBldgs,
            "evetr": InitialStateEvetr,
            "dectr": InitialStateDectr,
            "grass": InitialStateGrass,
            "bsoil": InitialStateBsoil,
            "water": InitialStateWater,
        }

        surfaces = {
            name: surface_class.from_df_state(df, grid_id, idx)
            for idx, (name, surface_class) in enumerate(surface_types.items())
        }

        def reconstruct_layers(
            layer_name: str, surface_class: Type[SurfaceInitialState], n_layers: int
        ) -> List[SurfaceInitialState]:
            layers = []
            for idx in range(n_layers):
                try:
                    layer = surface_class.from_df_state(df, grid_id, idx, layer_name)
                    layers.append(layer)
                except KeyError:
                    break
            return layers

        roofs = reconstruct_layers(
            "roof", SurfaceInitialState, len(cls.model_fields["roofs"].default)
        )
        walls = reconstruct_layers(
            "wall", SurfaceInitialState, len(cls.model_fields["walls"].default)
        )

        dqndt = df.loc[grid_id, ("dqndt", "0")]
        dqnsdt = df.loc[grid_id, ("dqnsdt", "0")]
        dt_since_start = df.loc[grid_id, ("dt_since_start", "0")]
        lenday_id = df.loc[grid_id, ("lenday_id", "0")]
        qn_av = df.loc[grid_id, ("qn_av", "0")]
        qn_s_av = df.loc[grid_id, ("qn_s_av", "0")]
        tair_av = df.loc[grid_id, ("tair_av", "0")]
        tmax_id = df.loc[grid_id, ("tmax_id", "0")]
        tmin_id = df.loc[grid_id, ("tmin_id", "0")]
        tstep_prev = df.loc[grid_id, ("tstep_prev", "0")]
        snowfallcum = df.loc[grid_id, ("snowfallcum", "0")]
        hdd_id_list = [df.loc[grid_id, (f"hdd_id", f"({i},)")] for i in range(12)]
        hdd_id = HDD_ID.from_list(hdd_id_list)

        initital_state = {
            "snowalb": snowalb,
            "paved": surfaces["paved"],
            "bldgs": surfaces["bldgs"],
            "evetr": surfaces["evetr"],
            "dectr": surfaces["dectr"],
            "grass": surfaces["grass"],
            "bsoil": surfaces["bsoil"],
            "water": surfaces["water"],
            "roofs": roofs,
            "walls": walls,
            "dqndt": dqndt,
            "dqnsdt": dqnsdt,
            "dt_since_start": dt_since_start,
            "lenday_id": lenday_id,
            "qn_av": qn_av,
            "qn_s_av": qn_s_av,
            "tair_av": tair_av,
            "tmax_id": tmax_id,
            "tmin_id": tmin_id,
            "tstep_prev": tstep_prev,
            "snowfallcum": snowfallcum,
            "hdd_id": hdd_id,
        }

        return cls(
            **initital_state,
        )
