import math
import numbers
from typing import Optional, Union, List, Literal, Type
import numpy as np
import pandas as pd
from pydantic import (
    ConfigDict,
    Field,
    field_validator,
    model_validator,
    PrivateAttr,
)

from .type import (
    DataModelBase,
    RefValue,
    Reference,
    FlexibleRefValue,
    df_from_cols,
    merge_df_state_cols,
    SurfaceType,
)
from ..._env import logger_supy


class SurfaceInitialState(DataModelBase):
    """Generic initial state parameters for all surface types"""

    model_config = ConfigDict(title="Generic Surface Initial State")

    state: FlexibleRefValue(float) = Field(
        description="Initial water state of the surface",
        json_schema_extra={"unit": "kg m-2", "display_name": "Initial Surface Water State"},
        default=0.0,
        ge=0,
    )  # Default set to 0.0 means dry surface.
    soilstore: FlexibleRefValue(float) = Field(
        description="Initial soil store. Related to Soil Moisture Deficit (SMD) as SMD = soilstorecap - soilstore.",
        json_schema_extra={"unit": "kg m-2", "display_name": "Initial Soil Store"},
        default=150.0,
        ge=10,
    )  # Default set to 150.0 (wet soil) and ge=10 (less than 10 would be too dry) are physically reasonable for a model run.
    snowfrac: Optional[Union[FlexibleRefValue(float), None]] = Field(
        description="Snow fraction",
        json_schema_extra={"unit": "1", "display_name": "Snow Fraction"},
        default=0.0,
        ge=0,
        le=1,
    )  # Default set to 0.0 means no snow on the ground.
    snowpack: Optional[Union[FlexibleRefValue(float), None]] = Field(
        description="Snow pack",
        json_schema_extra={"unit": "kg m-2", "display_name": "Snow Pack"},
        default=0.0,
        ge=0,
    )
    icefrac: Optional[Union[FlexibleRefValue(float), None]] = Field(
        description="Ice fraction",
        json_schema_extra={"unit": "1", "display_name": "Ice Fraction"},
        default=0.0,
        ge=0,
        le=1,
    )
    snowwater: Optional[Union[FlexibleRefValue(float), None]] = Field(
        description="Snow water",
        json_schema_extra={"unit": "kg m-2", "display_name": "Snow Water"},
        default=0.0,
        ge=0,
    )
    snowdens: Optional[Union[FlexibleRefValue(float), None]] = Field(
        description="Snow density",
        json_schema_extra={"unit": "kg m-3", "display_name": "Snow Density"},
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

    def _to_df_state_cols(
        self, grid_id: int, vert_idx: int | None = None, is_roof: bool = False
    ) -> dict:
        if vert_idx is None:
            surf_idx = self.get_surface_index()
            str_type = "surf"
        else:
            surf_idx = vert_idx
            str_type = "roof" if is_roof else "wall"
        cols = {
            ("gridiv", "0"): grid_id,
            (f"state_{str_type}", f"({surf_idx},)"): (
                self.state.value if isinstance(self.state, RefValue) else self.state
            ),
            (f"soilstore_{str_type}", f"({surf_idx},)"): (
                self.soilstore.value
                if isinstance(self.soilstore, RefValue)
                else self.soilstore
            ),
        }
        if self.snowfrac is not None:
            cols[("snowfrac", f"({surf_idx},)")] = (
                self.snowfrac.value
                if isinstance(self.snowfrac, RefValue)
                else self.snowfrac
            )
        if self.snowpack is not None:
            cols[("snowpack", f"({surf_idx},)")] = (
                self.snowpack.value
                if isinstance(self.snowpack, RefValue)
                else self.snowpack
            )
        if self.icefrac is not None:
            cols[("icefrac", f"({surf_idx},)")] = (
                self.icefrac.value
                if isinstance(self.icefrac, RefValue)
                else self.icefrac
            )
        if self.snowwater is not None:
            cols[("snowwater", f"({surf_idx},)")] = (
                self.snowwater.value
                if isinstance(self.snowwater, RefValue)
                else self.snowwater
            )
        if self.snowdens is not None:
            cols[("snowdens", f"({surf_idx},)")] = (
                self.snowdens.value
                if isinstance(self.snowdens, RefValue)
                else self.snowdens
            )
        temp_values = (
            self.temperature.value
            if isinstance(self.temperature, RefValue)
            else self.temperature
        )
        for i, temp in enumerate(temp_values):
            cols[(f"temp_{str_type}", f"({surf_idx}, {i})")] = temp
        if self.tsfc is not None:
            cols[(f"tsfc_{str_type}", f"({surf_idx},)")] = (
                self.tsfc.value if isinstance(self.tsfc, RefValue) else self.tsfc
            )
        if self.tin is not None:
            cols[(f"tin_{str_type}", f"({surf_idx},)")] = (
                self.tin.value if isinstance(self.tin, RefValue) else self.tin
            )
        return cols

    def to_df_state(
        self, grid_id: int, vert_idx: int = None, is_roof: bool = False
    ) -> pd.DataFrame:
        """Convert base surface initial state to DataFrame state format.

        Args:
            grid_id: Grid ID for the DataFrame index

        Returns:
            pd.DataFrame: DataFrame containing initial state parameters
        """
        return df_from_cols(
            self._to_df_state_cols(grid_id, vert_idx, is_roof),
            index=pd.Index([grid_id], name="grid"),
        )

    @classmethod
    def from_df_state(
        cls,
        df: pd.DataFrame,
        grid_id: int,
        surf_idx: int,
        str_type: str = "surf",
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

        Note:
            This method handles both column index formats:
            - Standard array format: "(idx,)" for multi-element arrays
            - Scalar format: "idx" for single-element arrays (e.g., when nlayer=1)
            The method tries the standard format first and falls back to scalar format if needed.
        """

        def safe_get_value(var_name: str, idx: int) -> float:
            """
            Safely retrieve value from DataFrame, trying both index formats.

            DataFrame columns can use different index formats:
            - "(idx,)" for arrays with multiple elements
            - "idx" for single-element arrays (scalar-like)

            This occurs when nlayer=1, where roof/wall arrays have only one element
            and may be stored with scalar indexing convention.
            """
            # Try standard array format first
            try:
                return df.loc[grid_id, (var_name, f"({idx},)")]
            except KeyError:
                # Fall back to scalar format
                try:
                    return df.loc[grid_id, (var_name, str(idx))]
                except KeyError:
                    raise KeyError(
                        f"Column ({var_name}, {idx}) not found in DataFrame. "
                        f"Tried both '({idx},)' and '{idx}' formats."
                    )

        # Base surface state parameters
        state = RefValue(safe_get_value(f"state_{str_type}", surf_idx))
        soilstore = RefValue(safe_get_value(f"soilstore_{str_type}", surf_idx))

        # Snow/ice parameters
        if str_type not in ["roof", "wall"]:
            snowfrac = RefValue(safe_get_value("snowfrac", surf_idx))
            snowpack = RefValue(safe_get_value("snowpack", surf_idx))
            icefrac = RefValue(safe_get_value("icefrac", surf_idx))
            snowwater = RefValue(safe_get_value("snowwater", surf_idx))
            snowdens = RefValue(safe_get_value("snowdens", surf_idx))
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
        tsfc = RefValue(safe_get_value(f"tsfc_{str_type}", surf_idx))
        tin = RefValue(safe_get_value(f"tin_{str_type}", surf_idx))

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


RETIRED_CONTINUATION_STATE_FIELDS = (
    "dt_since_start",
    "tair_av",
    "tstep_prev",
    "l_mod",
    "ustar",
    "ra_h",
    "rb",
    "rs",
)
VEG_SURFACE_NAMES = ("evetr", "dectr", "grass")


def _df_scalar_matches(value: object, allowed: tuple[float, ...]) -> bool:
    """Return True when ``value`` is a finite real scalar equal to one of ``allowed``."""
    if isinstance(value, (bool, np.bool_)) or not isinstance(value, numbers.Real):
        return False
    try:
        numeric = float(value)
    except (TypeError, ValueError, OverflowError):
        return False
    return math.isfinite(numeric) and any(numeric == target for target in allowed)


class InitialStatePaved(SurfaceInitialState):
    model_config = ConfigDict(title="Paved Surface Initial State")
    _surface_type: Literal[SurfaceType.PAVED] = SurfaceType.PAVED


class InitialStateBldgs(SurfaceInitialState):
    model_config = ConfigDict(title="Buildings Surface Initial State")
    _surface_type: Literal[SurfaceType.BLDGS] = SurfaceType.BLDGS


class InitialStateVeg(SurfaceInitialState):
    """Extended initial state parameters for vegetated surfaces with LAI and phenology"""

    model_config = ConfigDict(
        title="Vegetation Initial State",
        json_schema_extra={"not": {"required": ["wu"]}},
    )

    alb_id: Optional[FlexibleRefValue(float)] = Field(
        description=(
            "Albedo at the start of the model run. If not provided, it is "
            "computed from the LAI state and vegetation-specific albedo limits "
            "(alb_min, alb_max). Trees follow a direct LAI-albedo relationship "
            "(denser canopy -> higher albedo due to leaves being more reflective "
            "than bark), while grass follows a reversed relationship (denser grass "
            "-> lower albedo as soil is more reflective than grass blades)."
        ),
        json_schema_extra={"unit": "1", "display_name": "Initial Albedo"},
        default=None,
        ge=0.0,
        le=1.0,
    )
    lai_id: FlexibleRefValue(float) = Field(
        description="Leaf area index at the start of the model run.",
        json_schema_extra={
            "unit": "m2 m-2",
            "display_name": "Initial Leaf Area Index",
        },
        default=1.0,
    )
    gdd_id: FlexibleRefValue(float) = Field(
        description="Growing degree days at the start of the model run",
        json_schema_extra={
            "unit": "K d",
            "display_name": "Initial Growing Degree Days",
        },
        default=0,
    )  # We need to check this and give info for setting values.
    sdd_id: FlexibleRefValue(float) = Field(
        description="Senescence degree days at the start of the model run",
        json_schema_extra={
            "unit": "K d",
            "display_name": "Initial Senescence Degree Days",
        },
        default=0,
    )  # This need to be consistent with GDD.

    ref: Optional[Reference] = None

    @model_validator(mode="before")
    @classmethod
    def reject_water_use_memory_input(cls, values):
        """Keep internal irrigation memory out of fresh-run configuration."""
        if isinstance(values, dict) and "wu" in values:
            surface_attr = cls.__private_attributes__.get("_surface_type")
            surface_default = getattr(surface_attr, "default", None)
            surface_label = (
                surface_default.value.lower()
                if isinstance(surface_default, SurfaceType)
                else "<veg>"
            )
            raise ValueError(
                f"initial_states.{surface_label}.wu is internal irrigation memory "
                "and cannot be supplied for a fresh run. Remove the field; "
                "use a complete checkpoint for continuation."
            )
        return values

    def _get_alb_id_value(self) -> float:
        """Return a usable albedo value, falling back to a legacy default."""
        alb_val = (
            self.alb_id.value if isinstance(self.alb_id, RefValue) else self.alb_id
        )
        if alb_val is None:
            # Only reached when no land_cover properties are available to
            # derive albedo (the normal auto-calculation path runs in
            # SUEWSConfig.set_default_vegetation_albedo before this).
            logger_supy.warning(
                "alb_id is None for %s; using legacy default 0.25. "
                "Set alb_id explicitly or provide LAI params for auto-calculation.",
                type(self).__name__,
            )
            return 0.25
        return alb_val

    def _to_df_state_cols(self, grid_id: int) -> dict:
        cols = super()._to_df_state_cols(grid_id)
        surf_idx = self.get_surface_index()
        veg_idx = surf_idx - 2
        alb_val = self._get_alb_id_value()
        merge_df_state_cols(
            cols,
            {
                ("alb", f"({surf_idx},)"): alb_val,
                ("lai_id", f"({veg_idx},)"): (
                    self.lai_id.value
                    if isinstance(self.lai_id, RefValue)
                    else self.lai_id
                ),
                ("gdd_id", f"({veg_idx},)"): (
                    self.gdd_id.value
                    if isinstance(self.gdd_id, RefValue)
                    else self.gdd_id
                ),
                ("sdd_id", f"({veg_idx},)"): (
                    self.sdd_id.value
                    if isinstance(self.sdd_id, RefValue)
                    else self.sdd_id
                ),
            },
        )
        # Legacy DFState retains zero placeholders, never irrigation restart memory.
        for offset in range(3):
            cols[("wuday_id", f"({veg_idx * 3 + offset},)")] = 0.0
        return cols

    def to_df_state(self, grid_id: int) -> pd.DataFrame:
        """Convert vegetated surface initial state to DataFrame state format.

        Args:
            grid_id: Grid ID for the DataFrame index

        Returns:
            pd.DataFrame: DataFrame containing initial state parameters
        """
        return df_from_cols(
            self._to_df_state_cols(grid_id), index=pd.Index([grid_id], name="grid")
        )

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

        # A DFState is a fresh-run adapter, not a complete checkpoint.
        veg_idx = surf_idx - 2
        for offset in range(3):
            idx = veg_idx * 3 + offset
            column = ("wuday_id", f"({idx},)")
            if column in df.columns and not _df_scalar_matches(
                df.loc[grid_id, column], (0.0,)
            ):
                raise ValueError(
                    f"wuday_id[{idx}] cannot initialise irrigation memory from "
                    "DFState. Use a complete checkpoint for continuation."
                )

        return cls(
            **base_instance.model_dump(),
            alb_id=alb_id,
            lai_id=lai_id,
            gdd_id=gdd_id,
            sdd_id=sdd_id,
        )


class InitialStateEvetr(InitialStateVeg):
    """Initial state for evergreen trees with vegetation parameters."""

    model_config = ConfigDict(
        title="Evergreen Tree Initial State",
        json_schema_extra={"not": {"required": ["wu"]}},
    )

    _surface_type: Literal[SurfaceType.EVETR] = SurfaceType.EVETR

    def _to_df_state_cols(self, grid_id: int) -> dict:
        cols = super()._to_df_state_cols(grid_id)
        cols[("albevetr_id", "0")] = self._get_alb_id_value()
        return cols

    def to_df_state(self, grid_id: int) -> pd.DataFrame:
        """Convert evergreen tree initial state to DataFrame state format."""
        return df_from_cols(
            self._to_df_state_cols(grid_id), index=pd.Index([grid_id], name="grid")
        )

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
    """Initial state for deciduous trees with vegetation parameters plus porosity."""

    model_config = ConfigDict(
        title="Deciduous Tree Initial State",
        json_schema_extra={"not": {"required": ["wu"]}},
    )

    porosity_id: FlexibleRefValue(float) = Field(
        description="Porosity for deciduous trees at the start of the model run",
        json_schema_extra={"unit": "1", "display_name": "Initial Porosity"},
        default=0.2,
        ge=0,
        le=1,
    )
    decidcap_id: FlexibleRefValue(float) = Field(
        description="Deciduous capacity for deciduous trees at the start of the model run",
        json_schema_extra={"unit": "kg m-2", "display_name": "Initial Deciduous Capacity"},
        default=0.3,
        ge=0,
    )
    _surface_type: Literal[SurfaceType.DECTR] = SurfaceType.DECTR

    ref: Optional[Reference] = None

    def _to_df_state_cols(self, grid_id: int) -> dict:
        cols = super()._to_df_state_cols(grid_id)
        cols[("porosity_id", "0")] = (
            self.porosity_id.value
            if isinstance(self.porosity_id, RefValue)
            else self.porosity_id
        )
        cols[("decidcap_id", "0")] = (
            self.decidcap_id.value
            if isinstance(self.decidcap_id, RefValue)
            else self.decidcap_id
        )
        cols[("albdectr_id", "0")] = self._get_alb_id_value()
        return cols

    def to_df_state(self, grid_id: int) -> pd.DataFrame:
        """Convert deciduous tree initial state to DataFrame state format.

        Args:
            grid_id: Grid ID for the DataFrame index

        Returns:
            pd.DataFrame: DataFrame containing initial state parameters
        """
        return df_from_cols(
            self._to_df_state_cols(grid_id), index=pd.Index([grid_id], name="grid")
        )

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
    """Initial state for grass surfaces with vegetation parameters."""

    model_config = ConfigDict(
        title="Grass Initial State",
        json_schema_extra={"not": {"required": ["wu"]}},
    )

    _surface_type: Literal[SurfaceType.GRASS] = SurfaceType.GRASS

    def _to_df_state_cols(self, grid_id: int) -> dict:
        cols = super()._to_df_state_cols(grid_id)
        cols[("albgrass_id", "0")] = self._get_alb_id_value()
        return cols

    def to_df_state(self, grid_id: int) -> pd.DataFrame:
        """Convert grass initial state to DataFrame state format."""
        return df_from_cols(
            self._to_df_state_cols(grid_id), index=pd.Index([grid_id], name="grid")
        )

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
    model_config = ConfigDict(title="Bare Soil Surface Initial State")
    _surface_type: Literal[SurfaceType.BSOIL] = SurfaceType.BSOIL


class InitialStateWater(SurfaceInitialState):
    model_config = ConfigDict(title="Water Surface Initial State")
    _surface_type: Literal[SurfaceType.WATER] = SurfaceType.WATER

    # Override soilstore for water surfaces to allow 0 (water doesn't have soil)
    soilstore: FlexibleRefValue(float) = Field(
        description="Initial soil store (not applicable for water surfaces)",
        json_schema_extra={"unit": "kg m-2", "display_name": "Initial Soil Store"},
        default=0.0,
        ge=0,  # Water surfaces can have 0 soilstore
    )


class HDD_ID(DataModelBase):
    """Heating Degree Days and related meteorological tracking parameters.

    This structure maintains both current day accumulations (fields 1-6) and
    previous day values (fields 7-12) for various meteorological parameters
    used in anthropogenic heat and water use calculations.
    """

    model_config = ConfigDict(title="Heating Degree Days")

    # Current day accumulations (updated throughout the day)
    hdd_accum: float = Field(
        default=0.0,
        description="Current day's heating degree days accumulation [degC]",
        json_schema_extra={
            "unit": "degC",
            "display_name": "Heating Degree Days Accumulation",
            "internal_only": True,
        },
    )
    cdd_accum: float = Field(
        default=0.0,
        description="Current day's cooling degree days accumulation [degC]",
        json_schema_extra={
            "unit": "degC",
            "display_name": "Cooling Degree Days Accumulation",
            "internal_only": True,
        },
    )
    temp_accum: float = Field(
        default=0.0,
        description="Current day's temperature accumulation for daily mean [degC]",
        json_schema_extra={
            "unit": "degC",
            "display_name": "Temperature Accumulation",
            "internal_only": True,
        },
    )
    temp_5day_accum: float = Field(
        default=0.0,
        description="5-day running mean temperature accumulation [degC]",
        json_schema_extra={
            "unit": "degC",
            "display_name": "5-Day Temperature Accumulation",
            "internal_only": True,
        },
    )
    precip_accum: float = Field(
        default=0.0,
        description="Current day's precipitation total [mm]",
        json_schema_extra={
            "unit": "mm",
            "display_name": "Precipitation Accumulation",
            "internal_only": True,
        },
    )
    days_since_rain_accum: float = Field(
        default=0.0,
        description="Days since rain counter (current) [d]",
        json_schema_extra={
            "unit": "d",
            "display_name": "Days Since Rain (Accumulator)",
            "internal_only": True,
        },
    )

    # Previous day values (used in calculations)
    hdd_daily: float = Field(
        default=0.0,
        description="Previous day's heating degree days for QF calculations [degC]",
        json_schema_extra={
            "unit": "degC",
            "display_name": "Daily Heating Degree Days",
            "internal_only": True,
        },
    )
    cdd_daily: float = Field(
        default=0.0,
        description="Previous day's cooling degree days for QF calculations [degC]",
        json_schema_extra={
            "unit": "degC",
            "display_name": "Daily Cooling Degree Days",
            "internal_only": True,
        },
    )
    temp_daily_mean: float = Field(
        default=0.0,
        description="Previous day's mean temperature for water use calculations [degC]",
        json_schema_extra={
            "unit": "degC",
            "display_name": "Daily Mean Temperature",
            "internal_only": True,
        },
    )
    temp_5day_mean: float = Field(
        default=0.0,
        description="Previous 5-day running mean temperature for QF calculations [degC]",
        json_schema_extra={
            "unit": "degC",
            "display_name": "5-Day Mean Temperature",
            "internal_only": True,
        },
    )
    precip_daily_total: float = Field(
        default=0.0,
        description="Previous day's precipitation total [mm]",
        json_schema_extra={
            "unit": "mm",
            "display_name": "Daily Precipitation Total",
            "internal_only": True,
        },
    )
    days_since_rain: float = Field(
        default=0.0,
        description="Days since rain for irrigation calculations [d]",
        json_schema_extra={
            "unit": "d",
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


class InitialStates(DataModelBase):
    """Initial conditions for fresh SUEWS runs.

    Internal continuation state (radiation memory ``qn_surfs``/``dqndt_surf``,
    daily irrigation memory ``evetr``/``dectr``/``grass.wu``, and running
    timer/atmosphere diagnostics ``dt_since_start``, ``tair_av``,
    ``tstep_prev``, ``l_mod``, ``ustar``, ``ra_h``, ``rb``, ``rs``) is
    rejected on fresh runs and restored solely through complete checkpoints.
    Other unconfigured Fortran state members follow documented cold-start
    conventions: step-1 running diagnostics are overwritten before use,
    AnOHM buffers start at -999/0/false, ``tmin_id``/``tmax_id`` assume a
    midnight start at 0, DyOHM (methods 6-8) dynamic coefficients start at 0
    until the first midnight with ``t2_prev`` and ``temp_surf_dyohm`` at
    0 degC, and STEBBS under NetRadiationMethod < 1000 sees 0 W m-2 BEERS
    radiation on step 1 before BEERS runs at the end of the step.
    """

    model_config = ConfigDict(
        title="Initial States",
        json_schema_extra={
            "not": {"anyOf": [
                {"required": ["qn_surfs"]},
                {"required": ["dqndt_surf"]},
                {"required": ["dt_since_start"]},
                {"required": ["tair_av"]},
                {"required": ["tstep_prev"]},
                {"required": ["l_mod"]},
                {"required": ["ustar"]},
                {"required": ["ra_h"]},
                {"required": ["rb"]},
                {"required": ["rs"]},
            ]},
        },
    )

    snowalb: FlexibleRefValue(float) = Field(
        description="Snow albedo at the start of the model run",
        json_schema_extra={"unit": "1", "display_name": "Snow Albedo"},
        default=0.5,
        ge=0,
        le=1,
    )

    paved: InitialStatePaved = Field(
        default_factory=InitialStatePaved,
        description="Initial states for paved surfaces",
        json_schema_extra={"display_name": "Paved"},
    )
    bldgs: InitialStateBldgs = Field(
        default_factory=InitialStateBldgs,
        description="Initial states for building surfaces",
        json_schema_extra={"display_name": "Buildings"},
    )
    evetr: InitialStateEvetr = Field(
        default_factory=InitialStateEvetr,
        description="Initial states for evergreen tree surfaces",
        json_schema_extra={"display_name": "Evergreen Trees"},
    )
    dectr: InitialStateDectr = Field(
        default_factory=InitialStateDectr,
        description="Initial states for deciduous tree surfaces",
        json_schema_extra={"display_name": "Deciduous Trees"},
    )
    grass: InitialStateGrass = Field(
        default_factory=InitialStateGrass,
        description="Initial states for grass surfaces",
        json_schema_extra={"display_name": "Grass"},
    )
    bsoil: InitialStateBsoil = Field(
        default_factory=InitialStateBsoil,
        description="Initial states for bare soil surfaces",
        json_schema_extra={"display_name": "Bare Soil"},
    )
    water: InitialStateWater = Field(
        default_factory=InitialStateWater,
        description="Initial states for water surfaces",
        json_schema_extra={"display_name": "Water"},
    )
    roofs: Optional[List[SurfaceInitialState]] = Field(
        default=[
            SurfaceInitialState(
                snowfrac=None, snowpack=None, icefrac=None,
                snowwater=None, snowdens=None,
            ),
            SurfaceInitialState(
                snowfrac=None, snowpack=None, icefrac=None,
                snowwater=None, snowdens=None,
            ),
            SurfaceInitialState(
                snowfrac=None, snowpack=None, icefrac=None,
                snowwater=None, snowdens=None,
            ),
        ],
        description="Initial states for roof layers",
        json_schema_extra={"display_name": "Roofs"},
    )
    walls: Optional[List[SurfaceInitialState]] = Field(
        default=[
            SurfaceInitialState(
                snowfrac=None, snowpack=None, icefrac=None,
                snowwater=None, snowdens=None,
            ),
            SurfaceInitialState(
                snowfrac=None, snowpack=None, icefrac=None,
                snowwater=None, snowdens=None,
            ),
            SurfaceInitialState(
                snowfrac=None, snowpack=None, icefrac=None,
                snowwater=None, snowdens=None,
            ),
        ],
        description="Initial states for wall layers",
        json_schema_extra={"display_name": "Walls"},
    )

    @model_validator(mode="after")
    def _clear_building_surface_snow(self):
        """Roofs and walls do not track snow state independently.

        Snow on buildings is tracked via the aggregate Bldgs surface type.
        Proper per-surface snow handling is deferred to a future snow module update.
        """
        for surfaces in (self.roofs, self.walls):
            if surfaces:
                for surf in surfaces:
                    surf.snowfrac = None
                    surf.snowpack = None
                    surf.icefrac = None
                    surf.snowwater = None
                    surf.snowdens = None
        return self

    dqndt: float = Field(
        default=0,
        description="Change in net radiation",
        json_schema_extra={
            "display_name": "Net Radiation change rate",
            "internal_only": True,
        },
    )
    dqnsdt: float = Field(
        default=0,
        description="Change in net radiation over snow surface",
        json_schema_extra={
            "display_name": "Net Radiation change rate over Snow Surface",
            "internal_only": True,
        },
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
    tmax_id: float = Field(
        default=0,
        description="Maximum temperature ID (cold-start default assumes midnight start)",
        json_schema_extra={
            "display_name": "Maximum Temperature ID",
            "internal_only": True,
        },
    )
    tmin_id: float = Field(
        default=0,
        description="Minimum temperature ID (cold-start default assumes midnight start)",
        json_schema_extra={
            "display_name": "Minimum Temperature ID",
            "internal_only": True,
        },
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
        json_schema_extra={
            "display_name": "Heating Degree Days ID",
            "internal_only": True,
        },
        description="Heating degree days and meteorological tracking parameters (internal use only)",
    )

    @model_validator(mode="before")
    @classmethod
    def reject_radiation_memory_input(cls, values):
        """Keep internal radiation memory out of fresh-run configuration."""
        if isinstance(values, dict):
            for name in ("qn_surfs", "dqndt_surf"):
                if name in values:
                    raise ValueError(
                        f"initial_states.{name} is internal radiation memory and "
                        "cannot be supplied for a fresh run. Remove the field; "
                        "use a complete checkpoint for continuation."
                    )
        return values

    @model_validator(mode="before")
    @classmethod
    def reject_continuation_state_input(cls, values):
        """Keep internal continuation state and irrigation memory out of fresh runs."""
        if isinstance(values, dict):
            for name in RETIRED_CONTINUATION_STATE_FIELDS:
                if name in values:
                    raise ValueError(
                        f"initial_states.{name} is internal continuation state and "
                        "cannot be supplied for a fresh run. Remove the field; "
                        "use a complete checkpoint for continuation."
                    )
            for veg in VEG_SURFACE_NAMES:
                veg_state = values.get(veg)
                if isinstance(veg_state, dict) and "wu" in veg_state:
                    raise ValueError(
                        f"initial_states.{veg}.wu is internal irrigation memory and "
                        "cannot be supplied for a fresh run. Remove the field; "
                        "use a complete checkpoint for continuation."
                    )
        return values

    def _to_df_state_cols(self, grid_id: int) -> dict:
        cols = {
            ("gridiv", "0"): grid_id,
            ("snowalb", "0"): (
                self.snowalb.value
                if isinstance(self.snowalb, RefValue)
                else self.snowalb
            ),
            ("dqndt", "0"): self.dqndt,
            ("dqnsdt", "0"): self.dqnsdt,
            # Legacy DFState retains cold-start zero placeholders, never continuation state.
            ("dt_since_start", "0"): 0.0,
            ("lenday_id", "0"): self.lenday_id,
            ("qn_av", "0"): self.qn_av,
            ("qn_s_av", "0"): self.qn_s_av,
            ("tair_av", "0"): 0.0,
            ("tmax_id", "0"): self.tmax_id,
            ("tmin_id", "0"): self.tmin_id,
            ("tstep_prev", "0"): 0.0,
            ("snowfallcum", "0"): self.snowfallcum,
        }
        # Legacy DFState retains zero placeholders, never restart memory.
        for name in ("qn_surfs", "dqndt_surf"):
            for i in range(7):
                cols[(name, f"({i},)")] = 0.0
        for i, hdd in enumerate(self.hdd_id.to_list()):
            cols[("hdd_id", f"({i},)")] = hdd

        for surface in (
            self.paved,
            self.bldgs,
            self.evetr,
            self.dectr,
            self.grass,
            self.bsoil,
            self.water,
        ):
            merge_df_state_cols(cols, surface._to_df_state_cols(grid_id))

        for facet_list, is_roof in ((self.roofs, True), (self.walls, False)):
            if facet_list is not None:
                for i, facet in enumerate(facet_list):
                    merge_df_state_cols(cols, facet._to_df_state_cols(grid_id, i, is_roof))

        return dict(sorted(cols.items()))

    def to_df_state(self, grid_id: int) -> pd.DataFrame:
        """Convert initial states to DataFrame state format."""
        return df_from_cols(
            self._to_df_state_cols(grid_id), index=pd.Index([grid_id], name="grid")
        )

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

        # Detect the actual number of layers
        # First try to get nlayer directly from the dataframe
        try:
            n_layers = int(df.loc[grid_id, ("nlayer", "0")])
        except (KeyError, ValueError, TypeError):
            # If nlayer not available, use default with warning
            n_layers = len(cls.model_fields["roofs"].default)
            logger_supy.warning(
                f"nlayer not found in df_state for grid {grid_id}, "
                f"using default {n_layers} layers. "
                f"Consider adding nlayer to your GridLayout configuration."
            )

        roofs = reconstruct_layers("roof", SurfaceInitialState, n_layers)
        walls = reconstruct_layers("wall", SurfaceInitialState, n_layers)

        dqndt = df.loc[grid_id, ("dqndt", "0")]
        dqnsdt = df.loc[grid_id, ("dqnsdt", "0")]
        lenday_id = df.loc[grid_id, ("lenday_id", "0")]
        qn_av = df.loc[grid_id, ("qn_av", "0")]
        qn_s_av = df.loc[grid_id, ("qn_s_av", "0")]
        tmax_id = df.loc[grid_id, ("tmax_id", "0")]
        tmin_id = df.loc[grid_id, ("tmin_id", "0")]
        snowfallcum = df.loc[grid_id, ("snowfallcum", "0")]
        hdd_id_list = [df.loc[grid_id, (f"hdd_id", f"({i},)")] for i in range(12)]
        hdd_id = HDD_ID.from_list(hdd_id_list)
        # A DFState is a fresh-run adapter, not a complete checkpoint.
        for name in ("qn_surfs", "dqndt_surf"):
            for i in range(7):
                column = (name, f"({i},)")
                if column in df.columns and not _df_scalar_matches(
                    df.loc[grid_id, column], (0.0,)
                ):
                    raise ValueError(
                        f"{name}[{i}] cannot initialise radiation memory from "
                        "DFState. Use a complete checkpoint for continuation."
                    )

        tstep_allowed: tuple[float, ...] = (0.0,)
        if ("tstep", "0") in df.columns:
            raw_tstep = df.loc[grid_id, ("tstep", "0")]
            if not isinstance(raw_tstep, (bool, np.bool_)) and isinstance(
                raw_tstep, numbers.Real
            ):
                try:
                    tstep_num = float(raw_tstep)
                except (TypeError, ValueError, OverflowError):
                    tstep_num = math.nan
                if math.isfinite(tstep_num):
                    tstep_allowed = (0.0, tstep_num)

        df_placeholder_rules = (
            ("dt_since_start", (0.0,)),
            ("tair_av", (0.0, 273.15)),
            ("tstep_prev", tstep_allowed),
            ("l_mod", (0.0,)),
            ("ustar", (0.0,)),
            ("ra_h", (0.0,)),
            ("rb", (0.0,)),
            ("rs", (0.0,)),
        )
        for name, allowed in df_placeholder_rules:
            column = (name, "0")
            if column in df.columns and not _df_scalar_matches(
                df.loc[grid_id, column], allowed
            ):
                raise ValueError(
                    f"{name} cannot initialise continuation state from DFState. "
                    "Use a complete checkpoint for continuation."
                )

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
            "lenday_id": lenday_id,
            "qn_av": qn_av,
            "qn_s_av": qn_s_av,
            "tmax_id": tmax_id,
            "tmin_id": tmin_id,
            "snowfallcum": snowfallcum,
            "hdd_id": hdd_id,
        }

        return cls(
            **initital_state,
        )
