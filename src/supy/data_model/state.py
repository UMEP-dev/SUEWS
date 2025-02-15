from typing import Optional, Union, List, Literal, Type
import pandas as pd
from pydantic import BaseModel, Field, field_validator, model_validator, PrivateAttr

from .type import ValueWithDOI, Reference, init_df_state
from .site import SurfaceType





class SurfaceInitialState(BaseModel):
    """Base initial state parameters for all surface types"""

    state: ValueWithDOI[float] = Field(
        description="Initial state of the surface",
        default=ValueWithDOI(0.0),
        ge=0,
    )  # Default set to 0.0 means dry surface.
    soilstore: ValueWithDOI[float] = Field(
        description="Initial soil store (essential for QE)",
        default=ValueWithDOI(150.0),
        ge=10,
    )  # Default set to 150.0 (wet soil) and ge=10 (less than 10 would be too dry) are physically reasonable for a model run.
    snowfrac: Optional[Union[ValueWithDOI[float], None]] = Field(
        description="Snow fraction",
        default=ValueWithDOI(0.0),
        ge=0,
        le=1,
    )  # Default set to 0.0 means no snow on the ground.
    snowpack: Optional[Union[ValueWithDOI[float], None]] = Field(
        description="Snow pack",
        default=ValueWithDOI(0.0),
        ge=0,
    )
    icefrac: Optional[Union[ValueWithDOI[float], None]] = Field(
        description="Ice fraction",
        default=ValueWithDOI(0.0),
        ge=0,
        le=1,
    )
    snowwater: Optional[Union[ValueWithDOI[float], None]] = Field(
        description="Snow water",
        default=ValueWithDOI(0.0),
        ge=0,
    )
    snowdens: Optional[Union[ValueWithDOI[float], None]] = Field(
        description="Snow density",
        default=ValueWithDOI(0.0),
        ge=0,
    )
    temperature: ValueWithDOI[List[float]] = Field(
        description="Initial temperature for each thermal layer",
        default=ValueWithDOI([15.0, 15.0, 15.0, 15.0, 15.0]),
    )  # We need to check/undestand what model are these temperatures related to. ESTM? What surface type (wall and roof) of building?
    tsfc: Optional[Union[ValueWithDOI[float], None]] = Field(
        description="Initial exterior surface temperature",
        default=ValueWithDOI(15.0),
    )
    tin: Optional[Union[ValueWithDOI[float], None]] = Field(
        description="Initial interior surface temperature", default=ValueWithDOI(20.0)
    )  # We need to know which model is using this.
    _surface_type: Optional[SurfaceType] = PrivateAttr(default=None)

    ref: Optional[Reference] = None

    @field_validator("temperature", mode="before")
    def validate_temperature(cls, v):
        if isinstance(v, dict):
            value = v["value"]
        else:
            value = v.value
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
        df_state[(f"state_{str_type}", f"({idx},)")] = self.state.value
        df_state[(f"soilstore_{str_type}", f"({idx},)")] = self.soilstore.value

        # Set snow/ice parameters if present
        if self.snowfrac is not None:
            df_state[(f"snowfrac", f"({idx},)")] = self.snowfrac.value
        if self.snowpack is not None:
            df_state[(f"snowpack", f"({idx},)")] = self.snowpack.value
        if self.icefrac is not None:
            df_state[(f"icefrac", f"({idx},)")] = self.icefrac.value
        if self.snowwater is not None:
            df_state[(f"snowwater", f"({idx},)")] = self.snowwater.value
        if self.snowdens is not None:
            df_state[(f"snowdens", f"({idx},)")] = self.snowdens.value

        # Set temperature parameters
        for i, temp in enumerate(self.temperature.value):
            df_state[(f"temp_{str_type}", f"({idx}, {i})")] = temp

        if self.tsfc is not None:
            df_state[(f"tsfc_{str_type}", f"({idx},)")] = self.tsfc.value
        if self.tin is not None:
            df_state[(f"tin_{str_type}", f"({idx},)")] = self.tin.value

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
        state = ValueWithDOI[float](
            df.loc[grid_id, (f"state_{str_type}", f"({surf_idx},)")]
        )
        soilstore = ValueWithDOI[float](
            df.loc[grid_id, (f"soilstore_{str_type}", f"({surf_idx},)")]
        )

        # Snow/ice parameters
        if str_type not in ["roof", "wall"]:
            snowfrac = ValueWithDOI[float](
                df.loc[grid_id, (f"snowfrac", f"({surf_idx},)")]
            )
            snowpack = ValueWithDOI[float](
                df.loc[grid_id, (f"snowpack", f"({surf_idx},)")]
            )
            icefrac = ValueWithDOI[float](
                df.loc[grid_id, (f"icefrac", f"({surf_idx},)")]
            )
            snowwater = ValueWithDOI[float](
                df.loc[grid_id, (f"snowwater", f"({surf_idx},)")]
            )
            snowdens = ValueWithDOI[float](
                df.loc[grid_id, (f"snowdens", f"({surf_idx},)")]
            )
        else:
            snowfrac = None
            snowpack = None
            icefrac = None
            snowwater = None
            snowdens = None

        # Temperature parameters
        temperature = ValueWithDOI[List[float]](
            [
                df.loc[grid_id, (f"temp_{str_type}", f"({surf_idx}, {i})")]
                for i in range(5)
            ]
        )

        # Exterior and interior surface temperature
        tsfc = ValueWithDOI[float](
            df.loc[grid_id, (f"tsfc_{str_type}", f"({surf_idx},)")]
        )
        tin = ValueWithDOI[float](
            df.loc[grid_id, (f"tin_{str_type}", f"({surf_idx},)")]
        )

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
    wu_total: ValueWithDOI[float] = Field(
        description="Total water use",
        default=ValueWithDOI(value=0.0),
        ge=0,
    )  # Default set to 0.0 means no irrigation.
    wu_auto: ValueWithDOI[float] = Field(
        description="Automatic water use",
        default=ValueWithDOI(value=0.0),
        ge=0,
    )
    wu_manual: ValueWithDOI[float] = Field(
        description="Manual water use",
        default=ValueWithDOI(value=0.0),
        ge=0,
    )

    ref: Optional[Reference] = None

    def to_df_state(self, veg_idx: int, grid_id: int) -> pd.DataFrame:
        """Convert water use to DataFrame state format."""
        df_state = init_df_state(grid_id)
        df_state.loc[grid_id, ("wuday_id", f"({veg_idx * 3 + 0},)")] = (
            self.wu_total.value
        )
        df_state.loc[grid_id, ("wuday_id", f"({veg_idx * 3 + 1},)")] = (
            self.wu_auto.value
        )
        df_state.loc[grid_id, ("wuday_id", f"({veg_idx * 3 + 2},)")] = (
            self.wu_manual.value
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
            wu_total=ValueWithDOI[float](wu_total),
            wu_auto=ValueWithDOI[float](wu_auto),
            wu_manual=ValueWithDOI[float](wu_manual),
        )


class InitialStatePaved(SurfaceInitialState):
    _surface_type: Literal[SurfaceType.PAVED] = SurfaceType.PAVED


class InitialStateBldgs(SurfaceInitialState):
    _surface_type: Literal[SurfaceType.BLDGS] = SurfaceType.BLDGS


class InitialStateVeg(SurfaceInitialState):
    """Base initial state parameters for vegetated surfaces"""

    alb_id: ValueWithDOI[float] = Field(
        description="Initial albedo for vegetated surfaces (depends on time of year).",
        default=ValueWithDOI(0.25),
    )
    lai_id: ValueWithDOI[float] = Field(
        description="Initial leaf area index (depends on time of year).",
        default=ValueWithDOI(1.0),
    )
    gdd_id: ValueWithDOI[float] = Field(
        description="Growing degree days  on day 1 of model run ID",
        default=ValueWithDOI(0),
    )  # We need to check this and give info for setting values.
    sdd_id: ValueWithDOI[float] = Field(
        description="Senescence degree days ID", default=ValueWithDOI(0)
    )  # This need to be consistent with GDD.
    wu: WaterUse = Field(default_factory=WaterUse)

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
        df_state[("alb", f"({surf_idx},)")] = self.alb_id.value
        # others are aligned with veg_idx
        df_state[("lai_id", f"({veg_idx},)")] = self.lai_id.value
        df_state[("gdd_id", f"({veg_idx},)")] = self.gdd_id.value
        df_state[("sdd_id", f"({veg_idx},)")] = self.sdd_id.value

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

        # Convert to ValueWithDOI
        alb_id = ValueWithDOI[float](alb_id)
        lai_id = ValueWithDOI[float](lai_id)
        gdd_id = ValueWithDOI[float](gdd_id)
        sdd_id = ValueWithDOI[float](sdd_id)

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
        df_state[("albevetr_id", "0")] = self.alb_id.value
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

    porosity_id: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.2), description="Initial porosity for deciduous trees"
    )
    decidcap_id: ValueWithDOI[float] = Field(
        default=ValueWithDOI(0.3),
        description="Initial deciduous capacity for deciduous trees",
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
        df_state[("porosity_id", "0")] = self.porosity_id.value
        df_state[("decidcap_id", "0")] = self.decidcap_id.value
        df_state[("albdectr_id", "0")] = self.alb_id.value

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

        # Convert to ValueWithDOI
        porosity_id = ValueWithDOI[float](porosity_id)
        decidcap_id = ValueWithDOI[float](decidcap_id)
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
        df_state[("albgrass_id", "0")] = self.alb_id.value
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


class InitialStates(BaseModel):
    """Initial conditions for the SUEWS model"""

    snowalb: ValueWithDOI[float] = Field(
        description="Initial snow albedo",
        default=ValueWithDOI(0.5),
        ge=0,
        le=1,
    )
    paved: InitialStatePaved = Field(default_factory=InitialStatePaved)
    bldgs: InitialStateBldgs = Field(default_factory=InitialStateBldgs)
    evetr: InitialStateEvetr = Field(default_factory=InitialStateEvetr)
    dectr: InitialStateDectr = Field(default_factory=InitialStateDectr)
    grass: InitialStateGrass = Field(default_factory=InitialStateGrass)
    bsoil: InitialStateBsoil = Field(default_factory=InitialStateBsoil)
    water: InitialStateWater = Field(default_factory=InitialStateWater)
    roofs: Optional[List[SurfaceInitialState]] = Field(
        default=[
            SurfaceInitialState(),
            SurfaceInitialState(),
            SurfaceInitialState(),
        ],
        description="Initial states for roof layers",
    )
    walls: Optional[List[SurfaceInitialState]] = Field(
        default=[
            SurfaceInitialState(),
            SurfaceInitialState(),
            SurfaceInitialState(),
        ],
        description="Initial states for wall layers",
    )

    dqndt: float = Field(default=0, description="Change in net radiation")
    dqnsdt: float = Field(default=0, description="Change in net shortwave radiation")
    dt_since_start: float = Field(default=0, description="Time since start")
    lenday_id: int = Field(default=0, description="Length of the day ID")
    qn_av: float = Field(default=0, description="Average net radiation")
    qn_s_av: float = Field(default=0, description="Average net shortwave radiation")
    tair_av: float = Field(default=0, description="Average air temperature")
    tmax_id: float = Field(default=0, description="Maximum temperature ID")
    tmin_id: float = Field(default=0, description="Minimum temperature ID")
    tstep_prev: float = Field(default=0, description="Previous time step")
    snowfallcum: float = Field(default=0, description="Cumulative snowfall")
    hdd_id: List[float] = Field(
        default=[0] * 12, description="Heating degree days ID"
    )

    def to_df_state(self, grid_id: int) -> pd.DataFrame:
        """Convert initial states to DataFrame state format."""
        df_state = init_df_state(grid_id)

        # Add snowalb
        df_state[("snowalb", "0")] = self.snowalb.value

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
        # special treatment for hdd_id
        for i, hdd in enumerate(self.hdd_id):
            df_state[(f"hdd_id", f"({i},)")] = hdd
        df_state = df_state.sort_index(axis=1)

        # Drop duplicate columns while preserving first occurrence
        df_state = df_state.loc[:, ~df_state.columns.duplicated(keep="first")]

        return df_state

    @classmethod
    def from_df_state(cls, df: pd.DataFrame, grid_id: int) -> "InitialStates":
        snowalb = df.loc[grid_id, ("snowalb", "0")]
        snowalb = ValueWithDOI[float](snowalb)

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
        hdd_id = [df.loc[grid_id, (f"hdd_id", f"({i},)")] for i in range(12)]

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
