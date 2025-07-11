from pydantic import ConfigDict, BaseModel, Field, model_validator
from pydantic_core import PydanticUndefined
from typing import Optional, Union
import pandas as pd
import warnings
from .type import RefValue, Reference, FlexibleRefValue
from .profile import HourlyProfile, WeeklyProfile, DayProfile
from .type import init_df_state
from .validation_utils import (
    warn_missing_params, 
    check_missing_params,
    validate_only_when_complete
)


class IrrigationParams(
    BaseModel
):  # TODO: May need to add RefValue to the profiles here
    h_maintain: Optional[FlexibleRefValue(float)] = Field(
        default=None,
        description="Water depth to maintain through irrigation",
        json_schema_extra={"unit": "mm", "display_name": "Maintain Height"},
    )
    faut: FlexibleRefValue(float) = Field(
        default=0.0,
        description="Fraction of automatic irrigation",
        json_schema_extra={
            "unit": "dimensionless",
            "display_name": "Automatic Fraction",
        },
    )
    ie_start: Optional[FlexibleRefValue(float)] = Field(
        default=None,
        description="Start time of irrigation",
        json_schema_extra={"unit": "hour", "display_name": "Irrigation Start Hour"},
    )
    ie_end: Optional[FlexibleRefValue(float)] = Field(
        default=None,
        description="End time of irrigation",
        json_schema_extra={"unit": "hour", "display_name": "Irrigation End Hour"},
    )
    internalwateruse_h: Optional[FlexibleRefValue(float)] = Field(
        default=None,
        description="Internal water use rate",
        json_schema_extra={
            "unit": "mm h^-1",
            "display_name": "Internal Water Use Rate",
        },
    )
    daywatper: WeeklyProfile = Field(
        default_factory=WeeklyProfile,
        json_schema_extra={"display_name": "Day Water Per"},
    )
    daywat: WeeklyProfile = Field(
        default_factory=WeeklyProfile, json_schema_extra={"display_name": "Day Water"}
    )
    wuprofa_24hr: HourlyProfile = Field(
        default_factory=HourlyProfile,
        json_schema_extra={"display_name": "Water Use Profile Automatic (24hr)"},
    )
    wuprofm_24hr: HourlyProfile = Field(
        default_factory=HourlyProfile,
        json_schema_extra={"display_name": "Water Use Profile Manual (24hr)"},
    )

    ref: Optional[Reference] = None

    def to_df_state(self, grid_id: int) -> pd.DataFrame:
        """
        Convert irrigation parameters to DataFrame state format.

        Args:
            grid_id: Grid ID for the DataFrame index

        Returns:
            pd.DataFrame: DataFrame containing irrigation parameters
        """

        df_state = init_df_state(grid_id)

        df_state.loc[grid_id, ("h_maintain", "0")] = (
            self.h_maintain.value
            if isinstance(self.h_maintain, RefValue)
            else self.h_maintain
            if self.h_maintain is not None
            else 0.0
        )
        df_state.loc[grid_id, ("faut", "0")] = (
            self.faut.value if isinstance(self.faut, RefValue) else self.faut
        )
        df_state.loc[grid_id, ("ie_start", "0")] = (
            self.ie_start.value
            if isinstance(self.ie_start, RefValue)
            else self.ie_start
            if self.ie_start is not None
            else 0.0
        )
        df_state.loc[grid_id, ("ie_end", "0")] = (
            self.ie_end.value
            if isinstance(self.ie_end, RefValue)
            else self.ie_end
            if self.ie_end is not None
            else 0.0
        )
        df_state.loc[grid_id, ("internalwateruse_h", "0")] = (
            self.internalwateruse_h.value
            if isinstance(self.internalwateruse_h, RefValue)
            else self.internalwateruse_h
            if self.internalwateruse_h is not None
            else 0.0
        )

        df_daywatper = self.daywatper.to_df_state(grid_id, "daywatper")
        df_daywat = self.daywat.to_df_state(grid_id, "daywat")

        df_state = df_state.combine_first(df_daywatper)
        df_state = df_state.combine_first(df_daywat)

        df_wuprofa_24hr = self.wuprofa_24hr.to_df_state(grid_id, "wuprofa_24hr")
        df_wuprofm_24hr = self.wuprofm_24hr.to_df_state(grid_id, "wuprofm_24hr")

        df_state = df_state.combine_first(df_wuprofa_24hr)
        df_state = df_state.combine_first(df_wuprofm_24hr)

        return df_state

    @classmethod
    def from_df_state(cls, df: pd.DataFrame, grid_id: int) -> "IrrigationParams":
        """
        Reconstruct IrrigationParams from a DataFrame state format.

        Args:
            df: DataFrame containing irrigation parameters
            grid_id: Grid ID for the DataFrame index

        Returns:
            IrrigationParams: Instance of IrrigationParams
        """
        # Extract scalar attributes
        h_maintain = df.loc[grid_id, ("h_maintain", "0")]
        faut = df.loc[grid_id, ("faut", "0")]
        ie_start = df.loc[grid_id, ("ie_start", "0")]
        ie_end = df.loc[grid_id, ("ie_end", "0")]
        internalwateruse_h = df.loc[grid_id, ("internalwateruse_h", "0")]

        # Conver to RefValue
        h_maintain = RefValue(h_maintain)
        faut = RefValue(faut)
        ie_start = RefValue(ie_start)
        ie_end = RefValue(ie_end)
        internalwateruse_h = RefValue(internalwateruse_h)

        # Extract WeeklyProfile attributes
        daywatper = WeeklyProfile.from_df_state(df, grid_id, "daywatper")
        daywat = WeeklyProfile.from_df_state(df, grid_id, "daywat")

        # Extract HourlyProfile attributes
        wuprofa_24hr = HourlyProfile.from_df_state(df, grid_id, "wuprofa_24hr")
        wuprofm_24hr = HourlyProfile.from_df_state(df, grid_id, "wuprofm_24hr")

        # Construct and return the IrrigationParams instance
        return cls(
            h_maintain=h_maintain,
            faut=faut,
            ie_start=ie_start,
            ie_end=ie_end,
            internalwateruse_h=internalwateruse_h,
            daywatper=daywatper,
            daywat=daywat,
            wuprofa_24hr=wuprofa_24hr,
            wuprofm_24hr=wuprofm_24hr,
        )


class AnthropogenicHeat(
    BaseModel
):  # TODO: May need to add the RefValue to the profiles here
    qf0_beu: DayProfile = Field(
        description="Base anthropogenic heat flux for buildings, equipment and urban metabolism",
        default_factory=DayProfile,
        json_schema_extra={"display_name": "Base Energy Use QF"},
    )
    qf_a: DayProfile = Field(
        description="Coefficient a for anthropogenic heat flux calculation",
        default_factory=DayProfile,
        json_schema_extra={"display_name": "QF Coefficient A"},
    )
    qf_b: DayProfile = Field(
        description="Coefficient b for anthropogenic heat flux calculation",
        default_factory=DayProfile,
        json_schema_extra={"display_name": "QF Coefficient B"},
    )
    qf_c: DayProfile = Field(
        description="Coefficient c for anthropogenic heat flux calculation",
        default_factory=DayProfile,
        json_schema_extra={"display_name": "QF Coefficient C"},
    )
    baset_cooling: DayProfile = Field(
        description="Base temperature for cooling degree days",
        default_factory=DayProfile,
        json_schema_extra={"display_name": "Base Temperature Cooling"},
    )
    baset_heating: DayProfile = Field(
        description="Base temperature for heating degree days",
        default_factory=DayProfile,
        json_schema_extra={"display_name": "Base Temperature Heating"},
    )
    ah_min: DayProfile = Field(
        description="Minimum anthropogenic heat flux",
        default_factory=DayProfile,
        json_schema_extra={"display_name": "Minimum Anthropogenic Heat"},
    )
    ah_slope_cooling: DayProfile = Field(
        description="Slope of anthropogenic heat vs cooling degree days",
        default_factory=DayProfile,
        json_schema_extra={"display_name": "AH Slope Cooling"},
    )
    ah_slope_heating: DayProfile = Field(
        description="Slope of anthropogenic heat vs heating degree days",
        default_factory=DayProfile,
        json_schema_extra={"display_name": "AH Slope Heating"},
    )
    ahprof_24hr: HourlyProfile = Field(
        description="24-hour profile of anthropogenic heat flux",
        default_factory=HourlyProfile,
        json_schema_extra={"display_name": "Anthropogenic Heat Profile (24hr)"},
    )
    popdensdaytime: DayProfile = Field(
        description="Daytime population density",
        default_factory=DayProfile,
        json_schema_extra={"display_name": "Daytime Population Density"},
    )
    popdensnighttime: float = Field(
        default=10.0,
        description="Nighttime population density",
        json_schema_extra={
            "unit": "people ha^-1",
            "display_name": "Nighttime Population Density",
        },
    )
    popprof_24hr: HourlyProfile = Field(
        description="24-hour profile of population density",
        default_factory=HourlyProfile,
        json_schema_extra={"display_name": "Population Profile (24hr)"},
    )

    ref: Optional[Reference] = None

    # DayProfile coulmns need to be fixed
    def to_df_state(self, grid_id: int) -> pd.DataFrame:
        """
        Convert anthropogenic heat parameters to DataFrame state format.

        Args:
            grid_id (int): Grid ID for the DataFrame index.

        Returns:
            pd.DataFrame: DataFrame containing anthropogenic heat parameters.
        """

        df_state = init_df_state(grid_id)

        day_profiles = {
            "qf0_beu": self.qf0_beu,
            "qf_a": self.qf_a,
            "qf_b": self.qf_b,
            "qf_c": self.qf_c,
            "baset_cooling": self.baset_cooling,
            "baset_heating": self.baset_heating,
            "ah_min": self.ah_min,
            "ah_slope_cooling": self.ah_slope_cooling,
            "ah_slope_heating": self.ah_slope_heating,
            "popdensdaytime": self.popdensdaytime,
        }
        for param_name, profile in day_profiles.items():
            df_day_profile = profile.to_df_state(grid_id, param_name)
            df_state = df_state.combine_first(df_day_profile)

        hourly_profiles = {
            "ahprof_24hr": self.ahprof_24hr,
            "popprof_24hr": self.popprof_24hr,
        }
        for param_name, profile in hourly_profiles.items():
            df_hourly_profile = profile.to_df_state(grid_id, param_name)
            df_state = df_state.combine_first(df_hourly_profile)

        df_state.loc[grid_id, ("popdensnighttime", "0")] = self.popdensnighttime

        return df_state

    @classmethod
    def from_df_state(cls, df: pd.DataFrame, grid_id: int) -> "AnthropogenicHeat":
        """
        Reconstruct AnthropogenicHeat from a DataFrame state format.

        Args:
            df (pd.DataFrame): DataFrame containing anthropogenic heat parameters.
            grid_id (int): Grid ID for the DataFrame index.

        Returns:
            AnthropogenicHeat: Instance of AnthropogenicHeat.
        """

        # Extract DayProfile attributes
        day_profiles = {
            "qf0_beu": DayProfile.from_df_state(df, grid_id, "qf0_beu"),
            "qf_a": DayProfile.from_df_state(df, grid_id, "qf_a"),
            "qf_b": DayProfile.from_df_state(df, grid_id, "qf_b"),
            "qf_c": DayProfile.from_df_state(df, grid_id, "qf_c"),
            "baset_cooling": DayProfile.from_df_state(df, grid_id, "baset_cooling"),
            "baset_heating": DayProfile.from_df_state(df, grid_id, "baset_heating"),
            "ah_min": DayProfile.from_df_state(df, grid_id, "ah_min"),
            "ah_slope_cooling": DayProfile.from_df_state(
                df, grid_id, "ah_slope_cooling"
            ),
            "ah_slope_heating": DayProfile.from_df_state(
                df, grid_id, "ah_slope_heating"
            ),
            "popdensdaytime": DayProfile.from_df_state(df, grid_id, "popdensdaytime"),
        }

        # Extract HourlyProfile attributes
        hourly_profiles = {
            "ahprof_24hr": HourlyProfile.from_df_state(df, grid_id, "ahprof_24hr"),
            "popprof_24hr": HourlyProfile.from_df_state(df, grid_id, "popprof_24hr"),
        }

        # Extract scalar attribute
        popdensnighttime = df.loc[grid_id, ("popdensnighttime", "0")]

        # Construct and return AnthropogenicHeat instance
        return cls(
            **day_profiles,
            **hourly_profiles,
            popdensnighttime=popdensnighttime,
        )


class CO2Params(BaseModel):  # TODO: May need to add the RefValue to the profiles here
    co2pointsource: Optional[FlexibleRefValue(float)] = Field(
        default=None,
        description="CO2 point source emission factor",
        json_schema_extra={"unit": "kg m^-2 s^-1", "display_name": "CO2 Point Source"},
    )
    ef_umolco2perj: Optional[FlexibleRefValue(float)] = Field(
        default=None,
        description="CO2 emission factor per unit of fuel energy",
        json_schema_extra={
            "unit": "umol J^-1",
            "display_name": "Emission Factor (umol CO2/J)",
        },
    )
    enef_v_jkm: Optional[FlexibleRefValue(float)] = Field(
        default=None,
        description="Vehicle energy consumption factor",
        json_schema_extra={
            "unit": "J km^-1",
            "display_name": "Energy Emission Factor Vehicles",
        },
    )
    fcef_v_kgkm: DayProfile = Field(
        description="Fuel consumption efficiency for vehicles",
        default_factory=DayProfile,
        json_schema_extra={"display_name": "Fuel Carbon Emission Factor Vehicles"},
    )
    frfossilfuel_heat: Optional[FlexibleRefValue(float)] = Field(
        default=None,
        description="Fraction of heating energy from fossil fuels",
        json_schema_extra={
            "unit": "dimensionless",
            "display_name": "Fossil Fuel Fraction Heating",
        },
    )
    frfossilfuel_nonheat: Optional[FlexibleRefValue(float)] = Field(
        default=None,
        description="Fraction of non-heating energy from fossil fuels",
        json_schema_extra={
            "unit": "dimensionless",
            "display_name": "Fossil Fuel Fraction Non-Heating",
        },
    )
    maxfcmetab: Optional[FlexibleRefValue(float)] = Field(
        default=None,
        description="Maximum metabolic CO2 flux rate",
        json_schema_extra={
            "unit": "umol m^-2 s^-1",
            "display_name": "Maximum Metabolic CO2 Flux",
        },
    )
    maxqfmetab: Optional[FlexibleRefValue(float)] = Field(
        default=None,
        description="Maximum metabolic heat flux rate",
        json_schema_extra={
            "unit": "W m^-2",
            "display_name": "Maximum Metabolic Heat Flux",
        },
    )
    minfcmetab: Optional[FlexibleRefValue(float)] = Field(
        default=None,
        description="Minimum metabolic CO2 flux rate",
        json_schema_extra={
            "unit": "umol m^-2 s^-1",
            "display_name": "Minimum Metabolic CO2 Flux",
        },
    )
    minqfmetab: Optional[FlexibleRefValue(float)] = Field(
        default=None,
        description="Minimum metabolic heat flux rate",
        json_schema_extra={
            "unit": "W m^-2",
            "display_name": "Minimum Metabolic Heat Flux",
        },
    )
    trafficrate: DayProfile = Field(
        description="Traffic rate",
        default_factory=DayProfile,
        json_schema_extra={"display_name": "Traffic Rate"},
    )
    trafficunits: Optional[FlexibleRefValue(float)] = Field(
        default=None,
        description="Units for traffic density normalisation",
        json_schema_extra={"unit": "vehicle km ha^-1", "display_name": "Traffic Units"},
    )
    traffprof_24hr: HourlyProfile = Field(
        description="24-hour profile of traffic rate",
        default_factory=HourlyProfile,
        json_schema_extra={"display_name": "Traffic Profile (24hr)"},
    )
    humactivity_24hr: HourlyProfile = Field(
        description="24-hour profile of human activity",
        default_factory=HourlyProfile,
        json_schema_extra={"display_name": "Human Activity Profile (24hr)"},
    )

    ref: Optional[Reference] = None

    # DayProfile coulmns need to be fixed
    def to_df_state(self, grid_id: int) -> pd.DataFrame:
        """
        Convert CO2 parameters to DataFrame state format.

        Args:
            grid_id (int): Grid ID for the DataFrame index.

        Returns:
            pd.DataFrame: DataFrame containing CO2 parameters.
        """

        df_state = init_df_state(grid_id)

        scalar_params = {
            "co2pointsource": self.co2pointsource.value
            if isinstance(self.co2pointsource, RefValue)
            else self.co2pointsource
            if self.co2pointsource is not None
            else 0.0,
            "ef_umolco2perj": self.ef_umolco2perj.value
            if isinstance(self.ef_umolco2perj, RefValue)
            else self.ef_umolco2perj
            if self.ef_umolco2perj is not None
            else 0.0,
            "enef_v_jkm": self.enef_v_jkm.value
            if isinstance(self.enef_v_jkm, RefValue)
            else self.enef_v_jkm
            if self.enef_v_jkm is not None
            else 0.0,
            "frfossilfuel_heat": self.frfossilfuel_heat.value
            if isinstance(self.frfossilfuel_heat, RefValue)
            else self.frfossilfuel_heat
            if self.frfossilfuel_heat is not None
            else 0.0,
            "frfossilfuel_nonheat": self.frfossilfuel_nonheat.value
            if isinstance(self.frfossilfuel_nonheat, RefValue)
            else self.frfossilfuel_nonheat
            if self.frfossilfuel_nonheat is not None
            else 0.0,
            "maxfcmetab": self.maxfcmetab.value
            if isinstance(self.maxfcmetab, RefValue)
            else self.maxfcmetab
            if self.maxfcmetab is not None
            else 0.0,
            "maxqfmetab": self.maxqfmetab.value
            if isinstance(self.maxqfmetab, RefValue)
            else self.maxqfmetab
            if self.maxqfmetab is not None
            else 0.0,
            "minfcmetab": self.minfcmetab.value
            if isinstance(self.minfcmetab, RefValue)
            else self.minfcmetab
            if self.minfcmetab is not None
            else 0.0,
            "minqfmetab": self.minqfmetab.value
            if isinstance(self.minqfmetab, RefValue)
            else self.minqfmetab
            if self.minqfmetab is not None
            else 0.0,
            "trafficunits": self.trafficunits.value
            if isinstance(self.trafficunits, RefValue)
            else self.trafficunits
            if self.trafficunits is not None
            else 0.0,
        }
        for param_name, value in scalar_params.items():
            df_state.loc[grid_id, (param_name, "0")] = value

        day_profiles = {
            "fcef_v_kgkm": self.fcef_v_kgkm,
            "trafficrate": self.trafficrate,
        }
        for param_name, profile in day_profiles.items():
            df_day_profile = profile.to_df_state(grid_id, param_name)
            df_state = df_state.combine_first(df_day_profile)

        hourly_profiles = {
            "traffprof_24hr": self.traffprof_24hr,
            "humactivity_24hr": self.humactivity_24hr,
        }
        for param_name, profile in hourly_profiles.items():
            df_hourly_profile = profile.to_df_state(grid_id, param_name)
            df_state = df_state.combine_first(df_hourly_profile)

        return df_state

    @classmethod
    def from_df_state(cls, df: pd.DataFrame, grid_id: int) -> "CO2Params":
        """
        Reconstruct CO2Params from a DataFrame state format.

        Args:
            df (pd.DataFrame): DataFrame containing CO2 parameters.
            grid_id (int): Grid ID for the DataFrame index.

        Returns:
            CO2Params: Instance of CO2Params.
        """

        # Extract scalar attributes
        scalar_params = {
            "co2pointsource": df.loc[grid_id, ("co2pointsource", "0")],
            "ef_umolco2perj": df.loc[grid_id, ("ef_umolco2perj", "0")],
            "enef_v_jkm": df.loc[grid_id, ("enef_v_jkm", "0")],
            "frfossilfuel_heat": df.loc[grid_id, ("frfossilfuel_heat", "0")],
            "frfossilfuel_nonheat": df.loc[grid_id, ("frfossilfuel_nonheat", "0")],
            "maxfcmetab": df.loc[grid_id, ("maxfcmetab", "0")],
            "maxqfmetab": df.loc[grid_id, ("maxqfmetab", "0")],
            "minfcmetab": df.loc[grid_id, ("minfcmetab", "0")],
            "minqfmetab": df.loc[grid_id, ("minqfmetab", "0")],
            "trafficunits": df.loc[grid_id, ("trafficunits", "0")],
        }

        # Convert scalar attributes to RefValue
        scalar_params = {key: RefValue(value) for key, value in scalar_params.items()}

        # Extract DayProfile attributes
        day_profiles = {
            "fcef_v_kgkm": DayProfile.from_df_state(df, grid_id, "fcef_v_kgkm"),
            "trafficrate": DayProfile.from_df_state(df, grid_id, "trafficrate"),
        }

        # Extract HourlyProfile attributes
        hourly_profiles = {
            "traffprof_24hr": HourlyProfile.from_df_state(
                df, grid_id, "traffprof_24hr"
            ),
            "humactivity_24hr": HourlyProfile.from_df_state(
                df, grid_id, "humactivity_24hr"
            ),
        }

        # Construct and return CO2Params instance
        return cls(
            **scalar_params,
            **day_profiles,
            **hourly_profiles,
        )


class AnthropogenicEmissions(BaseModel):
    startdls: Optional[FlexibleRefValue(float)] = Field(
        default=None,
        description="Start of daylight savings time in decimal day of year",
        json_schema_extra={"unit": "day", "display_name": "Daylight Saving Start"},
    )
    enddls: Optional[FlexibleRefValue(float)] = Field(
        default=None,
        description="End of daylight savings time in decimal day of year",
        json_schema_extra={"unit": "day", "display_name": "Daylight Saving End"},
    )
    heat: AnthropogenicHeat = Field(
        description="Anthropogenic heat emission parameters",
        default_factory=AnthropogenicHeat,
        json_schema_extra={"display_name": "Heat"},
    )
    co2: CO2Params = Field(
        description="CO2 emission parameters",
        default_factory=CO2Params,
        json_schema_extra={"display_name": "CO2"},
    )

    ref: Optional[Reference] = None

    def to_df_state(self, grid_id: int) -> pd.DataFrame:
        """
        Convert anthropogenic emissions parameters to DataFrame state format.

        Args:
            grid_id (int): Grid ID for the DataFrame index.

        Returns:
            pd.DataFrame: DataFrame containing anthropogenic emissions parameters.
        """
        df_state = init_df_state(grid_id)

        # Set start and end daylight saving times
        df_state.loc[grid_id, ("startdls", "0")] = (
            self.startdls.value
            if isinstance(self.startdls, RefValue)
            else self.startdls
            if self.startdls is not None
            else 0.0
        )
        df_state.loc[grid_id, ("enddls", "0")] = (
            self.enddls.value
            if isinstance(self.enddls, RefValue)
            else self.enddls
            if self.enddls is not None
            else 0.0
        )

        # Add heat parameters
        df_heat = self.heat.to_df_state(grid_id)
        df_state = pd.concat([df_state, df_heat], axis=1)

        # Add CO2 parameters
        df_co2 = self.co2.to_df_state(grid_id)
        df_state = pd.concat([df_state, df_co2], axis=1)

        # Drop duplicate columns if necessary
        df_state = df_state.loc[:, ~df_state.columns.duplicated()]

        return df_state

    @classmethod
    def from_df_state(cls, df: pd.DataFrame, grid_id: int) -> "AnthropogenicEmissions":
        """
        Reconstruct AnthropogenicEmissions from a DataFrame state format.

        Args:
            df (pd.DataFrame): DataFrame containing anthropogenic emissions parameters.
            grid_id (int): Grid ID for the DataFrame index.

        Returns:
            AnthropogenicEmissions: Instance of AnthropogenicEmissions.
        """
        startdls = RefValue(df.loc[grid_id, ("startdls", "0")])
        enddls = RefValue(df.loc[grid_id, ("enddls", "0")])

        # Reconstruct heat parameters
        heat = AnthropogenicHeat.from_df_state(df, grid_id)

        # Reconstruct CO2 parameters
        co2 = CO2Params.from_df_state(df, grid_id)

        return cls(startdls=startdls, enddls=enddls, heat=heat, co2=co2)
