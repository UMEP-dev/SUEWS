from pydantic import ConfigDict, BaseModel, Field, field_validator, model_validator
from typing import Optional, Dict
import pandas as pd
from .type import Reference
from .type import init_df_state


class DayProfile(BaseModel):
    working_day: float = Field(
        default=1.0, json_schema_extra={"display_name": "Working Day"}
    )
    holiday: float = Field(default=0.0, json_schema_extra={"display_name": "Holiday"})

    ref: Optional[Reference] = None

    def to_df_state(self, grid_id: int, param_name: str) -> pd.DataFrame:
        """
        Convert day profile to DataFrame state format.

        Args:
            grid_id (int): Grid ID for the DataFrame index.
            param_name (str): Name of the parameter this profile belongs to.

        Returns:
            pd.DataFrame: DataFrame containing day profile parameters.
        """

        df_state = init_df_state(grid_id)

        day_map = {
            "working_day": 0,
            "holiday": 1,
        }

        for day, idx in day_map.items():
            df_state.loc[grid_id, (param_name, f"({idx},)")] = getattr(self, day)

        return df_state

    @classmethod
    def from_df_state(
        cls, df: pd.DataFrame, grid_id: int, param_name: str
    ) -> "DayProfile":
        """
        Reconstruct DayProfile from a DataFrame state format.

        Args:
            df (pd.DataFrame): DataFrame containing day profile parameters.
            grid_id (int): Grid ID for the DataFrame index.
            param_name (str): Name of the parameter this profile belongs to.

        Returns:
            DayProfile: Instance of DayProfile.
        """

        day_map = {
            "working_day": 0,
            "holiday": 1,
        }

        # Extract values for working day and holiday from the DataFrame
        params = {}
        for day, idx in day_map.items():
            col = (param_name, f"({idx},)")
            if col in df.columns:
                params[day] = df.loc[grid_id, col]
            else:
                raise KeyError(f"Column {col} not found in DataFrame")

        return cls(**params)


class WeeklyProfile(BaseModel):
    monday: float = 0.0
    tuesday: float = 0.0
    wednesday: float = 0.0
    thursday: float = 0.0
    friday: float = 0.0
    saturday: float = 0.0
    sunday: float = 0.0

    ref: Optional[Reference] = None

    def to_df_state(self, grid_id: int, param_name: str) -> pd.DataFrame:
        """Convert weekly profile to DataFrame state format.

        Args:
            grid_id: Grid ID for the DataFrame index
            param_name: Name of the parameter this profile belongs to

        Returns:
            pd.DataFrame: DataFrame containing weekly profile parameters
        """
        df_state = init_df_state(grid_id)

        # Map days to their index
        day_map = {
            "monday": 0,
            "tuesday": 1,
            "wednesday": 2,
            "thursday": 3,
            "friday": 4,
            "saturday": 5,
            "sunday": 6,
        }

        for day, idx in day_map.items():
            df_state[(param_name, f"({idx},)")] = getattr(self, day)

        return df_state

    @classmethod
    def from_df_state(
        cls, df: pd.DataFrame, grid_id: int, param_name: str
    ) -> "WeeklyProfile":
        """Reconstruct WeeklyProfile from a DataFrame state format.

        Args:
            df: DataFrame containing weekly profile parameters
            grid_id: Grid ID for the DataFrame index
            param_name: Name of the parameter to extract values from

        Returns:
            WeeklyProfile: Instance of WeeklyProfile
        """
        # Map days to their index
        day_map = {
            "monday": 0,
            "tuesday": 1,
            "wednesday": 2,
            "thursday": 3,
            "friday": 4,
            "saturday": 5,
            "sunday": 6,
        }

        # Extract values from DataFrame for each day
        params = {
            day: df.loc[grid_id, (param_name, f"({idx},)")]
            for day, idx in day_map.items()
        }

        # Create an instance of WeeklyProfile
        return cls(**params)


class HourlyProfile(BaseModel):
    working_day: Dict[str, float]
    holiday: Dict[str, float]

    ref: Optional[Reference] = None

    @classmethod
    def __init_default_values__(cls) -> Dict[str, Dict[str, float]]:
        """Generate default values for hourly profiles.

        Returns:
            Dict containing default working_day and holiday profiles with uniform distribution
        """
        # Create uniform distribution (1/24) for each hour
        uniform_value = 1.0 / 24.0

        # Generate hour keys 1-24 with uniform values
        hourly_values = {str(hour): uniform_value for hour in range(1, 25)}

        return {"working_day": hourly_values.copy(), "holiday": hourly_values.copy()}

    def __init__(self, **data):
        # If no values provided, use defaults
        if not data:
            defaults = self.__init_default_values__()
            data = defaults
        super().__init__(**data)

    @field_validator("working_day", "holiday", mode="before")
    def convert_keys_to_str(cls, v: Dict) -> Dict[str, float]:
        if isinstance(v, dict):
            return {str(k): float(v) for k, v in v.items()}
        return v

    @model_validator(mode="after")
    def validate_hours(self) -> "HourlyProfile":
        for profile in [self.working_day, self.holiday]:
            hours = [int(h) for h in profile.keys()]
            if not all(1 <= h <= 24 for h in hours):
                raise ValueError("Hour values must be between 1 and 24")
            if sorted(hours) != list(range(1, 25)):
                error_message = ValueError("Must have all hours from 1 to 24")
                raise ValueError("Must have all hours from 1 to 24")
        return self

    def to_df_state(self, grid_id: int, param_name: str) -> pd.DataFrame:
        """Convert hourly profile to DataFrame state format.

        Args:
            grid_id: Grid ID for the DataFrame index
            param_name: Name of the parameter this profile belongs to

        Returns:
            pd.DataFrame: DataFrame containing hourly profile parameters
        """
        df_state = init_df_state(grid_id)

        # Set working day values (index 0)
        for hour, value in self.working_day.items():
            df_state[(param_name, f"({int(hour) - 1}, 0)")] = value

        # Set holiday/weekend values (index 1)
        for hour, value in self.holiday.items():
            df_state[(param_name, f"({int(hour) - 1}, 1)")] = value

        return df_state

    @classmethod
    def from_df_state(
        cls, df: pd.DataFrame, grid_id: int, param_name: str
    ) -> "HourlyProfile":
        """Reconstruct HourlyProfile from a DataFrame state format.

        Args:
            df: DataFrame containing hourly profile parameters
            grid_id: Grid ID for the DataFrame index
            param_name: Name of the parameter to extract values from

        Returns:
            HourlyProfile: Instance of HourlyProfile
        """
        # Extract working day values (index 0)
        working_day = {
            str(hour + 1): df.loc[grid_id, (param_name, f"({hour}, 0)")]
            for hour in range(24)
        }

        # Extract holiday values (index 1)
        holiday = {
            str(hour + 1): df.loc[grid_id, (param_name, f"({hour}, 1)")]
            for hour in range(24)
        }

        # Create an instance of HourlyProfile
        return cls(working_day=working_day, holiday=holiday)
