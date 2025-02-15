from typing import Optional
import pandas as pd
from pydantic import BaseModel, Field
from .type import ValueWithDOI, Reference
from .type import init_df_state


class OHMCoefficients(BaseModel):
    a1: ValueWithDOI[float] = Field(
        description="OHM coefficient a1 for different seasons and wetness conditions",
        default=ValueWithDOI(0.0),
    )
    a2: ValueWithDOI[float] = Field(
        description="OHM coefficient a2 for different seasons and wetness conditions",
        default=ValueWithDOI(0.0),
    )
    a3: ValueWithDOI[float] = Field(
        description="OHM coefficient a3 for different seasons and wetness conditions",
        default=ValueWithDOI(0.0),
    )

    ref: Optional[Reference] = None

    def to_df_state(self, grid_id: int, surf_idx: int, idx_s) -> pd.DataFrame:
        """Convert OHM coefficients to DataFrame state format.

        Args:
            grid_id (int): Grid ID
            surf_idx (int): Surface index

        Returns:
            pd.DataFrame: DataFrame containing OHM coefficients with MultiIndex columns
        """
        df_state = init_df_state(grid_id)

        # Map season/wetness combinations to indices
        a_map = {
            "a1": 0,
            "a2": 1,
            "a3": 2,
        }

        # Set values for each season/wetness combination
        for aX, idx_a in a_map.items():
            str_idx = f"({surf_idx}, {idx_s}, {idx_a})"
            df_state.loc[grid_id, ("ohm_coef", str_idx)] = getattr(self, aX).value

        return df_state

    @classmethod
    def from_df_state(
        cls, df: pd.DataFrame, grid_id: int, surf_idx: int, idx_s: int
    ) -> "OHMCoefficients":
        """
        Reconstruct OHMCoefficients from DataFrame state format.

        Args:
            df (pd.DataFrame): DataFrame containing OHM coefficients.
            grid_id (int): Grid ID.
            surf_idx (int): Surface index.

        Returns:
            OHMCoefficients: Reconstructed instance.
        """
        # Map coefficient to indices
        a_map = {"a1": 0, "a2": 1, "a3": 2}

        # Extract values for each season/wetness combination
        params = {
            aX: df.loc[grid_id, ("ohm_coef", f"({surf_idx}, {idx_s}, {idx})")]
            for aX, idx in a_map.items()
        }

        # Convert to ValueWithDOI
        params = {key: ValueWithDOI(value) for key, value in params.items()}

        return cls(**params)


class OHM_Coefficient_season_wetness(BaseModel):
    summer_dry: OHMCoefficients = Field(
        description="OHM coefficient for summer dry conditions",
        default_factory=OHMCoefficients,
    )
    summer_wet: OHMCoefficients = Field(
        description="OHM coefficient for summer wet conditions",
        default_factory=OHMCoefficients,
    )
    winter_dry: OHMCoefficients = Field(
        description="OHM coefficient for winter dry conditions",
        default_factory=OHMCoefficients,
    )
    winter_wet: OHMCoefficients = Field(
        description="OHM coefficient for winter wet conditions",
        default_factory=OHMCoefficients,
    )

    ref: Optional[Reference] = None

    def to_df_state(self, grid_id: int, surf_idx: int) -> pd.DataFrame:
        """Convert OHM coefficients to DataFrame state format.

        Args:
            grid_id (int): Grid ID
            surf_idx (int): Surface index
            idx_a (int): Index for coefficient (0=a1, 1=a2, 2=a3)

        Returns:
            pd.DataFrame: DataFrame containing OHM coefficients with MultiIndex columns
        """
        df_state = init_df_state(grid_id)

        # Convert each coefficient
        for idx_s, coef in enumerate(
            [self.summer_wet, self.summer_dry, self.winter_wet, self.winter_dry]
        ):
            df_coef = coef.to_df_state(grid_id, surf_idx, idx_s)
            df_coef_extra = coef.to_df_state(
                grid_id, 7, idx_s
            )  # always include this extra row to conform to SUEWS convention
            df_state = pd.concat([df_state, df_coef, df_coef_extra], axis=1)

        # drop duplicate columns
        df_state = df_state.loc[:, ~df_state.columns.duplicated()]

        return df_state

    @classmethod
    def from_df_state(
        cls, df: pd.DataFrame, grid_id: int, surf_idx: int
    ) -> "OHM_Coefficient_season_wetness":
        """
        Reconstruct OHM_Coefficient_season_wetness from DataFrame state format.

        Args:
            df (pd.DataFrame): DataFrame containing OHM coefficients.
            grid_id (int): Grid ID.
            surf_idx (int): Surface index.
            idx_a (int): Index for coefficient (0=a1, 1=a2, 2=a3).

        Returns:
            OHM_Coefficient_season_wetness: Reconstructed instance.
        """

        summer_dry = OHMCoefficients.from_df_state(df, grid_id, surf_idx, 1)
        summer_wet = OHMCoefficients.from_df_state(df, grid_id, surf_idx, 0)
        winter_dry = OHMCoefficients.from_df_state(df, grid_id, surf_idx, 3)
        winter_wet = OHMCoefficients.from_df_state(df, grid_id, surf_idx, 2)

        return cls(
            summer_dry=summer_dry,
            summer_wet=summer_wet,
            winter_dry=winter_dry,
            winter_wet=winter_wet,
        )
