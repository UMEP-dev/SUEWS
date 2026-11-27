"""Profile management system for SUEWS conversion.

This module provides a robust profile management system that:
1. Validates profile references during conversion
2. Auto-generates missing profiles with sensible defaults
3. Provides clear error messages about missing profiles
4. Maintains a registry of available profiles
"""

from collections.abc import Sequence
import logging
from pathlib import Path
from typing import ClassVar

import pandas as pd

logger = logging.getLogger(__name__)


class ProfileManager:
    """Manages SUEWS profiles during conversion."""

    # Default profile codes for common cases
    DEFAULT_PROFILES: ClassVar[dict[str, int]] = {
        "DEFAULT": 999,  # Generic default
        "NO_ACTIVITY": 0,  # No activity profile
        "UNIFORM": 1,  # Uniform distribution
        "PLACEHOLDER": 999,  # Placeholder for missing data
    }

    # Profile types that need to exist in SUEWS_Profiles.txt
    PROFILE_TYPES: ClassVar[list[str]] = [
        "EnergyUseProfWD",
        "EnergyUseProfWE",
        "ActivityProfWD",
        "ActivityProfWE",
        "TraffProfWD",
        "TraffProfWE",
        "PopProfWD",
        "PopProfWE",
        "WaterUseProfAutoWD",
        "WaterUseProfAutoWE",
        "WaterUseProfManuWD",
        "WaterUseProfManuWE",
        "SnowClearingProfWD",
        "SnowClearingProfWE",
    ]

    def __init__(self, profiles_path: Path | None = None):
        """Initialize profile manager.

        Args:
            profiles_path: Path to SUEWS_Profiles.txt file
        """
        self.profiles_path = profiles_path
        self.available_profiles: set[int] = set()
        self.profile_data: pd.DataFrame | None = None
        self.missing_profiles: set[int] = set()
        self._load_error: Exception | None = None

        if profiles_path and profiles_path.exists():
            self.load_profiles()

    def load_profiles(self):
        """Load existing profiles from SUEWS_Profiles.txt."""
        self._load_error = None
        try:
            # Try reading with first row as header
            self.profile_data = pd.read_csv(
                self.profiles_path, sep=r"\s+", comment="!", header=0, index_col=0
            )
            first_index = (
                str(self.profile_data.index[0]).strip()
                if not self.profile_data.index.empty
                else ""
            )
            is_legacy_header = not first_index.lstrip("-").isdigit()

            if is_legacy_header:
                logger.info("Detected legacy profile format, retrying with skiprows=1")
                self.profile_data = pd.read_csv(
                    self.profiles_path,
                    sep=r"\s+",
                    comment="!",
                    header=0,
                    index_col=0,
                    skiprows=1,
                )

            # Convert index to integers to ensure type consistency
            self.profile_data.index = self.profile_data.index.astype(int)

            self.available_profiles = set(self.profile_data.index)
            logger.info(f"Loaded {len(self.available_profiles)} profiles")
        except Exception as e:
            logger.warning(f"Could not load profiles: {e}")
            self._load_error = e
            self.profile_data = None
            self.available_profiles = set()

    def validate_profile_code(
        self, code: int, field_name: str = ""
    ) -> tuple[bool, int | None]:
        """Validate a profile code and return replacement if needed.

        Args:
            code: Profile code to validate
            field_name: Name of field using this code (for logging)

        Returns:
            Tuple of (is_valid, replacement_code)
        """
        # Handle special codes
        if code in [-999, -9]:
            return True, code

        # Check if profile exists
        if code in self.available_profiles:
            return True, code

        # Track missing profile
        self.missing_profiles.add(code)

        # Log warning
        replacement_code = self.DEFAULT_PROFILES["DEFAULT"]
        logger.warning(
            f"Profile code {code} not found for field '{field_name}'. "
            f"Using default profile {replacement_code}"
        )

        # Also track the replacement code if it doesn't exist
        if replacement_code not in self.available_profiles:
            self.missing_profiles.add(replacement_code)

        # Return default replacement
        return False, replacement_code

    def create_default_profile(
        self,
        code: int,
        columns: pd.Index | Sequence[object] | None = None,
    ) -> pd.Series:
        """Create a default profile with uniform distribution.

        Parameters
        ----------
        code : int
            Profile code to create.
        columns : pd.Index | Sequence[object] | None, optional
            Target DataFrame column labels to align the 24 hourly values with.
            When ``None``, uses integer hour labels ``range(24)``.

        Returns
        -------
        pd.Series
            Profile data as a pandas Series with 24 uniform ``1.0`` entries.
        """
        # Create uniform distribution (same value for all 24 hours)
        if columns is None:
            index: pd.Index | range = range(24)
        else:
            target_cols = pd.Index(columns)
            if len(target_cols) < 24:
                raise ValueError(
                    f"Expected at least 24 profile columns, got {len(target_cols)}"
                )
            index = target_cols[:24]
        return pd.Series([1.0] * 24, index=index, name=code, dtype=float)

    def _prepare_profiles_df(self, action_desc: str) -> pd.DataFrame:
        """Return a validated working copy of profile data or an empty 24-hour frame."""
        if self.profile_data is None:
            if self._load_error is not None:
                raise ValueError(
                    f"Refusing to {action_desc} because loading "
                    f"{self.profiles_path} failed: {self._load_error}"
                ) from self._load_error
            return pd.DataFrame(columns=[str(i) for i in range(24)], dtype=float)

        n_cols = len(self.profile_data.columns)
        n_rows = len(self.profile_data.index)
        if n_cols == 0 and n_rows == 0:
            if self.profiles_path is not None and self.profiles_path.exists():
                raise ValueError("Expected at least 24 profile columns, got 0")
            return pd.DataFrame(columns=[str(i) for i in range(24)], dtype=float)

        if n_cols < 24:
            raise ValueError(f"Expected at least 24 profile columns, got {n_cols}")

        return self.profile_data.copy()

    def ensure_required_profiles(self, output_path: Path):
        """Ensure all required profiles exist, creating defaults if needed.

        Args:
            output_path: Directory to write SUEWS_Profiles.txt
        """
        profiles_file = output_path / "SUEWS_Profiles.txt"

        # Validate existing profile table or initialise a 24-hour DataFrame before writing
        profiles_df = self._prepare_profiles_df(f"write {profiles_file}")

        # Add default profiles if they don't exist
        for name, code in self.DEFAULT_PROFILES.items():
            if code not in profiles_df.index and code > 0:
                profiles_df.loc[code] = self.create_default_profile(
                    code, columns=profiles_df.columns
                )
                logger.info(f"Created default profile {code} ({name})")

        # Add any other missing profiles that were referenced
        for code in sorted(self.missing_profiles):
            if code not in profiles_df.index and code > 0:
                profiles_df.loc[code] = self.create_default_profile(
                    code, columns=profiles_df.columns
                )
                logger.info(f"Created placeholder profile {code}")

        # Sort by index
        profiles_df = profiles_df.sort_index()

        # Write to file with proper header (matching legacy column naming 0-23)
        with open(profiles_file, "w", encoding="utf-8") as f:
            # Write first line: column numbers (load_SUEWS_table skips this line)
            f.write("    ".join([str(i) for i in range(1, 26)]) + "\n")

            # Write second line: header with Code and hour columns
            f.write("Code    " + "    ".join([str(i) for i in range(24)]) + "\n")

            # Write profiles
            for code, row in profiles_df.iterrows():
                values = "    ".join([f"{v:.3f}" for v in row.iloc[:24]])
                f.write(f"{code:4d}    {values}\n")

        logger.info(f"Wrote {len(profiles_df)} profiles to {profiles_file}")

        # Update available profiles
        self.available_profiles = set(profiles_df.index)
        self.profile_data = profiles_df

    def get_profile_summary(self) -> dict:
        """Get summary of profile status.

        Returns:
            Dictionary with profile statistics
        """
        return {
            "available_profiles": len(self.available_profiles),
            "missing_profiles": len(self.missing_profiles),
            "missing_codes": sorted(self.missing_profiles),
            "default_profiles": self.DEFAULT_PROFILES,
        }

    def process_conversion_rules(self, rules_df: pd.DataFrame) -> pd.DataFrame:
        """Process conversion rules and fix profile references.

        Args:
            rules_df: DataFrame of conversion rules

        Returns:
            Updated rules DataFrame with validated profile codes
        """
        profile_fields = list(self.PROFILE_TYPES)

        for idx, row in rules_df.iterrows():
            # Check if this rule involves a profile field
            if row["Variable"] in profile_fields and row["Action"] == "Add":
                try:
                    code = int(row["Value"])
                    is_valid, replacement = self.validate_profile_code(
                        code, row["Variable"]
                    )
                    if not is_valid:
                        rules_df.at[idx, "Value"] = str(replacement)
                        logger.debug(
                            f"Replaced profile {code} with {replacement} "
                            f"for {row['Variable']}"
                        )
                except (ValueError, TypeError):
                    # Value is not a simple integer code
                    pass

        return rules_df


def validate_and_fix_profiles(
    conversion_dir: Path, rules_path: Path, debug: bool = False
) -> ProfileManager:
    """Validate and fix profile references during conversion.

    Args:
        conversion_dir: Directory containing conversion files
        rules_path: Path to rules.csv file
        debug: Enable debug logging

    Returns:
        ProfileManager instance with validation results
    """
    if debug:
        logging.basicConfig(level=logging.DEBUG)

    # Initialize profile manager
    profiles_path = conversion_dir / "SUEWS_Profiles.txt"
    manager = ProfileManager(profiles_path)

    # Validate load status and column shape before modifying rules.csv or SUEWS_Profiles.txt
    manager._prepare_profiles_df("update profiles or rules")

    # Load and process conversion rules
    if rules_path.exists():
        rules_df = pd.read_csv(rules_path)
        rules_df = manager.process_conversion_rules(rules_df)

        # Save updated rules if changes were made
        if manager.missing_profiles:
            backup_path = rules_path.with_suffix(".csv.bak")
            rules_path.rename(backup_path)
            rules_df.to_csv(rules_path, index=False)
            logger.info(f"Updated rules saved to {rules_path}")
            logger.info(f"Original rules backed up to {backup_path}")

    # Ensure all required profiles exist
    manager.ensure_required_profiles(conversion_dir)

    # Report summary
    summary = manager.get_profile_summary()
    if summary["missing_profiles"] > 0:
        logger.warning(
            f"Found {summary['missing_profiles']} missing profiles: "
            f"{summary['missing_codes']}"
        )

    return manager
