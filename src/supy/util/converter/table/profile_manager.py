"""Profile management system for SUEWS conversion.

This module provides a robust profile management system that:
1. Validates profile references during conversion
2. Auto-generates missing profiles with sensible defaults
3. Provides clear error messages about missing profiles
4. Maintains a registry of available profiles
"""

from collections.abc import Iterable, Sequence
import logging
from pathlib import Path
from typing import ClassVar

import numpy as np
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
        "DAY_NIGHT_DEFAULT": 99901,  # Uniform 1.0 (night level) on [1.0, 2.0] scale (#2043)
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

            # Drop legacy Fortran EOF footer rows (-9 sentinel with no hourly values)
            footer_mask = (
                self.profile_data.index == -9
            ) & self.profile_data.isna().all(axis=1)
            if footer_mask.any():
                self.profile_data = self.profile_data.loc[~footer_mask]

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
            if self.profiles_path is not None:
                raise FileNotFoundError(
                    f"Profile table not found at {self.profiles_path}"
                )
            return pd.DataFrame(columns=[str(i) for i in range(24)], dtype=float)

        n_cols = len(self.profile_data.columns)
        n_rows = len(self.profile_data.index)
        if n_cols == 0 and n_rows == 0:
            if self.profiles_path is not None and self.profiles_path.exists():
                raise ValueError("Expected at least 24 profile columns, got 0")
            return pd.DataFrame(columns=[str(i) for i in range(24)], dtype=float)

        if n_cols < 24:
            raise ValueError(f"Expected at least 24 profile columns, got {n_cols}")

        profiles_df = self.profile_data.copy()
        footer_mask = (profiles_df.index == -9) & profiles_df.iloc[:, :24].isna().all(
            axis=1
        )
        if footer_mask.any():
            profiles_df = profiles_df.loc[~footer_mask]

        for col_idx in range(24):
            pd.to_numeric(profiles_df.iloc[:, col_idx], errors="raise")

        return profiles_df

    def ensure_profile_codes(self, output_path: Path, codes: Iterable[int]) -> bool:
        """Materialise specific uniform-1.0 profile codes if missing from SUEWS_Profiles.txt.

        Unlike ``ensure_required_profiles``, this preserves existing rows verbatim
        and inserts only the requested missing codes before any ``-9`` table
        terminator, so conversion rules can rely on their default codes even when
        ``validate_profiles=False``.
        """
        requested = sorted({int(code) for code in codes if int(code) > 0})
        missing = [code for code in requested if code not in self.available_profiles]
        if not missing:
            return False

        profiles_file = output_path / "SUEWS_Profiles.txt"
        new_lines = [
            f"{code:4d}    " + "    ".join(["1.000"] * 24) + "\n" for code in missing
        ]

        if profiles_file.exists():
            existing_lines = profiles_file.read_text(encoding="utf-8").splitlines(
                keepends=True
            )
            insert_at = len(existing_lines)
            for idx, line in enumerate(existing_lines):
                tokens = line.split()
                if tokens and tokens[0] == "-9":
                    insert_at = idx
                    break
            prefix = existing_lines[:insert_at]
            suffix = existing_lines[insert_at:]
            if prefix and not prefix[-1].endswith(("\n", "\r")):
                prefix[-1] = prefix[-1] + "\n"
            profiles_file.write_text(
                "".join(prefix + new_lines + suffix), encoding="utf-8"
            )
        else:
            header = (
                "    ".join(str(i) for i in range(1, 26))
                + "\n"
                + "Code    "
                + "    ".join(str(i) for i in range(24))
                + "\n"
            )
            profiles_file.write_text(
                header + "".join(new_lines) + "-9\n-9\n", encoding="utf-8"
            )

        self.profiles_path = profiles_file
        self.load_profiles()
        logger.info(
            f"Materialised conversion default profile code(s) {missing} in {profiles_file}"
        )
        return True

    def resolve_day_night_default_code(self, output_path: Path) -> int:
        """Return a uniform-1.0 profile code in SUEWS_Profiles.txt without clobbering user rows.

        Reuses ``DAY_NIGHT_DEFAULT`` (99901) only when it is absent (creating it)
        or already holds twenty-four ``1.0`` values. If the table already defines
        99901 with different values, allocates the next unused code (>= 99902),
        materialises uniform ``1.0`` there, and leaves the user's 99901 untouched.
        """
        preferred = int(self.DEFAULT_PROFILES["DAY_NIGHT_DEFAULT"])
        if preferred in self.available_profiles and self.profile_data is not None:
            row = self.profile_data.loc[preferred]
            if isinstance(row, pd.DataFrame):
                row = row.iloc[0]
            vals = pd.to_numeric(row.iloc[:24], errors="coerce").to_numpy(dtype=float)
            if len(vals) == 24 and bool(np.all(np.isfinite(vals))) and bool(
                np.allclose(vals, 1.0, atol=1e-6)
            ):
                return preferred
            candidate = preferred + 1
            while candidate in self.available_profiles:
                candidate += 1
            self.ensure_profile_codes(output_path, [candidate])
            return candidate

        self.ensure_profile_codes(output_path, [preferred])
        return preferred

    def ensure_required_profiles(self, output_path: Path):
        """Ensure all required profiles exist, creating defaults if needed.

        Args:
            output_path: Directory to write SUEWS_Profiles.txt
        """
        profiles_file = output_path / "SUEWS_Profiles.txt"

        # Validate existing profile table or initialise a 24-hour DataFrame before writing
        profiles_df = self._prepare_profiles_df(f"write {profiles_file}")

        # Add default profiles if they don't exist (DAY_NIGHT_DEFAULT is resolved
        # on demand per conversion step via resolve_day_night_default_code).
        for name, code in self.DEFAULT_PROFILES.items():
            if name == "DAY_NIGHT_DEFAULT":
                continue
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

        # Format output in memory before opening profiles_file so formatting errors
        # never truncate an existing SUEWS_Profiles.txt.
        lines = [
            "    ".join([str(i) for i in range(1, 26)]),
            "Code    " + "    ".join([str(i) for i in range(24)]),
        ]
        for code, row in profiles_df.iterrows():
            values = "    ".join([f"{float(v):.3f}" for v in row.iloc[:24]])
            lines.append(f"{code:4d}    {values}")

        # Write to file with proper header (matching legacy column naming 0-23)
        with open(profiles_file, "w", encoding="utf-8") as f:
            f.write("\n".join(lines) + "\n")

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
