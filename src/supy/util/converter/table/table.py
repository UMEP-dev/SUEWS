#!/usr/bin/env python
########################################################
# Table Converter for SUEWS
# Ting Sun, ting.sun@reading.ac.uk
# Yihao Tang, Yihao.Tang@student.reading.ac.uk
# history:
# TS, 13 Oct 2017: initial version
# YT, 01 Jun 2018: added the chained conversion
# TS, 21 May 2019: integrated into supy
########################################################
# %%
from collections import defaultdict
from contextlib import nullcontext
from fnmatch import fnmatch
from heapq import heappop, heappush
import os
import os.path
from pathlib import Path
import re
import shutil
from shutil import copyfile, move, rmtree
import sys
from tempfile import TemporaryDirectory

# ignore warnings raised by numpy when reading-in -9 lines
import warnings

from chardet import detect
import f90nml
import numpy as np
import pandas as pd

from ...._env import logger_supy, trv_supy_module
from ...._load import load_SUEWS_nml_simple
from .profile_manager import ProfileManager

warnings.filterwarnings("ignore")
########################################################
# %%
# load the rule file
rules = pd.read_csv(trv_supy_module / "util" / "converter" / "table" / "rules.csv")
list_ver_from = rules["From"].unique().tolist()
list_ver_to = rules["To"].unique().tolist()


def _check_required_files(input_path, required_files):
    """Check if all required files exist."""
    return all((input_path / f).exists() for f in required_files)


def _check_specific_files(input_path, specific_files):
    """Check if specific files exist based on RunControl.nml paths."""
    # Try to read RunControl.nml to get actual input path
    runcontrol_path = input_path / "RunControl.nml"

    if runcontrol_path.exists():
        try:
            ser_nml = load_SUEWS_nml_simple(str(runcontrol_path)).runcontrol
            fileinputpath = ser_nml.get("fileinputpath", "./input/")

            if os.path.isabs(fileinputpath):
                actual_input_dir = Path(fileinputpath)
            else:
                actual_input_dir = (input_path / fileinputpath).resolve()

            # Check in the actual input directory
            for f in specific_files:
                if not ((input_path / f).exists() or (actual_input_dir / f).exists()):
                    return False
            return True
        except Exception:
            pass

    # Fallback: check root and Input/ subdirectory
    for f in specific_files:
        if not ((input_path / f).exists() or (input_path / "Input" / f).exists()):
            return False
    return True


def _check_columns_in_file(file_path, columns_to_check):
    """Check if specific columns exist in a file's header."""
    if not file_path.exists():
        return False

    try:
        with open(file_path, encoding="utf-8") as f:
            lines = f.readlines()
            if len(lines) > 1:
                headers = lines[1].strip().split()
                return all(col in headers for col in columns_to_check)
    except Exception:
        return False

    return False


def _check_columns(input_path, check_columns):
    """Check if required columns exist in specified files."""
    # Try to read RunControl.nml to get actual input path
    runcontrol_path = input_path / "RunControl.nml"
    actual_input_dir = None

    if runcontrol_path.exists():
        try:
            ser_nml = load_SUEWS_nml_simple(str(runcontrol_path)).runcontrol
            fileinputpath = ser_nml.get("fileinputpath", "./input/")

            if os.path.isabs(fileinputpath):
                actual_input_dir = Path(fileinputpath)
            else:
                actual_input_dir = (input_path / fileinputpath).resolve()
        except Exception:
            pass

    for file, columns in check_columns.items():
        # Check root first
        file_path = input_path / file

        # Then check actual input directory from RunControl
        if not file_path.exists() and actual_input_dir:
            file_path = actual_input_dir / file

        # Fallback to Input/ subdirectory
        if not file_path.exists():
            file_path = input_path / "Input" / file

        if not _check_columns_in_file(file_path, columns):
            return False

    return True


def _check_negative_columns(input_path, negative_columns):
    """Check that specified columns do NOT exist in files."""
    # Try to read RunControl.nml to get actual input path
    runcontrol_path = input_path / "RunControl.nml"
    actual_input_dir = None

    if runcontrol_path.exists():
        try:
            ser_nml = load_SUEWS_nml_simple(str(runcontrol_path)).runcontrol
            fileinputpath = ser_nml.get("fileinputpath", "./input/")

            if os.path.isabs(fileinputpath):
                actual_input_dir = Path(fileinputpath)
            else:
                actual_input_dir = (input_path / fileinputpath).resolve()
        except Exception:
            pass

    for file, columns in negative_columns.items():
        # Check root first
        file_path = input_path / file

        # Then check actual input directory from RunControl
        if not file_path.exists() and actual_input_dir:
            file_path = actual_input_dir / file

        # Fallback to Input/ subdirectory
        if not file_path.exists():
            file_path = input_path / "Input" / file

        if not file_path.exists():
            # If file doesn't exist, that's fine for negative check
            continue

        try:
            with open(file_path, encoding="utf-8") as f:
                lines = f.readlines()
                if len(lines) > 1:
                    headers = lines[1].strip().split()
                    for col in columns:
                        if col in headers:  # Should NOT be present
                            return False
        except Exception:
            return False

    return True


def _check_nml_parameters(input_path, check_nml):
    """Check if required parameters exist in .nml files."""
    for nml_file, params in check_nml.items():
        nml_path = input_path / nml_file
        if not nml_path.exists():
            return False

        try:
            nml = f90nml.read(str(nml_path))
            # Get the first (and usually only) section
            section = next(iter(nml.values())) if nml else {}
            # Check if ALL required parameters exist
            for param in params:
                if param.lower() not in [k.lower() for k in section]:
                    return False
        except Exception:
            return False

    return True


def detect_table_version(input_dir):
    """Auto-detect the version of SUEWS table files.

    Detection is based on:
    - File existence (e.g., AnthropogenicEmission vs AnthropogenicHeat)
    - Column presence/absence in specific tables
    - Parameters in RunControl.nml (for 2024a+)
    - Optional files like SPARTACUS.nml

    Each version has unique characteristics that allow precise identification.

    Args:
        input_dir: Path to the directory containing SUEWS table files

    Returns
    -------
        str: Detected version (e.g., '2016a', '2024a') or None if unable to detect

    Note
    ----
        Detection checks versions from newest to oldest using unique
        characteristics of each version. Some versions (e.g., 2018a/b/c,
        2020a/2021a) are identical in structure; any detection among them
        is acceptable.
    """
    input_path = Path(input_dir)

    # Key indicators for different versions based on actual conversion rules
    # Structure of indicators:
    # - required_files: Must exist in root directory
    # - file_exists: Must exist in root or Input/ subdirectory
    # - check_columns: Columns that MUST exist in specified files
    # - negative_columns: Columns that must NOT exist (for differentiation)
    # - check_nml: Parameters that MUST exist in .nml files
    # - optional_files: Files that may exist and support identification
    # - fallback: Use this version if no other matches
    version_indicators = {
        # 2025a: Added building statistics columns
        "2025a": {
            "required_files": ["RunControl.nml"],
            "check_columns": {
                "SUEWS_SiteSelect.txt": ["h_std", "n_buildings"]  # Added in 2025a
            },
        },
        # 2024a: Added diagnostic methods and SPARTACUS radiation scheme support
        "2024a": {
            "required_files": ["RunControl.nml"],
            # SPARTACUS files are part of 2024a specification
            "file_exists": ["SUEWS_SPARTACUS.nml", "GridLayoutKc.nml"],
            # Also has new parameters in RunControl
            "check_nml": {
                "RunControl.nml": ["diagmethod", "localclimatemethod", "faimethod"]
            },
        },
        # 2023a: Removed DiagQS/DiagQN from RunControl, removed BaseT_HC from AnthropogenicEmission
        "2023a": {
            "required_files": ["RunControl.nml"],
            # 2023a has H_maintain but NOT BaseT_HC (which was deleted in 2021a->2023a)
            "check_columns": {
                "SUEWS_Irrigation.txt": ["H_maintain"],
            },
            "negative_columns": {
                "SUEWS_AnthropogenicEmission.txt": ["BaseT_HC"]  # Removed in 2023a
            },
        },
        # 2021a: No changes from 2020a (Keep action only)
        "2021a": {
            "required_files": ["RunControl.nml"],
            # Has both H_maintain and BaseT_HC
            "check_columns": {
                "SUEWS_Irrigation.txt": ["H_maintain"],
                "SUEWS_AnthropogenicEmission.txt": [
                    "BaseT_HC"
                ],  # Still present in 2021a
            },
        },
        # 2020a: Added H_maintain and irrigation fractions
        "2020a": {
            "required_files": ["RunControl.nml"],
            "check_columns": {
                "SUEWS_Irrigation.txt": ["H_maintain"],  # Added in 2020a
                "SUEWS_SiteSelect.txt": [
                    "IrrFr_Paved",
                    "IrrFr_Bldgs",
                ],  # Added in 2020a
            },
        },
        "2019b": {
            "required_files": ["RunControl.nml", "SUEWS_AnthropogenicEmission.txt"],
            "check_columns": {
                "SUEWS_AnthropogenicEmission.txt": ["BaseT_HC"]  # Renamed from BaseTHDD
            },
        },
        "2019a": {
            "required_files": ["RunControl.nml"],
            "file_exists": [
                "SUEWS_AnthropogenicEmission.txt"
            ],  # Renamed from AnthropogenicHeat
            # Check for BaseTHDD column (renamed to BaseT_HC in 2019b/2020a)
            "check_columns": {
                "SUEWS_AnthropogenicEmission.txt": [
                    "BaseTHDD"
                ]  # Original name before 2019b
            },
        },
        # 2018c: Added FcEF_v columns and CO2PointSource (converted to 2019a)
        "2018c": {
            "required_files": ["RunControl.nml"],
            "file_exists": ["SUEWS_AnthropogenicHeat.txt"],  # Old name before 2019a
            "check_columns": {
                # These columns were added when converting 2018c->2019a
                "SUEWS_AnthropogenicHeat.txt": [
                    "FcEF_v_kgkmWE",
                    "FcEF_v_kgkmWD",
                    "CO2PointSource",
                ]
            },
        },
        # 2018b: No changes from 2018a (Keep action only)
        "2018b": {
            "required_files": ["RunControl.nml"],
            "file_exists": ["SUEWS_AnthropogenicHeat.txt"],
            # Same structure as 2018a - differentiate by NOT having 2018c columns
            "negative_columns": {
                "SUEWS_AnthropogenicHeat.txt": [
                    "FcEF_v_kgkmWE",
                    "CO2PointSource",
                ]  # Not in 2018b
            },
            "check_columns": {
                "SUEWS_BiogenCO2.txt": ["alpha", "beta", "theta"],  # Has 2018a features
            },
        },
        # 2018a: Major restructuring from 2017a
        "2018a": {
            "required_files": ["RunControl.nml"],
            "file_exists": ["SUEWS_AnthropogenicHeat.txt"],
            "check_columns": {
                "SUEWS_BiogenCO2.txt": ["alpha", "beta", "theta"],  # Added in 2018a
                "SUEWS_SiteSelect.txt": [
                    "TrafficRate_WD",
                    "TrafficRate_WE",
                ],  # Added in 2018a
                "SUEWS_AnthropogenicHeat.txt": [
                    "AHMin_WD",
                    "AHMin_WE",
                ],  # Added in 2018a
            },
        },
        "2017a": {
            "required_files": ["RunControl.nml"],
            "file_exists": ["SUEWS_AnthropogenicHeat.txt"],
            # 2017a has ESTMCoefficients but different structure than 2018a
            "check_columns": {
                "SUEWS_Conductance.txt": ["gsModel"],  # Added in 2017a
            },
        },
        "2016a": {
            "required_files": ["RunControl.nml"],
            # 2016a has old parameter names and lacks ESTM/gsModel features
            "negative_columns": {
                "SUEWS_Conductance.txt": ["gsModel"],  # Not in 2016a
                "SUEWS_NonVeg.txt": ["OHMThresh_SW", "ESTMCode"],  # Not in 2016a
                "SUEWS_ESTMCoefficients.txt": [
                    "Surf_thick1",
                    "Wall_thick1",
                ],  # Not in 2016a
            },
            # Has old RunControl parameter names
            "check_nml": {
                "RunControl.nml": [
                    "AnthropHeatChoice",
                    "QSChoice",
                ]  # Old names in 2016a
            },
            "fallback": True,  # Still use as fallback if no other matches
        },
    }

    # Check versions from newest to oldest - ORDER IS CRITICAL!
    # Newer versions often contain all features of older versions plus additions.
    # By checking newest first with negative checks, we avoid false positives.
    # Example: 2025a has H_maintain (like 2020a) but also has h_std/n_buildings.
    # If we checked 2020a first, it would incorrectly match 2025a files.
    for version in [
        "2025a",  # Has h_std and n_buildings columns (unique to 2025a)
        "2024a",  # Has SPARTACUS files and new RunControl parameters
        "2023a",  # Has H_maintain but NOT BaseT_HC (removed in this version)
        "2021a",  # Has both H_maintain and BaseT_HC
        "2020a",  # Has H_maintain and IrrFr_ columns (same as 2021a)
        "2019b",  # Has BaseT_HC in AnthropogenicEmission (renamed from BaseTHDD)
        "2019a",  # Has BaseTHDD and AnthropogenicEmission.txt file
        "2018c",  # Same as 2018a/b (will be added FcEF columns when converting to 2019a)
        "2018b",  # Same as 2018a (no structural differences)
        "2018a",  # Has BiogenCO2 with alpha/beta, TrafficRate_WD
        "2017a",  # Has gsModel in Conductance, ESTM features
        "2016a",  # Oldest version with old parameter names
    ]:
        indicators = version_indicators.get(version, {})

        # Check required files exist
        required_files = indicators.get("required_files", [])
        if required_files and not _check_required_files(input_path, required_files):
            continue

        # Check for specific file existence (version-specific files)
        specific_files = indicators.get("file_exists", [])
        if specific_files and not _check_specific_files(input_path, specific_files):
            continue

        # Check for optional files (these can help identify version but aren't required)
        optional_files = indicators.get("optional_files", [])
        if optional_files:
            # If any optional file exists, it's a positive indicator
            for f in optional_files:
                if (input_path / f).exists() or (input_path / "Input" / f).exists():
                    # Found an optional file that helps identify this version
                    break

        # Check columns in text files
        check_columns = indicators.get("check_columns", {})
        if check_columns and not _check_columns(input_path, check_columns):
            continue

        # Check for columns that should NOT exist (negative check)
        negative_columns = indicators.get("negative_columns", {})
        if negative_columns and not _check_negative_columns(
            input_path, negative_columns
        ):
            continue

        # Check nml parameters for versions that need it (e.g., 2024a)
        check_nml = indicators.get("check_nml", {})
        if check_nml and not _check_nml_parameters(input_path, check_nml):
            continue

        # If this is a fallback version, only use if nothing else matched
        if indicators.get("fallback", False):
            logger_supy.warning(
                f"Could not determine exact version, assuming {version}"
            )

        # For versions without distinct table changes (e.g., 2023a, 2024a have same
        # structure as 2021a/2020a), we may detect an earlier version. This is fine
        # since the conversion rules are identical for these versions.
        logger_supy.info(f"Auto-detected table version: {version}")
        return version

    logger_supy.warning("Could not auto-detect table version")
    return None


# %%
########################################################
# define action functions:
# the current supported actions:
# rename, delete, add, move


# rename:
# rename file
def rename_file(toFile, _toVar, _toCol, toVal):
    # _toVar, _toCol are ignored
    if not Path(toFile).exists():
        logger_supy.error(f"{toFile} not existing")
        sys.exit()
    else:
        dir = Path(toFile).resolve().parent
        path_toFile_renamed = dir / toVal
        os.rename(toFile, path_toFile_renamed)


# rename variable
def rename_var(toFile, toVar, _toCol, toVal):
    # if namelist:
    if toFile.endswith(".nml"):
        logger_supy.info(f"{toFile} {toVar} {toVal}")
        rename_var_nml(toFile, toVar, toVal)
    else:
        # First, read the file to find where data ends (before -9 lines)
        with open(toFile, encoding="utf-8") as f:
            lines = f.readlines()

        # Find where data ends (first line starting with -9)
        data_end_idx = len(lines)
        for i, line in enumerate(lines):
            if line.strip().startswith("-9"):
                data_end_idx = i
                break

        # Read only the data portion
        try:
            dataX = pd.read_csv(
                toFile,
                sep=r"\s+",
                comment="!",
                encoding="UTF8",
                skiprows=2,  # Skip both header lines
                nrows=data_end_idx - 2 if data_end_idx > 2 else None,
                header=None,
            )
            # Get the header from the second line
            if len(lines) > 1:
                headers = lines[1].strip().split()
                dataX.columns = headers
        except Exception as e:
            logger_supy.error(f"Could not read {toFile}: {e}")
            return

        # Rename the column
        if toVar in dataX.columns:
            dataX = dataX.rename(columns={toVar: toVal})
        else:
            logger_supy.warning(f"Column {toVar} not found in {toFile}")

        # Get headers
        headers = list(dataX.columns)

        # Create header line
        headerLine = (
            " ".join(str(i + 1) for i in range(len(headers))) + "\n" + " ".join(headers)
        )

        # Convert to string
        dataX = dataX.astype(str)

        # Write the file
        with open(toFile, "w", encoding="utf-8") as f:
            f.write(headerLine + "\n")
            dataX.to_csv(f, sep=" ", index=False, header=False)
            # NO footer lines - these are legacy and should not be added

        logger_supy.debug(f"Renamed {toVar} to {toVal} in {toFile}")
        return


def rename_var_nml(to_file, to_var, to_val):
    """Rename a variable in a .nml file, using lower case for consistency."""
    nml = f90nml.read(to_file)
    title = next(iter(nml.keys()))
    to_var_lower = to_var.lower()
    to_val_lower = to_val.lower()
    if to_var_lower in nml[title]:
        nml[title][to_val_lower] = nml[title].pop(to_var_lower)
    else:
        logger_supy.warning(f"{to_var} does not exist!")
    nml.write(to_file, force=True)


# delete:
# delete variable
def delete_var(toFile, toVar, _toCol, toVal):
    if toFile.endswith(".nml"):
        delete_var_nml(toFile, toVar, toVal)
    else:
        # First, read the file to find where data ends (before -9 lines)
        with open(toFile, encoding="utf-8") as f:
            lines = f.readlines()

        # Find where data ends (first line starting with -9)
        data_end_idx = len(lines)
        for i, line in enumerate(lines):
            if line.strip().startswith("-9"):
                data_end_idx = i
                break

        # Read only the data portion
        try:
            dataX = pd.read_csv(
                toFile,
                sep=r"\s+",
                comment="!",
                encoding="UTF8",
                skiprows=2,  # Skip both header lines
                nrows=data_end_idx - 2 if data_end_idx > 2 else None,
                header=None,
            )
            # Get the header from the second line
            if len(lines) > 1:
                headers = lines[1].strip().split()
                dataX.columns = headers
        except Exception as e:
            logger_supy.error(f"Could not read {toFile}: {e}")
            return

        # Delete the column
        if toVar in dataX.columns:
            dataX = dataX.drop(columns=[toVar])
        else:
            logger_supy.warning(f"Column {toVar} not found in {toFile}")
            return

        # Get headers after deletion
        headers = list(dataX.columns)

        # Create header line
        headerLine = (
            " ".join(str(i + 1) for i in range(len(headers))) + "\n" + " ".join(headers)
        )

        # Convert to string
        dataX = dataX.astype(str)

        # Write the file
        with open(toFile, "w", encoding="utf-8") as f:
            f.write(headerLine + "\n")
            dataX.to_csv(f, sep=" ", index=False, header=False)
            # NO footer lines - these are legacy and should not be added

        logger_supy.debug(f"Deleted column {toVar} from {toFile}")
        return


def delete_var_nml(toFile, toVar, _toVal):
    nml = f90nml.read(toFile)
    toVarX = toVar.lower()
    title = next(iter(nml.keys()))
    if toVarX in nml[title]:
        nml[title].pop(toVarX)
    else:
        logger_supy.warning(f"{toVar} does not exist!")
    nml.write(toFile, force=True)


def _should_skip_line(line):
    """Check if a line should be skipped during cleaning."""
    # Skip empty lines and full-line comments
    if not line or line.strip().startswith("#"):
        return True

    # Skip lines that contain triple quotes or problematic quoted comments
    if '"""' in line or (
        '"' in line and ("Vegetation (average)" in line or "used for" in line)
    ):
        return True

    # Skip lines starting with -9 (legacy footers)
    return line.strip().startswith("-9")


def _process_line(line):
    """Process a single line: remove comments and tabs."""
    # Replace tabs with spaces
    line = line.replace("\t", " ")

    # Remove inline comments (everything after !)
    if "!" in line:
        line = line[: line.index("!")].rstrip()

    return line


def _ensure_consistent_columns(fields, header_col_count):
    """Ensure field count matches header column count."""
    if not header_col_count:
        return fields

    if len(fields) == header_col_count:
        return fields

    # Truncate extra fields or pad with -999
    if len(fields) > header_col_count:
        return fields[:header_col_count]
    else:
        while len(fields) < header_col_count:
            fields.append("-999")
        return fields


def clean_legacy_table(file_path, output_path=None):
    r"""
    Clean legacy SUEWS table files for pandas compatibility.

    This function:
    - Removes inline comments (text after ! character)
    - Standardizes line endings (removes \r)
    - Removes empty trailing columns
    - Ensures consistent column counts
    - Handles tab-separated values
    - Removes ALL lines that start with -9 (legacy footers)

    Args:
        file_path: Path to the input file
        output_path: Optional path for cleaned output (if None, overwrites input)

    Returns
    -------
        Path to the cleaned file
    """
    if output_path is None:
        output_path = file_path

    logger_supy.debug(f"Cleaning legacy file: {file_path}")

    # Track what was cleaned for reporting
    cleaning_actions = []

    with open(file_path, encoding="utf-8", errors="replace") as f:
        lines = f.readlines()

    if len(lines) < 2:
        logger_supy.warning(
            f"File {file_path} has less than 2 lines, skipping cleaning"
        )
        return file_path

    header_lines = []  # Store header lines (first 2 lines)
    data_lines = []  # Store data lines
    header_col_count = None
    line_count = 0  # Track non-empty lines

    # Track cleaning statistics
    comments_removed = 0
    tabs_replaced = 0
    footer_removed = False
    columns_adjusted = 0

    for i, raw_line in enumerate(lines):
        # Remove carriage returns and trailing whitespace
        line = raw_line.replace("\r", "").rstrip()

        # Track tabs for reporting
        if "\t" in line:
            tabs_replaced += 1

        # Check if line should be skipped
        if _should_skip_line(line):
            if line.strip().startswith("-9"):
                footer_removed = True
                logger_supy.debug(
                    f"Removing legacy footer line {i + 1}: {line[:50]}... Stopping read after footer."
                )
                break  # Stop processing after footer
            elif '"""' in line or (
                '"' in line and ("Vegetation (average)" in line or "used for" in line)
            ):
                logger_supy.debug(
                    f"Skipping line {i + 1} with problematic quoted comments: {line[:50]}..."
                )
                cleaning_actions.append(f"Removed metadata line {i + 1}")
            continue

        # Process the line (remove comments and tabs)
        original_line = line
        line = _process_line(line)
        if "!" in original_line:
            comments_removed += 1

        # Split by spaces (tabs have been replaced with spaces)
        fields = line.split()

        # Skip empty lines after processing
        if not fields:
            continue

        # For the header rows (first 2 non-empty lines), establish column count
        if line_count < 2:
            if header_col_count is None:
                header_col_count = len(fields)
                logger_supy.debug(
                    f"Header column count set to {header_col_count} at line {i + 1}"
                )
            # Store header line
            header_lines.append(" ".join(fields))
            line_count += 1
            continue

        # For data lines
        line_count += 1

        # Ensure consistent column count
        original_field_count = len(fields)
        fields = _ensure_consistent_columns(fields, header_col_count)
        if len(fields) != original_field_count:
            columns_adjusted += 1
            if original_field_count > header_col_count:
                logger_supy.debug(
                    f"Line {i + 1}: Truncating from {original_field_count} to {header_col_count} fields"
                )

        # Store processed data line
        data_lines.append(" ".join(fields))

    # Combine header and data lines
    cleaned_lines = header_lines + data_lines

    # Note: We do NOT add footer lines - the -9 lines are removed entirely

    # Write cleaned content
    with open(output_path, "w", encoding="utf-8") as f:
        f.write("\n".join(cleaned_lines))
        if cleaned_lines and not cleaned_lines[-1].endswith("\n"):
            f.write("\n")

    # Report what was cleaned
    if (
        comments_removed > 0
        or tabs_replaced > 0
        or footer_removed
        or columns_adjusted > 0
    ):
        clean_summary = []
        if comments_removed > 0:
            clean_summary.append(f"{comments_removed} inline comments")
        if tabs_replaced > 0:
            clean_summary.append(f"{tabs_replaced} tabs replaced")
        if footer_removed:
            clean_summary.append("legacy footer removed")
        if columns_adjusted > 0:
            clean_summary.append(
                f"{columns_adjusted} lines adjusted for column consistency"
            )
        if cleaning_actions:
            clean_summary.append(f"{len(cleaning_actions)} metadata lines removed")

        logger_supy.info(
            f"[OK] Cleaned {Path(file_path).name}: {', '.join(clean_summary)}"
        )
    else:
        logger_supy.debug(f"File {Path(file_path).name} was already clean")

    return output_path


# Helper function to read SUEWS files robustly (kept for backward compatibility but simplified)
def read_suews_table(toFile):
    """Read SUEWS table file using numpy - simpler approach."""
    try:
        dataX = np.genfromtxt(
            toFile,
            dtype=str,
            skip_header=1,
            comments="!",
            names=True,
            invalid_raise=False,
            encoding="UTF8",
        )

        # Convert to pandas DataFrame for compatibility
        if dataX.size == 0:
            return pd.DataFrame(columns=list(dataX.dtype.names))
        else:
            return pd.DataFrame(dataX.tolist(), columns=list(dataX.dtype.names))
    except Exception as e:
        logger_supy.error(f"Failed to read {toFile}: {e!s}")
        raise


# add:
# add variable(s) to a file
def add_var(toFile, toVar, toCol, toVal):
    # if namelist:
    if toFile.endswith(".nml"):
        add_var_nml(toFile, toVar, toVal)
    else:
        # First, read the file to find where data ends (before -9 lines)
        with open(toFile, encoding="utf-8") as f:
            lines = f.readlines()

        # Find where data ends (first line starting with -9)
        data_end_idx = len(lines)
        for i, line in enumerate(lines):
            if line.strip().startswith("-9"):
                data_end_idx = i
                break

        # Read only the data portion (skip headers and footers)
        try:
            # Use pandas to read only the data lines
            dataX = pd.read_csv(
                toFile,
                sep=r"\s+",  # Use regex for whitespace separation
                comment="!",
                encoding="UTF8",
                skiprows=2,  # Skip both header lines
                nrows=data_end_idx - 2
                if data_end_idx > 2
                else None,  # Read only data rows
                header=None,  # No header in data
            )

            # Get the header from the second line
            if len(lines) > 1:
                headers = lines[1].strip().split()
                dataX.columns = headers
            else:
                headers = []
        except Exception as e:
            logger_supy.debug(f"Could not read {toFile} with pandas: {e}")
            # If file doesn't exist or is empty, create minimal structure
            dataX = pd.DataFrame()
            headers = []

        # Check if column already exists
        if toVar in headers:
            logger_supy.warning(
                f"{toVar} already exists in {toFile}, skipping add operation"
            )
            return

        # Calculate target position (convert from 1-based to 0-based)
        target_col = int(toCol) - 1

        # Insert the new column at the specified position
        if target_col <= len(headers):
            headers.insert(target_col, toVar)
            # Add the new column to dataX with the default value
            if not dataX.empty:
                # Insert column with the same value for all rows
                dataX.insert(target_col, toVar, toVal)
            else:
                # Create a new dataframe with just the header
                dataX = pd.DataFrame(columns=headers)

        # Create header line with column indices
        headerLine = (
            " ".join(str(i + 1) for i in range(len(headers))) + "\n" + " ".join(headers)
        )

        # Save the dataframe to file
        # Convert to string to ensure all values are saved as text
        if not dataX.empty:
            dataX = dataX.astype(str)

        # Write the file with headers
        with open(toFile, "w", encoding="utf-8") as f:
            # Write header lines
            f.write(headerLine + "\n")
            # Write data without index (only if there's data)
            if not dataX.empty:
                dataX.to_csv(f, sep=" ", index=False, header=False)
            # NO footer lines - these are legacy and should not be added


def add_var_nml(toFile, toVar, toVal):
    nml = f90nml.read(toFile)
    toVarX = toVar.lower()
    title = next(iter(nml.keys()))
    if toVarX not in nml[title]:
        # Convert string values to appropriate types for .nml files
        # Try to convert to int or float if possible
        try:
            # First try integer
            if "." not in str(toVal):
                toVal = int(toVal)
            else:
                # If it has a decimal point, use float
                toVal = float(toVal)
        except (ValueError, TypeError):
            # Keep as string if conversion fails
            pass
        nml[title][toVarX] = toVal
    else:
        logger_supy.warning(f"{toVar} exists!")
    nml.write(toFile, force=True)


def change_var_nml(toFile, toVar, toVal):
    nml = f90nml.read(toFile)
    nml[toVar] = toVal
    nml.write(toFile)


def _copy_and_clean_files(fromDir, toDir, file_patterns, clean_txt=True):
    """Copy files matching patterns and optionally clean text files."""
    for fileX in os.listdir(fromDir):
        if any(fnmatch(fileX, p) for p in file_patterns):
            file_src = os.path.join(fromDir, fileX)
            file_dst = os.path.join(toDir, fileX)
            copyfile(file_src, file_dst)
            convert_utf8(file_dst)
            if clean_txt and fnmatch(fileX, "*.txt"):
                clean_legacy_table(file_dst)


def _handle_same_version_copy(fromDir, toDir, fromVer):
    """Handle the special case where source and target versions are the same."""
    logger_supy.info(
        f"Source and target versions are the same ({fromVer}). Only cleaning files..."
    )

    # Read RunControl.nml to determine file structure
    runcontrol_path = Path(fromDir) / "RunControl.nml"
    if not runcontrol_path.exists():
        raise FileNotFoundError(f"RunControl.nml not found in {fromDir}")

    # Load RunControl to get file paths
    ser_nml = load_SUEWS_nml_simple(str(runcontrol_path)).runcontrol

    # Resolve input path from RunControl
    fileinputpath = ser_nml.get("fileinputpath", "./input/")
    if os.path.isabs(fileinputpath):
        # Absolute path
        input_dir = Path(fileinputpath)
    else:
        # Relative path from fromDir
        input_dir = (Path(fromDir) / fileinputpath).resolve()

    # Copy files from the actual input directory
    if input_dir.exists():
        _copy_and_clean_files(
            str(input_dir), toDir, ["SUEWS_*.txt", "*.nml"], clean_txt=True
        )

    # Also copy RunControl.nml and any other .nml files from root
    _copy_and_clean_files(fromDir, toDir, ["*.nml"], clean_txt=False)

    # Create the standard directory structure
    ser_nml = load_SUEWS_nml_simple(str(Path(toDir) / "RunControl.nml")).runcontrol
    path_input = (Path(toDir) / ser_nml["fileinputpath"]).resolve()
    path_output = (Path(toDir) / ser_nml["fileoutputpath"]).resolve()
    path_input.mkdir(exist_ok=True)
    path_output.mkdir(exist_ok=True)

    # Move table files to Input directory
    list_table_input = list(Path(toDir).glob("SUEWS*.txt")) + [
        x for x in Path(toDir).glob("*.nml") if "RunControl" not in str(x)
    ]
    for fileX in list_table_input:
        move(fileX.resolve(), path_input / fileX.name)

    logger_supy.info(f"Files cleaned and copied to {toDir}")


def _build_file_list(fromDir, fromVer):
    """Build list of files to process based on RunControl.nml structure."""
    fileList = []

    # Read RunControl.nml to determine file structure
    runcontrol_path = Path(fromDir) / "RunControl.nml"
    if not runcontrol_path.exists():
        # If no RunControl.nml, fall back to checking root
        logger_supy.warning(
            f"RunControl.nml not found in {fromDir}, checking root directory"
        )
        for fileX in os.listdir(fromDir):
            if any(fnmatch(fileX, p) for p in ["SUEWS*.txt", "*.nml", "*.txt"]):
                fileList.append(("", fileX))
        return fileList

    # Load RunControl to get file paths
    ser_nml = load_SUEWS_nml_simple(str(runcontrol_path)).runcontrol

    # Resolve input path from RunControl
    fileinputpath = ser_nml.get("fileinputpath", "./input/")
    if os.path.isabs(fileinputpath):
        # Absolute path
        input_dir = Path(fileinputpath)
    else:
        # Relative path from fromDir
        input_dir = (Path(fromDir) / fileinputpath).resolve()

    # Check for files in the input directory specified by RunControl
    if input_dir.exists():
        logger_supy.debug(
            f"Found input directory at {input_dir}, scanning for SUEWS_*.txt files"
        )
        # Get relative path from fromDir to input_dir for the subdir part
        try:
            rel_path = input_dir.relative_to(Path(fromDir).resolve())
            subdir = str(rel_path)
        except ValueError:
            # If not relative, use empty string
            subdir = ""

        for fileX in os.listdir(input_dir):
            if fnmatch(fileX, "SUEWS_*.txt") or fnmatch(fileX, "*.nml"):
                fileList.append((subdir, fileX))
                logger_supy.debug(f"Found file in {subdir}: {fileX}")

    # Also check root for .nml files and txt files
    for fileX in os.listdir(fromDir):
        if fnmatch(fileX, "*.nml") or fnmatch(fileX, "*.txt"):
            fileList.append(("", fileX))
            logger_supy.debug(f"Found file in root: {fileX}")

    return fileList


# a single conversion between two versions
def SUEWS_Converter_single(fromDir, toDir, fromVer, toVer):
    # copy files in fromDir to toDir, only: *.nml, SUEWS_*.txt
    if os.path.exists(toDir) is False:
        os.mkdir(toDir)

    # Special case: if fromVer == toVer, just copy and clean without conversion
    if fromVer == toVer:
        _handle_same_version_copy(fromDir, toDir, fromVer)
        return

    # Normal conversion process continues below
    fileList = _build_file_list(fromDir, fromVer)

    for subdir, fileX in fileList:
        file_src = (
            os.path.join(fromDir, subdir, fileX)
            if subdir
            else os.path.join(fromDir, fileX)
        )
        # Always copy to root of toDir (flattening the structure)
        file_dst = os.path.join(toDir, fileX)
        logger_supy.debug(f"Copying {file_src} to {file_dst}")
        copyfile(file_src, file_dst)
        convert_utf8(file_dst)

    # Note: File cleaning is now done once in convert_table() when files are first copied
    # This avoids redundant cleaning during chained conversions

    # Special handling: Create SPARTACUS.nml and GridLayoutKc.nml when converting 2023a→2024a
    # These files are introduced in 2024a and should only be created at this specific step
    # In a chained conversion, this ensures they're created at the right point
    if fromVer == "2023a" and toVer == "2024a":
        spartacus_path = os.path.join(toDir, "SUEWS_SPARTACUS.nml")
        if not os.path.exists(spartacus_path):
            # Create a minimal SPARTACUS.nml file with default values
            spartacus_content = """&Spartacus_Settings
use_sw_direct_albedo = false
n_vegetation_region_urban = 1
n_stream_sw_urban = 4
n_stream_lw_urban = 4
/
&Spartacus_Constant_Parameters
sw_dn_direct_frac = 0.45
air_ext_sw = 0.0
air_ssa_sw = 0.95
veg_ssa_sw = 0.46
air_ext_lw = 0.0
air_ssa_lw = 0.0
veg_ssa_lw = 0.06
veg_fsd_const = 0.75
veg_contact_fraction_const = 0.
ground_albedo_dir_mult_fact = 1.
/
&radsurf_driver
/
&radsurf
/
"""
            with open(spartacus_path, "w", encoding="utf-8") as f:
                f.write(spartacus_content)
            logger_supy.info(f"Created placeholder SUEWS_SPARTACUS.nml for {toVer}")

        # Also create GridLayoutKc.nml for 2024a+
        gridlayout_path = os.path.join(toDir, "GridLayoutKc.nml")
        if not os.path.exists(gridlayout_path):
            # Create a complete GridLayoutKc.nml file with thermal layer data
            gridlayout_content = """&dim
nlayer = 3
/
&geom
height = 0., 11., 15., 22.
building_frac = 0.43, 0.38, .2
veg_frac = 0.01, 0.02, .01
building_scale = 50., 50., 50
veg_scale = 10., 10., 10
/
&roof
sfr_roof = .3, .3, .4
tin_roof = 5, 5, 6
alb_roof = .5, .5, .2
emis_roof = .95, .95, .95
state_roof = .0, .0, .0
statelimit_roof = 5, 5, 5
wetthresh_roof = 5, 5, 5
soilstore_roof = 20, 20, 20
soilstorecap_roof = 120, 120, 120

roof_albedo_dir_mult_fact(1,:) = 1., 1., 1.

dz_roof(1,:) = .2, .1, .1, .01, .01
k_roof(1,:) = 1.2, 1.2, 1.2, 1.2, 1.2
cp_roof(1,:) = 2e6, 2e6, 2e6, 2e6, 2e6

dz_roof(2,:) = .2, .1, .1, .01, .01
k_roof(2,:) = 2.2, 1.2, 1.2, 1.2, 1.2
cp_roof(2,:) = 2e6, 3e6, 2e6, 2e6, 2e6

dz_roof(3,:) = .2, .1, .1, .01, .01
k_roof(3,:) = 2.2, 1.2, 1.2, 1.2, 1.2
cp_roof(3,:) = 2e6, 3e6, 2e6, 2e6, 2e6
/
&wall
sfr_wall = .3, .3, .4
tin_wall = 5, 5, 5
alb_wall = .5, .5, .5
emis_wall = .95, .95, .95
state_wall = .0, .0, .0
statelimit_wall = 5, 5, 5
wetthresh_wall = 5, 5, 5
soilstore_wall = 20, 20, 20
soilstorecap_wall = 120, 120, 120

wall_specular_frac(1,:) = 0., 0., 0.

dz_wall(1,:) = .2, .1, .1, .01, .01
k_wall(1,:) = 1.2, 1.2, 1.2, 1.2, 1.2
cp_wall(1,:) = 3e6, 2e6, 2e6, 2e6, 2e6

dz_wall(2,:) = .2, .1, .1, .01, .01
k_wall(2,:) = 1.2, 1.2, 1.2, 1.2, 1.2
cp_wall(2,:) = 2e6, 3e6, 2e6, 2e6, 2e6

dz_wall(3,:) = .2, .1, .1, .01, .01
k_wall(3,:) = 1.2, 1.2, 1.2, 1.2, 1.2
cp_wall(3,:) = 2e6, 3e6, 2e6, 2e6, 2e6
/
&surf
tin_surf = 2, 2, 2, 2, 2, 2, 2

dz_surf(1,:) = .2, .15, .01, .01, .01
k_surf(1,:) = 1.1, 1.1, 1.1, 1.1, 1.1
cp_surf(1,:) = 2.2e6, 2.2e6, 2.2e6, 2.2e6, 2.6e6

dz_surf(2,:) = .2, .1, .1, .5, 1.6
k_surf(2,:) = 1.2, 1.1, 1.1, 1.5, 1.6
cp_surf(2,:) = 1.2e6, 1.1e6, 1.1e6, 1.5e6, 1.6e6

dz_surf(3,:) = .2, .1, .1, .5, 1.6
k_surf(3,:) = 1.2, 1.1, 1.1, 1.5, 1.6
cp_surf(3,:) = 3.2e6, 1.1e6, 1.1e6, 1.5e6, 1.6e6

dz_surf(4,:) = .2, .1, .1, .1, 2.2
k_surf(4,:) = 1.2, 1.1, 1.1, 1.5, 1.6
cp_surf(4,:) = 3.2e6, 1.1e6, 1.1e6, 1.5e6, 1.6e6

dz_surf(5,:) = .2, .05, .1, .1, 2.2
k_surf(5,:) = 1.2, 1.1, 1.1, 1.5, 1.6
cp_surf(5,:) = 1.6e6, 1.1e6, 1.1e6, 1.5e6, 1.6e6

dz_surf(6,:) = .2, .05, .1, .1, 2.2
k_surf(6,:) = 1.2, 1.1, 1.1, 1.5, 1.6
cp_surf(6,:) = 1.9e6, 1.1e6, 1.1e6, 1.5e6, 1.6e6

dz_surf(7,:) = .2, .05, .1, .1, 2.2
k_surf(7,:) = 1.2, 1.1, 1.1, 1.5, 1.6
cp_surf(7,:) = 1.9e6, 1.1e6, 1.1e6, 1.5e6, 1.6e6
/
"""
            with open(gridlayout_path, "w", encoding="utf-8") as f:
                f.write(gridlayout_content)
            logger_supy.info(f"Created placeholder GridLayoutKc.nml for {toVer}")

    # list all files involved in the given conversion
    posRules = np.unique(
        np.where(
            np.array(rules.loc[:, ["From", "To"]].values.tolist()) == [fromVer, toVer]
        )[0]
    )
    filesToConvert = set(rules["File"][posRules]) - {"-999"}

    # Also include SUEWS_*.txt files that exist in source but aren't in rules
    # This ensures files like OHMCoefficients, Profiles, Soil, WithinGridWaterDist are preserved
    existing_files = set()
    for fileX in os.listdir(toDir):
        if fnmatch(fileX, "SUEWS_*.txt"):
            existing_files.add(fileX)

    # Add existing files not in rules to the conversion list
    # These will just be copied without modifications
    files_without_rules = existing_files - filesToConvert
    if files_without_rules:
        logger_supy.info(
            f"Files without rules (will be preserved): {list(files_without_rules)}"
        )

    # Combine both sets
    filesToConvert |= files_without_rules

    logger_supy.info(f"filesToConvert: {list(filesToConvert)}")

    for fileX in filesToConvert:
        logger_supy.info(f"working on file: {fileX}")

        # Special debugging for ESTM file
        if "ESTM" in fileX:
            full_path = os.path.join(toDir, fileX)
            if Path(full_path).exists():
                logger_supy.warning(
                    f"ESTM file already exists at start of processing: {full_path}, size={Path(full_path).stat().st_size}"
                )

        try:
            actionList = rules.values[posRules].compress(
                rules["File"].values[posRules] == fileX, axis=0
            )

            # If no rules exist for this file, it will just be copied as-is (already done in SUEWS_Converter_single)
            if len(actionList) == 0:
                logger_supy.info(
                    f"No conversion rules for {fileX}, file preserved as-is"
                )
                continue

            actionList = actionList[:, 2:]
            # actionList = np.array(actionList.tolist())[:, 2:].astype('S140')
            # prepend toDir to fileX
            actionList[:, 1] = os.path.join(toDir, fileX)
            # print('actionList:', actionList)
            SUEWS_Converter_file(os.path.join(toDir, fileX), actionList)
        except Exception as e:
            logger_supy.error(
                f"Failed to convert {fileX} from {fromVer} to {toVer}: {e!s}"
            )
            # Don't continue with a broken conversion - fail fast
            raise RuntimeError(f"Conversion stopped at {fileX}: {e!s}") from e


def SUEWS_Converter_file(fileX, actionList):
    # actionList:[Action,File,Variable,Column,Value]
    # for a given fileX, action order:
    # 1. rename
    # 2. delete
    # 3. move
    # 4. add
    # 5. rename file
    order = {
        "Keep": 0,
        "Rename": 1,
        "Delete": 2,
        "Move": 3,
        "Add": 4,
        "Rename_File": 5,
    }

    todoList = np.array([
        np.concatenate(([order[x[0]]], x)).tolist() for x in actionList
    ])

    # sort by Column number, then by Action order in actionList; also expand
    # dtype size
    todoList = todoList[np.lexsort((todoList[:, 4].astype(int), todoList[:, 0]))][:, 1:]

    # Check if file exists before processing
    if "ESTM" in fileX and Path(fileX).exists():
        file_size = Path(fileX).stat().st_size
        logger_supy.warning(
            f"ESTM file already exists before placeholder creation: {fileX}, size={file_size} bytes"
        )
        # Read first few lines to see what's in it
        with open(fileX, encoding="utf-8") as f:
            first_lines = f.readlines()[:3]
            logger_supy.warning(f"ESTM file first lines: {first_lines}")

    if not Path(fileX).exists():
        # Only create placeholder for .txt files, not .nml files
        if fileX.endswith(".txt"):
            # Create appropriate placeholder based on file type
            if "BiogenCO2" in fileX:
                # Create minimal BiogenCO2 file - columns will be added by conversion rules
                # Just create the basic structure with Code column only
                placeholder = "1\nCode\n"
                placeholder += "31\n"  # Code 31 is commonly referenced
            elif "ESTMCoefficients" in fileX:
                # Create minimal ESTM file - columns will be added by conversion rules
                # Just create the basic structure with Code column only
                placeholder = "1\nCode\n"
                placeholder += (
                    "800\n801\n802\n803\n804\n805\n806\n807\n808\n60\n61\n200\n"
                )
                logger_supy.warning(
                    f"Creating ESTM placeholder with minimal structure: {len(placeholder)} bytes"
                )
            else:
                # Default placeholder
                placeholder = "1\nCode\n800\n"
            Path(fileX).write_text(placeholder, encoding="UTF8")
            logger_supy.debug(f"Created placeholder for missing file: {fileX}")
        elif fileX.endswith(".nml"):
            # For missing .nml files, skip processing
            logger_supy.warning(f"Namelist file {fileX} does not exist, skipping")
            return  # Skip processing this file
        else:
            logger_supy.warning(f"Unknown file type {fileX} does not exist, skipping")
            return

    if not fileX.endswith("-999"):
        logger_supy.info(f"working on {fileX} in {get_encoding_type(fileX)}")
    # correct file names with proper path
    todoList[:, 1] = fileX
    # print todoList,fileX
    for action in todoList:
        # print(action)
        try:
            SUEWS_Converter_action(*action)
        except Exception as e:
            logger_supy.error(f"Failed to perform action {action[0]} on {fileX}: {e!s}")
            raise RuntimeError(
                f"Conversion failed at {action[0]} for {fileX}: {e!s}"
            ) from e


def keep_file(_toFile, _var, _col, _val):
    pass


def SUEWS_Converter_action(action, toFile, var, col, val):
    logger_supy.info(f"{action}, {toFile}, {var}, {col}, {val}")

    actionFunc = {
        "Rename": rename_var,
        "Delete": delete_var,
        "Add": add_var,
        "Rename_File": rename_file,
        "Keep": keep_file,
    }
    actionFunc[action](toFile, var, col, val)

    logger_supy.info(f"{action} {var} for {toFile} done!")


def dijkstra(edges, f, t):
    g = defaultdict(list)
    for src, dst, weight in edges:
        g[src].append((weight, dst))
    q, seen = [(0, f, ())], set()

    while q:
        (cost, v1, path) = heappop(q)

        if v1 not in seen:
            seen.add(v1)
            path = (v1, path)
            if v1 == t:
                return cost, path
            for c, v2 in g.get(v1, ()):
                if v2 not in seen:
                    heappush(q, (cost + c, v2, path))

    return float("inf")


def version_list(fromVer, toVer):
    edges = []
    # a = pd.read_csv('rules.csv')
    a = rules
    v_from = np.unique(a["From"])
    for i in v_from:
        df = a[a["From"] == i]
        for k in np.unique(df["To"]):
            edges.append((i, k, 1))

    s = dijkstra(edges, fromVer, toVer)
    chain_ver = []
    while s:
        chain_ver.append(s[0])
        s = s[1]
    return chain_ver


# a chained conversion across multiple versions
def convert_table(
    fromDir, toDir, fromVer, toVer, debug_dir=None, validate_profiles=True
):
    """Convert SUEWS table files between versions.

    This function performs chained conversion between SUEWS table versions,
    automatically handling intermediate version transitions when needed.

    Args:
        fromDir: Path to directory containing source SUEWS table files
        toDir: Path to directory where converted tables will be saved
        fromVer: Source version (e.g., '2016a', '2020a', '2024a')
        toVer: Target version (e.g., '2024a', '2025a')
        debug_dir: Optional directory to save intermediate conversion files
        validate_profiles: Whether to validate and auto-create missing profile entries

    Returns
    -------
        None

    Note
    ----
        If fromVer == toVer, the function only cleans/reformats files without conversion.

        The conversion process:
        1. Reads input files from fromDir (using paths in RunControl.nml)
        2. Performs chained conversion through intermediate versions if needed
        3. Writes converted files to toDir in the target version format

        With debug_dir specified, intermediate conversion steps are preserved for inspection.

    Examples
    --------
        >>> from supy.util.converter import convert_table
        >>>
        >>> # Convert from 2016a to 2024a
        >>> convert_table(
        ...     fromDir="path/to/old_data",
        ...     toDir="path/to/new_data",
        ...     fromVer="2016a",
        ...     toVer="2024a",
        ... )
        >>>
        >>> # Convert with debug output
        >>> convert_table(
        ...     fromDir="path/to/old_data",
        ...     toDir="path/to/new_data",
        ...     fromVer="2020a",
        ...     toVer="2024a",
        ...     debug_dir="debug_output",
        ... )
    """
    # Special case: if fromVer == toVer, just clean without conversion
    if fromVer == toVer:
        logger_supy.info(
            f"Source and target versions are the same ({fromVer}). Only cleaning files..."
        )
        SUEWS_Converter_single(fromDir, toDir, fromVer, toVer)
        return

    chain_ver = version_list(fromVer, toVer)
    len_chain = chain_ver[0]
    logger_supy.info(f"working on chained conversion {len_chain} actions to take")
    logger_supy.info(f"chained list: {chain_ver[1:]} \n")

    # Create debug directory if specified
    if debug_dir is not None:
        debug_path = Path(debug_dir)
        debug_path.mkdir(parents=True, exist_ok=True)
        logger_supy.info(
            f"Debug mode: intermediate files will be saved in {debug_path}"
        )

    # use a persistent directory when debug_dir is provided
    temp_ctx = (
        TemporaryDirectory()
        if debug_dir is None
        else nullcontext(str(debug_path) if debug_dir else None)
    )
    with temp_ctx as dir_temp:
        # dir_temp=xx
        tempDir_1 = Path(dir_temp) / "temp1"
        tempDir_2 = Path(dir_temp) / "temp2"
        i = chain_ver[0]

        # Create temporary folders
        if os.path.exists(tempDir_1) is False:
            os.mkdir(tempDir_1)
        if os.path.exists(tempDir_2) is False:
            os.mkdir(tempDir_2)

        # flatten all file structures in tempDir_1
        # locate input folder
        ser_nml = load_SUEWS_nml_simple(
            str(Path(fromDir) / "RunControl.nml")
        ).runcontrol
        path_input = (Path(fromDir) / ser_nml["fileinputpath"]).resolve()
        list_table_input = (
            list(
                path_input.glob("SUEWS_*.txt")
            )  # Fixed: Added underscore to match SUEWS_*.txt files
            + list(path_input.glob("*.nml"))
            + list(Path(fromDir).resolve().glob("*.nml"))
            + list(
                Path(fromDir).resolve().glob("SUEWS_*.txt")
            )  # Also check root for SUEWS_*.txt files
        )
        # copy flattened files into tempDir_1 for later processing
        # also convert all files to UTF-8 encoding in case inconsistent encoding exists
        for fileX in list_table_input:
            # print(fileX)
            path_dst = Path(tempDir_1) / fileX.name
            copyfile(fileX.resolve(), path_dst)
            convert_utf8(path_dst)
            # Clean legacy table files once at the beginning
            if path_dst.suffix == ".txt":
                logger_supy.debug(f"Cleaning original file: {fileX.name}")
                clean_legacy_table(path_dst)

        # Indirect version conversion process
        # The alternation logic needs to account for starting position
        # Files start in tempDir_1, so first conversion should read from tempDir_1
        while i > 1:
            logger_supy.info("**************************************************")
            logger_supy.info(f"working on: {chain_ver[i + 1]} --> {chain_ver[i]}")

            # Create snapshot directory for this step if in debug mode
            if debug_dir is not None:
                snapshot_dir = (
                    Path(dir_temp) / f"step_{chain_ver[i + 1]}_to_{chain_ver[i]}"
                )
                snapshot_dir.mkdir(exist_ok=True)

            # Fix the alternation logic: if chain starts with even length, first step should be from temp1
            # Original length is chain_ver[0], current step is i
            # If (original_length - i) is even, use temp1 -> temp2, else temp2 -> temp1
            steps_completed = chain_ver[0] - i

            if steps_completed % 2 == 0:
                # Even number of steps completed (including 0), so temp1 -> temp2
                SUEWS_Converter_single(
                    tempDir_1, tempDir_2, chain_ver[i + 1], chain_ver[i]
                )

                # Validate and fix profiles after conversion if enabled
                if validate_profiles:
                    try:
                        profile_manager = ProfileManager(
                            tempDir_2 / "SUEWS_Profiles.txt"
                        )
                        profile_manager.ensure_required_profiles(tempDir_2)
                        if profile_manager.missing_profiles:
                            logger_supy.info(
                                f"Fixed {len(profile_manager.missing_profiles)} missing profile references: {sorted(profile_manager.missing_profiles)}"
                            )
                    except Exception as e:
                        logger_supy.warning(f"Profile validation skipped: {e}")

                # Save snapshot in debug mode
                if debug_dir is not None:
                    for file in Path(tempDir_2).glob("*"):
                        copyfile(file, snapshot_dir / file.name)
                    logger_supy.info(
                        f"Debug: Saved snapshot of {chain_ver[i]} in {snapshot_dir}"
                    )

                # Remove input temporary folders only if not in debug mode
                if debug_dir is None:
                    rmtree(tempDir_1, ignore_errors=True)
                else:
                    # In debug mode, preserve intermediate results
                    logger_supy.info(
                        f"Debug: Preserved intermediate files in {tempDir_2}"
                    )

            else:
                # Odd number of steps completed, so temp2 -> temp1
                SUEWS_Converter_single(
                    tempDir_2, tempDir_1, chain_ver[i + 1], chain_ver[i]
                )

                # Validate and fix profiles after conversion if enabled
                if validate_profiles:
                    try:
                        profile_manager = ProfileManager(
                            tempDir_1 / "SUEWS_Profiles.txt"
                        )
                        profile_manager.ensure_required_profiles(tempDir_1)
                        if profile_manager.missing_profiles:
                            logger_supy.info(
                                f"Fixed {len(profile_manager.missing_profiles)} missing profile references: {sorted(profile_manager.missing_profiles)}"
                            )
                    except Exception as e:
                        logger_supy.warning(f"Profile validation skipped: {e}")

                # Save snapshot in debug mode
                if debug_dir is not None:
                    for file in Path(tempDir_1).glob("*"):
                        copyfile(file, snapshot_dir / file.name)
                    logger_supy.info(
                        f"Debug: Saved snapshot of {chain_ver[i]} in {snapshot_dir}"
                    )

                # Remove input temporary folders only if not in debug mode
                if debug_dir is None:
                    rmtree(tempDir_2, ignore_errors=True)
                else:
                    # In debug mode, preserve intermediate results
                    logger_supy.info(
                        f"Debug: Preserved intermediate files in {tempDir_1}"
                    )
            logger_supy.info("**************************************************")
            i -= 1

        logger_supy.info("**************************************************")
        logger_supy.info(f"working on: {chain_ver[i + 1]} --> {chain_ver[i]}")

        # Determine which temp directory has the final results
        # After the loop, we've completed (chain_ver[0] - 1) steps
        total_steps = chain_ver[0] - 1
        if total_steps % 2 == 0:
            # Even number of steps means files are in tempDir_1
            final_source = tempDir_1
        else:
            # Odd number of steps means files are in tempDir_2
            final_source = tempDir_2

        SUEWS_Converter_single(final_source, toDir, chain_ver[2], chain_ver[1])

        # Final profile validation
        if validate_profiles:
            try:
                profile_manager = ProfileManager(
                    Path(toDir) / "input" / "SUEWS_Profiles.txt"
                )
                profile_manager.ensure_required_profiles(Path(toDir) / "input")
                if profile_manager.missing_profiles:
                    logger_supy.info(
                        f"Final profile validation: Fixed {len(profile_manager.missing_profiles)} missing profiles"
                    )
                    logger_supy.info(
                        f"Missing profile codes: {sorted(profile_manager.missing_profiles)}"
                    )
            except Exception:
                # Try the toDir directly if input dir doesn't exist yet
                try:
                    profile_manager = ProfileManager(Path(toDir) / "SUEWS_Profiles.txt")
                    profile_manager.ensure_required_profiles(Path(toDir))
                    if profile_manager.missing_profiles:
                        logger_supy.info(
                            f"Final profile validation: Fixed {len(profile_manager.missing_profiles)} missing profiles"
                        )
                except Exception as e2:
                    logger_supy.warning(f"Final profile validation skipped: {e2}")

        # Save final snapshot in debug mode
        if debug_dir is not None:
            snapshot_dir = (
                Path(dir_temp) / f"step_{chain_ver[2]}_to_{chain_ver[1]}_final"
            )
            snapshot_dir.mkdir(exist_ok=True)
            for file in Path(toDir).glob("*"):
                if file.is_file():
                    copyfile(file, snapshot_dir / file.name)
            logger_supy.info(f"Debug: Saved final snapshot in {snapshot_dir}")
        logger_supy.info("**************************************************")

        # Remove temporary folders unless in debug mode
        if debug_dir is None:
            rmtree(tempDir_1, ignore_errors=True)
            rmtree(tempDir_2, ignore_errors=True)

    # cleaning and move input tables into the `input` folder
    ser_nml = load_SUEWS_nml_simple(str(Path(toDir) / "RunControl.nml")).runcontrol

    path_input = (Path(toDir) / ser_nml["fileinputpath"]).resolve()
    path_output = (Path(toDir) / ser_nml["fileoutputpath"]).resolve()
    path_input.mkdir(exist_ok=True)
    path_output.mkdir(exist_ok=True)

    list_table_input = list(Path(toDir).glob("SUEWS*.txt")) + [
        x for x in Path(toDir).glob("*.nml") if "RunControl" not in str(x)
    ]

    for fileX in list_table_input:
        # Check if we need to rename InitialConditions files when multipleinitfiles == 0
        target_name = fileX.name
        if (
            "InitialConditions" in fileX.name
            and ser_nml.get("multipleinitfiles", 0) == 0
        ):
            # Remove grid number from filename (e.g., InitialConditionsKc1_2011.nml -> InitialConditionsKc_2011.nml)
            # Pattern to match InitialConditionsXXX#_YYYY.nml where XXX is filecode, # is grid number, YYYY is year
            pattern = r"(InitialConditions[A-Za-z]+)\d+(_\d{4}\.nml)"
            new_name = re.sub(pattern, r"\1\2", fileX.name)
            if new_name != fileX.name:
                target_name = new_name
                logger_supy.debug(
                    f"Renaming {fileX.name} to {target_name} (multipleinitfiles=0)"
                )

        move(fileX.resolve(), path_input / target_name)


# get file encoding type
def get_encoding_type(file):
    with open(file, "rb") as f:
        rawdata = f.read()
    return detect(rawdata)["encoding"]


def convert_utf8(file_src):
    path_src = Path(file_src).resolve()
    from_codec = get_encoding_type(path_src)
    logger_supy.debug(f"encoding {from_codec} detected in {path_src.name}")

    with TemporaryDirectory() as dir_temp:
        path_dst = Path(dir_temp) / "out-UTF8.txt"
        path_dst.touch()

        # add try: except block for reliability
        try:
            with (
                open(path_src, encoding=from_codec) as f,
                open(path_dst, "w", encoding="utf-8") as e,
            ):
                text = f.read()  # for small files, for big use chunks
                e.write(text)

            os.remove(path_src)  # remove old encoding file
            try:
                path_dst.rename(path_src)
            except OSError as e:
                if e.errno == 18:
                    logger_supy.error("Invalid cross-link device")
                    shutil.copy2(path_dst, path_src)
                    os.remove(path_dst)
                else:
                    raise e

            # os.rename(trgfile, srcfile) # rename new encoding
        except UnicodeDecodeError:
            logger_supy.error("Decode Error")
        except UnicodeEncodeError:
            logger_supy.error("Encode Error")
