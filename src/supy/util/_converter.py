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
import os
import os.path
import sys

# ignore warnings raised by numpy when reading-in -9 lines
import warnings
from collections import defaultdict
from fnmatch import fnmatch
from heapq import heappop, heappush
from pathlib import Path
from shutil import copyfile, move, rmtree
from tempfile import TemporaryDirectory
from contextlib import nullcontext
import shutil

import f90nml
import numpy as np
import pandas as pd
from chardet import detect

from .._env import logger_supy, trv_supy_module
from .._load import load_SUEWS_nml_simple

warnings.filterwarnings("ignore")
########################################################
# %%
# load the rule file
rules = pd.read_csv(trv_supy_module / "util" / "rules.csv")
list_ver_from = rules["From"].unique().tolist()
list_ver_to = rules["To"].unique().tolist()

# %%
########################################################
# define action functions:
# the current supported actions:
# rename, delete, add, move


# rename:
# rename file
def rename_file(toFile, toVar, toCol, toVal):
    # toVar, toCol are ignored
    if not Path(toFile).exists():
        logger_supy.error(f"{toFile} not existing")
        sys.exit()
    else:
        dir = Path(toFile).resolve().parent
        path_toFile_renamed = dir / toVal
        os.rename(toFile, path_toFile_renamed)


# rename variable
def rename_var(toFile, toVar, toCol, toVal):
    # if namelist:
    if toFile.endswith(".nml"):
        logger_supy.info(f"{toFile} {toVar} {toVal}")
        rename_var_nml(toFile, toVar, toVal)
    else:
        # First, read the file to find where data ends (before -9 lines)
        with open(toFile, encoding='utf-8') as f:
            lines = f.readlines()

        # Find where data ends (first line starting with -9)
        data_end_idx = len(lines)
        for i, line in enumerate(lines):
            if line.strip().startswith('-9'):
                data_end_idx = i
                break

        # Read only the data portion
        try:
            dataX = pd.read_csv(
                toFile,
                delim_whitespace=True,
                comment='!',
                encoding='UTF8',
                skiprows=2,  # Skip both header lines
                nrows=data_end_idx - 2 if data_end_idx > 2 else None,
                header=None
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
            " ".join(str(i + 1) for i in range(len(headers)))
            + "\n"
            + " ".join(headers)
        )

        # Convert to string
        dataX = dataX.astype(str)

        # Write the file
        with open(toFile, 'w', encoding='utf-8') as f:
            f.write(headerLine + "\n")
            dataX.to_csv(f, sep=' ', index=False, header=False)
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
def delete_var(toFile, toVar, toCol, toVal):
    if toFile.endswith(".nml"):
        delete_var_nml(toFile, toVar, toVal)
    else:
        # First, read the file to find where data ends (before -9 lines)
        with open(toFile, encoding='utf-8') as f:
            lines = f.readlines()

        # Find where data ends (first line starting with -9)
        data_end_idx = len(lines)
        for i, line in enumerate(lines):
            if line.strip().startswith('-9'):
                data_end_idx = i
                break

        # Read only the data portion
        try:
            dataX = pd.read_csv(
                toFile,
                delim_whitespace=True,
                comment='!',
                encoding='UTF8',
                skiprows=2,  # Skip both header lines
                nrows=data_end_idx - 2 if data_end_idx > 2 else None,
                header=None
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
            " ".join(str(i + 1) for i in range(len(headers)))
            + "\n"
            + " ".join(headers)
        )

        # Convert to string
        dataX = dataX.astype(str)

        # Write the file
        with open(toFile, 'w', encoding='utf-8') as f:
            f.write(headerLine + "\n")
            dataX.to_csv(f, sep=' ', index=False, header=False)
            # NO footer lines - these are legacy and should not be added

        logger_supy.debug(f"Deleted column {toVar} from {toFile}")
        return


def delete_var_nml(toFile, toVar, toVal):
    nml = f90nml.read(toFile)
    toVarX = toVar.lower()
    title = next(iter(nml.keys()))
    if toVarX in nml[title]:
        nml[title].pop(toVarX)
    else:
        logger_supy.warning(f"{toVar} does not exist!")
    nml.write(toFile, force=True)


def sanitize_legacy_suews_file(file_path, output_path=None):
    r"""
    Sanitize legacy SUEWS table files (particularly 2016a format) for pandas compatibility.

    This function:
    - Removes inline comments (text after ! character)
    - Standardizes line endings (removes \r)
    - Removes empty trailing columns
    - Ensures consistent column counts
    - Handles tab-separated values
    - Removes ALL lines that start with -9 (legacy footers)

    Args:
        file_path: Path to the input file
        output_path: Optional path for sanitized output (if None, overwrites input)

    Returns:
        Path to the sanitized file
    """
    if output_path is None:
        output_path = file_path

    logger_supy.debug(f"Sanitizing legacy file: {file_path}")

    with open(file_path, encoding='utf-8', errors='replace') as f:
        lines = f.readlines()

    if len(lines) < 2:
        logger_supy.warning(f"File {file_path} has less than 2 lines, skipping sanitization")
        return file_path

    header_lines = []  # Store header lines (first 2 lines)
    data_lines = []  # Store data lines
    header_col_count = None
    line_count = 0  # Track non-empty lines

    for i, line in enumerate(lines):
        # Remove carriage returns and trailing whitespace
        line = line.replace('\r', '').rstrip()

        # IMPORTANT: Replace all tabs with spaces for consistent parsing
        line = line.replace('\t', ' ')

        if not line or line.strip().startswith('#'):
            continue  # Skip empty lines and full-line comments

        # Skip lines that contain triple quotes or problematic quoted comments
        # These are typically metadata lines in 2016a format that shouldn't be data
        if '"""' in line or ('"' in line and ('Vegetation (average)' in line or 'used for' in line)):
            logger_supy.debug(f"Skipping line {i+1} with problematic quoted comments: {line[:50]}...")
            continue

        # IMPORTANT: Skip ALL lines starting with -9 (legacy footers that should be removed)
        if line.strip().startswith('-9'):
            logger_supy.debug(f"Removing legacy footer line {i+1}: {line[:50]}... Stopping read after footer.")
            break  # Stop processing any further lines after footer

        # Remove inline comments (everything after !)
        if '!' in line:
            line = line[:line.index('!')].rstrip()

        # Split by spaces (tabs have been replaced with spaces)
        fields = line.split()

        # Skip empty lines after processing
        if not fields:
            continue

        # For the header rows (first 2 non-empty lines), establish column count
        if line_count < 2:
            if header_col_count is None:
                header_col_count = len(fields)
                logger_supy.debug(f"Header column count set to {header_col_count} at line {i+1}")
            # Store header line
            header_lines.append(' '.join(fields))
            line_count += 1
            continue

        # For data lines
        line_count += 1

        # Ensure consistent column count (pad or truncate as needed)
        if header_col_count and len(fields) != header_col_count:
            if len(fields) > header_col_count:
                # Truncate extra fields (likely comments)
                logger_supy.debug(f"Line {i+1}: Truncating from {len(fields)} to {header_col_count} fields")
                fields = fields[:header_col_count]
            else:
                # Pad with -999 for missing fields
                while len(fields) < header_col_count:
                    fields.append('-999')

        # Store processed data line
        data_lines.append(' '.join(fields))

    # Combine header and data lines
    sanitized_lines = header_lines + data_lines

    # Note: We do NOT add footer lines - the -9 lines are removed entirely

    # Write sanitized content
    with open(output_path, 'w', encoding='utf-8') as f:
        f.write('\n'.join(sanitized_lines))
        if sanitized_lines and not sanitized_lines[-1].endswith('\n'):
            f.write('\n')

    logger_supy.debug(f"Sanitized file written to: {output_path}")
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
        logger_supy.error(f"Failed to read {toFile}: {str(e)}")
        raise

# add:
# add variable(s) to a file
def add_var(toFile, toVar, toCol, toVal):
    # if namelist:
    if toFile.endswith(".nml"):
        add_var_nml(toFile, toVar, toVal)
    else:
        # First, read the file to find where data ends (before -9 lines)
        with open(toFile, encoding='utf-8') as f:
            lines = f.readlines()

        # Find where data ends (first line starting with -9)
        data_end_idx = len(lines)
        for i, line in enumerate(lines):
            if line.strip().startswith('-9'):
                data_end_idx = i
                break

        # Read only the data portion (skip headers and footers)
        try:
            # Use pandas to read only the data lines
            dataX = pd.read_csv(
                toFile,
                delim_whitespace=True,  # Faster C engine
                comment='!',
                encoding='UTF8',
                skiprows=2,  # Skip both header lines
                nrows=data_end_idx - 2 if data_end_idx > 2 else None,  # Read only data rows
                header=None  # No header in data
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
            " ".join(str(i + 1) for i in range(len(headers)))
            + "\n"
            + " ".join(headers)
        )

        # Save the dataframe to file
        # Convert to string to ensure all values are saved as text
        if not dataX.empty:
            dataX = dataX.astype(str)

        # Write the file with headers
        with open(toFile, 'w', encoding='utf-8') as f:
            # Write header lines
            f.write(headerLine + "\n")
            # Write data without index (only if there's data)
            if not dataX.empty:
                dataX.to_csv(f, sep=' ', index=False, header=False)
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
            if '.' not in str(toVal):
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


# a single conversion between two versions
def SUEWS_Converter_single(fromDir, toDir, fromVer, toVer):
    # copy files in fromDir to toDir, only: *.nml, SUEWS_*.txt
    if os.path.exists(toDir) is False:
        os.mkdir(toDir)
    fileList = []

    # Special case: if fromVer == toVer, just copy and sanitize without conversion
    if fromVer == toVer:
        logger_supy.info(f"Source and target versions are the same ({fromVer}). Only sanitizing files...")

        # Determine file structure based on version
        if fromVer == "2016a":
            # Look for files in Input/ subdirectory for 2016a format
            input_dir = os.path.join(fromDir, "Input")
            if os.path.exists(input_dir):
                for fileX in os.listdir(input_dir):
                    if fnmatch(fileX, "SUEWS_*.txt") or fnmatch(fileX, "*.nml"):
                        file_src = os.path.join(input_dir, fileX)
                        file_dst = os.path.join(toDir, fileX)
                        copyfile(file_src, file_dst)
                        convert_utf8(file_dst)
                        if fnmatch(fileX, "*.txt"):
                            sanitize_legacy_suews_file(file_dst)
            # Also check root for .nml files
            for fileX in os.listdir(fromDir):
                if fnmatch(fileX, "*.nml"):
                    file_src = os.path.join(fromDir, fileX)
                    file_dst = os.path.join(toDir, fileX)
                    copyfile(file_src, file_dst)
                    convert_utf8(file_dst)
        else:
            # Standard structure - all files in root
            for fileX in os.listdir(fromDir):
                if fnmatch(fileX, "SUEWS*.txt") or fnmatch(fileX, "*.nml"):
                    file_src = os.path.join(fromDir, fileX)
                    file_dst = os.path.join(toDir, fileX)
                    copyfile(file_src, file_dst)
                    convert_utf8(file_dst)
                    if fnmatch(fileX, "*.txt"):
                        sanitize_legacy_suews_file(file_dst)

        # Create the standard directory structure
        ser_nml = load_SUEWS_nml_simple(str(Path(toDir) / "RunControl.nml")).runcontrol
        path_input = (Path(toDir) / ser_nml["fileinputpath"]).resolve()
        path_output = (Path(toDir) / ser_nml["fileoutputpath"]).resolve()
        path_input.mkdir(exist_ok=True)
        path_output.mkdir(exist_ok=True)

        # Move table files to Input directory
        list_table_input = [x for x in Path(toDir).glob("SUEWS*.txt")] + [
            x for x in Path(toDir).glob("*.nml") if "RunControl" not in str(x)
        ]
        for fileX in list_table_input:
            move(fileX.resolve(), path_input / fileX.name)

        logger_supy.info(f"Files sanitized and copied to {toDir}")
        return

    # Normal conversion process continues below
    fileList = []

    # Check for 2016a structure with Input/ subdirectory
    if fromVer == "2016a":
        # Look for files in Input/ subdirectory for 2016a format
        input_dir = os.path.join(fromDir, "Input")
        if os.path.exists(input_dir):
            logger_supy.debug(f"Found Input/ subdirectory for {fromVer}, scanning for SUEWS_*.txt files")
            for fileX in os.listdir(input_dir):
                if fnmatch(fileX, "SUEWS_*.txt"):
                    fileList.append(("Input", fileX))
                    logger_supy.debug(f"Found file in Input/: {fileX}")
        # Also check root for .nml files and ALL txt files (including SUEWS_*.txt that might be in root)
        for fileX in os.listdir(fromDir):
            if fnmatch(fileX, "*.nml") or fnmatch(fileX, "SUEWS_*.txt") or fnmatch(fileX, "*.txt"):
                fileList.append(("", fileX))
                logger_supy.debug(f"Found file in root: {fileX}")
    else:
        # Standard structure - all files in root
        for fileX in os.listdir(fromDir):
            if any(fnmatch(fileX, p) for p in ["SUEWS*.txt", "*.nml"]):
                fileList.append(("", fileX))

    for subdir, fileX in fileList:
        file_src = os.path.join(fromDir, subdir, fileX) if subdir else os.path.join(fromDir, fileX)
        # Always copy to root of toDir (flattening the structure)
        file_dst = os.path.join(toDir, fileX)
        logger_supy.debug(f"Copying {file_src} to {file_dst}")
        copyfile(file_src, file_dst)
        convert_utf8(file_dst)

    # Sanitize ALL txt table files at the beginning for 2016a format
    # This ensures all table files are properly formatted before any operations
    if fromVer == "2016a":
        logger_supy.info("Sanitizing all 2016a table files before conversion...")
        for fileX in os.listdir(toDir):
            if fnmatch(fileX, "*.txt"):  # Apply to ALL .txt files, not just SUEWS_*.txt
                file_path = os.path.join(toDir, fileX)
                logger_supy.debug(f"Sanitizing 2016a file: {fileX}")
                sanitize_legacy_suews_file(file_path)

    # Special handling: Create SPARTACUS.nml when converting to 2024a or later
    # This file doesn't exist in earlier versions but is expected in 2024a+
    if fromVer in ["2023a", "2021a", "2020a", "2019b", "2019a", "2018c", "2018b", "2018a", "2017a", "2016a"] and toVer in ["2024a", "2025a"]:
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
            with open(spartacus_path, 'w') as f:
                f.write(spartacus_content)
            logger_supy.info(f"Created placeholder SUEWS_SPARTACUS.nml for {toVer}")

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
        logger_supy.info(f"Files without rules (will be preserved): {list(files_without_rules)}")

    # Combine both sets
    filesToConvert = filesToConvert | files_without_rules

    logger_supy.info(f"filesToConvert: {list(filesToConvert)}")

    for fileX in filesToConvert:
        logger_supy.info(f"working on file: {fileX}")
        try:
            actionList = rules.values[posRules].compress(
                rules["File"].values[posRules] == fileX, axis=0
            )

            # If no rules exist for this file, it will just be copied as-is (already done in SUEWS_Converter_single)
            if len(actionList) == 0:
                logger_supy.info(f"No conversion rules for {fileX}, file preserved as-is")
                continue

            actionList = actionList[:, 2:]
            # actionList = np.array(actionList.tolist())[:, 2:].astype('S140')
            # prepend toDir to fileX
            actionList[:, 1] = os.path.join(toDir, fileX)
            # print('actionList:', actionList)
            SUEWS_Converter_file(os.path.join(toDir, fileX), actionList)
        except Exception as e:
            logger_supy.error(f"Failed to convert {fileX} from {fromVer} to {toVer}: {str(e)}")
            # Don't continue with a broken conversion - fail fast
            raise RuntimeError(f"Conversion stopped at {fileX}: {str(e)}")


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
    if not Path(fileX).exists():
        # Only create placeholder for .txt files, not .nml files
        if fileX.endswith('.txt'):
            # Create placeholder WITHOUT footer lines
            Path(fileX).write_text("1\nCode\n800\n", encoding="UTF8")
            logger_supy.debug(f"Created placeholder for missing file: {fileX}")
        elif fileX.endswith('.nml'):
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
            logger_supy.error(f"Failed to perform action {action[0]} on {fileX}: {str(e)}")
            raise RuntimeError(f"Conversion failed at {action[0]} for {fileX}: {str(e)}")


def keep_file(toFile, var, col, val):
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
    return


def dijkstra(edges, f, t):
    g = defaultdict(list)
    for l, r, c in edges:
        g[l].append((c, r))
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
def convert_table(fromDir, toDir, fromVer, toVer, debug_dir=None):
    # Special case: if fromVer == toVer, just sanitize without conversion
    if fromVer == toVer:
        logger_supy.info(f"Source and target versions are the same ({fromVer}). Only sanitizing files...")
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
        logger_supy.info(f"Debug mode: intermediate files will be saved in {debug_path}")
    
    # use a persistent directory when debug_dir is provided
    temp_ctx = TemporaryDirectory() if debug_dir is None else nullcontext(str(debug_path) if debug_dir else None)
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
            [x for x in path_input.glob("SUEWS_*.txt")]  # Fixed: Added underscore to match SUEWS_*.txt files
            + [x for x in path_input.glob("*.nml")]
            + [x for x in Path(fromDir).resolve().glob("*.nml")]
            + [x for x in Path(fromDir).resolve().glob("SUEWS_*.txt")]  # Also check root for SUEWS_*.txt files
        )
        # copy flattened files into tempDir_1 for later processing
        # also convert all files to UTF-8 encoding in case inconsistent encoding exists
        for fileX in list_table_input:
            # print(fileX)
            path_dst = Path(tempDir_1) / fileX.name
            copyfile(fileX.resolve(), path_dst)

        # Indirect version conversion process
        while i > 1:
            logger_supy.info("**************************************************")
            logger_supy.info(f"working on: {chain_ver[i + 1]} --> {chain_ver[i]}")
            
            # Create snapshot directory for this step if in debug mode
            if debug_dir is not None:
                snapshot_dir = Path(dir_temp) / f"step_{chain_ver[i + 1]}_to_{chain_ver[i]}"
                snapshot_dir.mkdir(exist_ok=True)
            
            if i % 2:
                # tempDir_2 = "temp2"
                SUEWS_Converter_single(
                    tempDir_1, tempDir_2, chain_ver[i + 1], chain_ver[i]
                )
                
                # Save snapshot in debug mode
                if debug_dir is not None:
                    for file in Path(tempDir_2).glob("*"):
                        copyfile(file, snapshot_dir / file.name)
                    logger_supy.info(f"Debug: Saved snapshot of {chain_ver[i]} in {snapshot_dir}")
                
                # tempDir_1 = "temp1"
                # Remove input temporary folders only if not in debug mode
                if debug_dir is None:
                    rmtree(tempDir_1, ignore_errors=True)
                else:
                    # In debug mode, preserve intermediate results
                    logger_supy.info(f"Debug: Preserved intermediate files in {tempDir_2}")

            else:
                # tempDir_1 = "temp1"
                SUEWS_Converter_single(
                    tempDir_2, tempDir_1, chain_ver[i + 1], chain_ver[i]
                )
                
                # Save snapshot in debug mode
                if debug_dir is not None:
                    for file in Path(tempDir_1).glob("*"):
                        copyfile(file, snapshot_dir / file.name)
                    logger_supy.info(f"Debug: Saved snapshot of {chain_ver[i]} in {snapshot_dir}")
                
                # tempDir_2 = "temp2"
                # Remove input temporary folders only if not in debug mode
                if debug_dir is None:
                    rmtree(tempDir_2, ignore_errors=True)
                else:
                    # In debug mode, preserve intermediate results
                    logger_supy.info(f"Debug: Preserved intermediate files in {tempDir_1}")
                # this loop always break in this part
            logger_supy.info("**************************************************")
            i -= 1

        logger_supy.info("**************************************************")
        logger_supy.info(f"working on: {chain_ver[i + 1]} --> {chain_ver[i]}")
        SUEWS_Converter_single(tempDir_1, toDir, chain_ver[2], chain_ver[1])
        
        # Save final snapshot in debug mode
        if debug_dir is not None:
            snapshot_dir = Path(dir_temp) / f"step_{chain_ver[2]}_to_{chain_ver[1]}_final"
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

    list_table_input = [x for x in Path(toDir).glob("SUEWS*.txt")] + [
        x for x in Path(toDir).glob("*.nml") if "RunControl" not in str(x)
    ]

    for fileX in list_table_input:
        move(fileX.resolve(), path_input / fileX.name)


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
