"""YAML conversion functionality for SUEWS configuration."""

from pathlib import Path
import tempfile
from typing import Optional

import click
import f90nml

from ..._load import load_InitialCond_grid_df
from ...data_model.core import SUEWSConfig
from ...data_model.core.model import OutputConfig
from .table import convert_table


def _prepare_processing_dir(
    input_path: Path,
    from_ver: Optional[str],
    debug_dir: Optional[str],
    validate_profiles: bool,
) -> tuple[Path, Optional[tempfile.TemporaryDirectory]]:
    """Prepare processing directory, converting tables if needed.

    Returns
    -------
        Tuple of (processing_dir, temp_dir_obj)
    """
    if not from_ver:
        return input_path, None

    # Convert to 2025a table format first
    to_ver = "2025a"
    click.echo(f"Step 1: Converting tables from version {from_ver} to {to_ver}...")
    temp_dir_obj = tempfile.TemporaryDirectory()
    temp_dir_path = Path(temp_dir_obj.name)
    convert_table(
        str(input_path),
        str(temp_dir_path),
        from_ver,
        to_ver,
        debug_dir=debug_dir,
        validate_profiles=validate_profiles,
    )
    click.echo(f"Table conversion complete. Using converted tables in: {temp_dir_path}")
    return temp_dir_path, temp_dir_obj


def _configure_forcing_and_output(
    config: SUEWSConfig,
    path_runcontrol: Path,
    input_dir: str,
) -> None:
    """Configure forcing file and output settings based on RunControl."""
    runcontrol_data = f90nml.read(str(path_runcontrol))
    if "runcontrol" not in runcontrol_data:
        return

    runcontrol = runcontrol_data["runcontrol"]
    filecode = runcontrol.get("filecode", "forcing")

    # Look for forcing files matching the pattern
    input_path = Path(input_dir)
    input_subdir = (
        input_path / "Input" if (input_path / "Input").exists() else input_path
    )

    # Pattern: {filecode}_{year}_data_{resolution}.txt
    forcing_files = list(input_subdir.glob(f"{filecode}_*_data_*.txt"))

    if forcing_files:
        # Use the first matching file
        forcing_file = forcing_files[0].name
        config.model.control.forcing_file = forcing_file
        click.echo(
            f"  - Set forcing file to: {forcing_file} (from FileCode='{filecode}')"
        )
    else:
        # If no matching files found, construct expected name
        click.echo(
            f"  - Warning: No forcing file found matching pattern {filecode}_*_data_*.txt"
        )

    # Set proper output configuration
    resolutionfilesout = runcontrol.get("resolutionfilesout", 3600)

    # Create OutputConfig based on RunControl settings
    output_config = OutputConfig(
        format="txt",  # Default to txt format for compatibility
        freq=resolutionfilesout if resolutionfilesout > 0 else 3600,
        groups=["SUEWS", "DailyState"],  # Default groups
    )

    config.model.control.output_file = output_config
    click.echo(
        f"  - Set output configuration: format={output_config.format}, freq={output_config.freq}s"
    )


def convert_to_yaml(
    input_file: str,
    output_file: str,
    from_ver: Optional[str] = None,
    debug_dir: Optional[str] = None,
    validate_profiles: bool = True,
):
    """Convert SUEWS input to YAML configuration.

    Supports both:
    1. Table-based input (RunControl.nml)
    2. df_state input (CSV or pickle files)

    Args:
        input_file: Path to input file (RunControl.nml or df_state CSV/pickle)
        output_file: Output YAML file path
        from_ver: Source version (for table conversion, ignored for df_state)
        debug_dir: Directory for debug files (optional)
        validate_profiles: Whether to validate profiles

    Returns
    -------
        None
    """
    from . import detect_input_type
    from .df_state import (
        load_df_state_file,
        detect_df_state_version,
        convert_df_state_format,
        validate_converted_df_state,
    )

    input_path = Path(input_file)
    output_path = Path(output_file)

    temp_dir_obj = None

    try:
        # Detect input type from file
        input_type = detect_input_type(input_path)

        if input_type == "df_state":
            # df_state conversion path
            click.echo(f"Processing df_state file: {input_path.name}")

            # Load df_state
            df_state = load_df_state_file(input_path)
            click.echo(
                f"  Loaded df_state: {df_state.shape[0]} grids, {len(df_state.columns)} columns"
            )

            # Check version and convert if needed
            version = detect_df_state_version(df_state)
            click.echo(f"  Detected format: {version}")

            if version == "old":
                click.echo("  Converting from old format to current...")
                df_state = convert_df_state_format(df_state)

                # Validate
                is_valid, message = validate_converted_df_state(df_state)
                if is_valid:
                    click.echo(f"  [OK] {message}")
                else:
                    click.echo(f"  [WARNING] {message}", err=True)
            elif version == "unknown":
                click.echo("  Warning: Unknown format, attempting conversion anyway...")
                df_state = convert_df_state_format(df_state)

            # Create config from df_state
            click.echo("Creating YAML configuration...")
            config = SUEWSConfig.from_df_state(df_state)

        elif input_type == "nml":
            # Table conversion path - extract directory from nml path
            table_dir = input_path.parent
            click.echo(f"Processing table files from: {table_dir}")

            # Prepare processing directory
            processing_dir, temp_dir_obj = _prepare_processing_dir(
                table_dir, from_ver, debug_dir, validate_profiles
            )

            path_runcontrol = processing_dir / "RunControl.nml"
            if not path_runcontrol.exists():
                raise click.ClickException(
                    f"RunControl.nml not found in {processing_dir}"
                )

            click.echo("Loading SUEWS input tables...")
            try:
                df_state = load_InitialCond_grid_df(path_runcontrol)
            except Exception as e:
                raise click.ClickException(f"Failed to load SUEWS tables: {e}") from e

            click.echo("Creating YAML configuration...")
            try:
                config = SUEWSConfig.from_df_state(df_state)

                # Configure forcing file and output settings
                _configure_forcing_and_output(config, path_runcontrol, str(table_dir))

            except Exception as e:
                raise click.ClickException(
                    f"Failed to create configuration: {e}"
                ) from e
        else:
            raise click.ClickException(f"Unexpected input type: {input_type}")

        # Save to YAML
        click.echo(f"Saving to: {output_path}")
        config.to_yaml(output_path)

    finally:
        if temp_dir_obj:
            temp_dir_obj.cleanup()
