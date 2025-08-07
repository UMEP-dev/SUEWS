"""YAML conversion functionality for SUEWS configuration."""

import tempfile
from pathlib import Path
import click
import f90nml

from ..._load import load_InitialCond_grid_df
from ...data_model.core import SUEWSConfig
from ...data_model.model import OutputConfig
from .table import convert_table


def convert_to_yaml(
    input_dir: str,
    output_file: str,
    from_ver: str = None,
    debug_dir: str = None,
    validate_profiles: bool = True,
):
    """Convert SUEWS table-based input to YAML configuration.
    
    Args:
        input_dir: Directory with SUEWS table files
        output_file: Output YAML file path
        from_ver: Source version (auto-detect if None)
        debug_dir: Directory for debug files (optional)
        validate_profiles: Whether to validate profiles
    
    Returns:
        None
    """
    input_path = Path(input_dir)
    output_path = Path(output_file)
    
    processing_dir = input_path
    temp_dir_obj = None
    
    try:
        if from_ver:
            # Convert to 2025a table format first
            to_ver = "2025a"
            click.echo(
                f"Step 1: Converting tables from version {from_ver} to {to_ver}..."
            )
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
            processing_dir = temp_dir_path
            click.echo(
                f"Table conversion complete. Using converted tables in: {processing_dir}"
            )
        
        path_runcontrol = processing_dir / "RunControl.nml"
        if not path_runcontrol.exists():
            raise click.ClickException(f"RunControl.nml not found in {processing_dir}")
        
        click.echo("Step 2: Loading SUEWS input tables into data model...")
        try:
            df_state = load_InitialCond_grid_df(path_runcontrol)
        except Exception as e:
            raise click.ClickException(f"Failed to load SUEWS tables: {e}")
        
        click.echo("Step 3: Creating Pydantic configuration object...")
        try:
            config = SUEWSConfig.from_df_state(df_state)
            
            # Set the forcing file path based on RunControl settings
            runcontrol_data = f90nml.read(str(path_runcontrol))
            if "runcontrol" in runcontrol_data:
                runcontrol = runcontrol_data["runcontrol"]
                filecode = runcontrol.get("filecode", "forcing")
                
                # Look for forcing files matching the pattern
                input_path = Path(input_dir)
                input_subdir = (
                    input_path / "Input"
                    if (input_path / "Input").exists()
                    else input_path
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
                writeoutoption = runcontrol.get("writeoutoption", 0)
                
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
        
        except Exception as e:
            raise click.ClickException(f"Failed to create configuration: {e}")
        
        click.echo(f"Step 4: Saving configuration to YAML file: {output_path}...")
        config.to_yaml(output_path)
        
        click.secho(f"Successfully converted to {output_path}", fg="green")
    
    finally:
        if temp_dir_obj:
            temp_dir_obj.cleanup()