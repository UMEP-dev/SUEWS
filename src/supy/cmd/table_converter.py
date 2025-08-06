# command line tools
import click
import sys
from pathlib import Path

from ..util._converter import convert_table, list_ver_from, list_ver_to


@click.command()
@click.option(
    "-f",
    "--from",
    "fromVer",
    help="Version to convert from",
    type=click.Choice(list_ver_from),
    required=True,
)
@click.option(
    "-t",
    "--to",
    "toVer",
    help="Version to convert to (2025a+ converts to YAML format)",
    type=str,  # Changed from Choice to allow 'yaml' or any version
    required=True,
)
@click.option(
    "-i",
    "--input",
    "input_path",
    help="Input directory containing SUEWS files (must have RunControl.nml)",
    type=click.Path(exists=True),
    required=True,
)
@click.option(
    "-o",
    "--output",
    "output_path",
    help="Output path (directory for table conversion, file path for YAML)",
    type=click.Path(),
    required=True,
)
def convert_table_cmd(fromVer: str, toVer: str, input_path: str, output_path: str):
    """Convert SUEWS input files between versions.
    
    Automatically determines conversion type based on target version:
    - Versions before 2025 (e.g., 2024a): Table-to-table conversion
    - Version 2025a or later: Convert to YAML format
    
    Examples:
        suews-convert -f 2020a -t 2024a -i input_dir -o output_dir  # Table to table
        suews-convert -f 2024a -t 2025a -i input_dir -o config.yml  # Table to YAML (2025+)
    """
    # Determine conversion type based on target version
    to_yaml = False
    
    # Check if target is a version >= 2025
    if toVer in list_ver_to:
        # Extract year from version (e.g., '2025a' -> 2025)
        try:
            year = int(toVer[:4])
            if year >= 2025:
                to_yaml = True
        except (ValueError, IndexError):
            # If we can't parse the year, treat as table conversion
            pass
    else:
        # Check if it's a future version format like '2025a+' or '2026a'
        try:
            year = int(toVer[:4])
            if year >= 2025:
                to_yaml = True
            else:
                click.secho(f"Error: '{toVer}' is not a valid target version", fg="red")
                sys.exit(1)
        except (ValueError, IndexError):
            click.secho(f"Error: '{toVer}' is not a valid target version", fg="red")
            sys.exit(1)
    
    if to_yaml:
        # Convert to YAML
        click.secho(f"Converting from {fromVer} tables to YAML format...", fg="cyan")
        
        # Import the to_yaml function
        from .to_yaml import to_yaml
        
        # Ensure output path has .yml or .yaml extension
        output_file = Path(output_path)
        if not output_file.suffix in ['.yml', '.yaml']:
            if output_file.is_dir() or not output_file.suffix:
                # If it's a directory or has no extension, add default filename
                output_file = output_file / 'suews_config.yml' if output_file.is_dir() else Path(str(output_file) + '.yml')
                click.secho(f"Output will be saved to: {output_file}", fg="yellow")
        
        # Create a context and invoke the to_yaml function
        ctx = click.Context(click.Command('to_yaml'))
        ctx.invoke(to_yaml, input_dir=input_path, output_file=str(output_file), from_ver=fromVer)
        
        click.secho(f"Successfully converted to YAML: {output_file}", fg="green")
    else:
        # Convert between table versions
        if toVer not in list_ver_to:
            click.secho(f"Error: '{toVer}' is not a valid table version. Available versions: {', '.join(list_ver_to)}", fg="red")
            sys.exit(1)
        
        click.secho(f"Converting tables from {fromVer} to {toVer}...", fg="cyan")
        
        # Ensure output is treated as directory for table conversion
        output_dir = Path(output_path)
        
        convert_table(Path(input_path), output_dir, fromVer, toVer)
        
        click.secho(f"Successfully converted tables from {fromVer} to {toVer}", fg="green")