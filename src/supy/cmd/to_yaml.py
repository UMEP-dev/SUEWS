import click
from pathlib import Path
import tempfile
import shutil


@click.command(
    short_help="Convert SUEWS table-based input to a YAML configuration file."
)
@click.option(
    "-i",
    "--input-dir",
    "input_dir",
    help="Directory with the SUEWS table-based input files (must contain RunControl.nml).",
    type=click.Path(exists=True, file_okay=False, dir_okay=True),
    required=True,
)
@click.option(
    "-o",
    "--output-file",
    "output_file",
    help="Path for the output YAML configuration file.",
    type=click.Path(),
    required=True,
)
@click.option(
    "-f",
    "--from-ver",
    "from_ver",
    help="[Optional] The source version of the tables (e.g., '2020a'). If provided, a table conversion to the latest version will be performed first.",
    type=str,
    default=None,
)
@click.option(
    "-d",
    "--debug-dir",
    "debug_dir",
    help="[Optional] Directory to save intermediate conversion files for debugging.",
    type=click.Path(),
    default=None,
)
@click.option(
    "--no-profile-validation",
    "no_validate_profiles",
    is_flag=True,
    default=False,
    help="Disable automatic profile validation and creation of missing profiles",
)
def to_yaml(input_dir: str, output_file: str, from_ver: str, debug_dir: str = None, no_validate_profiles: bool = False):
    """
    This tool facilitates the transition from the legacy table-based SUEWS input format
    to the new YAML-based configuration format.

    It performs a two-step process:
    1.  Optionally converts older versions of input tables to the latest available version.
    2.  Reads the complete set of table-based inputs and converts them into a single, comprehensive YAML file.
    """
    # Lazy imports to avoid circular import issues
    from supy.util._converter import convert_table, list_ver_to
    from supy._load import load_InitialCond_grid_df
    from supy.data_model.core import SUEWSConfig

    input_path = Path(input_dir)
    output_path = Path(output_file)

    processing_dir = input_path
    temp_dir_obj = None

    try:
        if from_ver:
            # Convert to 2025a table format first (which includes adding n_buildings and h_std columns)
            # Then convert those 2025a tables to YAML
            to_ver = "2025a"
            click.echo(
                f"Step 1: Converting tables from version {from_ver} to {to_ver}..."
            )
            temp_dir_obj = tempfile.TemporaryDirectory()
            temp_dir_path = Path(temp_dir_obj.name)
            convert_table(str(input_path), str(temp_dir_path), from_ver, to_ver, debug_dir=debug_dir, validate_profiles=not no_validate_profiles)
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
        except Exception as e:
            raise click.ClickException(f"Failed to create configuration: {e}")

        click.echo(f"Step 4: Saving configuration to YAML file: {output_path}...")
        config.to_yaml(output_path)

        click.secho(f"Successfully converted to {output_path}", fg="green")

    finally:
        if temp_dir_obj:
            temp_dir_obj.cleanup()


if __name__ == "__main__":
    to_yaml()
