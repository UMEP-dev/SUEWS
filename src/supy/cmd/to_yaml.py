import click


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
def to_yaml(
    input_dir: str,
    output_file: str,
    from_ver: str,
    debug_dir: str = None,
    no_validate_profiles: bool = False,
):
    """
    This tool facilitates the transition from the legacy table-based SUEWS input format
    to the new YAML-based configuration format.

    It performs a two-step process:
    1.  Optionally converts older versions of input tables to the latest available version.
    2.  Reads the complete set of table-based inputs and converts them into a single, comprehensive YAML file.
    """
    # Import the converter function
    from ..util.converter import convert_to_yaml

    # Call the converter
    convert_to_yaml(
        input_dir=input_dir,
        output_file=output_file,
        from_ver=from_ver,
        debug_dir=debug_dir,
        validate_profiles=not no_validate_profiles,
    )


if __name__ == "__main__":
    to_yaml()
