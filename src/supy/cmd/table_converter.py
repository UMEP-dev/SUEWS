# command line tools
import click
import sys
from pathlib import Path

from ..util._converter import convert_table, list_ver_from, list_ver_to


# convert SUEWS tables from older versions to newer ones
@click.command(
    short_help="Convert SUEWS input tables from older versions to newer ones (one-way only)"
)
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
    help="Version to convert to",
    type=click.Choice(list_ver_to),
    required=True,
)
@click.option(
    "-i",
    "--input",
    "fromDir",
    help="Original directory to convert, which must have the `RunControl.nml` file",
    type=click.Path(),
    required=True,
)
@click.option(
    "-o",
    "--output",
    "toDir",
    help="New directory to create for converted tables. Note: the created directory will have the same structure as the origianl one; however, forcing files and output folder won't be includede.",
    type=click.Path(),
    required=True,
)
def convert_table_cmd(fromDir: Path, toDir: Path, fromVer: str, toVer: str):
    """Convert SUEWS input tables from older versions to newer ones (one-way only)."""
    convert_table(fromDir, toDir, fromVer, toVer)
