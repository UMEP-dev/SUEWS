"""Contracts relied on by in-process CLI regression tests."""

import subprocess

import click
from conftest import run_cli_command
import pytest

pytestmark = pytest.mark.api


@pytest.mark.parametrize("check", [False, True])
@pytest.mark.parametrize("exit_code", [0, 7])
def test_cli_capture_preserves_streams_and_exit_status(cli_runner, check, exit_code):
    """Capture streams verbatim, including stderr without diagnostic keywords."""
    stdout = "WARNING is an ordinary data value here\n"
    stderr = "Cannot open input\n  Please choose an existing file\n\n"

    @click.command()
    def command():
        click.echo(stdout, nl=False)
        click.echo(stderr, err=True, nl=False)
        raise SystemExit(exit_code)

    if check and exit_code:
        with pytest.raises(subprocess.CalledProcessError) as caught:
            run_cli_command(cli_runner, command, [], check=True)
        result = caught.value
        assert result.cmd == ["command"]
    else:
        result = run_cli_command(cli_runner, command, [], check=check)

    assert result.returncode == exit_code
    assert result.stdout == stdout
    assert result.stderr == stderr
