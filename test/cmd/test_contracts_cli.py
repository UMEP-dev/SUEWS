"""Tests for the ``suews contracts`` CLI command group."""

from __future__ import annotations

import json
from pathlib import Path

from click.testing import CliRunner
import pytest

from supy.cmd.contracts_cli import contracts_group
from supy.cmd.suews_cli import cli
from supy.data_model.contracts import (
    BUNDLE_FILENAMES,
    sha256_digest,
)
from supy.data_model.forcing.version import CURRENT_FORCING_VERSION, FORCING_VERSIONS
from supy.data_model.output.version import CURRENT_OUTPUT_VERSION, OUTPUT_VERSIONS

pytestmark = [pytest.mark.api, pytest.mark.contract("C-API", "C-SCHEMA")]


def test_contracts_subcommand_registered_on_suews_cli() -> None:
    """Expose ``suews contracts`` on the top-level unified CLI."""
    assert cli.commands["contracts"] is contracts_group
    result = CliRunner().invoke(cli, ["contracts", "--help"])
    assert result.exit_code == 0, result.output
    for sub in ("info", "show", "export", "publish", "validate"):
        assert sub in result.output


@pytest.mark.parametrize(
    "subcommand", ["info", "show", "export", "publish", "validate"]
)
def test_contracts_subcommands_have_help(subcommand: str) -> None:
    """Every ``suews contracts`` subcommand renders help cleanly."""
    result = CliRunner().invoke(cli, ["contracts", subcommand, "--help"])
    assert result.exit_code == 0, result.output


def test_contracts_info_text_and_json() -> None:
    """``suews contracts info`` reports both interfaces in text and Envelope JSON."""
    runner = CliRunner()

    text_res = runner.invoke(cli, ["contracts", "info"])
    assert text_res.exit_code == 0, text_res.output
    assert f"[forcing] current: {CURRENT_FORCING_VERSION}" in text_res.output
    assert f"[output] current: {CURRENT_OUTPUT_VERSION}" in text_res.output
    assert "https://suews.io/contracts/forcing/latest/catalogue.json" in text_res.output
    assert "https://suews.io/contracts/output/latest/catalogue.json" in text_res.output

    json_res = runner.invoke(cli, ["contracts", "info", "--format", "json"])
    assert json_res.exit_code == 0, json_res.output
    env = json.loads(json_res.output)
    assert env["status"] == "success"
    data = env["data"]
    assert data["contracts_url"] == "https://suews.io/contracts/"
    assert data["interfaces"]["forcing"]["current_version"] == CURRENT_FORCING_VERSION
    assert data["interfaces"]["output"]["current_version"] == CURRENT_OUTPUT_VERSION
    assert [v["version"] for v in data["interfaces"]["forcing"]["versions"]] == list(
        FORCING_VERSIONS
    )
    assert [v["version"] for v in data["interfaces"]["output"]["versions"]] == list(
        OUTPUT_VERSIONS
    )


def test_contracts_info_single_interface_and_custom_base_url() -> None:
    """``suews contracts info forcing`` filters to one interface and honours ``--base-url``."""
    result = CliRunner().invoke(
        cli,
        [
            "contracts",
            "info",
            "forcing",
            "--base-url",
            "https://suews.io/preview/pr-42/",
            "--format",
            "json",
        ],
    )
    assert result.exit_code == 0, result.output
    env = json.loads(result.output)
    assert env["status"] == "success"
    assert list(env["data"]["interfaces"]) == ["forcing"]
    assert (
        env["data"]["interfaces"]["forcing"]["latest_urls"]["catalogue"]
        == "https://suews.io/preview/pr-42/contracts/forcing/latest/catalogue.json"
    )


@pytest.mark.parametrize("interface", ["forcing", "output"])
@pytest.mark.parametrize("artefact", ["catalogue", "schema", "manifest"])
def test_contracts_show_current_text_and_json(interface: str, artefact: str) -> None:
    """``suews contracts show`` inspects catalogue, schema, and manifest in both formats."""
    runner = CliRunner()
    expected_version = (
        CURRENT_FORCING_VERSION if interface == "forcing" else CURRENT_OUTPUT_VERSION
    )

    text_res = runner.invoke(
        cli, ["contracts", "show", interface, "--artefact", artefact]
    )
    assert text_res.exit_code == 0, text_res.output
    assert f"SUEWS {interface} contract v{expected_version}" in text_res.output

    json_res = runner.invoke(
        cli,
        ["contracts", "show", interface, "--artefact", artefact, "--format", "json"],
    )
    assert json_res.exit_code == 0, json_res.output
    env = json.loads(json_res.output)
    assert env["status"] == "success"
    assert env["data"]["interface"] == interface
    assert env["data"]["version"] == expected_version
    assert env["data"]["artefact"] == artefact
    assert env["data"]["digest"].startswith("sha256:")
    assert isinstance(env["data"]["payload"], dict)


def test_contracts_show_historical_and_unknown_version() -> None:
    """Historical versions resolve cleanly while unknown versions return an error envelope."""
    runner = CliRunner()

    hist_res = runner.invoke(
        cli,
        ["contracts", "show", "forcing", "--version", "1.0.0", "--format", "json"],
    )
    assert hist_res.exit_code == 0, hist_res.output
    hist_env = json.loads(hist_res.output)
    assert hist_env["status"] == "success"
    assert hist_env["data"]["version"] == "1.0.0"
    assert hist_env["data"]["digest"] == FORCING_VERSIONS["1.0.0"]
    var_names = [v["name"] for v in hist_env["data"]["payload"]["variables"]]
    assert "xsmd" in var_names

    bad_res = runner.invoke(
        cli,
        ["contracts", "show", "output", "--version", "9.9.9", "--format", "json"],
    )
    assert bad_res.exit_code == 1
    bad_env = json.loads(bad_res.output)
    assert bad_env["status"] == "error"
    assert any("9.9.9" in err["message"] for err in bad_env["errors"])


def test_contracts_export_stdout_file_and_bundle(tmp_path: Path) -> None:
    """``suews contracts export`` writes exact canonical bytes matching registered digests."""
    runner = CliRunner()

    # Raw stdout export for forcing catalogue matches FORCING_VERSIONS digest exactly.
    raw_res = runner.invoke(cli, ["contracts", "export", "forcing"])
    assert raw_res.exit_code == 0, raw_res.output
    assert CURRENT_FORCING_VERSION is not None
    assert (
        sha256_digest(raw_res.output.encode("utf-8"))
        == FORCING_VERSIONS[CURRENT_FORCING_VERSION]
    )

    # Single-file export with JSON envelope status.
    out_file = tmp_path / "output-manifest.json"
    file_res = runner.invoke(
        cli,
        [
            "contracts",
            "export",
            "output",
            "--artefact",
            "manifest",
            "-o",
            str(out_file),
            "--format",
            "json",
        ],
    )
    assert file_res.exit_code == 0, file_res.output
    assert CURRENT_OUTPUT_VERSION is not None
    file_env = json.loads(file_res.output)
    assert file_env["status"] == "success"
    assert file_env["data"]["digest"] == OUTPUT_VERSIONS[CURRENT_OUTPUT_VERSION]
    assert (
        sha256_digest(out_file.read_bytes()) == OUTPUT_VERSIONS[CURRENT_OUTPUT_VERSION]
    )

    # Bundle export writes all three canonical files.
    bundle_dir = tmp_path / "bundle"
    bundle_res = runner.invoke(
        cli,
        [
            "contracts",
            "export",
            "output",
            "--version",
            "1.0.0",
            "--artefact",
            "bundle",
            "-o",
            str(bundle_dir),
            "--format",
            "json",
        ],
    )
    assert bundle_res.exit_code == 0, bundle_res.output
    bundle_env = json.loads(bundle_res.output)
    assert bundle_env["status"] == "success"
    assert {p.name for p in bundle_dir.iterdir()} == set(BUNDLE_FILENAMES)
    assert (
        sha256_digest((bundle_dir / "manifest.json").read_bytes())
        == OUTPUT_VERSIONS["1.0.0"]
    )

    # Bundle export without --output fails clearly.
    missing_out = runner.invoke(
        cli,
        ["contracts", "export", "forcing", "--artefact", "bundle", "--format", "json"],
    )
    assert missing_out.exit_code == 1
    missing_env = json.loads(missing_out.output)
    assert missing_env["status"] == "error"
    assert any("--output" in err["message"] for err in missing_env["errors"])


def test_contracts_publish_and_validate_cli(tmp_path: Path) -> None:
    """``suews contracts publish`` and ``validate`` build and verify the Pages layout."""
    runner = CliRunner()
    site_contracts = tmp_path / "_site" / "contracts"

    pub_res = runner.invoke(
        cli,
        [
            "contracts",
            "publish",
            "--output-dir",
            str(site_contracts),
            "--base-url",
            "https://suews.io",
            "--format",
            "json",
        ],
    )
    assert pub_res.exit_code == 0, pub_res.output
    pub_env = json.loads(pub_res.output)
    assert pub_env["status"] == "success"
    assert pub_env["data"]["validated"] is True
    assert (site_contracts / "index.html").is_file()
    assert (site_contracts / "forcing" / "latest" / "catalogue.json").is_file()
    assert (site_contracts / "output" / "latest" / "manifest.json").is_file()

    val_res = runner.invoke(
        cli,
        [
            "contracts",
            "validate",
            str(site_contracts),
            "--base-url",
            "https://suews.io",
            "--format",
            "json",
        ],
    )
    assert val_res.exit_code == 0, val_res.output
    val_env = json.loads(val_res.output)
    assert val_env["status"] == "success"
    assert val_env["data"]["interfaces"]["forcing"] == list(FORCING_VERSIONS)
    assert val_env["data"]["interfaces"]["output"] == list(OUTPUT_VERSIONS)
    assert val_env["data"]["relative_links_checked"] > 0
