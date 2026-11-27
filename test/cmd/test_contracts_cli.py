"""Tests for the ``suews contracts`` CLI command group."""

from __future__ import annotations

import json
from pathlib import Path
import subprocess
import sys

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

pytestmark = [pytest.mark.unit, pytest.mark.contract("C-API", "C-SCHEMA")]


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


@pytest.mark.parametrize(
    "bad_url",
    [
        "https://suews.io?preview=1",
        "https://suews.io#fragment",
        "ftp://suews.io",
        "javascript:alert(1)",
        "https://suews.io/bad\npath",
        "https://[::1",
        "https://suews.io:abc",
        "https://suews.io:",
        "   ",
    ],
)
def test_contracts_rejects_invalid_base_url(bad_url: str) -> None:
    """Invalid base URLs return a structured error envelope rather than broken links."""
    result = CliRunner().invoke(
        cli, ["contracts", "info", "--base-url", bad_url, "--format", "json"]
    )
    assert result.exit_code == 1
    env = json.loads(result.output)
    assert env["status"] == "error"
    assert env["errors"]


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


@pytest.mark.e2e
@pytest.mark.os_compat
def test_contracts_export_stdout_file_and_bundle(tmp_path: Path) -> None:
    """``suews contracts export`` writes exact canonical bytes matching registered digests."""
    runner = CliRunner()

    # Raw stdout export via Click binary stream matches FORCING_VERSIONS digest exactly.
    raw_res = runner.invoke(cli, ["contracts", "export", "forcing"])
    assert raw_res.exit_code == 0, raw_res.output
    assert CURRENT_FORCING_VERSION is not None
    assert (
        sha256_digest(raw_res.stdout_bytes) == FORCING_VERSIONS[CURRENT_FORCING_VERSION]
    )

    # Subprocess stdout preserves raw LF bytes without text-stream translation.
    proc = subprocess.run(
        [sys.executable, "-m", "supy.cmd.suews_cli", "contracts", "export", "forcing"],
        capture_output=True,
        check=False,
    )
    assert proc.returncode == 0, proc.stderr.decode("utf-8", errors="replace")
    assert b"\r\n" not in proc.stdout
    assert sha256_digest(proc.stdout) == FORCING_VERSIONS[CURRENT_FORCING_VERSION]

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
    assert pub_env["data"]["validation"]["contracts_dir"] == str(
        site_contracts.resolve()
    )
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


def test_contracts_validate_cli_handles_malformed_index_and_bad_utf8(
    tmp_path: Path,
) -> None:
    """Malformed JSON shapes and non-UTF-8 HTML emit JSON error envelopes, not tracebacks."""
    runner = CliRunner()
    site_contracts = tmp_path / "contracts"
    pub_res = runner.invoke(
        cli, ["contracts", "publish", "--output-dir", str(site_contracts)]
    )
    assert pub_res.exit_code == 0, pub_res.output

    # 1. interfaces: null in root index.json
    root_idx = site_contracts / "index.json"
    orig_root = root_idx.read_bytes()
    root_idx.write_text(
        json.dumps({
            "base_url": "https://suews.io",
            "contracts_url": "https://suews.io/contracts/",
            "interfaces": None,
        })
        + "\n",
        encoding="ascii",
    )
    res_null = runner.invoke(
        cli, ["contracts", "validate", str(site_contracts), "--format", "json"]
    )
    assert res_null.exit_code == 1
    env_null = json.loads(res_null.output)
    assert env_null["status"] == "error"
    root_idx.write_bytes(orig_root)

    # 2. latest_paths: [] in root index.json
    bad_root = json.loads(orig_root.decode("ascii"))
    bad_root["interfaces"]["forcing"]["latest_paths"] = []
    root_idx.write_text(json.dumps(bad_root) + "\n", encoding="ascii")
    res_list = runner.invoke(
        cli, ["contracts", "validate", str(site_contracts), "--format", "json"]
    )
    assert res_list.exit_code == 1
    assert json.loads(res_list.output)["status"] == "error"
    root_idx.write_bytes(orig_root)

    # 3. paths: null inside interface index.json
    iface_idx = site_contracts / "forcing" / "index.json"
    orig_iface = iface_idx.read_bytes()
    bad_iface = json.loads(orig_iface.decode("ascii"))
    bad_iface["versions"][0]["paths"] = None
    iface_idx.write_text(json.dumps(bad_iface) + "\n", encoding="ascii")
    res_paths = runner.invoke(
        cli, ["contracts", "validate", str(site_contracts), "--format", "json"]
    )
    assert res_paths.exit_code == 1
    assert json.loads(res_paths.output)["status"] == "error"
    iface_idx.write_bytes(orig_iface)

    # 4. Invalid UTF-8 bytes in index.html
    root_html = site_contracts / "index.html"
    orig_html = root_html.read_bytes()
    root_html.write_bytes(b"\xff\xfeinvalid-utf8")
    res_utf8 = runner.invoke(
        cli, ["contracts", "validate", str(site_contracts), "--format", "json"]
    )
    assert res_utf8.exit_code == 1
    assert json.loads(res_utf8.output)["status"] == "error"
    root_html.write_bytes(orig_html)


def test_contracts_cli_reports_nul_paths_and_missing_dirs_as_json_envelopes(
    tmp_path: Path,
) -> None:
    """Embedded NUL paths and missing directory arguments emit JSON error envelopes."""
    runner = CliRunner()
    site_contracts = tmp_path / "contracts"
    pub_res = runner.invoke(
        cli, ["contracts", "publish", "--output-dir", str(site_contracts)]
    )
    assert pub_res.exit_code == 0, pub_res.output

    # Embedded NUL byte in indexed path produces a JSON error envelope, not ValueError.
    root_idx = site_contracts / "index.json"
    orig_root = root_idx.read_bytes()
    nul_root = json.loads(orig_root.decode("ascii"))
    nul_root["interfaces"]["forcing"]["latest_paths"]["catalogue"] = (
        "forcing/latest/catalogue.json\x00bad"
    )
    root_idx.write_text(json.dumps(nul_root) + "\n", encoding="ascii")
    res_nul = runner.invoke(
        cli, ["contracts", "validate", str(site_contracts), "--format", "json"]
    )
    assert res_nul.exit_code == 1
    env_nul = json.loads(res_nul.output)
    assert env_nul["status"] == "error"
    root_idx.write_bytes(orig_root)

    # Missing directories for validate and --preserve-from emit JSON error envelopes.
    missing_dir = tmp_path / "does-not-exist"
    res_missing_val = runner.invoke(
        cli, ["contracts", "validate", str(missing_dir), "--format", "json"]
    )
    assert res_missing_val.exit_code == 1
    env_missing_val = json.loads(res_missing_val.output)
    assert env_missing_val["status"] == "error"

    res_missing_pres = runner.invoke(
        cli,
        [
            "contracts",
            "publish",
            "--output-dir",
            str(tmp_path / "out-missing-pres"),
            "--preserve-from",
            str(missing_dir),
            "--format",
            "json",
        ],
    )
    assert res_missing_pres.exit_code == 1
    env_missing_pres = json.loads(res_missing_pres.output)
    assert env_missing_pres["status"] == "error"
