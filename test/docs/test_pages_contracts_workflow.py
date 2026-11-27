"""Contract tests for GitHub Pages and CI data-contracts publication wiring."""

from __future__ import annotations

from contextlib import contextmanager
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
import importlib.util
import json
import os
from pathlib import Path
import re
import shutil
import subprocess
import sys
import threading
from typing import Any

import pytest
import yaml

pytestmark = [pytest.mark.tooling, pytest.mark.contract("C-TOOLING")]

REPO_ROOT = Path(__file__).resolve().parents[2]
PAGES_WORKFLOW = REPO_ROOT / ".github/workflows/pages-deploy.yml"
AUDIT_WORKFLOW = REPO_ROOT / ".github/workflows/data-interface-version-audit.yml"
PUBLISH_SCRIPT = REPO_ROOT / "scripts/suews/publish_contracts.py"
_SHA_REF_RE = re.compile(r"^[^@\s]+@[0-9a-fA-F]{40}$")


def _load_contracts_module():
    """Load the checkout-local contracts module via scripts/suews/publish_contracts.py."""
    spec = importlib.util.spec_from_file_location("publish_contracts", PUBLISH_SCRIPT)
    assert spec is not None and spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module._load_contracts_module()


def _load_workflow(path: Path) -> dict[str, Any]:
    """Parse a GitHub Actions workflow YAML file."""
    return yaml.safe_load(path.read_text(encoding="utf-8"))


def _get_step(steps: list[dict[str, Any]], name: str) -> dict[str, Any]:
    """Return the first workflow step matching ``name``."""
    return next(step for step in steps if step.get("name") == name)


@contextmanager
def _serve_responses(
    routes: dict[str, tuple[int, bytes, dict[str, str]]],
):
    """Run an ephemeral localhost HTTP server serving ``routes``."""

    class _Handler(BaseHTTPRequestHandler):
        def do_GET(self) -> None:
            entry = routes.get(self.path)
            if entry is None:
                self.send_response(404)
                self.send_header("Content-Type", "text/plain")
                self.end_headers()
                self.wfile.write(b"not found")
                return
            status, body, extra_headers = entry
            self.send_response(status)
            for key, value in extra_headers.items():
                self.send_header(key, value)
            self.send_header("Content-Length", str(len(body)))
            self.end_headers()
            self.wfile.write(body)

        def log_message(self, format: str, *args: Any) -> None:
            return

    server = ThreadingHTTPServer(("127.0.0.1", 0), _Handler)
    thread = threading.Thread(target=server.serve_forever, daemon=True)
    thread.start()
    try:
        host, port = server.server_address
        yield f"http://{host}:{port}"
    finally:
        server.shutdown()
        server.server_close()
        thread.join(timeout=5)


def _add_historical_release(
    site_root: Path,
    interface: str,
    version: str,
    base_url: str,
) -> dict[str, bytes]:
    """Insert a synthetic historical release into a published contracts tree."""
    contracts = _load_contracts_module()
    _, source_bundle = contracts.load_contract_bundle_bytes(interface, version="1.0.0")
    cat_bytes = source_bundle["catalogue.json"]
    schema_bytes = source_bundle["catalogue.schema.json"]
    manifest_bytes = contracts.canonical_json_bytes({
        "files": {
            "catalogue.json": contracts.sha256_digest(cat_bytes),
            "catalogue.schema.json": contracts.sha256_digest(schema_bytes),
        },
        "interface": interface,
        "version": version,
    })
    bundle = {
        "catalogue.json": cat_bytes,
        "catalogue.schema.json": schema_bytes,
        "manifest.json": manifest_bytes,
    }
    ver_dir = site_root / interface / version
    ver_dir.mkdir(parents=True, exist_ok=True)
    for fname, content in bundle.items():
        (ver_dir / fname).write_bytes(content)

    digests = {
        fname: contracts.sha256_digest(content) for fname, content in bundle.items()
    }
    contract_digest = (
        digests["catalogue.json"]
        if interface == "forcing"
        else digests["manifest.json"]
    )
    iface_idx_path = site_root / interface / "index.json"
    iface_idx = json.loads(iface_idx_path.read_text(encoding="ascii"))
    entry = {
        "version": version,
        "is_current": False,
        "preserved": False,
        "contract_digest": contract_digest,
        "manifest_digest": digests["manifest.json"],
        "digests": digests,
        "paths": {
            "catalogue": f"{version}/catalogue.json",
            "schema": f"{version}/catalogue.schema.json",
            "manifest": f"{version}/manifest.json",
        },
        "urls": {
            "catalogue": f"{base_url}/contracts/{interface}/{version}/catalogue.json",
            "schema": f"{base_url}/contracts/{interface}/{version}/catalogue.schema.json",
            "manifest": f"{base_url}/contracts/{interface}/{version}/manifest.json",
        },
    }
    versions_list = [e for e in iface_idx["versions"] if e["version"] != version]
    versions_list.append(entry)
    versions_list.sort(key=lambda e: tuple(int(x) for x in e["version"].split(".")))
    iface_idx["versions"] = versions_list
    iface_idx["version_count"] = len(versions_list)
    iface_idx_path.write_bytes(contracts.canonical_json_bytes(iface_idx))

    root_idx_path = site_root / "index.json"
    root_idx = json.loads(root_idx_path.read_text(encoding="ascii"))
    root_idx["interfaces"][interface]["versions"] = [
        e["version"] for e in versions_list
    ]
    root_idx["interfaces"][interface]["version_count"] = len(versions_list)
    root_idx_path.write_bytes(contracts.canonical_json_bytes(root_idx))
    return bundle


def _tree_to_routes(
    contracts_dir: Path,
) -> dict[str, tuple[int, bytes, dict[str, str]]]:
    """Map every file under ``contracts_dir`` to ``/contracts/...`` HTTP routes."""
    routes: dict[str, tuple[int, bytes, dict[str, str]]] = {}
    for path in sorted(contracts_dir.rglob("*")):
        if path.is_file():
            rel = path.relative_to(contracts_dir).as_posix()
            ctype = "application/json" if path.suffix == ".json" else "text/html"
            routes[f"/contracts/{rel}"] = (
                200,
                path.read_bytes(),
                {"Content-Type": ctype},
            )
    return routes


@pytest.mark.smoke
@pytest.mark.core
def test_pages_and_audit_workflows_wire_contracts_with_least_privilege() -> None:
    """Pages and data-interface audit workflows wire contracts while keeping #1431/#1432."""
    pages_wf = _load_workflow(PAGES_WORKFLOW)
    triggers = pages_wf[True] if True in pages_wf else pages_wf["on"]

    # Top-level permissions stay minimal; write scopes live only on jobs.deploy.
    assert pages_wf["permissions"] == {"contents": "read"}
    deploy_job = pages_wf["jobs"]["deploy"]
    assert deploy_job["permissions"] == {
        "contents": "read",
        "pages": "write",
        "id-token": "write",
        "pull-requests": "write",
    }

    # Explicit bootstrap input defaults to false so push deploys fail closed on 404.
    dispatch_inputs = triggers["workflow_dispatch"]["inputs"]
    bootstrap_input = dispatch_inputs["allow_contracts_bootstrap"]
    assert bootstrap_input["type"] == "boolean"
    assert bootstrap_input["default"] is False

    # Push triggers include data_model and publish_contracts.py; PR triggers stay bounded.
    push_paths = triggers["push"]["paths"]
    assert "src/supy/data_model/**" in push_paths
    assert "scripts/suews/publish_contracts.py" in push_paths
    pr_paths = triggers["pull_request"]["paths"]
    assert "docs/**" not in pr_paths
    assert "src/supy/data_model/**" not in pr_paths
    assert "scripts/suews/publish_contracts.py" not in pr_paths

    steps = deploy_job["steps"]
    for step in steps:
        uses = step.get("uses")
        if uses:
            assert _SHA_REF_RE.match(uses), f"Unpinned action: {uses}"

    names = [s.get("name") for s in steps]
    build_site_idx = names.index("Build site")
    fetch_idx = names.index("Fetch previous published contracts")
    publish_idx = names.index("Build and validate data contracts")
    upload_idx = next(
        i
        for i, s in enumerate(steps)
        if str(s.get("uses", "")).startswith("actions/upload-pages-artifact@")
    )
    assert build_site_idx < fetch_idx < publish_idx < upload_idx

    fetch_step = _get_step(steps, "Fetch previous published contracts")
    assert "inputs.allow_contracts_bootstrap" in str(
        fetch_step.get("env", {}).get("ALLOW_CONTRACTS_BOOTSTRAP", "")
    )

    for step_name in (
        "Build site",
        "Fetch previous published contracts",
        "Build and validate data contracts",
        "Check if fork PR",
    ):
        step = _get_step(steps, step_name)
        assert "${{" not in str(step.get("run", "")), (
            f"Direct expression interpolation in {step_name}"
        )

    for step_name in (
        "Fetch previous published contracts",
        "Build and validate data contracts",
    ):
        step = _get_step(steps, step_name)
        assert step.get("if") == "github.event_name != 'pull_request'"

    # Unprivileged PR/merge-queue audit workflow validates static publication too.
    audit_wf = _load_workflow(AUDIT_WORKFLOW)
    assert audit_wf["permissions"] == {"contents": "read"}
    audit_steps = audit_wf["jobs"]["check-interface-version-history"]["steps"]
    pub_step = _get_step(audit_steps, "Validate static data-contracts publication")
    pub_run = str(pub_step["run"])
    assert "scripts/suews/publish_contracts.py" in pub_run
    assert "--output-dir" in pub_run
    assert "--validate-only" in pub_run


@pytest.mark.core
@pytest.mark.skipif(
    sys.platform == "win32" or shutil.which("bash") is None,
    reason="Workflow shell steps run under bash on Linux CI",
)
def test_pages_workflow_steps_first_deploy_and_historical_preservation(
    tmp_path: Path,
) -> None:
    """Workflow steps handle first-deploy 404 and preserve prior versions across deploys."""
    pages_wf = _load_workflow(PAGES_WORKFLOW)
    steps = pages_wf["jobs"]["deploy"]["steps"]
    fetch_run = str(_get_step(steps, "Fetch previous published contracts")["run"])
    publish_run = str(_get_step(steps, "Build and validate data contracts")["run"])

    # 1. Authorised initial bootstrap: all indices return 404 + bootstrap=true -> fresh build.
    prev_dir_1 = tmp_path / "prev_first"
    out_dir_1 = tmp_path / "site_first" / "contracts"
    with _serve_responses({}) as base_url:
        # Without explicit bootstrap authorisation, root 404 must fail closed.
        unauthorised_res = subprocess.run(
            ["bash", "-c", fetch_run],
            cwd=REPO_ROOT,
            env={
                **os.environ,
                "CONTRACTS_BASE_URL": base_url,
                "PREV_CONTRACTS_DIR": str(prev_dir_1),
                "ALLOW_CONTRACTS_BOOTSTRAP": "false",
                "CONTRACTS_FETCH_RETRIES": "1",
                "CONTRACTS_FETCH_RETRY_DELAY": "0",
            },
            capture_output=True,
            text=True,
            check=False,
        )
        assert unauthorised_res.returncode == 1
        assert "ALLOW_CONTRACTS_BOOTSTRAP" in unauthorised_res.stderr
        assert not prev_dir_1.exists()

        # With explicit bootstrap authorisation and all indices absent, proceed cleanly.
        fetch_res = subprocess.run(
            ["bash", "-c", fetch_run],
            cwd=REPO_ROOT,
            env={
                **os.environ,
                "CONTRACTS_BASE_URL": base_url,
                "PREV_CONTRACTS_DIR": str(prev_dir_1),
                "ALLOW_CONTRACTS_BOOTSTRAP": "true",
                "CONTRACTS_FETCH_RETRIES": "1",
                "CONTRACTS_FETCH_RETRY_DELAY": "0",
            },
            capture_output=True,
            text=True,
            check=False,
        )
        assert fetch_res.returncode == 0, fetch_res.stdout + fetch_res.stderr
        assert "ALLOW_CONTRACTS_BOOTSTRAP=true" in fetch_res.stdout
        assert not prev_dir_1.exists()

        pub_res = subprocess.run(
            ["bash", "-c", publish_run],
            cwd=REPO_ROOT,
            env={
                **os.environ,
                "CONTRACTS_PYTHON": sys.executable,
                "CONTRACTS_BASE_URL": base_url,
                "CONTRACTS_OUTPUT_DIR": str(out_dir_1),
                "PREV_CONTRACTS_DIR": str(prev_dir_1),
            },
            capture_output=True,
            text=True,
            check=False,
        )
        assert pub_res.returncode == 0, pub_res.stdout + pub_res.stderr
        assert (out_dir_1 / "index.json").is_file()
        assert (out_dir_1 / "forcing" / "latest" / "catalogue.json").is_file()
        assert (out_dir_1 / "output" / "latest" / "catalogue.json").is_file()

    # 2. Subsequent deploy: prior server holds historical forcing 0.9.0 -> preserved.
    upstream_contracts = tmp_path / "upstream_contracts"
    prev_dir_2 = tmp_path / "prev_second"
    out_dir_2 = tmp_path / "site_second" / "contracts"

    # Publish baseline and inject synthetic historical forcing/0.9.0.
    contracts = _load_contracts_module()
    contracts.publish_contracts_site(upstream_contracts, base_url="https://suews.io")
    hist_bundle = _add_historical_release(
        upstream_contracts,
        "forcing",
        "0.9.0",
        "https://suews.io",
    )
    routes = _tree_to_routes(upstream_contracts)

    with _serve_responses(routes) as base_url:
        fetch_res_2 = subprocess.run(
            ["bash", "-c", fetch_run],
            cwd=REPO_ROOT,
            env={
                **os.environ,
                "CONTRACTS_BASE_URL": base_url,
                "PREV_CONTRACTS_DIR": str(prev_dir_2),
                "CONTRACTS_FETCH_RETRIES": "1",
                "CONTRACTS_FETCH_RETRY_DELAY": "0",
            },
            capture_output=True,
            text=True,
            check=False,
        )
        assert fetch_res_2.returncode == 0, fetch_res_2.stdout + fetch_res_2.stderr
        assert (prev_dir_2 / "forcing" / "0.9.0" / "catalogue.json").is_file()

        pub_res_2 = subprocess.run(
            ["bash", "-c", publish_run],
            cwd=REPO_ROOT,
            env={
                **os.environ,
                "CONTRACTS_PYTHON": sys.executable,
                "CONTRACTS_BASE_URL": base_url,
                "CONTRACTS_OUTPUT_DIR": str(out_dir_2),
                "PREV_CONTRACTS_DIR": str(prev_dir_2),
            },
            capture_output=True,
            text=True,
            check=False,
        )
        assert pub_res_2.returncode == 0, pub_res_2.stdout + pub_res_2.stderr
        assert not prev_dir_2.exists()  # cleaned up after publish + validate
        for fname, expected_bytes in hist_bundle.items():
            assert (out_dir_2 / "forcing" / "0.9.0" / fname).read_bytes() == (
                expected_bytes
            )
        root_idx = json.loads((out_dir_2 / "index.json").read_text(encoding="ascii"))
        assert "0.9.0" in root_idx["interfaces"]["forcing"]["versions"]


@pytest.mark.core
@pytest.mark.skipif(
    sys.platform == "win32" or shutil.which("bash") is None,
    reason="Workflow shell steps run under bash on Linux CI",
)
def test_pages_fetch_step_fails_closed_on_outage_partial_tree_traversal_and_redirect(
    tmp_path: Path,
) -> None:
    """Fetcher fails closed on 500s, missing indexed files, traversal versions, and redirects."""
    pages_wf = _load_workflow(PAGES_WORKFLOW)
    fetch_step = _get_step(
        pages_wf["jobs"]["deploy"]["steps"], "Fetch previous published contracts"
    )
    fetch_run = str(fetch_step["run"])

    def _run_fetch(
        base_url: str,
        dest: Path,
        *,
        allow_bootstrap: str = "false",
    ) -> subprocess.CompletedProcess[str]:
        return subprocess.run(
            ["bash", "-c", fetch_run],
            cwd=REPO_ROOT,
            env={
                **os.environ,
                "CONTRACTS_BASE_URL": base_url,
                "PREV_CONTRACTS_DIR": str(dest),
                "ALLOW_CONTRACTS_BOOTSTRAP": allow_bootstrap,
                "CONTRACTS_FETCH_RETRIES": "1",
                "CONTRACTS_FETCH_RETRY_DELAY": "0",
            },
            capture_output=True,
            text=True,
            check=False,
        )

    # Case 0: Root index 404 while interface index still exists must fail closed
    # even when ALLOW_CONTRACTS_BOOTSTRAP=true (lost root index on established site).
    seed = tmp_path / "seed_contracts"
    contracts = _load_contracts_module()
    contracts.publish_contracts_site(seed, base_url="https://suews.io")
    missing_root_routes = _tree_to_routes(seed)
    missing_root_routes.pop("/contracts/index.json")
    dest_missing_root = tmp_path / "prev_missing_root"
    with _serve_responses(missing_root_routes) as base_url:
        res_default = _run_fetch(base_url, dest_missing_root, allow_bootstrap="false")
        assert res_default.returncode == 1
        assert "ALLOW_CONTRACTS_BOOTSTRAP" in res_default.stderr
        assert not dest_missing_root.exists()

        res_boot = _run_fetch(base_url, dest_missing_root, allow_bootstrap="true")
        assert res_boot.returncode == 1
        assert "still exists" in res_boot.stderr
        assert not dest_missing_root.exists()

    # Case 1: HTTP 500 on root index must fail closed rather than silently dropping history.
    dest_500 = tmp_path / "prev_500"
    with _serve_responses({"/contracts/index.json": (500, b"boom", {})}) as base_url:
        res = _run_fetch(base_url, dest_500)
        assert res.returncode == 1
        assert "HTTP 500" in res.stderr
        assert not dest_500.exists()

    # Case 2: Root index present, but indexed bundle file returns 404 -> must fail closed.
    partial_routes = _tree_to_routes(seed)
    partial_routes.pop("/contracts/forcing/1.0.0/manifest.json")
    dest_partial = tmp_path / "prev_partial"
    with _serve_responses(partial_routes) as base_url:
        res = _run_fetch(base_url, dest_partial)
        assert res.returncode == 1
        assert "HTTP 404" in res.stderr
        assert not dest_partial.exists()

    # Case 3: Hostile path-traversal version name in root index -> rejected before fetch.
    bad_index = json.dumps({
        "interfaces": {
            "forcing": {"versions": ["../escape"]},
            "output": {"versions": ["1.0.0"]},
        }
    }).encode("utf-8")
    dest_trav = tmp_path / "prev_trav"
    with _serve_responses({"/contracts/index.json": (200, bad_index, {})}) as base_url:
        res = _run_fetch(base_url, dest_trav)
        assert res.returncode == 1
        assert "invalid version" in res.stderr
        assert not dest_trav.exists()

    # Case 4: Cross-origin redirect on root index -> rejected.
    dest_redir = tmp_path / "prev_redir"
    with _serve_responses({
        "/contracts/index.json": (
            302,
            b"",
            {"Location": "https://example.invalid/contracts/index.json"},
        )
    }) as base_url:
        res = _run_fetch(base_url, dest_redir)
        assert res.returncode == 1
        assert "Refused cross-origin" in res.stderr
        assert not dest_redir.exists()
