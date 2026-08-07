"""Tests for the published output-contract artefacts."""

from __future__ import annotations

import importlib.resources
import importlib.util
from pathlib import Path
import shutil
import sys

import pytest

pytestmark = pytest.mark.api

PROJECT_ROOT = Path(__file__).resolve().parents[2]
SCRIPT_PATH = PROJECT_ROOT / "scripts/lint/check_output_contract_artefacts.py"
SCRIPT_SPEC = importlib.util.spec_from_file_location(
    "check_output_contract_artefacts",
    SCRIPT_PATH,
)
assert SCRIPT_SPEC is not None
assert SCRIPT_SPEC.loader is not None
audit = importlib.util.module_from_spec(SCRIPT_SPEC)
sys.modules[SCRIPT_SPEC.name] = audit
SCRIPT_SPEC.loader.exec_module(audit)


def test_committed_output_contract_is_current() -> None:
    """Require committed bytes, schema, manifest, and version digest to agree."""
    assert audit.audit_output_contract(PROJECT_ROOT) == ["1.0.0"]


def test_current_bundle_generation_is_deterministic() -> None:
    """Generate identical canonical bytes from the output registry."""
    assert audit.build_output_contract_bundle("1.0.0") == (
        audit.build_output_contract_bundle("1.0.0")
    )


def test_audit_rejects_tampered_and_extra_files(tmp_path: Path) -> None:
    """Reject content drift and files outside the fixed release bundle."""
    artefacts = tmp_path / "src/supy/data_model/output/artefacts"
    shutil.copytree(audit.ARTEFACT_ROOT, artefacts)
    catalogue = artefacts / "1.0.0/catalogue.json"
    catalogue.write_bytes(catalogue.read_bytes() + b" ")

    with pytest.raises(audit.OutputContractAuditError, match="digest"):
        audit.audit_output_contract(tmp_path)

    shutil.rmtree(artefacts)
    shutil.copytree(audit.ARTEFACT_ROOT, artefacts)
    (artefacts / "1.0.0/unexpected.json").write_text("{}\n", encoding="ascii")

    with pytest.raises(audit.OutputContractAuditError, match="exactly"):
        audit.audit_output_contract(tmp_path)


def test_published_catalogue_is_packaged() -> None:
    """Expose the versioned catalogue through installed package resources."""
    catalogue = importlib.resources.files("supy.data_model.output").joinpath(
        "artefacts/1.0.0/catalogue.json"
    )
    assert catalogue.is_file()
