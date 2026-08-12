"""Tests for the published forcing-contract artefact and user reference."""

from __future__ import annotations

import importlib.resources
import importlib.util
from pathlib import Path
import shutil
import sys

import pytest

pytestmark = pytest.mark.api

PROJECT_ROOT = Path(__file__).resolve().parents[2]


def _load_script(name: str, path: Path):
    spec = importlib.util.spec_from_file_location(name, path)
    assert spec is not None
    assert spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


audit = _load_script(
    "check_forcing_contract_artefacts",
    PROJECT_ROOT / "scripts/lint/check_forcing_contract_artefacts.py",
)
docs = _load_script(
    "generate_forcing_variable_rst",
    PROJECT_ROOT / "docs/generate_forcing_variable_rst.py",
)


def test_committed_forcing_contract_is_current() -> None:
    """Require canonical bytes, registry state, and version digest to agree."""
    assert audit.audit_forcing_contract(PROJECT_ROOT) == ["1.0.0", "1.1.0"]


def test_current_artefact_generation_is_deterministic() -> None:
    """Generate identical canonical bytes from the forcing registry."""
    assert audit.build_forcing_contract_artefact() == (
        audit.build_forcing_contract_artefact()
    )


def test_audit_rejects_tampered_and_extra_files(tmp_path: Path) -> None:
    """Reject content drift and files outside the registered releases."""
    artefacts = tmp_path / "src/supy/data_model/forcing/artefacts"
    shutil.copytree(audit.ARTEFACT_ROOT, artefacts)
    contract = artefacts / "1.0.0.json"
    contract.write_bytes(contract.read_bytes() + b" ")

    with pytest.raises(audit.ForcingContractAuditError, match="digest"):
        audit.audit_forcing_contract(tmp_path)

    shutil.rmtree(artefacts)
    shutil.copytree(audit.ARTEFACT_ROOT, artefacts)
    (artefacts / "unexpected.json").write_text("{}\n", encoding="ascii")

    with pytest.raises(audit.ForcingContractAuditError, match="exactly"):
        audit.audit_forcing_contract(tmp_path)


def test_published_forcing_contract_is_packaged() -> None:
    """Expose the versioned contract through installed package resources."""
    contract = importlib.resources.files("supy.data_model.forcing").joinpath(
        "artefacts/1.0.0.json"
    )
    assert contract.is_file()

    utc_contract = importlib.resources.files("supy.data_model.forcing").joinpath(
        "artefacts/1.1.0.json"
    )
    assert utc_contract.is_file()


def test_forcing_reference_is_registry_derived_and_current() -> None:
    """Keep the tracked user reference equal to its registry projection."""
    reference = PROJECT_ROOT / "docs/source/data-structures/df_forcing.rst"
    rendered = docs.render_forcing_reference()
    assert reference.read_text(encoding="utf-8") == rendered
    assert ".. option:: isec" not in rendered
    assert rendered.count(".. option:: ") == len(audit.FORCING_REGISTRY.variables)
