"""Documentation contract tests for migration commands in transition_guide.rst."""

from pathlib import Path
import re

import pytest

pytestmark = [pytest.mark.tooling, pytest.mark.contract("C-MIGRATE", "C-SCHEMA")]

ROOT = Path(__file__).resolve().parents[2]
TRANSITION_GUIDE = ROOT / "docs" / "source" / "inputs" / "transition_guide.rst"


def test_transition_guide_uses_unpinned_migrate_commands_for_dev_schemas() -> None:
    """Dev-schema sections must not pin stale --target-version 2026.6.devN commands."""
    text = TRANSITION_GUIDE.read_text(encoding="utf-8")

    # Canonical unpinned command and warning explanation live in the intro.
    assert "suews schema migrate site.yml" in text
    assert "Omit ``--target-version``" in text
    assert "Pinning ``--target-version`` to an older schema emits a" in text

    # No section should trap readers on a pinned development schema target.
    stale_dev_pins = re.findall(
        r"suews(?:-schema|\s+schema)\s+migrate[^\n]*--target-version\s+\S*\.dev\d+",
        text,
    )
    assert stale_dev_pins == [], (
        f"Found stale dev-schema --target-version pins in {TRANSITION_GUIDE}: {stale_dev_pins}"
    )

    # Deprecated hyphenated alias must not appear in migration commands.
    assert "suews-schema" not in text

    # Any illustrative historical --target-version example must warn about historical output.
    explicit_pins = re.findall(
        r"suews\s+schema\s+migrate[^\n]*--target-version\s+(\S+)",
        text,
    )
    assert explicit_pins == ["2026.4"]
    assert "Pinning an older ``--target-version`` emits a warning" in text
