"""Guard tests for Rust bridge build-script rerun triggers (gh#1790)."""

from __future__ import annotations

from pathlib import Path

import pytest

pytestmark = [pytest.mark.api, pytest.mark.contract("C-TOOLING")]

REPO_ROOT = Path(__file__).resolve().parents[2]
BRIDGE_BUILD_RS = REPO_ROOT / "src" / "suews_bridge" / "build.rs"


def _code_lines(text: str) -> list[str]:
    return [
        code for line in text.splitlines() if (code := line.split("//", 1)[0].strip())
    ]


def test_bridge_build_rs_excludes_git_metadata_from_rerun_triggers() -> None:
    """build.rs must not recompile 48 c_api Fortran files on git HEAD/tag moves."""
    code_lines = _code_lines(BRIDGE_BUILD_RS.read_text(encoding="utf-8"))

    for line in code_lines:
        assert ".git/HEAD" not in line and ".git/refs" not in line, (
            f"src/suews_bridge/build.rs must not watch git metadata: {line}"
        )

    for directive in (
        "cargo:rerun-if-changed=build.rs",
        "cargo:rerun-if-env-changed=SUEWS_VERSION",
        "cargo:rerun-if-env-changed=SUEWS_COMMIT",
        "cargo:rerun-if-env-changed=SUEWS_BRIDGE_LINT_ONLY",
        "cargo:rerun-if-env-changed=SUEWS_BUILD_PROFILE",
    ):
        assert any(directive in line for line in code_lines), (
            f"src/suews_bridge/build.rs missing expected rerun directive: {directive}"
        )
