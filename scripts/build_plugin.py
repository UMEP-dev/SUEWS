#!/usr/bin/env python3
"""Generate the distributable SUEWS plugin bundle from the single source of truth.

Source of truth: ``.claude/skills/<name>/`` (plus ``mcp/`` for the server). The
bundle under ``plugins/suews/skills/`` is a **build artifact** — never edit it by
hand. Run ``make plugin`` (or this script) after changing a skill; the parity
test in ``test/mcp/test_packaging_manifests.py`` then guards against drift.

The user-facing plugin ships a single skill, ``suews`` (fresh-site setup is a
reference inside it, not a separate skill). Anything else found in the bundle's
skills directory is pruned.
"""

from __future__ import annotations

import shutil
from pathlib import Path

REPO = Path(__file__).resolve().parent.parent
SRC_SKILLS = REPO / ".claude" / "skills"
BUNDLE_SKILLS = REPO / "plugins" / "suews" / "skills"

# Skills shipped in the distributable user-facing plugin bundle.
BUNDLED = ["suews"]

_IGNORE = shutil.ignore_patterns("__pycache__", "*.pyc", ".DS_Store")


def build() -> None:
    BUNDLE_SKILLS.mkdir(parents=True, exist_ok=True)

    # Sync each bundled skill from its source (clean replace).
    for name in BUNDLED:
        src = SRC_SKILLS / name
        if not src.is_dir():
            raise SystemExit(f"source skill missing: {src}")
        dst = BUNDLE_SKILLS / name
        if dst.exists():
            shutil.rmtree(dst)
        shutil.copytree(src, dst, ignore=_IGNORE)
        print(f"synced {name}: .claude/skills/{name} -> plugins/suews/skills/{name}")

    # Prune any skill dir in the bundle that is no longer shipped (e.g. one that
    # has since been merged into another skill).
    for child in sorted(BUNDLE_SKILLS.iterdir()):
        if child.is_dir() and child.name not in BUNDLED:
            shutil.rmtree(child)
            print(f"pruned stale bundled skill: {child.name}")

    print("plugin bundle up to date.")


if __name__ == "__main__":
    build()
