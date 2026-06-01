#!/usr/bin/env python3
"""Write the release-branch Read the Docs config and provenance file.

Populates a ``release``-branch staging directory with a copy-only RTD config
and a provenance record.

Usage::

    make_release_branch_files.py <staging_dir> <tag> <version>

``<staging_dir>`` already contains ``docs/build/html`` (the pre-built docs).
This script adds:

- ``.readthedocs.yaml`` -- tells Read the Docs to serve that HTML verbatim
  (no source build, no Fortran compiler);
- ``docs/.docs-release.json`` -- provenance for humans inspecting the branch.

Called by ``.github/workflows/docs-release.yml``. Kept as a tracked helper
(rather than an in-workflow heredoc) so it is reviewable and unit-testable,
and so the YAML block-scalar indentation cannot corrupt the generated files.
"""

from __future__ import annotations

import datetime
import json
from pathlib import Path
import sys

# Copy-only RTD config: the release branch carries pre-built HTML only.
RTD_CONFIG = """\
# Read the Docs configuration for the `release` branch.
# This branch carries ONLY pre-built HTML (built from the clean CalVer tag by
# .github/workflows/docs-release.yml). RTD copies it verbatim -- no source
# build, no Fortran compiler. Do not edit by hand; the docs-release workflow
# force-replaces this branch on every release.
version: 2
build:
  os: ubuntu-24.04
  tools:
    python: "3.13"  # Required by the RTD v2 schema; unused here.
  commands:
    - |
      if [ ! -f docs/build/html/index.html ]; then
        echo "ERROR: Pre-built HTML not found on the release branch."
        exit 1
      fi
    - mkdir -p $READTHEDOCS_OUTPUT/html
    - cp -r docs/build/html/* $READTHEDOCS_OUTPUT/html/
formats: []
"""


def make_files(staging: Path, tag: str, version: str) -> None:
    """Write ``.readthedocs.yaml`` and ``docs/.docs-release.json`` into ``staging``."""
    (staging / ".readthedocs.yaml").write_text(RTD_CONFIG, encoding="utf-8")

    provenance = {
        "source_tag": tag,
        "docs_version": version,
        "docs_built": datetime.datetime.now(datetime.timezone.utc).isoformat(),
        "builder": "github-actions",
        "note": (
            "Release docs built at the clean CalVer tag and served by Read "
            "the Docs as the 'release' version. Force-replaced on each "
            "release by .github/workflows/docs-release.yml."
        ),
    }
    docs_dir = staging / "docs"
    docs_dir.mkdir(parents=True, exist_ok=True)
    (docs_dir / ".docs-release.json").write_text(
        json.dumps(provenance, indent=2) + "\n", encoding="utf-8"
    )


def main(argv: list[str]) -> int:
    """Parse ``argv`` (staging_dir, tag, version) and write the files."""
    if len(argv) != 4:
        print(__doc__)
        print(f"ERROR: expected 3 arguments, got {len(argv) - 1}", file=sys.stderr)
        return 2
    staging, tag, version = Path(argv[1]), argv[2], argv[3]
    if not staging.is_dir():
        print(f"ERROR: staging dir does not exist: {staging}", file=sys.stderr)
        return 2
    make_files(staging, tag, version)
    print(f"Wrote .readthedocs.yaml + docs/.docs-release.json for {tag} ({version})")
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv))
