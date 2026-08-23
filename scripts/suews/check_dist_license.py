#!/usr/bin/env python3
"""Validate the licence metadata and files in SUEWS wheel archives."""

from __future__ import annotations

import argparse
from email.parser import BytesParser
from pathlib import Path, PurePosixPath
import zipfile

EXPECTED_EXPRESSION = "MPL-2.0 AND Apache-2.0"
EXPECTED_LICENSE_FILES = {
    "LICENSE",
    "src/suews/ext_lib/spartacus-surface/LICENSE",
    "src/suews/ext_lib/spartacus-surface/NOTICE",
}


def _one(candidates: list[str], description: str, archive: Path) -> str:
    if len(candidates) != 1:
        raise ValueError(
            f"{archive}: expected one {description}, found {len(candidates)}: "
            f"{candidates}"
        )
    return candidates[0]


def _validate_metadata(metadata_bytes: bytes, archive: Path) -> None:
    metadata = BytesParser().parsebytes(metadata_bytes)
    if metadata["Metadata-Version"] != "2.4":
        raise ValueError(
            f"{archive}: expected Metadata-Version 2.4, "
            f"found {metadata['Metadata-Version']!r}"
        )
    if metadata["License-Expression"] != EXPECTED_EXPRESSION:
        raise ValueError(
            f"{archive}: expected License-Expression {EXPECTED_EXPRESSION!r}, "
            f"found {metadata['License-Expression']!r}"
        )
    if metadata["License"] is not None:
        raise ValueError(
            f"{archive}: legacy License field is still present: {metadata['License']!r}"
        )

    actual_files = set(metadata.get_all("License-File", []))
    if actual_files != EXPECTED_LICENSE_FILES:
        raise ValueError(
            f"{archive}: expected License-File entries "
            f"{sorted(EXPECTED_LICENSE_FILES)!r}, found {sorted(actual_files)!r}"
        )


def _validate_wheel(archive: Path) -> None:
    with zipfile.ZipFile(archive) as wheel:
        names = wheel.namelist()
        metadata_name = _one(
            [name for name in names if name.endswith(".dist-info/METADATA")],
            "wheel METADATA file",
            archive,
        )
        _validate_metadata(wheel.read(metadata_name), archive)

        dist_info = PurePosixPath(metadata_name).parent
        expected_members = {
            str(dist_info / "licenses" / relative)
            for relative in EXPECTED_LICENSE_FILES
        }
        missing = expected_members.difference(names)
        if missing:
            raise ValueError(
                f"{archive}: missing licence archive members: {sorted(missing)!r}"
            )


def validate_archive(archive: Path) -> None:
    """Validate one wheel archive."""
    if archive.suffix != ".whl":
        raise ValueError(f"{archive}: expected a .whl archive")
    _validate_wheel(archive)


def main() -> None:
    """Validate every wheel archive supplied on the command line."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("archives", nargs="+", type=Path)
    args = parser.parse_args()

    for archive in args.archives:
        validate_archive(archive)
        print(f"licence metadata OK: {archive}")


if __name__ == "__main__":
    main()
