#!/usr/bin/env python3
"""Check RST source labels with the same rule used by the Sphinx extension."""

import argparse
from pathlib import Path
import sys

sys.path.insert(0, str(Path(__file__).resolve().parents[2] / "docs/source/_ext"))

from doc_source import check_tree, read_tool_outputs


def main() -> int:
    """Validate an optional source tree without importing Sphinx or SuPy."""
    parser = argparse.ArgumentParser(description=__doc__)
    path_repo = Path(__file__).resolve().parents[2]
    parser.add_argument("--source-dir", type=Path, default=path_repo / "docs/source")
    parser.add_argument("--repo-root", type=Path, default=path_repo)
    args = parser.parse_args()
    try:
        outputs = read_tool_outputs(args.source_dir / "conf.py")
        errors, counts = check_tree(args.source_dir, args.repo_root, outputs)
    except (OSError, ValueError, SyntaxError) as error:
        parser.exit(2, f"Cannot check documentation source labels: {error}\n")
    if errors:
        print("Documentation source labels:\n" + "\n".join(errors))
        return 1
    print("Documentation source labels: OK")
    for source, count in sorted(counts.items()):
        print(f"  {source}: {count}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
