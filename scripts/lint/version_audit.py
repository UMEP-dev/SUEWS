"""Policy-neutral plumbing for repository version audits."""

from __future__ import annotations

import ast
import subprocess


def run_git(args: list[str]) -> str:
    """Run a Git command and return stdout; raise on non-zero exit."""
    result = subprocess.run(
        ["git", *args],
        check=True,
        capture_output=True,
        text=True,
    )
    return result.stdout


def resolve_merge_base(base_ref: str) -> str:
    """Return the merge-base commit between ``base_ref`` and ``HEAD``."""
    return run_git(["merge-base", base_ref, "HEAD"]).strip()


def read_file_at_ref(ref: str, path: str) -> str | None:
    """Return ``path`` at ``ref``, or ``None`` when it does not exist there."""
    entries = run_git(["ls-tree", "-r", "--name-only", "-z", ref, "--", path])
    if path not in entries.split("\0"):
        return None
    return run_git(["show", f"{ref}:{path}"])


def extract_literal_assignments(
    source: str,
    names: tuple[str, ...],
) -> dict[str, object]:
    """Return requested top-level literal assignments from Python source."""
    tree = ast.parse(source)
    values: dict[str, object] = {}
    for node in tree.body:
        if isinstance(node, ast.Assign):
            assigned_names = {
                target.id for target in node.targets if isinstance(target, ast.Name)
            }
            for name in names:
                if name in assigned_names and name not in values:
                    values[name] = ast.literal_eval(node.value)
        elif (
            isinstance(node, ast.AnnAssign)
            and isinstance(node.target, ast.Name)
            and node.target.id in names
            and node.target.id not in values
            and node.value is not None
        ):
            values[node.target.id] = ast.literal_eval(node.value)

    missing = [name for name in names if name not in values]
    if missing:
        raise ValueError(f"could not read {', '.join(missing)}")
    return values
