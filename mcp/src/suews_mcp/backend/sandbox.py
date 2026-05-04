"""Project-root sandbox.

Every tool that takes a path argument resolves it through a
:class:`ProjectRoot` instance. Paths that escape the root (via ``..``,
absolute paths to other directories, or symlinks) raise
:class:`SUEWSMCPSandboxError`.
"""

from __future__ import annotations

import os
from pathlib import Path
from typing import Optional, Union

from ..constants import ENV_PROJECT_ROOT


class SUEWSMCPSandboxError(PermissionError):
    """Raised when a path resolves outside the configured project root."""


class ProjectRoot:
    """Validates and resolves user-supplied paths against a fixed root."""

    def __init__(self, root: Optional[Union[str, Path]] = None) -> None:
        if root is None:
            root = os.environ.get(ENV_PROJECT_ROOT, os.getcwd())
        self._root = Path(root).resolve(strict=False)

    @property
    def root(self) -> Path:
        return self._root

    def resolve(self, path: Union[str, Path]) -> Path:
        """Resolve ``path`` and ensure it is under :attr:`root`.

        Raises :class:`SUEWSMCPSandboxError` if the resolved path is not a
        descendant of ``self._root`` (or the root itself). The error
        names the configured project root explicitly so users debugging
        a "wrong root" launch (no ``--root`` flag, server inheriting a
        Conductor temp cwd) can correct it without reading the
        sandbox's source (gh#1405).
        """
        candidate = Path(path)
        if not candidate.is_absolute():
            candidate = self._root / candidate

        resolved = candidate.resolve(strict=False)
        try:
            resolved.relative_to(self._root)
        except ValueError as exc:
            raise SUEWSMCPSandboxError(
                f"Path {path!r} resolves to {resolved} which is outside "
                f"the project root {self._root}. The project root is set "
                f"by the suews-mcp `--root` flag (or the "
                f"{ENV_PROJECT_ROOT} environment variable); pass the "
                f"workspace directory there if you expected the path to "
                f"be accepted."
            ) from exc

        return resolved
