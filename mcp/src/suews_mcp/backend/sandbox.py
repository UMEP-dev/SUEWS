"""Project-root sandbox.

Every tool that takes a path argument resolves it through a
:class:`ProjectRoot` instance. Paths that escape the root (via ``..``,
absolute paths to other directories, or symlinks) raise
:class:`SUEWSMCPSandboxError`.
"""

from __future__ import annotations

import json
import os
from pathlib import Path
from typing import Optional, Union

from ..constants import ENV_ALLOWED_ROOTS, ENV_PROJECT_ROOT


class SUEWSMCPSandboxError(PermissionError):
    """Raised when a path resolves outside the configured project root."""


def _canonical_path(path: Union[str, Path]) -> Path:
    try:
        return Path(path).resolve(strict=False)
    except (OSError, RuntimeError, ValueError) as exc:
        raise SUEWSMCPSandboxError(f"Cannot resolve path {path!r}: {exc}") from exc


def _allowed_roots() -> list[Path]:
    """Read only trusted process configuration, never tool arguments."""
    try:
        home = _canonical_path(Path.home())
    except RuntimeError as exc:
        raise SUEWSMCPSandboxError(
            f"Cannot determine the home directory: {exc}"
        ) from exc
    if home == Path(home.anchor):
        raise SUEWSMCPSandboxError("The home directory must not be a filesystem root.")
    roots = [home]
    configured = os.environ.get(ENV_ALLOWED_ROOTS)
    if configured is None:
        return roots
    try:
        values = json.loads(configured)
        if not isinstance(values, list):
            raise ValueError("expected a JSON array")
        for value in values:
            if not isinstance(value, str) or not value or not Path(value).is_absolute():
                raise ValueError(
                    "each entry must be a non-empty absolute directory path"
                )
            root = _canonical_path(value)
            if root == Path(root.anchor) or not root.is_dir():
                raise ValueError(
                    "entries must be existing directories, not filesystem roots"
                )
            roots.append(root)
    except (ValueError, OSError) as exc:
        raise SUEWSMCPSandboxError(
            f"Invalid {ENV_ALLOWED_ROOTS}: {exc}. Configure a JSON array of "
            "absolute directory roots in the trusted MCP launcher."
        ) from exc
    return roots


class ProjectRoot:
    """Validates and resolves user-supplied paths against a fixed root."""

    def __init__(self, root: Optional[Union[str, Path]] = None) -> None:
        configured_root = os.environ.get(ENV_PROJECT_ROOT)
        if configured_root is not None:
            if not configured_root.strip():
                raise SUEWSMCPSandboxError(f"{ENV_PROJECT_ROOT} must not be empty.")
            session_root = _canonical_path(configured_root)
            requested_root = _canonical_path(root) if root is not None else session_root
            # Retain the session clamp: caller roots can narrow, never widen it.
            self._root = (
                requested_root
                if requested_root.is_relative_to(session_root)
                else session_root
            )
            return

        allowed = _allowed_roots()
        requested_root = _canonical_path(root if root is not None else ".")
        if requested_root == Path(requested_root.anchor) or not any(
            requested_root.is_relative_to(parent) for parent in allowed
        ):
            raise SUEWSMCPSandboxError(
                f"Project root {requested_root} is not authorised. Select a directory "
                f"under {allowed[0]}, or ask the trusted launcher to configure "
                f"{ENV_ALLOWED_ROOTS} (JSON array of absolute directory roots) or "
                f"{ENV_PROJECT_ROOT}. A temporary host CWD does not prevent passing "
                "an authorised project_root explicitly."
            )
        self._root = requested_root

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

        resolved = _canonical_path(candidate)
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

    def check_tree(self, path: Union[str, Path]) -> Path:
        """Check child targets, including dangling links and directory aliases.

        This is a preflight check, not an atomic filesystem sandbox: another
        process can still replace a path between this check and CLI access.
        """
        resolved = self.resolve(path)
        pending = [resolved]
        seen: set[Path] = set()
        while pending:
            current = self.resolve(pending.pop())
            if current in seen:
                continue
            seen.add(current)
            if current.is_dir():
                pending.extend(current.iterdir())
        return resolved

    def check_config(self, path: Path) -> None:
        """Confine forcing references using the inspector's config-relative base."""
        import yaml

        if not path.is_file():
            return  # The CLI supplies the missing-file diagnostic.
        try:
            data = yaml.safe_load(path.read_text(encoding="utf-8"))
            control = data.get("model", {}).get("control", {})
            forcing = control.get("forcing", {})
            value = (
                forcing["file"]
                if isinstance(forcing, dict) and "file" in forcing
                else control.get("forcing_file")
            )
            if isinstance(value, dict):
                value = value.get("value")
            for reference in value if isinstance(value, list) else [value]:
                if isinstance(reference, str):
                    self.resolve(path.parent / reference)
        except yaml.YAMLError:
            # SUEWSConfig.from_yaml uses the same safe loader, so malformed
            # YAML cannot reach an embedded file read. Keep CLI diagnostics.
            return
        except (OSError, ValueError, AttributeError) as exc:
            raise SUEWSMCPSandboxError(
                f"Cannot preflight config {path}: {exc}"
            ) from exc

    def check_runcontrol(self, path: Path, *, versioned: bool = False) -> None:
        """Check the canonical namelist and table paths used by conversion."""
        import f90nml

        # convert_to_yaml uses the supplied file's parent, then opens this name.
        canonical = self.resolve(path.parent / "RunControl.nml")
        if not canonical.is_file():
            return

        def check_reference(reference: str) -> Path:
            candidate = Path(reference)
            # Versioned conversion copies the namelist to a temporary folder.
            # A parent traversal safe beside the source may escape there.
            if versioned and not candidate.is_absolute() and ".." in candidate.parts:
                raise SUEWSMCPSandboxError(
                    "Versioned MCP conversion cannot relocate parent-relative "
                    "RunControl paths safely; use an absolute path within the project root."
                )
            return self.check_tree(path.parent / candidate)

        try:
            control = f90nml.read(canonical).get("runcontrol", {})
            # Both defaults occur in the legacy loaders/converters.
            references = (
                [control["fileinputpath"]]
                if "fileinputpath" in control
                else ["Input", "input"]
            )
            for reference in references:
                input_dir = check_reference(reference)
                # The legacy loader merges SPARTACUS namelist fields into
                # RunControl before loading the remaining tables.
                spartacus = self.resolve(input_dir / "SUEWS_SPARTACUS.nml")
                if spartacus.is_file():
                    for group in f90nml.read(spartacus).values():
                        if "fileinputpath" in group:
                            check_reference(group["fileinputpath"])
            # Conversion also scans the root and the conventional Input folder.
            self.check_tree(path.parent / "Input")
            for pattern in ("*.nml", "SUEWS*.txt", "*_data_*.txt"):
                for child in path.parent.glob(pattern):
                    self.resolve(child)
            # Versioned table conversion creates this directory, too.
            check_reference(control.get("fileoutputpath", "Output"))
        except (OSError, ValueError, TypeError) as exc:
            raise SUEWSMCPSandboxError(
                f"Cannot preflight RunControl {canonical}: {exc}"
            ) from exc
