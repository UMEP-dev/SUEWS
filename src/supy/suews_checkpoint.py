"""Typed restart checkpoint support for SUEWS simulations."""

from __future__ import annotations

from dataclasses import dataclass, field
from datetime import datetime, timezone
import json
from pathlib import Path
from typing import Any, Mapping

import pandas as pd

from ._version_scm import __version__


def _utc_now_iso() -> str:
    return datetime.now(timezone.utc).isoformat().replace("+00:00", "Z")


def _normalise_timestamp(value: Any | None) -> str | None:
    if value is None:
        return None
    if isinstance(value, pd.Timestamp):
        return value.isoformat()
    if isinstance(value, datetime):
        return value.isoformat()
    return str(value)


def _state_to_json(value: Any) -> str:
    if isinstance(value, str):
        json.loads(value)
        return value
    return json.dumps(value, separators=(",", ":"), sort_keys=True)


def _state_to_object(value: str) -> Any:
    return json.loads(value)


def _normalise_grid_states(grid_states: Mapping[Any, Any]) -> dict[int, str]:
    if not grid_states:
        raise ValueError("SUEWSCheckpoint requires at least one grid state.")

    normalised: dict[int, str] = {}
    for grid_id, state in grid_states.items():
        normalised[int(grid_id)] = _state_to_json(state)
    return normalised


def _infer_state_schema_version(grid_states: Mapping[int, str]) -> int | None:
    versions: set[int] = set()
    for state_json in grid_states.values():
        payload = _state_to_object(state_json)
        if isinstance(payload, dict) and payload.get("schema_version") is not None:
            versions.add(int(payload["schema_version"]))

    if len(versions) > 1:
        raise ValueError(
            "SUEWSCheckpoint grid states contain multiple state schema versions: "
            f"{sorted(versions)}"
        )
    return next(iter(versions), None)


@dataclass(frozen=True)
class SUEWSCheckpoint:
    """Typed restart artifact carrying Rust SUEWS state JSON by grid ID."""

    grid_states: dict[int, str]
    supy_version: str = __version__
    state_schema_version: int | None = None
    created_at: str = field(default_factory=_utc_now_iso)
    last_timestamp: str | None = None

    def __post_init__(self) -> None:
        normalised_states = _normalise_grid_states(self.grid_states)
        object.__setattr__(self, "grid_states", normalised_states)
        object.__setattr__(self, "supy_version", self.supy_version or __version__)
        object.__setattr__(self, "created_at", self.created_at or _utc_now_iso())

        if self.state_schema_version is None:
            object.__setattr__(
                self,
                "state_schema_version",
                _infer_state_schema_version(normalised_states),
            )
        else:
            object.__setattr__(
                self, "state_schema_version", int(self.state_schema_version)
            )

        object.__setattr__(
            self, "last_timestamp", _normalise_timestamp(self.last_timestamp)
        )

    @classmethod
    def from_grid_states(
        cls,
        grid_states: Mapping[Any, Any],
        *,
        supy_version: str | None = None,
        state_schema_version: int | None = None,
        created_at: str | None = None,
        last_timestamp: Any | None = None,
    ) -> "SUEWSCheckpoint":
        """Build a checkpoint from Rust ``state_json`` values keyed by grid ID."""
        return cls(
            grid_states=dict(grid_states),
            supy_version=supy_version or __version__,
            state_schema_version=state_schema_version,
            created_at=created_at or _utc_now_iso(),
            last_timestamp=_normalise_timestamp(last_timestamp),
        )

    @classmethod
    def from_dict(cls, payload: Mapping[str, Any]) -> "SUEWSCheckpoint":
        """Load a checkpoint from a decoded JSON mapping."""
        if "grid_states" not in payload:
            raise ValueError("Checkpoint JSON must contain a 'grid_states' object.")
        return cls(
            grid_states=dict(payload["grid_states"]),
            supy_version=str(payload.get("supy_version") or __version__),
            state_schema_version=payload.get("state_schema_version"),
            created_at=str(payload.get("created_at") or _utc_now_iso()),
            last_timestamp=payload.get("last_timestamp"),
        )

    @classmethod
    def from_file(cls, path: str | Path) -> "SUEWSCheckpoint":
        """Read a checkpoint JSON file."""
        path = Path(path).expanduser().resolve()
        with path.open("r", encoding="utf-8") as checkpoint_file:
            payload = json.load(checkpoint_file)
        return cls.from_dict(payload)

    def to_dict(self) -> dict[str, Any]:
        """Return a JSON-serialisable checkpoint payload."""
        return {
            "supy_version": self.supy_version,
            "state_schema_version": self.state_schema_version,
            "created_at": self.created_at,
            "last_timestamp": self.last_timestamp,
            "grid_states": {
                str(grid_id): _state_to_object(state_json)
                for grid_id, state_json in sorted(self.grid_states.items())
            },
        }

    def to_file(self, path: str | Path) -> Path:
        """Write the checkpoint JSON file and return its path."""
        path = Path(path).expanduser().resolve()
        path.parent.mkdir(parents=True, exist_ok=True)
        with path.open("w", encoding="utf-8") as checkpoint_file:
            json.dump(self.to_dict(), checkpoint_file, indent=2)
            checkpoint_file.write("\n")
        return path
