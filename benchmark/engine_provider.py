"""Engine-provider seam for retrospective runs.

Documents how a historical version is actually executed. Today the only
realisation is the implicit ``PinnedVenvProvider`` -- the per-version
``uv venv`` + ``pip install supy==X`` flow described in REPRODUCE.md. A future
``RustBridgeProvider`` would dispatch in-process through the bridge's runtime
version-query seam. Neither is implemented in Phase 1.

Note: the Rust bridge only exists from ~2026.4.3, so it can reach FEWER
historical versions than pinned venvs already do; it is an elegance /
performance option for the modern era, never the vehicle for historical reach.
"""

from __future__ import annotations

from abc import ABC, abstractmethod
from typing import Any


class EngineProvider(ABC):
    """Runs a SUEWS configuration under a specific historical version."""

    @abstractmethod
    def run(self, tag: str, config: Any, forcing: Any) -> Any:
        """Execute ``config`` against ``forcing`` under release ``tag``."""


class PinnedVenvProvider(EngineProvider):
    """Phase 2: provision a pinned ``uv`` venv and run ``supy==tag`` in it."""

    def run(self, tag: str, config: Any, forcing: Any) -> Any:
        raise NotImplementedError(
            "pinned-venv orchestration is Phase 2 (see benchmark/REPRODUCE.md)"
        )


class RustBridgeProvider(EngineProvider):
    """Phase 2+: in-process dispatch via the bridge runtime version query."""

    def run(self, tag: str, config: Any, forcing: Any) -> Any:
        raise NotImplementedError("in-process engine dispatch is Phase 2+")
