"""Reference-source seam for the drift harness.

Unit B reads release reference statistics through this interface so that the
Phase-2 legacy import (the external SUEWS-Benchmark 73-column outputs) can be
added without touching the drift code.

Today only ``LocalBenchmarkRef`` is realised; ``ExternalLegacyRef`` is a
documented Phase-2 stub.
"""

from __future__ import annotations

from abc import ABC, abstractmethod


class ReferenceSource(ABC):
    """A source of per-release reference statistics keyed by version tag."""

    @abstractmethod
    def stats_for(self, tag: str) -> dict:
        """Return the reference statistics block for ``tag``."""


class LocalBenchmarkRef(ReferenceSource):
    """Reads modern reference blocks straight from a parsed ``index.json``."""

    def __init__(self, index: dict):
        self._versions = index.get("versions", {})

    def stats_for(self, tag: str) -> dict:
        if tag not in self._versions:
            raise KeyError(f"no local benchmark entry for {tag!r}")
        return self._versions[tag]


class ExternalLegacyRef(ReferenceSource):
    """Phase 2: parse the external SUEWS-Benchmark 73-column ``*_SUEWS_60.txt``.

    Constructed with the case path (e.g.
    ``cases/London_KCL/v2020a``) recorded in the registry's
    ``reference_source`` field. Not implemented in Phase 1.
    """

    def __init__(self, case_path: str):
        self.case_path = case_path

    def stats_for(self, tag: str) -> dict:
        raise NotImplementedError(
            "legacy external reference import is Phase 2 "
            f"(case_path={self.case_path!r})"
        )
