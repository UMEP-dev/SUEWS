"""``suews://examples/{name}`` resource."""

from __future__ import annotations

from typing import Any

from ..tools.examples import read_example


def read_example_resource(name: str) -> dict[str, Any]:
    """Resource alias for the :func:`read_example` tool."""
    return read_example(name)
