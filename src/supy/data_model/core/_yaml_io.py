"""Preserve the public PyYAML reader and accelerate generated YAML output."""

from __future__ import annotations

from typing import Any

import yaml
from yaml import safe_load as safe_load_yaml

__all__ = ["safe_dump_yaml", "safe_load_yaml"]

_SAFE_YAML_DUMPER = getattr(yaml, "CSafeDumper", yaml.SafeDumper)


def safe_dump_yaml(data: Any, stream: Any = None, **kwargs: Any) -> str | None:
    """Serialise safe YAML using ``CSafeDumper`` when available.

    Parameters
    ----------
    data : Any
        Safe Python structure to serialise.
    stream : file-like, optional
        Destination stream; returns a YAML string when ``None``.
    **kwargs : Any
        Additional keyword arguments forwarded to :func:`yaml.dump`.

    Returns
    -------
    str or None
        Serialised YAML text when *stream* is ``None``, else ``None``.
    """
    return yaml.dump(data, stream=stream, Dumper=_SAFE_YAML_DUMPER, **kwargs)
