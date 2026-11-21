"""Safe YAML load and dump helpers preferring PyYAML's libyaml C bindings."""

from __future__ import annotations

from typing import Any

import yaml

_SAFE_YAML_LOADER = getattr(yaml, "CSafeLoader", yaml.SafeLoader)
_SAFE_YAML_DUMPER = getattr(yaml, "CSafeDumper", yaml.SafeDumper)


def safe_load_yaml(stream: Any) -> Any:
    """Parse a single YAML document using ``CSafeLoader`` when available.

    Parameters
    ----------
    stream : str or bytes or file-like
        YAML text or readable stream.

    Returns
    -------
    Any
        Parsed Python object with the same safe-constructor semantics as
        :func:`yaml.safe_load`.
    """
    return yaml.load(stream, Loader=_SAFE_YAML_LOADER)


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
