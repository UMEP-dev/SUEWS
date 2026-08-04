"""Deterministic JSON serialisation for governed data-interface artefacts."""

from __future__ import annotations

import hashlib
import json
from typing import Any


def canonical_json_bytes(value: Any) -> bytes:
    """Return the canonical UTF-8 representation used for stored artefacts.

    Keys are sorted, insignificant whitespace is removed and non-finite numbers
    are rejected because they are not valid JSON.  The trailing newline is part
    of the canonical byte sequence and therefore part of its digest.
    """
    text = json.dumps(
        value,
        allow_nan=False,
        ensure_ascii=False,
        separators=(",", ":"),
        sort_keys=True,
    )
    return f"{text}\n".encode()


def sha256_digest(content: bytes) -> str:
    """Return a labelled SHA-256 digest for exact artefact bytes."""
    return f"sha256:{hashlib.sha256(content).hexdigest()}"
