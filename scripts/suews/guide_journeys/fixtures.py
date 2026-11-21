"""Reconstruct checksum-pinned fixture variants by exact byte replacement."""

import hashlib
from pathlib import Path


def fixture_bytes(
    root: Path, fixtures: dict, fixture_id: str, ancestors: tuple = ()
) -> bytes:
    """Check bases, unique replacement matches and final bytes before use."""
    if fixture_id in ancestors:
        raise ValueError(f"Cyclic fixture base: {fixture_id}")
    fixture = fixtures[fixture_id]
    if "base" in fixture:
        data = fixture_bytes(root, fixtures, fixture["base"], (*ancestors, fixture_id))
        for replacement in fixture["replacements"]:
            before = replacement["before"].encode("utf-8")
            after = replacement["after"].encode("utf-8")
            if not before or data.count(before) != 1:
                raise ValueError(
                    f"Fixture replacement must match exactly once: {fixture_id}"
                )
            data = data.replace(before, after, 1)
    else:
        data = (root / fixture["path"]).read_bytes()
    if hashlib.sha256(data).hexdigest() != fixture["sha256"]:
        raise ValueError(f"Fixture checksum mismatch: {fixture_id}")
    return data
