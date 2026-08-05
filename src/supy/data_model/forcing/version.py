"""Forcing data-contract version management."""

# No public version is declared until the forcing registry is complete.
CURRENT_FORCING_VERSION: str | None = None

# Published version -> manifest digest.
FORCING_VERSIONS: dict[str, str] = {}
