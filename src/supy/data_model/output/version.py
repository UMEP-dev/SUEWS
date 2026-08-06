"""Output data-contract version management."""

# No public version is declared until the output registry is complete.
CURRENT_OUTPUT_VERSION: str | None = None

# Published version -> contract digest.
OUTPUT_VERSIONS: dict[str, str] = {}
