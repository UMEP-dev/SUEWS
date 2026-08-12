"""Forcing data-contract version management."""

CURRENT_FORCING_VERSION: str | None = "1.0.0"

# Published version -> contract digest.
FORCING_VERSIONS: dict[str, str] = {
    "1.0.0": "sha256:365357c22ab32a51b3404802462540c1a0e2ec41b6dbaced33348e191e729ff0",
}
