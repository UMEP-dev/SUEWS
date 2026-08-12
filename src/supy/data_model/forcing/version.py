"""Forcing data-contract version management."""

CURRENT_FORCING_VERSION: str | None = "1.1.0"

# Published version -> contract digest.
FORCING_VERSIONS: dict[str, str] = {
    "1.0.0": "sha256:365357c22ab32a51b3404802462540c1a0e2ec41b6dbaced33348e191e729ff0",
    "1.1.0": "sha256:74ff7013f1413715674609c4c27151b348488be9158e919304c25b6bb66c091f",
}
