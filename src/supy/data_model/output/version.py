"""Output data-contract version management."""

CURRENT_OUTPUT_VERSION: str | None = "1.1.0"

# Published version -> contract digest.
OUTPUT_VERSIONS: dict[str, str] = {
    "1.0.0": "sha256:742bf9f81da50dcfbeefada59fc32cc2ef62b88ae46b0ae7517e148a9d414ca0",
    "1.1.0": "sha256:cb083b648318a831784a15165cbcda53a509acc881d508d7dacc4aeb1e89c066",
}
