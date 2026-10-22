"""Output data-contract version management."""

CURRENT_OUTPUT_VERSION: str | None = "1.1.4"
CURRENT_OUTPUT_VERSION: str | None = "1.1.6"

# Published version -> contract digest.
OUTPUT_VERSIONS: dict[str, str] = {
    "1.0.0": "sha256:742bf9f81da50dcfbeefada59fc32cc2ef62b88ae46b0ae7517e148a9d414ca0",
    "1.1.0": "sha256:cb083b648318a831784a15165cbcda53a509acc881d508d7dacc4aeb1e89c066",
    "1.1.1": "sha256:da6fccae0df185365141eb35d7c23ee55e2e1287e7f869799d9457dc22683498",
    "1.1.2": "sha256:71928fd2a0efcdacf594443c94a0712ac1ac9b9d51a2eebb50fba62f91d48e31",
    "1.1.3": "sha256:97b9038bdaab8d3cd5a2a29bbb900f8c29be77cbc8de34cbd52876bb78107463",
    "1.1.4": "sha256:2b1ad14acf3791a66b63118c031797bf91340b7bb79bd5a8e3918b91017464bd",
    "1.1.6": "sha256:f6e072ed0460aa554425814f4c953cd005e4848ab690ef13907df327ca7bc5ce",
}
