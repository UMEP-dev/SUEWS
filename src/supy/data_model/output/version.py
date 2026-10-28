"""Output data-contract version management."""

CURRENT_OUTPUT_VERSION: str | None = "1.1.9"

# Published version -> contract digest.
OUTPUT_VERSIONS: dict[str, str] = {
    "1.0.0": "sha256:742bf9f81da50dcfbeefada59fc32cc2ef62b88ae46b0ae7517e148a9d414ca0",
    "1.1.0": "sha256:cb083b648318a831784a15165cbcda53a509acc881d508d7dacc4aeb1e89c066",
    "1.1.1": "sha256:da6fccae0df185365141eb35d7c23ee55e2e1287e7f869799d9457dc22683498",
    "1.1.2": "sha256:71928fd2a0efcdacf594443c94a0712ac1ac9b9d51a2eebb50fba62f91d48e31",
    "1.1.3": "sha256:97b9038bdaab8d3cd5a2a29bbb900f8c29be77cbc8de34cbd52876bb78107463",
    "1.1.6": "sha256:f6e072ed0460aa554425814f4c953cd005e4848ab690ef13907df327ca7bc5ce",
    "1.1.7": "sha256:291af80c2d871df639fb377171562bf33deb2f9f0fbde3199f22000147250afc",
    "1.1.8": "sha256:0c39af71535be170d07967a6c2e36e0d23ef21a20d75132444c627167368f9c4",
    "1.1.9": "sha256:a0ac5f8bb5d6342888abb786c166af1c3490494cc22e46894b468868dac1cd1c",
}
