"""Forcing data-contract version management."""

CURRENT_FORCING_VERSION: str | None = "2.1.0"

# Published version -> contract digest.
FORCING_VERSIONS: dict[str, str] = {
    "1.0.0": "sha256:365357c22ab32a51b3404802462540c1a0e2ec41b6dbaced33348e191e729ff0",
    "1.1.0": "sha256:74ff7013f1413715674609c4c27151b348488be9158e919304c25b6bb66c091f",
    "1.2.0": "sha256:6e64955e934fa6d0df177041f0706b277cebb488f52016dffbc0da997f22d26c",
    "2.0.0": "sha256:ba83cde8b1f66ebbf8f7f11dc178f40f961f8012c11c61f97207c26ecb710784",
    "2.1.0": "sha256:33a90350d0665bbde9801482ec2f78aa29e3fa4e9e7f358734ba5fc552a7de7e",
}
