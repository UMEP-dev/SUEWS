# Migration-guide fixtures

These are journey inputs, not release fixtures. Only tagged-dev11.yml and dev12.yml are stored as YAML files. Each derived input below is reconstructed from its named base by the exact byte replacement in test/docs/journeys/transition_guide.yml; each replacement must match once, and the derived SHA256 must match before copying into a journey. YAML is never parsed and re-serialised to derive a fixture. No forcing data is vendored; the runner copies it from the installed package using importlib.resources.

## tagged-dev11.yml

src/supy/sample_data/sample_config.yml at bbc19108f2, with exactly one paved to_soilstore: {value: 0.0} line inserted. The building block already omits that field.

SHA256: `32ee276339e7501939f724e72d244053bcd60cf27944375b23cd69306d521c85`

## untagged-dev11 (derived bytes)

tagged-dev11.yml with only the schema_version line removed.

SHA256: `51e71e9b5a3b236f2ffd5a10ae061346ce83b6e67f964d782df7159a81135acf`

## dev12.yml

Verbatim src/supy/sample_data/sample_config.yml at 590d3db0355f18d7a9337ac1117e715189703974.

SHA256: `90a6cb94829c95227546f1e6878f5bc41d5f3cee8f1be2969cc22a5a4fb8954c`

## nonzero-dev11 (derived bytes)

tagged-dev11.yml with the inserted paved to_soilstore value changed to 0.1.

SHA256: `03acb6d9f0222be13b8b44eabf264f3e509cb477767bd7097718bbd621ad3cbb`

## zero-area-dev12 (derived bytes)

dev12.yml with the bsoil waterdist block replaced by {}. Its surface fraction was already zero.

SHA256: `02c10251b6a0b83961bf6971728510a342fb561b250155e13943b3df51468fe1`

## earlier-guide.rst

Verbatim dev12 section of docs/source/inputs/transition_guide.rst at bd584e0d2c8d (upstream PR #2117), from original line 222 through the blank lines before the next section target. Preserved because the upstream branch may be squash-merged.

SHA256: `6028e7ede6a7990671b2b698ccb64826e2c75a53a4deed3a512cbffc9f21f9a1`

## stale-dev12 (derived bytes)

Derived from dev12.yml by changing only the top-level name to unrelated_archived_run. This is an unrelated, current-schema copy left by an earlier run. Older refers to directory history, not an unsupported schema. Its successful replay demonstrates why validity does not establish creation provenance.

SHA256: `10e2aa91e3a7eeb7e1f1dd3978479d4f77d0b497478dcf1ec135bb5fb93637ec`

## positive-area-dev12 (derived bytes)

The negative control uses zero-area-dev12 as its base and applies the manifest's two exact byte replacements: bare-soil fraction 0.0 to 0.01, and grass fraction 0.03 to 0.02, keeping the total surface fraction unchanged. The bare-soil waterdist remains {}. The labelled control expects validation to reject missing destinations for positive-area bare soil.

SHA256: `c1272713cf9e5776f64f5692845e8e95d767110afc763397754be8d26dc6c7d9`
