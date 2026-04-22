# Release-config fixtures

Vendored sample YAML configs from past formal supy releases. Used by
`test/data_model/test_release_compat.py` to guard against schema drift across
releases - see [#1301](https://github.com/UMEP-dev/SUEWS/issues/1301) and
[#1302](https://github.com/UMEP-dev/SUEWS/issues/1302).

## Contents

One fixture per formal supy release tag, vendored verbatim via
`git show <tag>:src/supy/sample_data/sample_config.yml`.
`test_release_compat.py` asserts each fixture either parses directly
under the current validator (identity path) or upgrades cleanly via the
registered handler in `src/supy/util/converter/yaml_upgrade.py`.

- `2025.10.15.yml` - tag
  [`2025.10.15`](https://github.com/UMEP-dev/SUEWS/releases/tag/2025.10.15).
  Identity path (parses unchanged).
- `2025.11.20.yml` - tag
  [`2025.11.20`](https://github.com/UMEP-dev/SUEWS/releases/tag/2025.11.20).
  Identity path (parses unchanged).
- `2026.1.28.yml` - tag
  [`2026.1.28`](https://github.com/UMEP-dev/SUEWS/releases/tag/2026.1.28).
  Predates the STEBBS setpoint split
  ([#1261](https://github.com/UMEP-dev/SUEWS/pull/1261)); upgraded by the
  `2026.1 -> 2026.4` handler in `src/supy/util/converter/yaml_upgrade.py`.
- `2026.4.3.yml` - tag
  [`2026.4.3`](https://github.com/UMEP-dev/SUEWS/releases/tag/2026.4.3).
  Predates the Category 1 fused-identifier rename sweep
  ([#1256](https://github.com/UMEP-dev/SUEWS/issues/1256)); upgraded by
  the `2026.4 -> 2026.5` handler in
  `src/supy/util/converter/yaml_upgrade.py`.

## Policy

- Do **not** hand-edit these files.
- To refresh for a new formal release, run
  `git show <new-tag>:src/supy/sample_data/sample_config.yml
   > test/fixtures/release_configs/<new-tag>.yml`
  as part of the `prep-release` workflow, with a justifying commit message.
- If a PR on master breaks parsing of any vendored fixture, the PR must either
  (a) add a matching handler in `src/supy/util/converter/yaml_upgrade.py`
  (see [#1304](https://github.com/UMEP-dev/SUEWS/issues/1304)) or
  (b) refresh the fixture with an explanatory commit that documents the
  breaking change.
