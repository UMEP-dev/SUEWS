# Recorded test debt

`known_defects.yml` links existing ledger records to the primary contract in
`contracts.yml`, a fixing or disposition issue, and a classification.
The ledgers retain the test behaviour; the mapping adds ownership and reporting.

Run `python scripts/lint/check_test_debt.py --strict --json test-debt.json` to
check the inventory and print Markdown, or use `--report` to report findings
without failing the command.
The checker uses only the standard library and PyYAML and never imports test or
model code.

A selector requires a ledger ID and may restrict exact `id`, `file`, `function`,
`group`, `category` or `owner_issue` values, or a literal `id_prefix`.
All attributes are conjunctive; there are no regular expressions or fallback
mappings.
Each extracted record must match exactly one selector, and each selector must
match at least one record.
An existing native owner must agree with the mapping.
Issue references are positive integers or strings of the form `#123`.
When the last entry in a selector disappears, remove its mapping too.

The registered sources are:

| Ledger | Source | Record unit |
| --- | --- | --- |
| R | `scripts/lint/rust_optional_reads_baseline.json` | Function/reader counter; reads reported as occurrences |
| U | `scripts/lint/fortran_uninit_baseline.json` | Compiler diagnostic |
| L | `scripts/lint/fortran_literals_baseline.json` | File/kind counter; literals reported as occurrences |
| S | `test/data_model/schema_contract_baseline.json` | Classified field, pending rule or warning swallow |
| M | `test/data_model/known_mutation_gaps.json` | Mutation record; anchor/mutation slices reported separately |
| J1, J2, J3 | `src/suews_bridge/tests/yaml_jacobian.rs` | Dead key, clobbered layer or non-unit delta pair |
| J0-multi, J0-nan | Same Rust file | Reviewed multi-owner or non-finite slot (currently empty) |
| O | `scripts/suews/gen_capi_field_observer.py` | Canonical excluded component; generated aggregate copies checked but not counted again |
| N | `scripts/lint/fortran_snan_expected_failures.json` | Expected failing JUnit node |
| X1 | `test/physics/test_option_sweep.py` | `OPTION_VALUES_XFAIL` entry |
| X | Python files under `test/` and `benchmark/` | Other xfail source site, identified by file, scope and ordinal |
| rust-ignore | Rust files under `src/suews_bridge/` | Ignore attribute, including conditional ignore (currently empty) |
| Q | `test/data_model/test_unit_metadata.py` | Conditional unit exception |
| T1, T2 | `test/data_model/test_legacy_table_roundtrip.py` | Sentinel column or version/variable pair |
| Z | `scripts/lint/check_saved_fortran_locals.py` | Allowed file/local pair (currently empty) |

Counts describe stored records, not unique underlying bugs: several records and
ledgers can refer to the same fault.
Occurrence weights are meaningful only within their own counter ledgers.
`defect`, `coverage_gap` and `accepted_exception` are reported separately.
Pending documentation rules use `coverage_gap`; active mutation failures and
unwired parameters use `defect`.
Legacy docstring ignores in `.ruff.toml` are outside this test-defect inventory.
Environment-dependent skips are not exhaustively inventoried.
The Rust reader supports literal string/pair arrays and fails on unsupported
syntax; it is not a full Rust parser.
The suppression scan detects calls named `xfail` and Rust ignore attributes;
it does not resolve arbitrary Python aliases or Rust macros.

The closing rule is detection only, as decided by Ting Sun on 21 November 2026.
The separate `Recorded test debt` workflow checks on pull requests and daily on
the default branch that every owner exists, is an issue rather than a pull
request, and is open.
Closed, missing and unverifiable owners fail that workflow and appear in its job
summary.
It has read permissions only, does not reopen issues, and is not connected to
an existing required check.
A successful audit says `green with N recorded defects`; this describes inventory
and ownership checks, not the scientific validity of the model.
Issue state is time-dependent and is not checked by offline pytest.
