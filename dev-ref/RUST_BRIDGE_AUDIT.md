# Rust bridge baseline audit

Date: 6 Nov 2026. Source revision:
`387339e1186e22f33167d1244589fa66e5754116` (the #1996 documentation branch,
based on `1726870a66662abdf444463130d1dcd79d66381f`). The accompanying contract,
design document and comments do not change runtime behaviour.

This audit applies [BR-01 through BR-10](../.claude/rules/rust/bridge-contract.md)
to the current implementation. Issue and PR references below describe the
reviewed snapshot, not their future merge status. A proposed fix is not evidence
that this revision is fixed.

Since this baseline, #1997, #1998, #1999 and #2000 have been closed by #1992 to #1995,
and #2004 by #2009; the findings below describe the baseline, not current master.
If the documentation branch is deleted, the baseline is recoverable from its base
commit `1726870a66662abdf444463130d1dcd79d66381f` on master.

## Method and limits

Inspected the module families in the [design map](RUST_BRIDGE_DESIGN.md), the
Python entry points in `_run_rust.py`, the C-API driver, representative parameter
facades, build configuration and relevant tests. Followed STEBBS, nested
vegetation parameters, site scalars, state/checkpoint, direct-output paths and
the legacy scalar OHM facade.
The findings below identify source symbols so they remain locatable after
comments move line numbers.

This is a static boundary audit plus the validation recorded below. It is not
an exhaustive proof of every field in every facade, a scientific validation of
all options, or a concurrency/unsafe-code proof. In particular, representative
pack/unpack inspection does not certify all handwritten offsets. Existing
tests are listed as acceptance evidence to use; actual executions and results
are recorded in the validation section below.

## Boundary coverage

| Boundary / rules | Status and evidence | Limit or next action |
|---|---|---|
| Ownership and configuration, BR-01/02/03 | **Violation.** `yaml_config::load_run_config_from_value` constructs Fortran defaults, applies optional overrides and supplies dimension fallbacks; `SiteScalars::default` supplies more values. | Remove implicit resolution under #2001/#2002/#2003/#2004. Keep file-path absence distinct from missing in-memory forcing values. |
| Legacy OHM scalar API, BR-01 | **Violation, statically confirmed.** `c_api/ohm.f95` contains the QS formula and derivative-update calculations; `core::qs_calc` and `core::ohm_step` invoke this separate implementation. | Proposed follow-up under #1989; duplication is confirmed, but numerical divergence from the owning model has not been established. |
| Names and scalar mapping, BR-04 | **Violation.** `apply_site_scalar_overrides` reads the legacy CO2 and FlowChange paths; normalisation and nested land-cover loading do not make those scalar reads correct. | #1998 and #2000; require tests at the buffer/kernel destination. |
| Shared codecs and representative parameter facades, BR-04 | **Checked, statically, for structure.** `validate_flat_len`, schema helpers and nested parent layouts expose lengths/versions; `stebbs_prm` and `lc_evetr_prm` show the hand-maintained mappings. | Complete field-to-kernel coverage is **not verified**. Default round trips can miss swapped positions and wrong destinations. |
| Site/state composition, BR-04/06 | **Checked, statically.** `encode_site_members`, `encode_state_members`, `validate_site_codec` and the driver table of contents establish the composite boundary. | All option-dependent state initialisation is **not verified**; #2003/#2010 remain open work. |
| Python output-buffer boundary, BR-05 | **Checked, statically.** `lend_output_buffer`, `copy_forcing_buffer`, `check_disjoint` and `run_simulation_into` check layout, overlap, dimensions and native integer conversions. | The checks depend on live buffer guards and no concurrent caller mutation. No blanket FFI soundness claim; regression tests in `test/core/test_output_buffers.py`. |
| Checkpoint envelope, BR-06 | **Checked, statically.** `checkpoint::validate_timer` checks integer/range constraints; envelope loading rejects legacy state-only and unsupported versions; `suews_state::validate_member_set` requires the complete set. | Cold-start defaults are a separate concern. Use `test/core/test_checkpoint.py` and `test/core/test_stream_chunk.py` for restart equivalence. |
| Forcing reader/interpolation, BR-01/07 | **Checked, statically, as adjacent I/O.** `read_forcing_block` performs pressure conversion; `interpolate_forcing` selects behaviour by `InterpKind`. | Full parity with Python for every variable/timestep is **not verified**. Preserve owning registry semantics; #1547 is a unit-policy discussion, not permission to alter units here. |
| Output representation, BR-04/07 | **Checked, statically, for dimensions.** `OutputBlockMatrix::new` uses checked multiplication; nested payloads carry dimensions; `build.rs` generates group widths. **Not verified** for full metadata parity. | `output_io::SUEWS_OUTPUT_NAMES` is manual; other Arrow groups use index-based names. Width generation does not establish canonical names/units (#1657). |
| Diagnostics, BR-08 | **Checked, statically.** `run_simulation_into` checks the C-API status, kernel error message and warning buffers before returning state/results. | Warning storage is bounded by `KERNEL_WARNING_MAX`; inspect total versus stored counts when changing this path. Not an audit of every Fortran error site. |
| Build modes, BR-09 | **Violation, reproduced.** Non-physics `build.rs` selects a limited kernel source set while compiling facades that require additional modules (#1990). The Rust test workflow selects `physics`; Clippy is lint-only. | Establish the supported feature/API contract, then repair and test any retained mode or explicitly retire it. Do not present Clippy as a native link test. |
| Change governance, BR-10 | **Checked for this change.** Contract, design, source comments and this audit share one rule set and explicit follow-ups. | Future changes need their own revision-specific evidence; this document is not a permanent approval. |

## Confirmed findings and disposition

Severity is the repository's `blocking | major | minor | nit` vocabulary.
These are existing implementation findings, not new behaviour introduced by
the documentation change. Each row gives a stable signature and a concrete
acceptance condition for its follow-up.

| Signature / rule | Severity and finding | Follow-up and acceptance |
|---|---|---|
| `yaml_config.rs / load_run_config_from_value / implicit defaults` BR-01/02 | **Major:** default-then-override accepts incomplete input; `unwrap_or(5)` chooses layer dimensions. | #2001/#2002. Resolve in the Python model, reject missing active fields in the bridge with a path, and exercise both Python and engine input. |
| `c_api/ohm.f95 / suews_ohm_qs_calc and suews_ohm_dqndt_step / copied formulas` BR-01 | **Major, responsibility duplication:** the legacy MVP facade directly implements formulas owned by `module_phys_ohm`; its header acknowledges the mirroring. The QS expression also exists in `suews_phys_ohm.f95::OHM_QS_cal`. No numerical defect is claimed here. | Follow-up proposed, not filed: link to #1989 and establish the scalar API/feature-mode contract, then use the owning model procedures or an explicitly agreed API change. Verify valid/invalid timestep and state behaviour against those procedures; do not expand the copied implementation. |
| `yaml_config.rs / parse_numeric_sequence / malformed element` BR-02 | **Major:** failure to parse one element can discard the whole supplied list and retain defaults. | #1997, proposed PR #1992. Error must identify the list/index; valid input must retain every value. |
| `yaml_config.rs / apply_site_scalar_overrides / CO2 path` BR-04 | **Major:** the scalar reader asks for `properties.CO2PointSource` after normalisation, rather than the current nested anthropogenic-emissions path. | #1998, proposed PR #1993. A distinct `anthropogenic_emissions.co2.emission_co2_point_source` must reach the intended site scalar and kernel input. |
| `yaml_config.rs / apply_site_overrides / resizing before land-cover overrides` BR-02/04 | **Major:** layer arrays can be filled from pre-override land-cover values. | #1999, proposed PR #1994. Check non-default land-cover values and a supplied/omitted layer array separately; do not introduce new bridge defaults as the long-term remedy. |
| `yaml_config.rs / apply_site_scalar_overrides / FlowChange path` BR-04 | **Major:** the water parameter and driver scalar are loaded from different paths, leaving the driver scalar at its default. | #2000, proposed PR #1995. Verify transport from the nested water property; coordinate the kernel rate conversion below before enabling the corrected input. |
| `yaml_config.rs / set_mapped_value / unknown key` BR-02/08 | **Major:** a map-based key not found in the target map is silently ignored. | #2002. Establish permitted metadata versus model fields; reject unrecognised active model inputs without breaking documented metadata handling. |
| `yaml_config.rs / apply_state_overrides / cold-start fallback` BR-01/03/06 | **Major:** state construction still mixes transport with fallback initialisation, including deep-soil and hot-water values. | #2003. Resolve active initial state upstream and check option branches; checkpoint decoding must remain strict. |
| `suews_ctrl_driver.f95 / modState_tstepstart / snow disabled` BR-03/06 | **Major:** initial snow values can be captured before the snow-off reset. This is a kernel/state-ordering issue as well as a bridge-input issue. | #2010. Compare snow-off runs with contrasting initial snow and obtain scientific evidence for any behaviour change. Do not guess new initial values in Rust. |
| `suews_phys_snow.f95 / SnowCalc / FlowChange timestep scaling` BR-01/07 | **Major:** forwarding a previously dropped non-zero hourly rate exposes the snow path adding the full hourly value at every timestep. | #2012 overlaps #1927 item 1; draft PR #1929 contains a kernel fix. Integrate with #2000 using the current nested path and test multiple timesteps. |
| `build.rs / non-physics Fortran source selection / missing type modules` BR-09 | **Major within the advertised reduced build, Fortran build configuration:** the selected sources do not provide all modules required by the facades' `USE` statements. This is not a Rust language or hydrology-model defect. | #1990. Establish which feature modes and scalar APIs remain supported. If the reduced mode is retained, build its full ordered type-module dependency closure and test clean compilation/linking. If retired, update the advertised contract and provide a clear transition. Normal packaging and Rust CI use the separate physics mode. |

This change adds comments, design and audit rules. Runtime findings with
existing issues stay there; the additional OHM ownership finding is a proposed
follow-up, not a newly filed issue. Fixes remain small, testable and coordinated.
#1989 is already a child of #1991 and remains the
discussion point for auditability and proposed additions.

For #1990, `suews_type_hydro.f95` defines `module_type_hydro` and depends on
the constants module. Its `.mod` file is a compiler-generated interface needed
when compiling `c_api/hydro_state.f95`, which has `USE module_type_hydro`.
The non-physics branch of `build.rs` compiles only `suews_ctrl_const.f95` and
`suews_type_surface.f95` before the facades, omitting the hydrology type and
other prerequisites. The physics branch instead searches the prebuilt
`src/suews/mod` directory. Any retained reduced-mode repair belongs in native build orchestration;
`src/suews/Makefile.deps` already records the kernel's Fortran dependencies.

## Remaining evidence and related work

- **BR-04, not verified:** field-by-field provenance for all parameter/state
  families and every nested parent. `codec::to_map` zips names and values;
  `schema_info` checks their counts, but a round trip alone does not prove
  mapping order or that a caller invoked that check. Extend wiring coverage
  alongside each affected fix; existing STEBBS and irrigation wiring tests are
  examples, not whole-bridge certification.
- **BR-03, not verified:** all active versus inactive state fields. #1932's
  first-step temperature ordering and #2010 need model-owner reasoning.
  Some optional snow parameters in #1982 also remain unresolved; fixing null
  `sdd_id` and `flowchange` does not settle the whole snow parameter family.
- **BR-07, not verified:** complete forcing/output registry parity (#1651,
  #1657), including Arrow names outside the SUEWS group. Existing generated
  widths reduce one drift risk but leave metadata and unit evidence separate.
- **BR-09, related maintenance:** #1790 concerns rebuild cost and dependency
  tracking. Parallel native compilation already exists; do not re-propose it
  as the remaining fix. Performance is separate from contract correctness.

These gaps need focused acceptance evidence, not a new generic abstraction or
a second set of defaults. New proposals should link back to #1989; this audit
does not create or rewrite another contributor's issue.

## Validation of this documentation change

Ran in an isolated Linux source snapshot of the baseline plus this
documentation/comment change, with a fresh environment: Python 3.12.12,
Rust 1.95.0 and GNU Fortran 11.5.0. The archive build used generated version
metadata (`2026.11.6.dev4`) and `SUEWS_KNOWLEDGE_GIT_SHA` set to the baseline
revision above. Both Python and the native extension were confirmed to load
from this snapshot, not an existing checkout.

| Check | Result |
|---|---|
| `uv pip install --no-build-isolation --no-deps -e .` | Passed; built the native extension from the snapshot. |
| `make test-smoke` | 23 passed, 1 skipped, 3200 deselected. |
| `python -m pytest test/docs/test_bridge_parameter_guide.py test/core/test_checkpoint.py test/core/test_stream_chunk.py test/core/test_output_buffers.py -q` | 220 passed, 25 warnings, including provisional-output warnings; no failures. |
| `cargo test --manifest-path src/suews_bridge/Cargo.toml --features physics` | 384 library tests and 137 CLI tests passed; zero doc tests. |
| `SUEWS_BRIDGE_LINT_ONLY=1 RUSTDOCFLAGS="-D warnings" cargo doc --manifest-path src/suews_bridge/Cargo.toml --all-features --document-private-items --no-deps` | Passed, including private-item documentation. This does not test native linkage. |
| `cargo test --manifest-path src/suews_bridge/Cargo.toml --no-default-features` | Reproduced #1990: `c_api/hydro_state.f95` cannot find `module_type_hydro.mod`; exit 101. Kept as an unresolved implementation finding. |
| Markdown target and ASCII inspection | 21 relative links resolved; new documents contain ASCII text. |
| `python scripts/lint/check_rust_yaml_aliases.py` | Passed: 194 alias pairs, 139 canonical Rust field identifiers and 3 physics-family entries. |
| Rust source diff inspection | All 69 added/removed source lines are comments or whitespace; no executable Rust change. |

These checks validate the documentation change and selected existing
boundaries. They do not close the runtime findings or the unverified coverage
listed above.
