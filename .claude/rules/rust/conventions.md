---
paths:
  - src/suews_bridge/**/*.rs
  - src/suews_bridge/c_api/**/*.f9*
  - src/suews_bridge/Cargo.toml
  - src/suews_bridge/Cargo.lock
  - src/suews_bridge/build.rs
---

# Rust Bridge Conventions

Apply [bridge-contract.md](bridge-contract.md) for responsibility boundaries
and the BR-01 through BR-10 audit requirements. The
[design](../../../dev-ref/RUST_BRIDGE_DESIGN.md) maps the implementation;
the [baseline audit](../../../dev-ref/RUST_BRIDGE_AUDIT.md) records known gaps.

The Rust bridge (`src/suews_bridge/`) replaced f90wrap as the Fortran-Python interface. It is a three-layer system:

- **Python layer** (PyO3) -- `#[pyclass]` types exposed to supy via maturin wheels
- **Rust safe wrapper** -- typed structs, codec, config parsing, CLI, simulation orchestration
- **Fortran C API** (`ffi.rs` + `c_api/` sources) -- `unsafe extern "C"` bindings to Fortran physics

---

## Architecture

- `ffi.rs` -- unsafe extern declarations for Fortran C API functions
- `codec.rs` -- `StateCodec` / `CompositeCodec` traits for flat-array <-> typed-struct conversion
- `lib.rs` -- module re-exports + PyO3 Python module definition
- `main.rs` -- standalone CLI application (JSON I/O subcommands)
- `sim.rs` -- multi-timestep simulation orchestration
- `yaml_config.rs` -- YAML run configuration parsing
- `build.rs` -- compiles Fortran C API sources via gfortran, generates `output_cols.rs`
- State modules (`heat_state.rs`, `hydro_state.rs`, `core.rs`, etc.) -- typed wrappers around Fortran state arrays
- Parameter modules (`ohm_prm.rs`, `ehc_prm.rs`, `snow_prm.rs`, etc.) -- typed parameter structs

---

## Building

```bash
# Rust engine CLI (standalone, no Python): cargo build --features physics,arrow-output
# Needs the SUEWS physics library first (make dev, or make -C src/suews all)
make bridge
# Output: src/suews_bridge/target/release/suews-engine
# `suews-engine run` takes resolved YAML only (what `suews run` hands the
# bridge); it is a developer/test binary, not a user command (gh#2004)

# Python module (for development)
cd src/suews_bridge && maturin develop --features python-extension,physics

# Full project build (includes Rust bridge as dependency)
make dev
```

**Prerequisites**: Rust toolchain (`rustup.rs`), gfortran (for Fortran C API compilation in build.rs)

No `rust-toolchain.toml` -- uses system Rust. CI installs via rustup.

---

## Feature Flags (Cargo.toml)

- `physics` -- enables Fortran physics compilation and FFI bindings (required for simulation)
- `python-extension` -- enables `python` and PyO3's extension-module linking
- `python` -- enables PyO3; the dependency selects the Python 3.12+ stable ABI
- `arrow-output` -- enables Apache Arrow IPC output format
- `capi-observer` -- test-only feature (implies `physics`; mutually exclusive with `python-extension` in `build.rs`) that compiles the forward field observers and the reverse neighbourhood-state observer for the `capi_field_observer` integration test [fact: git grep -n "capi-observer" src/suews_bridge/Cargo.toml src/suews_bridge/build.rs]

The maturin build uses `python-extension` + `physics`.

---

## Adding a New State Type

Follow the existing pattern (e.g. `ohm_state.rs` or `heat_state.rs`):

1. Create `src/<name>_state.rs` with a struct holding typed fields
2. Implement `StateCodec` trait:
   - `schema()` -- return `TypeSchema` with field names and flat length
   - `from_flat(flat: &[f64])` -- deserialise from Fortran flat array
   - `to_flat(&self)` -- serialise back to flat array
3. Add `#[pyclass]` and PyO3 methods if Python-visible (`to_dict`, `from_dict`, `to_values_payload`, `from_values_payload`)
4. Register in `lib.rs` module list and Python module
5. Add corresponding Fortran C API functions in `c_api/` if new physics calculations are needed
6. Update `src/suews_bridge/build.rs` Fortran source list if new `.f95` facades are added to `c_api/`, and the derived type's entry in `src/suews_bridge/bridge-manifest.json` (`python scripts/suews/check_bridge_manifest.py`) when a public Fortran type becomes bridged [fact: git grep -n c_api/driver.f95 src/suews_bridge/build.rs && git grep -n "\"status\": \"bridged\"" src/suews_bridge/bridge-manifest.json]
7. Regenerate and verify the C-API field-binding observer artefacts and test harness (see "C-API field-binding observer" below) whenever a bridged Fortran `TYPE`, `c_api/*_unpack` routine, Rust struct/codec, or `bridge-manifest.json` entry changes [fact: git grep -n "gen_capi_field_observer.py" .github/workflows/rust-capi-observer.yml]

---

## Adding a New FFI Function

1. Declare the `extern "C"` function in `ffi.rs` with `#[cfg(feature = "physics")]`
2. Write the Fortran C API wrapper in `c_api/` (ISO_C_BINDING interface to existing Fortran physics)
3. Add the `.f95` file to the source list in `build.rs`
4. Call from Rust via `unsafe { ffi::function_name(...) }` with proper error checking

---

## Codec Pattern

The bridge marshals data between Fortran flat arrays and typed Rust structs:

- Fortran stores state as contiguous `real(8)` arrays
- Rust `StateCodec::from_flat()` reads the array into named struct fields
- Rust `StateCodec::to_flat()` writes struct fields back to a flat array
- Schema versioning (`schema_version`) identifies compatible payloads; it does
  not automatically migrate an incompatible layout
- `ValuesPayload` wraps `(schema_version, Vec<f64>)` for JSON serialisation

---

## Error Handling

- Fortran C API returns integer error codes (`SUEWS_CAPI_OK`, `SUEWS_CAPI_BAD_BUFFER`, etc.)
- Rust converts these to `BridgeError` variants via `Result<T, BridgeError>`
- Python receives these as native Python exceptions via PyO3

---

## Code Size (heuristics, not gates)

- **One concern per module.** Follow the existing layout: each responsibility has
  its own file (`ffi.rs`, `codec.rs`, `sim.rs`, ...), and each new state or
  parameter type gets its own `<name>_state.rs` / `<name>_prm.rs` (see "Adding a
  New State Type"). Do not accumulate unrelated types in one module.
- **Small functions.** Keep functions focused. `cargo clippy` is an automated
  CI gate (`.github/workflows/rust-clippy.yml`, `-D warnings`); see "Clippy CI
  gate" below. `too_many_arguments` (>7 args) and `type_complexity` are
  deliberately allowed crate-wide (top of `src/suews_bridge/src/lib.rs`) and so
  remain informal signals, not gating -- the FFI/binding surface has many-arg
  constructors and wide tuple returns inherent to the model. Cognitive-complexity
  is a `nursery` lint you must still opt into (`cargo clippy -- -W
  clippy::cognitive_complexity` or a `[lints]` entry).
- A change that only stays small by editing an oversized module is a signal to
  split off a preparatory refactor first (see `.claude/rules/work-sizing.md`).

---

## Clippy CI gate

`cargo clippy` is a CI check on any PR touching `src/suews_bridge/**`
(`.github/workflows/rust-clippy.yml`, `-D warnings`, `--all-features
--all-targets`).

- **Lint-only build.** The job sets `SUEWS_BRIDGE_LINT_ONLY=1`, which makes
  `build.rs` skip native Fortran compilation/linking and only generate
  `output_cols.rs`. clippy type-checks the whole crate (including the
  `physics`-gated FFI) without gfortran or any prebuilt SUEWS `.mod` / static
  libs, because a check build never links.
- **Justified crate-wide allowances** live at the top of `lib.rs`:
  `useless_conversion` (pyo3 `#[pymethods]` macro noise), `non_camel_case_types`
  (community-standard forcing/variable names, naming-convention Rule 5),
  `type_complexity` and `too_many_arguments` (informal signals; see "Code
  Size"). Everything else is denied. Prefer a *scoped* `#[allow]` with a
  one-line justification for a genuine local false positive over widening the
  crate-level set.
- **Bypass.** A maintainer may add the `0-ci:clippy-ok` label to skip the gate
  for a PR.

---

## C-API field-binding observer

The [parent build workflow](../../../.github/workflows/build-publish_to_pypi.yml)
(`#2144`) calls the reusable
[observer workflow](../../../.github/workflows/rust-capi-observer.yml) for
`pull_request` and `merge_group` when the
[`rust_capi_observer` path filter](../../../.github/path-filters.yml) matches.
It passes the merge-group head SHA (or `github.sha`) to the child. The parent's
`PR build validation` job depends on `rust_capi_observer` and passes its result
and changed flag to [pr-gate.sh](../../../.github/scripts/pr-gate.sh), whose
`check_path_filtered_producer` enforces success when the filter matches.
The standalone observer retains its path-filtered `pull_request` trigger; it
has no standalone `merge_group` trigger. Merge-queue enforcement comes through
the parent, not the standalone run
[fact: git grep -n "rust_capi_observer:\|RUST_CAPI_OBSERVER\|head_sha:" .github/workflows/build-publish_to_pypi.yml .github/scripts/pr-gate.sh && git grep -n "pull_request:\|workflow_call:\|merge_group:" .github/workflows/rust-capi-observer.yml].
It checks that every Rust encoder and Fortran `c_api/*_unpack` routine delivers
globally distinct sentinels to the exact named Fortran kernel component rather
than relying on `*_shadow` or `*_pack` round trips.

- **Generated test artefacts (do not hand-edit).** The stdlib-only generator
  `scripts/suews/gen_capi_field_observer.py` parses canonical `TYPE`
  declarations in `src/suews/src/suews_type_*.f95` and `suews_ctrl_type.f95`
  and emits two deterministic ASCII files under `src/suews_bridge/test_observer/`
  (`capi_field_observer.f95` and `observer_manifest.rs`), outside `c_api/`
  [fact: git grep -n "test_observer" scripts/suews/gen_capi_field_observer.py].
  `.gitattributes` pins `src/suews_bridge/test_observer/** text eol=lf` so
  `--check` byte-for-byte freshness stays portable across platforms
  [fact: git grep -n "test_observer" .gitattributes].
- **Production isolation.** `capi-observer` implies `physics` and is mutually
  exclusive with `python-extension`: `build.rs` panics if both are enabled, so
  `libsuews_capi_observer.a` compiles and links only under the opt-in test
  feature and never enters shipped wheels or default CLI builds
  [fact: git grep -n "capi-observer\|suews_capi_observer" src/suews_bridge/Cargo.toml src/suews_bridge/build.rs].
- **Coverage and harness.** `src/suews_bridge/tests/capi_field_observer.rs`
  holds the `#[test]` suite across all 47 bridged types inventoried in
  `src/suews_bridge/bridge-manifest.json`: 30 direct `*_unpack` runtime
  observers, 4 driver-aggregate runtime observers (`SUEWS_CONFIG`,
  `SUEWS_TIMER`, `SUEWS_SITE`, `SUEWS_STATE`, exercising the 8 nested derived
  types live inside their parent unpackers), reverse-pack and forcing-row
  runtime observers, 1 manifest-coverage check over all
  47 bridged entries, 1 standalone nested-type codec round-trip check
  (`standalone_nested_type_codecs_roundtrip_distinct_sentinels`), 1 inventory
  and codec check for the 5 non-unpacked types
  (`non_unpacked_types_inventory_and_codecs`), and 1 truncated-buffer rejection
  check (`observer_rejects_truncated_flat_buffer`)
  [fact: python3 scripts/suews/gen_capi_field_observer.py --check && git grep -c '"status": "bridged"' src/suews_bridge/bridge-manifest.json && git grep -c "^#\[test" src/suews_bridge/tests/capi_field_observer.rs].
  In the original field-binding tests, 42 types have live receiver coverage (30 direct + 8 nested + 4 driver
  aggregates); the 5 non-unpacked types have inventory/schema/default/codec
  checks, not live receiver evidence
  [fact: git grep -n "fn manifest_covers_all_47_bridged_types_with_zero_gaps\|fn non_unpacked_types_inventory_and_codecs\|assert_eq!(non_unpacked_count, 5)" src/suews_bridge/tests/capi_field_observer.rs].
- **Reverse neighbourhood binding.** In the same integration target,
  `tests/reverse_nhood/mod.rs` checks the five named `NHOOD_STATE` members
  returned by the driver packer and the short-buffer error. The hand-written
  `test_observer/reverse_nhood_observer.f95` assigns values directly to the
  kernel type before calling the production packer
  [fact: git grep -n "reverse_nhood" src/suews_bridge/build.rs src/suews_bridge/tests].
- The same integration target runs two reverse-checkpoint cases in
  `tests/reverse_checkpoint_leaves/mod.rs`. Its feature-only Fortran witness
  seeds three STEBBS temperatures and all 27 stored heat/hydro allocatable
  arrays before the production aggregate packer. The tests check both the
  Rust return state and the state reopened from a disk checkpoint
  [fact: git grep -n "SUEWS_CAPI_OBSERVER\|mod reverse_checkpoint_leaves" src/suews_bridge].
- **Regenerate and verify** after changing any bridged Fortran `TYPE`,
  `c_api/*_unpack` routine, Rust struct/codec, or `bridge-manifest.json` entry
  (update `tests/capi_field_observer.rs` as well if struct fields or bridged
  types change) [procedure: Principle 1]:

```bash
# Regenerate test_observer/capi_field_observer.f95 and observer_manifest.rs
python3 scripts/suews/gen_capi_field_observer.py

# Verify committed artefacts match fresh generator output byte for byte
python3 scripts/suews/gen_capi_field_observer.py --check

# Build Fortran kernel libraries and run the complete observer target
make -C src/suews all
cargo test --manifest-path src/suews_bridge/Cargo.toml --features physics,capi-observer --test capi_field_observer --test snapshot_native
```

---

## Key Rules

- **Keep representation explicit** -- parameter/state fields use the codecs;
  forcing and direct-output buffers have separate checked layouts. Raw pointer
  access stays inside documented FFI boundaries (BR-04/05).
- **Feature-gate simulation** -- simulation driver bindings and calls require
  `#[cfg(feature = "physics")]`. Shared schema/default facades also serve the
  non-physics mode and need their own complete native source closure (BR-09).
- **build.rs source list** -- when adding Fortran C API files to `c_api/`, also add them to the compilation list in `build.rs`
- **Test via CLI** -- the `main.rs` CLI with JSON I/O is the fastest way to test bridge changes without Python: `cargo run -- qs --help`
- **Standard Rust style** -- `cargo fmt` applies. `cargo clippy` is gated in CI
  (see "Clippy CI gate"); the only project-specific overrides are the justified
  crate-level allowances at the top of `lib.rs`
