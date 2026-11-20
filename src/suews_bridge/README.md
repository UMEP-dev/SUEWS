# SUEWS Rust bridge

This crate connects SuPy (Python) and the standalone `suews-engine` CLI to the
Fortran SUEWS (Surface Urban Energy and Water Balance Scheme) kernel. It
transports resolved configuration/site parameters, complete model state,
forcing and multi-timestep outputs through checked Rust wrappers and Fortran
`bind(c)` facades in `c_api/`. Python bindings use PyO3 (`abi3-py312`): the
root SuPy wheel build (`pyproject.toml`, `meson-python`) compiles and stages
the extension via `src/supy/build_rust_bridge.py` -> `cargo`, while
`src/suews_bridge/pyproject.toml` provides a separate standalone `maturin`
crate build.

Keep the transport responsibility narrow: names, representation, dimensions,
buffer safety, kernel invocation and diagnostics. Configuration defaults belong
to the Python data model (`src/supy/data_model/`); scientific decisions belong
to the Fortran kernel (`src/suews/src/`). The crate also contains standalone
engine forcing I/O and optional Apache Arrow IPC output, each with its own
contract.

## Start here

Read these authoritative references rather than duplicating their rules here:

- [Design, ownership and data flow](../../dev-ref/RUST_BRIDGE_DESIGN.md)
- [Shared bridge contract and audit rules (BR-01 to BR-10)](../../.claude/rules/rust/bridge-contract.md)
- [Baseline audit and known gaps](../../dev-ref/RUST_BRIDGE_AUDIT.md)
- [Adding a parameter end to end (8-step recipe and checklist)](../../dev-ref/RUST_BRIDGE_ADD_PARAMETER.md)
- [Rust bridge conventions](../../.claude/rules/rust/conventions.md)

The required contract is ahead of the legacy YAML loader in several places.
Missing inputs can still retain defaults at the audited revision; the baseline
audit links the corrective work. Do not copy that behaviour into new readers.

## End-to-end architecture and data flow

Data crosses four layers during a simulation:

```text
+-----------------------------------------------------------------------------+
| 1. Python layer (src/supy/)                                                 |
|    data_model/ (Pydantic validation, defaults, migrations)                  |
|      -> _run_rust.py (config_to_engine_yaml / _grid_config_jsons,           |
|                       forcing array, output buffers)                        |
|      -> suews_checkpoint.py (SUEWSCheckpoint v2 envelope per grid)          |
+-------------------------------------+---------------------------------------+
                                      | PyO3 (lib.rs) or resolved YAML (main.rs)
                                      v
+-----------------------------------------------------------------------------+
| 2. Rust bridge crate (src/suews_bridge/src/)                                |
|    yaml_config.rs + field_renames.rs -> RunConfig (config/timer/site/state) |
|    forcing_io.rs + forcing_time.rs   -> row-major forcing (engine CLI only) |
|    checkpoint.rs + suews_state.rs    -> validated restart state + timer     |
|    sim.rs                            -> flat buffers + TOCs + FFI checks    |
|    ffi.rs                            -> unsafe extern "C" declarations      |
|    output_io.rs (feature arrow-output) -> suews_output.arrow (engine CLI)   |
+-------------------------------------+---------------------------------------+
                                      | bind(c) static archive libsuews_bridge.a
                                      v
+-----------------------------------------------------------------------------+
| 3. Fortran C-API facades (src/suews_bridge/c_api/, 48 files)                |
|    driver.f95 (suews_cal_multitsteps_c) -> unpacks TOCs via c_api/*_unpack  |
|    common.f95 + sunposition.f95 + 45 per-type facades                       |
|      (30 with direct *_unpack + 15 schema/default-only)                     |
+-------------------------------------+---------------------------------------+
                                      | Fortran module USE + static archives
                                      v
+-----------------------------------------------------------------------------+
| 4. Fortran kernel (src/suews/src/)                                          |
|    suews_ctrl_driver.f95::SUEWS_cal_multitsteps_dts                         |
|    suews_type_*.f95, suews_ctrl_type.f95, suews_ctrl_const.f95, phys_*.f95  |
+-----------------------------------------------------------------------------+
```

### 1. Configuration and parameters

- **Python route:** for single-grid runs and `suews run`,
  `src/supy/_run_rust.py::config_to_engine_yaml` serialises a validated
  `SUEWSConfig` into resolved engine YAML; for multi-grid runs,
  `src/supy/_run_rust.py::_grid_config_jsons` dumps one site per JSON string
  (parsed by the same YAML loader in `src/yaml_config.rs`).
- **Rust parsing:** `src/yaml_config.rs` normalises field names using
  `src/field_renames.rs` and populates a `RunConfig` holding `SuewsConfig`,
  `SuewsTimer`, `SiteScalars` (24 site-level scalars), `SuewsSite` (18 member
  parameter structs) and cold-start `SuewsState` (13 member state structs).
- **ABI packing:** `src/sim.rs` encodes each member through its `StateCodec`
  flat `[f64]` buffer (`src/codec.rs`), concatenates site and state members
  into contiguous arrays with `(offset, length)` tables of contents (TOCs), and
  invokes `ffi::suews_cal_multitsteps_c`.
- **Fortran unpack:** `c_api/driver.f95` slices each TOC span, calls the
  corresponding `c_api/<type>.f95` `*_unpack` routine into fresh kernel derived
  types, and calls `SUEWS_cal_multitsteps_dts`.

For the step-by-step procedure and pitfalls when adding or modifying a
parameter, see [dev-ref/RUST_BRIDGE_ADD_PARAMETER.md](../../dev-ref/RUST_BRIDGE_ADD_PARAMETER.md).

### 2. Forcing

- **Python route (`SuPy`):** forcing file reading, `kPa -> hPa` pressure
  conversion and timestep resampling happen on the Python side (`supy._load` /
  `SUEWSForcing`) before `src/supy/_run_rust.py::_prepare_forcing_block` packs
  a C-contiguous `float64` array of shape `(len_sim, 32)` (`MET_FORCING_COLS`).
  Python's resampling may interpolate across interior gaps, but it does not
  provide general gap filling: the public forcing contract requires callers to
  resolve active missing values before simulation.
  `src/lib.rs::copy_forcing_buffer` copies that NumPy buffer once into an owned
  Rust `Vec<f64>` (ending the Python buffer borrow and avoiding per-element
  Python float boxing), and `run_suews_multi*` clones the `Vec<f64>` per grid
  to satisfy `sim`'s by-value parameter. Across the C ABI (`src/ffi.rs`),
  `forcing_flat` is read-only (`*const c_double`, `intent(in)` in
  `c_api/driver.f95`); `driver.f95` copies it into a per-call local
  `metforcing_block` before invoking the Fortran kernel.
- **Standalone engine route (`suews-engine run`):** `src/forcing_io.rs` and
  `src/forcing_time.rs` implement the engine's own meteorological text-file
  reader, `kPa -> hPa` pressure conversion (`* 10.0`), optional-column `-999.0`
  fill/sentinel handling and instantaneous/average/sum interpolation
  (`interpolate_forcing`). Because this reader is separate from SuPy's Python
  forcing pipeline, resampling and missing-value handling are scoped to each
  entry point rather than guaranteed identical across odd resolutions or
  missing-data edge cases (see the forcing boundary notes in
  `dev-ref/RUST_BRIDGE_AUDIT.md`).

### 3. Model state and restart checkpoints

- Cold-start state construction lives in `src/yaml_config.rs` and is kept
  separate from restart decoding (`BR-06`).
- After each run, `c_api/driver.f95` packs the updated Fortran `SUEWS_STATE`
  and `SUEWS_TIMER` back into flat buffers. `src/sim.rs` decodes them and
  `src/checkpoint.rs::suews_checkpoint_to_json` emits a schema-version-2 JSON
  envelope containing both `timer` (`dt_since_start`, `dt_since_start_prev`,
  `tstep`, `new_day`) and all 13 `state` members (`src/suews_state.rs`).
- On restart, `suews_checkpoint_from_json` requires schema version 2, a valid
  elapsed timer and the complete 13-member state set; it rejects legacy
  state-only envelopes, missing members and `+/-Inf` while preserving `NaN` as
  JSON `null` for inactive-state sentinels. On the Python side,
  `src/supy/suews_checkpoint.py::SUEWSCheckpoint` wraps these per-grid JSON
  envelopes with atomic file persistence.

### 4. Outputs and diagnostics

- **Direct-output Python route (default in SuPy):** `lib.rs::run_suews_into`
  and `lib.rs::run_suews_multi_into` validate caller-owned writable
  Fortran-ordered (`order="F"`) NumPy arrays (`lend_output_buffer`) and verify
  that no two lent column runs overlap (`check_disjoint`), then pass raw
  pointers with explicit leading dimension `ld` (`OutputBuffers` /
  `OutputMatrix` in `src/sim.rs`) while holding the GIL and buffer guards live.
  The kernel writes data columns (`OUTPUT_DATA_COLS`) and 5 datetime columns
  (`OUTPUT_DATETIME_COLS`) directly into the caller's tables without an extra
  copy.
- **Flat-buffer and Arrow routes:** `lib.rs::run_suews` returns a row-major
  flat `[f64]` buffer packed as Python `bytes` (`f64s_to_pybytes`) covering all
  11 concatenated output groups (`SUEWS`, `snow`, `BEERS`, `ESTM`, `EHC`,
  `DailyState`, `RSL`, `debug`, `SPARTACUS`, `STEBBS`, `NHood`). The standalone
  `suews-engine run` command writes the same flat buffer to
  `suews_output.arrow` via `src/output_io.rs` (when compiled with
  `--features physics,arrow-output`). Group widths are generated at build time
  into `$OUT_DIR/output_cols.rs` by `build.rs` from `ncolumnsDataOut*` in
  `src/suews/src/suews_ctrl_const.f95`.
- **Errors and warnings:** Fortran C-API status codes (`SUEWS_CAPI_*` in
  `c_api/common.f95` / `src/ffi.rs`) and kernel fatal errors map to
  `BridgeError` (`src/error.rs`) and then to Python exceptions in `src/lib.rs`.
  Non-fatal kernel warnings return via a bounded ring/buffer
  (`KERNEL_WARNING_MAX`) alongside total warning count.

## File-by-file role map

Verified against the repository layout (`61` Rust modules under `src/`, `48`
Fortran facades under `c_api/`, and `49` bridged derived types in
`bridge-manifest.json`).

### Crate root, build and Python glue

| Path | Role |
|---|---|
| `src/suews_bridge/Cargo.toml` | Crate manifest defining `suews_bridge` (`rlib`, `cdylib`), `suews-engine` binary, 5 optional features and `capi_field_observer` test target. |
| `src/suews_bridge/pyproject.toml` | Separate standalone `maturin` crate build metadata (`python-extension` + `physics`, installing top-level `suews_bridge`); the root SuPy wheel instead uses `meson-python` (`pyproject.toml` -> `src/supy/meson.build` -> `src/supy/build_rust_bridge.py` -> `cargo`, installing `supy.suews_bridge`). |
| `src/suews_bridge/build.rs` | Build script: embeds git version (`SUEWS_VERSION`/`SUEWS_COMMIT`), compiles `c_api/*.f95` into `libsuews_bridge.a` (and optional `libsuews_capi_observer.a`), links SUEWS static libraries + `libgfortran`, and generates `$OUT_DIR/output_cols.rs` from `suews_ctrl_const.f95`. Supports `SUEWS_BRIDGE_LINT_ONLY=1` and `SUEWS_BUILD_PROFILE=release\|checked`. |
| `src/suews_bridge/bridge-manifest.json` | Machine-checked inventory of all 49 `TYPE, PUBLIC` derived types in `src/suews/src/*.f95` and their bridge status (`bridged`, `planned`, `excluded`). |
| `src/supy/build_rust_bridge.py` | Meson build helper invoked by the root `meson-python` build (`pyproject.toml` -> `src/supy/meson.build`): runs `cargo build` for the PyO3 extension and `suews-engine` binary, copies them into package staging paths, and writes a Ninja depfile tracking Rust, C-API and driver sources. |
| `src/supy/cmd/rust_bridge.py` | Thin Python wrapper locating and invoking the bundled `supy/bin/suews-engine` binary. |
| `src/supy/_run_rust.py` | Primary Python-to-Rust integration layer: validates output column layout parity against Fortran, serialises `SUEWSConfig` (single-grid YAML via `config_to_engine_yaml`, one-site-per-grid JSON via `_grid_config_jsons`), prepares `(len_sim, 32)` forcing blocks, allocates Fortran-ordered output stores, dispatches single/multi-grid/chunked/streamed runs, and wraps results into pandas DataFrames. |
| `src/supy/suews_checkpoint.py` | Python `SUEWSCheckpoint` dataclass wrapping per-grid version-2 checkpoint JSON envelopes with schema validation and atomic file writes. |
| `src/suews_bridge/examples/ohm_state_python_demo.py` | Minimal Python demonstration of constructing and inspecting `OhmState` via PyO3. |

### Rust entry points, orchestration and ABI (`src/suews_bridge/src/`)

| File | Role |
|---|---|
| `src/lib.rs` | Crate root, module re-exports, justified crate-level clippy allowances, and the `#[cfg(feature = "python")]` PyO3 module (`suews_bridge`: 40+ `#[pyclass]` wrappers, `run_suews*` / `run_suews*_into` direct-output buffer lending, and schema introspection functions). |
| `src/main.rs` | Standalone `suews-engine` CLI binary (`run` for resolved YAML batch simulation with Arrow output; `schema` subcommands for JSON schema/sample inspection). |
| `src/sim.rs` | Multi-timestep simulation orchestration (`physics`-gated): encodes `SiteScalars` (24), `SuewsSite` (18 members + TOC) and `SuewsState` (13 members + TOC), validates dimensions/overflow/timestep compatibility, invokes `ffi::suews_cal_multitsteps_c`, decodes post-run state/timer/warnings, and owns `OutputBuffers` / `OutputMatrix` and `OUTPUT_GROUP_LAYOUT`. |
| `src/ffi.rs` | Raw `unsafe extern "C"` bindings to `libsuews_bridge.a` and `SUEWS_CAPI_*` status code constants. No safety invariants are enforced here; callers own pointer/layout checks. |
| `src/error.rs` | `BridgeError` enum mapping C-API status codes (`from_code`), checkpoint envelope/value errors, and kernel `SimulationError` diagnostics. |

### Configuration, codecs, composite containers, forcing and outputs

| File | Role |
|---|---|
| `src/yaml_config.rs` | Parses resolved engine YAML into `RunConfig` (`physics`-gated) using strict `require_*` helpers and remaining tracked legacy optional readers (ratcheted by `check_rust_optional_reads.py`). |
| `src/field_renames.rs` | YAML key-name preprocessor (`FIELD_RENAMES` and `FIELD_COMPAT_ALIASES`) bridging Python `snake_case` and legacy Fortran/YAML spellings; audited against `src/supy/data_model/core/field_renames.py`. |
| `src/config.rs` | `SuewsConfig` codec (physics method switches and control flags matching `SUEWS_CONFIG`). |
| `src/timer.rs` | `SuewsTimer` codec (calendar fields, timestep and elapsed counters matching `SUEWS_TIMER`). |
| `src/codec.rs` | Shared `StateCodec` / `CompositeCodec` traits, `TypeSchema` / `ValuesPayload` / `ValuesPayloadWithDims` types, flat-length and dimension checks, and the `impl_state_module_fns!` macro generating per-type FFI helpers. |
| `src/suews_site.rs` | Composite `SuewsSite` container aggregating all 18 site parameter members (`SUEWS_SITE_MEMBER_ORDER`) with nested and dotted-map codecs. |
| `src/suews_state.rs` | Composite `SuewsState` container aggregating all 13 model state members (`SUEWS_STATE_MEMBER_ORDER`), enforcing full-member restart validation and `NaN` <-> JSON `null` checkpoint encoding. |
| `src/suews_state_block.rs` | Dimension-bearing vector of `SuewsState` snapshots (`SUEWS_STATE_BLOCK`). |
| `src/suews_debug.rs` | 15-stage intra-timestep `SuewsState` snapshot container (`SUEWS_DEBUG`). |
| `src/checkpoint.rs` | Version-2 restart envelope (`CheckpointTimer` + `SuewsCheckpointData`) serialising/deserialising JSON with exact `float_roundtrip` parsing and strict version/timer checks. |
| `src/forcing.rs` | Single-timestep `SuewsForcing` flat codec (`SUEWS_FORCING`). |
| `src/forcing_io.rs` | Standalone engine (`suews-engine run`) meteorological forcing file reader, `kPa -> hPa` pressure conversion (`* 10.0`), optional-column `-999.0` sentinel handling and timestep interpolation (`physics`-gated; separate from SuPy's Python forcing preparation). |
| `src/forcing_time.rs` | Gregorian leap-year and signed-second calendar conversions (`to_seconds` / `from_seconds`) and `TimeColumns` indices used by `forcing_io.rs`. |
| `src/output_line.rs` | Single-timestep `OutputLine` flat codec across datetime and all 11 output groups using build-time `$OUT_DIR/output_cols.rs` widths. |
| `src/output_block.rs` | Dimension-bearing `OutputBlock` / `OutputBlockMatrix` codec (11 per-group 2D matrices); distinct from the direct-output `OutputMatrix` view in `sim.rs`. |
| `src/output_io.rs` | Optional Apache Arrow IPC file writer (`write_output_arrow` -> `suews_output.arrow`, gated by `physics` + `arrow-output`), encoding `SUEWS_OUTPUT_NAMES`, indexed non-SUEWS names and `group_layout` schema metadata, with a unit test pinning parity against `src/supy/data_model/output/`. |

### Per-type parameter and state codecs (40 modules under `src/suews_bridge/src/`)

Each module follows the `StateCodec` pattern (`src/codec.rs`) and pairs 1:1
with its Fortran facade in `c_api/`.

| Family | Modules |
|---|---|
| Land-cover parents (7) | `lc_paved_prm.rs`, `lc_bldg_prm.rs`, `lc_evetr_prm.rs` (annotated nested reference), `lc_dectr_prm.rs`, `lc_grass_prm.rs`, `lc_bsoil_prm.rs`, `lc_water_prm.rs` |
| Nested land-cover blocks (6) | `ohm_prm.rs`, `ohm_coef_lc.rs`, `soil.rs`, `bioco2.rs`, `lai.rs`, `water_dist.rs` |
| Subsystem parameters (13) | `anthro_emis_prm.rs`, `anthro_heat_prm.rs`, `building_archetype_prm.rs`, `conductance.rs`, `ehc_prm.rs`, `irrig_daywater.rs`, `irrigation_prm.rs`, `lumps.rs`, `snow_prm.rs`, `spartacus_layer_prm.rs`, `spartacus_prm.rs`, `stebbs_prm.rs` (annotated flat reference), `surf_store.rs` |
| State and diagnostics (14) | `anthroemis.rs`, `atm.rs`, `core.rs` (`OhmState` v2 + `sunposition_calc` + legacy scalar OHM helpers -- documented BR-01 deviation mirrored in `c_api/ohm.f95`), `error_entry.rs`, `error_state.rs`, `flag.rs`, `heat_state.rs`, `hydro_state.rs`, `nhood.rs`, `phenology.rs`, `roughness.rs`, `snow.rs`, `solar.rs`, `stebbs_state.rs` |

### Fortran C-API facades (`src/suews_bridge/c_api/`, 48 files)

| File(s) | Role |
|---|---|
| `c_api/common.f95` | Shared `SUEWS_CAPI_*` error codes, error text lookup and null-terminated Fortran-to-C string copy (`copy_to_c_buffer`). |
| `c_api/driver.f95` | Batch simulation C entry point `suews_cal_multitsteps_c` (`physics`-gated): validates lengths/TOCs, copies `intent(in)` `forcing_flat` into a local `metforcing_block`, unpacks timer/config/site scalars/18 site members/13 state members, calls `SUEWS_cal_multitsteps_dts`, writes outputs/warnings and packs post-run state/timer. |
| `c_api/sunposition.f95` | Thin `bind(c)` wrapper around `module_phys_narp::NARP_cal_SunPosition` (`physics`-gated). |
| 45 per-type `c_api/<type>.f95` | Per-type Fortran C-API facades (`3` non-per-type files above + `45` per-type = `48` total): `30` provide direct `*_unpack` routines (including `c_api/stebbs_prm.f95` as the annotated reference facade and `c_api/ohm.f95`, which also carries the legacy scalar OHM helper routines recorded as a BR-01 duplication in `dev-ref/RUST_BRIDGE_AUDIT.md`), and `15` provide schema/default routines only (`8` nested parameter types unpacked inline by parent facades, `config.f95` and `timer.f95` unpacked inside `driver.f95`, plus `forcing.f95`, `output_line.f95`, `output_block.f95`, `error_entry.f95` and `error_state.f95`). |

### Verification harnesses, observers and repository scripts

| Path | Role |
|---|---|
| `src/suews_bridge/tests/yaml_jacobian.rs` | Integration suite probing `EngineYAML -> RunConfig` discrete Jacobians, dead-key ratchets, slot ownership, named-receiver identity, delta fidelity and NaN-poison handling across resolved anchor configs (`--features physics`). |
| `src/suews_bridge/tests/capi_field_observer.rs` | Opt-in integration suite (`--features physics,capi-observer`) proving every Rust encoder + Fortran `*_unpack` routine delivers globally distinct sentinels to the exact named Fortran kernel component across all 49 manifest types. |
| `src/suews_bridge/test_observer/` | Generated test-only observer artefacts (`capi_field_observer.f95` and `observer_manifest.rs`), excluded from production wheels (`build.rs` panics if `capi-observer` is combined with `python-extension`). |
| `scripts/suews/gen_capi_field_observer.py` | Deterministic stdlib-only generator and `--check` CI validator for `test_observer/`. |
| `scripts/suews/check_bridge_manifest.py` | CI check ensuring every `TYPE, PUBLIC` in `src/suews/src/*.f95` is tracked in `bridge-manifest.json`. |
| `scripts/suews/gen_bridge_fixtures.py` | Regenerates resolved engine YAML test fixtures in `test/fixtures/bridge/`. |
| `scripts/lint/check_rust_optional_reads.py` | Downward-only CI ratchet (`rust_optional_reads_baseline.json`) forbidding new legacy optional reader calls in `src/yaml_config.rs`. |
| `scripts/lint/check_rust_yaml_aliases.py` | CI parity check between `src/field_renames.rs` and `src/supy/data_model/core/field_renames.py`. |

## `suews-engine run` takes resolved YAML only

`suews run` is the user command: it validates a YAML config through the SuPy
data model, fills every default there, and hands the engine the resolved
config (`supy._run_rust.config_to_engine_yaml`). `suews-engine run` is a
developer and test binary that expects that resolved form; it does not
resolve a hand-written config itself. This follows the bridge's pass-through
caller contract (#1991): defaults belong in the data model (#2004), and
callers must supply complete, resolved input. Enforcement at the engine
boundary is still being completed in #1991/#2002; some omitted values can
still retain legacy bridge fallbacks. Acceptance by the engine does not
establish that incomplete input is valid.

For source configurations using the former standalone spelling
`sites[*].properties.lon`, use `suews run` or load with
`SUEWSConfig.from_yaml` / `SUEWSConfig.from_dict` before calling
`supy._run_rust.config_to_engine_yaml`. The data model folds that spelling
to `lng` on input; providing both `lon` and `lng` is an error, even if
their values agree. Other namespaces and reference metadata are untouched.
This adds alias tolerance to Python source loading: the earlier native
reader tried `lng` first and then `lon`, whereas the Python site model
declared only `lng` and forbade extra fields. It does not preserve native
fallback or duplicate-key precedence. The folded value goes through the
existing `lng` type and range validation.

Annotated validation YAML and resolved engine YAML are different artefacts.
Normal Phase C copies the source with a standard header, so its updated
YAML retains the input longitude spelling and value. ABC folds an input
alias in Phase A and records the site-specific rename in its report.
Reload accepted annotated YAML with `SUEWSConfig.from_yaml` and call
`config_to_engine_yaml` to produce engine input.
Resolved engine input uses only `lng` and must supply finite `lat`, `lng`,
`alt`, `timezone`, `surfacearea` and `z` values. Zero coordinates, altitude
and UTC offset remain valid. Missing source longitude retains the data
model's default of -0.13 degrees; Rust supplies no longitude default.

To build a runnable engine (needs the SUEWS physics library, e.g. `make dev`):

```bash
make bridge   # cargo build --release --features physics,arrow-output
src/suews_bridge/target/release/suews-engine run resolved_config.yml
```

## Build, features and verification

### Cargo features (no default features)

| Feature | Effect |
|---|---|
| `physics` | Compiles `c_api/driver.f95` and `c_api/sunposition.f95`, links SUEWS static libraries (`libsuewsdriver.a`, `libsuewsphys.a`, `libspartacus.a`, `libsuewsutil.a`), and enables `sim`, `yaml_config` and `forcing_io`. |
| `python` | Enables PyO3 bindings targeting the Python 3.12+ stable ABI (`abi3-py312`). |
| `python-extension` | Enables `python` plus PyO3's `extension-module` linking for wheel builds. |
| `arrow-output` | Enables Apache Arrow IPC output in `src/output_io.rs` and the `suews-engine run` subcommand (used together with `physics`). |
| `capi-observer` | Test-only feature (implies `physics`, mutually exclusive with `python-extension`) compiling `test_observer/capi_field_observer.f95` for the `capi_field_observer` integration test. |

The non-physics native build has incomplete Fortran type-module prerequisites
(#1990). Its facades still need those modules even without the simulation
kernel; validate that mode separately from the physics build.

### Environment switches

- `SUEWS_BUILD_PROFILE=release` (default) or `checked` (adds `-fcheck=all` to
  the Fortran C-API compilation; matches `src/suews/Makefile.gfortran` and
  `src/supy/run_make.py`).
- `SUEWS_BRIDGE_LINT_ONLY=1`: skips native Fortran compilation and linking in
  `build.rs` while still generating `$OUT_DIR/output_cols.rs`. Useful for
  `cargo clippy` and `cargo doc`, but provides no evidence that a native build
  links or runs (`BR-09`).

### Common commands (from the repository root)

```bash
# Build native Fortran libraries and standalone simulation-capable engine
make -C src/suews all
make bridge

# Native-linked Rust unit, CLI and Jacobian tests
cargo test --manifest-path src/suews_bridge/Cargo.toml --features physics
cargo test --manifest-path src/suews_bridge/Cargo.toml --features physics,arrow-output

# Fortran C-API field-binding observer tests (all 49 manifest types)
cargo test --manifest-path src/suews_bridge/Cargo.toml --features physics,capi-observer --test capi_field_observer

# Lint-only Clippy and rustdoc checks (no Fortran libraries required)
SUEWS_BRIDGE_LINT_ONLY=1 cargo clippy --manifest-path src/suews_bridge/Cargo.toml --all-features --all-targets -- -D warnings
SUEWS_BRIDGE_LINT_ONLY=1 RUSTDOCFLAGS="-D warnings" cargo doc --manifest-path src/suews_bridge/Cargo.toml --no-deps --features physics,arrow-output

# Bridge static contract checks
python scripts/lint/check_rust_optional_reads.py
python scripts/lint/check_rust_yaml_aliases.py
python scripts/suews/check_bridge_manifest.py
python scripts/suews/gen_capi_field_observer.py --check

# Full SuPy development install via root meson-python (`build_rust_bridge.py` -> cargo)
# Installs the extension as `supy.suews_bridge` (and CLI at `supy/bin/suews-engine`)
make dev
python -c "import supy.suews_bridge as sb; print(sb.output_buffer_ncolumns())"
make test-smoke

# Or separate standalone crate-only Python extension via maturin (`src/suews_bridge/pyproject.toml`)
# Installs the extension as top-level `suews_bridge` (`supy._run_rust` tries `supy.suews_bridge` first, then `suews_bridge`)
maturin develop --manifest-path src/suews_bridge/Cargo.toml --features python-extension,physics
python -c "import suews_bridge as sb; print(sb.output_buffer_ncolumns())"
```

For CLI arguments, run `src/suews_bridge/target/release/suews-engine --help`.
