---
paths:
  - src/suews_bridge/**/*.rs
  - src/suews_bridge/Cargo.toml
  - src/suews_bridge/build.rs
---

# Rust Bridge Conventions

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
# Rust CLI binary (standalone, no Python)
make bridge
# Output: src/suews_bridge/target/release/suews

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
- `python-extension` -- enables PyO3 with stable ABI for Python 3.9+ wheels
- `python` -- enables PyO3 without stable ABI (for development)
- `arrow-output` -- enables Apache Arrow IPC output format

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
6. Update `build.rs` Fortran source list if new `.f95` files added to `c_api/`

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
- Schema versioning (`schema_version`) ensures forward compatibility
- `ValuesPayload` wraps `(schema_version, Vec<f64>)` for JSON serialisation

---

## Error Handling

- Fortran C API returns integer error codes (`SUEWS_CAPI_OK`, `SUEWS_CAPI_BAD_BUFFER`, etc.)
- Rust converts these to `BridgeError` variants via `Result<T, BridgeError>`
- Python receives these as native Python exceptions via PyO3

---

## Key Rules

- **Never bypass the codec** -- all Fortran data access goes through `StateCodec` traits, not raw pointer arithmetic
- **Feature-gate physics** -- all FFI calls must be behind `#[cfg(feature = "physics")]`
- **build.rs source list** -- when adding Fortran C API files to `c_api/`, also add them to the compilation list in `build.rs`
- **Test via CLI** -- the `main.rs` CLI with JSON I/O is the fastest way to test bridge changes without Python: `cargo run -- qs --help`
- **Standard Rust style** -- `cargo fmt` and `cargo clippy` apply; no project-specific overrides
