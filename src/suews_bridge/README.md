# SUEWS Rust bridge

This crate connects Python and the standalone `suews-engine` CLI to the Fortran
model. It transports parameter/site types, complete model state, forcing and
multi-timestep outputs through checked Rust wrappers and Fortran `bind(c)`
facades in `c_api/`. Python bindings use PyO3 and maturin.

Keep the transport responsibility narrow: names, representation, dimensions,
buffer safety, invocation and diagnostics. Configuration defaults belong to the
Python data model; scientific decisions belong to the model. The crate also
contains engine forcing I/O and optional Arrow output, with their own contracts.

## Start here

- [Design and data flow](../../dev-ref/RUST_BRIDGE_DESIGN.md)
- [Shared contract and audit rules](../../.claude/rules/rust/bridge-contract.md)
- [Baseline audit and known gaps](../../dev-ref/RUST_BRIDGE_AUDIT.md)
- [Adding a parameter end to end](../../dev-ref/RUST_BRIDGE_ADD_PARAMETER.md)

The required contract is ahead of the legacy YAML loader in several places.
Missing inputs can still retain defaults at the audited revision; the baseline
audit links the corrective work. Do not copy that behaviour into new readers.
The older `docs-bridge-rationale.md` and phase-two roadmap are historical design
records, not a description of the current crate scope.

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

To build a runnable engine (needs the SUEWS physics library, e.g. `make dev`):

```bash
make bridge   # cargo build --release --features physics,arrow-output
src/suews_bridge/target/release/suews-engine run resolved_config.yml
```

## Build and check

Run builds and tests on the designated compute host. From the repository root:

```bash
# Native libraries and a standalone simulation-capable engine
make -C src/suews all
make bridge

# Native-linked Rust tests
cargo test --manifest-path src/suews_bridge/Cargo.toml --features physics

# Python extension in an activated Python 3.12+ environment
maturin develop --manifest-path src/suews_bridge/Cargo.toml --features python-extension,physics

# Repository regression smoke suite (requires the supy development installation)
make test-smoke
```

Alternatively `make dev` at the repository root builds the complete development
installation. For CLI arguments use
`src/suews_bridge/target/release/suews-engine --help`.

`physics` enables full simulation; `python` enables PyO3 with the configured
Python 3.12+ stable ABI; `python-extension` adds extension-module linking;
`arrow-output` enables the engine's Arrow writer. There are no default Cargo
features. The non-physics native build has incomplete Fortran type-module
prerequisites (#1990). Its facades still need those modules even without the
simulation kernel; validate that mode separately from the physics build.

Clippy's `SUEWS_BRIDGE_LINT_ONLY=1` mode skips native compilation/linking and
still generates output widths. It is useful for type/lint checks and rustdoc,
but provides no evidence that a native build links or runs.
