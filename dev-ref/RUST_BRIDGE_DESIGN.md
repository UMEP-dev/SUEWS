# Rust bridge: responsibilities and data flow

The Rust bridge connects Python's resolved configuration and arrays to the
Fortran model. Its core responsibility is faithful transport with explicit
checks and errors. It should remain small in purpose even when the model has
many parameter and state types.

The normative requirements are the
[bridge contract and audit rules](../.claude/rules/rust/bridge-contract.md).
This document explains their design. The [baseline audit](RUST_BRIDGE_AUDIT.md)
separates existing checks from known deviations and unverified coverage.

## Ownership

| Layer | Owns | Must not acquire through bridge maintenance |
|---|---|---|
| Python data model and configuration tools | Public configuration schema, documented defaults, conditional requirements, resolution and migration | A second set of defaults hidden in Rust or C-API shadow types |
| Bridge transport core | Names and representation, dimensions, ABI packing, checked buffer access, kernel invocation, state/output transfer and diagnostics | Scientific equations, new physical assumptions, automatic parameter repair or heuristic fallback |
| Fortran model | Scientific calculations, option semantics, model evolution and diagnostic meaning | Dependence on an undocumented bridge-specific substitute for an input |
| Forcing and output I/O | File formats, established column/unit/time contracts and format conversions | New scientific policy disguised as parsing or serialisation |
| Maintainers | Requirements, scope, scientific decisions and acceptance criteria | A requirement to manually maintain every generated implementation detail |

The crate directory is broader than the transport core. It already contains a
standalone CLI, forcing interpolation and an optional Arrow writer. Those are
adjacent responsibilities, not permission to add arbitrary preprocessing to
the codecs or YAML loader. A new capability must name its owner and establish
why it belongs there, following the repository's
[placement rule](../.claude/rules/code-design.md).

There is also legacy duplication: `c_api/ohm.f95` implements scalar OHM
formulas rather than calling the owning `module_phys_ohm` procedures. Rust's
`core` helpers and Python/CLI scalar APIs expose them. This is a recorded
BR-01 deviation, not a pattern for new facades; resolving it needs a focused
follow-up that preserves or explicitly changes the existing API contract.

## Parameter and configuration flow

```text
user configuration
    -> Python data model: validate, resolve defaults and conditional requirements
    -> serialised engine configuration
    -> yaml_config: names and representation
    -> Rust parameter/site structs
    -> codecs: ordered flat buffers + member table of contents
    -> Fortran C-API unpack
    -> kernel derived types
```

This is the required ownership model. At the audited revision it is not yet
fully enforced: `_run_rust.py` serialises with `model_dump(exclude_none=True)`,
while `yaml_config.rs` begins with Fortran shadow defaults and applies the keys
it recognises. The standalone engine can also read YAML directly. Missing keys
can therefore retain numerical values that were never explicitly resolved.
The #1991 work tracks removal of that behaviour; #2002 and #2004 cover strict
reads and the CLI route. No new parameter should extend the legacy fallback.

Both Python and standalone engine callers must ultimately supply the same
resolved contract. The engine must not need Python at runtime, but a user CLI
may use the Python data model to produce that input before invoking it. A file
path may legitimately be absent when the caller supplies an in-memory forcing
buffer; that is a transport choice, not a missing meteorological value.

For each transported model field, establish:

- canonical path/name and owning schema;
- units, dimensions, element order and destination kernel member;
- whether it is always required or required under a named option;
- explicit representation when unused, with evidence that it is not read;
- malformed-input diagnostic and a non-default wiring check.

Fortran shadow initialisers and Rust `Default` values currently support legacy
construction and codec tests. They are not the source of application defaults.
Zero-filled scratch storage is also different: it is acceptable only if the
producer writes every consumed value before the consumer reads it.

See [adding a parameter](RUST_BRIDGE_ADD_PARAMETER.md) for the hand-maintained
mapping sites. `bridge-manifest.json` inventories types during the bridge
transition; it does not prove field-level coverage. `build.rs` generates output
widths, not parameter pack/unpack mappings.

## Direction: generated interfaces behind the project build

Contributors should be able to change model behaviour in Fortran or the public
configuration in Python without routinely editing Rust transport code.
The proposed next step is a build-time generator and contract check, reached
through the normal project build and test commands.
This is a design direction, not a generator already provided by this change.

Keep the authoritative declarations in their owning layers.
Fortran owns kernel types and procedures; Python owns public configuration,
defaults and conditional requirements.
A small binding description can connect those declarations by recording the
source path, destination member, direction, dimensions and active-option rule.
Reference existing unit and field metadata rather than copying their policy
into another registry.
The generator must reject conflicting declarations or an unsupported construct
with a useful diagnostic instead of inferring a mapping or supplying a value.

For a supported type family, generate the mechanical transport together:
Fortran pack/unpack code, Rust representations and codecs, field names, lengths
and nested offsets.
Keep kernel calculations, configuration resolution and defaults outside the
generated transport.
The existing public-type manifest is useful coverage input, but marking a type
as bridged cannot substitute for checking every transported field.

The build should check that generated files match their inputs before compiling
the Fortran and Rust layers with the agreed features and dependency order.
CI should repeat generation deterministically and fail on unexplained drift.
Independent field-delivery tests must still use distinct values at the public
input and verify the actual kernel destination; tests derived solely from the
same binding description could repeat its mistakes.
Layout or checkpoint changes also require the existing version and restart
compatibility checks.

Introduce this one type family at a time, initially preserving its existing
layout and behaviour and comparing generated output with the current mapping.
A calculation-only Fortran change or a Python-only workflow change needs no
new binding when its interface is unchanged.
A new cross-boundary input needs an explicit declaration of its meaning and
destination; a compiler cannot decide its scientific default or units.
This keeps automation general while the bridge's responsibility remains narrow.

## State, forcing and outputs

```text
resolved cold-start state OR validated checkpoint (state + elapsed timer)
    -> state codecs -> Fortran driver -> updated state + timer
    -> checked decode -> Python result / checkpoint envelope

Python forcing arrays OR engine forcing-file reader
    -> ordered forcing buffer -> Fortran driver
    -> output and datetime buffers -> Python arrays / engine file writer
```

Cold-start state may require resolution in the owning model layer. Restart is
different: `checkpoint.rs` requires a versioned envelope with both state and
elapsed timer. `suews_state.rs` validates the nested member set and payloads.
The simulation entry point also checks checkpoint timestep compatibility.
A restart must not silently become a cold start when a member is absent.
Some inactive fields retain legacy sentinels; a blanket finite-value rule would
change that contract and needs separate justification.

The Python forcing buffer is copied from C-contiguous `float64` input. The
traditional returned output vector is row-major. The direct `run_suews_into`
route instead lends writable output and datetime matrices with contiguous
rows within each column and an explicit leading dimension. These layouts are
different contracts; do not reuse a shape check as proof of layout.

At the direct-output boundary, `lib.rs` checks buffer type, shape, strides and
overlap. The buffer guards must live through the native call; the caller must
not mutate the buffers concurrently. `sim.rs` checks simulation dimensions,
buffer reach and integer conversions before passing pointers to `ffi.rs`.
An `unsafe` declaration alone provides none of those guarantees. Changes to
parallelism or GIL handling must re-establish them explicitly.

`forcing_io.rs` implements the engine's file-reader/interpolation contract,
including the existing pressure conversion and distinct instantaneous,
average and cumulative treatments. That policy belongs to forcing I/O and
must stay consistent with the Python forcing contract. `output_io.rs` writes
Arrow when enabled; generated group widths and manually maintained names are
different sources of evidence. A correct width does not prove a correct name
or unit.

## Module map and audit surface

| Family | Main files under `src/suews_bridge/` | Boundary to inspect |
|---|---|---|
| Public entry points | `src/lib.rs`, `src/main.rs` | Python buffers, engine arguments, returned exceptions/results |
| Legacy OHM scalar helpers | `src/core.rs`, `c_api/ohm.f95` | Copied model formulas and the exposed scalar API; a BR-01 deviation |
| Configuration | `src/yaml_config.rs`, `src/field_renames.rs`, `src/config.rs` | Resolved input, aliases, required paths and option conditions |
| Parameter transport | `src/*_prm.rs`, `src/bioco2.rs`, `src/conductance.rs`, `src/soil.rs`, `src/lai.rs`, `src/water_dist.rs`, corresponding `c_api/` files | Field order, nested offsets and unpack destination |
| Site and orchestration | `src/suews_site.rs`, `src/sim.rs`, `src/timer.rs`, `c_api/driver.f95` | Member table of contents, dimensions, call lifetime and diagnostics |
| State and restart | `src/*_state.rs`, `src/suews_state_block.rs`, `src/checkpoint.rs`, `src/core.rs`, `src/atm.rs`, `src/solar.rs`, `src/phenology.rs`, `src/roughness.rs`, `src/snow.rs`, `src/anthroemis.rs`, `src/flag.rs` | Full member set, timer continuity and restart compatibility |
| Forcing | `src/forcing.rs`, `src/forcing_io.rs`, `src/forcing_time.rs` | Columns, time, units, optional values and interpolation |
| Outputs | `src/output_line.rs`, `src/output_block.rs`, `src/output_io.rs` | Widths, names, group order, dimensions and memory layout |
| Shared representation | `src/codec.rs`, `src/ffi.rs`, `src/error.rs`, `c_api/` | Length/version checks, unsafe preconditions and error transfer |
| Build and packaging | `build.rs`, `Cargo.toml`, `pyproject.toml` | Feature-specific sources, native linking and extension ABI |

This is a family map, not a per-field proof. For a change, follow every affected
caller and nested parent rather than checking only the named module. The audit
record must state which paths were inspected and which remain unverified.

## How changes proceed

Proposed additions should be follow-up issues linked to #1989 and the relevant
#1991 item. Each proposal states the required behaviour, owning layer, input
and output contract, invariants and acceptance checks. Implementations then
use BR-01 through BR-10 consistently, regardless of which agent writes them.

Maintainers decide what the model should mean and whether a scope or scientific
exception is acceptable. Agents can implement, comment and verify the agreed
contract. This does not add routine human line-by-line Rust auditing as a gate;
it does retain the existing scientific-evidence and schema-migration rules.

Use a revision-specific audit record, not a numerical quality score. Report
`checked`, `violation`, `not verified` or `not applicable` for each affected
rule, backed by symbols and concrete checks. Existing gaps stay visible until
the corresponding implementation and acceptance evidence land. A successful
round trip, lint run or smoke suite is useful evidence but cannot certify all
field mappings or scientific behaviour.
