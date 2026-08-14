# SUEWS output-variable definitions

## Overview

`OUTPUT_REGISTRY` is the source of truth for Python-side output labels and
metadata. It supplies variable names, units, descriptions, temporal aggregation
rules, groups, and selection levels. The compiled model still produces the
numerical arrays; separate checks compare its per-group column counts with the
registry. A count match does not by itself prove the scientific meaning of each
compiled column.

Query the live registry size with `len(OUTPUT_REGISTRY.variables)` rather than
copying the count into code or documentation.

## Groups and contract scope

The output contract classifies groups according to their current intended
stability:

| Scope | Groups |
|-------|--------|
| coordinate | `datetime` |
| stable | `SUEWS`, `snow`, `ESTM`, `RSL`, `BL`, `DailyState` |
| provisional | `EHC`, `BEERS`, `SPARTACUS`, `STEBBS`, `NHood` |
| internal | `debug` |

These classifications are exposed by `OUTPUT_GROUP_SCOPES`. Output contract
`1.0.0` freezes the registry projection after the observable layouts were
validated. Output contract `1.1.0` adds the supported saved-output timestamp
references, with `follow` retaining the forcing clock by default. A group is
covered when that group is present; the contract does not promise that every
optional group is emitted by every run.

## Architecture

```text
Per-group OutputVariable definitions
    |
    v
OUTPUT_REGISTRY
    |-- pandas labels and aggregation rules
    |-- generated RST reference
    `-- get_output_contract_catalogue()
```

`get_output_contract_catalogue()` returns a cached, in-memory deterministic
projection of the registry. The catalogue is constructed only when requested.
Each variable is identified by `(group, name)`, and its zero-based ordinal is
derived from registry order. The catalogue does not create a second registry.

Common value metadata is recorded once for the catalogue. It describes the
missing-value encoding when a group is present in each representation; the
format-specific placement of coordinate fields is handled separately:

- values are numeric scalars;
- pandas output uses `NaN` for missing values;
- text output uses the `-999.0` sentinel;
- Parquet uses null values.

The representation metadata also declares `follow` as the default timestamp
reference and lists the supported `follow`, `utc`, `local_standard_time`, and
`daylight` policies.

`output_contract_json_schema()` returns the JSON Schema for this in-memory
catalogue. Each published version stores `catalogue.json`,
`catalogue.schema.json`, and `manifest.json` under `artefacts/<version>/`.
`OUTPUT_VERSIONS` records the SHA-256 digest of the exact canonical manifest
bytes, which in turn contain the catalogue and schema digests.

## Core models

- `OutputVariable`: metadata for one registry entry.
- `OutputVariableRegistry`: ordered collection and query methods.
- `AggregationMethod`: resampling behaviour (`T`, `A`, `S`, or `L`).
- `OutputGroup`: logical output group.
- `OutputLevel`: variable-selection level used by SUEWS text output.

Definitions are organised in one module per group, such as `suews_vars.py`,
`snow_vars.py`, and `dailystate_vars.py`. Add new variables to the appropriate
group module; do not edit the contract catalogue directly.

## Usage

```python
from supy.data_model.output import (
    OUTPUT_REGISTRY,
    OutputGroup,
    get_output_contract_catalogue,
)

suews_variables = OUTPUT_REGISTRY.by_group(OutputGroup.SUEWS)
qh = OUTPUT_REGISTRY.by_name("QH")
aggregation_rules = OUTPUT_REGISTRY.get_aggregation_rules()

first_contract_entry = get_output_contract_catalogue().variables[0]
print(first_contract_entry.group, first_contract_entry.name)
```

`by_name()` returns the first matching name across all groups. Use a group
filter when the same variable name occurs in more than one group.

The compatibility DataFrame is available through `OUTPUT_REGISTRY.to_dataframe()`.
It has a `(group, var)` index and `aggm`, `outlevel`, and `func` columns.

## Documentation and tests

Generate the RST reference from the repository root with:

```bash
python docs/generate_output_variable_rst.py
```

Check the stored release against the registry with:

```bash
python scripts/lint/check_output_contract_artefacts.py
```

The generator preserves registry order so that the reference agrees with
contract ordinals. Focused registry and contract tests live under
`test/data_model/`.

## Files

```text
src/supy/data_model/output/
|-- __init__.py          # Public exports
|-- variables.py         # Registry models and enums
|-- contract.py          # Output-owned contract projection
|-- registry.py          # Registry assembly
|-- version.py           # Output contract version history
|-- artefacts/           # Immutable versioned catalogues and manifests
|-- *_vars.py            # Per-group variable definitions
`-- README.md
```

Related integration points are `src/supy/_post.py`, `src/supy/_save.py`, and
`docs/generate_output_variable_rst.py`.
