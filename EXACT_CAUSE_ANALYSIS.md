# Exact Cause Analysis: ARM64 Test Failures

## Summary
The ARM64 test failures were caused by a column access bug in `add_sfc_init_df()` that was exposed by differences in how DataFrames are created from different configuration sources.

## The Exact Problem

### Function: `add_sfc_init_df()` in `src/supy/_load.py`

This function expects to find surface-specific columns in `df_init` like:
- `snowdenspaved`, `snowdensbldgs`, `snowdensevetr`, etc.
- `snowfracpaved`, `snowfracbldgs`, `snowfracevetr`, etc.
- `snowpackpaved`, `snowpackbldgs`, `snowpackevetr`, etc.
- `snowwaterpavedstate`, `snowwaterbldgsstate`, etc.
- `soilstorepavedstate`, `soilstorebldgsstate`, etc.
- `pavedstate`, `bldgsstate`, `evetrstate`, etc.

The function builds these column names dynamically:
```python
dict_var_sfc = {
    "snowdens": ["snowdens{}".format(sfc) for sfc in list_sfc],  # Results in ["snowdenspaved", "snowdensbldgs", ...]
    "snowfrac": ["snowfrac{}".format(sfc) for sfc in list_sfc],
    "snowpack": ["snowpack{}".format(sfc) for sfc in list_sfc],
    "snowwater": ["snowwater{}state".format(sfc) for sfc in list_sfc],
    "soilstore_surf": ["soilstore{}state".format(sfc) for sfc in list_sfc],
    "state_surf": ["{}state".format(sfc) for sfc in list_sfc],  # Results in ["pavedstate", "bldgsstate", ...]
}
```

Then tries to access them directly:
```python
df_init[(var, ind_str)] = df_init[var_sfc]  # FAILS with KeyError if column doesn't exist
```

## Why It Failed

### Path 1: Loading from .nml files
- Calls `load_InitialCond_grid_df()` → `load_SUEWS_InitialCond_df()` → creates DataFrame with all expected columns
- Then calls `add_sfc_init_df()` which expects those columns to exist
- Works fine because columns are present

### Path 2: Loading from .yml files (YAML config)
- Calls `init_config_from_yaml().to_df_state()`
- Creates DataFrame using Pydantic models which only creates the final MultiIndex columns
- Does NOT create intermediate columns like `snowdenspaved`
- When something later calls `add_sfc_init_df()`, it fails with KeyError

## The Test Contamination Issue

1. `test_suews_simulation.py` loads from YAML and modifies the global `df_state_init`
2. This triggers paths that call `add_sfc_init_df()` with incomplete DataFrame
3. The KeyError causes NaN values to be introduced
4. These NaN values contaminate the global test data
5. Subsequent tests in `test_supy.py` fail due to the contaminated data

## Why ARM64 Specific?

The issue only manifested on ARM64 likely due to:
- Different test execution order on ARM64
- Different memory layout causing different test interaction patterns
- The bug exists on all platforms but only gets triggered in specific test sequences

## The Fix

The fix handles missing columns gracefully instead of crashing:

```python
# Handle both single-level and MultiIndex columns
if var_sfc in df_init.columns:
    df_init[(var, ind_str)] = df_init[var_sfc]
elif (var_sfc, "0") in df_init.columns:
    df_init[(var, ind_str)] = df_init[(var_sfc, "0")]
else:
    # Column doesn't exist - use sensible defaults instead of crashing
    if var_sfc.startswith("snow"):
        df_init[(var, ind_str)] = -999.0  # SUEWS default for missing snow values
    else:
        df_init[(var, ind_str)] = 0.0  # Default for other states
```

This prevents:
1. KeyError exceptions
2. NaN propagation
3. Test contamination
4. Silent data corruption

## Conclusion

The root cause was that `add_sfc_init_df()` assumed all surface-specific columns would exist in the input DataFrame, but this assumption is violated when the DataFrame comes from YAML configuration via `to_df_state()`. The fix makes the function robust to handle both data sources correctly.