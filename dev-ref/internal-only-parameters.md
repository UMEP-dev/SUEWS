# Internal-Only Parameters Guide

## Overview

This document provides guidance on identifying and managing internal-only parameters in SUEWS/SuPy. Internal-only parameters are those that should not be exposed to end users in the YAML configuration interface, as they are either calculated internally by the model or used solely for debugging/development purposes.

## Criteria for Internal-Only Parameters

Parameters should be marked as `internal_only=True` when they meet one or more of these criteria:

### 1. **Internal State Variables**
Variables that the model calculates and maintains internally during runtime:
- Derivatives and rate calculations (e.g., `dqndt`, `dqnsdt`)
- Running averages (e.g., `qn_av`, `qn_s_av`, `tair_av`)
- Timestep tracking (e.g., `dt_since_start`, `tstep_prev`)
- Accumulated values for internal calculations (e.g., `temp_accum`, `precip_accum`)

### 2. **Diagnostic/Debug Parameters**
Parameters originally designed for developer debugging:
- `diagnose` - Legacy debugging flag from Fortran implementation
- `kdownzen` - Internal zenith angle calculations

### 3. **Internal Indices and Identifiers**
Variables used for internal array indexing or state tracking:
- `lenday_id` - Day length identifier
- `tmax_id`, `tmin_id` - Temperature extreme tracking
- Array indices for internal calculations

## Parameters Requiring User Access

The following should remain user-accessible despite being related to model state:

### 1. **Initial Conditions**
Users need to set starting conditions for simulations:
- `state` - Surface water depth (mm) for each surface type
- `snowfallcum` - Initial accumulated snowfall for mid-winter starts
- Surface temperatures and soil moisture

### 2. **Computational Settings**
Advanced users may need to tune these for performance:
- SPARTACUS `n_stream_lw_urban`, `n_stream_sw_urban` - Radiation accuracy vs speed
- Physics method selections
- Time step configurations

### 3. **Physical Properties**
All physical characteristics of the urban environment:
- Albedo, emissivity, conductivity values
- Building dimensions and properties
- Surface fractions and characteristics

## Current Internal-Only Parameters

Based on analysis of the codebase (see `src/supy/data_model/initial_states/internal_only_parameters_report.md`):

### Parameters Already Marked Internal-Only (24 total)

**In `model.py`:**
- `diagnose` - Developer debugging flag
- `kdownzen` - Internal calculation

**In `state.py`:**
- Accumulation variables: `cdd_accum`, `hdd_accum`, `temp_accum`, `temp_5day_accum`, `precip_accum`, `days_since_rain_accum`
- Daily calculations: `cdd_daily`, `hdd_daily`, `temp_daily_mean`, `temp_5day_mean`, `precip_daily_total`
- Time tracking: `days_since_rain`, `dt_since_start`, `tstep_prev`
- Averaging variables: `dqndt`, `dqnsdt`, `qn_av`, `qn_s_av`
- Indices: `lenday_id`, `tmax_id`, `tmin_id`
- Physical state: `snowfallcum`

## Recommendations for Additional Parameters

### Should Be Made Internal-Only:
1. **`tair_av`** - Internal air temperature averaging variable
2. **`hdd_id` array** - Internal heating degree day tracking array

### Should Remain User-Accessible:
1. **All `state` parameters** - Essential for setting initial water depths
2. **SPARTACUS numerical parameters** - Valid user tuning options
3. **`snowfallcum`** - Users may need to set initial snow conditions

### Requires Further Discussion:
1. **`diagnose`** - Currently accessible but questionable user value
   - Pro: Some users might use for troubleshooting
   - Con: Legacy developer tool that may confuse users

## Implementation Notes

When marking parameters as internal-only in the data model:

```python
field_name: type = Field(
    default=value,
    description="Description",
    json_schema_extra={
        "internal_only": True,
        "unit": "unit_if_applicable"
    }
)
```

## Future Considerations

1. **User/Developer Mode**: Consider implementing separate configuration modes
2. **Documentation**: Clearly document which parameters are available to users
3. **Migration Path**: Plan for moving parameters between internal/external status
4. **Validation**: Ensure internal parameters are properly initialized by the model

## References

- Issue #555: Identify complete list of internal-only parameters
- PR #556: Comprehensive internal-only parameters analysis
- Analysis script: `src/supy/data_model/initial_states/analyze_internal_only_parameters.py`