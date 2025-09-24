# Internal-Only Parameters Analysis Report

Generated for GitHub Issue #555: Identify complete list of internal-only parameters

## Summary Statistics

- **Total internal-only parameters**: 24
- **Parameters in sample_config.yml**: 11
- **Parameters NOT in sample_config.yml**: 13
- **Files analyzed**: 2

## Parameters by File

### model.py (2 parameters)

| Parameter | In sample_config.yml |
|-----------|---------------------|
| `diagnose` | Yes |
| `kdownzen` | No |

### state.py (22 parameters)

| Parameter | In sample_config.yml |
|-----------|---------------------|
| `cdd_accum` | No |
| `cdd_daily` | No |
| `days_since_rain` | No |
| `days_since_rain_accum` | No |
| `dqndt` | Yes |
| `dqnsdt` | Yes |
| `dt_since_start` | Yes |
| `hdd_accum` | No |
| `hdd_daily` | No |
| `lenday_id` | Yes |
| `precip_accum` | No |
| `precip_daily_total` | No |
| `qn_av` | Yes |
| `qn_s_av` | Yes |
| `snowfallcum` | Yes |
| `temp_5day_accum` | No |
| `temp_5day_mean` | No |
| `temp_accum` | No |
| `temp_daily_mean` | No |
| `tmax_id` | Yes |
| `tmin_id` | Yes |
| `tstep_prev` | Yes |

## Parameters Present in sample_config.yml

These parameters are marked as internal-only but appear in the sample configuration:

| Parameter | File |
|-----------|------|
| `diagnose` | model.py:667 |
| `dqndt` | state.py:815 |
| `dqnsdt` | state.py:820 |
| `dt_since_start` | state.py:825 |
| `lenday_id` | state.py:830 |
| `qn_av` | state.py:835 |
| `qn_s_av` | state.py:843 |
| `snowfallcum` | state.py:877 |
| `tmax_id` | state.py:856 |
| `tmin_id` | state.py:864 |
| `tstep_prev` | state.py:872 |


## Parameters NOT in sample_config.yml

These parameters are internal-only and do not appear in the sample configuration:

| Parameter | File |
|-----------|------|
| `cdd_accum` | state.py:621 |
| `cdd_daily` | state.py:677 |
| `days_since_rain` | state.py:713 |
| `days_since_rain_accum` | state.py:657 |
| `hdd_accum` | state.py:612 |
| `hdd_daily` | state.py:668 |
| `kdownzen` | model.py:657 |
| `precip_accum` | state.py:648 |
| `precip_daily_total` | state.py:704 |
| `temp_5day_accum` | state.py:639 |
| `temp_5day_mean` | state.py:695 |
| `temp_accum` | state.py:630 |
| `temp_daily_mean` | state.py:686 |

## Recommendations

1. **Parameters in sample_config.yml**: Review whether these should remain internal-only or be exposed to users
2. **Parameters NOT in sample_config.yml**: These are likely true internal state variables that should remain hidden
3. **Consider advanced/developer mode**: For parameters needed in benchmark/testing scenarios

## Usage

This analysis was generated using:
```bash
python analyze_internal_only_parameters.py
```

Generated on: 2025-07-25 14:44:46
