# Internal-Only Parameters Analysis Report

Generated for GitHub Issue #555: Identify complete list of internal-only parameters

## Summary Statistics

- **Total internal-only parameters**: 24
- **Parameters in sample_config.yml**: 11
- **Parameters NOT in sample_config.yml**: 13
- **Files analyzed**: 2

## Parameters by File

### model.py (2 parameters)

| Parameter | In sample_config.yml | Unit |
|-----------|---------------------|------|
| `diagnose` | Yes |  |
| `kdownzen` | No |  |

### state.py (22 parameters)

| Parameter | In sample_config.yml | Unit |
|-----------|---------------------|------|
| `cdd_accum` | No | degC |
| `cdd_daily` | No | degC |
| `days_since_rain` | No | days |
| `days_since_rain_accum` | No | days |
| `dqndt` | Yes |  |
| `dqnsdt` | Yes |  |
| `dt_since_start` | Yes |  |
| `hdd_accum` | No | degC |
| `hdd_daily` | No | degC |
| `lenday_id` | Yes |  |
| `precip_accum` | No | mm |
| `precip_daily_total` | No | mm |
| `qn_av` | Yes |  |
| `qn_s_av` | Yes |  |
| `snowfallcum` | Yes |  |
| `temp_5day_accum` | No | degC |
| `temp_5day_mean` | No | degC |
| `temp_accum` | No | degC |
| `temp_daily_mean` | No | degC |
| `tmax_id` | Yes |  |
| `tmin_id` | Yes |  |
| `tstep_prev` | Yes |  |

## Parameters Present in sample_config.yml

These parameters are marked as internal-only but appear in the sample configuration:

| Parameter | File | Location in sample_config.yml |
|-----------|------|-------------------------------|
| `diagnose` | model.py:667 | diagnose |
| `dqndt` | state.py:815 | dqndt |
| `dqnsdt` | state.py:820 | dqnsdt |
| `dt_since_start` | state.py:825 | dt_since_start |
| `lenday_id` | state.py:830 | lenday_id |
| `qn_av` | state.py:835 | qn_av |
| `qn_s_av` | state.py:843 | qn_s_av |
| `snowfallcum` | state.py:877 | snowfallcum |
| `tmax_id` | state.py:856 | tmax_id |
| `tmin_id` | state.py:864 | tmin_id |
| `tstep_prev` | state.py:872 | tstep_prev |


## Parameters NOT in sample_config.yml

These parameters are internal-only and do not appear in the sample configuration:

| Parameter | File | Unit |
|-----------|------|------|
| `cdd_accum` | state.py:621 | degC |
| `cdd_daily` | state.py:677 | degC |
| `days_since_rain` | state.py:713 | days |
| `days_since_rain_accum` | state.py:657 | days |
| `hdd_accum` | state.py:612 | degC |
| `hdd_daily` | state.py:668 | degC |
| `kdownzen` | model.py:657 |  |
| `precip_accum` | state.py:648 | mm |
| `precip_daily_total` | state.py:704 | mm |
| `temp_5day_accum` | state.py:639 | degC |
| `temp_5day_mean` | state.py:695 | degC |
| `temp_accum` | state.py:630 | degC |
| `temp_daily_mean` | state.py:686 | degC |

## Recommendations

1. **Parameters in sample_config.yml**: Review whether these should remain internal-only or be exposed to users
2. **Parameters NOT in sample_config.yml**: These are likely true internal state variables that should remain hidden
3. **Consider advanced/developer mode**: For parameters needed in benchmark/testing scenarios

## Usage

This analysis was generated using:
```bash
python analyze_internal_only_parameters.py
```

Generated on: 2025-07-25 14:43:05
