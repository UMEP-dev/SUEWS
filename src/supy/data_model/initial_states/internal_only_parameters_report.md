# Internal-Only Parameters Analysis Report

Generated for GitHub Issue #555: Identify complete list of internal-only parameters

## Summary Statistics

- **Total internal-only parameters**: 24
- **Parameters in sample_config.yml**: 11
- **Parameters NOT in sample_config.yml**: 13
- **Files analyzed**: 2

## Parameters by File

### model.py (2 parameters)

| Parameter | In sample_config.yml | Description | Unit |
|-----------|---------------------|-------------|------|
| `diagnose` | Yes | Level of diagnostic output (0=none, 1=basic, 2=detailed) |  |
| `kdownzen` | No | Use zenithal correction for downward shortwave radiation |  |

### state.py (22 parameters)

| Parameter | In sample_config.yml | Description | Unit |
|-----------|---------------------|-------------|------|
| `cdd_accum` | No | Current day | degC |
| `cdd_daily` | No | Previous day | degC |
| `days_since_rain` | No | Days since rain for irrigation calculations [days] | days |
| `days_since_rain_accum` | No | Days since rain counter (current) [days] | days |
| `dqndt` | Yes | Change in net radiation |  |
| `dqnsdt` | Yes | Change in net shortwave radiation |  |
| `dt_since_start` | Yes | Time since start |  |
| `hdd_accum` | No | Current day | degC |
| `hdd_daily` | No | Previous day | degC |
| `lenday_id` | Yes | Length of the day ID |  |
| `precip_accum` | No | Current day | mm |
| `precip_daily_total` | No | Previous day | mm |
| `qn_av` | Yes | Average net radiation |  |
| `qn_s_av` | Yes | Average net shortwave radiation |  |
| `snowfallcum` | Yes | Cumulative snowfall |  |
| `temp_5day_accum` | No | 5-day running mean temperature accumulation [degC] | degC |
| `temp_5day_mean` | No | Previous 5-day running mean temperature for QF calculations [degC] | degC |
| `temp_accum` | No | Current day | degC |
| `temp_daily_mean` | No | Previous day | degC |
| `tmax_id` | Yes | Maximum temperature ID |  |
| `tmin_id` | Yes | Minimum temperature ID |  |
| `tstep_prev` | Yes | Previous time step |  |

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

| Parameter | File | Description |
|-----------|------|-------------|
| `cdd_accum` | state.py:621 | Current day |
| `cdd_daily` | state.py:677 | Previous day |
| `days_since_rain` | state.py:713 | Days since rain for irrigation calculations [days] |
| `days_since_rain_accum` | state.py:657 | Days since rain counter (current) [days] |
| `hdd_accum` | state.py:612 | Current day |
| `hdd_daily` | state.py:668 | Previous day |
| `kdownzen` | model.py:657 | Use zenithal correction for downward shortwave radiation |
| `precip_accum` | state.py:648 | Current day |
| `precip_daily_total` | state.py:704 | Previous day |
| `temp_5day_accum` | state.py:639 | 5-day running mean temperature accumulation [degC] |
| `temp_5day_mean` | state.py:695 | Previous 5-day running mean temperature for QF calculations [degC] |
| `temp_accum` | state.py:630 | Current day |
| `temp_daily_mean` | state.py:686 | Previous day |

## Recommendations

1. **Parameters in sample_config.yml**: Review whether these should remain internal-only or be exposed to users
2. **Parameters NOT in sample_config.yml**: These are likely true internal state variables that should remain hidden
3. **Consider advanced/developer mode**: For parameters needed in benchmark/testing scenarios

## Usage

This analysis was generated using:
```bash
python analyze_internal_only_parameters.py
```

Generated on: 2025-07-25 14:40:51
