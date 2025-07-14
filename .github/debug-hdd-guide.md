# HDD NaN Debug Guide for SSH Session

## Quick Start
```bash
# Once in SSH session:
source ~/debug_hdd_issue.sh
# Choose option 7 to run all steps
```

## Issue Summary
- **Problem**: HDD3_Tmean and HDD4_T5d are NaN only in CI (ARM Mac + Python 3.12)
- **Works locally**: Same code produces valid values on local ARM Mac
- **GitHub Issue**: #478

## Key Investigation Points

### 1. Check Initial Values
```python
# In Python, check if hdd_id starts with NaN
import supy as sp
df_state_init, _ = sp.load_SampleData()
for i in range(12):
    col = ('hdd_id', f'({i},)')
    print(f"hdd_id[{i}] = {df_state_init.iloc[0][col]}")
```

### 2. Check dt_since_start
The 5-day rolling mean calculation depends on `dt_since_start`. Check if it's initialized properly:
```python
# Run with debug mode
df_output, df_state = sp.run_supy(df_forcing[:288*10], df_state_init, debug_mode=True)
# Check debug output for timer-related values
```

### 3. Floating Point Precision
Check for ARM64-specific float issues:
```python
# Test the exact calculation
days_prev = 0  # If dt_since_start is 0
hdd3 = 13.5   # Daily mean
hdd4_prev = 13.5  # Previous 5-day mean
hdd4_new = (hdd4_prev * days_prev + hdd3) / (days_prev + 1)
print(f"Result: {hdd4_new}, IsNaN: {np.isnan(hdd4_new)}")
```

### 4. Compare Environments
```bash
# Check compiler flags
gfortran -v 2>&1 | grep "configure"

# Check Python/NumPy build info
python -c "import numpy; numpy.show_config()"
```

## Claude Code Context
If installing Claude Code in the session:
```bash
# Tell Claude about the issue
claude -p "We're debugging issue #478 where HDD3_Tmean and HDD4_T5d are NaN in CI but not locally. 
The Fortran code at src/suews/src/suews_phys_dailystate.f95 calculates these values. 
HDD_id(3) is daily mean temp, HDD_id(4) is 5-day rolling mean."
```

## Suspected Causes
1. **Uninitialized dt_since_start**: Leading to division issues
2. **NaN in initial hdd_id**: Propagating through calculations  
3. **Compiler optimization**: Different float handling in CI
4. **Array bounds**: Fortran array indexing issues

## Direct Test Path
```bash
cd /Users/runner/work/SUEWS/SUEWS
python -m pytest test/test_resample_output.py::TestResampleOutput::test_resample_with_dailystate -xvs
```