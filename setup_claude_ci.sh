#!/bin/bash
# Setup Claude Code in CI SSH session for HDD NaN debugging

echo "=== Setting up Claude Code for HDD NaN Investigation ==="
echo

# 1. Install Claude Code
install_claude() {
    echo "Installing Claude Code..."
    
    # Try npm first (usually available on GitHub runners)
    if command -v npm &> /dev/null; then
        npm install -g @anthropic-ai/claude-cli
    else
        # Install via curl
        curl -fsSL https://storage.googleapis.com/claude-cli/install.sh | sh
        export PATH="$HOME/.local/bin:$PATH"
    fi
    
    # Verify installation
    if command -v claude &> /dev/null; then
        echo "✓ Claude Code installed successfully"
        claude --version
    else
        echo "✗ Failed to install Claude Code"
        return 1
    fi
}

# 2. Create context file for Claude
create_context() {
    echo -e "\nCreating context file..."
    
    cat > ~/claude_context.md << 'EOF'
# HDD NaN Debugging Context

## Issue Summary
We're investigating GitHub issue #478: HDD3_Tmean and HDD4_T5d columns are completely NaN in CI (ARM Mac + Python 3.12) but work fine locally.

## Current Environment
- Platform: ARM64 Mac (GitHub Actions runner)
- Python: 3.12
- Location: /Users/runner/work/SUEWS/SUEWS
- Branch: feature/pragmatic-test

## The Problem
In the DailyState output:
- HDD1_h and HDD2_c have valid values
- HDD3_Tmean (daily mean temp) is completely NaN
- HDD4_T5d (5-day rolling mean) is completely NaN

## Key Code Locations
1. Fortran calculation: `src/suews/src/suews_phys_dailystate.f95`
   - Line 821: `HDD_id(3) = HDD_id(3) + Tair` (accumulates daily mean)
   - Line 1233: `HDD_id(4) = (HDD_id(4)*days_prev + HDD_id(3))/(days_prev + 1)` (5-day mean)

2. Python interface: `src/supy/_post.py`
   - Modified to use `dropna(how='all')` for DailyState

## Suspected Causes
1. `dt_since_start` might be 0 or uninitialized in CI
2. Initial HDD values might be NaN (locally they're 13.5)
3. Division by zero in 5-day mean calculation
4. Platform-specific floating point behavior

## Quick Test Commands
```python
# Test 1: Check initial values
import supy as sp
df_state_init, df_forcing = sp.load_SampleData()
grid = df_state_init.index[0]
for i in range(12):
    col = ('hdd_id', f'({i},)')
    val = df_state_init.loc[grid, col]
    print(f'hdd_id[{i}] = {val}')

# Test 2: Run and check
df_output, df_state = sp.run_supy(df_forcing[:288*10], df_state_init)
df_daily = df_output.loc[:, 'DailyState'].dropna(how='all')
print(f"HDD3_Tmean NaN count: {df_daily['HDD3_Tmean'].isna().sum()}/{len(df_daily)}")
print(f"HDD4_T5d NaN count: {df_daily['HDD4_T5d'].isna().sum()}/{len(df_daily)}")
```

## Investigation Tasks
1. Check if initial hdd_id values are NaN
2. Verify dt_since_start initialization
3. Test the exact 5-day mean calculation with CI values
4. Compare gfortran flags between local and CI
5. Check for any divide-by-zero scenarios

## Files Already Modified
- `src/supy/_post.py` - Uses dropna(how='all') for DailyState
- `test/test_resample_output.py` - Added HDD-specific debugging

The test is now passing but we need to understand why these columns are NaN.
EOF

    echo "✓ Context file created at ~/claude_context.md"
}

# 3. Create investigation script
create_investigation_script() {
    echo -e "\nCreating investigation script..."
    
    cat > ~/investigate_hdd.py << 'EOF'
#!/usr/bin/env python3
"""Investigate HDD NaN issue in CI"""

import numpy as np
import pandas as pd
import supy as sp
import sys
import os

print("=== HDD NaN Investigation ===")
print(f"Python: {sys.version}")
print(f"NumPy: {np.__version__}")
print(f"Pandas: {pd.__version__}")
print(f"Working dir: {os.getcwd()}")

# Load sample data
print("\n1. Loading sample data...")
df_state_init, df_forcing = sp.load_SampleData()

# Check initial HDD values
print("\n2. Initial HDD values:")
grid = df_state_init.index[0]
hdd_values = []
for i in range(12):
    col = ('hdd_id', f'({i},)')
    if col in df_state_init.columns:
        val = df_state_init.loc[grid, col]
        hdd_values.append(val)
        print(f"   hdd_id[{i}] = {val} (type: {type(val).__name__})")
        if pd.isna(val):
            print(f"   WARNING: hdd_id[{i}] is NaN!")

# Check if it's a data type issue
print(f"\n3. HDD array type check:")
print(f"   All same type? {len(set(type(v) for v in hdd_values)) == 1}")
print(f"   All numeric? {all(isinstance(v, (int, float, np.number)) for v in hdd_values)}")

# Run simulation
print("\n4. Running simulation...")
df_forcing_10d = df_forcing.iloc[:288*10]
df_output, df_state = sp.run_supy(df_forcing_10d, df_state_init)

# Check DailyState
print("\n5. DailyState analysis:")
df_daily_full = df_output.loc[:, 'DailyState']
df_daily = df_daily_full.dropna(how='all')

print(f"   Shape after dropna: {df_daily.shape}")
print(f"   Time range: {df_daily.index[0]} to {df_daily.index[-1]}")

# Focus on HDD columns
hdd_cols = ['HDD1_h', 'HDD2_c', 'HDD3_Tmean', 'HDD4_T5d']
print("\n6. HDD column status:")
for col in hdd_cols:
    if col in df_daily.columns:
        nan_count = df_daily[col].isna().sum()
        if nan_count == len(df_daily):
            print(f"   {col}: COMPLETELY NaN ❌")
        elif nan_count > 0:
            print(f"   {col}: {nan_count}/{len(df_daily)} NaN ⚠️")
        else:
            print(f"   {col}: No NaN ✓")
            print(f"      Sample values: {df_daily[col].iloc[:3].values}")

# Check temperature data
print("\n7. Temperature check (for HDD calculation):")
df_met = df_output.loc[:, 'met']
if 'Tair' in df_met.columns:
    print(f"   Tair range: {df_met.Tair.min():.2f} to {df_met.Tair.max():.2f}")
    print(f"   Tair mean: {df_met.Tair.mean():.2f}")
    print(f"   Any NaN? {df_met.Tair.isna().any()}")

# Test the calculation directly
print("\n8. Testing 5-day mean calculation:")
# Simulate what happens in Fortran
dt_since_start = 0  # Day 1
days_prev = min(4, int(dt_since_start / (24*60*60)))
hdd3 = 13.5  # Typical value
hdd4_prev = 13.5

print(f"   dt_since_start = {dt_since_start}")
print(f"   days_prev = {days_prev}")
print(f"   Calculation: ({hdd4_prev} * {days_prev} + {hdd3}) / ({days_prev} + 1)")

if days_prev == 0:
    hdd4_new = hdd3  # Special case for first day
else:
    hdd4_new = (hdd4_prev * days_prev + hdd3) / (days_prev + 1)

print(f"   Result: {hdd4_new}")
print(f"   Is NaN? {np.isnan(hdd4_new)}")

# Check for any infinity or special values
print("\n9. Checking for special float values in initial state:")
for i, val in enumerate(hdd_values[:6]):  # First 6 are the active values
    if isinstance(val, (float, np.floating)):
        print(f"   hdd_id[{i}]: finite={np.isfinite(val)}, inf={np.isinf(val)}, nan={np.isnan(val)}")

print("\n=== Investigation Complete ===")
EOF

    chmod +x ~/investigate_hdd.py
    echo "✓ Investigation script created at ~/investigate_hdd.py"
}

# 4. Launch Claude with context
launch_claude() {
    echo -e "\nLaunching Claude Code with context..."
    
    # Create a launch script that provides full context
    cat > ~/claude_debug.sh << 'EOF'
#!/bin/bash
cd /Users/runner/work/SUEWS/SUEWS

# Run investigation first
echo "Running investigation script..."
python ~/investigate_hdd.py

echo -e "\n=== Launching Claude Code ==="
echo "Context file: ~/claude_context.md"
echo "Investigation results above ↑"
echo

# Launch Claude with the context
claude -p "I need help debugging issue #478 in SUEWS. The context is in ~/claude_context.md. 
We just ran investigate_hdd.py and the results are shown above. 

The main issue: HDD3_Tmean and HDD4_T5d are NaN in CI but not locally.

Key files to check:
- src/suews/src/suews_phys_dailystate.f95 (Fortran calculations)
- src/supy/_post.py (already modified with dropna fix)
- test/test_resample_output.py (the test that was failing)

Please help me understand why these specific columns are NaN only in CI."
EOF

    chmod +x ~/claude_debug.sh
    echo "✓ Launch script created"
    echo
    echo "To start debugging, run: ~/claude_debug.sh"
}

# Main execution
echo "This script will set up Claude Code for debugging the HDD NaN issue."
echo
echo "Steps:"
echo "1. Install Claude Code"
echo "2. Create context file with issue details"
echo "3. Create investigation script"
echo "4. Create launch script"
echo
read -p "Continue? (y/n) " -n 1 -r
echo

if [[ $REPLY =~ ^[Yy]$ ]]; then
    install_claude
    create_context
    create_investigation_script
    launch_claude
    
    echo -e "\n=== Setup Complete ==="
    echo "Quick commands:"
    echo "  python ~/investigate_hdd.py  # Run investigation"
    echo "  ~/claude_debug.sh           # Launch Claude with context"
    echo "  cat ~/claude_context.md     # View context"
    echo
    echo "Claude will have full context about the HDD NaN issue."
else
    echo "Setup cancelled."
fi