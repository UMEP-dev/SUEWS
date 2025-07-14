#!/bin/bash
# Quick debug script for HDD NaN issue in SSH session

echo "=== HDD NaN Debug Script ==="
echo "Issue: HDD3_Tmean and HDD4_T5d are NaN in CI but not locally"
echo

# Quick setup
setup_env() {
    echo "Setting up environment..."
    cd $GITHUB_WORKSPACE || cd /Users/runner/work/SUEWS/SUEWS
    
    # Install dependencies
    pip install --upgrade pip
    pip install pandas numpy pytest meson-python meson ninja
    pip install -r .github/requirements-ci.txt
    
    # Install gfortran if needed
    if ! command -v gfortran &> /dev/null; then
        brew install gfortran
    fi
}

# Build supy
build_supy() {
    echo -e "\nBuilding supy..."
    pip install -e . --no-build-isolation -v
}

# Test HDD issue
test_hdd() {
    echo -e "\nTesting HDD issue..."
    python -c "
import pandas as pd
import numpy as np
import supy as sp

# Load sample data
df_state_init, df_forcing = sp.load_SampleData()

# Check initial HDD values
print('Initial HDD values:')
grid_idx = df_state_init.index[0]
for i in range(12):
    col = ('hdd_id', f'({i},)')
    if col in df_state_init.columns:
        val = df_state_init.loc[grid_idx, col]
        print(f'  hdd_id[{i}] = {val}')

# Run simulation
df_forcing_10d = df_forcing.iloc[:288*10]
df_output, df_state = sp.run_supy(df_forcing_10d, df_state_init)

# Check DailyState
df_dailystate = df_output.loc[:, 'DailyState']
df_after_dropna = df_dailystate.dropna(how='all')

print(f'\nDailyState shape after dropna: {df_after_dropna.shape}')

# Check HDD columns
problem_cols = []
for col in ['HDD3_Tmean', 'HDD4_T5d']:
    if col in df_after_dropna.columns:
        if df_after_dropna[col].isna().all():
            problem_cols.append(col)
            print(f'PROBLEM: {col} is completely NaN!')
        else:
            print(f'{col}: {df_after_dropna[col].notna().sum()} valid values')

if problem_cols:
    print(f'\nInvestigating {problem_cols}...')
    # Check if it's a dt_since_start issue
    print('This might be related to dt_since_start initialization')
"
}

# Main menu
echo "Commands:"
echo "  source debug_hdd_ssh.sh  # Reload this script"
echo "  setup_env               # Install dependencies"
echo "  build_supy              # Build supy from source"
echo "  test_hdd                # Run HDD test"
echo
echo "Or run all: setup_env && build_supy && test_hdd"