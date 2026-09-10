# GH-846 Test Fixture

Real URBANFLUXES (2017) data from Fredrik Lindberg that triggered the converter bug.

## Contents

- `RunControl.nml`: Minimal runcontrol from 2017 URBANFLUXES project
- `Inputbarb_v7/`: The input tables and namelists of the original dataset
- **Missing**: `SUEWS_SPARTACUS.nml` (this is the key - file doesn't exist in old data)

The original upload also carried the site's meteorological forcing
(`barb_2015_data_5.txt` and `barb_2015_ESTM_Ts_data_5.txt`, 7.3 MB between
them). Those were removed: `convert_table` collects its inputs by globbing
`SUEWS_*.txt` and `*.nml` only, so neither file was ever read by the tests that
use this fixture, and no test runs the dataset. Every remaining table is one the
conversion chain carries or converts, so the fixture stays a real legacy dataset
rather than a synthetic minimum.

## Source

Downloaded from: https://github.com/user-attachments/files/23478665/Inputbarb_v7.zip
Issue: https://github.com/UMEP-dev/SUEWS/issues/846

## Purpose

Tests that the YAML converter gracefully handles:
1. Missing SPARTACUS.nml (old configs don't have it)
2. Logging when sys.stdout is None (QGIS environment)

This data reproduces the exact scenario that caused the original bug.
Version detection resolves it to 2017a, so `convert_table(..., "2025a")` runs the
full ten-hop legacy chain.
