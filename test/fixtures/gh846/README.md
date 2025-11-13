# GH-846 Test Fixture

Real URBANFLUXES (2017) data from Fredrik Lindberg that triggered the converter bug.

## Contents

- `RunControl.nml`: Minimal runcontrol from 2017 URBANFLUXES project
- `Inputbarb_v7/`: Original input data (21 files, 25MB uncompressed)
- **Missing**: `SUEWS_SPARTACUS.nml` (this is the key - file doesn't exist in old data)

## Source

Downloaded from: https://github.com/user-attachments/files/23478665/Inputbarb_v7.zip
Issue: https://github.com/UMEP-dev/SUEWS/issues/846

## Purpose

Tests that the YAML converter gracefully handles:
1. Missing SPARTACUS.nml (old configs don't have it)
2. Logging when sys.stdout is None (QGIS environment)

This data reproduces the exact scenario that caused the original bug.
