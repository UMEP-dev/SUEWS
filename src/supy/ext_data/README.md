# External Data Directory

This directory contains external datasets used by SUEWS/SuPy for various calculations and initialisations.

## Current Datasets

### CRU_TS4.06_1991_2020.parquet
- **Source**: Climatic Research Unit (CRU) TS4.06 dataset
- **Content**: Monthly temperature normals (1991-2020) on a 0.5° global grid
- **Format**: Parquet (compressed from original 19MB CSV to 2.3MB)
- **Usage**: Provides location-specific mean monthly air temperatures for SUEWS precheck initialisation
- **Access**: Loaded via `importlib.resources` for proper package resource management

## Adding New External Data

When adding new external datasets:
1. Use efficient formats (Parquet, HDF5, etc.) to minimise package size
2. Document the data source, content, and usage in this README
3. Update `src/supy/meson.build` to include the new data file
4. Use `importlib.resources` for loading to ensure compatibility with installed packages

## Development Notes

- During development, the code will fall back to test fixtures if package resources are not available
- The original CSV version of CRU data is retained in `test/fixtures/cru_data/` for testing and verification