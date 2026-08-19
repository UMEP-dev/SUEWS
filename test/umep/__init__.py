"""UMEP/QGIS compatibility tests (GH-901).

This package tests SUEWS compatibility with UMEP plugins in QGIS environment.
Target environment: Windows + Python 3.12. Current Windows QGIS 3 LTR and
QGIS 4 runtimes share this CPython line; these tests guard plugin-facing API
contracts rather than Rust backend physics.

Test modules:
- test_preprocessor.py: Database Manager, Database Prepare, ERA5 Download
- test_processor.py: SUEWS model runs (init, run, save)
- test_postprocessor.py: Output path handling
- test_environment.py: QGIS-specific environment (None stdout/stderr)
- test_imports.py: Import path verification

See: https://github.com/UMEP-dev/SUEWS/issues/901
"""
