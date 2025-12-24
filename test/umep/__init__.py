"""UMEP/QGIS compatibility tests (GH-901, GH-1035).

This package tests SUEWS compatibility with UMEP plugins in QGIS environment.
Target environment: Windows + Python 3.12 (QGIS 3.40 LTR bundled Python).

Test modules:
- test_preprocessor.py: Database Manager, Database Prepare, ERA5 Download APIs
- test_processor.py: SUEWS model init/run/save and output path handling
- test_environment.py: QGIS-specific environment (None stdout/stderr)
- test_pyqgis_error_handling.py: Error handling in PyQGIS context (GH-1035)

See: https://github.com/UMEP-dev/SUEWS/issues/901
"""
