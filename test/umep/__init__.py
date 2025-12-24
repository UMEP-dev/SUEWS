"""UMEP/QGIS compatibility tests (GH-901, GH-1035).

This package tests SUEWS compatibility with UMEP plugins in QGIS environment.
Target environment: Windows + Python 3.12 (QGIS 3.40 LTR bundled Python).

Test modules:
- test_umep_api.py: All UMEP API tests (skipped unless Windows + Py3.12)
- test_pyqgis_error_handling.py: Error handling in PyQGIS context (CI)

See: https://github.com/UMEP-dev/SUEWS/issues/901
"""
