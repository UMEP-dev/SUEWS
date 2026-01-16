"""UMEP/QGIS compatibility tests (GH-901, GH-1035).

Tests skipped unless on Windows + QGIS LTR Python version.
See: https://github.com/UMEP-dev/SUEWS/issues/901
"""

import sys

# QGIS LTR Python version - update when QGIS LTR changes
# QGIS 3.40 LTR (2024-2025) uses Python 3.12
# Check: https://qgis.org/en/site/forusers/visualchangelog340/
QGIS_LTR_PYTHON_VERSION = (3, 12)

# Target environment check
IS_QGIS_TARGET = (
    sys.platform == "win32"
    and sys.version_info[:2] == QGIS_LTR_PYTHON_VERSION
)
