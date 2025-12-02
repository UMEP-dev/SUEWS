"""Tests for QGIS environment handling (GH-901, GH-902).

This module tests QGIS-specific environment handling:
- None stdout/stderr handling (QGIS Python console)
- Logging with None streams
- safe_stdout_stderr() context manager

See: https://github.com/UMEP-dev/SUEWS/issues/901
"""

import sys
from unittest import TestCase

import pytest

from .conftest import _HAS_SAFE_STDOUT_STDERR


class TestQGISEnvironment(TestCase):
    """Test QGIS-specific environment handling.

    QGIS has a unique environment where sys.stdout may be None,
    especially in the Python console.
    """

    def test_suews_import_with_none_stdout(self):
        """Test that SUEWS imports correctly when stdout is None."""
        original_stdout = sys.stdout

        try:
            # Simulate QGIS environment
            sys.stdout = None

            # Re-import should not crash
            # Note: Full reimport is complex, so we test the handler
            from supy._env import get_console_handler

            handler = get_console_handler()
            self.assertIsNotNone(handler)

        finally:
            sys.stdout = original_stdout

    def test_logging_with_none_stdout(self):
        """Test that logging works when stdout is None."""
        import logging

        from supy._env import get_console_handler

        original_stdout = sys.stdout

        try:
            sys.stdout = None

            handler = get_console_handler()
            logger = logging.getLogger("test_qgis_compat")
            logger.addHandler(handler)
            logger.setLevel(logging.INFO)

            # This should not raise an error
            logger.info("Test message in QGIS-like environment")

        finally:
            sys.stdout = original_stdout

    @pytest.mark.skipif(
        not _HAS_SAFE_STDOUT_STDERR,
        reason="safe_stdout_stderr not available (requires PR #903)",
    )
    def test_safe_stdout_stderr_with_none_streams(self):
        """Test that safe_stdout_stderr() handles None stdout/stderr (GH-902)."""
        from supy.util._era5 import safe_stdout_stderr

        # Save originals
        orig_stdout = sys.stdout
        orig_stderr = sys.stderr

        try:
            # Simulate QGIS Processing environment
            sys.stdout = None
            sys.stderr = None

            # Inside context, streams should be valid file objects
            with safe_stdout_stderr():
                self.assertIsNotNone(sys.stdout)
                self.assertIsNotNone(sys.stderr)
                # Should be able to write without crash
                sys.stdout.write("test")
                sys.stderr.write("test")

            # After context, streams should be restored to None
            self.assertIsNone(sys.stdout)
            self.assertIsNone(sys.stderr)

        finally:
            sys.stdout = orig_stdout
            sys.stderr = orig_stderr

    @pytest.mark.skipif(
        not _HAS_SAFE_STDOUT_STDERR,
        reason="safe_stdout_stderr not available (requires PR #903)",
    )
    def test_safe_stdout_stderr_noop_with_valid_streams(self):
        """Test that safe_stdout_stderr() is a no-op when streams are valid."""
        from supy.util._era5 import safe_stdout_stderr

        # With valid streams, context manager should not modify them
        orig_stdout = sys.stdout
        orig_stderr = sys.stderr

        with safe_stdout_stderr():
            self.assertIs(sys.stdout, orig_stdout)
            self.assertIs(sys.stderr, orig_stderr)

        # After context, streams should still be the same
        self.assertIs(sys.stdout, orig_stdout)
        self.assertIs(sys.stderr, orig_stderr)

    @pytest.mark.skipif(
        not _HAS_SAFE_STDOUT_STDERR,
        reason="safe_stdout_stderr not available (requires PR #903)",
    )
    def test_tqdm_with_none_stdout(self):
        """Test that tqdm doesn't crash when stdout is None (GH-902)."""
        from tqdm import tqdm

        from supy.util._era5 import safe_stdout_stderr

        orig_stdout = sys.stdout
        orig_stderr = sys.stderr

        try:
            sys.stdout = None
            sys.stderr = None

            # This should NOT crash
            with safe_stdout_stderr():
                for _ in tqdm(range(3), desc="Testing"):
                    pass

        finally:
            sys.stdout = orig_stdout
            sys.stderr = orig_stderr
