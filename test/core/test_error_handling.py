"""
Test error handling for GH#1035: QGIS crash when SUEWS error occurs.

This test verifies that Fortran errors now raise SUEWSKernelError
instead of terminating Python via Fortran STOP.
"""

import pytest
from supy import SUEWSKernelError
from supy._run import _check_supy_error, _reset_supy_error


@pytest.mark.smoke
class TestSUEWSKernelError:
    """Test the SUEWSKernelError mechanism that replaces Fortran STOP."""

    def test_error_state_set_and_check(self):
        """Test that setting error state and checking raises SUEWSKernelError."""
        from supy import _supy_driver as _sd

        # Ensure clean state
        _reset_supy_error()

        # Set error via Fortran function
        _sd.f90wrap_module_ctrl_error_state__set_supy_error(
            42, "Test error for GH#1035"
        )

        # Verify error flag is set
        flag = _sd.f90wrap_module_ctrl_error_state__get__supy_error_flag()
        assert flag, "Error flag should be True after set_supy_error"

        # Verify _check_supy_error raises SUEWSKernelError
        with pytest.raises(SUEWSKernelError) as exc_info:
            _check_supy_error()

        assert exc_info.value.code == 42
        assert "Test error" in str(exc_info.value.message)

        # Verify error was reset after exception
        flag = _sd.f90wrap_module_ctrl_error_state__get__supy_error_flag()
        assert not flag, "Error flag should be reset after exception"

    def test_reset_supy_error(self):
        """Test that _reset_supy_error clears error state."""
        from supy import _supy_driver as _sd

        # Set an error
        _sd.f90wrap_module_ctrl_error_state__set_supy_error(99, "Error to reset")

        # Reset it
        _reset_supy_error()

        # Verify all fields are cleared
        flag = _sd.f90wrap_module_ctrl_error_state__get__supy_error_flag()
        code = _sd.f90wrap_module_ctrl_error_state__get__supy_error_code()
        msg = _sd.f90wrap_module_ctrl_error_state__get__supy_error_message()

        assert not flag, "Error flag should be False after reset"
        assert code == 0, "Error code should be 0 after reset"
        assert msg.strip() == b"" or msg.strip() == "", "Error message should be empty"

    def test_no_error_does_not_raise(self):
        """Test that _check_supy_error does nothing when no error is set."""
        # Ensure clean state
        _reset_supy_error()

        # This should NOT raise
        _check_supy_error()  # If this raises, the test fails

    def test_suews_kernel_error_attributes(self):
        """Test SUEWSKernelError has correct attributes."""
        error = SUEWSKernelError(36, "z-d needs to be larger than 0")

        assert error.code == 36
        assert error.message == "z-d needs to be larger than 0"
        assert "SUEWS Error 36" in str(error)
        assert "z-d needs to be larger than 0" in str(error)

    def test_suews_kernel_error_is_runtime_error(self):
        """Test SUEWSKernelError is a RuntimeError subclass."""
        error = SUEWSKernelError(1, "test")
        assert isinstance(error, RuntimeError)
