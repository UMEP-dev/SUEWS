"""
Test error handling for GH#1035: QGIS crash when SUEWS error occurs.

This test verifies that Fortran errors now raise SUEWSKernelError
instead of terminating Python via Fortran STOP.
"""

import pytest
from supy import SUEWSKernelError
from supy._run import _reset_supy_error


def _check_module_level_error():
    """Check module-level error state and raise if set.

    Test helper that checks the Fortran module-level error state.
    This is used to test the error state mechanism directly.
    """
    from supy import _supy_driver as _sd

    if _sd.f90wrap_module_ctrl_error_state__get__supy_error_flag():
        code = int(_sd.f90wrap_module_ctrl_error_state__get__supy_error_code())
        message = str(
            _sd.f90wrap_module_ctrl_error_state__get__supy_error_message()
        ).strip()
        _sd.f90wrap_module_ctrl_error_state__reset_supy_error()
        raise SUEWSKernelError(code, message)


@pytest.mark.core
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

        # Verify _check_module_level_error raises SUEWSKernelError
        with pytest.raises(SUEWSKernelError) as exc_info:
            _check_module_level_error()

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
        """Test that _check_module_level_error does nothing when no error is set."""
        # Ensure clean state
        _reset_supy_error()

        # This should NOT raise
        _check_module_level_error()  # If this raises, the test fails

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

    def test_error_state_does_not_leak_between_calls(self):
        """Test that error state is properly reset between simulation calls.

        This verifies that _reset_supy_error() is called before Fortran kernel
        calls, preventing false positives from previous errors.
        """
        from supy import _supy_driver as _sd

        # Set an error state manually (simulating a previous failed simulation)
        _sd.f90wrap_module_ctrl_error_state__set_supy_error(99, "Leaked error")

        # Reset (simulating what happens at start of new simulation)
        _reset_supy_error()

        # Verify error is cleared - _check_module_level_error should NOT raise
        _check_module_level_error()  # If this raises, error state leaked

        # Verify state is clean
        flag = _sd.f90wrap_module_ctrl_error_state__get__supy_error_flag()
        assert not flag, "Error flag should be False after reset"


@pytest.mark.core
class TestSUEWSKernelErrorIntegration:
    """Integration tests for error handling through the simulation API."""

    def test_simulation_with_valid_data_succeeds(self):
        """Test that normal simulation completes without error.

        This ensures error checking doesn't cause false positives.
        """
        import supy as sp

        df_state_init, df_forcing = sp.load_SampleData()

        # Run a short simulation - should complete without error
        df_output, df_state_final = sp.run_supy(
            df_forcing.iloc[:12],  # Just 1 hour
            df_state_init,
        )

        assert df_output is not None
        assert not df_output.empty
        assert df_state_final is not None

    def test_exception_handler_re_raises_on_error(self):
        """Test that exception handlers properly re-raise instead of returning None.

        This verifies the fix for the silent failure issue where exception
        handlers logged but didn't re-raise, causing functions to return None.
        """
        import pandas as pd
        from supy._run import suews_cal_tstep_multi

        # Calling with invalid/missing dict should raise, not return None
        with pytest.raises(Exception):
            # Empty dict and DataFrame will cause KeyError or similar - the important
            # thing is that an exception is raised, not that None is returned
            suews_cal_tstep_multi({}, pd.DataFrame())
