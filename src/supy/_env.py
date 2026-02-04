try:
    from importlib.resources import files
except ImportError:
    # backport for python < 3.9
    from importlib_resources import files


from logging.handlers import TimedRotatingFileHandler
import functools
import sys
import logging
import inspect
from pathlib import Path
import tempfile


########################################################################
# this file provides variable and functions useful for the whole module.
########################################################################
# get Traversable object for loading resources in this package
# this can be used similarly as `pathlib.Path` object
trv_supy_module = files("supy")

# set up logger format, note `u` to guarantee UTF-8 encoding
FORMATTER = logging.Formatter("%(asctime)s - %(name)s - %(levelname)s - %(message)s")

# log file name
LOG_FILE = "SuPy.log"

# issue reporting URL
ISSUES_URL = "https://github.com/UMEP-dev/SUEWS/issues/new"


def get_console_handler():
    # Check if stdout is available (can be None in GUI environments like QGIS)
    if sys.stdout is not None:
        console_handler = logging.StreamHandler(sys.stdout)
        console_handler.setFormatter(FORMATTER)
        return console_handler
    else:
        # Return NullHandler if stdout is not available
        return logging.NullHandler()


def get_file_handler():
    try:
        path_logfile = Path(LOG_FILE)
        path_logfile.touch()
    except Exception as e:
        import warnings

        tempdir = tempfile.gettempdir()
        path_logfile = Path(tempdir) / LOG_FILE
        warnings.warn(
            f"Could not create log file at {LOG_FILE} ({e}); using {path_logfile} instead",
            UserWarning,
            stacklevel=2,
        )

    file_handler = TimedRotatingFileHandler(
        path_logfile,
        when="midnight",
        encoding="utf-8",
    )
    file_handler.setFormatter(FORMATTER)
    return file_handler


def get_logger(logger_name, level=logging.DEBUG):
    logger = logging.getLogger(logger_name)
    # better to have too much log than not enough
    logger.setLevel(level)
    logger.addHandler(get_console_handler())
    logger.addHandler(get_file_handler())

    # with this pattern, it's rarely necessary to propagate the error up to parent
    logger.propagate = False
    return logger


logger_supy = get_logger("SuPy", logging.INFO)
logger_supy.debug("a debug message from SuPy")


if sys.version_info >= (3, 8):
    from importlib import metadata
else:
    from importlib_metadata import metadata


########################################################################
# DTS (Derived Type Structure) availability check
########################################################################
# DTS features require a full build with type wrappers (wrap_dts_types=true).
# Fast builds (make dev) do not include DTS support.

DTS_ERROR_MSG = (
    "DTS features not available in this build.\n"
    "This build was compiled with 'make dev' (fast build without DTS support).\n"
    "To use DTS features, rebuild with: make clean && make dev-dts"
)


@functools.cache
def _init_dts_check():
    """Check DTS availability after supy_driver is loaded.

    Returns
    -------
    bool
        True if DTS type classes are available in this build.

    Notes
    -----
    Result is cached after first call since DTS availability
    does not change during a session.
    """
    try:
        from . import supy_driver as _supy_driver

        # Check if DTS type classes exist (only present in full build with wrap_dts_types=true)
        # The module_type_heat module exists in both builds, but the HEAT_STATE class
        # is only generated when DTS type wrappers are enabled
        return hasattr(_supy_driver, "module_type_heat") and hasattr(
            _supy_driver.module_type_heat, "HEAT_STATE"
        )
    except ImportError:
        return False
    except Exception as exc:
        logger_supy.debug("Unexpected error checking DTS availability: %s", exc)
        return False


def check_dts_available():
    """Raise RuntimeError if DTS features are not available."""
    if not _init_dts_check():
        raise RuntimeError(DTS_ERROR_MSG)
