try:
    from importlib.resources import files
except ImportError:
    # backport for python < 3.9
    from importlib_resources import files


from logging.handlers import TimedRotatingFileHandler
import sys
import logging
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
# DTS and f90wrap backends have been removed.
# The Rust bridge is now the only execution backend.

DTS_ERROR_MSG = (
    "DTS features have been removed.\n"
    "The f90wrap/f2py backends are no longer available.\n"
    "Use the Rust bridge backend via SUEWSSimulation instead."
)


def _init_dts_check():
    """DTS is no longer available (f90wrap removed). Always returns False."""
    return False


def check_dts_available():
    """Raise RuntimeError — DTS features have been removed."""
    raise RuntimeError(DTS_ERROR_MSG)
