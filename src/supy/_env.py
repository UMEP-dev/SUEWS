try:
    from importlib.resources import files
except ImportError:
    # backport for python < 3.9
    from importlib_resources import files


import logging
import os
import sys
import tempfile
import warnings
from logging.handlers import TimedRotatingFileHandler
from pathlib import Path


########################################################################
# this file provides variable and functions useful for the whole module.
########################################################################
# get Traversable object for loading resources in this package
# this can be used similarly as `pathlib.Path` object
trv_supy_module = files("supy")

# set up logger format, note `u` to guarantee UTF-8 encoding
FORMATTER = logging.Formatter("%(asctime)s - %(name)s - %(levelname)s - %(message)s")

# default log file name; only written when file logging is opted into
LOG_FILE = "SuPy.log"

# Environment variables to opt in to file logging. By default supy logs only to
# the console -- no SuPy.log is created -- unless one of these is set or
# `enable_file_logging()` is called explicitly. This avoids dropping a stray
# (often empty) SuPy.log into whatever directory a script happens to run from.
ENV_LOGFILE = "SUPY_LOGFILE"  # explicit path to the log file
ENV_LOG_DIR = "SUPY_LOG_DIR"  # directory; the file is <dir>/SuPy.log

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


def _coerce_logfile_path(path):
    """Resolve a user-supplied location to a concrete log-file path.

    A leading ``~`` is expanded to the user's home directory. The path is
    treated as a directory (the default ``SuPy.log`` name is appended) only
    when it already exists as a directory or is written with a trailing
    separator; otherwise it is taken as the log-file path itself.

    Parameters
    ----------
    path : str or pathlib.Path
        A log-file path, or a directory in which the default ``SuPy.log`` name
        is used.

    Returns
    -------
    pathlib.Path
        The resolved log-file path.
    """
    raw = str(path)
    path_logfile = Path(path).expanduser()
    if path_logfile.is_dir() or raw.endswith((os.sep, "/")):
        return path_logfile / LOG_FILE
    return path_logfile


def _resolve_env_logfile():
    """Return the opt-in log-file path requested via environment, or ``None``.

    Returns
    -------
    pathlib.Path or None
        Path from ``SUPY_LOGFILE`` (preferred) or ``SUPY_LOG_DIR``; ``None``
        when neither is set.
    """
    env_file = os.environ.get(ENV_LOGFILE)
    if env_file:
        return Path(env_file)
    env_dir = os.environ.get(ENV_LOG_DIR)
    if env_dir:
        return Path(env_dir) / LOG_FILE
    return None


def get_file_handler(path=None):
    """Build a rotating file handler that defers file creation.

    ``delay=True`` means the underlying file is opened only when the first
    record is emitted, so merely constructing the handler -- or importing
    supy -- never leaves a stray empty ``SuPy.log`` behind.

    Parameters
    ----------
    path : str or pathlib.Path, optional
        Target log file or directory. Defaults to ``SuPy.log`` in the current
        working directory.

    Returns
    -------
    logging.handlers.TimedRotatingFileHandler
        A handler configured for daily rotation at midnight, UTF-8 encoded.
    """
    path_logfile = _coerce_logfile_path(path) if path is not None else Path(LOG_FILE)
    log_dir = path_logfile.parent

    # Best-effort: make sure the target directory exists when one was asked
    # for. No log file is created here -- that is what `delay=True` guarantees.
    try:
        log_dir.mkdir(parents=True, exist_ok=True)
    except OSError:
        pass

    if not os.access(log_dir, os.W_OK):
        path_fallback = Path(tempfile.gettempdir()) / path_logfile.name
        warnings.warn(
            f"Log directory '{log_dir}' is not writable; "
            f"writing SuPy log to '{path_fallback}' instead",
            UserWarning,
            stacklevel=2,
        )
        path_logfile = path_fallback

    file_handler = TimedRotatingFileHandler(
        path_logfile,
        when="midnight",
        encoding="utf-8",
        delay=True,
    )
    file_handler.setFormatter(FORMATTER)
    return file_handler


def get_logger(logger_name, level=logging.DEBUG):
    logger = logging.getLogger(logger_name)
    # better to have too much log than not enough
    logger.setLevel(level)
    logger.addHandler(get_console_handler())
    # File logging is opt-in: no file handler is attached by default (see
    # `enable_file_logging` and the SUPY_LOGFILE / SUPY_LOG_DIR env vars).

    # with this pattern, it's rarely necessary to propagate the error up to parent
    logger.propagate = False
    return logger


logger_supy = get_logger("SuPy", logging.INFO)
logger_supy.debug("a debug message from SuPy")

# Reference to the active opt-in file handler (None until file logging is on).
_file_handler = None


def enable_file_logging(path=None):
    """Write SuPy log messages to a file (opt-in).

    Parameters
    ----------
    path : str or pathlib.Path, optional
        Target log file (a leading ``~`` is expanded to the home directory),
        or an existing directory / a path with a trailing separator, in which
        a ``SuPy.log`` file is written. Defaults to ``SuPy.log`` in the current
        working directory. For a directory that may not exist yet, prefer the
        ``SUPY_LOG_DIR`` environment variable.

    Returns
    -------
    pathlib.Path
        The resolved log-file path. The file itself is not created until the
        first log record is emitted (``delay=True``).

    Notes
    -----
    Calling this again after file logging is already enabled is a no-op; the
    existing handler's path is returned. Call :func:`disable_file_logging`
    first to switch to a different file.
    """
    global _file_handler
    if _file_handler is not None:
        return Path(_file_handler.baseFilename)
    handler = get_file_handler(path)
    logger_supy.addHandler(handler)
    _file_handler = handler
    return Path(handler.baseFilename)


def disable_file_logging():
    """Detach and close the opt-in file handler, if one is active.

    Returns
    -------
    None
    """
    global _file_handler
    if _file_handler is not None:
        logger_supy.removeHandler(_file_handler)
        _file_handler.close()
        _file_handler = None


# Honour the opt-in environment variables at import time so users can enable
# file logging without touching code (e.g. ``export SUPY_LOG_DIR=~/supy-logs``).
_env_logfile = _resolve_env_logfile()
if _env_logfile is not None:
    enable_file_logging(_env_logfile)


if sys.version_info >= (3, 8):
    from importlib import metadata
else:
    from importlib_metadata import metadata
