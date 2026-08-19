"""Opt-in file logging: importing/using supy must not create SuPy.log.

Regression guard for the opt-in file-logging behaviour. Historically
``supy._env`` attached a ``TimedRotatingFileHandler`` at import time that
eagerly ``touch()``-ed an empty ``SuPy.log`` in the current working
directory the instant any heavy submodule was imported. File logging is now
opt-in: a file is written only when the user sets ``SUPY_LOG_DIR`` /
``SUPY_LOGFILE`` or calls :func:`supy.enable_file_logging`, and even then it
is created only on the first emitted record (``delay=True``).

Import-time behaviour is exercised in clean subprocesses because ``_env`` is
imported only once per interpreter; an in-process test cannot observe the
first import.
"""

import os
import subprocess
import sys

import pytest

pytestmark = [pytest.mark.api, pytest.mark.util]

LOG_FILE = "SuPy.log"


def _run(code, cwd, extra_env=None):
    """Run ``code`` in a clean subprocess with the opt-in env vars stripped."""
    env = dict(os.environ)
    env.pop("SUPY_LOGFILE", None)
    env.pop("SUPY_LOG_DIR", None)
    if extra_env:
        env.update(extra_env)
    return subprocess.run(
        [sys.executable, "-c", code],
        cwd=str(cwd),
        env=env,
        capture_output=True,
        text=True,
        check=False,
    )


def test_no_logfile_on_import_and_use(tmp_path):
    """Importing supy and touching heavy modules must not create SuPy.log."""
    code = (
        "import supy\n"
        "_ = supy.util\n"  # forces _env import via a heavy submodule
        "from supy._env import logger_supy\n"  # direct _env import
        "print('OK')\n"
    )
    res = _run(code, tmp_path)
    assert res.returncode == 0, res.stderr
    assert "OK" in res.stdout
    assert not (tmp_path / LOG_FILE).exists(), (
        f"SuPy.log must not be created merely by importing/using supy; "
        f"stderr={res.stderr}"
    )


def test_enable_file_logging_is_delayed(tmp_path):
    """enable_file_logging() attaches a handler but defers file creation."""
    code = (
        "from pathlib import Path\n"
        "import supy\n"
        "supy.enable_file_logging()\n"
        "assert not Path('SuPy.log').exists(), 'delay=True: no file before first write'\n"
        "from supy._env import logger_supy\n"
        "logger_supy.info('hello from regression test')\n"
        "assert Path('SuPy.log').exists(), 'file must appear after first INFO write'\n"
        "print('OK')\n"
    )
    res = _run(code, tmp_path)
    assert res.returncode == 0, res.stderr
    assert "OK" in res.stdout
    assert (tmp_path / LOG_FILE).exists()


def test_env_var_opt_in_writes_to_requested_dir(tmp_path):
    """SUPY_LOG_DIR routes the log file into the requested directory only."""
    log_dir = tmp_path / "logs"
    code = (
        "import supy\n"
        "from supy._env import logger_supy\n"  # env var read at _env import
        "logger_supy.info('hello')\n"
        "print('OK')\n"
    )
    res = _run(code, tmp_path, extra_env={"SUPY_LOG_DIR": str(log_dir)})
    assert res.returncode == 0, res.stderr
    assert "OK" in res.stdout
    assert (log_dir / LOG_FILE).exists(), res.stderr
    # The current working directory must stay clean.
    assert not (tmp_path / LOG_FILE).exists()


def test_logfile_path_expands_user():
    """A leading ``~`` in the log path is expanded to the home directory."""
    from supy._env import _coerce_logfile_path

    resolved = str(_coerce_logfile_path("~/supy_logtest_dir/run.log"))
    assert "~" not in resolved
    assert resolved.startswith(os.path.expanduser("~"))
    assert resolved.endswith("run.log")
