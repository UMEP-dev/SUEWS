"""Fortran build-profile plumbing in ``src/supy/run_make.py``.

The profile decides whether the physics library and the Rust bridge's own
Fortran sources are compiled with gfortran runtime checks (``checked``) or
optimised without them (``release``). The mapping to the Makefile's ``DEBUG``
variable is what this file pins, because a wrong default silently ships a
different binary.
"""

from __future__ import annotations

import importlib.util
from pathlib import Path

import pytest

pytestmark = pytest.mark.api


def _load_run_make():
    path = Path(__file__).resolve().parents[2] / "src" / "supy" / "run_make.py"
    spec = importlib.util.spec_from_file_location("suews_run_make", path)
    module = importlib.util.module_from_spec(spec)
    assert spec.loader is not None
    spec.loader.exec_module(module)
    return module


@pytest.fixture(scope="module")
def run_make():
    return _load_run_make()


def test_default_profile_is_release(run_make):
    assert run_make.build_profile_from_env({}) == "release"
    assert run_make.build_profile_from_env({"SUEWS_BUILD_PROFILE": ""}) == "release"


@pytest.mark.parametrize(
    ("raw", "expected"),
    [("release", "release"), ("checked", "checked"), (" Release ", "release")],
)
def test_profile_is_normalised(run_make, raw, expected):
    assert run_make.build_profile_from_env({"SUEWS_BUILD_PROFILE": raw}) == expected


def test_unknown_profile_is_rejected(run_make):
    with pytest.raises(SystemExit, match="SUEWS_BUILD_PROFILE must be one of"):
        run_make.build_profile_from_env({"SUEWS_BUILD_PROFILE": "fast"})


def test_checked_profile_forces_debug_on(run_make):
    assert run_make.make_args_for_profile("checked") == ["DEBUG=1"]


def test_release_profile_forces_debug_off(run_make):
    # An empty command-line DEBUG reaches the Makefile's ``ifndef DEBUG`` as
    # undefined and overrides its ``DEBUG ?= 1`` default.
    assert run_make.make_args_for_profile("release") == ["DEBUG="]


def test_make_jobs_default_is_capped_cpu_count(run_make):
    assert run_make.make_jobs_from_env({}, cpu_count=4) == 4
    assert run_make.make_jobs_from_env({}, cpu_count=64) == (
        run_make.MAX_DEFAULT_MAKE_JOBS
    )
    # os.cpu_count() may return None
    assert run_make.make_jobs_from_env({}, cpu_count=None) >= 1


def test_make_jobs_env_override(run_make):
    env = {"SUEWS_MAKE_JOBS": " 1 "}
    assert run_make.make_jobs_from_env(env, cpu_count=16) == 1
    env = {"SUEWS_MAKE_JOBS": "32"}
    assert run_make.make_jobs_from_env(env, cpu_count=4) == 32


@pytest.mark.parametrize("raw", ["0", "-2", "many"])
def test_make_jobs_rejects_invalid_override(run_make, raw):
    with pytest.raises(SystemExit, match="SUEWS_MAKE_JOBS must be a positive"):
        run_make.make_jobs_from_env({"SUEWS_MAKE_JOBS": raw})
