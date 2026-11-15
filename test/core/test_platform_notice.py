"""The once-per-process notice for platforms with reduced test coverage."""

from __future__ import annotations

from types import SimpleNamespace
import warnings

import pytest

import supy._env as env
import supy._run_rust as run_rust

pytestmark = pytest.mark.api


@pytest.fixture
def fresh_notice(monkeypatch):
    """Let the notice fire again; the session fixture marks it as shown."""
    monkeypatch.setattr(env, "_platform_notice_shown", False)


def _as_platform(monkeypatch, system: str, machine: str, rosetta: bool) -> None:
    monkeypatch.setattr(env.sys, "platform", system)
    monkeypatch.setattr("platform.machine", lambda: machine)
    monkeypatch.setattr(env, "_running_under_rosetta", lambda: rosetta)


@pytest.mark.parametrize(
    ("system", "machine"),
    [("darwin", "arm64"), ("linux", "x86_64"), ("win32", "AMD64")],
)
def test_no_notice_on_fully_tested_platforms(monkeypatch, system, machine):
    _as_platform(monkeypatch, system, machine, rosetta=False)
    assert env._platform_support_notice() is None


def test_intel_mac_notice_asks_for_a_report(monkeypatch):
    _as_platform(monkeypatch, "darwin", "x86_64", rosetta=False)
    notice = env._platform_support_notice()
    assert notice.startswith("SuPy on macOS Intel")
    assert env.ISSUES_URL in notice
    assert notice.isascii()


def test_rosetta_notice_points_at_the_arm64_build(monkeypatch):
    _as_platform(monkeypatch, "darwin", "x86_64", rosetta=True)
    notice = env._platform_support_notice()
    assert notice.startswith("SuPy on macOS Intel")
    assert "arm64" in notice
    assert notice.isascii()


def test_notice_warns_once_per_process(monkeypatch, fresh_notice):
    _as_platform(monkeypatch, "darwin", "x86_64", rosetta=False)
    with warnings.catch_warnings(record=True) as caught:
        warnings.simplefilter("always")
        env.warn_platform_support()
        env.warn_platform_support()
    notices = [w for w in caught if "SuPy on macOS Intel" in str(w.message)]
    assert len(notices) == 1
    assert notices[0].category is UserWarning


def test_documented_filter_silences_the_notice(monkeypatch, fresh_notice):
    _as_platform(monkeypatch, "darwin", "x86_64", rosetta=True)
    with warnings.catch_warnings(record=True) as caught:
        warnings.simplefilter("always")
        warnings.filterwarnings("ignore", message="SuPy on macOS Intel")
        env.warn_platform_support()
    assert caught == []


def test_engine_check_raises_the_notice(monkeypatch, fresh_notice):
    """Every run path loads the engine through this check."""
    _as_platform(monkeypatch, "darwin", "x86_64", rosetta=False)
    engine = SimpleNamespace(run_suews_into=object())
    monkeypatch.setattr(run_rust, "_load_rust_module", lambda: engine)
    with pytest.warns(UserWarning, match="SuPy on macOS Intel"):
        assert run_rust._check_rust_available() is engine


def test_rosetta_probe_reads_false_off_macos():
    if env.sys.platform == "darwin":
        pytest.skip("the probe answers for the real machine on macOS")
    assert env._running_under_rosetta() is False
