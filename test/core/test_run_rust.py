from importlib import import_module

import pytest

_run_rust = import_module("supy._run_rust")


class FakeRustModule:
    def __init__(self, group_ncolumns):
        self._group_ncolumns = group_ncolumns

    def output_group_ncolumns(self):
        return self._group_ncolumns


@pytest.fixture(autouse=True)
def reset_output_layout_validation(monkeypatch):
    _run_rust._validate_output_layout.cache_clear()
    yield
    _run_rust._validate_output_layout.cache_clear()


def test_validate_output_layout_ignores_registry_only_groups(monkeypatch):
    monkeypatch.setattr(_run_rust, "_GROUP_ORDER", ("SUEWS", "DailyState"))
    monkeypatch.setattr(
        _run_rust,
        "_GROUP_DATA_COLS_BY_NAME",
        {"SUEWS": 2, "DailyState": 3},
    )

    rust_module = FakeRustModule([
        ("datetime", 5),
        ("SUEWS", 2),
        ("DailyState", 3),
        ("BL", 999),
    ])

    _run_rust._validate_output_layout(rust_module)

    assert _run_rust._validate_output_layout.cache_info().currsize == 1


def test_validate_output_layout_reports_generated_registry_path(monkeypatch):
    monkeypatch.setattr(_run_rust, "_GROUP_ORDER", ("SUEWS", "DailyState"))
    monkeypatch.setattr(
        _run_rust,
        "_GROUP_DATA_COLS_BY_NAME",
        {"SUEWS": 2, "DailyState": 3},
    )

    rust_module = FakeRustModule([
        ("datetime", 5),
        ("SUEWS", 2),
    ])

    with pytest.raises(RuntimeError, match="DailyState") as exc_info:
        _run_rust._validate_output_layout(rust_module)

    assert "src/supy/data_model/output/dailystate_vars.py" in str(exc_info.value)
