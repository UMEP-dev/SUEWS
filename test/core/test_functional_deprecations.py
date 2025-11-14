"""Tests for legacy functional API deprecation warnings."""

import warnings

import pytest
import supy as sp

from supy._supy_module import _load_sample_data, _run_supy, trv_supy_module


@pytest.fixture(scope="module")
def _sample_run():
    df_state, df_forcing = _load_sample_data()
    result = _run_supy(df_forcing, df_state)
    df_output, df_state_final = result
    return df_state, df_forcing, df_output, df_state_final


def _assert_deprecation(warnings_record):
    dep_warnings = [
        warning
        for warning in warnings_record
        if issubclass(warning.category, DeprecationWarning)
    ]
    assert dep_warnings, "Expected DeprecationWarning but none were emitted."


def test_init_supy_deprecation_warning():
    path_config = trv_supy_module / "sample_data" / "sample_config.yml"
    with warnings.catch_warnings(record=True) as caught:
        warnings.simplefilter("always", DeprecationWarning)
        sp._deprecated_init_supy(str(path_config))
    _assert_deprecation(caught)


def test_run_supy_deprecation_warning(_sample_run):
    df_state, df_forcing, _, _ = _sample_run
    with warnings.catch_warnings(record=True) as caught:
        warnings.simplefilter("always", DeprecationWarning)
        sp._deprecated_run_supy(df_forcing, df_state)
    _assert_deprecation(caught)


def test_save_supy_deprecation_warning(_sample_run, tmp_path):
    _, _, df_output, df_state_final = _sample_run
    with warnings.catch_warnings(record=True) as caught:
        warnings.simplefilter("always", DeprecationWarning)
        sp._deprecated_save_supy(
            df_output,
            df_state_final,
            path_dir_save=tmp_path,
        )
    _assert_deprecation(caught)
