"""Annual EHC comparisons against the explicit Step-1 parent and the previous EHC head.

Explicit coupling must preserve every parent column over the entire year.
Default implicit coupling must preserve all recorded columns with stability
control off, bit for bit. Set SUEWS_EHC_EXPLICIT_REFERENCE to the Step-1
annual output directory and SUEWS_EHC_IMPLICIT_REFERENCE to the previous EHC
annual output directory. References use flat group|variable column names.
The compact, committed references in test_ehc_reference.py provide the CI gate;
these external annual comparisons are supplementary.
Missing files in an explicitly supplied reference directory are failures.
"""

import os
from pathlib import Path
import sys

import numpy as np
import pandas as pd
import pytest

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "fixtures"))
from ehc_reference_io import build_sim

pytestmark = [
    pytest.mark.physics,
    pytest.mark.contract("C-PARITY", "C-RESPONSE"),
]


def _reference_dir(name):
    path = os.environ.get(name)
    if not path:
        pytest.skip(f"{name} not set: requires external same-build annual outputs")
    result = Path(path)
    assert result.is_dir(), result
    return result


def _assert_bits(actual, expected):
    pd.testing.assert_frame_equal(actual, expected, check_exact=True)
    np.testing.assert_array_equal(
        actual.to_numpy(dtype=np.float64).view(np.uint64),
        expected.to_numpy(dtype=np.float64).view(np.uint64),
    )


def _save_result(frame, name):
    path = os.environ.get("SUEWS_EHC_RESULT_DIR")
    if path:
        target = Path(path)
        target.mkdir(parents=True, exist_ok=True)
        frame.to_parquet(target / f"{name}.parquet")


@pytest.mark.parametrize("case", ["sample", "ehc_slab", "ehc_facets"])
def test_explicit_matches_step1_parent(case):
    reference = _reference_dir("SUEWS_EHC_EXPLICIT_REFERENCE") / f"{case}.parquet"
    assert reference.is_file(), reference
    expected = pd.read_parquet(reference)
    actual = build_sim(case, "explicit").run(n_jobs=1).df
    _save_result(actual, f"explicit_{case}")
    actual.columns = ["|".join(column) for column in actual.columns]
    missing = expected.columns.difference(actual.columns)
    assert missing.empty, f"parent columns missing: {list(missing)[:10]}"
    _assert_bits(actual[expected.columns], expected)


@pytest.mark.parametrize("case", ["ehc_slab", "ehc_facets"])
def test_default_implicit_matches_recorded_stability_off(case):
    reference = _reference_dir("SUEWS_EHC_IMPLICIT_REFERENCE") / f"{case}.parquet"
    assert reference.is_file(), reference
    expected = pd.read_parquet(reference)
    actual = build_sim(case).run(n_jobs=1).df
    _save_result(actual, f"default_{case}")
    actual.columns = ["|".join(column) for column in actual.columns]
    _assert_bits(actual, expected)
