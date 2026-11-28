"""Immutable method-5 output gate, mandatory in the CI reference lane.

A fast startup witness (``STARTUP_WINDOW``, 1-2 January) runs in ``core`` on
both slab and SPARTACUS facet routes against the matching rows of the
committed reference CSVs. Full-year evolution supplies compact startup, summer
and winter windows on both routes: ``ehc_slab`` stays in ``core``, while the
~280 s full-year ``ehc_facets`` run executes in ``physics-full`` and nightly
tiers (#2349 W1-5a). Exact comparison is restricted to the generating
toolchain, just as for the OHM reference; other platforms retain the model and
analytic tests.
"""

import hashlib
import json
import os
from pathlib import Path
import sys

import pandas as pd
import pytest

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / "test" / "fixtures"))
sys.path.insert(0, str(ROOT / "test" / "fixtures" / "data_test"))
from ehc_reference_io import (  # ruff: ignore[module-import-not-at-top-of-file] - repository fixture helpers
    CASES,
    REFERENCE_DIR,
    STARTUP_WINDOW,
    build_sim,
    read_reference_window_csv,
    reference_csv,
    select_reference,
    select_startup_reference,
)
from sample_output_io import (  # ruff: ignore[module-import-not-at-top-of-file]
    reference_environment_mismatch,
)

pytestmark = [
    pytest.mark.physics,
    pytest.mark.contract("C-RESPONSE", "C-OUTPUT"),
]


def _require_reference_toolchain():
    provenance = json.loads(
        (REFERENCE_DIR / "provenance.json").read_text(encoding="utf-8")
    )
    mismatch = reference_environment_mismatch(provenance, ROOT)
    if os.environ.get("SUEWS_REFERENCE_LANE") == "1":
        assert mismatch is None, f"CI reference toolchain mismatch: {mismatch}"
    elif mismatch:
        pytest.skip(
            f"Exact EHC reference requires the generating toolchain: {mismatch}"
        )


@pytest.mark.core
def test_ehc_reference_provenance():
    provenance = json.loads((REFERENCE_DIR / "provenance.json").read_text(encoding="utf-8"))
    for case in CASES:
        payload = (REFERENCE_DIR / f"{case}.csv").read_bytes().replace(b"\r\n", b"\n")
        assert hashlib.sha256(payload).hexdigest() == provenance["cases"][case]["sha256"]


@pytest.mark.core
@pytest.mark.parametrize("case", CASES)
def test_default_ehc_startup_matches_committed_reference(case):
    """Fast core witness comparing the committed 1-2 January startup window."""
    _require_reference_toolchain()
    end_date = pd.Timestamp(STARTUP_WINDOW[1]) - pd.Timedelta(seconds=1)
    actual = select_startup_reference(
        build_sim(case).run(end_date=end_date, n_jobs=1).df
    )
    expected = read_reference_window_csv(case, STARTUP_WINDOW)
    assert reference_csv(actual) == expected, (
        f"{case} startup window changed: inspect the output and scientific evidence "
        "before regenerating with python scripts/suews/gen_ehc_reference.py in the "
        "CI reference image"
    )


@pytest.mark.slow(reason="full-year evolution into compact summer and winter windows")
@pytest.mark.timeout(600)
@pytest.mark.parametrize(
    "case",
    [
        pytest.param("ehc_slab", marks=pytest.mark.core),
        pytest.param("ehc_facets"),
    ],
)
def test_default_ehc_matches_committed_reference(case):
    _require_reference_toolchain()
    actual = select_reference(build_sim(case).run(n_jobs=1).df)
    expected = (REFERENCE_DIR / f"{case}.csv").read_text(encoding="utf-8")
    assert reference_csv(actual) == expected, (
        f"{case} changed: inspect the output and scientific evidence before regenerating "
        "with python scripts/suews/gen_ehc_reference.py in the CI reference image"
    )
